# Weekly NIH RePORTER scan for taste/smell/chemosensory projects.
# Queries each term separately (a long OR list silently drops matches),
# unions the hits, pulls abstracts, and writes data/latest.json plus a dated archive copy.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr2, jsonlite)

`%||%` <- function(a, b) if (is.null(a)) b else a

api      <- Sys.getenv("REPORTER_API", "https://api.reporter.nih.gov/v2/projects/search")
days     <- as.integer(Sys.getenv("SCAN_DAYS", "7"))
today    <- Sys.Date()
window   <- list(from_date = format(today - days), to_date = format(today))

terms <- c("olfaction", "olfactory", "smell", "odor", "odorant", "odour", "anosmia",
           "hyposmia", "parosmia", "phantosmia", "gustation", "gustatory", "taste",
           "chemosensory", "chemosensation", "flavor", "dysgeusia", "pheromone",
           "vomeronasal", "sweetener", "bitter", "umami", "trigeminal")
search_fields <- c("projecttitle", "terms")
out_fields <- c("ProjectNum", "ApplId", "ProjectTitle", "PrincipalInvestigators",
                "Organization", "AgencyIcAdmin", "AwardAmount", "AwardType",
                "ActivityCode", "AwardNoticeDate", "ProjectDetailUrl", "DateAdded")

# One POST to RePORTER; RePORTER asks for <= 1 request/second.
reporter <- function(criteria, include_fields, offset = 0, limit = 500) {
  Sys.sleep(1)
  request(api) |>
    req_body_json(list(criteria = criteria, include_fields = as.list(include_fields),
                       offset = offset, limit = limit)) |>
    req_retry(max_tries = 5) |>
    req_timeout(120) |>
    req_perform() |>
    resp_body_json()
}

flatten_project <- function(x) {
  pis <- map_chr(x$principal_investigators %||% list(), \(p) str_squish(p$full_name %||% ""))
  tibble(
    appl_id           = x$appl_id,
    project_num       = x$project_num %||% NA_character_,
    title             = str_squish(x$project_title %||% NA_character_),
    pis               = paste(pis[pis != ""], collapse = "; "),
    org               = x$organization$org_name %||% NA_character_,
    org_city          = x$organization$org_city %||% NA_character_,
    org_state         = x$organization$org_state %||% NA_character_,
    ic                = x$agency_ic_admin$abbreviation %||% NA_character_,
    activity_code     = x$activity_code %||% NA_character_,
    award_type        = as.character(x$award_type %||% NA),
    award_amount      = as.numeric(x$award_amount %||% NA),
    award_notice_date = str_sub(x$award_notice_date %||% NA_character_, 1, 10),
    date_added        = str_sub(x$date_added %||% NA_character_, 1, 10),
    url               = x$project_detail_url %||% paste0("https://reporter.nih.gov/project-details/", x$appl_id)
  )
}

# ---- 1. one query per term x field, paginated ----
hits <- list()
for (t in terms) for (f in search_fields) {
  offset <- 0
  repeat {
    r <- reporter(list(date_added = window,
                       advanced_text_search = list(operator = "and", search_field = f, search_text = t)),
                  out_fields, offset = offset)
    if (length(r$results) > 0) {
      hits[[length(hits) + 1]] <- map(r$results, flatten_project) |> list_rbind() |>
        mutate(hit = paste0(str_sub(f, 1, 1), ":", t))
    }
    offset <- offset + 500
    if (offset >= (r$meta$total %||% 0)) break
  }
  message(sprintf("%-15s %-12s done", t, f))
}

rows <- list_rbind(hits)
if (nrow(rows) == 0) {
  projects <- tibble()
} else {
  projects <- rows |>
    select(-hit) |>
    distinct(appl_id, .keep_all = TRUE) |>
    left_join(rows |> summarise(hits = paste(unique(hit), collapse = ","), .by = appl_id),
              by = "appl_id")

  # ---- 2. abstracts, 200 appl_ids per call ----
  ids <- projects$appl_id
  abstracts <- split(ids, ceiling(seq_along(ids) / 200)) |>
    map(\(chunk) {
      r <- reporter(list(appl_ids = as.list(chunk)), c("ApplId", "AbstractText"))
      map(r$results, \(x) tibble(appl_id = x$appl_id,
                                 abstract = str_squish(x$abstract_text %||% ""))) |> list_rbind()
    }) |>
    list_rbind()
  projects <- projects |> left_join(abstracts, by = "appl_id") |> arrange(ic, project_num)
}

# ---- 3. write output ----
out <- list(
  generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  window           = window,
  terms            = terms,
  n_projects       = nrow(projects),
  projects         = projects
)
dir.create("data/archive", recursive = TRUE, showWarnings = FALSE)
write_json(out, "data/latest.json", auto_unbox = TRUE, pretty = TRUE, na = "null", dataframe = "rows")
invisible(file.copy("data/latest.json", file.path("data/archive", paste0(today, ".json")), overwrite = TRUE))
message(sprintf("Wrote %d projects for %s to %s", nrow(projects), window$from_date, window$to_date))
