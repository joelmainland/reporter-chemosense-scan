# reporter-chemosense-scan

Weekly NIH RePORTER pull of projects added in the last 7 days that match taste/smell/chemosensory terms.

- `scan.R` queries each term separately against project titles and RePORTER terms, unions the hits, fetches abstracts, and writes `data/latest.json` (plus `data/archive/YYYY-MM-DD.json`).
- `.github/workflows/scan.yml` runs it Sundays at 12:00 UTC (after RePORTER's Saturday load) and commits the output.
- A Claude scheduled task reads `data/latest.json` on Monday morning, screens relevance on title and abstract, and sends the report.

Manual run: **Actions → Weekly RePORTER chemosensory scan → Run workflow**.
Local run: `Rscript scan.R` (set `SCAN_DAYS=14` to widen the window).

The data is public NIH records, so the repo can be public. The Claude task reads it from raw.githubusercontent.com.
