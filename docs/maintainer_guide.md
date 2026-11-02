# Maintainer guide

This guide is for whoever looks after the dashboard's code: running it from
RStudio, testing changes against the real reports and publishing a new
version. Staff who only use the dashboard should read the
[user guide](user_guide.pdf) instead.

For more detail, see the main [README](../README.md): *Project structure*,
*Adding future report years*, *Testing* and *Performance and website
deployment*.

## 1. Run the dashboard from RStudio

The published website needs no installation. To run the dashboard locally
instead, for example while changing the code:

1. Install R 4.4 and RStudio.
2. Download or clone this repository and open `monash-sport-dashboard.Rproj`
   in RStudio.
3. The first time only, install the packages recorded for the project:

   ```r
   renv::restore()
   ```

4. Start the dashboard:

   ```r
   shiny::runApp()
   ```

   It opens in a browser window. Leave RStudio open while you use it.

Reading the reports is much faster in desktop R than in the browser. When the
dashboard runs from RStudio, uploaded files are copied only to a temporary
folder on that computer and removed when the session ends.

## 2. Run the full tests on the real reports

After changing the import, cleaning, validation or dashboard code, run the
full test suite against the real reports:

1. Copy the shared report folder's three subfolders into `data/raw/` in the
   project, keeping the same structure:

   ```text
   data/raw/
     Monthly Site Report/
     Business Unit Templates that feed master reports/<year>/
     Business Unit Tracking Reports/
   ```

   `data/raw/` is listed in `.gitignore`, so its contents are never committed.

2. Run, from the project root:

   ```r
   source("tests/run_full_tests.R")
   ```

   The run takes several minutes and ends with `ALL TESTS PASSED`.

Quicker test runs that need only the synthetic workbooks are described in
[tests/README.md](../tests/README.md). GitHub Actions runs the sample tests on
every push.

## 3. Publish a new version

The website is a Shinylive build published to GitHub Pages by
`.github/workflows/deploy-shinylive.yml` on every push to `main`. To build it
locally, run from the project root, outside renv:

```r
Rscript --vanilla deploy/build_shinylive.R
```

The build contains only the app code (`app.R`, `R/`, `www/` and `config/`),
never `data/`, `tests/` or `renv/`. The rendered user guide
(`docs/user_guide.html` and `.pdf`) is copied next to the app, so it is
published at `https://vicky-fengz.github.io/ETC5543-Monash-Sports/user_guide.html`,
where the **User guide** link in the dashboard's navbar points
(`USER_GUIDE_URL` in `R/config.R`).

When a change alters the structure of the cleaned tables, increase
`BASIC_SAVED_DATA_VERSION` in `R/mod_data_upload.R`, so that saved data files
from the earlier version ask users to upload their reports again.

## 4. Keep the data private

- Never commit the confidential reports, files in `data/raw/` or saved data
  files (`monash-sport-saved-data*.rds`) to Git, and never attach them to
  issues.
- Use the synthetic workbooks in `data/sample/` for screenshots, examples and
  bug reports.

## 5. Update the user guide

The user guide is written in Quarto (`docs/user_guide.qmd`). Its screenshots
are in `docs/images/user_guide/` and must be taken with the synthetic sample
workbooks, never the real reports. After editing it, render both the web page
and the PDF from the project root (RStudio Terminal):

```bash
quarto render docs/user_guide.qmd
```

This writes `docs/user_guide.html` and `docs/user_guide.pdf`. Commit both, so
staff can open the PDF directly on GitHub.
