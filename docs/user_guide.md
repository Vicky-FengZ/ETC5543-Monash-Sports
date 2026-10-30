# User guide for Monash Sport staff

This guide is for the Monash Sport team who use the dashboard with their own
reports. It covers how to run the dashboard, where to keep the report
workbooks, how to upload them and how to add a new year. Reviewers without
access to the reports can use the synthetic workbooks in `data/sample/`
instead (see the README).

## 1. Open the dashboard

Open **[https://vicky-fengz.github.io/ETC5543-Monash-Sports/](https://vicky-fengz.github.io/ETC5543-Monash-Sports/)** in Chrome or Edge. Nothing needs to be installed.

The first visit takes about 30 seconds while the dashboard loads into the
browser; later visits are faster. The whole dashboard runs inside your
browser, so the reports you upload stay on your computer (see section 5).

### Running it from RStudio instead

Maintainers can also run it locally:

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

## 2. Where to keep the reports

Keep one shared folder for the dashboard reports, for example on the team's
OneDrive, SharePoint or Teams site with access limited to Monash Sport staff.
Use one subfolder per report family:

```text
Monash Sport Dashboard Reports/
  Monthly Site Report/
    Monthly Site Report 2025 -December.xlsx
  Business Unit Templates that feed master reports/
    2025/
      Aquatics Site Report 2025.xlsx
      Caulfield Site Report 2025.xlsx
      Health & Fitness Site Report 2025.xlsx
      Participation Program Report 2025.xlsx
      Peninsula Site Report 2025.xlsx
      Venues Site Report 2025.xlsx
  Business Unit Tracking Reports/
    2025 Venues Tracking Report.xlsx
```

The dashboard reads the report year from the file name and recognises the
report type from it, so:

| Rule | Example |
|---|---|
| Each file name contains **exactly one** four-digit year | `Aquatics Site Report 2025.xlsx`, not `Aquatics Site Report 2025 (copy of 2024).xlsx` |
| Monthly Site Reports start with `Monthly Site Report` | `Monthly Site Report 2026 - March.xlsx` |
| Business unit reports contain `Site Report` or `Program Report` | `Participation Program Report 2026.xlsx` |
| Venues reports contain `Venues Tracking Report` | `2026 Venues Tracking Report.xlsx` |
| Keep **one** Monthly Site Report and one Venues Tracking Report per year | Replace the year's file with the latest version instead of adding a second one |
| Keep drafts, old copies and other files elsewhere | Duplicate years are flagged as warnings |

Keep the worksheet names, headings and layouts of the existing reports. Blank
templates are available from the Data page and in `www/templates/`; see
[Excel templates](excel_templates.md) for how to fill in and extend them.

## 3. Upload the reports

1. Open the **Data** page and stay on **Upload & Manage**.
2. Under **Upload all reports**, select **Choose folder** and pick the shared
   report folder (subfolders are included), or select **Choose files** and
   select all the workbooks at once. Each file is matched to its report type
   from its file name, so the naming rules above matter.
3. Keep the page open while the files are parsed. A blue message shows the
   progress, and the upload buttons are paused until parsing finishes.
4. Read the summary that appears below the upload cards. It lists how many
   files were matched to each report type, and any files that were **not
   recognised** (rename these to follow the naming rules, or upload them
   through the matching report-type card). Other files in the folder, such as
   Word documents, are ignored.
5. Wait until each file shows `ready`. Select **View details** under a file to
   read any warnings; a `failed` file is left out of the dashboard. If a file
   was uploaded through the wrong report-type card, its message says which
   card to use.
6. Use the other pages. **View Cleaned Data** shows and downloads the
   standardised tables.

To add or replace a single file later, use the card for its report type under
**Or add files to one report type**.

If a red message says **The upload did not finish**, the files never reached
the dashboard. Wait until any parsing has finished, then select them again.

Close the workbooks in Excel before uploading. Uploads last only for the
current session: closing or refreshing the dashboard clears them, and your
original files are never changed.

## 4. Add a new month or year

- **New month:** save the updated Monthly Site Report over the year's existing
  file (or rename it so only the latest version remains), then upload it
  again. A file with the same name replaces the earlier upload.
- **New year:** add the new workbooks to the same subfolders, following the
  naming rules above, and upload them with the earlier years. Forecasting
  needs at least 12 continuous months and works best with two or more years.

The README section *Adding future report years* lists the structure each
report must keep.

## 5. Keeping the data private

- The reports are confidential and are **not stored in this repository**.
  Never add them to Git or attach them to issues.
- The website contains only the dashboard's code. When you upload reports,
  they are read by the dashboard **inside your browser** and are never sent
  to a server, including GitHub.
- Uploads last only while the page is open. Closing or refreshing the page
  removes them.
- When the dashboard runs from RStudio, uploaded files are copied only to a
  temporary folder on that computer and removed when the session ends.

## 6. For maintainers: running the full tests

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

Before committing, check that `git status` does not list any file from
`data/raw/`.
