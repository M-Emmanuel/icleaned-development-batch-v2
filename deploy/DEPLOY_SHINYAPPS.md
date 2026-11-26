# Publishing a test copy of iCLEANED on Posit Connect Cloud

Posit is replacing shinyapps.io with **Posit Connect Cloud** (shinyapps.io apps are
moved there from January 2027), so the test copy is published on Connect Cloud. The
free plan allows up to 5 apps, 4 GB of memory, and public apps only.

## 1. Put both on GitHub (once)

Connect Cloud installs R packages from CRAN or GitHub, so the new CLEANED must be on
GitHub, in a **public** repository on the free plan:

- package: `M-Emmanuel/cleaned-staging-v2`, branch `feature/correct-functions`;
- app: `M-Emmanuel/icleaned-development-batch-v2`, branch `batch-testing`.

Never commit a `.Renviron` file (it is in `.gitignore`): it can hold Auth0 keys.

## 2. Connect RStudio to Connect Cloud (once)

1. Sign in at https://connect.posit.cloud (with GitHub is easiest).
2. In RStudio, update rsconnect and restart R:

   ```r
   install.packages("rsconnect")
   ```

3. Run `rsconnect::connectCloudUser()`. A browser window opens: sign in and allow
   access. `rsconnect::accounts()` then lists your Connect Cloud account.

No token has to be copied: the sign-in happens in the browser.

## 3. Publish

Open `icleaned.Rproj` (the app clone) in RStudio and run:

```r
source("deploy/deploy_shinyapps.R")
```

The script installs CLEANED from GitHub (`cleaned_github` at the top of the script),
checks that it has batch processing, puts a copy of the app with the test settings in
`deploy/bundle` and publishes it. The first publication takes 5 to 15 minutes (all
packages are installed); the app opens when it is ready. Share the link it shows.

After a change: push it to GitHub, then run the script again. The same app is updated
(the record is kept in `deploy/bundle/rsconnect`, which is not committed).

The test settings are in `deploy/bundle/.Renviron` and are read by `global.R` at
start-up: no login (`AUTH0_ENABLED=false`), the Batch page shown
(`ICLEANED_BATCH=true`), and a data folder created in the app (`ICLEANED_DATA_INIT=true`,
`DATA_DIR=icleaned_data`). The same variables can also be set in Connect Cloud under the
app's **Settings > Variables**.

## What testers should know

- **No login**: everyone uses the same test user and sees each other's scenarios,
  databases and batches. The first visitor sees the onboarding questions.
- **Temporary data**: anything saved may be lost when the app restarts or is published
  again. Download what you want to keep.
- Use batches of up to about 50 farms first.
- Report problems with the step number of the test plan, the time, and a screenshot.

## If something goes wrong

- *"CLEANED was not installed from GitHub"*: check `cleaned_github` (account,
  repository, branch) and that the branch was pushed.
- *Publication fails while installing packages*: open the app's logs in Connect Cloud;
  the package named there must be installable from CRAN or a public GitHub repository.
- *The app shows an error page about DATA_DIR*: the settings were not read; add the
  variables above under **Settings > Variables** in Connect Cloud and restart the app.
- *To use Auth0 logins instead*: add the app's Connect Cloud address to the allowed
  callback and logout URLs of the Auth0 application, and set `AUTH0_ENABLED=true`,
  `AUTH0_USER`, `AUTH0_KEY`, `AUTH0_SECRET` and `APP_URL` as secret variables in Connect
  Cloud (never in GitHub).
