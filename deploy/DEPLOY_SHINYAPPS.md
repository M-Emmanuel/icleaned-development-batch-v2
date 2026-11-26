# Publishing a test copy of iCLEANED on shinyapps.io

These steps publish the app with CLEANED 0.20.0 on a free shinyapps.io account, so
testers only need a link. Steps 1 and 2 are done once; step 4 is repeated after each
change.

## 1. Put both on GitHub (your forks)

shinyapps.io installs R packages only from CRAN or GitHub, so CLEANED 0.20.0 must be
on GitHub.

1. On GitHub, fork the CLEANED repository (CIAT/cleaned) and the iCLEANED app repository.
2. Clone each fork and make a branch, for example `batch-testing`:

   ```
   git clone https://github.com/<you>/cleaned.git
   cd cleaned
   git checkout -b batch-testing
   ```

3. Copy the content of `cleaned-staging-updated.zip` into the package clone (replace
   the files), and the content of `icleaned-staging-updated.zip` into the app clone.
   Delete the stray file `offfarm_grazing` in the package folder.
4. Check what changed, then commit and push (do the same for the app):

   ```
   git status
   git add -A
   git commit -m "CLEANED 0.20.0: batch processing, farmer survey, livestock categories"
   git push -u origin batch-testing
   ```

   Never commit a `.Renviron` file (it is in `.gitignore`): it can hold Auth0 keys.

## 2. Connect your shinyapps.io account to R

1. Sign up at https://www.shinyapps.io (free plan).
2. Open **Account > Tokens > Add token > Show**, then **Show secret**, and copy the
   `rsconnect::setAccountInfo(name = ..., token = ..., secret = ...)` command.
3. Paste it once in the RStudio console and run it. Keep the token and secret private:
   do not put them in a script or on GitHub.

## 3. Check the settings of the deploy script

Open `deploy/deploy_shinyapps.R` in the app clone and set:

- `account`: your shinyapps.io account name;
- `app_name`: for example `icleaned-test` (the link will be
  `https://<account>.shinyapps.io/icleaned-test/`);
- `cleaned_github`: your fork and branch, for example `M-Emmanuel/cleaned@batch-testing`.

## 4. Publish

Open `icleaned.Rproj` in RStudio and run:

```r
source("deploy/deploy_shinyapps.R")
```

The script installs CLEANED from GitHub, checks its version, copies the app with the
test settings into a temporary folder and publishes it. The first publication takes
5 to 15 minutes (shinyapps.io installs all packages); the app opens when it is ready.

After a change: push it to GitHub, then run the script again.

## 5. Settings on shinyapps.io (once)

In the dashboard, open the app, then **Settings**:

- **General > Instance idle timeout**: set the longest value offered, so a batch is not
  stopped while it runs.
- **Advanced > Max worker processes**: 1, so that all testers share the same data folder.

## What testers should know

- **No login**: everyone uses the same test user and sees each other's scenarios,
  databases and batches. The first visitor sees the onboarding questions.
- **Temporary data**: everything is lost when the app restarts (after being idle) or is
  published again. Download anything you want to keep.
- **Free plan limits**: 1 GB of memory and 25 active hours a month. Use batches of up to
  about 50 farms; larger batches may run out of memory.
- Report problems with the step number of the test plan, the time, and a screenshot.

## If something goes wrong

- *"CLEANED was not installed from GitHub"*: check `cleaned_github` (account, repository
  and branch) and that the branch was pushed.
- *Package installation fails on shinyapps.io*: open **Logs** in the dashboard; the
  package named there must be installable from CRAN or GitHub.
- *The app starts but shows an error page about DATA_DIR*: the `.Renviron` made by the
  script was not published; run the script again from the app folder.
- *To use Auth0 logins instead*: add `https://<account>.shinyapps.io/<app_name>/` to the
  allowed callback and logout URLs of the Auth0 application, and publish with
  `AUTH0_ENABLED=true` plus `AUTH0_USER`, `AUTH0_KEY` and `AUTH0_SECRET` in the
  `.Renviron` written by the script (never committed to GitHub).
