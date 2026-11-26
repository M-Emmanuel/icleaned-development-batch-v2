# Publish a test copy of iCLEANED on shinyapps.io ----------------------------------
#
# Read deploy/DEPLOY_SHINYAPPS.md first. In short: CLEANED is on GitHub (your
# fork, branch with version 0.20.0), and your shinyapps.io account is connected
# to R once with rsconnect::setAccountInfo(...) (the command shown on
# shinyapps.io > Account > Tokens).
#
# Run this script from the app folder (the folder with global.R), e.g. open
# icleaned.Rproj in RStudio and source("deploy/deploy_shinyapps.R").
#
# The test copy:
#   - has no login: every tester uses the same test user ("dev");
#   - shows the Batch page;
#   - keeps its data in a temporary folder that is emptied when the app
#     restarts (after being idle) or is published again.

# 1. Settings: change these ------------------------------------------------------
account        <- "your-shinyapps-account"            # shinyapps.io account name
app_name       <- "icleaned-test"                     # https://<account>.shinyapps.io/<app_name>/
cleaned_github <- "M-Emmanuel/cleaned@batch-testing"  # GitHub fork and branch with CLEANED 0.20.0

# 2. Packages ----------------------------------------------------------------------
for (p in c("rsconnect", "remotes")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

# CLEANED must be installed from GitHub: shinyapps.io installs it from the same place
remotes::install_github(cleaned_github, upgrade = "never", force = TRUE)
desc <- utils::packageDescription("cleaned")
if (!identical(desc$RemoteType, "github")) stop("CLEANED was not installed from GitHub; check cleaned_github.")
message("CLEANED ", desc$Version, " from ", desc$RemoteUsername, "/", desc$RemoteRepo, "@", desc$RemoteRef)
if (utils::compareVersion(desc$Version, "0.20.0") < 0) stop("This branch has CLEANED ", desc$Version, ", not 0.20.0 or later.")

# 3. shinyapps.io account ----------------------------------------------------------
if (!account %in% rsconnect::accounts()$name) {
  stop("Account '", account, "' is not connected. On shinyapps.io open Account > Tokens > Show, ",
       "copy the rsconnect::setAccountInfo(...) command and run it once in the R console.")
}

# 4. A copy of the app with the test settings --------------------------------------
if (!file.exists("global.R")) stop("Run this script from the app folder (the one with global.R).")
deploy_dir <- file.path(tempdir(), app_name)
unlink(deploy_dir, recursive = TRUE)
dir.create(deploy_dir)
app_files <- c("global.R", "server.R", "ui.R", "R", "www", "data", "_auth0.yml",
               "onboarding_questions.yaml", "LICENSE")
file.copy(app_files[file.exists(app_files)], deploy_dir, recursive = TRUE)
writeLines(c(
  "AUTH0_ENABLED=false",
  "ICLEANED_BATCH=true",
  "ICLEANED_DATA_INIT=true",
  "DATA_DIR=icleaned_data",
  sprintf("APP_URL=https://%s.shinyapps.io/%s/", account, app_name),
  paste0("CLEANED_GITHUB=", cleaned_github)
), file.path(deploy_dir, ".Renviron"))
# (renv.lock is not copied: the packages are taken from this computer's
# library, with CLEANED from GitHub)

# 5. Publish -----------------------------------------------------------------------
rsconnect::deployApp(
  appDir = deploy_dir,
  appFiles = list.files(deploy_dir, recursive = TRUE, all.files = TRUE),
  appName = app_name,
  appTitle = "iCLEANED (test)",
  account = account,
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = TRUE
)
