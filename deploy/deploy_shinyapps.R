# Publish a test copy of iCLEANED on Posit Connect Cloud ---------------------------
#
# (Connect Cloud replaces shinyapps.io: Posit moves shinyapps.io apps to it in
# 2027.) Read deploy/DEPLOY_SHINYAPPS.md first. In short: CLEANED is on GitHub
# (your fork, branch with batch processing), and RStudio is connected once to
# your Connect Cloud account with rsconnect::connectCloudUser().
#
# Run this script from the app folder (the folder with global.R), e.g. open
# icleaned.Rproj in RStudio and source("deploy/deploy_shinyapps.R").
#
# The test copy:
#   - has no login: every tester uses the same test user ("dev");
#   - shows the Batch page;
#   - keeps its data in a temporary folder that is emptied when the app
#     restarts or is published again.

# 1. Settings: change these if needed ----------------------------------------------
app_title      <- "iCLEANED test"
cleaned_github <- "M-Emmanuel/cleaned-staging-v2@feature/correct-functions"  # GitHub fork and branch of CLEANED

# 2. Packages ----------------------------------------------------------------------
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
# (Connect Cloud needs a recent rsconnect)
if (!requireNamespace("rsconnect", quietly = TRUE) ||
    !exists("connectCloudUser", envir = asNamespace("rsconnect"))) {
  install.packages("rsconnect")
  stop("rsconnect was installed or updated: restart R (Session > Restart R) and run this script again.")
}

# CLEANED must be installed from GitHub: Connect Cloud installs it from the same place
remotes::install_github(cleaned_github, upgrade = "never", force = TRUE)
desc <- utils::packageDescription("cleaned")
if (!identical(desc$RemoteType, "github")) stop("CLEANED was not installed from GitHub; check cleaned_github.")
message("CLEANED ", desc$Version, " from ", desc$RemoteUsername, "/", desc$RemoteRepo, "@", desc$RemoteRef)
if (!all(c("write_batch_template", "batch_default_project", "read_odk_export") %in% getNamespaceExports("cleaned"))) {
  stop("This branch of CLEANED has no batch processing: check cleaned_github.")
}

# 3. Connect Cloud account (a browser window opens the first time) -----------------
accounts <- rsconnect::accounts()
cloud <- accounts[grepl("posit.cloud", accounts$server), , drop = FALSE]
if (!nrow(cloud)) {
  rsconnect::connectCloudUser()
  accounts <- rsconnect::accounts()
  cloud <- accounts[grepl("posit.cloud", accounts$server), , drop = FALSE]
}
if (!nrow(cloud)) stop("No Connect Cloud account is connected: run rsconnect::connectCloudUser() and sign in.")

# 4. A copy of the app with the test settings --------------------------------------
if (!file.exists("global.R")) stop("Run this script from the app folder (the one with global.R).")
# (deploy/bundle keeps the record of the publication in deploy/bundle/rsconnect,
# so publishing again updates the same app instead of making a new one;
# it is not committed to GitHub)
deploy_dir <- file.path("deploy", "bundle")
dir.create(deploy_dir, showWarnings = FALSE)
old <- setdiff(list.files(deploy_dir, all.files = TRUE, no.. = TRUE), "rsconnect")
unlink(file.path(deploy_dir, old), recursive = TRUE)
app_files <- c("global.R", "server.R", "ui.R", "R", "www", "data", "_auth0.yml",
               "onboarding_questions.yaml", "LICENSE")
file.copy(app_files[file.exists(app_files)], deploy_dir, recursive = TRUE)
# test settings, read by global.R at start-up
writeLines(c(
  "AUTH0_ENABLED=false",
  "ICLEANED_BATCH=true",
  "ICLEANED_DATA_INIT=true",
  "DATA_DIR=icleaned_data",
  "APP_URL=https://connect.posit.cloud",
  paste0("CLEANED_GITHUB=", cleaned_github)
), file.path(deploy_dir, ".Renviron"))
# (renv.lock is not copied: the packages are taken from this computer's
# library, with CLEANED from GitHub)

# 5. Publish -----------------------------------------------------------------------
rsconnect::deployApp(
  appDir = deploy_dir,
  appFiles = grep("^rsconnect/", list.files(deploy_dir, recursive = TRUE, all.files = TRUE), value = TRUE, invert = TRUE),
  appTitle = app_title,
  account = cloud$name[1],
  server = cloud$server[1],
  forceUpdate = TRUE,
  launch.browser = TRUE
)
