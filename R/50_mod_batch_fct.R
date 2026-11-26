# Batch processing: helpers ------------------------------------------------------
#
# Batches are stored in <user folder>/batches/<batch id>/:
#   batch.json       settings and status (see batch_meta_read)
#   input.xlsx       the uploaded batch file
#   validation.rds   the check report
#   progress.txt     "<farm number> <farms> <time> <farm name>" (written while running)
#   output/          results written by cleaned::run_batch()

# Feature switch: set ICLEANED_BATCH=true (e.g. in .Renviron) to show the Batch page
batch_processing_enabled <- function() {
  tolower(Sys.getenv("ICLEANED_BATCH", "false")) %in% c("true", "1", "yes")
}


# Seconds without progress after which a running batch is considered stopped
batch_stall_seconds <- 15 * 60

# Friendly names of the summary indicators, grouped by topic
batch_indicator_labels <- c(
  total_milk_produced_kg_fpcm_per_year = "Production|Milk produced (kg FPCM/year)",
  total_meat_produced_kg_per_year = "Production|Meat produced (kg/year)",
  total_protein_produced_kg_per_year = "Production|Protein produced (kg/year)",
  total_tlu = "Production|Herd size (TLU)",
  total_land_requirement_ha = "Land|Land required (ha)",
  total_land_requirement_ha_per_kg_fpcm = "Land|Land per kg milk (ha/kg FPCM)",
  total_land_requirement_ha_per_kg_meat = "Land|Land per kg meat (ha/kg)",
  total_land_requirement_ha_per_kg_protein = "Land|Land per kg protein (ha/kg)",
  total_land_requirement_ha_per_tlu = "Land|Land per TLU (ha/TLU)",
  total_n_balance_kg_n_per_year = "Nitrogen|N balance (kg N/year)",
  percent_area_mining = "Nitrogen|Area with N mining (%)",
  percent_area_leaching = "Nitrogen|Area with N leaching risk (%)",
  n_balance_kg_n_per_ha_per_year = "Nitrogen|N balance per ha (kg N/ha/year)",
  n_balance_kg_n_per_kg_fpcm = "Nitrogen|N balance per kg milk (kg N/kg FPCM)",
  n_balance_kg_n_per_kg_meat = "Nitrogen|N balance per kg meat (kg N/kg)",
  n_balance_kg_n_per_kg_protein = "Nitrogen|N balance per kg protein (kg N/kg)",
  erosion_t_soil_year = "Soil erosion|Soil loss (t/year)",
  erosion_t_soil_per_ha_per_year = "Soil erosion|Soil loss per ha (t/ha/year)",
  erosion_kgsoil_per_kg_fpcm = "Soil erosion|Soil loss per kg milk (kg/kg FPCM)",
  erosion_kgsoil_per_kg_meat = "Soil erosion|Soil loss per kg meat (kg/kg)",
  erosion_kgsoil_per_kg_protein = "Soil erosion|Soil loss per kg protein (kg/kg)",
  ghg_emission_t_co2_eq_per_year = "Greenhouse gases|Emissions (t CO2-eq/year)",
  ghg_emission_t_co2_eq_per_ha_per_year = "Greenhouse gases|Emissions per ha (t CO2-eq/ha/year)",
  ghg_emission_kg_co2_eq_per_kg_fpcm = "Greenhouse gases|Emissions per kg milk (kg CO2-eq/kg FPCM)",
  ghg_emission_kg_co2_eq_per_kg_meat = "Greenhouse gases|Emissions per kg meat (kg CO2-eq/kg)",
  ghg_emission_kg_co2_eq_per_kg_protein = "Greenhouse gases|Emissions per kg protein (kg CO2-eq/kg)",
  percent_precipitation_used_for_feed_production = "Water|Rainfall used for feed (%)",
  total_water_use_m3 = "Water|Water use (m3/year)",
  total_water_use_m3_per_ha = "Water|Water use per ha (m3/ha)",
  total_water_use_m3_per_kg_fpcm = "Water|Water per kg milk (m3/kg FPCM)",
  total_water_use_m3_per_kg_meat = "Water|Water per kg meat (m3/kg)",
  total_water_use_m3_per_kg_protein = "Water|Water per kg protein (m3/kg)",
  carbon_stock_change_t_co2eq_per_year = "Carbon|Carbon stock change (t CO2-eq/year)",
  carbon_stock_change_t_co2eq_per_ha_per_year = "Carbon|Carbon stock change per ha (t CO2-eq/ha/year)",
  carbon_stock_change_t_co2eq_per_fpcm = "Carbon|Carbon stock change per kg milk",
  carbon_stock_change_t_co2eq_per_meat = "Carbon|Carbon stock change per kg meat",
  carbon_stock_change_t_co2eq_per_protein = "Carbon|Carbon stock change per kg protein",
  total_carbon_balance_per_fpcm = "Carbon|Carbon balance per kg milk",
  total_carbon_balance_per_meat = "Carbon|Carbon balance per kg meat",
  total_carbon_balance_per_protein = "Carbon|Carbon balance per kg protein"
)

# Choices for an indicator dropdown, grouped by topic
batch_indicator_choices <- function(available) {
  keep <- batch_indicator_labels[names(batch_indicator_labels) %in% available]
  topic <- sub("\\|.*", "", keep)
  lapply(split(stats::setNames(names(keep), sub(".*\\|", "", keep)), factor(topic, unique(topic))), as.list)
}
batch_indicator_label <- function(x) {
  lab <- batch_indicator_labels[x]
  ifelse(is.na(lab), x, sub(".*\\|", "", lab))
}

# Per-unit indicators only make sense for farms that produce that output
batch_indicator_base <- function(indicator) {
  if (grepl("fpcm$", indicator)) return("total_milk_produced_kg_fpcm_per_year")
  if (grepl("meat$", indicator)) return("total_meat_produced_kg_per_year")
  if (grepl("protein$", indicator)) return("total_protein_produced_kg_per_year")
  if (grepl("per_tlu$", indicator)) return("total_tlu")
  NULL
}
batch_base_words <- c(total_milk_produced_kg_fpcm_per_year = "produce no milk",
                      total_meat_produced_kg_per_year = "produce no meat",
                      total_protein_produced_kg_per_year = "produce no protein",
                      total_tlu = "have no animals")

# Values of one indicator, without farms for which it has no meaning
batch_indicator_data <- function(summary, indicator) {
  d <- summary
  d$value <- suppressWarnings(as.numeric(d[[indicator]]))
  base <- batch_indicator_base(indicator)
  drop <- !is.finite(d$value)
  why <- NULL
  if (!is.null(base) && base %in% names(d)) {
    no_output <- !(suppressWarnings(as.numeric(d[[base]])) > 0) %in% TRUE
    drop <- drop | no_output
    why <- batch_base_words[[base]]
  }
  out <- d[!drop, , drop = FALSE]
  attr(out, "left_out") <- list(n = sum(drop), why = why %||% "have no value")
  out
}

# Age groups of the respondents, from the ages where each group starts
# (e.g. "18, 35, 60" -> 18-34, 35-59, 60 or over); empty ages: "Age not given"
batch_age_starts <- function(text) {
  v <- suppressWarnings(as.numeric(trimws(strsplit(text %||% "", "[,; ]+")[[1]])))
  sort(unique(v[!is.na(v) & v >= 0]))
}
# Age group definitions offered on the results page (respondents are 18 or
# older: the youth groups start at 18). UN: youth 15-24 (UN Secretariat), older
# persons 65 or over (UN population statistics); African Union: youth 15-35
# (African Youth Charter).
batch_age_schemes <- list(
  un = list(label = "UN: youth 18-24, adults 25-64, 65 or over", starts = c(18, 25, 65),
            names = c("Youth (18-24)", "Adults (25-64)", "Older persons (65 or over)")),
  au = list(label = "African Union: youth 18-35, adults 36 or over", starts = c(18, 36),
            names = c("Youth (18-35)", "Adults (36 or over)"))
)
batch_age_groups <- function(age, starts, names = NULL) {
  age <- suppressWarnings(as.numeric(age))
  if (!length(starts)) return(ifelse(is.na(age), "Age not given", "All ages"))
  labels <- c(paste("Under", starts[1]),
              if (length(starts) > 1) paste0(starts[-length(starts)], "-", starts[-1] - 1),
              paste(starts[length(starts)], "or over"))
  if (length(names) == length(starts)) labels[-1] <- names
  k <- findInterval(age, starts) + 1
  out <- labels[k]
  out[is.na(age)] <- "Age not given"
  # in the order of the ages (then "not given")
  factor(out, levels = intersect(c(labels, "Age not given"), unique(out)))
}

# Median or mean of the indicators per group, computed from the farm summary
batch_group_stats <- function(summary, group, stat = "median") {
  ind <- intersect(names(batch_indicator_labels), names(summary))
  f <- if (stat == "mean") function(x) mean(x, na.rm = TRUE) else function(x) stats::median(x, na.rm = TRUE)
  groups <- levels(factor(group))
  out <- do.call(rbind, lapply(groups, function(gv) {
    rows <- !is.na(group) & as.character(group) == gv
    vals <- lapply(ind, function(i) { x <- suppressWarnings(as.numeric(summary[[i]][rows])); v <- f(x[is.finite(x)]); if (is.finite(v)) v else NA_real_ })
    data.frame(group = gv, farms = sum(rows), stats::setNames(vals, ind), check.names = FALSE, stringsAsFactors = FALSE)
  }))
  out
}

# Notes of finished farms, summarised: one row per note (and livestock type)
batch_summarise_notes <- function(log) {
  ok <- log[log$status == "ok" & !is.na(log$warnings), , drop = FALSE]
  if (!nrow(ok)) return(data.frame(note = character(0), livestock = character(0), farms = integer(0), farm_list = character(0)))
  rows <- do.call(rbind, lapply(seq_len(nrow(ok)), function(k) {
    notes <- strsplit(ok$warnings[k], " | ", fixed = TRUE)[[1]]
    do.call(rbind, lapply(notes, function(n) {
      if (grepl("^Dry matter intake above 4% of body weight: ", n)) {
        types <- strsplit(sub("^[^:]+: ", "", n), ", ", fixed = TRUE)[[1]]
        data.frame(note = "Daily dry matter intake above 4% of body weight", livestock = types, farm = ok$farm_name[k])
      } else {
        data.frame(note = n, livestock = "", farm = ok$farm_name[k])
      }
    }))
  }))
  key <- paste(rows$note, rows$livestock, sep = "\r")
  out <- do.call(rbind, lapply(split(rows, factor(key, unique(key))), function(x) data.frame(
    note = x$note[1], livestock = x$livestock[1], farms = length(unique(x$farm)),
    farm_list = paste(unique(x$farm), collapse = ", "))))
  rownames(out) <- NULL
  out[order(out$note, -out$farms), , drop = FALSE]
}

batch_root <- function(user_folder) file.path(user_folder, "batches")

# Name of the batch database, also when global.R is not loaded
batch_long_list_name <- function() {
  if (exists("batch_database_name", inherits = TRUE)) get("batch_database_name", inherits = TRUE) else "Batch parameters (all regions)"
}
# (earlier names of the batch database, still used by older batches and copies)
batch_old_database_names <- c("Long list - all regions")

# The batch database holds what things are (livestock types, feeds, crops and
# the reference lists) and the batch defaults: the values suggested for what the
# farms left blank. Each user gets an own copy, "My batch parameters", made on
# first use and selected on the Batch page: the feeds added while matching
# farmers' answers, and the answers matched, are kept in it.
batch_own_database_name <- "My batch parameters"
batch_ensure_database <- function(user_folder) {
  dbs <- file.path(user_folder, "parameters_database")
  mine <- file.path(dbs, batch_own_database_name)
  if (dir.exists(mine)) return(isTRUE(tryCatch(batch_update_own_database(user_folder), error = function(e) FALSE)))
  # an earlier own copy (with the feeds and answers added since) is the start
  earlier <- file.path(dbs, c("My batch defaults", batch_old_database_names, batch_long_list_name()))
  earlier <- earlier[dir.exists(earlier)]
  from <- if (length(earlier)) earlier[1] else file.path("data", "primary_database", batch_long_list_name())
  if (!dir.exists(from)) return(FALSE)
  dir.create(mine, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(from, full.names = TRUE), mine, overwrite = FALSE)
  try(batch_update_own_database(user_folder), silent = TRUE)
  batch_cleanup_old_databases(user_folder)
  TRUE
}

# Cow types of the batch database (November 2026): one type for dry cows and one
# for lactating cows - pregnant cows are in either (CLEANED counts pregnancy for
# every cow through the birth interval, and lactation through the farm's milk) -
# for local, improved and high productive cows, and steers/heifers (high
# productive). The dry and lactating types came with a body weight of 0 (their
# adult weight is used). Rows the user changed are left as they are.
batch_update_cow_types <- function(lt) {
  lt <- as.data.frame(lt, stringsAsFactors = FALSE, check.names = FALSE)
  lt[] <- lapply(lt, as.character)
  code <- trimws(lt$livetype_code)
  rename <- c("23" = "Cattle - Cows dry/pregnant (local)", "24" = "Cattle - Cows lactating/pregnant (local)",
              "26" = "Cattle - Cows dry/pregnant (improved)", "27" = "Cattle - Cows lactating/pregnant (improved)")
  old_names <- c("23" = "Cattle - Cows dry (local)", "24" = "Cattle - Cows lactating (local)",
                 "26" = "Cattle - Cows dry (improved)", "27" = "Cattle - Cows lactating (improved)")
  for (k in names(rename)) {
    i <- which(code == k & lt$livetype_desc == old_names[[k]])
    if (!length(i)) next
    lt$livetype_desc[i] <- rename[[k]]
    if (suppressWarnings(as.numeric(lt$body_weight[i])) %in% 0) lt$body_weight[i] <- lt$adult_weight[i]
  }
  # pregnant-only types are removed: pregnant cows are counted with the dry or
  # the lactating cows (CLEANED counts pregnancy through the birth interval)
  lt <- lt[!(code %in% c("25", "28") & grepl("^Cattle - Cows pregnant \\(", lt$livetype_desc)), , drop = FALSE]
  code <- trimws(lt$livetype_code)
  add <-function(new_code, from_desc, desc) {
    if (new_code %in% code || desc %in% lt$livetype_desc) return(lt)
    src <- lt[lt$livetype_desc == from_desc, , drop = FALSE]
    if (!nrow(src)) return(lt)
    row <- src[1, , drop = FALSE]; row$livetype_code <- new_code; row$livetype_desc <- desc
    rbind(lt, row)
  }
  lt <- add("29", "Cattle - Cows (high productive)", "Cattle - Cows dry/pregnant (high productive)")
  lt <- add("30", "Cattle - Cows (high productive)", "Cattle - Cows lactating/pregnant (high productive)")
  lt <- add("31", "Cattle - Steers/heifers (improved)", "Cattle - Steers/heifers (high productive)")
  # the general cow types are removed too: every cow is dry or lactating, so the
  # dry/pregnant and lactating/pregnant types cover them (batches are for new
  # surveys; single-farm databases keep them)
  general <- c("1" = "Cattle - Cows (local)", "2" = "Cattle - Cows (improved)", "3" = "Cattle - Cows (high productive)")
  drop <- trimws(lt$livetype_code) %in% names(general) & lt$livetype_desc %in% general
  if (any(drop) && all(c("29", "30") %in% trimws(lt$livetype_code))) lt <- lt[!drop, , drop = FALSE]
  lt <- batch_renumber_livetypes(lt)
  lt[order(suppressWarnings(as.numeric(lt$livetype_code))), , drop = FALSE]
}

# Codes of the batch livestock types, numbered from 1 and grouped by species
# (cattle cows first). Types the user added keep their code, unless it is now
# taken: they then get the next free code.
batch_livetype_order <- c(
  "Cattle - Cows dry/pregnant (local)", "Cattle - Cows lactating/pregnant (local)",
  "Cattle - Cows dry/pregnant (improved)", "Cattle - Cows lactating/pregnant (improved)",
  "Cattle - Cows dry/pregnant (high productive)", "Cattle - Cows lactating/pregnant (high productive)",
  "Cattle - Adult male",
  "Cattle - Steers/heifers", "Cattle - Steers/heifers (improved)", "Cattle - Steers/heifers (high productive)",
  "Cattle - Calves", "Cattle - Calves (improved)",
  "Buffalo - Cows", "Buffalo - Steers/heifers", "Buffalo - Calves",
  "Sheep - Ewes", "Sheep - Breeding Rams", "Sheep - Fattening Rams", "Sheep - Lambs",
  "Goats - Does", "Goats - Breeding Bucks", "Goats - Fattening Bucks", "Goats - Kids",
  "Pigs - lactating/pregnant sows", "Pigs - dry sows/boars", "Pigs - growers"
)
batch_renumber_livetypes <- function(lt) {
  code <- trimws(as.character(lt$livetype_code))
  target <- as.character(match(lt$livetype_desc, batch_livetype_order))
  builtin <- !is.na(target)
  # (already numbered, or not the batch list: left as it is)
  if (!any(builtin) || all(code[builtin] == target[builtin])) return(lt)
  new <- code
  new[builtin] <- target[builtin]
  own <- which(!builtin)
  taken <- target[builtin]
  nxt <- length(batch_livetype_order)
  for (i in own) {
    if (new[i] %in% taken) { repeat { nxt <- nxt + 1; if (!as.character(nxt) %in% c(taken, code[own])) break }; new[i] <- as.character(nxt) }
    taken <- c(taken, new[i])
  }
  lt$livetype_code <- new
  lt
}
batch_update_own_database <- function(user_folder) {
  f <- file.path(user_folder, "parameters_database", batch_own_database_name, "lkp_livetype.csv")
  if (!file.exists(f)) return(FALSE)
  lt <- utils::read.csv(f, colClasses = "character", check.names = FALSE, na.strings = character(0), fileEncoding = "UTF-8-BOM")
  new <- batch_update_cow_types(lt)
  if (identical(new$livetype_code, lt$livetype_code) && identical(new$livetype_desc, lt$livetype_desc) && identical(new$body_weight, lt$body_weight)) return(FALSE)
  data.table::fwrite(new, f)
  TRUE
}

# Earlier copies of the batch database ("My batch defaults", "Long list - all
# regions") are removed once "My batch parameters" exists: their feeds and
# answers were copied into it, and each batch run keeps its own copy of the
# database it used. Returns TRUE when something was removed.
batch_cleanup_old_databases <- function(user_folder) {
  dbs <- file.path(user_folder, "parameters_database")
  if (!dir.exists(file.path(dbs, batch_own_database_name))) return(FALSE)
  old <- file.path(dbs, c("My batch defaults", batch_old_database_names))
  old <- old[dir.exists(old)]
  # (only batch databases are removed)
  old <- old[file.exists(file.path(old, "batch_database.json"))]
  if (!length(old)) return(FALSE)
  unlink(old, recursive = TRUE)
  TRUE
}

# Databases the Batch page offers: the user's batch databases and the built-in one
batch_databases <- function(user_folder) {
  user_dbs <- list.files(file.path(user_folder, "parameters_database"))
  mine <- user_dbs[vapply(file.path(user_folder, "parameters_database", user_dbs), cleaned::is_batch_database, TRUE)]
  mine <- setdiff(mine, c("My batch defaults", batch_old_database_names))
  unique(c(intersect(batch_own_database_name, mine), sort(setdiff(mine, batch_own_database_name)), batch_long_list_name()))
}

# Path of a parameters database of the user (falls back to the built-in copy)
batch_database_path <- function(user_folder, name) {
  p <- file.path(user_folder, "parameters_database", name)
  if (dir.exists(p)) return(p)
  # earlier copies of the batch database (removed once their content was copied
  # into "My batch parameters"): batches made with them use that copy
  if (name %in% c("My batch defaults", batch_old_database_names)) {
    own <- file.path(user_folder, "parameters_database", batch_own_database_name)
    if (dir.exists(own)) return(own)
    name <- batch_long_list_name()
  }
  file.path("data", "primary_database", name)
}

# Does the database hold batch defaults (lkp_defaults etc.)?
batch_database_has_defaults <- function(path) {
  file.exists(file.path(path, "lkp_defaults.csv"))
}
batch_default_tables <- c("lkp_defaults.csv", "lkp_feeditem_defaults.csv", "lkp_crops_defaults.csv")

batch_meta_read <- function(batch_dir) {
  f <- file.path(batch_dir, "batch.json")
  if (!file.exists(f)) return(NULL)
  meta <- tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
  if (is.null(meta)) return(NULL)
  meta$dir <- batch_dir
  # A running batch whose progress stopped long ago was interrupted
  if (identical(meta$status, "running")) {
    p <- file.path(batch_dir, "progress.txt")
    last <- if (file.exists(p)) file.mtime(p) else as.POSIXct(meta$started %||% meta$created)
    if (as.numeric(difftime(Sys.time(), last, units = "secs")) > batch_stall_seconds) {
      meta$status <- "interrupted"
    }
  }
  meta
}

batch_meta_write <- function(batch_dir, ...) {
  f <- file.path(batch_dir, "batch.json")
  meta <- if (file.exists(f)) jsonlite::fromJSON(f) else list()
  new <- list(...)
  meta[names(new)] <- new
  meta$dir <- NULL
  jsonlite::write_json(meta, f, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(meta)
}

# All batches of a user, newest first
batch_list <- function(user_folder) {
  dirs <- list.dirs(batch_root(user_folder), recursive = FALSE)
  metas <- Filter(Negate(is.null), lapply(dirs, batch_meta_read))
  if (!length(metas)) return(list())
  metas[order(vapply(metas, function(m) m$created %||% "", ""), decreasing = TRUE)]
}

# Folder name for a new batch
batch_new_id <- function(name) {
  paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), "_", substr(gsub("[^A-Za-z0-9_-]+", "_", name), 1, 40))
}

batch_progress_read <- function(batch_dir) {
  p <- file.path(batch_dir, "progress.txt")
  if (!file.exists(p)) return(NULL)
  line <- tryCatch(readLines(p, n = 1, warn = FALSE), error = function(e) character(0))
  if (!length(line)) return(NULL)
  parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
  if (length(parts) < 4) return(NULL)
  list(i = as.integer(parts[1]), n = as.integer(parts[2]),
       time = as.POSIXct(as.numeric(parts[3]), origin = "1970-01-01"), farm = parts[4])
}

# Leave out the rows of some livestock types before checking or building
batch_exclude_livetypes <- function(input, codes) {
  if (!length(codes)) return(input)
  for (sh in c("livestock", "feed_basket")) {
    x <- input[[sh]]
    if (nrow(x) > 0 && "livetype_code" %in% names(x)) {
      input[[sh]] <- x[!x$livetype_code %in% codes, , drop = FALSE]
    }
  }
  input
}

# What the file contains, in words people use
batch_file_overview <- function(input) {
  farm <- input$farm
  fb <- input$feed_basket
  feed_cols <- grep("^[0-9]+$", names(fb), value = TRUE)
  used <- feed_cols[vapply(feed_cols, function(f) any(suppressWarnings(as.numeric(fb[[f]])) > 0, na.rm = TRUE), TRUE)]
  list(
    farms = sum(!is.na(farm$Ids %||% NA)),
    livestock_rows = nrow(input$livestock),
    livestock_types = length(unique(input$livestock$livetype_code %||% character(0))),
    feeds_used = length(used),
    own_seasons = length(unique(input$season$Ids %||% character(0))),
    feed_overrides = nrow(input$feed_items) + nrow(input$feed_type),
    fertilizer_rows = nrow(input$fertilizer),
    groups = sub("^group_", "", Filter(function(g) any(!is.na(farm[[g]])), grep("^group_", names(farm), value = TRUE)))
  )
}

# Background processes started by this app (kept so they can be checked)
batch_processes <- new.env()

# Start a batch in a separate R process, so the app stays responsive and the
# batch goes on if the browser is closed. The process runs a small script
# saved in the batch folder. Returns the process id.
# Copy of the database tables (and the batch marker) in the batch folder
batch_save_database <- function(db_path, batch_dir) {
  copy <- file.path(batch_dir, "parameters_database")
  unlink(copy, recursive = TRUE)
  dir.create(copy, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(db_path, pattern = "^lkp_.*\\.csv$|^batch_database\\.json$", full.names = TRUE)
  file.copy(files, copy, overwrite = TRUE)
  copy
}

# Project values of a batch ------------------------------------------------------
# The values used where the farms left something blank are the _p sheets of the
# batch's input.xlsx. They start from a source - the uploaded file's own _p
# sheets ("file"), the batch defaults of the database ("defaults") or an earlier
# batch (its folder name) - plus the changes made in the app (project_edits.rds),
# which are kept when the source is changed.
batch_has_project_api <- function() {
  all(c("batch_default_project", "read_batch_project", "write_batch_project", "batch_missing_values") %in%
        getNamespaceExports("cleaned"))
}

batch_project_sources <- function(user_folder, current_dir, has_file) {
  out <- c(if (has_file) c("Your file" = "file"), "Batch defaults" = "defaults")
  for (b in batch_list(user_folder)) {
    if (identical(normalizePath(b$dir, mustWork = FALSE), normalizePath(current_dir %||% "", mustWork = FALSE))) next
    if (!file.exists(file.path(b$dir, "input.xlsx"))) next
    label <- sprintf("Batch '%s' (%s)", b$name %||% basename(b$dir), substr(b$created %||% "", 1, 10))
    out <- c(out, stats::setNames(basename(b$dir), label))
  }
  out
}

batch_project_base <- function(dir, source, db_path, user_folder) {
  from_file <- if (file.exists(file.path(dir, "project_file.rds"))) readRDS(file.path(dir, "project_file.rds")) else NULL
  if (identical(source, "file") && !is.null(from_file)) return(from_file)
  if (!is.null(source) && !source %in% c("file", "defaults")) {
    other <- file.path(batch_root(user_folder), source, "input.xlsx")
    p <- if (file.exists(other)) tryCatch(cleaned::read_batch_project(other), error = function(e) NULL) else NULL
    if (!is.null(p)) return(p)
  }
  cleaned::batch_default_project(db_path)
}

# Change one value of the project. key: livestock type, feed, crop or fertiliser;
# for farm_p "" (all farms) or "group_x=value"; for season_p the value is
# "Name days; Name days"
batch_project_set <- function(project, sheet, key, field, value) {
  x <- project[[sheet]]
  if (is.null(x)) x <- data.frame(check.names = FALSE)
  x$.row <- NULL
  value <- if (is.null(value) || is.na(value) || !nzchar(trimws(value))) NA_character_ else trimws(as.character(value))
  ensure <- function(x, cols) { for (cl in setdiff(cols, names(x))) x[[cl]] <- rep(NA_character_, nrow(x)); x }
  add_row <- function(x) { x[nrow(x) + 1, ] <- NA; x }
  if (sheet == "season_p") {
    parts <- trimws(gsub("[()]", " ", strsplit(value %||% "", "[;,]")[[1]])); parts <- parts[nzchar(parts)]
    x <- data.frame(season_name = trimws(sub("\\s+[0-9.]+\\s*(days)?$", "", parts)),
                    season_length = sub("^.*?([0-9.]+)\\s*(days)?$", "\\1", parts), stringsAsFactors = FALSE)
  } else if (sheet == "farm_p") {
    g <- if (nzchar(key)) strsplit(strsplit(key, "&", fixed = TRUE)[[1]], "=", fixed = TRUE) else list()
    gcols <- vapply(g, `[`, "", 1); gvals <- vapply(g, `[`, "", 2)
    x <- ensure(x, c(gcols, field))
    all_g <- grep("^group_", names(x), value = TRUE)
    hit <- rep(TRUE, nrow(x))
    for (gc in all_g) {
      want <- if (gc %in% gcols) gvals[gcols == gc] else NA
      hit <- hit & (if (is.na(want)) is.na(x[[gc]]) else !is.na(x[[gc]]) & x[[gc]] == want)
    }
    j <- which(hit)
    if (!length(j)) { x <- add_row(x); j <- nrow(x); for (k in seq_along(gcols)) x[[gcols[k]]][j] <- gvals[k] }
    x[[field]][j[1]] <- value
  } else {
    keycol <- c(livestock_p = "livetype_code", feed_items_p = "feed_item_code", feed_type_p = "crop_code",
                fertilizer_p = "fertilizer_desc")[[sheet]]
    x <- ensure(x, c(keycol, field))
    j <- which(as.character(x[[keycol]]) == key)
    if (!length(j)) { x <- add_row(x); j <- nrow(x); x[[keycol]][j] <- key }
    x[[field]][j[1]] <- value
  }
  project[[sheet]] <- x
  project
}

# Rebuild the _p sheets of a batch's input.xlsx from its source and the changes made in the app
batch_project_rebuild <- function(dir, db_path, user_folder, source = NULL) {
  if (!batch_has_project_api() || !file.exists(file.path(dir, "input.xlsx"))) return(invisible(NULL))
  source <- source %||% batch_meta_read(dir)$project_source %||% "defaults"
  project <- batch_project_base(dir, source, db_path, user_folder)
  edits <- if (file.exists(file.path(dir, "project_edits.rds"))) readRDS(file.path(dir, "project_edits.rds")) else NULL
  for (i in seq_len(NROW(edits))) {
    project <- batch_project_set(project, edits$sheet[i], edits$key[i], edits$field[i], edits$value[i])
  }
  cleaned::write_batch_project(file.path(dir, "input.xlsx"), project)
  batch_meta_write(dir, project_source = source)
  invisible(project)
}

batch_project_edit <- function(dir, sheet, key, field, value) {
  f <- file.path(dir, "project_edits.rds")
  edits <- if (file.exists(f)) readRDS(f) else data.frame(sheet = character(0), key = character(0), field = character(0),
                                                         value = character(0), stringsAsFactors = FALSE)
  edits <- edits[!(edits$sheet == sheet & edits$key == key & edits$field == field), , drop = FALSE]
  edits <- rbind(edits, data.frame(sheet = sheet, key = key, field = field, value = value %||% NA_character_, stringsAsFactors = FALSE))
  saveRDS(edits, f)
}

batch_start <- function(batch_dir, db_path, db_name, exclude_livetypes, primary_excel = NULL) {
  batch_dir <- normalizePath(batch_dir)
  batch_meta_write(batch_dir, status = "running", started = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                   finished = NULL, message = NULL)
  unlink(file.path(batch_dir, "progress.txt"))
  # The batch is run with a copy of the parameters database saved in the batch
  # folder: its results stay linked to the values they were calculated with,
  # even if the database is edited or deleted later
  # (the values used for what the farms left blank are in the _p sheets of input.xlsx)
  db_copy <- batch_save_database(db_path, batch_dir)
  batch_meta_write(batch_dir, database_copy = TRUE)
  saveRDS(list(batch_dir = batch_dir, db_path = normalizePath(db_copy), db_name = db_name,
               exclude_livetypes = exclude_livetypes,
               primary_excel = if (is.null(primary_excel)) NULL else normalizePath(primary_excel),
               lib_paths = .libPaths()),
          file.path(batch_dir, "run_settings.rds"))
  writeLines(batch_run_script, file.path(batch_dir, "run.R"))
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  proc <- processx::process$new(
    rscript, c("--vanilla", file.path(batch_dir, "run.R"), batch_dir),
    stdout = file.path(batch_dir, "run.log"), stderr = "2>&1",
    cleanup = FALSE, windows_hide_window = TRUE
  )
  assign(batch_dir, proc, envir = batch_processes)
  batch_meta_write(batch_dir, pid = proc$get_pid())
  proc$get_pid()
}

# Script run in the background process (argument: the batch folder)
batch_run_script <- c(
  "batch_dir <- commandArgs(trailingOnly = TRUE)[1]",
  "s <- readRDS(file.path(batch_dir, 'run_settings.rds'))",
  ".libPaths(s$lib_paths)",
  "write_meta <- function(...) {",
  "  f <- file.path(batch_dir, 'batch.json'); meta <- jsonlite::fromJSON(f); new <- list(...)",
  "  meta[names(new)] <- new",
  "  jsonlite::write_json(meta, f, auto_unbox = TRUE, pretty = TRUE, null = 'null')",
  "}",
  "progress_file <- file.path(batch_dir, 'progress.txt')",
  "res <- tryCatch({",
  "  suppressPackageStartupMessages({ library(cleaned); library(dplyr); library(tidyr) })",
  "  cleaned::run_batch(",
  "    file.path(batch_dir, 'input.xlsx'), file.path(batch_dir, 'output'),",
  "    parameter_database = s$db_path, database_name = s$db_name,",
  "    exclude_livetypes = s$exclude_livetypes, primary_excel = s$primary_excel,",
  "    progress = function(i, n, farm) {",
  "      tmp <- paste0(progress_file, '.tmp')",
  "      writeLines(paste(i, n, as.numeric(Sys.time()), farm, sep = '\\t'), tmp)",
  "      file.rename(tmp, progress_file)",
  "    })",
  "}, error = function(e) e)",
  "now <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')",
  "if (inherits(res, 'error')) {",
  "  write_meta(status = 'failed', finished = now, message = conditionMessage(res))",
  "} else {",
  "  write_meta(status = 'done', finished = now,",
  "             farms_ok = sum(res$log$status == 'ok'), farms_failed = sum(res$log$status == 'failed'),",
  "             farms_with_warnings = sum(res$log$status == 'ok' & !is.na(res$log$warnings)))",
  "}"
)

# A batch marked as running whose process has ended without saying so
batch_check_process <- function(batch_dir) {
  batch_dir <- normalizePath(batch_dir, mustWork = FALSE)
  if (!exists(batch_dir, envir = batch_processes, inherits = FALSE)) return(invisible())
  proc <- get(batch_dir, envir = batch_processes)
  if (proc$is_alive()) return(invisible())
  rm(list = batch_dir, envir = batch_processes)
  meta <- batch_meta_read(batch_dir)
  if (identical(meta$status, "running")) {
    log <- tryCatch(readLines(file.path(batch_dir, "run.log"), warn = FALSE), error = function(e) character(0))
    batch_meta_write(batch_dir, status = "failed", finished = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                     message = paste(c("The calculation process stopped unexpectedly.", utils::tail(log, 3)), collapse = " "))
  }
  invisible()
}

batch_cancel <- function(batch_dir) {
  meta <- batch_meta_read(batch_dir)
  if (!is.null(meta$pid)) try(tools::pskill(meta$pid), silent = TRUE)
  batch_meta_write(batch_dir, status = "cancelled", finished = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

# Validation messages in the words of the template (no database table names)
batch_friendly_message <- function(x) {
  x <- gsub("\\(see lkp_[a-z_]+\\.[a-z_]+\\)", "(use the dropdown in the template, or the list in its ref_ sheets)", x)
  x <- gsub(" \\(lkp_[a-z]+\\)", "", x)
  x <- gsub("lkp_defaults(, section [a-z]+)?", "the database defaults", x)
  x <- gsub("lkp_feeditem_defaults", "the database feed defaults", x)
  x <- gsub("lkp_crops_defaults", "the database crop defaults", x)
  x <- gsub("parameter database \\(the database defaults\\)", "database defaults", x)
  x <- gsub("lkp_[a-z_]+", "the parameters database", x)
  x
}

# "2 min 5 s"
batch_format_duration <- function(seconds) {
  if (is.na(seconds) || !is.finite(seconds)) return("")
  seconds <- round(seconds)
  if (seconds < 60) return(paste(seconds, "s"))
  m <- seconds %/% 60
  if (m < 60) return(sprintf("%d min %02d s", m, seconds %% 60))
  sprintf("%d h %02d min", m %/% 60, m %% 60)
}

# Plain-language status of a batch
batch_status_label <- function(status) {
  switch(status %||% "",
         checked = "Ready to run", needs_fixes = "Needs fixes", needs_matching = "Answers to match", running = "Running", done = "Finished",
         failed = "Stopped with an error", cancelled = "Cancelled",
         interrupted = "Interrupted", "Not run yet")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Library of the feeds and crops of all default parameter databases, used to
# suggest feeds that the selected database does not have. Built once and kept
# until a default database changes.
batch_library_cache <- new.env()
batch_feed_library <- function(folder = file.path("data", "primary_database")) {
  dirs <- list.dirs(folder, recursive = FALSE)
  # the regional databases (the batch long list is made from them)
  dirs <- dirs[!vapply(dirs, cleaned::is_batch_database, TRUE)]
  files <- file.path(dirs, c("lkp_feeditem.csv"))
  stamp <- paste(c(dirs, as.character(file.mtime(files))), collapse = "|")
  if (!identical(batch_library_cache$stamp, stamp)) {
    batch_library_cache$library <- tryCatch(cleaned::build_feed_library(dirs), error = function(e) NULL)
    batch_library_cache$stamp <- stamp
  }
  batch_library_cache$library
}
