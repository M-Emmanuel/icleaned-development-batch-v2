# Batch processing page: server ------------------------------------------------------

batch_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    user_folder <- reactive({ req(session$userData$user_folder); session$userData$user_folder })

    # Show the reason when a download cannot be made (instead of a failed download)
    batch_safely <- function(what, expr) {
      tryCatch(expr, error = function(e) {
        message(what, ": ", conditionMessage(e))
        showNotification(paste0(what, ": ", conditionMessage(e)), type = "error", duration = NULL)
        stop(e)
      })
    }

    # Batch processing needs the CLEANED version that comes with this app
    missing_fns <- setdiff(c("write_batch_template", "validate_batch_input", "run_batch",
                             "write_odk_form", "read_odk_export"),
                           getNamespaceExports("cleaned"))
    if (length(missing_fns)) {
      showNotification(sprintf(
        "The installed CLEANED package (version %s) is too old for batch processing. Install the CLEANED version that comes with this app and restart it.",
        as.character(utils::packageVersion("cleaned"))), type = "error", duration = NULL)
    }

    current <- reactiveVal(NULL)          # folder of the batch on screen
    history_tick <- reactiveVal(0)        # bump to refresh the batch list
    meta <- reactiveVal(NULL)             # batch.json of the current batch
    watching <- reactiveVal(character(0)) # batches started in this session (to notify)

    refresh_meta <- function() {
      m <- if (is.null(current())) NULL else batch_meta_read(current())
      meta(m)
      history_tick(history_tick() + 1)
    }

    # ------ Databases -------------------------------------------------------------
    db_tick <- reactiveVal(0)  # bumped when batch defaults are added to a database
    # The long list of all regions is the database of batch processing: the user
    # works on an own copy of it (made here on first use)
    observe({
      if (isTRUE(tryCatch(batch_ensure_database(user_folder()), error = function(e) FALSE)) &&
          !is.null(session$userData$databases)) {
        session$userData$databases(list.files(file.path(user_folder(), "parameters_database")))
      }
    })
    observe({
      db_tick()
      session$userData$databases()
      dbs <- batch_databases(user_folder())
      # a batch made before the long list keeps its own database while open
      m_db <- meta()$database
      if (!is.null(m_db) && !m_db %in% dbs && dir.exists(batch_database_path(user_folder(), m_db))) dbs <- c(dbs, m_db)
      selected <- if (!is.null(m_db) && m_db %in% dbs) m_db else isolate(input$database)
      if (is.null(selected) || !selected %in% dbs) {
        selected <- intersect(c(batch_own_database_name, batch_long_list_name()), dbs)[1] %||% dbs[1]
        if (is.na(selected)) selected <- dbs[1]
      }
      # (a database that is not a batch database is only offered for the earlier batch that used it)
      labels <- ifelse(dbs %in% batch_databases(user_folder()), dbs, paste0(dbs, " (used by this earlier batch)"))
      updateSelectInput(session, "database", choices = stats::setNames(dbs, labels), selected = selected)
    })
    db_path <- reactive({ req(input$database); batch_database_path(user_folder(), input$database) })
    db <- reactive({ db_tick(); cleaned::read_parameter_database(db_path()) })

    output$database_note <- renderUI({
      db_tick()
      path <- db_path()
      if (batch_database_has_defaults(path)) return(NULL)
      primary <- file.path("data", "primary_database", input$database)
      can_copy <- !identical(normalizePath(path, mustWork = FALSE), normalizePath(primary, mustWork = FALSE)) &&
        batch_database_has_defaults(primary)
      div(
        class = "alert alert-warning batch-alert mt-2", role = "status",
        icon("circle-info"), strong(" This database has no batch defaults."),
        " Every required value then has to be in your file. ",
        if (can_copy) tagList(
          "The built-in version of this database has them; you can add them to your copy.",
          div(actionButton(ns("copy_defaults"), "Add the built-in batch defaults", class = "btn-sm btn-outline-primary mt-2"))
        )
      )
    })
    observeEvent(input$copy_defaults, {
      primary <- file.path("data", "primary_database", input$database)
      file.copy(file.path(primary, batch_default_tables), db_path(), overwrite = FALSE)
      db_tick(db_tick() + 1)
      showNotification("Batch defaults added to your database.", type = "message")
    })

    output$download_odk <- downloadHandler(
      filename = function() "iCLEANED farmer survey.xlsx",
      content = function(file) batch_safely("The ODK form could not be created", {
        groups <- trimws(strsplit(input$odk_groups %||% "", ",")[[1]])
        groups <- gsub("[^A-Za-z0-9_]+", "_", groups[nzchar(groups)])
        cleaned::write_odk_form(db_path(), file, database_name = input$database, groups = groups)
      })
    )

    output$download_template <- downloadHandler(
      filename = function() paste0("iCLEANED batch template - ", input$database, ".xlsx"),
      content = function(file) batch_safely("The template could not be created", {
        cleaned::write_batch_template(db_path(), file, database_name = input$database)
      })
    )

    # ------ Upload and check --------------------------------------------------------
    # Check input.xlsx of a batch and record the result -------------------------
    check_batch_file <- function(dir, name, file_name, from_odk, database) {
      path <- batch_database_path(user_folder(), database)
      report <- tryCatch(
        withProgress(message = "Checking your data...", value = 0.5, {
          cleaned::validate_batch_input(file.path(dir, "input.xlsx"), path)
        }),
        error = function(e) e
      )
      if (inherits(report, "error")) {
        showNotification(paste("This file could not be read:", conditionMessage(report),
                               "Please use the Excel template from step 1."), type = "error", duration = NULL)
        return(FALSE)
      }
      saveRDS(report, file.path(dir, "validation.rds"))
      # What's missing: the values left blank and what is used for them
      mv <- if (batch_has_project_api()) tryCatch(cleaned::batch_missing_values(file.path(dir, "input.xlsx"), path),
                                                  error = function(e) NULL) else NULL
      saveRDS(mv, file.path(dir, "missing.rds"))
      missing_tick(isolate(missing_tick()) + 1)
      overview <- batch_file_overview(cleaned::read_batch_input(file.path(dir, "input.xlsx")))
      batch_meta_write(
        dir, name = name, file_name = file_name, from_odk = from_odk, database = database,
        created = batch_meta_read(dir)$created %||% format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        status = if (attr(report, "summary")[["errors"]] > 0) "needs_fixes" else "checked",
        farms = overview$farms, errors = attr(report, "summary")[["errors"]],
        warnings = attr(report, "summary")[["warnings"]], overview = overview
      )
      TRUE
    }

    # Read the ODK export of a batch; with all answers matched, save input.xlsx
    convert_odk <- function(dir, database, matches = NULL) {
      path <- batch_database_path(user_folder(), database)
      export <- list.files(dir, pattern = "^odk_export\\.(zip|xlsx)$", full.names = TRUE)[1]
      r <- withProgress(message = "Reading the ODK submissions...", value = 0.4, {
        cleaned::read_odk_export(export, path, path, matches = matches, library = batch_feed_library())
      })
      # Bought feeds are counted with the land and emissions of producing them
      # off the farm through copies of the feeds tagged OFR / OFC / IP: add the
      # missing ones to the user's own copy of the database, then read again
      own_copy <- file.path(user_folder(), "parameters_database", database)
      if (!is.null(r$input) && !is.null(r$variants) && nrow(r$variants)) {
        if (dir.exists(own_copy)) {
          added <- tryCatch(cleaned::add_feed_variants(r$variants, own_copy), error = function(e) e)
          if (inherits(added, "error")) {
            showNotification(paste("The bought feeds could not be added to your database:", conditionMessage(added)),
                             type = "error", duration = NULL)
          } else {
            if (!is.null(added)) {
              showNotification(paste0("Added to your database '", database, "' for the feeds bought: ",
                                      paste0(added$name[added$kind == "feed"], " (feed ", added$code[added$kind == "feed"], ")", collapse = "; "), "."),
                               type = "message", duration = 15)
              db_tick(db_tick() + 1)
            }
            r <- withProgress(message = "Reading the ODK submissions...", value = 0.8, {
              cleaned::read_odk_export(export, own_copy, own_copy, matches = matches, library = batch_feed_library())
            })
          }
        } else {
          showNotification(paste("Feeds bought are counted without the land and emissions of producing them,",
                                 "because the tagged feeds (OFR / OFC / IP) can only be added to your own copy of the database.",
                                 "Clone the database in the parameters page and upload the file again."),
                           type = "warning", duration = NULL)
        }
      }
      saveRDS(r$bought, file.path(dir, "bought.rds"))
      saveRDS(r$excluded, file.path(dir, "excluded.rds"))
      saveRDS(r$renamed, file.path(dir, "renamed.rds"))
      saveRDS(r$to_match, file.path(dir, "to_match.rds"))
      if (!is.null(r$conversions)) saveRDS(r$conversions, file.path(dir, "conversions.rds"))
      saveRDS(r$assumed, file.path(dir, "assumed.rds"))
      if (!is.null(r$input)) {
        cleaned::write_batch_file(r$input, path, file.path(dir, "input.xlsx"), database_name = database)
        # project values: the batch defaults or the source chosen, with the changes made in the app
        tryCatch(batch_project_rebuild(dir, path, user_folder()), error = function(e) NULL)
      }
      r
    }

    observeEvent(input$file, {
      f <- input$file
      root <- batch_root(user_folder())
      dir.create(root, showWarnings = FALSE, recursive = TRUE)
      m <- meta()
      reuse <- !is.null(m) && m$status %in% c("checked", "needs_fixes", "needs_matching")
      # The batch is named after the uploaded file (the date and time of upload are
      # saved with it); names are kept unique so batches can be told apart
      name <- tools::file_path_sans_ext(f$name)
      others <- Filter(function(b) !(reuse && identical(normalizePath(b$dir, mustWork = FALSE),
                                                        normalizePath(current() %||% "", mustWork = FALSE))),
                       batch_list(user_folder()))
      taken <- vapply(others, function(b) b$name %||% "", "")
      base <- name; k <- 1
      while (name %in% taken) { k <- k + 1; name <- sprintf("%s (%d)", base, k) }
      dir <- if (reuse) current() else file.path(root, batch_new_id(name))
      dir.create(dir, showWarnings = FALSE)
      unlink(file.path(dir, c("input.xlsx", "validation.rds", "to_match.rds", "conversions.rds", "assumed.rds", "bought.rds", "excluded.rds", "odk_export.zip", "odk_export.xlsx",
                              "missing.rds", "project_file.rds", "project_edits.rds", "renamed.rds")))
      if (file.exists(file.path(dir, "batch.json"))) batch_meta_write(dir, project_source = NULL)
      # ODK Central .zip, or KoboToolbox .xlsx export (told apart from a batch template)
      from_odk <- grepl("\\.zip$", f$name, ignore.case = TRUE) || isTRUE(cleaned::is_odk_export(f$datapath))
      if (from_odk) {
        # ODK Central export: farmers' answers are matched first, then turned
        # into a batch Excel file
        file.copy(f$datapath, file.path(dir, paste0("odk_export.", tolower(tools::file_ext(f$name)))), overwrite = TRUE)
        r <- tryCatch(convert_odk(dir, input$database), error = function(e) e)
        if (inherits(r, "error")) {
          if (!reuse) unlink(dir, recursive = TRUE)
          showNotification(paste("The ODK export could not be read:", conditionMessage(r),
                                 "Upload the .zip from ODK Central's download (CSV files) or the KoboToolbox XLS export with 'XML values and headers',",
                                 "with the database the form was made from selected in step 1."),
                           type = "error", duration = NULL)
          return()
        }
        if (is.null(r$input)) {
          batch_meta_write(dir, name = name, file_name = f$name, from_odk = TRUE, database = input$database,
                           created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), status = "needs_matching",
                           farms = NULL, errors = NULL, warnings = NULL, overview = NULL)
          current(dir)
          refresh_meta()
          return()
        }
      } else {
        file.copy(f$datapath, file.path(dir, "input.xlsx"), overwrite = TRUE)
        # the file's own project values (_p sheets), if it has any
        if (batch_has_project_api()) {
          pf <- tryCatch(cleaned::read_batch_project(file.path(dir, "input.xlsx")), error = function(e) NULL)
          saveRDS(pf, file.path(dir, "project_file.rds"))
          tryCatch(batch_project_rebuild(dir, batch_database_path(user_folder(), input$database), user_folder(),
                                         source = if (is.null(pf)) "defaults" else "file"),
                   error = function(e) NULL)
        }
      }
      if (!check_batch_file(dir, name, f$name, from_odk, input$database)) {
        if (!reuse) unlink(dir, recursive = TRUE)
        return()
      }
      current(dir)
      refresh_meta()
    })

    # ------ What's missing: the values used for what the farms left blank ----------
    # They are the project values (_p sheets) of the batch's input.xlsx, which
    # start from the uploaded file, the batch defaults or an earlier batch.
    missing_tick <- reactiveVal(0)   # bumped when the values are recalculated
    missing_values <- reactive({
      m <- meta(); missing_tick()
      if (is.null(m) || !m$status %in% c("checked", "needs_fixes")) return(NULL)
      f <- file.path(m$dir, "missing.rds")
      if (file.exists(f)) readRDS(f) else NULL
    })
    missing_topics <- c(
      "Farm" = "Climate, soil, land management, inputs bought and losses along the value chain: for all farms, or per group.",
      "Seasons" = "The seasons of the farms that gave none.",
      "Livestock" = "What the farms do with each livestock type: milk, growth, time in each place, manure systems...",
      "Feeds" = "How each feed is grown: land cover, slope, fertiliser, cut and carry...",
      "Crops" = "How each crop is managed: share of the harvest and crop residues removed or burnt, grassland, rice...",
      "Fertiliser" = "The N content of each fertiliser (NPK has none: give it).",
      "Livestock types" = "The values of each livestock type (weight, milk and meat content...), the same for all farms."
    )
    missing_text <- function(v) ifelse(is.na(v) | !nzchar(v), "not set", v)
    missing_changed <- function(mv) {
      a <- trimws(ifelse(is.na(mv$value), "", mv$value)); b <- trimws(ifelse(is.na(mv$batch_default), "", mv$batch_default))
      na <- suppressWarnings(as.numeric(a)); nb <- suppressWarnings(as.numeric(b))
      !ifelse(!is.na(na) & !is.na(nb), abs(na - nb) < 1e-9, a == b)
    }

    output$missing_ui <- renderUI({
      m <- meta(); mv <- missing_values()
      if (is.null(m) || is.null(mv)) return(NULL)
      pf <- file.path(m$dir, "project_file.rds")
      has_file <- file.exists(pf) && !is.null(readRDS(pf))
      sources <- batch_project_sources(user_folder(), m$dir, has_file)
      src <- m$project_source %||% "defaults"
      changed <- missing_changed(mv)
      edit_link <- function(i) tags$a(
        href = "#", class = if (changed[i]) "batch-value batch-value-changed" else "batch-value",
        onclick = sprintf("Shiny.setInputValue('%s', {i: %d, n: Math.random()}); return false;", ns("missing_edit"), i),
        missing_text(mv$value[i]), " ", icon("pen", class = "small"))
      miss_table <- function(rows, show_item = FALSE) tags$table(
        class = "table table-sm batch-match-table batch-missing-table",
        tags$thead(tags$tr(if (show_item) tags$th("For"), tags$th("Value"), tags$th("Farms"),
                           tags$th("Used"), tags$th("Batch default"))),
        tags$tbody(lapply(rows, function(i) tags$tr(
          if (show_item) tags$td(mv$item[i]),
          tags$td(mv$label[i], br(), span(class = "batch-hint", mv$field[i])),
          tags$td(mv$farms[i]),
          tags$td(edit_link(i)),
          tags$td(span(class = "batch-hint", missing_text(mv$batch_default[i])),
                  if (changed[i]) span(class = "batch-chip batch-chip-done ms-1", "changed"))
        ))))
      count <- function(rows) {
        n_ch <- sum(changed[rows])
        sprintf("%d value%s%s", length(rows), if (length(rows) == 1) "" else "s", if (n_ch) sprintf(", %d changed", n_ch) else "")
      }
      div(
        class = "batch-missing mt-4",
        h3("What's missing"),
        p(class = "batch-hint", "Values some farms left blank, and what is used for them. Click a value to change it:",
          "the change applies to the farms that left it blank (a value given by a farm is always kept)."),
        div(class = "d-flex flex-wrap align-items-end gap-2",
            selectInput(ns("project_source"), "Suggested values from", choices = sources, selected = src,
                        width = "340px", selectize = FALSE),
            downloadButton(ns("download_input"), "Download as Excel", class = "btn-outline-primary btn-sm mb-3",
                           icon = icon("file-excel"))),
        p(class = "batch-hint",
          if (src == "file") "Your file's _p sheets give the values; blanks there use the batch defaults."
          else if (src == "defaults") "The batch defaults of the database give the values."
          else "The values of that earlier batch are used.",
          "Values you change here are kept if you switch, and are saved in the batch file (its _p sheets)."),
        if (!nrow(mv)) p(class = "batch-hint", icon("check"), " Nothing is missing: every farm gave every value.")
        else lapply(intersect(names(missing_topics), unique(mv$topic)), function(t) {
          rows <- which(mv$topic == t)
          body <- if (t %in% c("Farm", "Seasons", "Fertiliser")) miss_table(rows, show_item = t %in% c("Farm", "Fertiliser"))
          else lapply(unique(mv$item[rows]), function(it) {
            r <- rows[mv$item[rows] == it]
            tags$details(class = "batch-details ms-3 mt-1", tags$summary(sprintf("%s (%s)", it, count(r))), miss_table(r))
          })
          tags$details(class = "batch-details mt-2", open = if (t %in% c("Farm", "Seasons")) NA,
                       tags$summary(strong(t), sprintf(": %s", count(rows))),
                       p(class = "batch-hint mt-2", missing_topics[[t]]), body)
        })
      )
    })

    output$download_input <- downloadHandler(
      filename = function() paste0(meta()$name, " - batch file.xlsx"),
      content = function(file) file.copy(file.path(meta()$dir, "input.xlsx"), file, overwrite = TRUE)
    )

    missing_row <- reactiveVal(NULL)
    missing_spec <- function(r) {
      sh <- c("Farm" = "farm", "Seasons" = "season", "Livestock" = "livestock", "Livestock types" = "livestock",
              "Feeds" = "feed_items", "Crops" = "feed_type", "Fertiliser" = "fertilizer")[[r$topic]]
      d <- cleaned::batch_dictionary()
      d[d$sheet == sh & d$field == r$field, , drop = FALSE][1, ]
    }
    observeEvent(input$missing_edit, {
      mv <- missing_values(); i <- input$missing_edit$i
      req(mv, length(i), i >= 1, i <= nrow(mv))
      r <- mv[i, ]; missing_row(r)
      spec <- missing_spec(r)
      choices <- tryCatch(cleaned:::batch_allowed(spec, match_db()), error = function(e) NULL)
      if (r$field %in% "intercrop") choices <- c("Yes", "No")
      unit <- if (!is.na(spec$unit) && nzchar(spec$unit)) paste0(" (", spec$unit, ")") else ""
      value_input <- if (r$topic == "Seasons") {
        textInput(ns("missing_value"), "Seasons: name and number of days, separated by ';' (e.g. Wet 200; Dry 165)",
                  value = gsub("\\s*\\(([0-9.]+) days\\)", " \\1", ifelse(is.na(r$value), "", r$value)), width = "100%")
      } else if (length(choices)) {
        selectInput(ns("missing_value"), paste0("Value", unit), choices = unique(c(sort(choices), r$value[!is.na(r$value)])),
                    selected = r$value, width = "100%", selectize = FALSE)
      } else {
        textInput(ns("missing_value"), paste0("Value", unit), value = ifelse(is.na(r$value), "", r$value), width = "100%")
      }
      target <- if (r$topic == "Farm") {
        farm <- tryCatch(cleaned::read_batch_input(file.path(meta()$dir, "input.xlsx"))$farm, error = function(e) NULL)
        targets <- c("All farms" = "")
        for (g in grep("^group_", names(farm), value = TRUE)) {
          vals <- sort(unique(farm[[g]][!is.na(farm[[g]])]))
          if (length(vals)) targets <- c(targets, stats::setNames(paste0(g, "=", vals), paste0(sub("^group_", "", g), ": ", vals)))
        }
        if (length(targets) > 1) selectInput(ns("missing_target"), "Apply to", choices = targets, selected = r$key,
                                             width = "100%", selectize = FALSE)
      }
      showModal(modalDialog(
        title = paste0(r$label, if (!r$topic %in% c("Farm", "Seasons")) paste0(" - ", r$item) else ""),
        p(class = "batch-hint", sprintf("%d farm%s left this blank. Batch default: %s.", r$farms,
                                        if (r$farms == 1) "" else "s", missing_text(r$batch_default))),
        target, value_input,
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("missing_reset"), "Use the batch default", class = "btn-outline-secondary"),
                         actionButton(ns("missing_save"), "Apply", class = "btn-primary")),
        easyClose = TRUE
      ))
    })
    save_missing <- function(value) {
      r <- missing_row(); m <- meta(); req(r, m)
      spec <- missing_spec(r)
      value <- trimws(value %||% "")
      if (nzchar(value) && identical(spec$type, "number") && is.na(suppressWarnings(as.numeric(value)))) {
        showNotification("Please enter a number.", type = "warning")
        return()
      }
      key <- if (r$topic == "Farm") (input$missing_target %||% r$key) else r$key
      batch_project_edit(m$dir, r$sheet, key, r$field, if (nzchar(value)) value else NA_character_)
      removeModal()
      withProgress(message = "Checking the data again...", value = 0.5, {
        batch_project_rebuild(m$dir, batch_database_path(user_folder(), m$database), user_folder())
        check_batch_file(m$dir, m$name, m$file_name, isTRUE(m$from_odk), m$database)
      })
      refresh_meta()
    }
    observeEvent(input$missing_save, save_missing(input$missing_value))
    observeEvent(input$missing_reset, save_missing(missing_row()$batch_default))

    observeEvent(input$project_source, {
      m <- meta(); req(m, input$project_source)
      if (identical(input$project_source, m$project_source %||% "defaults")) return()
      withProgress(message = "Taking the suggested values...", value = 0.5, {
        batch_project_rebuild(m$dir, batch_database_path(user_folder(), m$database), user_folder(),
                              source = input$project_source)
        check_batch_file(m$dir, m$name, m$file_name, isTRUE(m$from_odk), m$database)
      })
      refresh_meta()
      showNotification("Suggested values updated. The values you changed here are kept.", type = "message")
    }, ignoreInit = TRUE)

    # ------ Match farmers' answers (ODK) ----------------------------------------
    to_match <- reactive({
      m <- meta()
      if (is.null(m) || !isTRUE(m$from_odk)) return(NULL)
      f <- file.path(m$dir, "to_match.rds")
      if (file.exists(f)) readRDS(f) else NULL
    })
    match_db <- reactive({
      m <- meta(); req(m)
      cleaned::read_parameter_database(batch_database_path(user_folder(), m$database))
    })

    output$match_ui <- renderUI({
      m <- meta(); tm <- to_match()
      if (is.null(m) || is.null(tm) || !nrow(tm)) return(NULL)
      pending <- identical(m$status, "needs_matching")
      db_m <- match_db()
      feeds <- stats::setNames(as.character(db_m$lkp_feeditem$feed_item_code), db_m$lkp_feeditem$feed_item_name)
      feeds <- feeds[order(names(feeds))]
      crops <- stats::setNames(as.character(db_m$lkp_crops$crop_code), db_m$lkp_crops$crop_name)
      crops <- crops[order(names(crops))]
      # Feeds and crops of the other default databases (copied in when chosen)
      lib <- batch_feed_library()
      lib_feeds <- lib_crops <- character(0)
      if (!is.null(lib)) {
        lf <- lib$feeds[!lib$feeds$key %in% cleaned::batch_normalise_text(names(feeds)), , drop = FALSE]
        lf <- lf[order(lf$feed_item_name), , drop = FALSE]
        lib_feeds <- stats::setNames(paste0("lib:", lf$lib_id), paste0(lf$feed_item_name, "  (", lf$source_db, ")"))
        lc <- lib$crops[!lib$crops$key %in% cleaned::batch_normalise_text(names(crops)), , drop = FALSE]
        lc <- lc[order(lc$crop_name), , drop = FALSE]
        lib_crops <- stats::setNames(paste0("libcrop:", lc$lib_crop_id), paste0(lc$crop_name, "  (", lc$source_db, ")"))
      }
      systems <- sort(unique(trimws(db_m$lkp_manureman$manureman_desc)))
      chip <- function(src, score) switch(src,
        "same name" = span(class = "batch-chip batch-chip-done", "Same name"),
        "remembered" = span(class = "batch-chip batch-chip-done", "Matched before"),
        "confirmed" = span(class = "batch-chip batch-chip-done", "Confirmed"),
        "suggested" = span(class = "batch-chip batch-chip-warning", sprintf("Check: %d%% sure", round(100 * score))),
        "library" = span(class = "batch-chip batch-chip-warning", "From another database: check the values"),
        span(class = "batch-chip batch-chip-error", "No match found"))
      row <- function(i) {
        x <- tm[i, ]
        new_label <- sprintf("Add '%s' as a new %s (copy the values of another)", x$answer, x$kind)
        choices <- if (x$kind == "feed") {
          list("Choose a feed..." = "", "In this database" = as.list(feeds),
               "From the other databases (copied into yours)" = as.list(lib_feeds),
               "Other" = stats::setNames(list("__new__", "__skip__"), c(new_label, "Leave this feed out")))
        } else if (x$kind == "crop") {
          list("Choose a crop..." = "", "In this database" = as.list(crops),
               "From the other databases (copied into yours)" = as.list(lib_crops),
               "Other" = stats::setNames(list("__new__", "__skip__"), c(new_label, "Leave this crop out")))
        } else c("Choose a manure system..." = "", stats::setNames(systems, systems))
        copy_choices <- if (x$kind == "feed") {
          list("Copy the values of..." = "", "In this database" = as.list(feeds), "From the other databases" = as.list(lib_feeds))
        } else list("Copy the values of..." = "", "In this database" = as.list(crops), "From the other databases" = as.list(lib_crops))
        selected <- if (is.na(x$value)) "" else if (x$value == "" && x$kind %in% c("feed", "crop")) "__skip__" else x$value
        tags$tr(
          tags$td(strong(x$answer), br(), span(class = "batch-hint", switch(x$kind, feed = "feed", crop = "crop grown", "what happens to the manure"))),
          tags$td(x$farms),
          tags$td(
            selectInput(ns(paste0("match_", i)), NULL, choices = choices, selected = selected,
                        width = "100%", selectize = FALSE),
            if (x$kind != "manure") conditionalPanel(
              condition = sprintf("input['%s'] == '__new__'", ns(paste0("match_", i))),
              selectInput(ns(paste0("copy_", i)), NULL, choices = copy_choices, width = "100%", selectize = FALSE),
              span(class = "batch-hint", if (x$kind == "feed")
                sprintf("'%s' gets the values of the feed chosen here, and its own crop '%s' (a copy of that feed's crop), so the farmer's answers about the crop reach it. You can edit the values later in the parameters database.", x$answer, x$answer)
                else sprintf("'%s' gets the values of the crop chosen here, and a feed of the same name so that diets can use it. You can edit the values later in the parameters database.", x$answer))
            )
          ),
          tags$td(chip(x$source, x$score))
        )
      }
      n_check <- sum(!tm$source %in% c("same name", "remembered", "confirmed"))
      table <- div(
        class = if (pending) "batch-match" else "batch-match batch-match-done",
        tags$table(
          class = "table table-sm batch-match-table",
          tags$thead(tags$tr(tags$th("Farmers' answer"), tags$th("Farms"), tags$th("Matches"), tags$th(""))),
          tags$tbody(lapply(seq_len(nrow(tm)), row))
        ),
        actionButton(ns("confirm_matches"), if (pending) "Confirm the matches and check the data" else "Confirm the changes and check again",
                     class = "btn-primary", icon = icon("check"))
      )
      if (pending) {
        tagList(
          div(class = "alert alert-warning batch-alert mt-3", role = "status", icon("circle-info"),
              strong(sprintf(" Please confirm how %d of the farmers' answers match the database.", n_check)),
              " Each answer is listed once with the number of farms that gave it; the most likely match is already chosen.",
              " Answers you confirm are remembered for the next survey."),
          table
        )
      } else {
        tags$details(
          class = "batch-details mt-3 mb-2",
          tags$summary(sprintf("How the farmers' answers were matched (%d answers)", nrow(tm))),
          p(class = "batch-hint mt-2", "You can change a match and confirm again."),
          table
        )
      }
    })

    output$conversions_ui <- renderUI({
      m <- meta()
      if (is.null(m) || !isTRUE(m$from_odk) || identical(m$status, "needs_matching")) return(NULL)
      f <- file.path(m$dir, "conversions.rds")
      fa <- file.path(m$dir, "assumed.rds")
      assumed <- if (file.exists(fa)) readRDS(fa) else NULL
      fx <- file.path(m$dir, "excluded.rds")
      excluded <- if (file.exists(fx)) readRDS(fx) else NULL
      excluded_ui <- if (!is.null(excluded) && nrow(excluded)) div(
        class = "alert alert-info batch-alert mt-2", role = "status",
        icon("circle-info"),
        strong(sprintf(" %d interview%s left out: ", nrow(excluded), if (nrow(excluded) == 1) " was" else "s were")),
        paste0(excluded$Ids, " (", ifelse(is.na(excluded$interviewer), "", paste0(excluded$interviewer, ": ")),
               tolower(sub("\\.$", "", excluded$reason)), ")", collapse = "; ") |> paste0("."))
      # interviews given the same farm ID: told apart, not merged
      fr <- file.path(m$dir, "renamed.rds")
      renamed <- if (file.exists(fr)) readRDS(fr) else NULL
      renamed_ui <- if (!is.null(renamed) && nrow(renamed)) div(
        class = "alert alert-warning batch-alert mt-2", role = "status", icon("triangle-exclamation"),
        strong(sprintf(" %d interviews shared a farm ID", nrow(renamed))),
        " and were kept as separate farms, with new IDs: ",
        paste0(renamed$new_id, " (was ", renamed$original_id,
               ifelse(is.na(renamed$interviewer) | renamed$interviewer == "", "", paste0(", ", renamed$interviewer)), ")",
               collapse = "; "), ". ",
        "If two of them are the same farm interviewed twice, remove one in the downloaded batch file and upload it.")
      fb <- file.path(m$dir, "bought.rds")
      bought <- if (file.exists(fb)) readRDS(fb) else NULL
      bought_ui <- if (!is.null(bought) && nrow(bought)) {
        flagged_b <- sum(nzchar(bought$note))
        tags$details(
          class = "batch-details mt-2 mb-2", open = if (flagged_b > 0) NA else NULL,
          tags$summary(sprintf("Feeds bought (%d on %d farm%s%s)", nrow(bought), length(unique(bought$Ids)),
                               if (length(unique(bought$Ids)) == 1) "" else "s",
                               if (flagged_b) sprintf(", %d to check", flagged_b) else "")),
          p(class = "batch-hint mt-2", "The part of a feed that is bought is counted with the land, fertiliser and emissions of",
            "producing it off the farm, reported apart from the farm's own: bought roughage (OFR), bought concentrate produced",
            "in the country (OFC) and imported concentrate (IP). The amounts bought are compared with what the diets suggest",
            "(about 2.5% of body weight a day): a large gap often means a wrong amount or diet share."),
          tags$table(
            class = "table table-sm batch-match-table",
            tags$thead(tags$tr(tags$th("Farm"), tags$th("Feed"), tags$th("Counted as"), tags$th("Share bought"),
                               tags$th("kg per year"), tags$th("kg DM: bought / diets"))),
            tags$tbody(lapply(seq_len(nrow(bought)), function(i) tagList(tags$tr(
              tags$td(bought$Ids[i]),
              tags$td(bought$feed[i], br(), span(class = "batch-hint", bought$matched_feed[i] %||% "")),
              tags$td(bought$counted_as[i], if (nzchar(bought$origin[i])) tagList(br(), span(class = "batch-hint", bought$origin[i]))),
              tags$td(if (is.na(bought$share_bought[i])) "" else paste0(round(100 * bought$share_bought[i]), "%")),
              tags$td(if (is.na(bought$kg_bought_year[i])) "-" else format(bought$kg_bought_year[i], big.mark = ",")),
              tags$td(paste(if (is.na(bought$kg_dm_year[i])) "-" else format(bought$kg_dm_year[i], big.mark = ","), "/",
                            if (is.na(bought$kg_dm_diets[i])) "-" else format(bought$kg_dm_diets[i], big.mark = ",")))
            ), if (nzchar(bought$note[i])) tags$tr(tags$td(), tags$td(colspan = 5,
                 span(class = "batch-chip batch-chip-warning batch-chip-wrap", bought$note[i]))))))
          )
        )
      }
      assumed_ui <- if (!is.null(assumed) && nrow(assumed)) tags$details(
        class = "batch-details mt-2 mb-2",
        tags$summary(sprintf("Values assumed (%d on %d farm%s)", nrow(assumed), length(unique(assumed$Ids)),
                             if (length(unique(assumed$Ids)) == 1) "" else "s")),
        p(class = "batch-hint mt-2", "The survey and the parameters database gave no value for these, so one was chosen",
          "(land cover from the crop type, the share of a bought crop taken from the supplier's field, the kind of a",
          "bought feed when the form did not ask). To use your own values, set them on the parameters page,",
          "'Batch defaults' tab, in your own copy (clone) of the database, then run the batch again.",
          "When part of the manure of a place is taken out and part stays, each part has its own management system."),
        tags$ul(class = "batch-issues", lapply(seq_len(nrow(assumed)), function(i) tags$li(
          if (nzchar(assumed$feed[i] %||% "")) sprintf("Farm %s, %s: %s '%s'", assumed$Ids[i], assumed$feed[i], assumed$field[i], assumed$value[i])
          else sprintf("Farm %s, %s: %s", assumed$Ids[i], assumed$field[i], assumed$value[i]))))
      )
      assumed_ui <- tagList(excluded_ui, renamed_ui, bought_ui, assumed_ui)
      if (!file.exists(f)) return(assumed_ui)
      cv <- readRDS(f)
      if (!nrow(cv)) return(assumed_ui)
      flagged <- sum(nzchar(cv$note) & !grepl("^Not (known|asked)", cv$note))
      tagList(assumed_ui, tags$details(
        class = "batch-details mt-2 mb-2", open = if (flagged > 0) NA else NULL,
        tags$summary(sprintf("How the farmers' harvests were turned into yields (%d crop%s%s)", nrow(cv), if (nrow(cv) == 1) "" else "s",
                             if (flagged) sprintf(", %d to check", flagged) else "")),
        p(class = "batch-hint mt-2", "Yield = amount per year x dry-matter share / area. Fresh amounts use the dry-matter content",
          "of the feed in the database. 'Removed from the field' is the share of what grows that is cut and taken away or eaten",
          "while grazing (the usual value is used when the farmer did not know). Crop residues not taken, grazed or burned stay",
          "on the field as mulch. To change a value, correct it in the downloaded batch file and upload that file."),
        tags$table(
          class = "table table-sm batch-match-table",
          tags$thead(tags$tr(tags$th("Farm"), tags$th("Crop"), tags$th("Farmer's answer"),
                             tags$th("kg per year"), tags$th("Dry matter"), tags$th("t DM/ha/year"),
                             tags$th("Removed from the field"), tags$th(""))),
          tags$tbody(lapply(seq_len(nrow(cv)), function(i) tags$tr(
            tags$td(cv$Ids[i]), tags$td(cv$crop[i], br(), span(class = "batch-hint", cv$matched_crop[i])),
            tags$td(cv$answer[i]), tags$td(format(cv$kg_per_year[i], big.mark = ",")),
            tags$td(if (is.na(cv$dm_fraction[i])) "" else paste0(round(100 * cv$dm_fraction[i]), "%")),
            tags$td(strong(if (is.na(cv$dry_yield_t_dm_ha_year[i])) "-" else cv$dry_yield_t_dm_ha_year[i])),
            tags$td(cv$removed[i] %||% "",
                    if (nzchar(cv$residues[i] %||% "")) tagList(br(), span(class = "batch-hint", "Crop residues: ", cv$residues[i]))),
            tags$td(if (nzchar(cv$note[i])) span(class = if (grepl("^Not (known|asked)", cv$note[i])) "batch-chip" else "batch-chip batch-chip-warning", cv$note[i]))
          )))
        )
      ))
    })

    observeEvent(input$confirm_matches, {
      m <- meta(); tm <- to_match(); req(m, tm)
      values <- vapply(seq_len(nrow(tm)), function(i) {
        v <- input[[paste0("match_", i)]] %||% ""
        if (v == "__new__") {
          from <- input[[paste0("copy_", i)]] %||% ""
          if (!nzchar(from)) return("")
          return(if (tm$kind[i] == "crop") paste0("newcrop:", from) else paste0("new:", from))
        }
        v
      }, "")
      missing <- values == ""
      if (any(missing)) {
        showNotification(sprintf("Choose a match for: %s.", paste(tm$answer[missing], collapse = ", ")),
                         type = "warning", duration = 8)
        return()
      }
      tm$value <- ifelse(values == "__skip__", "", values)
      own_copy <- file.path(user_folder(), "parameters_database", m$database)
      # Feeds and crops from other databases, or new ones, go into the user's own copy
      if (any(grepl("^(lib|libcrop|new|newcrop):", tm$value))) {
        if (!dir.exists(own_copy)) {
          showNotification("New feeds can only be added to your own copy of the database. Clone the database in the parameters page first.",
                           type = "error", duration = NULL)
          return()
        }
        tm <- tryCatch(cleaned::add_matches_to_database(tm, own_copy, batch_feed_library()), error = function(e) e)
        if (inherits(tm, "error")) {
          showNotification(paste("The feeds could not be added:", conditionMessage(tm)), type = "error", duration = NULL)
          return()
        }
        added <- attr(tm, "added")
        if (!is.null(added)) {
          saveRDS(added, file.path(m$dir, "added.rds"))
          showNotification(paste0("Added to your database '", m$database, "': ",
                                  paste0(added$name, " (", added$kind, " ", added$code, ", values of ", added$copied_from, ")", collapse = "; "), "."),
                           type = "message", duration = 15)
          db_tick(db_tick() + 1)
        }
      }
      # Remember the answers in the user's own copy of the database
      if (dir.exists(own_copy)) try(cleaned::remember_answer_matches(tm, own_copy), silent = TRUE)
      r <- tryCatch(convert_odk(m$dir, m$database, matches = tm), error = function(e) e)
      if (inherits(r, "error") || is.null(r$input)) {
        showNotification(paste("The answers could not be converted:", if (inherits(r, "error")) conditionMessage(r) else "some answers are still not matched."),
                         type = "error", duration = NULL)
        return()
      }
      check_batch_file(m$dir, m$name, m$file_name, TRUE, m$database)
      refresh_meta()
      showNotification("Matches confirmed and remembered. The data have been checked.", type = "message")
    })


    report <- reactive({
      m <- meta()
      if (is.null(m)) return(NULL)
      f <- file.path(m$dir, "validation.rds")
      if (file.exists(f)) readRDS(f) else NULL
    })

    output$check_result <- renderUI({
      m <- meta(); r <- report()
      if (is.null(m) || is.null(r)) return(NULL)
      s <- attr(r, "summary")
      ov <- m$overview
      plural <- function(n, word) paste0(n, " ", word, if (identical(as.numeric(n), 1)) "" else "s")
      facts <- c(
        plural(ov$farms, "farm"),
        sprintf("%s (%s)", plural(ov$livestock_rows, "livestock row"), plural(ov$livestock_types, "type")),
        paste(plural(ov$feeds_used, "feed"), "used"),
        if (ov$own_seasons > 0) sprintf("%s farm%s with their own seasons", ov$own_seasons, if (ov$own_seasons == 1) "" else "s"),
        if (ov$feed_overrides > 0) sprintf("%s farm-specific feed or crop value%s", ov$feed_overrides, if (ov$feed_overrides == 1) "" else "s"),
        if (length(ov$groups)) paste("grouped by", paste(ov$groups, collapse = ", "))
      )
      banner <- if (s[["errors"]] > 0) {
        div(class = "alert alert-danger batch-alert", role = "alert",
            icon("circle-xmark"),
            strong(sprintf(" %d problem%s must be fixed before running.", s[["errors"]], if (s[["errors"]] == 1) "" else "s")),
            " Fix them in your Excel file and upload it again.",
            if (s[["warnings"]] > 0) sprintf(" There are also %d thing%s to check.", s[["warnings"]], if (s[["warnings"]] == 1) "" else "s"))
      } else if (s[["warnings"]] > 0) {
        div(class = "alert alert-success batch-alert", role = "status",
            icon("circle-check"), strong(" Your file is ready to run."),
            sprintf(" %d thing%s to check: they will not stop the run.", s[["warnings"]], if (s[["warnings"]] == 1) "" else "s"))
      } else {
        div(class = "alert alert-success batch-alert", role = "status",
            icon("circle-check"), strong(" Your file is ready to run."), " No problems found.")
      }
      tagList(
        p(class = "batch-file-name", icon(if (isTRUE(m$from_odk)) "mobile-screen" else "file-excel"), " ", m$file_name),
        if (isTRUE(m$from_odk)) p(class = "batch-hint",
          sprintf("%s ODK submission%s turned into a batch file. Rows mentioned below refer to that file: ", ov$farms, if (identical(as.numeric(ov$farms), 1)) " was" else "s were"),
          downloadLink(ns("download_converted"), "download it"), " to correct it and upload it again."),
        tags$ul(class = "batch-facts", lapply(facts, tags$li)),
        banner
      )
    })

    observe({
      r <- report()
      shinyjs::toggle("problems_section", condition = !is.null(r) && nrow(r) > 0)
    })

    output$problems <- DT::renderDT({
      r <- report(); req(r, nrow(r) > 0)
      if (!identical(input$problem_filter, "all")) r <- r[r$severity == input$problem_filter, , drop = FALSE]
      where <- paste0(
        "<strong>", r$sheet, "</strong>", ifelse(is.na(r$row), "", paste0(", row ", r$row)),
        ifelse(is.na(r$Ids), "", paste0("<br><span class='batch-hint'>farm ", htmltools::htmlEscape(r$Ids), "</span>"))
      )
      value <- ifelse(is.na(r$field), "",
                      paste0("<code>", htmltools::htmlEscape(r$field), "</code>",
                             ifelse(is.na(r$value), "", paste0(" = ", htmltools::htmlEscape(r$value)))))
      sev <- ifelse(r$severity == "error",
                    '<span class="batch-chip batch-chip-error"><i class="fa fa-circle-xmark" aria-hidden="true"></i> Must fix</span>',
                    '<span class="batch-chip batch-chip-warning"><i class="fa fa-triangle-exclamation" aria-hidden="true"></i> To check</span>')
      DT::datatable(
        data.frame(Type = sev, Where = where, `Cell` = value,
                   `What we found` = htmltools::htmlEscape(batch_friendly_message(r$message)), check.names = FALSE),
        escape = FALSE, rownames = FALSE, selection = "none", class = "compact stripe batch-problems",
        options = list(pageLength = 10, dom = "ftip", autoWidth = FALSE,
                       columnDefs = list(list(width = "90px", targets = 0), list(width = "130px", targets = 1),
                                         list(width = "190px", targets = 2)),
                       language = list(search = "Search:", emptyTable = "Nothing in this category."))
      )
    })

    output$download_converted <- downloadHandler(
      filename = function() paste0(meta()$name %||% "batch", " - batch file.xlsx"),
      content = function(file) file.copy(file.path(meta()$dir, "input.xlsx"), file)
    )

    output$download_report <- downloadHandler(
      filename = function() paste0(meta()$name %||% "batch", " - check report.xlsx"),
      content = function(file) {
        r <- report()
        r$severity <- ifelse(r$severity == "error", "Must fix", "To check")
        names(r) <- c("Type", "Sheet", "Row", "Farm (Ids)", "Column", "Value", "What we found")
        openxlsx::write.xlsx(r, file, asTable = TRUE, colWidths = "auto")
      }
    )

    # ------ Run -----------------------------------------------------------------------
    # Livestock types that can be left out: the types of the farms in the file
    # (named as in the database, or in the file's livestock_p sheet)
    observe({
      m <- meta()
      if (is.null(m) || !file.exists(file.path(m$dir, "input.xlsx"))) {
        updateSelectizeInput(session, "exclude_types", choices = character(0), selected = character(0))
        return()
      }
      inp <- tryCatch(cleaned::read_batch_input(file.path(m$dir, "input.xlsx")), error = function(e) NULL)
      codes <- unique(as.character(inp$livestock$livetype_code))
      codes <- codes[!is.na(codes)]
      lt <- tryCatch(cleaned::read_parameter_database(batch_database_path(user_folder(), m$database))$lkp_livetype, error = function(e) NULL)
      lp <- attr(inp, "project")$livestock_p
      name_of <- function(k) {
        n <- if (!is.null(lp) && "livetype_desc" %in% names(lp)) lp$livetype_desc[as.character(lp$livetype_code) == k] else character(0)
        if (!length(n) || is.na(n[1])) n <- if (!is.null(lt)) lt$livetype_desc[as.character(lt$livetype_code) == k] else character(0)
        if (length(n) && !is.na(n[1])) n[1] else k
      }
      labels <- vapply(codes, name_of, "")
      choices <- stats::setNames(codes, labels)[order(labels)]
      updateSelectizeInput(session, "exclude_types", choices = choices,
                           selected = intersect(as.character(unlist(m$exclude_livetypes)), codes))
    })

    can_run <- reactive({
      m <- meta()
      !is.null(m) && m$status %in% c("checked", "done", "failed", "cancelled", "interrupted")
    })

    output$run_locked <- renderUI({
      m <- meta()
      msg <- if (is.null(m)) "Upload a file in step 2 first."
             else if (identical(m$status, "needs_fixes")) "Fix the problems found in step 2, then upload the file again."
             else if (identical(m$status, "needs_matching")) "Confirm how the farmers' answers match the database in step 2 first."
             else NULL
      if (is.null(msg)) return(NULL)
      p(class = "batch-locked", icon("lock"), " ", msg)
    })

    observe({
      m <- meta()
      running <- !is.null(m) && identical(m$status, "running")
      shinyjs::toggle("run_settings", condition = can_run() && !running)
      shinyjs::toggle("progress_section", condition = running)
      shinyjs::toggleState("run", condition = can_run() && !running)
      if (!is.null(m) && m$status %in% c("done", "failed", "cancelled", "interrupted")) {
        updateActionButton(session, "run", label = "Run the batch again")
      } else {
        updateActionButton(session, "run", label = "Run the batch")
      }
    })

    output$run_summary <- renderUI({
      m <- meta(); req(m, can_run())
      n <- m$farms
      p(class = "batch-run-summary",
        sprintf("%s farms will be run with the '%s' database. ", n, m$database),
        sprintf("This takes about %s. ", batch_format_duration(max(30, n * 1.5))),
        "You can keep working in iCLEANED while it runs.",
        if (identical(m$database, input$database)) NULL else
          tags$span(class = "text-danger", sprintf(" Note: this file was checked with '%s'; that database will be used.", m$database)))
    })

    observeEvent(input$run, {
      m <- meta(); req(m, can_run())
      running <- Filter(function(b) identical(b$status, "running"), batch_list(user_folder()))
      if (length(running) > 0) {
        showNotification(sprintf("The batch '%s' is still running. Please wait until it finishes or cancel it.",
                                 running[[1]]$name), type = "warning", duration = 8)
        return()
      }
      path <- batch_database_path(user_folder(), m$database)
      exclude <- as.character(input$exclude_types %||% character(0))
      # Leaving out livestock can leave a farm without animals: check again
      if (length(exclude)) {
        inp0 <- cleaned::read_batch_input(file.path(m$dir, "input.xlsx"))
        inp <- batch_exclude_livetypes(inp0, exclude)
        attr(inp, "project") <- attr(inp0, "project")
        r <- cleaned::validate_batch_input(inp, path)
        if (attr(r, "summary")[["errors"]] > 0) {
          showModal(modalDialog(
            title = "These livestock types cannot be left out for this file",
            p("Without them, the file has problems:"),
            tags$ul(lapply(utils::head(unique(paste0(r$Ids[r$severity == "error"], ": ", r$message[r$severity == "error"])), 5), tags$li)),
            footer = modalButton("OK"), easyClose = TRUE
          ))
          return()
        }
      }
      unlink(file.path(m$dir, "output"), recursive = TRUE)
      lt_names <- tryCatch({ lt <- cleaned::read_parameter_database(path)$lkp_livetype
        lt$livetype_desc[match(exclude, as.character(lt$livetype_code))] }, error = function(e) exclude)
      batch_meta_write(m$dir, exclude_calves = NULL, exclude_livetypes = if (length(exclude)) exclude else NULL,
                       exclude_names = if (length(exclude)) ifelse(is.na(lt_names), exclude, lt_names) else NULL,
                       farms_ok = NULL,
                       farms_failed = NULL, farms_with_warnings = NULL)
      ok <- tryCatch({
        # primary_excel = NULL: each farm gets the full detailed outputs of
        # CLEANED's batch processing (not the shorter app scenario workbook)
        batch_start(m$dir, path, m$database, exclude, primary_excel = NULL)
        TRUE
      }, error = function(e) {
        batch_meta_write(m$dir, status = "failed", finished = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                         message = paste("The batch could not be started:", conditionMessage(e)))
        showNotification(paste("The batch could not be started:", conditionMessage(e)), type = "error", duration = NULL)
        FALSE
      })
      if (ok) watching(c(watching(), m$dir))
      refresh_meta()
    })

    observeEvent(input$cancel, {
      showModal(modalDialog(
        title = "Cancel this run?",
        "Farms already finished are not kept. You can run the batch again later.",
        footer = tagList(modalButton("Keep running"),
                         actionButton(ns("confirm_cancel"), "Cancel the run", class = "btn-danger")),
        easyClose = TRUE
      ))
    })
    observeEvent(input$confirm_cancel, {
      removeModal()
      batch_cancel(current())
      refresh_meta()
      showNotification("The run was cancelled.", type = "warning")
    })

    # Progress: check the batch files while a batch is running
    progress <- reactiveVal(NULL)
    observe({
      m <- meta()
      if (is.null(m) || !identical(m$status, "running")) {
        # also notice batches started in this session that finish while another one is on screen
        if (length(watching())) invalidateLater(3000)
        for (dir in watching()) {
          b <- batch_meta_read(dir)
          if (!is.null(b) && !identical(b$status, "running") && !identical(dir, current())) {
            showNotification(sprintf("Batch '%s': %s.", b$name, tolower(batch_status_label(b$status))), type = "message", duration = 10)
            watching(setdiff(watching(), dir))
            history_tick(isolate(history_tick()) + 1)
          }
        }
        return()
      }
      invalidateLater(1500)
      batch_check_process(m$dir)
      progress(batch_progress_read(m$dir))
      fresh <- batch_meta_read(m$dir)
      if (!identical(fresh$status, "running")) {
        meta(fresh)
        history_tick(isolate(history_tick()) + 1)
        if (m$dir %in% watching()) {
          watching(setdiff(watching(), m$dir))
          msg <- switch(fresh$status,
                        done = sprintf("Batch '%s' finished: %s farms done.", fresh$name, fresh$farms_ok %||% 0),
                        failed = sprintf("Batch '%s' stopped: %s", fresh$name, fresh$message %||% ""),
                        sprintf("Batch '%s': %s.", fresh$name, tolower(batch_status_label(fresh$status))))
          showNotification(msg, type = if (identical(fresh$status, "done")) "message" else "error", duration = 10)
        }
      }
    })

    output$progress <- renderUI({
      m <- meta(); req(m)
      p <- progress()
      started <- as.POSIXct(m$started)
      elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
      if (is.null(p)) {
        pct <- 0
        text <- "Preparing the farms..."
        eta <- ""
      } else {
        done <- p$i - 1
        pct <- round(100 * done / p$n)
        text <- sprintf("Farm %d of %d: %s", p$i, p$n, p$farm)
        eta <- if (done > 0) paste("About", batch_format_duration(elapsed / done * (p$n - done)), "left") else "Estimating the time left..."
      }
      tagList(
        div(class = "d-flex justify-content-between batch-progress-text",
            span(text), span(eta)),
        div(class = "progress batch-progress", role = "progressbar",
            `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100,
            `aria-label` = "Batch progress",
            div(class = "progress-bar", style = sprintf("width: %d%%", pct), paste0(pct, "%"))),
        p(class = "batch-hint mt-2", `aria-live` = "polite",
          paste0("Running for ", batch_format_duration(elapsed), ". You can leave this page; the batch keeps running."))
      )
    })

    # ------ Results -----------------------------------------------------------------
    results <- reactive({
      m <- meta()
      if (is.null(m) || !identical(m$status, "done")) return(NULL)
      wb <- file.path(m$dir, "output", "batch_results.xlsx")
      if (!file.exists(wb)) return(NULL)
      sheets <- openxlsx::getSheetNames(wb)
      read <- function(s) if (s %in% sheets) openxlsx::read.xlsx(wb, sheet = s, check.names = FALSE) else data.frame()
      list(summary = read("summary"), log = read("log"),
           groups = stats::setNames(lapply(grep("^by_|^all_farms$", sheets, value = TRUE), read),
                                    grep("^by_|^all_farms$", sheets, value = TRUE)))
    })

    output$results_locked <- renderUI({
      m <- meta()
      if (!is.null(results())) return(NULL)
      msg <- if (is.null(m) || !m$status %in% c("running", "failed", "cancelled", "interrupted")) {
        "Results appear here when the batch has run."
      } else if (identical(m$status, "running")) {
        "The batch is running. Results appear here when it finishes."
      } else if (identical(m$status, "failed")) {
        paste("The batch stopped before finishing:", m$message %||% "unknown error.")
      } else {
        paste0("The batch was ", tolower(batch_status_label(m$status)), ". Run it again in step 3.")
      }
      p(class = if (identical(m$status, "failed")) "batch-locked text-danger" else "batch-locked", icon("hourglass-half"), " ", msg)
    })
    observe(shinyjs::toggle("results_section", condition = !is.null(results())))

    output$result_tiles <- renderUI({
      r <- results(); m <- meta(); req(r)
      log <- r$log
      ok <- sum(log$status == "ok"); failed <- sum(log$status == "failed")
      warned <- sum(log$status == "ok" & !is.na(log$warnings))
      secs <- as.numeric(difftime(as.POSIXct(m$finished), as.POSIXct(m$started), units = "secs"))
      tile <- function(value, label, cls = "") div(class = paste("batch-tile", cls), div(class = "batch-tile-value", value), div(class = "batch-tile-label", label))
      div(
        class = "batch-tiles",
        tile(ok, "farms finished"),
        tile(failed, "farms failed", if (failed > 0) "batch-tile-error" else ""),
        tile(warned, "farms with notes", if (warned > 0) "batch-tile-warning" else ""),
        tile(batch_format_duration(secs), "run time"),
        p(class = "batch-hint w-100 mt-2 mb-0",
          sprintf("Database: %s%s%s. Finished %s.", m$database,
                  if (isTRUE(m$database_copy)) " (the copy used is saved with the batch)" else "",
                  if (isTRUE(m$exclude_calves)) " - calves left out"
                  else if (length(m$exclude_names)) paste0(" - left out: ", paste(unlist(m$exclude_names), collapse = ", ")) else "",
                  m$finished))
      )
    })

    observe({
      r <- results(); req(r)
      updateSelectInput(session, "indicator", choices = batch_indicator_choices(names(r$summary)),
                        selected = if (isTRUE(isolate(input$indicator) %in% names(r$summary))) isolate(input$indicator) else "ghg_emission_kg_co2_eq_per_kg_fpcm")
      groups <- grep("^group_", names(r$summary), value = TRUE)
      groups <- groups[vapply(groups, function(g) any(!is.na(r$summary[[g]]) & r$summary[[g]] != ""), TRUE)]
      has_age <- "respondent_age" %in% names(r$summary) && any(!is.na(suppressWarnings(as.numeric(r$summary$respondent_age))))
      updateSelectInput(session, "group_by",
                        choices = c("All farms together" = "", stats::setNames(groups, sub("^group_", "", groups)),
                                    if (has_age) c("age of the respondent" = "__age__")))
    })

    # Age groups: a definition (UN, African Union) or the ages typed
    age_groups_of <- function(age) {
      sch <- batch_age_schemes[[input$age_scheme %||% "un"]]
      if (is.null(sch)) batch_age_groups(age, batch_age_starts(input$age_starts))
      else batch_age_groups(age, sch$starts, sch$names)
    }

    # The group of each farm for the chosen grouping
    farm_groups <- function(d) {
      g <- input$group_by %||% ""
      if (identical(g, "__age__") && "respondent_age" %in% names(d)) {
        return(as.character(age_groups_of(d$respondent_age)))
      }
      if (nzchar(g) && g %in% names(d)) return(ifelse(is.na(d[[g]]), "(blank)", as.character(d[[g]])))
      rep("All farms", nrow(d))
    }

    output$indicator_plot <- renderPlot({
      r <- results(); req(r, input$indicator, input$indicator %in% names(r$summary))
      d <- batch_indicator_data(r$summary, input$indicator)
      d$group <- farm_groups(d)
      # (youngest group at the top of the chart)
      if (identical(input$group_by, "__age__")) d$group <- factor(d$group, levels = rev(levels(age_groups_of(r$summary$respondent_age))))
      d <- d[is.finite(d$value), , drop = FALSE]
      # (shiny:: because jsonlite, loaded later by the app, also has validate())
      shiny::validate(shiny::need(nrow(d) > 0, "No farm has a value for this indicator."))
      left_out <- attr(d, "left_out")
      ggplot2::ggplot(d, ggplot2::aes(x = value, y = group)) +
        ggplot2::geom_boxplot(outlier.shape = NA, fill = "#E6F0F5", colour = "#005275", width = 0.5) +
        ggplot2::geom_jitter(height = 0.15, width = 0, size = 2.2, alpha = 0.65, colour = "#009ADB") +
        ggplot2::labs(x = batch_indicator_label(input$indicator), y = NULL,
                      caption = if (!is.null(left_out) && left_out$n > 0) sprintf("%d farm%s that %s %s not shown.", left_out$n, if (left_out$n == 1) "" else "s", left_out$why, if (left_out$n == 1) "is" else "are")) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
                       plot.caption = ggplot2::element_text(colour = "#5f6b72", size = 11))
    }, res = 96)

    output$farm_table <- DT::renderDT({
      r <- results(); req(r, input$indicator %in% names(r$summary))
      d <- r$summary
      used_groups <- Filter(function(g) any(!is.na(d[[g]]) & d[[g]] != ""), grep("^group_", names(d), value = TRUE))
      keep <- c("farm_name", used_groups, if ("respondent_age" %in% names(d)) "respondent_age", input$indicator)
      d <- d[, intersect(keep, names(d)), drop = FALSE]
      shown <- batch_indicator_data(r$summary, input$indicator)
      d[[input$indicator]] <- ifelse(d$farm_name %in% shown$farm_name,
                                     round(suppressWarnings(as.numeric(d[[input$indicator]])), 3), NA)
      names(d) <- c("Farm", sub("^respondent_age$", "age", sub("^group_", "", setdiff(names(d)[-1], input$indicator))), batch_indicator_label(input$indicator))
      DT::datatable(d[order(-d[[ncol(d)]], na.last = TRUE), , drop = FALSE], rownames = FALSE, selection = "none",
                    options = list(pageLength = 10, dom = "ftip",
                                   language = list(search = "Find a farm:")))
    })

    output$group_note <- renderUI({
      r <- results(); req(r)
      if (!any(grepl("^group_", names(r$summary)))) {
        p(class = "batch-hint", "Add columns starting with 'group_' to the farm sheet (for example group_site or group_district)",
          "to get averages per group. Below are the averages of all farms.")
      }
    })
    output$group_table <- DT::renderDT({
      r <- results(); req(r)
      stat <- input$group_stat %||% "median"
      if (identical(input$group_by, "__age__")) {
        # age groups: computed here from the ranges typed
        g <- batch_group_stats(r$summary, age_groups_of(r$summary$respondent_age), stat)
      } else {
        key <- if (nzchar(input$group_by %||% "")) paste0("by_", sub("^group_", "", input$group_by)) else "all_farms"
        g <- r$groups[[key]] %||% r$groups[["all_farms"]]
        req(g, nrow(g) > 0)
        g <- g[g$statistic == stat, , drop = FALSE]
      }
      req(nrow(g) > 0)
      ind <- intersect(names(batch_indicator_labels), names(g))
      groups <- paste0(g[[1]], " (", g$farms, ifelse(as.numeric(g$farms) == 1, " farm)", " farms)"))
      out <- data.frame(
        Topic = sub("\\|.*", "", batch_indicator_labels[ind]),
        Indicator = batch_indicator_label(ind),
        stats::setNames(lapply(seq_len(nrow(g)), function(k) signif(as.numeric(unlist(g[k, ind])), 4)), groups),
        check.names = FALSE
      )
      DT::datatable(out, rownames = FALSE, selection = "none",
                    options = list(dom = "ft", paging = FALSE, scrollY = "420px", scrollCollapse = TRUE,
                                   ordering = FALSE, language = list(search = "Find an indicator:")))
    })

    output$issues_tab_title <- renderUI({
      r <- results()
      failed <- if (is.null(r)) 0 else sum(r$log$status == "failed")
      noted <- if (is.null(r)) 0 else sum(r$log$status == "ok" & !is.na(r$log$warnings))
      span(if (failed > 0) "Failed farms and notes" else "Notes",
           if (failed + noted > 0) span(class = paste("batch-count", if (failed == 0) "batch-count-note"),
                                        `aria-label` = sprintf("%d farms", failed + noted), failed + noted))
    })
    output$issues <- renderUI({
      r <- results(); req(r)
      log <- r$log
      failed <- log[log$status == "failed", , drop = FALSE]
      notes <- batch_summarise_notes(log)
      if (!nrow(failed) && !nrow(notes)) return(p(icon("circle-check"), " All farms ran without problems."))
      tagList(
        if (nrow(failed)) tagList(
          h3("Farms that could not be calculated"),
          p(class = "batch-hint", "Correct these farms in your file and run the batch again."),
          tags$ul(class = "batch-issues", lapply(seq_len(nrow(failed)), function(k)
            tags$li(strong(failed$farm_name[k]), ": ", failed$message[k])))
        ),
        if (nrow(notes)) tagList(
          h3("Notes on finished farms"),
          p(class = "batch-hint", "These farms have results; the notes point to input values worth checking",
            "(for example body weight, feed quality or milk yield). Open a row to see the farms."),
          lapply(split(notes, factor(notes$note, unique(notes$note))), function(x) tagList(
            p(strong(x$note[1])),
            lapply(seq_len(nrow(x)), function(k) tags$details(
              class = "batch-details batch-note",
              tags$summary(if (nzchar(x$livestock[k])) x$livestock[k] else "Farms",
                           span(class = "batch-chip ms-2", sprintf("%d farm%s", x$farms[k], if (x$farms[k] == 1) "" else "s"))),
              p(class = "batch-hint mt-1", x$farm_list[k])
            ))
          ))
        )
      )
    })

    output$download_results <- downloadHandler(
      filename = function() paste0(meta()$name, " - farm summary.xlsx"),
      content = function(file) file.copy(file.path(meta()$dir, "output", "batch_results.xlsx"), file)
    )
    output$download_detailed <- downloadHandler(
      filename = function() paste0(meta()$name, " - detailed results.xlsx"),
      content = function(file) batch_safely("The detailed results could not be downloaded", {
        file.copy(file.path(meta()$dir, "output", "batch_detailed_results.xlsx"), file)
      })
    )
    observe({
      m <- meta()
      shinyjs::toggle("download_detailed", condition = !is.null(m) &&
                        file.exists(file.path(m$dir, "output", "batch_detailed_results.xlsx")))
    })

    output$download_input_used <- downloadHandler(
      filename = function() paste0(meta()$name, " - batch file with the values used.xlsx"),
      content = function(file) file.copy(file.path(meta()$dir, "input.xlsx"), file, overwrite = TRUE)
    )

    # The parameters database the batch was run with (copy saved with the batch)
    output$download_database <- downloadHandler(
      filename = function() paste0(meta()$name, " - parameters database (", meta()$database, ").zip"),
      content = function(file) batch_safely("The parameters database could not be downloaded", {
        copy <- file.path(meta()$dir, "parameters_database")
        if (!dir.exists(copy)) stop("This batch was run before databases were saved with batches.")
        zip::zip(file, files = list.files(copy), root = copy)
      })
    )
    observe({
      m <- meta()
      shinyjs::toggle("download_database", condition = !is.null(m) && dir.exists(file.path(m$dir, "parameters_database")))
    })

    output$download_all <- downloadHandler(
      filename = function() paste0(meta()$name, " - detailed results per farm.zip"),
      content = function(file) batch_safely("The farm results could not be downloaded", {
        # One workbook per farm (the detailed CLEANED outputs) and its study object
        out <- file.path(meta()$dir, "output")
        log <- results()$log
        tmp <- tempfile("farms_"); dir.create(file.path(tmp, "study_objects"), recursive = TRUE)
        for (folder in log$folder[log$status == "ok"]) {
          file.copy(file.path(out, "farms", folder, paste0(folder, ".xlsx")), file.path(tmp, paste0(folder, ".xlsx")))
          file.copy(file.path(out, "farms", folder, "study_object.json"), file.path(tmp, "study_objects", paste0(folder, ".json")))
        }
        zip::zip(file, files = list.files(tmp, recursive = TRUE), root = tmp)
      }),
      contentType = "application/zip"
    )


    # Copy farms to the user's scenarios so the Dashboard can show and compare them
    observeEvent(input$to_dashboard, {
      r <- results(); req(r)
      farms <- r$log$farm_name[r$log$status == "ok"]
      showModal(modalDialog(
        title = "Open farms in the Dashboard",
        p("The farms you choose are added to your scenarios (with their study objects),",
          "so you can view them and compare them in the Dashboard."),
        selectizeInput(ns("dashboard_farms"), "Farms", choices = farms, multiple = TRUE, width = "100%",
                       options = list(placeholder = "Type or pick farms", plugins = list("remove_button"))),
        p(class = "batch-hint", "Tip: pick a few farms; comparing more than 10 at once is hard to read."),
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("confirm_dashboard"), "Add to my scenarios", class = "btn-primary")),
        easyClose = TRUE
      ))
    })
    observeEvent(input$confirm_dashboard, {
      req(length(input$dashboard_farms) > 0)
      m <- meta(); log <- results()$log
      prefix <- gsub("[^A-Za-z0-9_-]+", "_", m$name)
      added <- character(0)
      for (farm in input$dashboard_farms) {
        folder <- log$folder[log$farm_name == farm][1]
        src <- file.path(m$dir, "output", "farms", folder)
        name <- paste0(prefix, "-", folder)
        scen <- file.path(user_folder(), "scenarios", name)
        dir.create(scen, recursive = TRUE, showWarnings = FALSE)
        file.copy(file.path(src, paste0(folder, ".json")), file.path(scen, paste0(name, ".json")), overwrite = TRUE)
        file.copy(file.path(src, paste0(folder, ".xlsx")), file.path(scen, paste0(name, ".xlsx")), overwrite = TRUE)
        file.copy(file.path(src, "scenario_data.rds"), file.path(scen, "scenario_data.rds"), overwrite = TRUE)
        file.copy(file.path(src, "study_object.json"), file.path(user_folder(), "study_objects", paste0(name, ".json")), overwrite = TRUE)
        added <- c(added, name)
      }
      removeModal()
      if (!is.null(session$userData$scenarios_list)) session$userData$scenarios_list(list.files(file.path(user_folder(), "scenarios")))
      if (!is.null(session$userData$study_objects)) session$userData$study_objects(list.files(file.path(user_folder(), "study_objects")))
      if (!is.null(session$userData$run_scenario_counter)) session$userData$run_scenario_counter(session$userData$run_scenario_counter() + 1)
      showNotification(sprintf("%d farm%s added to your scenarios (named '%s-...'). Open the Dashboard to see them.",
                               length(added), if (length(added) == 1) "" else "s", prefix), type = "message", duration = 8)
    })

    # ------ Your batches --------------------------------------------------------------
    batches <- reactive({ history_tick(); batch_list(user_folder()) })
    output$history <- renderUI({
      b <- batches()
      if (!length(b)) return(p(class = "batch-hint", "No batches yet."))
      tags$ul(
        class = "batch-history-list",
        lapply(b, function(x) {
          id <- basename(x$dir)
          active <- identical(x$dir, current())
          tags$li(
            class = if (active) "active" else NULL,
            tags$button(
              type = "button", class = "batch-history-item",
              `aria-current` = if (active) "true" else NULL,
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("open_batch"), id),
              div(class = "batch-history-name", x$name %||% id),
              div(class = "batch-history-meta",
                  span(class = paste0("batch-chip batch-chip-", x$status), batch_status_label(x$status)),
                  span(sprintf("%s farms", x$farms %||% "?")),
                  span(substr(x$created %||% "", 1, 16)))
            ),
            if (!identical(x$status, "running")) tags$button(
              type = "button", class = "batch-history-delete", title = paste("Delete", x$name),
              `aria-label` = paste("Delete batch", x$name),
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("delete_batch"), id),
              icon("trash-can")
            )
          )
        })
      )
    })

    open_batch <- function(dir) {
      current(dir)
      refresh_meta()
      m <- meta()
      if (!is.null(m)) {
        if (!is.null(m$database) && dir.exists(batch_database_path(user_folder(), m$database))) {
          updateSelectInput(session, "database", selected = m$database)
        }
      }
      progress(NULL)
    }
    observeEvent(input$open_batch, open_batch(file.path(batch_root(user_folder()), input$open_batch)))

    observeEvent(input$delete_batch, {
      dir <- file.path(batch_root(user_folder()), input$delete_batch)
      b <- batch_meta_read(dir)
      showModal(modalDialog(
        title = "Delete this batch?",
        p(sprintf("'%s' and all its results will be deleted. This cannot be undone.", b$name %||% input$delete_batch)),
        p(class = "batch-hint", "Farms you added to your scenarios are kept."),
        footer = tagList(modalButton("Keep it"),
                         actionButton(ns("confirm_delete"), "Delete", class = "btn-danger")),
        easyClose = TRUE
      ))
    })
    observeEvent(input$confirm_delete, {
      removeModal()
      dir <- file.path(batch_root(user_folder()), input$delete_batch)
      if (identical(dir, current())) { current(NULL); progress(NULL) }
      unlink(dir, recursive = TRUE)
      refresh_meta()
      showNotification("Batch deleted.", type = "message")
    })

    # The upload box; drawn again (empty) when a new batch is started, because
    # a file input cannot be cleared otherwise
    file_box <- reactiveVal(0)
    output$file_ui <- renderUI({
      file_box()
      fileInput(ns("file"), "Batch file (.xlsx), ODK Central export (.zip) or KoboToolbox export (.xlsx)", accept = c(".xlsx", ".zip"),
                buttonLabel = "Choose file...", placeholder = "No file chosen", width = "100%")
    })

    # Start a new batch: clear the screen, keep the earlier batches in the list,
    # and take the user to step 2 so it is clear what to do next
    observeEvent(input$new_batch, {
      current(NULL); progress(NULL)
      refresh_meta()
      file_box(file_box() + 1)
      shinyjs::runjs(sprintf(
        "document.getElementById('%s').scrollIntoView({behavior: 'smooth', block: 'start'});
         var el = document.getElementById('%s'); el.classList.remove('batch-flash'); void el.offsetWidth; el.classList.add('batch-flash');",
        ns("step2"), ns("step2")))
      showNotification("New batch: upload a filled template or an ODK export in step 2. Your earlier batches stay in the list.",
                       type = "message", duration = 6)
    })

    # Open the latest batch when the page is first shown
    observe({
      b <- isolate(batches())
      if (length(b)) open_batch(b[[1]]$dir)
    }) |> bindEvent(user_folder(), once = TRUE)

    # ------ Stepper ---------------------------------------------------------------
    output$stepper <- renderUI({
      m <- meta(); has_results <- !is.null(results())
      state <- c(
        "done",
        if (is.null(m)) "current" else if (m$status %in% c("needs_fixes", "needs_matching")) "attention" else "done",
        if (is.null(m) || m$status %in% c("needs_fixes", "needs_matching")) "todo" else if (has_results) "done" else "current",
        if (has_results) "current" else "todo"
      )
      if (!is.null(m) && identical(m$status, "running")) state[3] <- "running"
      labels <- c("Database and template", "Upload and check", "Run", "Results")
      words <- c(done = "done", current = "current step", attention = "needs your attention",
                 todo = "not yet available", running = "running")
      tags$ol(
        class = "batch-stepper",
        lapply(1:4, function(k) tags$li(
          class = paste0("batch-stepper-", state[k]),
          `aria-current` = if (state[k] %in% c("current", "attention", "running")) "step" else NULL,
          tags$a(href = paste0("#", ns(paste0("step", k))),
                 span(class = "batch-stepper-dot", `aria-hidden` = "true",
                      if (state[k] == "done") icon("check") else if (state[k] == "attention") "!" else k),
                 span(class = "batch-stepper-label", labels[k]),
                 span(class = "visually-hidden", paste0(" (", words[state[k]], ")")))
        ))
      )
    })
  })
}
