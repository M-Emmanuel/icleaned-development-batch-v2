# Batch processing page ------------------------------------------------------------
# A guided flow in four steps (database and template > upload and check > run >
# results), with the user's earlier batches listed alongside.

batch_step_card <- function(ns, number, title, ..., id) {
  div(
    id = ns(id),
    class = "batch-step card mb-4",
    div(
      class = "d-flex align-items-center mb-3",
      span(class = "batch-step-number", `aria-hidden` = "true", number),
      h2(class = "m-0", span(class = "visually-hidden", paste0("Step ", number, ": ")), title)
    ),
    ...
  )
}

batch_ui <- function(id) {
  ns <- NS(id)
  fixedPage(
    class = "batch-page pt-4 pb-5",
    # ------ Intro ---------------------------------------------------------------
    div(
      class = "mb-4",
      h2("Batch processing", class = "batch-title"),
      p(
        class = "batch-lead",
        "Run CLEANED for many farms at once, from field survey data collected with ODK or KoboToolbox,",
        "or from a filled Excel template. Each farm gets its own results, and you get a summary that compares them."
      )
    ),
    # ------ Stepper ---------------------------------------------------------------
    tags$nav(`aria-label` = "Batch steps", uiOutput(ns("stepper"))),
    fluidRow(
      # ------ Steps -------------------------------------------------------------
      column(
        width = 8,
        # Step 1 ------------------------------------------------------------------
        batch_step_card(
          ns, 1, "Get the template or the field form", id = "step1",
          p("Batch processing uses 'Batch parameters': one database that brings together the livestock types, feeds",
            "and crops of livestock systems in 14 regions across Africa, Asia and Latin America, with batch defaults",
            "for the values farms leave blank. The first time you open this page, your own copy, 'My batch parameters',",
            "is made and selected: feeds and answers you add while matching survey data are saved there,",
            "and the original stays unchanged."),
          selectInput(ns("database"), "Parameters database", choices = NULL, width = "100%",
                      selectize = FALSE),
          uiOutput(ns("database_note")),
          div(
            class = "d-flex flex-wrap gap-2 mt-3",
            downloadButton(ns("download_template"), "Download the Excel template",
                           class = "btn-primary", icon = icon("file-excel"))
          ),
          p(class = "batch-hint mt-2",
            "The template lists the codes of this database and has dropdowns for every choice.",
            "Each data sheet has a green twin ending in _p (for example livestock_p) with the values used where a",
            "farm's cell is left blank: they come filled with the batch defaults and can be changed for your project.",
            "Its 'README' and 'dictionary' sheets explain each column."),
          tags$details(
            class = "batch-details mt-2",
            tags$summary("Collecting data in the field with ODK?"),
            p(class = "mt-2", "Download the ODK form and publish it on ODK Central or KoboToolbox. It collects the minimum",
              "biophysical data required to run an iCLEANED assessment for each farm (livestock, feeding and crops)."),
            p(class = "batch-hint", "Results can be compared by region and gender.",
              "To compare by other groups, such as project site or system, list them here separated by commas."),
            div(class = "d-flex flex-wrap align-items-end gap-2",
                textInput(ns("odk_groups"), NULL, value = "", placeholder = "e.g. project site, system", width = "300px"),
                downloadButton(ns("download_odk"), "Download the ODK form (XLSForm)",
                               class = "btn-outline-primary mb-3", icon = icon("mobile-screen"))),
            p(class = "batch-hint", "When the data are in, download them and upload the file in step 2:",
              "from ODK Central the .zip of CSV files; from KoboToolbox an XLS export with 'XML values and headers'.")
          )
        ),
        # Step 2 ------------------------------------------------------------------
        batch_step_card(
          ns, 2, "Upload your filled file", id = "step2",
          p("Upload the survey data (the .zip from ODK Central, or the XLS export from KoboToolbox) or the filled Excel template.",
            "We check the data before anything is calculated, and tell you exactly what to fix.",
            "'What's missing' then shows the values used where farms left something blank, so you can change them before running."),
          # (drawn by the server so that "Start a new batch" can empty it; the batch
          # is named after the uploaded file)
          uiOutput(ns("file_ui")),
          uiOutput(ns("match_ui")),
          uiOutput(ns("conversions_ui")),
          uiOutput(ns("check_result")),
          shinyjs::hidden(div(
            id = ns("problems_section"),
            div(
              class = "d-flex flex-wrap align-items-center justify-content-between gap-2 mt-3 mb-2",
              radioButtons(ns("problem_filter"), NULL, inline = TRUE,
                           choices = c("All" = "all", "Must fix" = "error", "To check" = "warning")),
              downloadButton(ns("download_report"), "Download the check report",
                             class = "btn-outline-primary btn-sm", icon = icon("download"))
            ),
            DT::DTOutput(ns("problems"))
          ))
          ,
          uiOutput(ns("missing_ui"))
        ),
        # Step 3 ------------------------------------------------------------------
        batch_step_card(
          ns, 3, "Run the batch", id = "step3",
          uiOutput(ns("run_locked")),
          div(
            id = ns("run_settings"),
            selectizeInput(ns("exclude_types"), "Leave out livestock types (optional)", choices = NULL,
                           multiple = TRUE, width = "100%",
                           options = list(placeholder = "e.g. the calves")),
            p(class = "batch-hint mt-n2", "The types chosen are removed, with their feed baskets, from every farm;",
              "the other animals are not changed."),
            uiOutput(ns("run_summary")),
            actionButton(ns("run"), "Run the batch", icon = icon("play"),
                         class = "btn-primary batch-run-btn")
          ),
          shinyjs::hidden(div(
            id = ns("progress_section"),
            uiOutput(ns("progress")),
            actionButton(ns("cancel"), "Cancel the run", icon = icon("stop"),
                         class = "btn-outline-danger btn-sm mt-2")
          ))
        ),
        # Step 4 ------------------------------------------------------------------
        batch_step_card(
          ns, 4, "Results", id = "step4",
          uiOutput(ns("results_locked")),
          shinyjs::hidden(div(
            id = ns("results_section"),
            uiOutput(ns("result_tiles")),
            p(class = "batch-hint mt-3 mb-0",
              strong("Farm summary:"), " one row per farm with the main indicators, group averages and the run log. ",
              strong("Detailed results:"), " every result table of CLEANED's batch processing (land, feed quality, energy, ",
              "nitrogen, water, products, manure, all greenhouse gas tables, biomass, soil carbon, waste), all farms in one workbook. ",
              strong("Per farm:"), " the same tables as one workbook per farm, with the farm's study object."),
            div(
              class = "d-flex flex-wrap gap-2 my-3",
              downloadButton(ns("download_results"), "Farm summary (.xlsx)",
                             class = "btn-primary", icon = icon("file-excel")),
              downloadButton(ns("download_detailed"), "Detailed results (.xlsx)",
                             class = "btn-outline-primary", icon = icon("table")),
              downloadButton(ns("download_all"), "Detailed results per farm (.zip)",
                             class = "btn-outline-primary", icon = icon("file-zipper")),
              downloadButton(ns("download_input_used"), "Batch file with the values used (.xlsx)",
                             class = "btn-outline-primary", icon = icon("file-excel")),
              downloadButton(ns("download_database"), "Parameters database used (.zip)",
                             class = "btn-outline-primary", icon = icon("database")),
              actionButton(ns("to_dashboard"), "Open farms in the Dashboard",
                           class = "btn-outline-primary", icon = icon("chart-column"))
            ),
            div(class = "batch-group-by d-flex flex-wrap align-items-end gap-3",
                selectInput(ns("group_by"), "Group farms by", choices = NULL, width = "280px", selectize = FALSE),
                # the age ranges are chosen here (they differ by region and study)
                conditionalPanel(
                  condition = sprintf("input['%s'] == '__age__'", ns("group_by")),
                  selectInput(ns("age_scheme"), "Age groups", width = "340px", selectize = FALSE,
                              choices = c(stats::setNames(names(batch_age_schemes), vapply(batch_age_schemes, `[[`, "", "label")),
                                          "Custom: type where each group starts" = "custom"))
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == '__age__' && input['%s'] == 'custom'", ns("group_by"), ns("age_scheme")),
                  textInput(ns("age_starts"), "Groups start at (years, separated by commas)",
                            value = "18, 30, 50", width = "280px")
                )),
            tabsetPanel(
              id = ns("results_tabs"),
              tabPanel(
                "Compare farms",
                div(
                  class = "pt-3",
                  selectInput(ns("indicator"), "Indicator", choices = NULL, width = "100%", selectize = FALSE),
                  plotOutput(ns("indicator_plot"), height = "340px"),
                  p(class = "batch-hint", "Each dot is a farm; the box shows the middle half of the farms and the line the median."),
                  DT::DTOutput(ns("farm_table"))
                )
              ),
              tabPanel(
                "Group averages",
                div(class = "pt-3", uiOutput(ns("group_note")),
                    radioButtons(ns("group_stat"), "Show", inline = TRUE,
                                 choices = c("Median (typical farm)" = "median", "Mean" = "mean")),
                    DT::DTOutput(ns("group_table")))
              ),
              tabPanel(
                title = uiOutput(ns("issues_tab_title"), inline = TRUE),
                value = "issues",
                div(class = "pt-3", uiOutput(ns("issues")))
              )
            )
          ))
        )
      ),
      # ------ Your batches -----------------------------------------------------------
      column(
        width = 4,
        div(
          class = "card batch-history",
          h2("Your batches"),
          p(class = "batch-hint", "Batches are saved, so you can come back to the results later.",
            "A batch keeps running if you close the page."),
          actionButton(ns("new_batch"), "Start a new batch", icon = icon("plus"),
                       class = "btn-outline-primary btn-sm w-100 mb-3"),
          uiOutput(ns("history"))
        )
      )
    )
  )
}
