#' Launch the interactive workbook data manager
#'
#' Upload a bayesmaDatR workbook (or map columns from any XLSX, CSV, RDA or RDS
#' file), then manage the Study Details, Outcome Details, Risk of Bias and
#' INSPECT-SR sections, validate the data, and download the workbook or
#' analysis-ready data for one or more outcomes (a standard layout, or
#' MetaStan's arm-based format).
#'
#' The app opens in your web browser by default: RStudio's viewer cannot save
#' downloaded files.
#'
#' @param ... Arguments passed to [shiny::runApp()], e.g. `port`.
#' @param launch.browser Passed to [shiny::runApp()]; `TRUE` opens the system
#'   web browser.
#' @return Called for its side effect; returns the value of [shiny::runApp()].
#' @export
launch_data_entry <- function(..., launch.browser = TRUE) {
  shiny::runApp(data_entry_app(), ..., launch.browser = launch.browser)
}

#' The data manager as a Shiny app object
#'
#' Returns the app run by [launch_data_entry()], for hosting on a Shiny server
#' or Posit Connect Cloud: an `app.R` containing `bayesmaDatR::data_entry_app()`
#' is enough to deploy it.
#'
#' @return A [shiny::shinyApp()] object.
#' @export
data_entry_app <- function() {
  shiny::shinyApp(app_ui(), app_server)
}

app_sections <- function() {
  c(study = "studies", outcome = "outcomes", rob = "risk_of_bias", inspect = "inspect_sr")
}

protected_columns <- function() c("study_id", "outcome_id", "rob_tool", "rob_level")

column_type <- function(x) {
  if (inherits(x, "Date")) "date" else if (is.integer(x)) "integer" else if (is.numeric(x)) "numeric" else "text"
}

parse_numeric_if_possible <- function(x) {
  number <- suppressWarnings(as.numeric(x))
  if (all(is.na(number) == is_blank(x))) number else x
}

app_server <- function(input, output, session) {
  rv <- shiny::reactiveValues(
    tables = unclass(template_ma_data()),
    trigger = list(study = 0, outcome = 0, rob = 0, inspect = 0),
    sel = list(rob = NULL, inspect = NULL),
    upload = NULL,
    upload_n = 0,
    upload_file = NULL,
    files = list(),
    steps = list(),
    export = NULL
  )
  upload_dir <- tempfile("bayesmaDatR_upload_")
  session$onSessionEnded(\() unlink(upload_dir, recursive = TRUE))
  outcome_type_filter <- shiny::reactiveVal("all")
  new_outcome_type <- shiny::reactiveVal("binary")
  imp_outcomes <- shiny::reactiveVal(list(list(idx = 1L, type = "binary")))
  imp_next_idx <- shiny::reactiveVal(2L)
  imp_rob_tool <- shiny::reactiveVal("rob2")
  max_imp_outcomes <- 8L

  refresh <- function(prefix = names(app_sections())) {
    for (p in prefix) rv$trigger[[p]] <- rv$trigger[[p]] + 1
  }
  run_step <- function(step) {
    data <- tryCatch(run_ma_step(current_data(), step, rv$files), error = function(e) {
      shiny::showNotification(cli::ansi_strip(conditionMessage(e)), type = "error", duration = 12)
      NULL
    })
    if (is.null(data)) return(FALSE)
    rv$tables <- unclass(data)
    rv$steps <- c(rv$steps, list(step))
    TRUE
  }
  section_step <- function(fn, prefix, ...) new_step(fn, app_sections()[[prefix]], ...)
  get_section <- function(prefix) rv$tables[[app_sections()[[prefix]]]]
  current_data <- function() {
    x <- rv$tables
    new_ma_data(x$studies, x$outcomes, x$risk_of_bias, x$inspect_sr)
  }
  study_ids <- function() {
    ids <- rv$tables$studies$study_id
    unique(ids[!is_blank(ids)])
  }

  # ---- Views -----------------------------------------------------------------

  view_for <- function(prefix, df) {
    rows <- seq_len(nrow(df))
    cols <- names(df)
    if (prefix == "outcome") {
      filt <- input$outcome_filter %||% "(All outcomes)"
      if (!identical(filt, "(All outcomes)")) rows <- which(df$outcome_id %in% filt)
      type <- outcome_type_filter()
      if (type != "all") {
        cols <- intersect(c(outcome_key_columns(), outcome_type_columns(type)), names(df))
      }
    }
    if (prefix == "rob") {
      tool <- input$rob_tool %||% "rob2"
      rows <- which(df$rob_tool %in% tool | is_blank(df$rob_tool))
      all_domains <- unique(unlist(purrr::map(rob_tools(), rob_domains)))
      keys <- intersect(c("study_id", "outcome_id", "rob_tool", "rob_level"), names(df))
      cols <- c(keys, intersect(rob_domains(tool), names(df)),
                setdiff(names(df), c(keys, all_domains)))
    }
    list(rows = rows, cols = cols)
  }

  view_df <- function(prefix, df, view) df[view$rows, view$cols, drop = FALSE]

  display_names <- function(prefix, cols) {
    if (prefix != "inspect") return(cols)
    labels <- inspect_sr_labels()
    dplyr::coalesce(unname(labels[cols]), cols)
  }

  for (prefix in names(app_sections())) {
    local({
      p <- prefix
      tbl_id <- paste0(p, "_tbl")

      output[[tbl_id]] <- DT::renderDT({
        rv$trigger[[p]]
        if (p == "outcome") list(outcome_type_filter(), input$outcome_filter)
        if (p == "rob") input$rob_tool
        shiny::isolate({
          df <- get_section(p)
          view <- view_for(p, df)
          dt_editable(view_df(p, df, view), colnames = display_names(p, view$cols),
                      locked = intersect("design_effect", view$cols))
        })
      })

      shiny::observeEvent(input[[paste0(tbl_id, "_cell_edit")]], {
        edits <- as.data.frame(input[[paste0(tbl_id, "_cell_edit")]])
        view <- view_for(p, get_section(p))
        rows <- view$rows[as.integer(edits$row)]
        cols <- view$cols[as.integer(edits$col)]
        ok <- !is.na(rows) & !is.na(cols) & cols != "design_effect"
        if (any(ok)) {
          run_step(section_step("edit_ma_cells", p, row = as.numeric(rows[ok]), column = cols[ok],
                                value = blank_to_na(edits$value[ok])))
        }
        refresh(p)
      }, priority = 100)

      shiny::observeEvent(input[[paste0("del_", p)]], {
        selected <- unlist(input[[paste0(tbl_id, "_checked")]])
        if (!length(selected)) {
          shiny::showNotification("Tick the boxes of the rows to remove first.", type = "warning")
          return()
        }
        drop <- as.numeric(sort(view_for(p, get_section(p))$rows[unlist(selected)]))
        run_step(section_step("remove_ma_rows", p, rlang::expr(dplyr::row_number() %in% !!drop)))
        refresh(p)
      })

      shiny::observeEvent(input[[paste0("clear_", p)]], {
        shiny::showModal(shiny::modalDialog(
          title = paste0("Clear ", section_title(p), "?"),
          shiny::p("This removes ", shiny::strong("all rows"), " from this section. ",
                   "Other sections are not affected. This cannot be undone."),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(paste0("confirm_clear_", p), "Clear", class = "btn-danger")
          ),
          easyClose = TRUE
        ))
      })

      shiny::observeEvent(input[[paste0("confirm_clear_", p)]], {
        run_step(section_step("remove_ma_rows", p, TRUE))
        rv$sel[p] <- list(NULL)
        shiny::removeModal()
        refresh(p)
        shiny::showNotification(paste("Cleared", section_title(p), "."), type = "message")
      })

      shiny::observeEvent(input[[paste0("manage_", p)]], open_manage_modal(p))
    })
  }

  section_title <- function(prefix) {
    c(study = "Study Details", outcome = "Outcome Details",
      rob = "Risk of Bias", inspect = "INSPECT-SR")[[prefix]]
  }

  # ---- Manage columns ----------------------------------------------------------

  manage_section <- shiny::reactiveVal(NULL)

  open_manage_modal <- function(prefix) {
    df <- get_section(prefix)
    cols <- names(df)
    if (!length(cols)) {
      shiny::showNotification("No columns to manage in this section.", type = "warning")
      return()
    }
    rows <- purrr::imap(cols, \(col, i) {
      shiny::div(
        class = "row g-2 align-items-center mb-1",
        shiny::div(class = "col-6", shiny::textInput(paste0("mc_name_", i),
                                                     if (i == 1) "Column name", value = col, width = "100%")),
        shiny::div(class = "col-4", shiny::selectInput(
          paste0("mc_type_", i), if (i == 1) "Type", width = "100%",
          choices = c(Text = "text", Integer = "integer", Numeric = "numeric", Date = "date"),
          selected = column_type(df[[col]])
        )),
        shiny::div(class = "col-2 text-end pt-3",
                   if (col %in% protected_columns()) {
                     shiny::tags$small("locked", class = "text-muted")
                   } else {
                     shiny::checkboxInput(paste0("mc_drop_", i), "drop", value = FALSE, width = "auto")
                   })
      )
    })
    manage_section(prefix)
    shiny::showModal(shiny::modalDialog(
      title = paste("Manage columns -", section_title(prefix)), size = "l", easyClose = TRUE,
      shiny::p(class = "small text-muted", "Rename, set type, or drop columns. ",
               shiny::strong(paste(intersect(protected_columns(), cols), collapse = ", ")),
               " cannot be renamed or dropped."),
      shiny::tagList(rows),
      footer = shiny::tagList(
        shiny::actionButton("drop_empty_cols", "Drop empty columns", icon = shiny::icon("broom"),
                            class = "btn-outline-danger me-auto"),
        shiny::modalButton("Cancel"),
        shiny::actionButton("apply_manage_cols", "Apply", class = "btn-primary")
      )
    ))
  }

  shiny::observeEvent(input$drop_empty_cols, {
    prefix <- manage_section()
    shiny::req(prefix)
    df <- get_section(prefix)
    sheet <- app_sections()[[prefix]]
    kept <- drop_empty_columns(df, keep = c(core_columns(sheet), protected_columns()))
    dropped <- setdiff(names(df), names(kept))
    if (length(dropped)) run_step(section_step("modify_ma_columns", prefix, drop = dropped))
    shiny::removeModal()
    refresh(prefix)
    shiny::showNotification(
      if (length(dropped)) paste("Dropped:", paste(dropped, collapse = ", ")) else "No empty columns to drop.",
      type = "message"
    )
  })

  shiny::observeEvent(input$apply_manage_cols, {
    prefix <- manage_section()
    shiny::req(prefix)
    df <- get_section(prefix)
    cols <- names(df)
    keep <- !purrr::map_lgl(seq_along(cols), \(i) isTRUE(input[[paste0("mc_drop_", i)]]) &&
                              !cols[i] %in% protected_columns())
    new_names <- purrr::map_chr(seq_along(cols), \(i) {
      nm <- trimws(input[[paste0("mc_name_", i)]] %||% cols[i])
      if (!nzchar(nm) || cols[i] %in% protected_columns()) cols[i] else nm
    })
    if (anyDuplicated(new_names[keep])) {
      shiny::showNotification("Column names must be unique. Changes not applied.", type = "error")
      return()
    }
    types <- purrr::map_chr(seq_along(cols), \(i) input[[paste0("mc_type_", i)]] %||% "text")
    changed_type <- types != purrr::map_chr(df, column_type)
    renamed <- keep & new_names != cols
    args <- purrr::compact(list(
      types = if (any(changed_type & keep)) stats::setNames(types, cols)[changed_type & keep],
      drop = if (any(!keep)) cols[!keep],
      rename = if (any(renamed)) stats::setNames(cols[renamed], new_names[renamed])
    ))
    if (length(args)) run_step(rlang::exec(section_step, "modify_ma_columns", prefix, !!!args))
    shiny::removeModal()
    refresh(prefix)
    shiny::showNotification(paste("Updated", section_title(prefix), "columns."), type = "message")
  })

  # ---- Study Details -----------------------------------------------------------

  shiny::observeEvent(input$add_study, {
    run_step(section_step("add_ma_rows", "study"))
    refresh("study")
  })

  shiny::observeEvent(input$add_col_study, {
    has_upload <- !is.null(rv$upload)
    shiny::showModal(shiny::modalDialog(
      title = "Add column to Study Details",
      shiny::textInput("new_col_name", "Column name", placeholder = "e.g. country (defaults to the source column)"),
      if (has_upload) {
        shiny::tagList(
          shiny::tags$hr(),
          shiny::strong("Fill from the uploaded file (optional)"),
          shiny::p(class = "text-muted small mb-1",
                   "Values are matched to studies by study_id, or by author and year."),
          shiny::fluidRow(
            shiny::column(8, shiny::selectInput("add_col_sheet", "Sheet", choices = names(rv$upload), width = "100%")),
            shiny::column(4, shiny::numericInput("add_col_hdr", "Header row", value = 1, min = 1, width = "100%"))
          ),
          shiny::uiOutput("add_col_source_ui")
        )
      } else {
        shiny::p(class = "text-muted small", "Upload a file first to fill the column from it.")
      },
      footer = shiny::tagList(shiny::modalButton("Cancel"),
                              shiny::actionButton("confirm_add_col", "Add", class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  shiny::observeEvent(input$add_col_sheet, {
    shiny::updateNumericInput(session, "add_col_hdr", value = guess_header_row(rv$upload[[input$add_col_sheet]]))
  })

  add_col_df <- shiny::reactive({
    shiny::req(rv$upload, input$add_col_sheet %in% names(rv$upload))
    sheet_from_raw(rv$upload[[input$add_col_sheet]], input$add_col_hdr %||% 1)
  })

  output$add_col_source_ui <- shiny::renderUI({
    cols <- names(add_col_df())
    key_select <- function(id, label, expected, choices) {
      shiny::selectInput(id, label, choices = c("(none)", choices), width = "100%",
                         selected = best_column_matches(expected, choices)[1] %|NA|% "(none)")
    }
    shiny::tagList(
      shiny::selectInput("add_col_source", "Source column", choices = c("(empty column)", cols), width = "100%"),
      shiny::fluidRow(
        shiny::column(4, key_select("add_col_key_id", "study_id column", "study_id", cols)),
        shiny::column(4, key_select("add_col_key_author", "Author column", "author", cols)),
        shiny::column(4, key_select("add_col_key_year", "Year column", "year", cols))
      )
    )
  })

  shiny::observeEvent(input$confirm_add_col, {
    df <- rv$tables$studies
    source <- input$add_col_source %||% "(empty column)"
    name <- trimws(input$new_col_name %||% "")
    if (!nzchar(name) && source != "(empty column)") name <- source
    if (!nzchar(name) || name %in% names(df)) {
      shiny::showNotification("Enter a new, unused column name.", type = "warning")
      return()
    }
    if (source == "(empty column)") {
      run_step(section_step("modify_ma_columns", "study", add = name))
    } else {
      keys <- c(study_id = input$add_col_key_id, author = input$add_col_key_author, year = input$add_col_key_year)
      keys <- keys[keys != "(none)"]
      matched <- study_column_values(df, add_col_df(), source, keys)$matched
      step <- new_step("import_study_column", path = rv$upload_file, source = source,
                       name = if (name != source) name, sheet = input$add_col_sheet,
                       header_row = as.numeric(input$add_col_hdr %||% 1), keys = keys)
      step$args <- purrr::compact(step$args)
      if (run_step(step)) {
        shiny::showNotification(paste0("Matched ", matched, " of ", nrow(df), " studies."),
                                type = if (matched) "message" else "warning")
      }
    }
    refresh("study")
    shiny::removeModal()
  })

  # ---- Outcome Details ---------------------------------------------------------

  shiny::observeEvent(input$otype_all, outcome_type_filter("all"))
  purrr::walk(names(outcome_types()), \(type) {
    shiny::observeEvent(input[[paste0("otype_", type)]], outcome_type_filter(type))
  })
  output$otype_bar <- shiny::renderUI({
    active <- outcome_type_filter()
    types <- c(all = "All", outcome_types())
    shiny::div(class = "btn-bar mb-0", shiny::span(class = "label", "Show type:"),
               purrr::imap(types, \(label, type) {
                 shiny::actionButton(paste0("otype_", type), label,
                                     class = paste("btn-sm", if (type == active) "btn-primary" else "btn-outline-secondary"))
               }))
  })

  output$outcome_filter_ui <- shiny::renderUI({
    ids <- unique(stats::na.omit(rv$tables$outcomes$outcome_id))
    if (!length(ids)) return(NULL)
    selected <- shiny::isolate(input$outcome_filter) %||% "(All outcomes)"
    shiny::div(
      class = "btn-bar",
      shiny::span(class = "label", "Show outcome:"),
      shiny::selectInput("outcome_filter", NULL, choices = c("(All outcomes)", ids),
                         selected = if (selected %in% ids) selected else "(All outcomes)", width = "240px")
    )
  })

  shiny::observeEvent(input$add_outcome, {
    new_outcome_type("binary")
    shiny::showModal(shiny::modalDialog(
      title = "Add outcome",
      shiny::fluidRow(
        shiny::column(6, shiny::textInput("new_outcome_id", "Outcome ID (required)",
                                          placeholder = "e.g. pain_24h")),
        shiny::column(6, shiny::textInput("new_outcome_name", "Outcome name",
                                          placeholder = "e.g. Pain at 24 hours"))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::textInput("new_outcome_timepoint", "Time point", placeholder = "e.g. 24h")),
        shiny::column(6, shiny::selectInput("new_outcome_measure", "Measure", choices = effect_measures()))
      ),
      shiny::div(class = "mb-2", shiny::tags$label("Outcome type:"), shiny::uiOutput("new_outcome_type_btns")),
      shiny::uiOutput("new_outcome_cols_preview"),
      shiny::p(class = "text-muted small mb-0",
               "One row is added for each study in Study Details."),
      footer = shiny::tagList(shiny::modalButton("Cancel"),
                              shiny::actionButton("confirm_add_outcome", "Add outcome", class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  output$new_outcome_type_btns <- shiny::renderUI({
    selected <- new_outcome_type()
    shiny::div(class = "btn-bar mt-1", purrr::imap(outcome_types(), \(label, type) {
      shiny::actionButton(paste0("new_otype_", type), label,
                          class = paste("btn-sm", if (type == selected) "btn-primary" else "btn-outline-secondary"))
    }))
  })

  purrr::walk(names(outcome_types()), \(type) {
    shiny::observeEvent(input[[paste0("new_otype_", type)]], {
      new_outcome_type(type)
      shiny::updateSelectInput(session, "new_outcome_measure", selected = outcome_type_measure(type))
    }, ignoreInit = TRUE)
  })

  output$new_outcome_cols_preview <- shiny::renderUI({
    shiny::p(class = "text-muted small mt-2", "Columns shown for this type: ",
             shiny::tags$em(paste(outcome_type_columns(new_outcome_type()), collapse = ", ")))
  })

  shiny::observeEvent(input$confirm_add_outcome, {
    id <- trimws(input$new_outcome_id %||% "")
    timepoint <- trimws(input$new_outcome_timepoint %||% "")
    timepoint <- if (nzchar(timepoint)) timepoint else NA_character_
    df <- rv$tables$outcomes
    if (!nzchar(id)) {
      shiny::showNotification("Enter an outcome ID.", type = "warning")
      return()
    }
    if (any(df$outcome_id %in% id & df$timepoint %in% timepoint)) {
      shiny::showNotification(paste0("'", id, "' already exists at this time point."), type = "warning")
      return()
    }
    ids <- study_ids()
    n <- max(length(ids), 1)
    name <- trimws(input$new_outcome_name %||% "")
    values <- purrr::compact(list(
      study_id = if (length(ids)) ids,
      outcome_id = id,
      outcome_name = if (nzchar(name)) name,
      timepoint = if (!is.na(timepoint)) timepoint,
      measure = input$new_outcome_measure
    ))
    if (!run_step(rlang::exec(section_step, "add_ma_rows", "outcome", !!!values))) return()
    outcome_type_filter(new_outcome_type())
    shiny::removeModal()
    refresh("outcome")
  })

  shiny::observeEvent(input$del_outcome_group, {
    ids <- unique(stats::na.omit(rv$tables$outcomes$outcome_id))
    if (!length(ids)) {
      shiny::showNotification("No outcomes to remove.", type = "warning")
      return()
    }
    shiny::showModal(shiny::modalDialog(
      title = "Remove outcome",
      shiny::selectInput("del_outcome_choice", "Outcome to remove (all its rows):", choices = ids,
                         size = min(6, length(ids)), selectize = FALSE),
      footer = shiny::tagList(shiny::modalButton("Cancel"),
                              shiny::actionButton("confirm_del_outcome", "Remove", class = "btn-danger")),
      easyClose = TRUE
    ))
  })

  shiny::observeEvent(input$confirm_del_outcome, {
    choice <- input$del_outcome_choice
    run_step(section_step("remove_ma_rows", "outcome", rlang::call2("==", rlang::sym("outcome_id"), choice)))
    shiny::removeModal()
    refresh("outcome")
    shiny::showNotification(paste0("Removed outcome: ", input$del_outcome_choice), type = "message")
  })

  shiny::observeEvent(input$sync_outcome, {
    run_step(section_step("sync_ma_section", "outcome"))
    refresh("outcome")
  })

  # ---- Risk of Bias --------------------------------------------------------------

  shiny::observeEvent(input$rob_tool, {
    missing_domains <- setdiff(rob_domains(input$rob_tool), names(rv$tables$risk_of_bias))
    if (length(missing_domains)) run_step(section_step("modify_ma_columns", "rob", add = missing_domains))
    rv$sel["rob"] <- list(NULL)
  })

  output$rob_btn_bar <- shiny::renderUI({
    tool <- shiny::req(input$rob_tool)
    levels <- rob_judgements(tool)
    shiny::tagList(
      shiny::div(
        class = "btn-bar",
        shiny::span(class = "label", "Insert:"),
        purrr::imap(levels, \(level, i) {
          shiny::actionButton(paste0("btn_rob_", tool, "_", i), level,
                              class = paste("btn-sm", rob_button_class(level)))
        }),
        shiny::actionButton("btn_rob_clear", "Clear", class = "btn-sm btn-outline-secondary")
      ),
      shiny::p(class = "text-muted small mb-1",
               "Click a domain cell in the table, then click a button above to fill it.")
    )
  })

  purrr::walk(rob_tools(), \(tool) {
    purrr::iwalk(rob_judgements(tool), \(level, i) {
      shiny::observeEvent(input[[paste0("btn_rob_", tool, "_", i)]], fill_cell("rob", level))
    })
  })
  shiny::observeEvent(input$btn_rob_clear, fill_cell("rob", NA_character_))

  shiny::observeEvent(input$add_rob, {
    run_step(section_step("add_ma_rows", "rob", rob_tool = input$rob_tool, rob_level = "study"))
    refresh("rob")
  })

  shiny::observeEvent(input$sync_rob, {
    run_step(section_step("sync_ma_section", "rob", rob_tool = input$rob_tool))
    refresh("rob")
  })

  shiny::observeEvent(input$rob_help, {
    tool <- input$rob_tool %||% "rob2"
    shiny::showModal(shiny::modalDialog(
      title = shiny::tagList(shiny::icon("circle-question"), " ", rob_tool_labels()[[tool]], " guide"),
      size = "l", rob_help_content(tool), easyClose = TRUE, footer = shiny::modalButton("Close")
    ))
  })

  # ---- INSPECT-SR -------------------------------------------------------------

  shiny::observeEvent(input$btn_isp_none, fill_cell("inspect", "No concerns"))
  shiny::observeEvent(input$btn_isp_some, fill_cell("inspect", "Some concerns"))
  shiny::observeEvent(input$btn_isp_serious, fill_cell("inspect", "Serious concerns"))
  shiny::observeEvent(input$btn_isp_clear, fill_cell("inspect", NA_character_))

  shiny::observeEvent(input$sync_inspect, {
    run_step(section_step("sync_ma_section", "inspect"))
    refresh("inspect")
  })

  shiny::observeEvent(input$inspect_help, {
    shiny::showModal(shiny::modalDialog(
      title = shiny::tagList(shiny::icon("circle-question"), " INSPECT-SR Scoring Guide"),
      size = "xl", inspect_help_content(), easyClose = TRUE, footer = shiny::modalButton("Close")
    ))
  })

  # ---- Cell selection and fill buttons ----------------------------------------

  purrr::walk(c("rob", "inspect"), \(p) {
    shiny::observeEvent(input[[paste0(p, "_tbl_cell_clicked")]], {
      clicked <- input[[paste0(p, "_tbl_cell_clicked")]]
      if (!length(clicked$row)) return()
      df <- get_section(p)
      view <- view_for(p, df)
      row <- view$rows[clicked$row]
      col <- view$cols[clicked$col]
      fillable <- if (p == "rob") rob_domains(input$rob_tool) else inspect_sr_items()
      rv$sel[p] <- list(if (isTRUE(!is.na(row) && !is.na(col) && col %in% fillable)) list(row = row, col = col) else NULL)
    })
  })

  fill_cell <- function(p, value) {
    sel <- rv$sel[[p]]
    if (is.null(sel)) {
      shiny::showNotification("Click a rating cell in the table first.", type = "warning")
      return()
    }
    df <- get_section(p)
    if (!isTRUE(sel$row <= nrow(df)) || !sel$col %in% names(df)) return()
    run_step(section_step("edit_ma_cells", p, row = as.numeric(sel$row), column = sel$col, value = value))
    refresh(p)
  }

  selection_label <- function(p) {
    sel <- rv$sel[[p]]
    if (is.null(sel)) return(NULL)
    df <- get_section(p)
    label <- if (p == "rob") {
      rob_domain_labels(input$rob_tool)[sel$col] %|NA|% sel$col
    } else {
      inspect_sr_labels()[sel$col] %|NA|% sel$col
    }
    shiny::div(class = "selection-info", "Selected: ", shiny::tags$strong(df$study_id[sel$row]),
               " - ", paste0(sel$col, ": ", label))
  }
  output$rob_sel_info <- shiny::renderUI(selection_label("rob"))
  output$inspect_sel_info <- shiny::renderUI(selection_label("inspect"))

  # ---- Upload ----------------------------------------------------------------

  mid <- function(x) paste0("u", rv$upload_n, "_", x)
  section_labels <- c(study = "Study Details", outcome = "Outcome Details",
                      rob = "Risk of Bias", inspect = "INSPECT-SR")

  section_sheet <- function(sec) {
    sheets <- names(rv$upload)
    chosen <- input[[mid(paste0("sheet_", sec))]]
    if (!is.null(chosen) && chosen %in% sheets) return(chosen)
    match_section_sheet(sec, sheets) %|NA|% sheets[1]
  }

  section_header_row <- function(sec) {
    row <- input[[mid(paste0("hdr_", sec))]]
    if (is.null(row) || is.na(row)) guess_header_row(rv$upload[[section_sheet(sec)]]) else row
  }

  section_df <- function(sec) {
    sheet_from_raw(rv$upload[[section_sheet(sec)]], section_header_row(sec))
  }

  shiny::observeEvent(input$upload_map_btn, {
    file <- input$upload_file
    if (is.null(file)) {
      shiny::showNotification("Choose a file first.", type = "warning")
      return()
    }
    sheets <- tryCatch(read_upload_sheets(file$datapath, file$name), error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
      NULL
    })
    if (is.null(sheets)) return()
    copy <- file.path(upload_dir, rv$upload_n + 1, file$name)
    dir.create(dirname(copy), recursive = TRUE, showWarnings = FALSE)
    file.copy(file$datapath, copy, overwrite = TRUE)
    rv$files[[file$name]] <- copy
    rv$upload_file <- file$name
    rv$upload <- sheets
    rv$upload_n <- rv$upload_n + 1
    imp_outcomes(list(list(idx = 1L, type = "binary")))
    imp_next_idx(2L)
    imp_rob_tool(input$rob_tool %||% "rob2")

    known <- recognised_sheets(names(sheets))
    shiny::showModal(shiny::modalDialog(
      title = paste("Import from", file$name), size = "l",
      if (length(known)) {
        shiny::div(
          class = "selection-info",
          "Recognised sheets: ",
          paste0(known, " \u2192 ", section_labels[names(known)], collapse = "; "),
          ". Use ", shiny::strong("Load recognised sheets as-is"),
          " if they already use bayesmaDatR column names; otherwise map the columns below."
        )
      },
      shiny::checkboxGroupInput(mid("sections"), "Import into:", inline = TRUE,
                                choices = stats::setNames(names(section_labels), section_labels),
                                selected = if (length(known)) names(known) else c("study", "outcome")),
      shiny::p(class = "text-muted small mb-1",
               "Each section can come from its own sheet, and its own header row if the sheet ",
               "has titles above the column names. Sections not ticked are left unchanged."),
      shiny::tags$hr(),
      shiny::uiOutput("upload_mapping_ui"),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        if (length(known)) {
          shiny::actionButton("load_known", "Load recognised sheets as-is", class = "btn-outline-primary")
        },
        shiny::actionButton("confirm_import", "Import mapped columns", class = "btn-primary")
      )
    ))
  })

  purrr::walk(names(section_labels), \(sec) {
    shiny::observeEvent(input[[mid(paste0("sheet_", sec))]], {
      raw <- rv$upload[[section_sheet(sec)]]
      shiny::updateNumericInput(session, mid(paste0("hdr_", sec)), value = guess_header_row(raw))
    }, ignoreInit = TRUE)

    output[[paste0("raw_preview_", sec)]] <- shiny::renderTable({
      shiny::req(rv$upload)
      raw <- rv$upload[[section_sheet(sec)]]
      shown <- raw[seq_len(min(6, nrow(raw))), seq_len(min(8, ncol(raw))), drop = FALSE]
      out <- as.data.frame(shown, stringsAsFactors = FALSE)
      names(out) <- paste0("C", seq_len(ncol(out)))
      out[is.na(out)] <- ""
      cbind(Row = seq_len(nrow(out)), out)
    }, spacing = "xs", striped = TRUE, bordered = TRUE)
  })

  map_rows <- function(expected, id_prefix, file_cols, prefix = "", force = FALSE, labels = NULL) {
    purrr::map(expected, \(col) {
      id <- paste0(id_prefix, col)
      previous <- intersect(shiny::isolate(input[[id]]), file_cols)
      selected <- if (length(previous) && !force) previous else best_column_matches(col, file_cols, prefix)
      shiny::div(
        class = "map-row",
        shiny::div(class = "map-label", col,
                   if (!is.null(labels[[col]])) shiny::tags$small(class = "text-muted d-block", labels[[col]])),
        shiny::div(style = "flex:1;", shiny::selectizeInput(
          id, NULL, width = "100%", choices = unique(c(selected, file_cols)), selected = selected, multiple = TRUE,
          options = list(placeholder = "(skip)")
        ))
      )
    })
  }

  map_help <- function() {
    shiny::p(class = "text-muted small mb-1",
             "Pick more than one column to use the first non-empty value in each row, in the order picked ",
             "(e.g. the outcome-specific N, then the overall N). Leave empty to skip.")
  }

  id_block <- function(sec, file_cols) {
    shiny::div(
      class = "imp-outcome-block",
      shiny::strong("Study identifiers"),
      shiny::p(class = "text-muted small mb-1", "If study_id is skipped it is built from author and year."),
      map_rows(c("study_id", "author", "year"), mid(paste0(sec, "_id_")), file_cols)
    )
  }

  output$upload_mapping_ui <- shiny::renderUI({
    shiny::req(rv$upload)
    sections <- input[[mid("sections")]] %||% character()
    if (!length(sections)) return(shiny::p(class = "text-muted", "Tick at least one section."))
    sheets <- names(rv$upload)
    tabs <- purrr::map(sections, \(sec) {
      shiny::tabPanel(
        section_labels[[sec]],
        shiny::div(
          style = "padding-top:8px;",
          shiny::fluidRow(
            shiny::column(6, shiny::selectInput(
              mid(paste0("sheet_", sec)), "Sheet", choices = sheets,
              selected = shiny::isolate(section_sheet(sec)), width = "100%"
            )),
            shiny::column(3, shiny::numericInput(
              mid(paste0("hdr_", sec)), "Header row", min = 1, step = 1,
              value = shiny::isolate(section_header_row(sec)), width = "100%"
            ))
          ),
          shiny::p(class = "text-muted small mb-1",
                   "First rows of the sheet. Set Header row to the row holding the column names; ",
                   "rows above it (e.g. section titles) are ignored."),
          shiny::div(style = "max-height:200px; overflow:auto; font-size:0.8rem;",
                     shiny::tableOutput(paste0("raw_preview_", sec))),
          shiny::div(class = "btn-bar mt-2",
                     shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
                                        onclick = toggle_skipped_js(), shiny::icon("eye-slash"),
                                        " Hide skipped fields")),
          shiny::uiOutput(paste0("map_ui_", sec))
        )
      )
    })
    do.call(shiny::tabsetPanel, c(list(id = "mapping_tabs"), tabs))
  })

  output$map_ui_study <- shiny::renderUI({
    shiny::req(rv$upload)
    cols <- names(section_df("study"))
    shiny::tagList(
      id_block("study", cols),
      map_help(),
      shiny::div(style = "max-height:300px; overflow-y:auto;",
                 map_rows(setdiff(names(template_ma_data()$studies), c("study_id", "author", "year")),
                          mid("study_"), cols)),
      shiny::div(
        class = "imp-outcome-block mt-2",
        shiny::strong("Other columns to keep"),
        shiny::p(class = "text-muted small mb-1",
                 "Each column picked here is added to Study Details under its own name ",
                 "(e.g. subgroup1, subgroup2, follow_up)."),
        shiny::selectizeInput(mid("study_extra"), NULL, choices = cols, multiple = TRUE, width = "100%",
                              selected = intersect(shiny::isolate(input[[mid("study_extra")]]), cols),
                              options = list(placeholder = "(none)"))
      )
    )
  })

  block_sheet <- function(ii) {
    chosen <- input[[mid(paste0("osheet_", ii))]]
    if (!is.null(chosen) && chosen %in% names(rv$upload)) chosen else section_sheet("outcome")
  }

  block_header_row <- function(ii) {
    row <- input[[mid(paste0("ohdr_", ii))]]
    if (is.null(row) || is.na(row)) guess_header_row(rv$upload[[block_sheet(ii)]]) else row
  }

  block_df <- function(ii) sheet_from_raw(rv$upload[[block_sheet(ii)]], block_header_row(ii))

  output$map_ui_outcome <- shiny::renderUI({
    shiny::req(rv$upload)
    outcomes <- imp_outcomes()
    rematch_tick()
    forced <- rematch_pending
    rematch_pending <<- integer()
    sheets <- names(rv$upload)
    shiny::tagList(
      shiny::p(class = "text-muted small mb-2",
               "Each outcome can come from its own sheet and header row, with its own study identifiers."),
      purrr::map(outcomes, \(o) {
        ii <- o$idx
        text_value <- function(name) shiny::isolate(input[[mid(paste0(name, "_", ii))]]) %||% ""
        sheet <- block_sheet(ii)
        header <- block_header_row(ii)
        cols <- names(sheet_from_raw(rv$upload[[sheet]], header))
        shiny::div(
          class = "imp-outcome-block",
          shiny::fluidRow(
            shiny::column(3, shiny::textInput(mid(paste0("imp_id_", ii)), "Outcome ID", value = text_value("imp_id"),
                                              placeholder = "or map a column")),
            shiny::column(2, shiny::textInput(mid(paste0("imp_tp_", ii)), "Time point", value = text_value("imp_tp"))),
            shiny::column(3, shiny::textInput(mid(paste0("imp_prefix_", ii)), "Outcome column tag",
                                              value = text_value("imp_prefix"), placeholder = "e.g. hosplos",
                                              updateOn = "blur")),
            shiny::column(2,
              shiny::actionButton(paste0("imp_match_", ii), "Match", icon = shiny::icon("wand-magic-sparkles"),
                                  class = "btn-sm btn-outline-primary mt-4",
                                  title = "Re-match columns using the tag (e.g. N_hosplos_int before N_int)"),
              if (length(outcomes) > 1) {
                shiny::actionButton(paste0("imp_rm_outcome_", ii), "", icon = shiny::icon("trash"),
                                    class = "btn-sm btn-outline-danger mt-4")
              }
            )
          ),
          shiny::fluidRow(
            shiny::column(6, shiny::selectInput(mid(paste0("osheet_", ii)), "Sheet", choices = sheets,
                                                selected = sheet, width = "100%")),
            shiny::column(3, shiny::numericInput(mid(paste0("ohdr_", ii)), "Header row", value = header,
                                                 min = 1, step = 1, width = "100%", updateOn = "blur"))
          ),
          shiny::div(class = "btn-bar mb-1", shiny::span(class = "label", "Type:"),
                     purrr::imap(outcome_types(), \(lab, type) {
                       shiny::actionButton(paste0("imp_otype_", ii, "_", type), lab,
                                           class = paste("btn-sm", if (type == o$type) "btn-primary" else "btn-outline-secondary"))
                     })),
          shiny::div(
            style = "margin-bottom:6px;",
            shiny::strong(class = "small", "Study identifiers"),
            map_rows(c("study_id", "author", "year"), mid(paste0("outcome_", ii, "_id_")), cols,
                     force = ii %in% forced)
          ),
          shiny::p(class = "text-muted small mb-1",
                   "For a sheet with one row per study and outcome, map outcome_id (and timepoint) to columns ",
                   "instead of typing them."),
          map_help(),
          shiny::div(style = "max-height:300px; overflow-y:auto; margin-bottom:4px;",
                     map_rows(c("outcome_id", "outcome_name", "timepoint", "int_arm", "ctrl_arm",
                                outcome_type_columns(o$type),
                                if (o$type == "continuous") names(combined_cell_columns())),
                              mid(paste0("outcome_", ii, "_")), cols, prefix = text_value("imp_prefix"),
                              force = ii %in% forced,
                              labels = as.list(combined_cell_columns())))
        )
      }),
      if (length(outcomes) < max_imp_outcomes) {
        shiny::actionButton("imp_add_outcome", "Add outcome", icon = shiny::icon("plus"),
                            class = "btn-sm btn-outline-primary mt-1")
      }
    )
  })

  output$map_ui_rob <- shiny::renderUI({
    shiny::req(rv$upload)
    cols <- names(section_df("rob"))
    tool <- imp_rob_tool()
    shiny::tagList(
      id_block("rob", cols),
      shiny::div(class = "btn-bar mb-2", shiny::span(class = "label", "Tool:"),
                 purrr::map(rob_tools(), \(t) {
                   shiny::actionButton(paste0("imp_rob_tool_", t), rob_tool_labels()[[t]],
                                       class = paste("btn-sm", if (t == tool) "btn-primary" else "btn-outline-secondary"))
                 })),
      shiny::div(style = "max-height:300px; overflow-y:auto;",
                 map_rows(c("outcome_id", "rob_level", rob_domains(tool)), mid("rob_"), cols))
    )
  })

  output$map_ui_inspect <- shiny::renderUI({
    shiny::req(rv$upload)
    cols <- names(section_df("inspect"))
    shiny::tagList(
      id_block("inspect", cols),
      map_help(),
      shiny::div(style = "max-height:300px; overflow-y:auto;",
                 map_rows(setdiff(inspect_sr_columns(), c("study_id", "author", "year")), mid("inspect_"), cols))
    )
  })

  purrr::walk(paste0("map_ui_", names(section_labels)), \(id) {
    shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  })

  for (i in seq_len(max_imp_outcomes)) {
    local({
      ii <- i
      purrr::walk(names(outcome_types()), \(type) {
        shiny::observeEvent(input[[paste0("imp_otype_", ii, "_", type)]], {
          cur <- imp_outcomes()
          pos <- which(purrr::map_int(cur, "idx") == ii)
          if (length(pos)) {
            cur[[pos]]$type <- type
            imp_outcomes(cur)
          }
        }, ignoreInit = TRUE)
      })
      shiny::observeEvent(input[[paste0("imp_rm_outcome_", ii)]], {
        cur <- imp_outcomes()
        if (length(cur) > 1) imp_outcomes(cur[purrr::map_int(cur, "idx") != ii])
      }, ignoreInit = TRUE)
    })
  }
  purrr::walk(rob_tools(), \(tool) {
    shiny::observeEvent(input[[paste0("imp_rob_tool_", tool)]], imp_rob_tool(tool), ignoreInit = TRUE)
  })
  rematch_tick <- shiny::reactiveVal(0)
  rematch_pending <- integer()
  for (i in seq_len(max_imp_outcomes)) {
    local({
      ii <- i
      shiny::observeEvent(input[[paste0("imp_match_", ii)]], {
        rematch_pending <<- ii
        rematch_tick(rematch_tick() + 1)
      }, ignoreInit = TRUE)
      shiny::observeEvent(input[[mid(paste0("imp_prefix_", ii))]], {
        if (!nzchar(input[[mid(paste0("imp_prefix_", ii))]])) return()
        rematch_pending <<- ii
        rematch_tick(rematch_tick() + 1)
      }, ignoreInit = TRUE)
      shiny::observeEvent(input[[mid(paste0("osheet_", ii))]], {
        raw <- rv$upload[[block_sheet(ii)]]
        shiny::updateNumericInput(session, mid(paste0("ohdr_", ii)), value = guess_header_row(raw))
        rematch_pending <<- ii
      }, ignoreInit = TRUE)
    })
  }

  shiny::observeEvent(input$imp_add_outcome, {
    idx <- imp_next_idx()
    imp_outcomes(c(imp_outcomes(), list(list(idx = idx, type = "binary"))))
    imp_next_idx(idx + 1L)
  }, ignoreInit = TRUE)

  mapping_for <- function(id_prefix, fields, df) {
    columns <- purrr::map(fields, \(f) intersect(input[[paste0(id_prefix, f)]], names(df))) |>
      stats::setNames(fields)
    purrr::discard(columns, \(x) !length(x))
  }

  typed_value <- function(name) {
    value <- trimws(input[[mid(name)]] %||% "")
    if (nzchar(value)) value
  }

  sheet_spec <- function(sheet, header_row, ...) {
    purrr::compact(list(sheet = sheet, header_row = as.numeric(header_row), ...))
  }

  import_spec <- function(sections) {
    spec <- list()
    if ("study" %in% sections) {
      df <- section_df("study")
      fields <- setdiff(names(template_ma_data()$studies), c("study_id", "author", "year"))
      extra <- intersect(input[[mid("study_extra")]], names(df))
      spec$studies <- sheet_spec(
        section_sheet("study"), section_header_row("study"),
        columns = c(mapping_for(mid("study_id_"), c("study_id", "author", "year"), df),
                    mapping_for(mid("study_"), fields, df)),
        extra = if (length(extra)) extra
      )
    }
    if ("outcome" %in% sections) {
      spec$outcomes <- purrr::map(imp_outcomes(), \(o) {
        ii <- o$idx
        df <- block_df(ii)
        fields <- c("outcome_id", "outcome_name", "timepoint", "int_arm", "ctrl_arm", outcome_type_columns(o$type),
                    if (o$type == "continuous") names(combined_cell_columns()))
        c(list(type = o$type),
          sheet_spec(block_sheet(ii), block_header_row(ii),
                     outcome_id = typed_value(paste0("imp_id_", ii)),
                     timepoint = typed_value(paste0("imp_tp_", ii)),
                     columns = c(mapping_for(mid(paste0("outcome_", ii, "_id_")), c("study_id", "author", "year"), df),
                                 mapping_for(mid(paste0("outcome_", ii, "_")), fields, df))))
      })
    }
    if ("rob" %in% sections) {
      df <- section_df("rob")
      tool <- imp_rob_tool()
      spec$risk_of_bias <- c(list(tool = tool), sheet_spec(
        section_sheet("rob"), section_header_row("rob"),
        columns = c(mapping_for(mid("rob_id_"), c("study_id", "author", "year"), df),
                    mapping_for(mid("rob_"), c("outcome_id", "rob_level", rob_domains(tool)), df))
      ))
    }
    if ("inspect" %in% sections) {
      df <- section_df("inspect")
      spec$inspect_sr <- sheet_spec(
        section_sheet("inspect"), section_header_row("inspect"),
        columns = c(mapping_for(mid("inspect_id_"), c("study_id", "author", "year"), df),
                    mapping_for(mid("inspect_"), setdiff(inspect_sr_columns(), c("study_id", "author", "year")), df))
      )
    }
    spec
  }

  run_import <- function(spec, message) {
    if (!run_step(new_step("import_ma_data", path = rv$upload_file, spec = spec))) return()
    used <- intersect(unique(rv$tables$risk_of_bias$rob_tool), rob_tools())
    if (length(used) && !input$rob_tool %in% used) shiny::updateSelectInput(session, "rob_tool", selected = used[1])
    shiny::removeModal()
    refresh()
    shiny::showNotification(message, type = "message")
  }

  shiny::observeEvent(input$load_known, {
    shiny::req(rv$upload)
    known <- recognised_sheets(names(rv$upload))
    chosen <- intersect(names(known), input[[mid("sections")]] %||% names(known))
    if (!length(chosen)) return()
    spec <- purrr::map(chosen, \(sec) sheet_spec(known[[sec]], section_header_row(sec), as_is = TRUE)) |>
      stats::setNames(app_sections()[chosen])
    run_import(spec, paste0("Loaded: ", paste(section_labels[chosen], collapse = ", "), "."))
  })

  shiny::observeEvent(input$confirm_import, {
    shiny::req(rv$upload)
    sections <- input[[mid("sections")]] %||% character()
    if (!length(sections)) return()
    run_import(import_spec(sections), paste0("Imported: ", paste(section_labels[sections], collapse = ", "), "."))
  })

  # ---- Validate & export -----------------------------------------------------

  shiny::observe({
    ids <- unique(stats::na.omit(rv$tables$outcomes$outcome_id))
    selected <- intersect(shiny::isolate(input$exp_outcome), ids)
    shiny::updateSelectizeInput(session, "exp_outcome", choices = ids,
                                selected = if (length(selected)) selected else ids[1])
  })

  shiny::observeEvent(input$exp_outcome, {
    out <- rv$tables$outcomes
    rows <- out$outcome_id %in% input$exp_outcome
    timepoints <- unique(stats::na.omit(out$timepoint[rows]))
    shiny::updateSelectInput(session, "exp_timepoint", choices = c("(All)", timepoints),
                             selected = if (length(timepoints) == 1) timepoints else "(All)")
    measure <- unique(stats::na.omit(out$measure[rows]))
    shiny::updateSelectInput(session, "exp_measure",
                             selected = if (length(measure) == 1 && measure %in% effect_measures()) measure else "auto")
  })

  export_step <- function() {
    timepoint <- input$exp_timepoint
    timepoint <- if (is.null(timepoint) || timepoint %in% c("", "(All)")) NULL else timepoint
    if (!length(input$exp_outcome)) cli::cli_abort("Pick at least one outcome.")
    measure <- if (identical(input$exp_measure, "auto")) NULL else input$exp_measure
    shared <- list(outcome = input$exp_outcome, timepoint = timepoint, measure = measure,
                   estimate_means = input$exp_estimate %||% "none", design_effect = input$exp_deff %||% "apply")
    step <- if (identical(input$exp_package, "MetaStan")) {
      rlang::exec(new_step, "as_MetaStan", !!!shared, output = "wide")
    } else {
      rlang::exec(new_step, "as_bayesma", !!!shared, model_type = input$exp_model_type, format = input$exp_format)
    }
    step$args <- purrr::compact(step$args)
    step
  }

  export_data <- function() run_ma_step(current_data(), export_step())

  output$exp_package_note <- shiny::renderUI({
    if (identical(input$exp_package, "MetaStan")) {
      shiny::div(
        class = "selection-info",
        "MetaStan data: one row per study, arm 1 = control. Model type and layout do not apply. In R: ",
        shiny::code("MetaStan::create_MetaStan_dat(dat, armVars = c(mean = \"M\", std.err = \"SE\"), nArmsVar = \"nd\")"),
        " (", shiny::code("r"), "/", shiny::code("n"), " for OR/RR, ", shiny::code("count"), "/",
        shiny::code("exposure"), " for IRR), or ", shiny::code("as_MetaStan()"), " directly."
      )
    } else {
      shiny::div(
        class = "selection-info",
        "Standard data: one row per study (or per arm) with study details, each outcome's summaries, ",
        "design_effect, yi/sei for two-stage models, and risk of bias. It suits most packages ",
        "(e.g. bayesma, brms, metafor). In R: ", shiny::code("as_bayesma()"), "."
      )
    }
  })

  show_result <- function(df, msg, class) {
    rv$export <- list(df = df, msg = msg, class = class)
  }

  shiny::observeEvent(input$validate_btn, {
    issues <- validate_ma_data(current_data())
    if (nrow(issues)) {
      show_result(issues, paste(nrow(issues), "issue(s) found."), "alert alert-warning")
    } else {
      show_result(NULL, "No issues found.", "alert alert-success")
    }
  })

  shiny::observeEvent(input$preview_btn, {
    tryCatch(
      {
        notes <- character()
        data <- withCallingHandlers(export_data(), message = function(m) {
          notes <<- c(notes, cli::ansi_strip(conditionMessage(m)))
          invokeRestart("muffleMessage")
        }, warning = function(w) {
          notes <<- c(notes, paste("Warning:", cli::ansi_strip(conditionMessage(w))))
          invokeRestart("muffleWarning")
        })
        show_result(data, paste(c(paste(if (identical(input$exp_package, "MetaStan")) "MetaStan" else "Standard", "data ready."), notes), collapse = "\n"),
                    "alert alert-success")
      },
      error = function(e) show_result(NULL, cli::ansi_strip(conditionMessage(e)), "alert alert-danger")
    )
  })

  output$export_msg <- shiny::renderUI({
    res <- rv$export
    if (is.null(res)) return(NULL)
    shiny::div(class = paste(res$class, "py-2 small"), style = "white-space:pre-wrap;", res$msg)
  })

  output$export_tbl <- DT::renderDT({
    res <- rv$export
    shiny::req(res, res$df)
    DT::datatable(res$df, rownames = FALSE, class = "cell-border stripe", selection = "none",
                  options = list(pageLength = 15, scrollX = TRUE, dom = "tip"))
  })

  # ---- Downloads -------------------------------------------------------------

  dl_ext <- function() paste0(".", input$dl_format %||% "xlsx")

  section_download <- function(sheet) {
    df <- rv$tables[[sheet]]
    if (sheet == "outcomes") df <- fill_study_defaults(df, rv$tables$studies)
    if (sheet == "studies" && ncol(df)) df <- order_study_columns(df)
    df
  }

  safe_download <- function(write) {
    function(file) {
      tryCatch(write(file), error = function(e) {
        shiny::showNotification(paste("Download failed:", cli::ansi_strip(conditionMessage(e))),
                                type = "error", duration = NULL)
        stop(e)
      })
    }
  }

  output$dl_workbook <- shiny::downloadHandler(
    filename = function() paste0("bayesmaDatR_workbook.", input$wb_format %||% "xlsx"),
    content = safe_download(\(file) {
      name <- trimws(input$wb_object %||% "")
      write_ma_workbook(current_data(), file, layout = input$wb_layout %||% "sheets",
                        object_name = if (nzchar(name) && identical(input$wb_format, "rda")) name else NULL)
    })
  )

  purrr::walk(app_sections(), \(sheet) {
    output[[paste0("dl_", sheet)]] <- shiny::downloadHandler(
      filename = function() paste0(sheet, dl_ext()),
      content = safe_download(function(file) {
        df <- section_download(sheet)
        write_section_file(df, file, input$dl_format)
      })
    )
  })

  output$dl_export <- shiny::downloadHandler(
    filename = function() {
      paste0(tolower(input$exp_package %||% "standard"), "_", paste(input$exp_outcome, collapse = "_"), dl_ext())
    },
    content = function(file) {
      data <- tryCatch(export_data(), error = function(e) {
        show_result(NULL, cli::ansi_strip(conditionMessage(e)), "alert alert-danger")
        shiny::showNotification("Export failed; see the message on the export card.", type = "error")
        NULL
      })
      shiny::req(data)
      write_section_file(data, file, input$dl_format)
    }
  )

  output$dl_script <- shiny::downloadHandler(
    filename = function() "bayesmaDatR_script.R",
    content = safe_download(\(file) {
      export <- tryCatch(export_step(), error = function(e) NULL)
      data_dir <- input$script_dir %||% "."
      lines <- if (identical(input$script_style, "replay")) {
        format_ma_script(rv$steps, export, data_dir = data_dir)
      } else {
        tidy <- format_tidy_script(rv$steps, export, rv$files, data_dir = data_dir)
        expected <- if (!is.null(export)) tryCatch(suppressWarnings(suppressMessages(export_data())), error = \(e) NULL)
        if (is.null(expected)) tidy else note_tidy_check(tidy, check_tidy_script(tidy, rv$files, expected))
      }
      writeLines(lines, file)
    })
  )

  can_pick_dir <- function() {
    host <- session$clientData$url_hostname %||% ""
    host %in% c("127.0.0.1", "localhost", "::1") && folder_picker_available()
  }

  output$pick_script_dir_ui <- shiny::renderUI({
    if (!can_pick_dir()) return(NULL)
    shiny::actionButton("pick_script_dir", "Choose\u2026", icon = shiny::icon("folder-open"),
                        class = "btn-sm btn-outline-secondary", style = "margin-top:32px;")
  })

  shiny::observeEvent(input$pick_script_dir, {
    folder <- pick_folder("Folder holding the uploaded files")
    if (!is.null(folder)) shiny::updateTextInput(session, "script_dir", value = folder)
  })

  output$tpl_all <- shiny::downloadHandler(
    filename = function() "bayesmaDatR_template.xlsx",
    content = safe_download(\(file) write_ma_workbook(template_ma_data(rob_tool = input$rob_tool %||% "rob2"), file))
  )
}

`%|NA|%` <- function(x, y) {
  x <- unname(x)
  if (length(x) && !is.na(x)) x else y
}
