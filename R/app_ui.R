app_css <- function() {
  "
html, body { height: auto !important; min-height: 0 !important; overflow: visible !important; }
.container-fluid, .container, .bslib-page-fill, .html-fill-container {
  height: auto !important; min-height: 0 !important; overflow: visible !important;
}
body { font-size: 0.9rem; color: #2E3A4A; }
.section-card { margin-bottom: 1.5rem; }
.card-header-custom {
  background: #3191bf; color: white; padding: 0.6rem 1rem;
  display: flex; align-items: center; justify-content: space-between; gap: 8px;
  flex-wrap: wrap; border-radius: 4px 4px 0 0;
}
.card-header-custom h5 { margin: 0; font-size: 1rem; font-weight: 600; }
.card-header-custom .form-group { margin-bottom: 0; }
.app-blurb {
  background: #EAF4FB; border-left: 4px solid #3191bf; padding: 10px 16px;
  margin-bottom: 20px; border-radius: 0 4px 4px 0; font-size: 0.88rem; color: #2E3A4A;
}
.btn-rob-low      { background:#77bb41; color:white; border:none; }
.btn-rob-low:hover { background:#5e9430; color:white; }
.btn-rob-lowish   { background:#a6d36f; color:#2E3A4A; border:none; }
.btn-rob-lowish:hover { background:#8fbf57; color:#2E3A4A; }
.btn-rob-some     { background:#f5c518; color:#2E3A4A; border:none; }
.btn-rob-some:hover { background:#d4a814; color:#2E3A4A; }
.btn-rob-serious  { background:#e07000; color:white; border:none; }
.btn-rob-serious:hover { background:#c06000; color:white; }
.btn-rob-high     { background:#e32400; color:white; border:none; }
.btn-rob-high:hover { background:#c21e00; color:white; }
.btn-rob-critical { background:#8b0000; color:white; border:none; }
.btn-rob-critical:hover { background:#6a0000; color:white; }
.btn-rob-unclear  { background:#adb5bd; color:white; border:none; }
.btn-rob-unclear:hover { background:#8f979e; color:white; }
.btn-inspect-none    { background:#77bb41; color:white; border:none; }
.btn-inspect-none:hover { background:#5e9430; color:white; }
.btn-inspect-some    { background:#f5c518; color:#2E3A4A; border:none; }
.btn-inspect-some:hover { background:#d4a814; color:#2E3A4A; }
.btn-inspect-serious { background:#e32400; color:white; border:none; }
.btn-inspect-serious:hover { background:#c21e00; color:white; }
.selection-info {
  padding: 6px 12px; background:#EAF4FB; border-left: 3px solid #3191bf;
  font-size: 0.85rem; margin-bottom: 8px; border-radius: 0 4px 4px 0;
}
.btn-bar { display:flex; flex-wrap:wrap; gap:6px; align-items:center; margin-bottom:8px; }
.btn-bar .label { font-weight:600; font-size:0.85rem; margin-right:4px; color:#2E3A4A; }
.dl-row { display:flex; flex-wrap:wrap; gap:10px; align-items:flex-start; }
.map-row { display:flex; align-items:center; gap:8px; margin-bottom:4px; }
.map-row .form-group { margin-bottom: 0; }
.map-label { min-width:220px; font-size:0.85rem; font-weight:500; color:#2E3A4A; }
table.dataTable { border-collapse: collapse !important; }
table.dataTable thead tr th {
  background: #f0f4f8 !important; border: 1px solid #c8d6e0 !important;
  font-weight: 600; color: #2E3A4A; white-space: nowrap;
}
table.dataTable.stripe tbody tr.odd > * { box-shadow: none; background-color: #f7fafd; }
table.dataTable.stripe tbody tr.even > * { box-shadow: none; background-color: #ffffff; }
table.dataTable.cell-border tbody td {
  border-right: 1px solid #dce8ef !important; border-bottom: 1px solid #dce8ef !important;
  padding: 10px 10px !important; line-height: 1.5; cursor: cell;
}
table.dataTable tbody tr { height: 44px; }
table.dataTable tbody td.locked-cell { color: #6c7a89; font-style: italic; cursor: default; }
table.dataTable tbody tr:hover > * { box-shadow: none !important; }
table.dataTable tbody tr.selected > * {
  box-shadow: inset 0 0 0 9999px rgba(49,145,191,0.08) !important; color: inherit !important;
}
table.dataTable tbody td input[type='text'], table.dataTable tbody td input:not([type]),
table.dataTable tbody td textarea, table.dataTable tbody td select {
  outline: none !important; border: 2px solid #3191bf !important; border-radius: 3px;
  background: #ffffff !important; padding: 3px 6px !important; width: 100% !important;
  box-sizing: border-box; font-size: 0.9rem; color: #2E3A4A;
  box-shadow: 0 0 0 3px rgba(49,145,191,0.15) !important;
}
.import-box {
  resize: vertical; overflow: auto; min-height: 90px; border: 2px dashed #c8d6e0;
  border-radius: 6px; padding: 10px 12px; background: #fafcfe;
}
.imp-outcome-block {
  border: 1px solid #c8d6e0; border-radius: 5px; padding: 10px 12px;
  margin-bottom: 10px; background: #fafcfe;
}
.btn-help {
  background: none !important; border: none !important; box-shadow: none !important;
  color: rgba(255,255,255,0.8) !important; padding: 0 2px !important;
  font-size: 1rem; line-height: 1; cursor: pointer;
}
.btn-help:hover { color: white !important; }
.btn-help:focus { outline: none !important; box-shadow: none !important; }
"
}

toggle_skipped_js <- function() {
  paste0(
    "var btn = $(this); var pane = btn.closest('.tab-pane'); var hide = !btn.data('hiding');",
    "pane.find('.map-row').each(function() {",
    "  var sel = $(this).find('select')[0];",
    "  var empty = sel && sel.selectize ? sel.selectize.items.length === 0 : false;",
    "  $(this).toggle(!(hide && empty));",
    "});",
    "btn.data('hiding', hide);",
    "btn.html(hide ? '<i class=\"fa fa-eye\"></i> Show skipped fields' : '<i class=\"fa fa-eye-slash\"></i> Hide skipped fields');"
  )
}

blur_js <- function() {
  "$('.dataTable input:focus, .dataTable select:focus, .dataTable textarea:focus').blur();"
}

section_card <- function(header, ...) {
  shiny::div(
    class = "section-card",
    shiny::div(class = "card-header-custom", header),
    shiny::div(class = "card-body border border-top-0 rounded-bottom p-3", ...)
  )
}

header_title <- function(title, help_id = NULL, help_title = NULL) {
  shiny::div(
    style = "display:flex; align-items:center; gap:6px;",
    shiny::h5(title),
    if (!is.null(help_id)) {
      shiny::actionButton(help_id, "", icon = shiny::icon("circle-question"),
                          class = "btn-help", title = help_title)
    }
  )
}

section_buttons <- function(prefix, add_label = NULL, add_column = FALSE, extra = NULL) {
  shiny::div(
    if (!is.null(add_label)) {
      shiny::actionButton(paste0("add_", prefix), add_label, icon = shiny::icon("plus"),
                          class = "btn-sm btn-light")
    },
    if (add_column) {
      shiny::actionButton(paste0("add_col_", prefix), "Add column", icon = shiny::icon("columns"),
                          class = "btn-sm btn-outline-light ms-2")
    },
    shiny::actionButton(paste0("manage_", prefix), "Manage columns", icon = shiny::icon("sliders"),
                        class = "btn-sm btn-outline-light ms-2"),
    shiny::actionButton(paste0("del_", prefix), "Remove row", icon = shiny::icon("minus"),
                        class = "btn-sm btn-danger ms-2"),
    extra,
    shiny::actionButton(paste0("clear_", prefix), "Clear section", icon = shiny::icon("trash"),
                        class = "btn-sm btn-danger ms-2")
  )
}

sync_button <- function(id) {
  shiny::actionButton(id, "Sync studies", icon = shiny::icon("arrows-rotate"),
                      class = "btn-sm btn-outline-secondary ms-3", onclick = blur_js())
}

format_choices <- function() c("Excel (.xlsx)" = "xlsx", "CSV (.csv)" = "csv", "RDS (.rds)" = "rds")

app_ui <- function() {
  shiny::fluidPage(
    theme = bslib::bs_theme(bootswatch = "cosmo", primary = "#3191bf", secondary = "#3191bf"),
    title = "bayesmaDatR data manager",
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(app_css())),
      shiny::tags$script(src = "https://cdn.jsdelivr.net/npm/@iframe-resizer/child@5")
    ),
    shiny::div(
      style = "max-width:1400px; margin:0 auto; padding:16px;",

      shiny::div(
        class = "app-blurb",
        "Upload a workbook or enter study data in each section below. ",
        shiny::strong("Click"), " any cell to edit it. Use ", shiny::strong("Sync studies"),
        " to add every study in Study Details to the other sections, then ",
        shiny::strong("Validate & export"), " an outcome for analysis. Run ",
        shiny::code("bayesmaDatR::launch_data_entry()"), " locally for an offline version."
      ),

      section_card(
        shiny::h5("Upload Data"),
        shiny::div(
          class = "import-box",
          shiny::fluidRow(
            shiny::column(5, shiny::fileInput(
              "upload_file", "Choose file",
              accept = c(".xlsx", ".xls", ".csv", ".rda", ".rdata", ".rds"),
              placeholder = "XLSX workbook, CSV, RDA, or RDS"
            )),
            shiny::column(3, shiny::br(), shiny::actionButton(
              "upload_map_btn", "Map & Import", icon = shiny::icon("file-import"),
              class = "btn-primary btn-sm mt-1"
            ))
          ),
          shiny::p(class = "text-muted small mb-0",
                   "Choose the sheet and header row for each section, then map its columns. ",
                   "Sections can come from separate files: import one file, then the next. ",
                   "Sheets named studies, outcomes, risk_of_bias or inspect_sr can be loaded as-is.")
        )
      ),

      section_card(
        shiny::tagList(shiny::h5("Study Details"), section_buttons("study", "Add study", add_column = TRUE)),
        shiny::p(class = "text-muted small mb-2",
                 shiny::code("study_id"), " identifies each study across all sections; ",
                 "author and year are descriptive. Leave study_id blank to build it from author and year."),
        DT::DTOutput("study_tbl")
      ),

      section_card(
        shiny::tagList(shiny::h5("Outcome Details"), section_buttons(
          "outcome", "Add outcome",
          extra = shiny::actionButton("del_outcome_group", "Remove outcome",
                                      icon = shiny::icon("table-columns"), class = "btn-sm btn-warning ms-2")
        )),
        shiny::div(
          class = "btn-bar",
          shiny::uiOutput("otype_bar", inline = TRUE),
          sync_button("sync_outcome")
        ),
        shiny::uiOutput("outcome_filter_ui"),
        shiny::p(class = "text-muted small mb-2",
                 "One row per study, outcome and time point, each with its own n_int and n_ctrl. ",
                 "For multi-arm studies add one row per comparison and name the arms in int_arm and ctrl_arm. ",
                 "Leave n_int, n_ctrl, n_clusters or rho cells empty to use the values in Study Details; ",
                 "fill them in for outcome-specific values. design_effect is calculated from these and the ",
                 "trial design (1 when they are missing) and updates automatically."),
        DT::DTOutput("outcome_tbl")
      ),

      section_card(
        shiny::tagList(
          header_title("Risk of Bias", "rob_help", "Risk-of-bias tool guide"),
          shiny::div(
            style = "display:flex; gap:8px; align-items:center;",
            shiny::span("Tool:", style = "font-size:0.85rem;"),
            shiny::selectInput("rob_tool", NULL, width = "220px",
                               choices = stats::setNames(rob_tools(), rob_tool_labels()[rob_tools()]))
          )
        ),
        shiny::uiOutput("rob_btn_bar"),
        shiny::uiOutput("rob_sel_info"),
        shiny::div(
          class = "btn-bar mb-2",
          shiny::actionButton("add_rob", "Add row", icon = shiny::icon("plus"),
                              class = "btn-sm btn-outline-secondary"),
          sync_button("sync_rob"),
          shiny::actionButton("manage_rob", "Manage columns", icon = shiny::icon("sliders"),
                              class = "btn-sm btn-outline-secondary ms-2"),
          shiny::actionButton("del_rob", "Remove row", icon = shiny::icon("minus"),
                              class = "btn-sm btn-danger ms-2"),
          shiny::actionButton("clear_rob", "Clear section", icon = shiny::icon("trash"),
                              class = "btn-sm btn-danger ms-2")
        ),
        shiny::p(class = "text-muted small mb-2",
                 "Showing the selected tool's assessments. Set rob_level to ", shiny::code("study"),
                 " (applies to every outcome of the study) or ", shiny::code("outcome"),
                 " (applies to that outcome_id only)."),
        DT::DTOutput("rob_tbl")
      ),

      section_card(
        header_title("INSPECT-SR", "inspect_help", "INSPECT-SR scoring guide"),
        shiny::div(
          class = "btn-bar",
          shiny::span(class = "label", "Insert:"),
          shiny::actionButton("btn_isp_none", "No concerns", class = "btn-sm btn-inspect-none"),
          shiny::actionButton("btn_isp_some", "Some concerns", class = "btn-sm btn-inspect-some"),
          shiny::actionButton("btn_isp_serious", "Serious concerns", class = "btn-sm btn-inspect-serious"),
          shiny::actionButton("btn_isp_clear", "Clear", class = "btn-sm btn-outline-secondary"),
          sync_button("sync_inspect"),
          shiny::actionButton("manage_inspect", "Manage columns", icon = shiny::icon("sliders"),
                              class = "btn-sm btn-outline-secondary ms-2"),
          shiny::actionButton("del_inspect", "Remove row", icon = shiny::icon("minus"),
                              class = "btn-sm btn-danger ms-2"),
          shiny::actionButton("clear_inspect", "Clear section", icon = shiny::icon("trash"),
                              class = "btn-sm btn-danger ms-2")
        ),
        shiny::uiOutput("inspect_sel_info"),
        shiny::p(class = "text-muted small mb-2",
                 "D4.3, D4.6, D4.8 and D4.9 include input columns for the automated checks."),
        DT::DTOutput("inspect_tbl")
      ),

      section_card(
        shiny::h5("Validate & Export"),
        shiny::fluidRow(
          shiny::column(3, shiny::selectInput("exp_package", "Format", choices = c("Standard" = "standard", "MetaStan" = "MetaStan"))),
          shiny::column(4, shiny::selectInput("exp_deff", "Design effect (cluster / crossover)",
                                              choices = c("Apply" = "apply", "Ignore" = "ignore")))
        ),
        shiny::uiOutput("exp_package_note"),
        shiny::fluidRow(
          shiny::column(3, shiny::selectizeInput("exp_outcome", "Outcome(s)", choices = character(), multiple = TRUE,
                                                 options = list(placeholder = "Pick one or more"))),
          shiny::column(2, shiny::selectInput("exp_timepoint", "Time point", choices = character())),
          shiny::column(2, shiny::selectInput("exp_measure", "Measure",
                                              choices = c("Each outcome's" = "auto", effect_measures()))),
          shiny::column(2, shiny::selectInput("exp_model_type", "Model type",
                                              choices = c("Two-stage" = "two_stage", "One-stage" = "one_stage"))),
          shiny::column(3, shiny::selectInput("exp_format", "Layout",
                                              choices = c("One row per study" = "wide",
                                                          "One row per arm" = "long")))
        ),
        shiny::fluidRow(
          shiny::column(5, shiny::selectInput(
            "exp_estimate", "Studies reporting medians (MD / SMD)",
            choices = c("Keep as reported" = "none",
                        "Estimate mean and SD (Wan)" = "wan",
                        "Estimate mean and SD (quantile estimation)" = "qe")
          ))
        ),
        shiny::div(
          class = "btn-bar",
          shiny::actionButton("validate_btn", "Validate data", icon = shiny::icon("clipboard-check"),
                              class = "btn-sm btn-outline-primary"),
          shiny::actionButton("preview_btn", "Preview export", icon = shiny::icon("eye"),
                              class = "btn-sm btn-outline-primary"),
          shiny::downloadButton("dl_export", "Download export", class = "btn-sm btn-primary")
        ),
        shiny::uiOutput("export_msg"),
        DT::DTOutput("export_tbl")
      ),

      section_card(
        shiny::h5("Download"),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::h6("Entered data"),
            shiny::radioButtons("wb_layout", "Workbook layout:", inline = TRUE,
                                choices = c("Separate sheets" = "sheets", "Single sheet" = "single")),
            shiny::radioButtons("wb_format", "Workbook format:", inline = TRUE,
                                choices = c("Excel (.xlsx)" = "xlsx", "R data (.rda)" = "rda")),
            shiny::conditionalPanel(
              "input.wb_format == 'rda'",
              shiny::textInput("wb_object", "Data frame name", value = "ma_data", width = "260px"),
              shiny::p(class = "text-muted small mt-n2",
                       "Single sheet: the data frame's name. Separate sheets: a prefix, e.g. ",
                       shiny::code("ma_data_studies"), ". Leave empty for plain section names.")
            ),
            shiny::downloadButton("dl_workbook", "Download workbook", class = "btn-sm btn-primary mb-2"),
            shiny::radioButtons("dl_format", "Single sections as:", choices = format_choices(),
                                selected = "xlsx", inline = TRUE),
            shiny::div(
              class = "dl-row mb-2",
              shiny::downloadButton("dl_studies", "Study Details", class = "btn-sm btn-outline-primary"),
              shiny::downloadButton("dl_outcomes", "Outcome Data", class = "btn-sm btn-outline-primary"),
              shiny::downloadButton("dl_risk_of_bias", "Risk of Bias", class = "btn-sm btn-outline-primary"),
              shiny::downloadButton("dl_inspect_sr", "INSPECT-SR", class = "btn-sm btn-outline-primary")
            ),
            shiny::p(class = "text-muted small mt-1 mb-0",
                     "The Outcome Data file includes the Study Details values (N, trial design, clusters, rho) ",
                     "for any empty cells, so it stands on its own. ",
                     "Separate sheets re-open here (xlsx or rda) and in ", shiny::code("read_ma_workbook()"),
                     " (xlsx); a single sheet combines every section into one row per study and outcome. ",
                     "An .rda file restores the sections as data frames with ", shiny::code("load()"), ".")
          ),
          shiny::column(
            5,
            shiny::h6("Templates"),
            shiny::p(class = "text-muted small mb-1",
                     "A blank workbook with every column, using the selected risk-of-bias tool."),
            shiny::downloadButton("tpl_all", "Download template workbook", class = "btn-sm btn-outline-primary")
          )
        )
      ),
      shiny::tags$br()
    )
  )
}
