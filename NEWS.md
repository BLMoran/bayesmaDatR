# bayesmaDatR 0.1.0

* Initial release: workbook-to-model data manager for Bayesian meta-analysis.
* `launch_data_entry()` is the card-based workbook manager from bayesma's data
  entry app, rebuilt on the bayesmaDatR schema: upload a workbook or map
  columns from any XLSX/CSV/RDA/RDS file, edit Study Details, Outcome Details,
  Risk of Bias (all robbR tools, with coloured rating buttons and guides) and
  INSPECT-SR, sync studies across sections, validate, preview and download
  bayesma data, and download the workbook, single sections or a template.
* Study Details gains `n_periods`; the mean cluster size is
  N / (n_clusters x n_periods), with 1 period for cluster trials and 2 for
  cluster crossover trials when it is missing.
* `merge_ma_data()` (and the single-sheet workbook) now has one row per study,
  with each outcome's columns side by side, suffixed by outcome (e.g.
  `n_int_hosp_los`).
* `write_ma_workbook(object_name = )` and the app name the data frame(s) in an
  `.rda` file; prefixed section names (e.g. `los_studies`) are recognised on
  upload.
* Missing variances (e.g. no SD or IQR) no longer stop an export: the study is
  kept, `yi`/`sei` are left empty where they cannot be calculated, and a
  warning names the missing fields. `MedD` failures also warn.
* Exports and Study Details downloads use a fixed column order: study_id,
  author, year, trial_design, control, intervention, n_int, n_ctrl,
  n_clusters, rho_w, rho_b, rho_wp (empty design columns omitted), extra study
  columns; then each outcome's outcome_id, measure, n_int ... q3_ctrl,
  design_effect, yi, sei; then risk-of-bias columns.
* Import: "Other columns to keep" brings in any study columns (e.g. subgroup1,
  subgroup2) under their own names. `subgroup` and `follow_up` are no longer
  template fields.
* `launch_data_entry()` opens in the web browser by default, as RStudio's
  viewer cannot save downloads; failed downloads show the error in the app.
* The app's export "Format" offers a package-neutral "Standard" layout and
  MetaStan.
* `as_bayesma()` exports several outcomes side by side in wide format: one row
  per study, outcome columns suffixed with the outcome (`yi_hosp_los`,
  `sei_icu_los`, ...). Study rows with no reported data for an outcome are
  left out of it with a message. The app's export takes several outcomes.
* `design_effect` is always calculated (read-only in the app, not imported).
* Import: each outcome block has its own sheet, header row and study
  identifiers; the column tag re-matches when changed and matches tags in
  the middle of names (e.g. `Mean_LOS_ICU_int`).
* Study Details holds study-level `n_int`, `n_ctrl`, `n_clusters`, `rho_w`,
  `rho_b` and `rho_wp`; outcome rows inherit them where their own cells are
  empty (`fill_study_defaults()`), so outcome-specific values only need
  entering where they differ. `cluster_size` is replaced by `n_clusters`; the
  mean cluster size is derived as N / clusters (N / (2 x clusters) for cluster
  crossover trials). The Outcome Data download includes the inherited values.
* `write_ma_workbook()` and the app's workbook download can save an `.rda`
  file (one data frame per section, or one merged data frame).
* Missing design-effect inputs give a design effect of 1; cluster crossover
  trials without `rho_b` use `rho_b = 0`. Outcomes carry a `design_effect`
  column, filled on import, by `fill_design_effect()` or the app's
  "Recalculate design effect" button; a typed value is used as given.
* Trial design codes are normalised (e.g. `CXRCT`, `XRCT`, `cluster crossover`
  become `cxo_rct`, `xo_rct`); CLARITY's yes/no answers map to its
  judgements (e.g. "Probably no" is Probably high).
* Imports drop columns that were skipped (left empty); "Manage columns" has a
  "Drop empty columns" button. Tables have row checkboxes for removing
  several rows at once.
* `trial_design` now lives in Study Details (an outcome-level value still
  overrides it). Outcomes gain `cluster_size` for the design effect.
* `compute_design_effect()` ports bayesma's design effects; `compute_effect_size()`,
  `as_bayesma()` and `as_MetaStan()` use effective sample sizes for `yi`/`sei`
  (`design_effect = "ignore"` to switch off) and keep the reported `n_int` and
  `n_ctrl` alongside `n_int_eff`, `n_ctrl_eff` and `design_effect`.
* `as_MetaStan()` exports one outcome in MetaStan's arm-based format (arm 1 =
  control, multi-arm supported) and, by default, runs
  `MetaStan::create_MetaStan_dat()`.
* `merge_ma_data()` combines every section into one data frame;
  `write_ma_workbook(layout = "single")` writes it as one sheet.
* App: hide skipped fields while mapping, fill a new Study Details column from
  the uploaded file, choose bayesma or MetaStan and the design effect when
  exporting, and download the workbook as separate sheets or one sheet.
* `split_iqr()` and `split_range()` split Q1/Q3 or min/max reported in one
  cell; the importer does this for `iqr_*` and `range_*` columns.
* Import mappings can take several columns per field, using the first
  non-empty value per study (e.g. an outcome-specific N, then the overall N).
  The outcome column tag (e.g. `hosplos`) matches columns such as
  `N_hosplos_int` ahead of `N_int`.
* Continuous outcomes carry mean/SD, median, quartiles and range together;
  `as_bayesma(estimate_means = "wan"/"qe")` and the app's export card estimate
  missing means and SDs for MD/SMD. `convert_median_to_mean()` fills both arms
  by default.
* Imports can take each section from its own file and sheet, with a header row
  per section so title rows above the column names are skipped. Recognised
  sheets (`studies`, `outcomes`, `risk_of_bias`, `inspect_sr`) can be loaded
  as-is without replacing the other sections.
* Workbook schema with outcome-specific sample sizes, mean/SD, median/quartile/
  range, event and person-time summaries, `trial_design` (`rct`, `xo_rct`,
  `c_rct`, `cxo_rct`), `rho_w`/`rho_b`/`rho_wp`, and `int_arm`/`ctrl_arm` for
  multi-arm studies.
* `read_ma_workbook()`, `write_ma_workbook()`, `map_ma_columns()`,
  `sync_ma_data()` and `validate_ma_data()`.
* Risk of bias for the eight robbR tools: `rob_tools()`, `rob_domains()`,
  `rob_judgements()` and `add_rob()` with `rob_level = "study"` or `"outcome"`.
* `compute_effect_size()` (OR, RR, IRR, MD, SMD, MedD) using bayesma's rules and
  `convert_median_to_mean()` (Wan or QE via metamedian).
* `select_outcome()`, `prepare_arm_data()`, `prepare_outcome()` and the
  `as_bayesma()` exporter (wide or long, one- or two-stage).
* INSPECT-SR sheet with the 21 assessment items and auxiliary calculation fields.
