# Package index

## Data manager

Manage review workbooks interactively.

- [`launch_data_entry()`](https://blmoran.github.io/bayesmaDatR/reference/launch_data_entry.md)
  : Launch the interactive workbook data manager
- [`data_entry_app()`](https://blmoran.github.io/bayesmaDatR/reference/data_entry_app.md)
  : The data manager as a Shiny app object

## Workbook management

Create, import, combine and export review workbooks.

- [`new_ma_data()`](https://blmoran.github.io/bayesmaDatR/reference/new_ma_data.md)
  : Create a Bayesian meta-analysis data object

- [`template_ma_data()`](https://blmoran.github.io/bayesmaDatR/reference/template_ma_data.md)
  : Create a blank workbook template

- [`read_ma_workbook()`](https://blmoran.github.io/bayesmaDatR/reference/read_ma_workbook.md)
  :

  Read a workbook into an `ma_data` object

- [`write_ma_workbook()`](https://blmoran.github.io/bayesmaDatR/reference/write_ma_workbook.md)
  :

  Write an `ma_data` object to an Excel workbook or R data file

- [`map_ma_columns()`](https://blmoran.github.io/bayesmaDatR/reference/map_ma_columns.md)
  : Rename a section's columns to the package schema

- [`merge_ma_data()`](https://blmoran.github.io/bayesmaDatR/reference/merge_ma_data.md)
  : Combine all sections into one data frame

- [`sync_ma_data()`](https://blmoran.github.io/bayesmaDatR/reference/sync_ma_data.md)
  : Align study records across sections

- [`fill_study_defaults()`](https://blmoran.github.io/bayesmaDatR/reference/fill_study_defaults.md)
  : Fill outcome rows with their study's default values

- [`split_iqr()`](https://blmoran.github.io/bayesmaDatR/reference/split_iqr.md)
  [`split_range()`](https://blmoran.github.io/bayesmaDatR/reference/split_iqr.md)
  : Split paired values reported in one cell

## Validation and assessment fields

Check workbook structure and add risk-of-bias and INSPECT-SR fields.

- [`validate_ma_data()`](https://blmoran.github.io/bayesmaDatR/reference/validate_ma_data.md)
  : Validate managed meta-analysis data
- [`add_rob()`](https://blmoran.github.io/bayesmaDatR/reference/add_rob.md)
  : Add risk-of-bias assessments
- [`rob_tools()`](https://blmoran.github.io/bayesmaDatR/reference/rob_tools.md)
  : Built-in risk-of-bias tools
- [`rob_domains()`](https://blmoran.github.io/bayesmaDatR/reference/rob_domains.md)
  : Domain columns for a risk-of-bias tool
- [`rob_judgements()`](https://blmoran.github.io/bayesmaDatR/reference/rob_judgements.md)
  : Accepted judgements for a risk-of-bias tool
- [`inspect_sr_columns()`](https://blmoran.github.io/bayesmaDatR/reference/inspect_sr_columns.md)
  : INSPECT-SR workbook columns
- [`inspect_sr_labels()`](https://blmoran.github.io/bayesmaDatR/reference/inspect_sr_labels.md)
  : INSPECT-SR field labels

## Design effects

Adjust cluster and crossover trials for clustering.

- [`compute_design_effect()`](https://blmoran.github.io/bayesmaDatR/reference/compute_design_effect.md)
  : Design effect for clustered and crossover trials
- [`fill_design_effect()`](https://blmoran.github.io/bayesmaDatR/reference/fill_design_effect.md)
  : Add the design effect column

## Prepare model data

Select outcomes, calculate effect sizes and export analysis-ready data.

- [`select_outcome()`](https://blmoran.github.io/bayesmaDatR/reference/select_outcome.md)
  : Select outcome rows
- [`prepare_arm_data()`](https://blmoran.github.io/bayesmaDatR/reference/prepare_arm_data.md)
  : Prepare arm-level data
- [`prepare_outcome()`](https://blmoran.github.io/bayesmaDatR/reference/prepare_outcome.md)
  : Prepare a single outcome for analysis
- [`compute_effect_size()`](https://blmoran.github.io/bayesmaDatR/reference/compute_effect_size.md)
  : Estimate study-level effect sizes
- [`convert_median_to_mean()`](https://blmoran.github.io/bayesmaDatR/reference/convert_median_to_mean.md)
  : Estimate means and standard deviations from medians and quantiles
- [`as_bayesma()`](https://blmoran.github.io/bayesmaDatR/reference/as_bayesma.md)
  : Export outcomes for bayesma
- [`as_MetaStan()`](https://blmoran.github.io/bayesmaDatR/reference/as_MetaStan.md)
  : Export one outcome for MetaStan
