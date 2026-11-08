# Data Entry

The hosted data manager below runs the same app as
[`bayesmaDatR::launch_data_entry()`](https://blmoran.github.io/bayesmaDatR/reference/launch_data_entry.md).
Uploaded files are processed in your session only; run the app locally
for confidential data.

Upload a workbook or enter study data in each section below. **Click**
any cell to edit it. Use **Sync studies** to add every study in Study
Details to the other sections, then **Validate & export** an outcome for
analysis. Run `bayesmaDatR::launch_data_entry()` locally for an offline
version.

##### Upload Data

Choose file

Browse...

\

Map & Import

Choose the sheet and header row for each section, then map its columns.
Sections can come from separate files: import one file, then the next.
Sheets named studies, outcomes, risk_of_bias or inspect_sr can be loaded
as-is.

##### Study Details

Add study

Add column

Manage columns

Remove row

Clear section

`study_id` identifies each study across all sections; author and year
are descriptive. Leave study_id blank to build it from author and year.

##### Outcome Details

Add outcome

Manage columns

Remove row

Remove outcome

Clear section

Sync studies

One row per study, outcome and time point, each with its own n_int and
n_ctrl. For multi-arm studies add one row per comparison and name the
arms in int_arm and ctrl_arm. Leave n_int, n_ctrl, n_clusters or rho
cells empty to use the values in Study Details; fill them in for
outcome-specific values. design_effect is calculated from these and the
trial design (1 when they are missing) and updates automatically.

##### Risk of Bias

Tool:

RoB 2 ROBINS-I V2 ROBINS-E QUADAS-2 CLARITY RoB NMA PROBAST PROBAST+AI

Add row

Sync studies

Manage columns

Remove row

Clear section

Showing the selected tool's assessments. Set rob_level to `study`
(applies to every outcome of the study) or `outcome` (applies to that
outcome_id only).

##### INSPECT-SR

Insert:

No concerns

Some concerns

Serious concerns

Clear

Sync studies

Manage columns

Remove row

Clear section

D4.3, D4.6, D4.8 and D4.9 include input columns for the automated
checks.

##### Validate & Export

Format

Standard MetaStan

Design effect (cluster / crossover)

Apply Ignore

Outcome(s)

Time point

Measure

Each outcome's OR RR IRR MD SMD MedD

Model type

Two-stage One-stage

Layout

One row per study One row per arm

Studies reporting medians (MD / SMD)

Keep as reported Estimate mean and SD (Wan) Estimate mean and SD
(quantile estimation)

Validate data

Preview export

[ Download export](_w_605de51432bb47029460df10eaf34089/)

##### Download

###### Entered data

Workbook layout:

Separate sheets Single sheet

Workbook format:

Excel (.xlsx) R data (.rda)

Data frame name

Single sheet: the data frame's name. Separate sheets: a prefix, e.g.
`ma_data_studies` . Leave empty for plain section names.

[ Download workbook](_w_605de51432bb47029460df10eaf34089/)

Single sections as:

Excel (.xlsx) CSV (.csv) RDS (.rds)

[ Study Details](_w_605de51432bb47029460df10eaf34089/) [ Outcome
Data](_w_605de51432bb47029460df10eaf34089/) [ Risk of
Bias](_w_605de51432bb47029460df10eaf34089/) [
INSPECT-SR](_w_605de51432bb47029460df10eaf34089/)

The Outcome Data file includes the Study Details values (N, trial
design, clusters, rho) for any empty cells, so it stands on its own.
Separate sheets re-open here (xlsx or rda) and in `read_ma_workbook()`
(xlsx); a single sheet combines every section into one row per study and
outcome. An .rda file restores the sections as data frames with `load()`
.

###### Templates

A blank workbook with every column, using the selected risk-of-bias
tool.

[ Download template workbook](_w_605de51432bb47029460df10eaf34089/)

\
