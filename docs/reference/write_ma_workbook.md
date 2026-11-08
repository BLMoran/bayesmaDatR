# Write an `ma_data` object to an Excel workbook or R data file

The file type follows the extension of `path`: `.xlsx` for Excel, `.rda`
(or `.RData`) for R. In an `.rda` file, each section is saved as its own
data frame (`studies`, `outcomes`, `risk_of_bias`, `inspect_sr`, or
`<object_name>_studies` and so on), or, for `layout = "single"`, one
data frame called `object_name`;
[`load()`](https://rdrr.io/r/base/load.html) restores them.

## Usage

``` r
write_ma_workbook(
  data,
  path,
  ...,
  layout = c("sheets", "single"),
  object_name = NULL
)
```

## Arguments

- data:

  An `ma_data` object or a named list of data frames.

- path:

  Output path ending in `.xlsx`, `.rda` or `.RData`.

- ...:

  Unused; must be empty.

- layout:

  `"sheets"` (default) writes one sheet (or data frame) per section,
  which
  [`read_ma_workbook()`](https://blmoran.github.io/bayesmaDatR/reference/read_ma_workbook.md)
  and the app can read back. `"single"` writes one combined table from
  [`merge_ma_data()`](https://blmoran.github.io/bayesmaDatR/reference/merge_ma_data.md).

- object_name:

  For `.rda` files: the name of the saved data frame
  (`layout = "single"`, default `"ma_data"`), or a prefix for the
  section data frames (`layout = "sheets"`, default none). Must be a
  valid R name.

## Value

`data`, invisibly.
