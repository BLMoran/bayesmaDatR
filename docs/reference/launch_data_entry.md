# Launch the interactive workbook data manager

Upload a bayesmaDatR workbook (or map columns from any XLSX, CSV, RDA or
RDS file), then manage the Study Details, Outcome Details, Risk of Bias
and INSPECT-SR sections, validate the data, and download the workbook or
analysis-ready data for one or more outcomes (a standard layout, or
MetaStan's arm-based format).

## Usage

``` r
launch_data_entry(..., launch.browser = TRUE)
```

## Arguments

- ...:

  Arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), e.g.
  `port`.

- launch.browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html); `TRUE`
  opens the system web browser.

## Value

Called for its side effect; returns the value of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Details

The app opens in your web browser by default: RStudio's viewer cannot
save downloaded files.
