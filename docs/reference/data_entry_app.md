# The data manager as a Shiny app object

Returns the app run by
[`launch_data_entry()`](https://blmoran.github.io/bayesmaDatR/reference/launch_data_entry.md),
for hosting on a Shiny server or Posit Connect Cloud: an `app.R`
containing `bayesmaDatR::data_entry_app()` is enough to deploy it.

## Usage

``` r
data_entry_app()
```

## Value

A [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object.
