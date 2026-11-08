# Rename a section's columns to the package schema

Rename a section's columns to the package schema

## Usage

``` r
map_ma_columns(
  data,
  mapping,
  section = c("outcomes", "studies", "risk_of_bias", "inspect_sr")
)
```

## Arguments

- data:

  An `ma_data` object.

- mapping:

  Named character vector, `c(new_name = "old_name")`. Old names that are
  not present are ignored.

- section:

  Section to rename.

## Value

`data` with the renamed section.
