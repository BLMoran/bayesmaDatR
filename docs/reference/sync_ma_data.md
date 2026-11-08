# Align study records across sections

Adds a row to `studies` for every `study_id` used in another section but
not yet listed there, and adds any template columns missing from a
section.

## Usage

``` r
sync_ma_data(data)
```

## Arguments

- data:

  An `ma_data` object.

## Value

The synchronised `ma_data` object.
