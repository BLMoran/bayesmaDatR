# Split paired values reported in one cell

Studies often report an interquartile range or a range as a single cell,
e.g. `"2, 5"` or `"[2.5; 7]"`. `split_iqr()` returns the first and third
quartiles and `split_range()` the minimum and maximum. Cells that do not
hold exactly two numbers give `NA` for both.

## Usage

``` r
split_iqr(x, ..., sep = default_pair_sep())

split_range(x, ..., sep = default_pair_sep())
```

## Arguments

- x:

  Character vector of paired values.

- ...:

  Unused; must be empty.

- sep:

  Regular expression separating the two values. The default accepts
  commas, semicolons, `" to "` and en or em dashes. Hyphens are not
  split on by default because they clash with negative numbers; use
  `sep = "\\s+-\\s+"` for values like `"2 - 5"`.

## Value

A tibble with one row per element of `x` and columns `q1` and `q3`
(`split_iqr()`) or `min` and `max` (`split_range()`).

## Examples

``` r
split_iqr(c("2, 5", "[1.5; 3]", "4 to 9", "7"))
#> # A tibble: 4 × 2
#>      q1    q3
#>   <dbl> <dbl>
#> 1   2       5
#> 2   1.5     3
#> 3   4       9
#> 4  NA      NA
split_range("0 to 12")
#> # A tibble: 1 × 2
#>     min   max
#>   <dbl> <dbl>
#> 1     0    12
```
