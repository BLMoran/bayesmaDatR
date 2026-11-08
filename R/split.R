#' Split paired values reported in one cell
#'
#' Studies often report an interquartile range or a range as a single cell,
#' e.g. `"2, 5"` or `"[2.5; 7]"`. `split_iqr()` returns the first and third
#' quartiles and `split_range()` the minimum and maximum. Cells that do not hold
#' exactly two numbers give `NA` for both.
#'
#' @param x Character vector of paired values.
#' @param ... Unused; must be empty.
#' @param sep Regular expression separating the two values. The default accepts
#'   commas, semicolons, `" to "` and en or em dashes. Hyphens are not split on
#'   by default because they clash with negative numbers; use
#'   `sep = "\\s+-\\s+"` for values like `"2 - 5"`.
#' @return A tibble with one row per element of `x` and columns `q1` and `q3`
#'   (`split_iqr()`) or `min` and `max` (`split_range()`).
#' @examples
#' split_iqr(c("2, 5", "[1.5; 3]", "4 to 9", "7"))
#' split_range("0 to 12")
#' @export
split_iqr <- function(x, ..., sep = default_pair_sep()) {
  rlang::check_dots_empty()
  split_pair(x, c("q1", "q3"), sep)
}

#' @rdname split_iqr
#' @export
split_range <- function(x, ..., sep = default_pair_sep()) {
  rlang::check_dots_empty()
  split_pair(x, c("min", "max"), sep)
}

default_pair_sep <- function() "\\s*[,;\u2013\u2014]\\s*|\\s+to\\s+"

split_pair <- function(x, into, sep) {
  pairs <- x |>
    as.character() |>
    stringr::str_remove_all("[\\[\\]()]") |>
    stringr::str_trim() |>
    stringr::str_split(sep) |>
    purrr::map(\(p) suppressWarnings(as.numeric(stringr::str_trim(p)))) |>
    purrr::map(\(p) if (length(p) == 2L) p else c(NA_real_, NA_real_))
  tibble::tibble(
    !!into[1] := purrr::map_dbl(pairs, 1),
    !!into[2] := purrr::map_dbl(pairs, 2)
  )
}
