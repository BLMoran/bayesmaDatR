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
#'   commas, semicolons, `" to "`, en or em dashes, and hyphens that follow a
#'   number (so `"2-5"` and `"-3 - -1"` split, while a leading minus sign is
#'   kept).
#' @param names Names of the two output columns. Inside
#'   [dplyr::mutate()] or [dplyr::transmute()], an unnamed call adds both
#'   columns, e.g. `split_iqr(iqr_int, names = c("q1_int", "q3_int"))`.
#' @return A tibble with one row per element of `x` and two columns: `q1` and
#'   `q3` (`split_iqr()`) or `min` and `max` (`split_range()`) unless `names`
#'   says otherwise.
#' @examples
#' split_iqr(c("2, 5", "[1.5; 3]", "4 to 9", "3-6", "7"))
#' split_range("0 to 12")
#'
#' tibble::tibble(iqr_int = c("3-6", "2, 5")) |>
#'   dplyr::mutate(split_iqr(iqr_int, names = c("q1_int", "q3_int")))
#' @export
split_iqr <- function(x, ..., sep = default_pair_sep(), names = c("q1", "q3")) {
  rlang::check_dots_empty()
  split_pair(x, names, sep)
}

#' @rdname split_iqr
#' @export
split_range <- function(x, ..., sep = default_pair_sep(), names = c("min", "max")) {
  rlang::check_dots_empty()
  split_pair(x, names, sep)
}

default_pair_sep <- function() "\\s*[,;\u2013\u2014]\\s*|\\s+to\\s+|(?<=\\d)\\s*-\\s*"

split_pair <- function(x, into, sep, call = rlang::caller_env()) {
  if (!is.character(into) || length(into) != 2 || anyNA(into) || anyDuplicated(into)) {
    cli::cli_abort("{.arg names} must be two different column names.", call = call)
  }
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
