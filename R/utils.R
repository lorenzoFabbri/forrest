# Internal data utilities — not exported

#' Coerce data to a plain data.frame
#'
#' Handles [data.table][data.table::data.table] and
#' [tibble][tibble::tibble] inputs transparently.
#'
#' @param data A data frame, tibble, or data.table.
#' @return A plain `data.frame`.
#' @noRd
as_df <- function(data) {
  as.data.frame(data)
}

#' Validate that a required column name exists in the data
#'
#' @param data A data frame.
#' @param col Column name string to look up.
#' @param arg Argument name used in the error message.
#' @return `NULL` invisibly. Stops with an informative error if the column is
#'   absent.
#' @noRd
check_col <- function(data, col, arg) {
  if (!col %in% names(data)) {
    stop(
      sprintf("Column '%s' not found in data (argument `%s`).", col, arg),
      call. = FALSE
    )
  }
}

#' Validate an optional column name (NULL is allowed)
#'
#' @param data A data frame.
#' @param col Column name string, or `NULL`.
#' @param arg Argument name used in the error message.
#' @return `NULL` invisibly.
#' @noRd
check_col_opt <- function(data, col, arg) {
  if (!is.null(col)) check_col(data, col, arg)
}

#' Map a grouping vector to Okabe-Ito palette colours
#'
#' Returns a named character vector mapping each unique level of `groups` to a
#' colour from the Okabe-Ito colorblind-safe palette.
#'
#' @param groups Character vector of group labels.
#' @return Named character vector (`level -> hex colour`).
#' @noRd
group_colors <- function(groups) {
  lvls <- unique(groups)
  n    <- length(lvls)
  # Skip index 1 (white/near-white) to keep colours visible on white backgrounds
  pal  <- grDevices::palette.colors(n + 1L, "Okabe-Ito")[-1L]
  if (n > length(pal)) pal <- rep_len(pal, n)
  stats::setNames(pal[seq_len(n)], as.character(lvls))
}

#' Null-coalescing operator
#'
#' Returns `a` if it is not `NULL`, otherwise returns `b`.
#'
#' @param a,b Any R objects.
#' @return `a` if `!is.null(a)`, else `b`.
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b
