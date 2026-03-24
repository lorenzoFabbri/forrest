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
  n <- length(lvls)
  # Skip index 1 (white/near-white) to keep colours visible on white backgrounds
  pal <- grDevices::palette.colors(n + 1L, "Okabe-Ito")[-1L]
  if (n > length(pal)) {
    pal <- rep_len(pal, n)
  }
  stats::setNames(pal[seq_len(n)], as.character(lvls))
}

#' Assign visual group IDs for dodge layout
#'
#' Consecutive rows with the same label form one dodge group.
#' Header/spacer rows are always singleton groups.
#'
#' @param lbl Character vector of row labels.
#' @param is_header Logical vector; `TRUE` for header/spacer rows.
#' @return Integer vector of group IDs (1, 2, ..., n_groups).
#' @noRd
compute_dodge_groups <- function(lbl, is_header) {
  n <- length(lbl)
  group_ids <- integer(n)
  g <- 0L
  prev_lbl <- ""
  for (i in seq_len(n)) {
    if (is_header[i] || lbl[i] != prev_lbl) {
      g <- g + 1L
      prev_lbl <- if (is_header[i]) "" else lbl[i]
    }
    group_ids[i] <- g
  }
  group_ids
}

#' Map shape levels to pch values
#'
#' Returns a named integer vector mapping each unique level of `shapes` to a
#' point character value, cycling through a predefined set.
#'
#' @param shapes Character vector of shape labels.
#' @return Named integer vector (`level -> pch`).
#' @noRd
group_shapes <- function(shapes) {
  lvls <- unique(shapes)
  pchs <- c(16L, 17L, 15L, 18L, 8L, 10L, 3L, 4L)
  if (length(lvls) > length(pchs)) pchs <- rep_len(pchs, length(lvls))
  stats::setNames(pchs[seq_along(lvls)], as.character(lvls))
}

#' Null-coalescing operator
#'
#' Returns `a` if it is not `NULL`, otherwise returns `b`.
#'
#' @param a,b Any R objects.
#' @return `a` if `!is.null(a)`, else `b`.
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b
