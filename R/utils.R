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
#' @param a Any R object.
#' @param b Any R object.
#' @return `a` if `!is.null(a)`, else `b`.
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Expand a tidy data frame into the display-ready frame used by forrest()
#'
#' Inserts bold section header rows (and optional sub-header rows and blank
#' spacer rows) wherever the value of the `section` (or `subsection`) column
#' changes.  This separates structural display concerns from the data, so
#' callers never need to insert NA rows by hand.
#'
#' @param df A plain `data.frame` (already coerced by `as_df()`).
#' @param estimate Column name string for point estimates.
#' @param lower Column name string for lower CI bounds.
#' @param upper Column name string for upper CI bounds.
#' @param label Column name string for row labels, or `NULL`.
#' @param is_summary Column name string for the summary-row flag, or `NULL`.
#' @param weight Column name string for weights, or `NULL`.
#' @param section Column name string whose run-length boundaries trigger
#'   section header insertion.
#' @param subsection Column name string for a second-level header, or `NULL`.
#' @param section_indent Logical; prepend two spaces to labels within sections.
#' @param section_spacer Logical; append a blank spacer row after each section.
#' @param cols Character vector of data-column names used in text panels
#'   (the *values* of the `cols` named vector passed to `forrest()`).
#' @param section_cols Named character vector: names are `cols` display headers,
#'   values are data-column names whose first non-NA entry in each section is
#'   shown in the section header row.  `NULL` means all header text cells are
#'   empty.
#' @param subsection_cols Like `section_cols` but for subsection header rows.
#'
#' @return A list with four elements:
#'   \describe{
#'     \item{`df`}{The expanded `data.frame`.}
#'     \item{`is_section_header`}{Logical vector: `TRUE` for auto-inserted
#'       section header rows.}
#'     \item{`is_subsection_header`}{Logical vector: `TRUE` for auto-inserted
#'       subsection header rows.}
#'     \item{`is_spacer`}{Logical vector: `TRUE` for auto-inserted blank spacer
#'       rows.}
#'   }
#' @noRd
build_sections <- function(
  df,
  estimate,
  lower,
  upper,
  label,
  is_summary,
  weight,
  section,
  subsection       = NULL,
  section_indent   = TRUE,
  section_spacer   = TRUE,
  cols             = character(0),
  section_cols     = NULL,
  subsection_cols  = NULL
) {
  n_orig <- nrow(df)

  # Build an all-NA template row (one row, correct column types)
  na_row <- df[NA_integer_, , drop = FALSE]
  rownames(na_row) <- NULL

  # Helper: create a structural (header / spacer) row from the template.
  # label_val  — text to put in the label column (or "" for spacers)
  # cols_vals  — named character vector: data-column name -> value to insert
  make_struct_row <- function(label_val, cols_vals = character(0)) {
    r <- na_row
    if (!is.null(is_summary)) r[[is_summary]] <- FALSE
    if (!is.null(label))      r[[label]]      <- label_val
    r[[estimate]] <- NA_real_
    r[[lower]]    <- NA_real_
    r[[upper]]    <- NA_real_
    if (!is.null(weight)) r[[weight]] <- NA_real_
    # Text-panel columns default to ""
    for (cn in cols) r[[cn]] <- ""
    # Override with any section/subsection-level annotations
    for (i in seq_along(cols_vals)) r[[names(cols_vals)[i]]] <- cols_vals[[i]]
    r
  }

  # Helper: get the first non-NA, non-empty value of a column within a subset
  first_val <- function(sub_df, col_name) {
    v <- sub_df[[col_name]]
    v <- v[!is.na(v) & nzchar(as.character(v))]
    if (length(v) == 0L) "" else as.character(v[[1L]])
  }

  # Resolve annotation cols into a mapping: data-column name -> display value
  resolve_annotation <- function(annot_cols, sub_df) {
    if (is.null(annot_cols) || length(annot_cols) == 0L) return(character(0))
    vals <- vapply(
      unname(annot_cols),
      function(cn) first_val(sub_df, cn),
      character(1L)
    )
    stats::setNames(vals, unname(annot_cols))
  }

  # Section run-length boundaries
  sec_vals   <- as.character(df[[section]])
  sec_rle    <- rle(sec_vals)
  sec_starts <- cumsum(c(1L, sec_rle$lengths[-length(sec_rle$lengths)]))
  sec_ends   <- cumsum(sec_rle$lengths)
  n_sections <- length(sec_rle$values)

  out_rows              <- vector("list", n_orig * 4L)
  out_ptr               <- 0L
  is_section_hdr_vec    <- logical(0)
  is_subsection_hdr_vec <- logical(0)
  is_spacer_vec         <- logical(0)

  push <- function(rows_df, is_sec, is_sub, is_sp) {
    k <- nrow(rows_df)
    out_ptr <<- out_ptr + 1L
    out_rows[[out_ptr]] <<- rows_df
    is_section_hdr_vec    <<- c(is_section_hdr_vec,    rep(is_sec, k))
    is_subsection_hdr_vec <<- c(is_subsection_hdr_vec, rep(is_sub, k))
    is_spacer_vec         <<- c(is_spacer_vec,         rep(is_sp,  k))
  }

  for (s in seq_len(n_sections)) {
    sec_label <- sec_rle$values[s]
    sec_rows  <- df[sec_starts[s]:sec_ends[s], , drop = FALSE]

    # Section header row
    push(
      make_struct_row(sec_label, resolve_annotation(section_cols, sec_rows)),
      TRUE, FALSE, FALSE
    )

    if (!is.null(subsection)) {
      sub_vals   <- as.character(sec_rows[[subsection]])
      sub_rle    <- rle(sub_vals)
      sub_starts <- cumsum(c(1L, sub_rle$lengths[-length(sub_rle$lengths)]))
      sub_ends   <- cumsum(sub_rle$lengths)

      for (ss in seq_len(length(sub_rle$values))) {
        sub_label <- sub_rle$values[ss]
        sub_rows  <- sec_rows[sub_starts[ss]:sub_ends[ss], , drop = FALSE]

        # Subsection header (indented by 2 spaces relative to section)
        push(
          make_struct_row(
            paste0("  ", sub_label),
            resolve_annotation(subsection_cols, sub_rows)
          ),
          FALSE, TRUE, FALSE
        )

        # Data rows — indent by 4 spaces (2 for section + 2 for subsection)
        if (section_indent && !is.null(label)) {
          sub_rows[[label]] <- paste0("    ", sub_rows[[label]])
        }
        push(sub_rows, FALSE, FALSE, FALSE)
      }
    } else {
      # Data rows — indent by 2 spaces
      if (section_indent && !is.null(label)) {
        sec_rows[[label]] <- paste0("  ", sec_rows[[label]])
      }
      push(sec_rows, FALSE, FALSE, FALSE)
    }

    # Blank spacer row after section
    if (section_spacer) push(make_struct_row(""), FALSE, FALSE, TRUE)
  }

  out_df <- do.call(rbind, out_rows[seq_len(out_ptr)])
  rownames(out_df) <- NULL

  list(
    df                   = out_df,
    is_section_header    = is_section_hdr_vec,
    is_subsection_header = is_subsection_hdr_vec,
    is_spacer            = is_spacer_vec
  )
}
