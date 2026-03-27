# Graphical helpers for drawing forest plot elements — not exported

#' Draw a filled diamond polygon for a summary estimate row
#'
#' @param x x-coordinate of the point estimate (horizontal centre).
#' @param y y-coordinate (row position).
#' @param xmin Left tip of the diamond (lower CI bound).
#' @param xmax Right tip of the diamond (upper CI bound).
#' @param height Half-height of the diamond in y-axis units.
#' @param col Fill and border colour.
#'
#' @return Invisibly returns `NULL`. Called for its side effect.
#' @noRd
draw_diamond <- function(x, y, xmin, xmax, height = 0.35, col = "black") {
  if (anyNA(c(x, y, xmin, xmax))) {
    return(invisible(NULL))
  }
  vx <- c(xmin, x, xmax, x)
  vy <- c(y, y + height, y, y - height)
  graphics::polygon(vx, vy, col = col, border = col)
}

#' Draw a text panel alongside the forest plot
#'
#' Renders a column of labels (or statistics) in its own plot region after
#' [graphics::layout()] has positioned the viewport. Used for both the left
#' label panel and right statistics columns.
#'
#' @param labels Character vector of row labels. Row 1 maps to the top of the
#'   panel.
#' @param n_vis Number of visual rows (used to set the y-axis range). When
#'   dodge is active this equals the number of label groups, not the number of
#'   data rows.
#' @param header Optional bold header string placed above the topmost row.
#' @param align Text alignment: `"left"`, `"center"`, or `"right"`.
#' @param bold_idx Integer vector of row indices to render in bold.
#' @param top_mar Top margin in lines (must match the main plot margin).
#' @param bot_mar Bottom margin in lines.
#' @param cex Character expansion factor for text.
#' @param y_positions Numeric vector of y coordinates for each label. When
#'   `NULL` (default), labels are placed at `seq(n_vis, 1)`. When dodge is
#'   active, pass the per-row dodged y positions here.
#'
#' @return Invisibly returns `NULL`. Called for its side effect.
#' @noRd
draw_text_panel <- function(
  labels,
  n_vis,
  header = NULL,
  align = "left",
  bold_idx = integer(0),
  top_mar = 1.5,
  bot_mar = 4,
  cex = 1,
  y_positions = NULL
) {
  graphics::par(mar = c(bot_mar, 0.2, top_mar, 0.2))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0.5, n_vis + 0.5))

  x_pos <- switch(
    align,
    left = 0.05,
    center = 0.5,
    right = 0.95,
    stop("align must be 'left', 'center', or 'right'.", call. = FALSE)
  )
  adj <- switch(align, left = 0, center = 0.5, right = 1)

  if (!is.null(header)) {
    graphics::text(
      x = x_pos,
      y = n_vis + 0.6,
      labels = header,
      adj = c(adj, 0.5),
      font = 2L,
      cex = cex,
      xpd = TRUE
    )
    graphics::segments(
      0,
      n_vis + 0.2,
      1,
      n_vis + 0.2,
      col = "gray70",
      xpd = TRUE
    )
  }

  row_y <- if (!is.null(y_positions)) y_positions else seq(n_vis, 1L)
  font_vec <- ifelse(seq_along(labels) %in% bold_idx, 2L, 1L)
  for (i in seq_along(labels)) {
    graphics::text(
      x = x_pos,
      y = row_y[i],
      labels = labels[i],
      adj = c(adj, 0.5),
      font = font_vec[i],
      cex = cex,
      xpd = TRUE
    )
  }
}
