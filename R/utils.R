# Internal utility functions — not exported

# Coerce data to a plain data.frame, handling data.table and tibbles
as_df <- function(data) {
  as.data.frame(data)
}

# Validate that a column name string exists in the data
check_col <- function(data, col, arg) {
  if (!col %in% names(data)) {
    stop(
      sprintf("Column '%s' not found in data (argument `%s`).", col, arg),
      call. = FALSE
    )
  }
}

# Validate an optional column name (NULL is allowed)
check_col_opt <- function(data, col, arg) {
  if (!is.null(col)) check_col(data, col, arg)
}

# Map a grouping vector to colors using the Okabe-Ito palette (colorblind-safe)
# Returns a named character vector mapping each unique level to a color.
group_colors <- function(groups) {
  lvls <- unique(groups)
  n <- length(lvls)
  # Okabe-Ito palette (skip the first entry which is white/light)
  pal <- grDevices::palette.colors(n + 1L, "Okabe-Ito")[-1L]
  if (n > length(pal)) {
    pal <- rep_len(pal, n)
  }
  stats::setNames(pal[seq_len(n)], as.character(lvls))
}

# Draw a diamond (filled polygon) for a summary estimate row.
# x      : x-coordinate of the point estimate (horizontal centre)
# y      : y-coordinate (row position)
# xmin   : left tip of diamond (lower CI)
# xmax   : right tip of diamond (upper CI)
# height : half-height of diamond in y-axis units
# col    : fill and border color
draw_diamond <- function(x, y, xmin, xmax, height = 0.35, col = "black") {
  if (anyNA(c(x, y, xmin, xmax))) return(invisible(NULL))
  vx <- c(xmin, x, xmax, x)
  vy <- c(y, y + height, y, y - height)
  graphics::polygon(vx, vy, col = col, border = col)
}

# Draw a text panel (labels or statistics column) using base R graphics.
# Called after layout() has positioned the current plot region.
#
# labels   : character vector of labels, length n (row 1 = top)
# n        : number of data rows
# header   : optional bold header string at top of panel
# align    : "left", "center", or "right"
# bold_idx : integer vector of rows to render in bold
# top_mar  : top margin in lines (should match the main plot margin)
# bot_mar  : bottom margin in lines
draw_text_panel <- function(
  labels,
  n,
  header = NULL,
  align = "left",
  bold_idx = integer(0),
  top_mar = 1.5,
  bot_mar = 4
) {
  graphics::par(mar = c(bot_mar, 0.2, top_mar, 0.2))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0.5, n + 0.5))

  x_pos <- switch(
    align,
    left = 0.05,
    center = 0.5,
    right = 0.95,
    stop("align must be 'left', 'center', or 'right'.", call. = FALSE)
  )
  adj <- switch(align, left = 0, center = 0.5, right = 1)

  # Header row (drawn above the topmost data row)
  if (!is.null(header)) {
    graphics::text(
      x = x_pos,
      y = n + 0.6,
      labels = header,
      adj = c(adj, 0.5),
      font = 2,
      xpd = TRUE
    )
    # Thin separator line
    graphics::segments(0, n + 0.2, 1, n + 0.2, col = "gray70", xpd = TRUE)
  }

  # Data labels — row 1 of labels → top of panel (y = n), row n → bottom (y = 1)
  row_y <- seq(n, 1)
  font_vec <- ifelse(seq_len(n) %in% bold_idx, 2L, 1L)
  for (i in seq_len(n)) {
    graphics::text(
      x = x_pos,
      y = row_y[i],
      labels = labels[i],
      adj = c(adj, 0.5),
      font = font_vec[i],
      xpd = TRUE
    )
  }
}

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b
