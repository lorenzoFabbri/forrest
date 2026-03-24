#' Create a forest plot
#'
#' Draws a publication-ready forest plot from a data frame. Each row represents
#' one study, subgroup, or estimate. Rows whose `estimate` is `NA` are treated
#' as spacer/header rows: they produce no point or confidence interval, and
#' their label is rendered in bold if non-empty.
#'
#' @param data A data frame, [tibble][tibble::tibble], or
#'   [data.table][data.table::data.table].
#' @param estimate Column name (string) for point estimates.
#' @param lower Column name (string) for lower confidence interval bounds.
#' @param upper Column name (string) for upper confidence interval bounds.
#' @param label Column name (string) for row labels displayed on the y-axis or
#'   in the left text panel. If `NULL`, row numbers are used.
#' @param group Column name (string) for a grouping variable. Rows that share
#'   a group value receive the same color, and a legend is drawn automatically.
#' @param is_summary Column name (string) of a logical vector. Rows where this
#'   is `TRUE` are drawn as filled diamonds (pooled / summary estimates) rather
#'   than squares with whiskers.
#' @param weight Column name (string) of numeric study weights (e.g.
#'   inverse-variance weights or effective sample sizes). When provided, point
#'   size scales as `cex * sqrt(weight / max(weight))`, so larger studies are
#'   displayed with a bigger marker — the conventional meta-analysis convention.
#'   Weights for summary rows are ignored (diamond size is controlled by `cex`).
#' @param ref_line Numeric. Position of the vertical reference line (e.g. `0`
#'   for log-odds / risk differences, `1` for odds ratios on the natural scale).
#'   Set to `NULL` to suppress. Default is `0`.
#' @param log_scale Logical. If `TRUE`, apply a log transformation to the
#'   x-axis. Useful when plotting odds ratios, hazard ratios, or risk ratios
#'   on the natural scale. Default is `FALSE`.
#' @param xlim Numeric vector of length 2 giving x-axis limits. Computed from
#'   the data when `NULL` (default). Confidence intervals that extend beyond
#'   `xlim` are clipped at the axis boundary and an arrow is drawn to indicate
#'   truncation.
#' @param xlab Label for the x-axis. Default is `"Estimate (95% CI)"`.
#' @param title Plot title. Default is `NULL` (no title).
#' @param header Optional header string placed above the label column. When
#'   `cols` is provided this appears above the left text panel; otherwise it is
#'   drawn above the topmost row on the y-axis.
#' @param cols Named character vector specifying extra text columns to display
#'   to the right of the plot. Names become column headers; values are column
#'   names in `data`. Example:
#'   `cols = c("OR (95\% CI)" = "or_text", "P" = "pval_text")`.
#' @param widths Numeric vector of relative panel widths. When `cols` is
#'   `NULL`, ignored. Otherwise, length must equal `2 + length(cols)`: label
#'   panel, plot panel, then one entry per extra column. Sensible defaults are
#'   chosen automatically.
#' @param pch Point character for non-summary rows. Default is `15` (filled
#'   square), which is conventional for forest plots.
#' @param lwd Line width for confidence interval whiskers. Default is `2`.
#' @param cex Point size multiplier. Default is `1`.
#' @param col Color or character vector of colors. When `NULL` (default) and
#'   `group` is specified, the Okabe-Ito colorblind-safe palette is used. When
#'   `NULL` and no `group`, a single dark color is used.
#' @param legend_pos Position of the legend when `group` is supplied. Passed
#'   to [legend()]. Set to `NULL` to suppress the legend. Default is
#'   `"topright"`.
#' @param ... Additional graphical parameters passed to the internal
#'   [tinyplot::tinyplot()] call that sets up the main plot area (e.g.
#'   `cex.axis`, `cex.lab`, `font.main`).
#'
#' @return Invisibly returns `NULL`. Called for its side effect of producing a
#'   plot.
#'
#' @examples
#' # Basic forest plot
#' dat <- data.frame(
#'   label    = c("Study A", "Study B", "Study C", "Pooled"),
#'   estimate = c(-0.15, 0.10, -0.05, -0.05),
#'   lower    = c(-0.40, -0.10, -0.25, -0.20),
#'   upper    = c(0.10, 0.30, 0.15, 0.10),
#'   is_sum   = c(FALSE, FALSE, FALSE, TRUE)
#' )
#' forrest(dat,
#'   estimate   = "estimate",
#'   lower      = "lower",
#'   upper      = "upper",
#'   label      = "label",
#'   is_summary = "is_sum"
#' )
#'
#' # With subgroup headers (NA estimate rows) and text columns
#' dat2 <- data.frame(
#'   label    = c(
#'     "Subgroup A", "Study 1", "Study 2", "Pooled A",
#'     "", "Subgroup B", "Study 3", "Study 4", "Pooled B"
#'   ),
#'   estimate = c(NA, 0.5, 0.7, 0.6, NA, NA, 1.1, 0.9, 1.0),
#'   lower    = c(NA, 0.3, 0.5, 0.45, NA, NA, 0.8, 0.6, 0.75),
#'   upper    = c(NA, 0.7, 0.9, 0.75, NA, NA, 1.4, 1.2, 1.25),
#'   is_sum   = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
#'   or_ci    = c(
#'     "", "0.50 (0.30-0.70)", "0.70 (0.50-0.90)", "0.60 (0.45-0.75)",
#'     "", "", "1.10 (0.80-1.40)", "0.90 (0.60-1.20)", "1.00 (0.75-1.25)"
#'   )
#' )
#' forrest(dat2,
#'   estimate   = "estimate",
#'   lower      = "lower",
#'   upper      = "upper",
#'   label      = "label",
#'   is_summary = "is_sum",
#'   ref_line   = 1,
#'   log_scale  = TRUE,
#'   header     = "Study",
#'   cols       = c("OR (95% CI)" = "or_ci")
#' )
#'
#' @export
forrest <- function(
  data,
  estimate,
  lower,
  upper,
  label = NULL,
  group = NULL,
  is_summary = NULL,
  weight = NULL,
  ref_line = 0,
  log_scale = FALSE,
  xlim = NULL,
  xlab = "Estimate (95% CI)",
  title = NULL,
  header = NULL,
  cols = NULL,
  widths = NULL,
  pch = 15,
  lwd = 2,
  cex = 1,
  col = NULL,
  legend_pos = "topright",
  ...
) {
  # ── 1. Input validation ────────────────────────────────────────────────────
  if (
    !is.data.frame(data) &&
      !inherits(data, "data.table") &&
      !inherits(data, "tbl_df")
  ) {
    stop("`data` must be a data frame, tibble, or data.table.", call. = FALSE)
  }
  df <- as_df(data)
  n <- nrow(df)
  if (n == 0L) stop("`data` has zero rows.", call. = FALSE)

  check_col(df, estimate, "estimate")
  check_col(df, lower, "lower")
  check_col(df, upper, "upper")
  check_col_opt(df, label, "label")
  check_col_opt(df, group, "group")
  check_col_opt(df, is_summary, "is_summary")
  check_col_opt(df, weight, "weight")
  if (!is.null(cols)) {
    if (is.null(names(cols)) || any(names(cols) == "")) {
      stop("`cols` must be a *named* character vector.", call. = FALSE)
    }
    for (nm in cols) check_col(df, nm, sprintf("cols[\"%s\"]", nm))
  }

  # ── 2. Extract columns ─────────────────────────────────────────────────────
  est <- as.numeric(df[[estimate]])
  lo <- as.numeric(df[[lower]])
  hi <- as.numeric(df[[upper]])
  lbl <- if (!is.null(label)) as.character(df[[label]]) else
    as.character(seq_len(n))
  grp <- if (!is.null(group)) as.character(df[[group]]) else NULL
  is_sum <- if (!is.null(is_summary)) as.logical(df[[is_summary]]) else
    rep(FALSE, n)
  wt <- if (!is.null(weight)) as.numeric(df[[weight]]) else NULL

  # Rows with NA estimate (and not summary) are header/spacer rows
  is_header <- is.na(est) & !is_sum

  # ── 3. Colors ──────────────────────────────────────────────────────────────
  if (!is.null(col)) {
    col_vec <- rep_len(col, n)
    grp_col_map <- NULL
  } else if (!is.null(grp)) {
    grp_col_map <- group_colors(grp)
    col_vec <- unname(grp_col_map[grp])
  } else {
    col_vec <- rep("#333333", n)
    grp_col_map <- NULL
  }

  # ── 4. Per-row point sizes (scaled to study weight when provided) ──────────
  if (!is.null(wt)) {
    reg_wt <- wt
    reg_wt[is_sum | is.na(est)] <- NA_real_
    max_wt <- max(reg_wt, na.rm = TRUE)
    cex_vec <- cex * sqrt(reg_wt / max_wt)
    cex_vec[is.na(cex_vec)] <- cex # summary rows / headers keep base cex
  } else {
    cex_vec <- rep(cex, n)
  }

  # ── 5. X-axis limits ───────────────────────────────────────────────────────
  xlim_auto <- is.null(xlim)
  if (xlim_auto) {
    vals <- c(lo, hi)
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0L) stop("No finite CI values found.", call. = FALSE)
    rng <- range(vals)
    pad <- diff(rng) * 0.06
    xlim <- rng + c(-pad, pad)
    if (!is.null(ref_line) && is.finite(ref_line)) {
      xlim[1L] <- min(xlim[1L], ref_line - pad)
      xlim[2L] <- max(xlim[2L], ref_line + pad)
    }
  }

  # ── 6. Margins and layout ──────────────────────────────────────────────────
  has_cols <- !is.null(cols)
  top_mar <- if (!is.null(title)) 3 else 1.5
  bot_mar <- 4
  ylim <- c(0.5, n + 0.5)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  if (has_cols) {
    n_right <- length(cols)
    if (is.null(widths)) {
      widths <- c(2.5, 4.5, rep(1.8, n_right))
    }
    if (length(widths) != 2L + n_right) {
      stop(
        sprintf(
          "`widths` must have length %d (label + plot + %d column(s)).",
          2L + n_right,
          n_right
        ),
        call. = FALSE
      )
    }
    graphics::layout(matrix(seq_len(2L + n_right), nrow = 1L), widths = widths)
  }

  # ── 7. Left label panel ────────────────────────────────────────────────────
  if (has_cols) {
    bold_rows <- which(is_header & nchar(trimws(lbl)) > 0L)
    draw_text_panel(
      labels = lbl,
      n = n,
      header = header,
      align = "left",
      bold_idx = bold_rows,
      top_mar = top_mar,
      bot_mar = bot_mar
    )
  }

  # ── 8. Main forest plot panel ──────────────────────────────────────────────
  if (has_cols) {
    left_mar <- 0.3
  } else {
    max_chars <- max(nchar(lbl), na.rm = TRUE)
    left_mar <- max(3.5, max_chars * 0.55)
  }
  graphics::par(mar = c(bot_mar, left_mar, top_mar, 1))

  # Row positions: first data row at top (y = n), last at bottom (y = 1)
  row_y <- rev(seq_len(n))

  # Rows that will receive a point + CI (non-header, non-NA, non-summary)
  reg <- !is_sum & !is.na(est)

  # Use tinyplot for the empty plot frame (axes, labels, tinyplot theme)
  tinyplot::tinyplot(
    x = xlim,
    y = c(0.5, n + 0.5),
    type = "n",
    xlim = xlim,
    ylim = ylim,
    xlab = xlab,
    ylab = "",
    main = title %||% "",
    yaxt = "n",
    log = if (log_scale) "x" else "",
    ...
  )

  # Horizontal gridlines
  graphics::abline(h = seq_len(n), col = "#ebebeb", lty = 1L, lwd = 0.8)

  # Reference line
  if (!is.null(ref_line) && is.finite(ref_line)) {
    graphics::abline(v = ref_line, lty = 2L, lwd = 1, col = "gray45")
  }

  # CI whiskers and points for regular rows
  if (any(reg)) {
    reg_idx <- which(reg)
    lo_reg <- lo[reg_idx]
    hi_reg <- hi[reg_idx]
    est_reg <- est[reg_idx]
    y_reg <- row_y[reg_idx]
    col_reg <- col_vec[reg_idx]
    cex_reg <- cex_vec[reg_idx]

    # Clip CI to xlim; note where clipping occurs
    clip_lo <- lo_reg < xlim[1L]
    clip_hi <- hi_reg > xlim[2L]
    lo_draw <- pmax(lo_reg, xlim[1L])
    hi_draw <- pmin(hi_reg, xlim[2L])

    cap_h <- 0.12
    for (k in seq_along(reg_idx)) {
      # Main CI line
      graphics::segments(
        x0 = lo_draw[k],
        y0 = y_reg[k],
        x1 = hi_draw[k],
        y1 = y_reg[k],
        lwd = lwd,
        col = col_reg[k]
      )
      # Lower end cap (or arrow if clipped)
      if (clip_lo[k]) {
        graphics::arrows(
          x0 = lo_draw[k] + diff(xlim) * 0.02,
          y0 = y_reg[k],
          x1 = lo_draw[k],
          y1 = y_reg[k],
          length = 0.06,
          angle = 25,
          lwd = lwd,
          col = col_reg[k],
          code = 2L
        )
      } else {
        graphics::segments(
          x0 = lo_draw[k],
          y0 = y_reg[k] - cap_h,
          x1 = lo_draw[k],
          y1 = y_reg[k] + cap_h,
          lwd = lwd,
          col = col_reg[k]
        )
      }
      # Upper end cap (or arrow if clipped)
      if (clip_hi[k]) {
        graphics::arrows(
          x0 = hi_draw[k] - diff(xlim) * 0.02,
          y0 = y_reg[k],
          x1 = hi_draw[k],
          y1 = y_reg[k],
          length = 0.06,
          angle = 25,
          lwd = lwd,
          col = col_reg[k],
          code = 2L
        )
      } else {
        graphics::segments(
          x0 = hi_draw[k],
          y0 = y_reg[k] - cap_h,
          x1 = hi_draw[k],
          y1 = y_reg[k] + cap_h,
          lwd = lwd,
          col = col_reg[k]
        )
      }
    }

    # Point estimates (only if the point itself is within xlim)
    vis <- est_reg >= xlim[1L] & est_reg <= xlim[2L]
    if (any(vis)) {
      graphics::points(
        x = est_reg[vis],
        y = y_reg[vis],
        pch = pch,
        cex = cex_reg[vis],
        col = col_reg[vis]
      )
    }
  }

  # Diamonds for summary rows
  sum_rows <- which(is_sum & !is.na(est))
  for (i in sum_rows) {
    draw_diamond(
      x = est[i],
      y = row_y[i],
      xmin = max(lo[i], xlim[1L]),
      xmax = min(hi[i], xlim[2L]),
      height = 0.38 * cex,
      col = col_vec[i]
    )
  }

  # Y-axis labels
  if (!has_cols) {
    bold_rows <- which(is_header & nchar(trimws(lbl)) > 0L)
    font_vec <- ifelse(seq_len(n) %in% bold_rows, 2L, 1L)
    for (i in seq_len(n)) {
      graphics::mtext(
        text = lbl[i],
        side = 2L,
        at = row_y[i],
        las = 1L,
        line = 0.3,
        font = font_vec[i],
        cex = graphics::par("cex.axis")
      )
    }
    if (!is.null(header)) {
      graphics::mtext(
        text = header,
        side = 2L,
        at = n + 0.6,
        las = 1L,
        line = 0.3,
        font = 2L,
        cex = graphics::par("cex.axis"),
        xpd = TRUE
      )
    }
  } else {
    graphics::axis(side = 2L, labels = FALSE, tick = FALSE)
  }

  # Legend for group colors
  if (!is.null(grp_col_map) && !is.null(legend_pos)) {
    graphics::legend(
      x = legend_pos,
      legend = names(grp_col_map),
      fill = grp_col_map,
      border = NA,
      bty = "n",
      cex = 0.85
    )
  }

  # ── 9. Right text columns ──────────────────────────────────────────────────
  if (has_cols) {
    col_nms <- names(cols)
    bold_rows <- which(is_header & nchar(trimws(lbl)) > 0L)
    for (j in seq_along(cols)) {
      vals <- as.character(df[[cols[j]]])
      draw_text_panel(
        labels = vals,
        n = n,
        header = col_nms[j],
        align = "center",
        bold_idx = bold_rows,
        top_mar = top_mar,
        bot_mar = bot_mar
      )
    }
  }

  invisible(NULL)
}
