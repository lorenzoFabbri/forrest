#' Create a forest plot
#'
#' Draws a publication-ready forest plot from a data frame. Each row
#' represents one estimate — a study, a predictor, a model, a subgroup, or any
#' other unit of analysis. Rows whose `estimate` is `NA` are treated as
#' spacer/header rows: they produce no point or confidence interval, and their
#' label is rendered in bold if non-empty.
#'
#' @param data A data frame, [tibble][tibble::tibble], or
#'   [data.table][data.table::data.table].
#' @param estimate Column name (string) for point estimates.
#' @param lower Column name (string) for lower confidence interval bounds.
#' @param upper Column name (string) for upper confidence interval bounds.
#' @param label Column name (string) for row labels displayed on the y-axis or
#'   in the left text panel. If `NULL`, row numbers are used.
#' @param group Column name (string) for a grouping variable. Rows that share
#'   a group value receive the same colour, and a legend is drawn automatically.
#' @param is_summary Column name (string) of a logical vector. Rows where this
#'   is `TRUE` are drawn as filled diamonds (e.g. pooled or overall estimates)
#'   rather than squares with whiskers.
#' @param weight Column name (string) of numeric row weights. When provided,
#'   point size scales as `cex * sqrt(weight / max(weight))`, so rows with
#'   larger weights appear with a bigger marker. Weights for summary rows are
#'   ignored (diamond size is fixed by `cex`).
#' @param ref_line Numeric. Position of the vertical reference line (e.g. `0`
#'   for differences, `1` for ratio measures on the natural scale). Set to
#'   `NULL` to suppress. Default is `0`.
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
#' @param stripe Logical. If `TRUE`, alternate rows are shaded with a light
#'   grey background to improve readability. Default is `FALSE`.
#' @param pch Point character for non-summary rows. Default is `15` (filled
#'   square), which is conventional for forest plots.
#' @param lwd Line width for confidence interval whiskers. Default is `2`.
#' @param cex Point size multiplier. Default is `1`.
#' @param col Colour or character vector of colours. When `NULL` (default) and
#'   `group` is specified, the Okabe-Ito colorblind-safe palette is used. When
#'   `NULL` and no `group`, a single dark colour is used.
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
#' # Basic forest plot — results from a single regression model
#' dat <- data.frame(
#'   label    = c("Exposure A", "Exposure B", "Exposure C", "Exposure D"),
#'   estimate = c(0.42, -0.18, 0.31, -0.07),
#'   lower    = c(0.22, -0.38, 0.12, -0.24),
#'   upper    = c(0.62,  0.02, 0.50,  0.10)
#' )
#' forrest(dat,
#'   estimate = "estimate",
#'   lower    = "lower",
#'   upper    = "upper",
#'   label    = "label",
#'   xlab     = "Regression coefficient (95% CI)"
#' )
#'
#' # Multiple models with subgroup headers and a text column
#' dat2 <- data.frame(
#'   label    = c(
#'     "Model 1", "  Exposure A", "  Exposure B",
#'     "", "Model 2", "  Exposure A", "  Exposure B"
#'   ),
#'   estimate = c(NA,  0.42, -0.18, NA, NA,  0.35, -0.14),
#'   lower    = c(NA,  0.22, -0.38, NA, NA,  0.16, -0.32),
#'   upper    = c(NA,  0.62,  0.02, NA, NA,  0.54,  0.04),
#'   is_sum   = rep(FALSE, 7),
#'   est_ci   = c(
#'     "", "0.42 (0.22, 0.62)", "-0.18 (-0.38, 0.02)",
#'     "", "", "0.35 (0.16, 0.54)", "-0.14 (-0.32, 0.04)"
#'   )
#' )
#' forrest(dat2,
#'   estimate   = "estimate",
#'   lower      = "lower",
#'   upper      = "upper",
#'   label      = "label",
#'   is_summary = "is_sum",
#'   header     = "Predictor",
#'   cols       = c("Coef (95% CI)" = "est_ci")
#' )
#'
#' @export
forrest <- function(
  data,
  estimate,
  lower,
  upper,
  label      = NULL,
  group      = NULL,
  is_summary = NULL,
  weight     = NULL,
  ref_line   = 0,
  log_scale  = FALSE,
  xlim       = NULL,
  xlab       = "Estimate (95% CI)",
  title      = NULL,
  header     = NULL,
  cols       = NULL,
  widths     = NULL,
  stripe     = FALSE,
  pch        = 15,
  lwd        = 2,
  cex        = 1,
  col        = NULL,
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
  n  <- nrow(df)
  if (n == 0L) stop("`data` has zero rows.", call. = FALSE)

  check_col(df, estimate, "estimate")
  check_col(df, lower,    "lower")
  check_col(df, upper,    "upper")
  check_col_opt(df, label,      "label")
  check_col_opt(df, group,      "group")
  check_col_opt(df, is_summary, "is_summary")
  check_col_opt(df, weight,     "weight")
  if (!is.null(cols)) {
    if (is.null(names(cols)) || any(names(cols) == "")) {
      stop("`cols` must be a *named* character vector.", call. = FALSE)
    }
    for (nm in cols) check_col(df, nm, sprintf("cols[\"%s\"]", nm))
  }

  # ── 2. Extract columns ─────────────────────────────────────────────────────
  est    <- as.numeric(df[[estimate]])
  lo     <- as.numeric(df[[lower]])
  hi     <- as.numeric(df[[upper]])
  lbl    <- if (!is.null(label)) as.character(df[[label]]) else
    as.character(seq_len(n))
  grp    <- if (!is.null(group)) as.character(df[[group]]) else NULL
  is_sum <- if (!is.null(is_summary)) as.logical(df[[is_summary]]) else
    rep(FALSE, n)
  wt     <- if (!is.null(weight)) as.numeric(df[[weight]]) else NULL

  # Rows with NA estimate (and not marked as summary) are header/spacer rows
  is_header <- is.na(est) & !is_sum

  # ── 3. Colours ─────────────────────────────────────────────────────────────
  if (!is.null(col)) {
    col_vec      <- rep_len(col, n)
    grp_col_map  <- NULL
  } else if (!is.null(grp)) {
    grp_col_map  <- group_colors(grp)
    col_vec      <- unname(grp_col_map[grp])
  } else {
    col_vec      <- rep("#333333", n)
    grp_col_map  <- NULL
  }

  # ── 4. Per-row point sizes (scaled to weight when provided) ────────────────
  if (!is.null(wt)) {
    reg_wt              <- wt
    reg_wt[is_sum | is.na(est)] <- NA_real_
    max_wt              <- max(reg_wt, na.rm = TRUE)
    cex_vec             <- cex * sqrt(reg_wt / max_wt)
    cex_vec[is.na(cex_vec)] <- cex  # summary rows / headers keep base cex
  } else {
    cex_vec <- rep(cex, n)
  }

  # ── 5. X-axis limits ───────────────────────────────────────────────────────
  xlim_auto <- is.null(xlim)
  if (xlim_auto) {
    vals <- c(lo, hi)
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0L) stop("No finite CI values found.", call. = FALSE)
    rng  <- range(vals)
    pad  <- diff(rng) * 0.06
    xlim <- rng + c(-pad, pad)
    if (!is.null(ref_line) && is.finite(ref_line)) {
      xlim[1L] <- min(xlim[1L], ref_line - pad)
      xlim[2L] <- max(xlim[2L], ref_line + pad)
    }
  }

  # ── 6. Margins and layout ──────────────────────────────────────────────────
  has_cols <- !is.null(cols)
  top_mar  <- if (!is.null(title)) 3 else 1.5
  bot_mar  <- 4
  ylim     <- c(0.5, n + 0.5)

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
    graphics::layout(
      matrix(seq_len(2L + n_right), nrow = 1L),
      widths = widths
    )
  }

  # ── 7. Left label panel ────────────────────────────────────────────────────
  if (has_cols) {
    bold_rows <- which(is_header & nchar(trimws(lbl)) > 0L)
    draw_text_panel(
      labels   = lbl,
      n        = n,
      header   = header,
      align    = "left",
      bold_idx = bold_rows,
      top_mar  = top_mar,
      bot_mar  = bot_mar
    )
  }

  # ── 8. Main forest plot panel ──────────────────────────────────────────────
  if (has_cols) {
    left_mar <- 0.3
  } else {
    max_chars <- max(nchar(lbl), na.rm = TRUE)
    left_mar  <- max(3.5, max_chars * 0.55)
  }
  graphics::par(mar = c(bot_mar, left_mar, top_mar, 1))

  # Row positions: first data row at top (y = n), last at bottom (y = 1)
  row_y <- rev(seq_len(n))

  # Regular rows: non-header, non-NA, non-summary
  reg <- !is_sum & !is.na(est)

  # Use tinyplot for the empty plot frame (axes, labels, tinyplot theme)
  tinyplot::tinyplot(
    x    = xlim,
    y    = c(0.5, n + 0.5),
    type = "n",
    xlim = xlim,
    ylim = ylim,
    xlab = xlab,
    ylab = "",
    main = title %||% "",
    yaxt = "n",
    log  = if (log_scale) "x" else "",
    ...
  )

  # Optional alternating row stripes (drawn before gridlines and data)
  if (stripe) {
    usr <- graphics::par("usr")
    x_lo <- if (log_scale) 10^usr[1L] else usr[1L]
    x_hi <- if (log_scale) 10^usr[2L] else usr[2L]
    stripe_rows <- which(row_y %% 2 == 0)
    for (i in stripe_rows) {
      graphics::rect(
        xleft   = x_lo,
        xright  = x_hi,
        ybottom = row_y[i] - 0.5,
        ytop    = row_y[i] + 0.5,
        col     = "#f2f2f2",
        border  = NA
      )
    }
  }

  # Horizontal gridlines
  graphics::abline(
    h   = seq_len(n),
    col = "#e8e8e8",
    lty = 1L,
    lwd = 0.7
  )

  # Reference line
  if (!is.null(ref_line) && is.finite(ref_line)) {
    graphics::abline(v = ref_line, lty = 2L, lwd = 1, col = "gray45")
  }

  # CI whiskers and points for regular rows
  if (any(reg)) {
    reg_idx <- which(reg)
    lo_reg  <- lo[reg_idx]
    hi_reg  <- hi[reg_idx]
    est_reg <- est[reg_idx]
    y_reg   <- row_y[reg_idx]
    col_reg <- col_vec[reg_idx]
    cex_reg <- cex_vec[reg_idx]

    # Clip CI to xlim; record where clipping occurs
    clip_lo <- lo_reg < xlim[1L]
    clip_hi <- hi_reg > xlim[2L]
    lo_draw <- pmax(lo_reg, xlim[1L])
    hi_draw <- pmin(hi_reg, xlim[2L])

    cap_h <- 0.12
    for (k in seq_along(reg_idx)) {
      # Main CI line
      graphics::segments(
        x0  = lo_draw[k],
        y0  = y_reg[k],
        x1  = hi_draw[k],
        y1  = y_reg[k],
        lwd = lwd,
        col = col_reg[k]
      )
      # Lower end: cap or arrow if clipped
      if (clip_lo[k]) {
        graphics::arrows(
          x0     = lo_draw[k] + diff(xlim) * 0.02,
          y0     = y_reg[k],
          x1     = lo_draw[k],
          y1     = y_reg[k],
          length = 0.06,
          angle  = 25,
          lwd    = lwd,
          col    = col_reg[k],
          code   = 2L
        )
      } else {
        graphics::segments(
          x0  = lo_draw[k],
          y0  = y_reg[k] - cap_h,
          x1  = lo_draw[k],
          y1  = y_reg[k] + cap_h,
          lwd = lwd,
          col = col_reg[k]
        )
      }
      # Upper end: cap or arrow if clipped
      if (clip_hi[k]) {
        graphics::arrows(
          x0     = hi_draw[k] - diff(xlim) * 0.02,
          y0     = y_reg[k],
          x1     = hi_draw[k],
          y1     = y_reg[k],
          length = 0.06,
          angle  = 25,
          lwd    = lwd,
          col    = col_reg[k],
          code   = 2L
        )
      } else {
        graphics::segments(
          x0  = hi_draw[k],
          y0  = y_reg[k] - cap_h,
          x1  = hi_draw[k],
          y1  = y_reg[k] + cap_h,
          lwd = lwd,
          col = col_reg[k]
        )
      }
    }

    # Point estimates (only when the point itself is within xlim)
    vis <- est_reg >= xlim[1L] & est_reg <= xlim[2L]
    if (any(vis)) {
      graphics::points(
        x   = est_reg[vis],
        y   = y_reg[vis],
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
      x      = est[i],
      y      = row_y[i],
      xmin   = max(lo[i], xlim[1L]),
      xmax   = min(hi[i], xlim[2L]),
      height = 0.38 * cex,
      col    = col_vec[i]
    )
  }

  # Y-axis labels (when no separate label panel is used)
  if (!has_cols) {
    bold_rows <- which(is_header & nchar(trimws(lbl)) > 0L)
    font_vec  <- ifelse(seq_len(n) %in% bold_rows, 2L, 1L)
    for (i in seq_len(n)) {
      graphics::mtext(
        text = lbl[i],
        side = 2L,
        at   = row_y[i],
        las  = 1L,
        line = 0.3,
        font = font_vec[i],
        cex  = graphics::par("cex.axis")
      )
    }
    if (!is.null(header)) {
      graphics::mtext(
        text = header,
        side = 2L,
        at   = n + 0.6,
        las  = 1L,
        line = 0.3,
        font = 2L,
        cex  = graphics::par("cex.axis"),
        xpd  = TRUE
      )
    }
  } else {
    graphics::axis(side = 2L, labels = FALSE, tick = FALSE)
  }

  # Legend for group colours
  if (!is.null(grp_col_map) && !is.null(legend_pos)) {
    graphics::legend(
      x      = legend_pos,
      legend = names(grp_col_map),
      fill   = grp_col_map,
      border = NA,
      bty    = "n",
      cex    = 0.85
    )
  }

  # ── 9. Right text columns ──────────────────────────────────────────────────
  if (has_cols) {
    col_nms   <- names(cols)
    bold_rows <- which(is_header & nchar(trimws(lbl)) > 0L)
    for (j in seq_along(cols)) {
      vals <- as.character(df[[cols[j]]])
      draw_text_panel(
        labels   = vals,
        n        = n,
        header   = col_nms[j],
        align    = "center",
        bold_idx = bold_rows,
        top_mar  = top_mar,
        bot_mar  = bot_mar
      )
    }
  }

  invisible(NULL)
}
