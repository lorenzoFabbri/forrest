#' Create a forest plot
#'
#' Draws a publication-ready forest plot from a data frame. Each row
#' represents one estimate — a study, a predictor, a model, a subgroup, or any
#' other unit of analysis. Rows whose `estimate` is `NA` are treated as
#' spacer/header rows: they produce no point or confidence interval, and their
#' label is rendered in bold if non-empty.
#'
#' @param data A data frame, tibble, or data.table.
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
#'   names in `data`. Example: `cols = c("OR (95\\% CI)" = "or_ci")`.
#' @param widths Numeric vector of relative panel widths. When `cols` is
#'   `NULL`, ignored. Otherwise, length must equal `2 + length(cols)`: label
#'   panel, plot panel, then one entry per extra column. Sensible defaults are
#'   chosen automatically.
#' @param stripe Logical. If `TRUE`, alternate rows are shaded with a light
#'   grey background to improve readability. Default is `FALSE`.
#' @param dodge Logical or positive numeric. When `TRUE` (or a positive
#'   number), consecutive rows that share the same `label` value are grouped
#'   together and their confidence intervals are drawn with a small vertical
#'   offset so that they do not overlap. The shared label is displayed once at
#'   the centre of the group. Use together with `group` (for colour) and/or
#'   `shape` (for point characters) to distinguish the overlaid series. A
#'   numeric value sets the offset between rows in a group directly (in y-axis
#'   units); `TRUE` uses a default of `0.25`. Default is `FALSE`.
#' @param pch Point character for non-summary rows. Default is `15` (filled
#'   square). When `shape` is provided, `pch` is used only as a fallback for
#'   rows whose shape value is `NA`.
#' @param shape Column name (string) for a shape variable. When provided,
#'   different values of the column are rendered with different point characters
#'   and a shape legend is drawn. Use together with `group` to distinguish two
#'   categorical dimensions simultaneously (e.g. colour = time period, shape =
#'   sex).
#' @param lwd Line width for confidence interval whiskers. Default is `2`.
#' @param cex Point size multiplier. Default is `1`.
#' @param col Colour or character vector of colours. When `NULL` (default) and
#'   `group` is specified, the Okabe-Ito colorblind-safe palette is used. When
#'   `NULL` and no `group`, a single dark colour is used.
#' @param legend_pos Position of the colour legend when `group` is supplied.
#'   Passed to `legend()`. Use `NULL` to suppress.
#'   Default is `"topright"`.
#' @param cols_by_group Logical. Relevant only when `dodge` is active. When
#'   `TRUE`, each text column in `cols` is collapsed to one value per label
#'   group: the first non-empty entry within the group is displayed at the
#'   group centre y position. This produces a wide-format text table with one
#'   row per label and one column per condition. Populate each text column so
#'   that the value is non-empty only for the matching condition row and empty
#'   (`""`) for all others; `forrest()` picks up the right value automatically.
#'   When `FALSE` (default), text values are drawn at each individual row's
#'   dodged y position, keeping them aligned with their CI whiskers.
#' @param legend_shape_pos Position of the shape legend when `shape` is
#'   supplied. Passed to `legend()`. Use `NULL` to suppress.
#'   Default is `"bottomright"`.
#' @param theme Visual theme name ("default", "minimal", "classic") or a
#'   named list of style overrides. Default is `"default"`.
#' @param ... Graphical parameters forwarded to the internal tinyplot call
#'   (e.g. `cex.axis`, `cex.lab`).
#'
#' @return Invisibly returns `NULL`. Called for its side effect of producing a
#'   plot.
#'
#' @examples
#' # Basic forest plot: linear model coefficients
#' dat <- data.frame(
#'   predictor = c("Age (per 10 y)", "Female sex",
#'                 "BMI (per 5 kg/m\u00b2)", "Current smoker"),
#'   estimate  = c(0.42, -0.38,  0.19, -0.31),
#'   lower     = c(0.22, -0.56, -0.02, -0.51),
#'   upper     = c(0.62, -0.20,  0.40, -0.11)
#' )
#' forrest(dat,
#'   estimate = "estimate",
#'   lower    = "lower",
#'   upper    = "upper",
#'   label    = "predictor",
#'   xlab     = "Regression coefficient (95% CI)"
#' )
#'
#' # Multiple estimates per row (dodge) - same predictor in two time windows
#' dat2 <- data.frame(
#'   predictor = rep(c("Air pollution (PM2.5)",
#'                     "Sedentary behaviour",
#'                     "Diet quality score"), each = 2),
#'   period    = rep(c("Childhood", "Adulthood"), 3),
#'   estimate  = c( 0.18,  0.30, -0.12, -0.22,  0.09,  0.14),
#'   lower     = c(-0.02,  0.10, -0.30, -0.40, -0.08, -0.04),
#'   upper     = c( 0.38,  0.50,  0.06, -0.04,  0.26,  0.32)
#' )
#' forrest(dat2,
#'   estimate = "estimate",
#'   lower    = "lower",
#'   upper    = "upper",
#'   label    = "predictor",
#'   group    = "period",
#'   dodge    = TRUE,
#'   xlab     = "Regression coefficient (95% CI)"
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
  stripe = FALSE,
  dodge = FALSE,
  pch = 15,
  shape = NULL,
  lwd = 2,
  cex = 1,
  col = NULL,
  cols_by_group = FALSE,
  legend_pos = "topright",
  legend_shape_pos = "bottomright",
  theme = "default",
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
  if (n == 0L) {
    stop("`data` has zero rows.", call. = FALSE)
  }

  check_col(df, estimate, "estimate")
  check_col(df, lower, "lower")
  check_col(df, upper, "upper")
  check_col_opt(df, label, "label")
  check_col_opt(df, group, "group")
  check_col_opt(df, is_summary, "is_summary")
  check_col_opt(df, weight, "weight")
  check_col_opt(df, shape, "shape")
  if (!is.null(cols)) {
    if (is.null(names(cols)) || any(names(cols) == "")) {
      stop("`cols` must be a *named* character vector.", call. = FALSE)
    }
    for (nm in cols) {
      check_col(df, nm, sprintf("cols[\"%s\"]", nm))
    }
  }

  # ── 2. Theme ───────────────────────────────────────────────────────────────
  th <- resolve_theme(theme)

  # ── 3. Extract columns ─────────────────────────────────────────────────────
  # Coerce to atomic vectors so downstream code can index without
  # method dispatch
  est <- as.numeric(df[[estimate]])
  lo  <- as.numeric(df[[lower]])
  hi  <- as.numeric(df[[upper]])
  # Default label: row index as a string (rarely used but keeps the rest of
  # the function uniform — `lbl` is always a character vector of length n)
  lbl <- if (!is.null(label)) {
    as.character(df[[label]])
  } else {
    as.character(seq_len(n))
  }
  grp    <- if (!is.null(group))  as.character(df[[group]])  else NULL
  shp    <- if (!is.null(shape))  as.character(df[[shape]])  else NULL
  # `is_sum` defaults to all FALSE; summary rows draw a diamond instead of
  # a point + whiskers
  is_sum <- if (!is.null(is_summary)) {
    as.logical(df[[is_summary]])
  } else {
    rep(FALSE, n)
  }
  wt <- if (!is.null(weight)) as.numeric(df[[weight]]) else NULL

  # Header rows: NA estimate AND not a summary row.  These produce no graphical
  # element; their label is rendered in bold if non-empty, or as a blank
  # spacer if empty ("").
  is_header <- is.na(est) & !is_sum

  # ── 4. Colours ─────────────────────────────────────────────────────────────
  # Priority: explicit `col` > group-mapped colours > single dark grey.
  # `grp_col_map` is kept as a named vector (level → hex) so it can be passed
  # directly to `legend(fill = ...)`.
  if (!is.null(col)) {
    col_vec    <- rep_len(col, n)
    grp_col_map <- NULL
  } else if (!is.null(grp)) {
    grp_col_map <- group_colors(grp)
    col_vec     <- unname(grp_col_map[grp])
  } else {
    col_vec     <- rep("#333333", n)
    grp_col_map <- NULL
  }

  # ── 5. Point characters ────────────────────────────────────────────────────
  # When `shape` is supplied every unique value gets a distinct pch.
  # The fallback (`pch` argument) is used for rows whose shape value is NA.
  if (!is.null(shp)) {
    shp_pch_map <- group_shapes(shp)
    pch_vec     <- unname(shp_pch_map[shp])
    pch_vec[is.na(pch_vec)] <- pch
  } else {
    pch_vec     <- rep(pch, n)
    shp_pch_map <- NULL
  }

  # ── 6. Per-row point sizes (scaled to weight when provided) ─────────────────
  # Square-root scaling: size ∝ sqrt(weight) so that the *area* of the marker
  # is proportional to weight (inverse variance), which is the convention in
  # meta-analysis forest plots.  Summary and header rows always use `cex` 1×.
  if (!is.null(wt)) {
    reg_wt <- wt
    # NA out summary and header rows so they don't influence max_wt
    reg_wt[is_sum | is.na(est)] <- NA_real_
    max_wt  <- max(reg_wt, na.rm = TRUE)
    cex_vec <- cex * sqrt(reg_wt / max_wt)
    # Restore cex for summary / header rows (their weight was set to NA above)
    cex_vec[is.na(cex_vec)] <- cex
  } else {
    cex_vec <- rep(cex, n)
  }

  # ── 7. X-axis limits ───────────────────────────────────────────────────────
  # Auto-compute from CI bounds: take the range of all finite lo/hi values,
  # add 6% padding on each side, then widen slightly so the reference line
  # always has at least `pad` whitespace around it.
  xlim_auto <- is.null(xlim)
  if (xlim_auto) {
    vals <- c(lo, hi)
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0L) {
      stop("No finite CI values found.", call. = FALSE)
    }
    rng <- range(vals)
    if (log_scale) {
      # On a log axis, padding must be multiplicative (geometric) so that the
      # left limit stays positive: work in log-space, then exponentiate back.
      log_rng <- log(rng)
      log_pad <- diff(log_rng) * 0.06
      xlim    <- exp(log_rng + c(-log_pad, log_pad))
      if (!is.null(ref_line) && is.finite(ref_line) && ref_line > 0) {
        xlim[1L] <- min(xlim[1L], ref_line / exp(log_pad))
        xlim[2L] <- max(xlim[2L], ref_line * exp(log_pad))
      }
    } else {
      pad  <- diff(rng) * 0.06
      xlim <- rng + c(-pad, pad)
      if (!is.null(ref_line) && is.finite(ref_line)) {
        xlim[1L] <- min(xlim[1L], ref_line - pad)
        xlim[2L] <- max(xlim[2L], ref_line + pad)
      }
    }
  }

  # ── 8. Dodge: visual groups and y positions ─────────────────────────────────
  # `dodge_amt` is the vertical offset (in y-axis units) between rows that
  # share the same label.  0 means no dodging (one row per data row).
  dodge_amt <- if (isTRUE(dodge)) 0.25 else
    if (is.numeric(dodge) && dodge > 0) as.numeric(dodge) else 0

  if (dodge_amt > 0) {
    # Assign each data row to a visual group; consecutive rows with the same
    # label form one group.  Header/spacer rows are always singletons.
    group_ids    <- compute_dodge_groups(lbl, is_header)
    n_vis <- max(group_ids)  # number of vertical slots in the plot

    # Visual slot centres: slot 1 is at y = n_vis (top), slot n_vis at y = 1.
    grp_center_y <- (n_vis + 1L) - seq_len(n_vis)

    # Per-row y positions: rows within a group are symmetrically spread around
    # the group's centre y using equally-spaced offsets of `dodge_amt`.
    row_y <- numeric(n)
    for (g in seq_len(n_vis)) {
      idx     <- which(group_ids == g)
      k       <- length(idx)
      offsets <- if (k == 1L) 0 else
        seq(-(k - 1L) / 2, (k - 1L) / 2, length.out = k) * dodge_amt
      row_y[idx] <- grp_center_y[g] + offsets
    }

    # Collect one representative label and header flag per visual group (first
    # row of each group) — used for y-axis text and bold formatting
    vis_lbl       <- vapply(
      seq_len(n_vis),
      function(g) lbl[which(group_ids == g)[1L]],
      character(1L)
    )
    vis_center_y  <- grp_center_y
    vis_is_header <- vapply(
      seq_len(n_vis),
      function(g) is_header[which(group_ids == g)[1L]],
      logical(1L)
    )
  } else {
    # No dodging: one visual row per data row, top to bottom
    n_vis         <- n
    row_y         <- rev(seq_len(n))  # row 1 maps to y = n (top of plot)
    vis_lbl       <- lbl
    vis_center_y  <- row_y
    vis_is_header <- is_header
  }

  # y-axis limits include half a row of padding above/below the outermost rows
  ylim <- c(0.5, n_vis + 0.5)

  # ── 9. Margins and layout ──────────────────────────────────────────────────
  has_cols <- !is.null(cols)
  # Extra top margin when a header is displayed without a title, so the header
  # text and its separator line are not clipped by the plot boundary.
  top_mar  <- if (!is.null(title)) 3 else if (!is.null(header)) 2.5 else 1.5
  bot_mar  <- 4

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

  # ── 10. Left label panel ───────────────────────────────────────────────────
  if (has_cols) {
    bold_rows <- which(vis_is_header & nchar(trimws(vis_lbl)) > 0L)
    draw_text_panel(
      labels   = vis_lbl,
      n_vis    = n_vis,
      header   = header,
      align    = "left",
      bold_idx = bold_rows,
      top_mar  = top_mar,
      bot_mar  = bot_mar
    )
  }

  # ── 11. Main forest plot panel ─────────────────────────────────────────────
  if (has_cols) {
    left_mar <- 0.3
  } else {
    # Approximate the left margin width from the longest label. The 0.6 factor
    # converts characters to margin lines; the minimum of 4 lines ensures
    # short labels still have breathing room.
    max_chars <- max(nchar(lbl), na.rm = TRUE)
    left_mar  <- max(4, max_chars * 0.6)
  }
  graphics::par(mar = c(bot_mar, left_mar, top_mar, 1))

  # Regular rows: non-header, non-NA, non-summary
  reg <- !is_sum & !is.na(est)

  tinyplot::tinyplot(
    x    = xlim,
    y    = c(0.5, n_vis + 0.5),
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

  # Alternating row stripes — one stripe per visual group (even-numbered rows)
  if (stripe) {
    usr  <- graphics::par("usr")
    x_lo <- if (log_scale) 10^usr[1L] else usr[1L]
    x_hi <- if (log_scale) 10^usr[2L] else usr[2L]
    for (yc in vis_center_y[vis_center_y %% 2 == 0]) {
      graphics::rect(
        x_lo, yc - 0.5, x_hi, yc + 0.5,
        col = th$stripe_col, border = NA
      )
    }
  }

  # Horizontal gridlines — one per visual row, behind data elements
  graphics::abline(
    h   = seq_len(n_vis),
    col = th$grid_col,
    lty = th$grid_lty,
    lwd = th$grid_lwd
  )

  # Vertical reference line (e.g. 0 for differences, 1 for ratios)
  if (!is.null(ref_line) && is.finite(ref_line)) {
    graphics::abline(v = ref_line, lty = th$ref_lty, lwd = 1, col = th$ref_col)
  }

  # CI whiskers and points for regular rows (not header, not NA, not summary)
  if (any(reg)) {
    # Subset to regular rows only
    reg_idx <- which(reg)
    lo_reg  <- lo[reg_idx]
    hi_reg  <- hi[reg_idx]
    est_reg <- est[reg_idx]
    y_reg   <- row_y[reg_idx]
    col_reg <- col_vec[reg_idx]
    cex_reg <- cex_vec[reg_idx]
    pch_reg <- pch_vec[reg_idx]

    # Detect CI bounds that fall outside the visible x-range
    clip_lo <- lo_reg < xlim[1L]
    clip_hi <- hi_reg > xlim[2L]
    # Clamp the drawn endpoints to xlim — the original values are unchanged
    lo_draw <- pmax(lo_reg, xlim[1L])
    hi_draw <- pmin(hi_reg, xlim[2L])

    # Height of the T-cap tick mark at each CI endpoint (in y-axis units)
    cap_h <- 0.12
    for (k in seq_along(reg_idx)) {
      # Horizontal whisker from lower to upper CI bound
      graphics::segments(
        lo_draw[k], y_reg[k], hi_draw[k], y_reg[k],
        lwd = lwd, col = col_reg[k]
      )
      # Lower end: arrow if clipped (CI extends off-screen), T-cap otherwise
      if (clip_lo[k]) {
        graphics::arrows(
          lo_draw[k] + diff(xlim) * 0.02, y_reg[k],
          lo_draw[k], y_reg[k],
          length = 0.06, angle = 25, lwd = lwd, col = col_reg[k], code = 2L
        )
      } else {
        graphics::segments(
          lo_draw[k], y_reg[k] - cap_h, lo_draw[k], y_reg[k] + cap_h,
          lwd = lwd, col = col_reg[k]
        )
      }
      # Upper end: same logic
      if (clip_hi[k]) {
        graphics::arrows(
          hi_draw[k] - diff(xlim) * 0.02, y_reg[k],
          hi_draw[k], y_reg[k],
          length = 0.06, angle = 25, lwd = lwd, col = col_reg[k], code = 2L
        )
      } else {
        graphics::segments(
          hi_draw[k], y_reg[k] - cap_h, hi_draw[k], y_reg[k] + cap_h,
          lwd = lwd, col = col_reg[k]
        )
      }
    }

    # Draw point estimates — skip any whose value is outside xlim (can happen
    # when the user supplies a manual xlim that doesn't cover all estimates)
    vis_pts <- est_reg >= xlim[1L] & est_reg <= xlim[2L]
    if (any(vis_pts)) {
      graphics::points(
        est_reg[vis_pts], y_reg[vis_pts],
        pch = pch_reg[vis_pts],
        cex = cex_reg[vis_pts],
        col = col_reg[vis_pts]
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

  # Y-axis labels (when no separate label panel is used).
  # Labels are rendered as horizontal text in the left margin (las = 1).
  # line = 0.5 gives a small gap between the axis and the text, and together
  # with left_mar = max(4, max_chars * 0.6) this keeps labels within bounds.
  if (!has_cols) {
    bold_rows <- which(vis_is_header & nchar(trimws(vis_lbl)) > 0L)
    font_vec  <- ifelse(seq_len(n_vis) %in% bold_rows, 2L, 1L)
    for (i in seq_len(n_vis)) {
      graphics::mtext(
        text = vis_lbl[i],
        side = 2L,
        at   = vis_center_y[i],
        las  = 1L,
        line = 0.5,
        font = font_vec[i],
        cex  = graphics::par("cex.axis")
      )
    }
    if (!is.null(header)) {
      # Bold section header placed just above the topmost row in the top margin
      graphics::mtext(
        text = header,
        side = 2L,
        at   = n_vis + 0.6,
        las  = 1L,
        line = 0.5,
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

  # Legend for point shapes
  if (!is.null(shp_pch_map) && !is.null(legend_shape_pos)) {
    graphics::legend(
      x      = legend_shape_pos,
      legend = names(shp_pch_map),
      pch    = unname(shp_pch_map),
      bty    = "n",
      cex    = 0.85
    )
  }

  # ── 12. Right text columns ─────────────────────────────────────────────────
  if (has_cols) {
    col_nms    <- names(cols)
    use_groups <- isTRUE(cols_by_group) && dodge_amt > 0

    if (use_groups) {
      # Wide-format mode: one value per label group, at group centre y.
      # Take the first non-empty entry within each group for each column.
      vis_bold <- which(vis_is_header & nchar(trimws(vis_lbl)) > 0L)
      for (j in seq_along(cols)) {
        raw <- as.character(df[[cols[j]]])
        grp_vals <- vapply(seq_len(n_vis), function(g) {
          v <- raw[which(group_ids == g)]
          v <- v[nzchar(trimws(v)) & !is.na(v)]
          if (length(v) == 0L) "" else v[1L]
        }, character(1L))
        draw_text_panel(
          labels   = grp_vals,
          n_vis    = n_vis,
          header   = col_nms[j],
          align    = "center",
          bold_idx = vis_bold,
          top_mar  = top_mar,
          bot_mar  = bot_mar
        )
      }
    } else {
      # Row-level mode (default): text at each row's dodged y position.
      bold_rows <- which(is_header & nchar(trimws(lbl)) > 0L)
      for (j in seq_along(cols)) {
        vals <- as.character(df[[cols[j]]])
        draw_text_panel(
          labels      = vals,
          n_vis       = n_vis,
          header      = col_nms[j],
          align       = "center",
          bold_idx    = bold_rows,
          top_mar     = top_mar,
          bot_mar     = bot_mar,
          y_positions = row_y
        )
      }
    }
  }

  invisible(NULL)
}
