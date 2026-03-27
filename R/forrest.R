#' Create a forest plot
#'
#' Draws a publication-ready forest plot from a data frame. Each row
#' represents one estimate — a study, a predictor, a model, a subgroup, or any
#' other unit of analysis.
#'
#' Rows with `NA` estimates are treated as reference-category rows: they produce
#' no point or confidence interval, and their label is rendered in regular
#' (non-bold) font.  To create section headers and spacers automatically, use
#' the `section` (and optionally `subsection`) arguments instead of inserting
#' NA rows by hand.
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
#' @param section Column name (string) for a grouping variable that determines
#'   section structure. Whenever the value of this column changes (run-length
#'   boundary), a bold section header row is automatically inserted before the
#'   group. Row order is preserved; no automatic sorting is applied. See also
#'   `section_indent`, `section_spacer`, and `section_cols`.
#' @param subsection Column name (string) for a second-level grouping variable.
#'   Requires `section`. Inserts indented sub-headers beneath each section
#'   header. See also `subsection_cols`.
#' @param section_indent Logical. If `TRUE` (default), label values of data
#'   rows within a section are automatically indented by two spaces (four spaces
#'   for rows within a subsection).
#' @param section_spacer Logical. If `TRUE` (default), a blank spacer row is
#'   appended after the last row of each section.
#' @param section_cols Named character vector. Names must be a subset of the
#'   names of `cols`. Values are column names in `data` whose first non-NA
#'   entry in each section is shown in that section's header row. Columns not
#'   listed here display `""` in the header row. Use this to show section-level
#'   summaries (e.g. `"k = 3 studies"`) next to the section header.
#' @param subsection_cols Like `section_cols` but for subsection header rows.
#' @param ref_label Logical. When `TRUE` and `section` is provided, rows with
#'   `NA` estimates that are present in the original data (i.e. reference
#'   category rows, not auto-generated headers) have `" (Ref.)"` appended to
#'   their label. Default is `FALSE`.
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
#'   Passed to `legend()`. Use `NULL` to suppress. Default is `"topright"`.
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
#' @param theme Visual theme name (`"default"`, `"minimal"`, `"classic"`) or a
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
#' # Section headers from a grouping column
#' dat2 <- data.frame(
#'   domain    = c("Lifestyle", "Lifestyle", "Clinical", "Clinical"),
#'   predictor = c("Physical activity", "Diet quality",
#'                 "BMI (per 5 kg/m\u00b2)", "Systolic BP (per 10 mmHg)"),
#'   estimate  = c(-0.31, -0.18,  0.19,  0.25),
#'   lower     = c(-0.51, -0.36, -0.02,  0.08),
#'   upper     = c(-0.11, -0.00,  0.40,  0.42)
#' )
#' forrest(dat2,
#'   estimate  = "estimate",
#'   lower     = "lower",
#'   upper     = "upper",
#'   label     = "predictor",
#'   section   = "domain",
#'   xlab      = "Regression coefficient (95% CI)"
#' )
#'
#' @export
forrest <- function(
  data,
  estimate,
  lower,
  upper,
  label            = NULL,
  group            = NULL,
  is_summary       = NULL,
  weight           = NULL,
  section          = NULL,
  subsection       = NULL,
  section_indent   = TRUE,
  section_spacer   = TRUE,
  section_cols     = NULL,
  subsection_cols  = NULL,
  ref_label        = FALSE,
  ref_line         = 0,
  log_scale        = FALSE,
  xlim             = NULL,
  xlab             = "Estimate (95% CI)",
  title            = NULL,
  header           = NULL,
  cols             = NULL,
  widths           = NULL,
  stripe           = FALSE,
  dodge            = FALSE,
  pch              = 15,
  shape            = NULL,
  lwd              = 2,
  cex              = 1,
  col              = NULL,
  cols_by_group    = FALSE,
  legend_pos       = "topright",
  legend_shape_pos = "bottomright",
  theme            = "default",
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
  check_col(df, lower,    "lower")
  check_col(df, upper,    "upper")
  check_col_opt(df, label,      "label")
  check_col_opt(df, group,      "group")
  check_col_opt(df, is_summary, "is_summary")
  check_col_opt(df, weight,     "weight")
  check_col_opt(df, shape,      "shape")
  check_col_opt(df, section,    "section")
  check_col_opt(df, subsection, "subsection")

  if (!is.null(subsection) && is.null(section)) {
    stop("`subsection` requires `section` to also be specified.", call. = FALSE)
  }
  if (!is.null(section) && !is.null(label) && section == label) {
    stop("`section` and `label` must refer to different columns.", call. = FALSE)
  }

  if (!is.null(cols)) {
    if (is.null(names(cols)) || any(names(cols) == "")) {
      stop("`cols` must be a *named* character vector.", call. = FALSE)
    }
    for (nm in cols) check_col(df, nm, sprintf("cols[\"%s\"]", nm))
  }

  # section_cols / subsection_cols keys must be subsets of cols names
  if (!is.null(section_cols)) {
    if (is.null(cols)) {
      stop("`section_cols` requires `cols` to be specified.", call. = FALSE)
    }
    bad <- setdiff(names(section_cols), names(cols))
    if (length(bad) > 0L) {
      stop(
        sprintf(
          "`section_cols` names must match `cols` names. Unknown: %s.",
          paste(bad, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    for (cn in unname(section_cols)) check_col(df, cn, "section_cols")
  }
  if (!is.null(subsection_cols)) {
    if (is.null(cols)) {
      stop("`subsection_cols` requires `cols` to be specified.", call. = FALSE)
    }
    bad <- setdiff(names(subsection_cols), names(cols))
    if (length(bad) > 0L) {
      stop(
        sprintf(
          "`subsection_cols` names must match `cols` names. Unknown: %s.",
          paste(bad, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    for (cn in unname(subsection_cols)) check_col(df, cn, "subsection_cols")
  }

  # ── 1b. Section expansion ──────────────────────────────────────────────────
  # When `section` is provided, build_sections() inserts bold header rows,
  # optional sub-headers, and blank spacer rows into `df` before any drawing.
  # The three flag vectors identify which of the expanded rows are structural
  # (auto-inserted) vs. original data rows.
  if (!is.null(section)) {
    # Map section_cols / subsection_cols from display-header names to data-
    # column names so build_sections() can look up values in df
    sc_data_cols  <- if (!is.null(section_cols))    unname(section_cols)    else NULL
    ssc_data_cols <- if (!is.null(subsection_cols)) unname(subsection_cols) else NULL

    sec_result <- build_sections(
      df              = df,
      estimate        = estimate,
      lower           = lower,
      upper           = upper,
      label           = label,
      is_summary      = is_summary,
      weight          = weight,
      section         = section,
      subsection      = subsection,
      section_indent  = section_indent,
      section_spacer  = section_spacer,
      cols            = if (!is.null(cols)) unname(cols) else character(0),
      section_cols    = if (!is.null(sc_data_cols))
        stats::setNames(sc_data_cols, sc_data_cols) else NULL,
      subsection_cols = if (!is.null(ssc_data_cols))
        stats::setNames(ssc_data_cols, ssc_data_cols) else NULL
    )
    df                    <- sec_result$df
    n                     <- nrow(df)
    .is_section_hdr       <- sec_result$is_section_header
    .is_subsection_hdr    <- sec_result$is_subsection_header
    .is_spacer            <- sec_result$is_spacer
  } else {
    .is_section_hdr    <- rep(FALSE, n)
    .is_subsection_hdr <- rep(FALSE, n)
    .is_spacer         <- rep(FALSE, n)
  }

  # ── 2. Theme ───────────────────────────────────────────────────────────────
  th <- resolve_theme(theme)

  # ── 3. Extract columns ─────────────────────────────────────────────────────
  est <- as.numeric(df[[estimate]])
  lo  <- as.numeric(df[[lower]])
  hi  <- as.numeric(df[[upper]])
  lbl <- if (!is.null(label)) {
    as.character(df[[label]])
  } else {
    as.character(seq_len(n))
  }
  grp <- if (!is.null(group))  as.character(df[[group]])  else NULL
  shp <- if (!is.null(shape))  as.character(df[[shape]])  else NULL
  is_sum <- if (!is.null(is_summary)) {
    as.logical(df[[is_summary]])
  } else {
    rep(FALSE, n)
  }
  wt <- if (!is.null(weight)) as.numeric(df[[weight]]) else NULL

  # Structural header rows: auto-inserted section/subsection headers and
  # spacers.  These produce no graphical element; section/subsection labels
  # are rendered in bold; spacer labels ("") are skipped.
  is_struct <- .is_section_hdr | .is_subsection_hdr | .is_spacer

  # Reference-category rows: user-supplied NA-estimate rows that are not
  # structural.  They produce no CI or point, and their label is regular
  # (non-bold) font.  Optionally annotated with " (Ref.)".
  is_ref <- is.na(est) & !is_sum & !is_struct

  # All rows that produce no graphical CI element
  is_no_ci <- is_struct | is_ref

  # Optionally append "(Ref.)" to reference-category labels
  if (isTRUE(ref_label) && any(is_ref)) {
    lbl[is_ref] <- paste0(lbl[is_ref], " (Ref.)")
  }

  # Bold rows: only auto-inserted structural headers with non-empty labels
  is_bold <- (.is_section_hdr | .is_subsection_hdr) & nchar(trimws(lbl)) > 0L

  # ── 4. Colours ─────────────────────────────────────────────────────────────
  if (!is.null(col)) {
    col_vec     <- rep_len(col, n)
    grp_col_map <- NULL
  } else if (!is.null(grp)) {
    grp_col_map <- group_colors(grp)
    col_vec     <- unname(grp_col_map[grp])
  } else {
    col_vec     <- rep("#333333", n)
    grp_col_map <- NULL
  }

  # ── 5. Point characters ────────────────────────────────────────────────────
  if (!is.null(shp)) {
    shp_pch_map <- group_shapes(shp)
    pch_vec     <- unname(shp_pch_map[shp])
    pch_vec[is.na(pch_vec)] <- pch
  } else {
    pch_vec     <- rep(pch, n)
    shp_pch_map <- NULL
  }

  # ── 6. Per-row point sizes ─────────────────────────────────────────────────
  # Square-root scaling so marker area is proportional to weight.
  # Structural, reference, and summary rows always use cex 1x.
  if (!is.null(wt)) {
    reg_wt <- wt
    reg_wt[is_sum | is_no_ci] <- NA_real_
    max_wt  <- max(reg_wt, na.rm = TRUE)
    cex_vec <- cex * sqrt(reg_wt / max_wt)
    cex_vec[is.na(cex_vec)] <- cex
  } else {
    cex_vec <- rep(cex, n)
  }

  # ── 7. X-axis limits ───────────────────────────────────────────────────────
  xlim_auto <- is.null(xlim)
  if (xlim_auto) {
    vals <- c(lo, hi)
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0L) stop("No finite CI values found.", call. = FALSE)
    rng <- range(vals)
    if (log_scale) {
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
  dodge_amt <- if (isTRUE(dodge)) 0.25 else
    if (is.numeric(dodge) && dodge > 0) as.numeric(dodge) else 0

  if (dodge_amt > 0) {
    # Structural rows (headers, spacers) are always dodge singletons
    group_ids    <- compute_dodge_groups(lbl, is_struct)
    n_vis <- max(group_ids)

    grp_center_y <- (n_vis + 1L) - seq_len(n_vis)

    row_y <- numeric(n)
    for (g in seq_len(n_vis)) {
      idx     <- which(group_ids == g)
      k       <- length(idx)
      offsets <- if (k == 1L) 0 else
        seq(-(k - 1L) / 2, (k - 1L) / 2, length.out = k) * dodge_amt
      row_y[idx] <- grp_center_y[g] + offsets
    }

    vis_lbl       <- vapply(
      seq_len(n_vis),
      function(g) lbl[which(group_ids == g)[1L]],
      character(1L)
    )
    vis_center_y  <- grp_center_y
    vis_is_bold   <- vapply(
      seq_len(n_vis),
      function(g) is_bold[which(group_ids == g)[1L]],
      logical(1L)
    )
  } else {
    n_vis         <- n
    row_y         <- rev(seq_len(n))
    vis_lbl       <- lbl
    vis_center_y  <- row_y
    vis_is_bold   <- is_bold
  }

  ylim <- c(0.5, n_vis + 0.5)

  # ── 9. Margins and layout ──────────────────────────────────────────────────
  has_cols <- !is.null(cols)
  top_mar  <- if (!is.null(title)) 3 else if (!is.null(header)) 2.5 else 1.5
  bot_mar  <- 4

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  if (has_cols) {
    n_right <- length(cols)
    if (is.null(widths)) widths <- c(2.5, 4.5, rep(1.8, n_right))
    if (length(widths) != 2L + n_right) {
      stop(
        sprintf(
          "`widths` must have length %d (label + plot + %d column(s)).",
          2L + n_right, n_right
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
    draw_text_panel(
      labels   = vis_lbl,
      n_vis    = n_vis,
      header   = header,
      align    = "left",
      bold_idx = which(vis_is_bold),
      top_mar  = top_mar,
      bot_mar  = bot_mar
    )
  }

  # ── 11. Main forest plot panel ─────────────────────────────────────────────
  if (has_cols) {
    left_mar <- 0.3
  } else {
    max_chars <- max(nchar(lbl), na.rm = TRUE)
    left_mar  <- max(4, max_chars * 0.6)
  }
  graphics::par(mar = c(bot_mar, left_mar, top_mar, 1))

  # Regular rows: have a finite estimate, are not summary, not structural,
  # and not reference-category rows
  reg <- !is_sum & !is_no_ci & !is.na(est)

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

  # Alternating row stripes
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

  # Horizontal gridlines
  graphics::abline(
    h   = seq_len(n_vis),
    col = th$grid_col,
    lty = th$grid_lty,
    lwd = th$grid_lwd
  )

  # Vertical reference line
  if (!is.null(ref_line) && is.finite(ref_line)) {
    graphics::abline(v = ref_line, lty = th$ref_lty, lwd = 1, col = th$ref_col)
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
    pch_reg <- pch_vec[reg_idx]

    clip_lo <- lo_reg < xlim[1L]
    clip_hi <- hi_reg > xlim[2L]
    lo_draw <- pmax(lo_reg, xlim[1L])
    hi_draw <- pmin(hi_reg, xlim[2L])

    cap_h <- 0.12
    for (k in seq_along(reg_idx)) {
      graphics::segments(
        lo_draw[k], y_reg[k], hi_draw[k], y_reg[k],
        lwd = lwd, col = col_reg[k]
      )
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

  # Y-axis labels (when no separate label panel is used)
  if (!has_cols) {
    font_vec <- ifelse(vis_is_bold, 2L, 1L)
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
      # Wide-format mode: one value per label group at group centre y.
      # Section/subsection header rows contribute "" (already set by
      # build_sections), so they never crowd out row-level values.
      vis_bold_idx <- which(vis_is_bold)
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
          bold_idx = vis_bold_idx,
          top_mar  = top_mar,
          bot_mar  = bot_mar
        )
      }
    } else {
      # Row-level mode: text at each row's (possibly dodged) y position.
      bold_rows <- which(is_bold)
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
