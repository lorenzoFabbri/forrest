# forrest 0.3.0

## New features

- `forrest()` gains a `dodge` argument (logical or positive numeric, default
  `FALSE`). When set, consecutive rows that share the same `label` value are
  grouped together and their confidence intervals are drawn with a small
  vertical offset so they do not overlap. The shared label is displayed once
  at the centre of the group. Designed to be used with `group` (for colour)
  and/or `shape` (for point character) to visually distinguish the overlaid
  series. A numeric value controls the offset directly; `TRUE` uses a default
  of `0.25` y-axis units.

- `forrest()` gains a `shape` argument (column name string, default `NULL`).
  When provided, different values of the column are rendered with different
  point characters from a built-in set (circle, triangle, square, diamond, …),
  and a shape legend is drawn automatically. Most useful in combination with
  `group` to encode two categorical dimensions simultaneously (e.g. colour =
  time period, shape = sex).

- `forrest()` gains a `legend_shape_pos` argument (default `"bottomright"`)
  to control the position of the shape legend independently of `legend_pos`.

- `forrest()` gains a `cols_by_group` argument (default `FALSE`). When `TRUE`
  and `dodge` is active, each text column in `cols` is collapsed to one value
  per label group displayed at the group centre y position. This produces a
  wide-format text table — one row per label, one column per condition — as
  commonly seen in multi-period epidemiology papers (vs. the default behaviour
  of stacking text at each individual row's dodged y position).

## Documentation

- README quick-start gains a _Multiple estimates per row_ example
  demonstrating `dodge` with `group`.

- README quick-start regression example now shows a formatted text column
  (`cols`) and a panel header (`header`) alongside the plot, making all three
  core features visible in one place.

- Getting-started vignette gains two new sections: _Multiple estimates per
  row_ and _Point shapes_.

- Regression vignette: the _Multiple predictors from one model_ example now
  includes a `cols = c("Coef (95% CI)" = "coef_ci")` column and matching
  `header`, consistent with the logistic and dose-response examples.

## Infrastructure

- Website URL added to `DESCRIPTION` `URL` field.

---

# forrest 0.2.0

## New features

- `save_forrest()` — new exported function to write a forest plot to a file.
  The graphics device (PDF, PNG, SVG, TIFF) is inferred from the file
  extension; resolution can be controlled via `dpi` for raster formats.

- `forrest()` gains a `stripe` argument (`FALSE` by default). When `TRUE`,
  alternate rows are shaded with a light-grey background to improve
  readability in tables with many rows.

## Documentation

- Revised framing: `forrest` is now documented as a general-purpose tool
  for any tabular estimates-and-CIs data, not only meta-analyses. The
  description, README, and vignettes have been updated accordingly.

- New vignette _Forest plots for regression results_ covers four practical
  patterns: multiple predictors from one model, comparing estimates across
  adjustment models, same predictor across multiple outcomes, and
  dose-response (exposure categories). Examples use `broom::tidy()` for
  parameter extraction.

- Getting-started vignette updated with a `stripe` example and a _Saving
  plots_ section.

- README now leads with a regression-model example and an updated feature
  comparison table.

## Refactoring

- Drawing helpers (`draw_diamond()`, `draw_text_panel()`) moved from
  `R/utils.R` to a dedicated `R/draw.R`, improving code organisation.

- `save_forrest()` lives in its own `R/save.R`.

## Bug fixes

- Horizontal gridlines are now drawn at `lwd = 0.7` (was `0.8`) for a
  slightly lighter appearance.

---

# forrest 0.1.0

- Initial release.
- `forrest()` creates publication-ready forest plots with support for subgroup
  headers, summary estimates (diamonds), grouped colour mapping, and optional
  text columns alongside the plot.
- `forrest()` gains a `weight` argument to scale point size proportionally to
  row weights.
- Confidence intervals that extend beyond `xlim` are clipped at the axis
  boundary; a directional arrow indicates the truncated side.
