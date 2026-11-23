# forrest (development version)

## Bug fixes

- When `group` or `shape` named the same column as `label` and `section` was set, the legend listed the section names and indented labels as group levels. Group and shape values are now read before section rows are inserted, and inserted rows no longer add an `NA` entry to either legend.
- `dodge = TRUE` failed with "missing value where TRUE/FALSE needed" when a label was `NA`. Missing labels are now drawn as blank text.
- On a log axis, the arrows that mark an interval running past `xlim` had shafts of very different lengths at the two ends of the axis. Their length is now 2% of the axis width on both sides.
- `col` was ignored for the legend when `group` was also set, and a per-row `col` vector lost its alignment with the rows once `section` inserted header rows. With `group`, `col` now gives one colour per group level, by name or in order; without `group`, it must have length 1 or one entry per row.
- `section_cols` and `subsection_cols` wrote the header value into their source column rather than into the column shown under the given header, so the header cell stayed empty whenever the two columns differed.
- Panel headers set by `header` and the names of `cols` overlapped the first row in plots with many rows; they now sit a fixed distance above the plot region.
- With `dodge`, summary diamonds in the same row overlapped each other. Their height is now capped at 0.45 times the dodge offset.
- A `weight` column with no usable values produced a warning from `max()`. Marker sizes now fall back to `cex`.

## Changes

- With `log_scale = TRUE`, `ref_line` now defaults to 1 instead of 0, which cannot be drawn on a log axis. An explicit `ref_line` of 0 or less gives a warning and no line.
- `log_scale = TRUE` with a non-positive estimate, confidence limit or `xlim` is now an error that explains the cause, instead of a failure inside the plotting code.
- Column arguments must be single strings, `theme` lists may use only the six documented keys, and `save_forrest()` checks that `plot` is a function. Each case now stops with a message naming the argument.

## Documentation

- The theme keys are listed in `?forrest`, and the documentation of `ref_label`, `ref_line` and `col` matches what they do.
- Regression vignette: the g-computation and IPW examples labelled the male stratum "Female" and the female stratum "Male"; the age-quartile labels did not match the quartiles of the data; the simulated confidence intervals in the distributed-lag and life-course examples were not centred on their estimates; and odds ratios and mean differences shared one axis. All are corrected.
- The several-exposures example in the meta-analysis vignette and the stage-comparison example in the regression vignette now sort their rows so that `dodge` groups them.
- The README comparison table is corrected against the current releases of forestplot, forestploter and ggforestplot. The README and vignettes are rewritten throughout.

# forrest 0.3.0

## New features

- `forrest()` gains a `dodge` argument (logical or positive numeric, default `FALSE`). When set, consecutive rows that share the same `label` value are grouped together and their confidence intervals are drawn with a small vertical offset so they do not overlap. The shared label is displayed once at the centre of the group. Designed to be used with `group` (for colour) and/or `shape` (for point character) to visually distinguish the overlaid series. A numeric value controls the offset directly; `TRUE` uses a default of `0.25` y-axis units.

- `forrest()` gains a `shape` argument (column name string, default `NULL`). When provided, different values of the column are rendered with different point characters from a built-in set (circle, triangle, square, diamond, …), and a shape legend is drawn automatically. Most useful in combination with `group` to encode two categorical dimensions simultaneously (e.g. colour = time period, shape = sex).

- `forrest()` gains a `legend_shape_pos` argument (default `"bottomright"`) to control the position of the shape legend independently of `legend_pos`.

- `forrest()` gains a `cols_by_group` argument (default `FALSE`). When `TRUE` and `dodge` is active, each text column in `cols` is collapsed to one value per label group displayed at the group centre y position. This produces a wide-format text table — one row per label, one column per condition — as commonly seen in multi-period epidemiology papers (vs. the default behaviour of stacking text at each individual row's dodged y position).

## Documentation

- README quick-start gains a _Multiple estimates per row_ example demonstrating `dodge` with `group`.

- README quick-start regression example now shows a formatted text column (`cols`) and a panel header (`header`) alongside the plot, making all three core features visible in one place.

- Getting-started vignette gains two new sections: _Multiple estimates per row_ and _Point shapes_.

- Regression vignette: the _Multiple predictors from one model_ example now includes a `cols = c("Coef (95% CI)" = "coef_ci")` column and matching `header`, consistent with the logistic and dose-response examples.

## Infrastructure

- Website URL added to `DESCRIPTION` `URL` field.

# forrest 0.2.0

## New features

- `save_forrest()` — new exported function to write a forest plot to a file. The graphics device (PDF, PNG, SVG, TIFF) is inferred from the file extension; resolution can be controlled via `dpi` for raster formats.

- `forrest()` gains a `stripe` argument (`FALSE` by default). When `TRUE`, alternate rows are shaded with a light-grey background to improve readability in tables with many rows.

## Documentation

- Revised framing: `forrest` is now documented as a general-purpose tool for any tabular estimates-and-CIs data, not only meta-analyses. The description, README, and vignettes have been updated accordingly.

- New vignette _Forest plots for regression results_ covers four practical patterns: multiple predictors from one model, comparing estimates across adjustment models, same predictor across multiple outcomes, and dose-response (exposure categories). Examples use `broom::tidy()` for parameter extraction.

- Getting-started vignette updated with a `stripe` example and a _Saving plots_ section.

- README now leads with a regression-model example and an updated feature comparison table.

## Refactoring

- Drawing helpers (`draw_diamond()`, `draw_text_panel()`) moved from `R/utils.R` to a dedicated `R/draw.R`, improving code organisation.

- `save_forrest()` lives in its own `R/save.R`.

## Bug fixes

- Horizontal gridlines are now drawn at `lwd = 0.7` (was `0.8`) for a slightly lighter appearance.

# forrest 0.1.0

- Initial release.
- `forrest()` creates publication-ready forest plots with support for subgroup headers, summary estimates (diamonds), grouped colour mapping, and optional text columns alongside the plot.
- `forrest()` gains a `weight` argument to scale point size proportionally to row weights.
- Confidence intervals that extend beyond `xlim` are clipped at the axis boundary; a directional arrow indicates the truncated side.
