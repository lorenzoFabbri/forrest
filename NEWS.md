# forrest 0.2.0

## New features

* `save_forrest()` — new exported function to write a forest plot to a file.
  The graphics device (PDF, PNG, SVG, TIFF) is inferred from the file
  extension; resolution can be controlled via `dpi` for raster formats.

* `forrest()` gains a `stripe` argument (`FALSE` by default). When `TRUE`,
  alternate rows are shaded with a light-grey background to improve
  readability in tables with many rows.

## Documentation

* Revised framing: `forrest` is now documented as a general-purpose tool
  for any tabular estimates-and-CIs data, not only meta-analyses. The
  description, README, and vignettes have been updated accordingly.

* New vignette *Forest plots for regression results* covers four practical
  patterns: multiple predictors from one model, comparing estimates across
  adjustment models, same predictor across multiple outcomes, and
  dose-response (exposure categories). Examples use `broom::tidy()` for
  parameter extraction.

* Getting-started vignette updated with a `stripe` example and a *Saving
  plots* section.

* README now leads with a regression-model example and an updated feature
  comparison table.

## Refactoring

* Drawing helpers (`draw_diamond()`, `draw_text_panel()`) moved from
  `R/utils.R` to a dedicated `R/draw.R`, improving code organisation.

* `save_forrest()` lives in its own `R/save.R`.

## Bug fixes

* Horizontal gridlines are now drawn at `lwd = 0.7` (was `0.8`) for a
  slightly lighter appearance.

---

# forrest 0.1.0

* Initial release.
* `forrest()` creates publication-ready forest plots with support for subgroup
  headers, summary estimates (diamonds), grouped colour mapping, and optional
  text columns alongside the plot.
* `forrest()` gains a `weight` argument to scale point size proportionally to
  row weights.
* Confidence intervals that extend beyond `xlim` are clipped at the axis
  boundary; a directional arrow indicates the truncated side.
