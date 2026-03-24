# forrest 0.1.0

* Initial release.
* `forrest()` creates publication-ready forest plots with support for subgroup
  headers, summary estimates (diamonds), grouped color mapping, and optional
  text columns alongside the plot.
* `forrest()` gains a `weight` argument to scale point size proportionally to
  study weights — the conventional meta-analysis display.
* Confidence intervals that extend beyond `xlim` are now clipped at the axis
  boundary; a directional arrow indicates the truncated side.
