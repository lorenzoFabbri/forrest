# Theme infrastructure for forrest plots — not exported

#' Built-in visual themes
#'
#' Each entry is a named list of style overrides applied on top of the
#' `"default"` base theme.  Users can also pass a plain list of any of
#' these keys to `forrest(theme = list(...))` for bespoke styling.
#'
#' @section Theme keys:
#' \describe{
#'   \item{`grid_col`}{Colour of horizontal gridlines.}
#'   \item{`grid_lty`}{Line type for gridlines.}
#'   \item{`grid_lwd`}{Line width for gridlines.}
#'   \item{`ref_col`}{Colour of the reference (null) vertical line.}
#'   \item{`ref_lty`}{Line type for the reference line.}
#'   \item{`stripe_col`}{Background colour for alternating stripe rows.}
#' }
#' @noRd
.theme_defaults <- list(
  grid_col   = "#e8e8e8",
  grid_lty   = 1L,
  grid_lwd   = 0.7,
  ref_col    = "gray45",
  ref_lty    = 2L,
  stripe_col = "#f2f2f2"
)

.themes <- list(
  default = list(),
  minimal = list(
    grid_col = "#f0f0f0",
    grid_lwd = 0.5,
    ref_col  = "#777777"
  ),
  classic = list(
    grid_col   = "lightgray",
    grid_lty   = 3L,
    grid_lwd   = 0.7,
    ref_col    = "black",
    ref_lty    = 1L,
    stripe_col = "#efefef"
  )
)

#' Resolve a theme specification to a complete style list
#'
#' @param theme `NULL`, a single character string naming a built-in theme, or
#'   a named list of style overrides.
#' @return A named list with keys from `.theme_defaults`.
#' @noRd
resolve_theme <- function(theme) {
  if (is.null(theme) || identical(theme, "default")) {
    return(.theme_defaults)
  }
  if (is.character(theme)) {
    if (length(theme) != 1L || !theme %in% names(.themes)) {
      stop(
        sprintf(
          "`theme` must be one of: %s.",
          paste(sprintf('"%s"', names(.themes)), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    base      <- .theme_defaults
    overrides <- .themes[[theme]]
    base[names(overrides)] <- overrides
    return(base)
  }
  if (is.list(theme)) {
    base              <- .theme_defaults
    base[names(theme)] <- theme
    return(base)
  }
  stop("`theme` must be NULL, a character string, or a named list.",
       call. = FALSE)
}
