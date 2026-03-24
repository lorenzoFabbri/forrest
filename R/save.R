#' Save a forest plot to a file
#'
#' Writes the plot produced by [forrest()] (or any base-R plotting code) to a
#' file. The graphics device is inferred from the file extension.
#'
#' @param file Output file path. Supported extensions: `.pdf`, `.png`, `.svg`,
#'   `.tiff`.
#' @param plot A zero-argument function (or `function() { ... }` block) whose
#'   body calls [forrest()]. It is evaluated inside the open graphics device.
#' @param width Plot width in inches. Default `7`.
#' @param height Plot height in inches. Default `5`.
#' @param dpi Resolution in dots per inch for raster formats (`.png`, `.tiff`).
#'   Ignored for vector formats (`.pdf`, `.svg`). Default `300`.
#' @param bg Background colour. Default `"white"`.
#'
#' @return Invisibly returns `file`.
#'
#' @examples
#' dat <- data.frame(
#'   label    = c("Exposure A", "Exposure B", "Exposure C"),
#'   estimate = c(0.42, -0.18, 0.31),
#'   lower    = c(0.22, -0.38, 0.12),
#'   upper    = c(0.62,  0.02, 0.50)
#' )
#' tmp <- tempfile(fileext = ".pdf")
#' save_forrest(tmp, function() {
#'   forrest(
#'     dat,
#'     estimate = "estimate",
#'     lower    = "lower",
#'     upper    = "upper",
#'     label    = "label",
#'     xlab     = "Regression coefficient (95% CI)"
#'   )
#' })
#'
#' @export
save_forrest <- function(
  file,
  plot,
  width  = 7,
  height = 5,
  dpi    = 300,
  bg     = "white"
) {
  ext <- tolower(tools::file_ext(file))
  switch(
    ext,
    pdf  = grDevices::pdf(
      file,
      width  = width,
      height = height,
      bg     = bg
    ),
    png  = grDevices::png(
      file,
      width  = width,
      height = height,
      units  = "in",
      res    = dpi,
      bg     = bg
    ),
    svg  = grDevices::svg(
      file,
      width  = width,
      height = height,
      bg     = bg
    ),
    tiff = grDevices::tiff(
      file,
      width       = width,
      height      = height,
      units       = "in",
      res         = dpi,
      bg          = bg,
      compression = "lzw"
    ),
    stop(
      "Cannot infer device from extension '.", ext, "'. ",
      "Supported extensions: pdf, png, svg, tiff.",
      call. = FALSE
    )
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  plot()
  invisible(file)
}
