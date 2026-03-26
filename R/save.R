#' Save a forest plot to a file
#'
#' Writes the plot produced by [forrest()] (or any base-R plotting code) to a
#' file. The graphics device is inferred from the file extension.
#'
#' @param file Output file path. Supported extensions: `.pdf`, `.png`, `.svg`,
#'   `.tiff`.
#' @param plot A zero-argument function whose body calls [forrest()].
#'   Evaluated inside the open graphics device.
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
#'   label    = c("Age (per 10 y)", "Female sex", "Current smoker"),
#'   estimate = c(0.42, -0.38, -0.31),
#'   lower    = c(0.22, -0.56, -0.51),
#'   upper    = c(0.62, -0.20, -0.11)
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
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
) {
  # Extract the file extension and dispatch to the right graphics device.
  # `tools::file_ext()` handles paths like "dir/plot.pdf" → "pdf"
  ext <- tolower(tools::file_ext(file))
  switch(
    ext,
    # Vector formats: PDF and SVG ignore `dpi`
    pdf = grDevices::pdf(
      file,
      width  = width,
      height = height,
      bg     = bg
    ),
    # Raster format: PNG — `units = "in"` + `res` converts from inches to pixels
    png = grDevices::png(
      file,
      width  = width,
      height = height,
      units  = "in",
      res    = dpi,
      bg     = bg
    ),
    # Vector format: SVG
    svg = grDevices::svg(
      file,
      width  = width,
      height = height,
      bg     = bg
    ),
    # Raster format: TIFF with lossless LZW compression
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
      "Cannot infer device from extension '.",
      ext,
      "'. ",
      "Supported extensions: pdf, png, svg, tiff.",
      call. = FALSE
    )
  )
  # Register `dev.off()` before calling `plot()` so the device is always
  # closed even if plotting raises an error
  on.exit(grDevices::dev.off(), add = TRUE)
  plot()
  invisible(file)
}
