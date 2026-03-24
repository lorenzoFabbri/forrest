basic_dat <- function() {
  data.frame(
    label = c("Study A", "Study B", "Study C", "Pooled"),
    estimate = c(-0.15, 0.10, -0.05, -0.05),
    lower = c(-0.40, -0.10, -0.25, -0.20),
    upper = c(0.10, 0.30, 0.15, 0.10),
    is_sum = c(FALSE, FALSE, FALSE, TRUE),
    grp = c("A", "A", "B", "B"),
    or_text = c(
      "-0.15 (-0.40, 0.10)",
      "0.10 (-0.10, 0.30)",
      "-0.05 (-0.25, 0.15)",
      "-0.05 (-0.20, 0.10)"
    )
  )
}

# ── Basic rendering ────────────────────────────────────────────────────────────

test_that("forrest() renders without error for minimal input", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  expect_no_error(
    forrest(dat, estimate = "estimate", lower = "lower", upper = "upper")
  )
})

test_that("forrest() renders with all core arguments", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label",
      is_summary = "is_sum",
      ref_line = 0,
      xlab = "Effect (95% CI)",
      title = "Test plot"
    )
  )
})

test_that("forrest() renders with group coloring and legend", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label",
      group = "grp"
    )
  )
})

test_that("forrest() renders with right text columns", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label",
      header = "Study",
      cols = c("OR (95% CI)" = "or_text")
    )
  )
})

test_that("forrest() renders on log scale with ref_line = 1", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    label = c("Study A", "Study B", "Pooled"),
    estimate = c(1.2, 0.8, 1.0),
    lower = c(0.9, 0.6, 0.85),
    upper = c(1.6, 1.1, 1.20),
    is_sum = c(FALSE, FALSE, TRUE)
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label",
      is_summary = "is_sum",
      ref_line = 1,
      log_scale = TRUE
    )
  )
})

# ── Subgroup header rows (NA estimates) ────────────────────────────────────────

test_that("forrest() handles NA estimate rows as headers", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    label = c("Subgroup A", "Study 1", "Study 2", "", "Subgroup B", "Study 3"),
    estimate = c(NA, 0.5, 0.7, NA, NA, 1.1),
    lower = c(NA, 0.3, 0.5, NA, NA, 0.8),
    upper = c(NA, 0.7, 0.9, NA, NA, 1.4),
    is_sum = rep(FALSE, 6)
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label"
    )
  )
})

# ── Input type compatibility ───────────────────────────────────────────────────

test_that("forrest() accepts a tibble", {
  skip_if_not_installed("tibble")
  pdf(nullfile())
  on.exit(dev.off())
  dat <- tibble::as_tibble(basic_dat())
  expect_no_error(
    forrest(dat, estimate = "estimate", lower = "lower", upper = "upper")
  )
})

test_that("forrest() accepts a data.table", {
  skip_if_not_installed("data.table")
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.table::as.data.table(basic_dat())
  expect_no_error(
    forrest(dat, estimate = "estimate", lower = "lower", upper = "upper")
  )
})

# ── Error handling ─────────────────────────────────────────────────────────────

test_that("forrest() errors on missing estimate column", {
  dat <- basic_dat()
  expect_error(
    forrest(dat, estimate = "nope", lower = "lower", upper = "upper"),
    "Column 'nope' not found"
  )
})

test_that("forrest() errors on non-data-frame input", {
  expect_error(
    forrest(list(a = 1), estimate = "a", lower = "b", upper = "c"),
    "data frame"
  )
})

test_that("forrest() errors when cols is unnamed", {
  dat <- basic_dat()
  expect_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      cols = c("or_text") # unnamed → error
    ),
    "named"
  )
})

test_that("forrest() errors when widths length is wrong", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  expect_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      cols = c("OR" = "or_text"),
      widths = c(1, 2) # should be length 3
    ),
    "widths"
  )
})

test_that("forrest() renders with study weights", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  dat$weight <- c(10, 5, 20, 35)
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label",
      weight = "weight"
    )
  )
})

test_that("forrest() clips CI lines at xlim and draws arrows", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    estimate = c(-0.05, 0.10),
    lower = c(-0.80, -0.05), # first row's lower extends beyond xlim
    upper = c(0.05, 0.90) # second row's upper extends beyond xlim
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      xlim = c(-0.3, 0.3)
    )
  )
})

test_that("forrest() returns NULL invisibly", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  result <- forrest(
    dat,
    estimate = "estimate",
    lower = "lower",
    upper = "upper"
  )
  expect_null(result)
})

test_that("forrest() renders with stripe = TRUE", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label",
      stripe = TRUE
    )
  )
})

# ── dodge ──────────────────────────────────────────────────────────────────────

dodge_dat <- function() {
  data.frame(
    label    = rep(c("A", "B", "C"), each = 2),
    period   = rep(c("early", "late"), 3),
    estimate = c( 0.42,  0.30, -0.18, -0.10,  0.31,  0.22),
    lower    = c( 0.22,  0.12, -0.38, -0.28,  0.12,  0.04),
    upper    = c( 0.62,  0.48,  0.02,  0.08,  0.50,  0.40)
  )
}

test_that("forrest() renders with dodge = TRUE", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- dodge_dat()
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower    = "lower",
      upper    = "upper",
      label    = "label",
      group    = "period",
      dodge    = TRUE
    )
  )
})

test_that("forrest() renders with numeric dodge", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- dodge_dat()
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower    = "lower",
      upper    = "upper",
      label    = "label",
      dodge    = 0.3
    )
  )
})

test_that("forrest() dodge works with cols (row-level text)", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- dodge_dat()
  dat$txt <- sprintf("%.2f", dat$estimate)
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower    = "lower",
      upper    = "upper",
      label    = "label",
      group    = "period",
      dodge    = TRUE,
      header   = "Exposure",
      cols     = c("Coef" = "txt")
    )
  )
})

test_that("forrest() dodge works with cols_by_group = TRUE", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- dodge_dat()
  dat$est_ci <- sprintf("%.2f", dat$estimate)
  dat$text_early <- ifelse(dat$period == "early", dat$est_ci, "")
  dat$text_late  <- ifelse(dat$period == "late",  dat$est_ci, "")
  expect_no_error(
    forrest(
      dat,
      estimate      = "estimate",
      lower         = "lower",
      upper         = "upper",
      label         = "label",
      group         = "period",
      dodge         = TRUE,
      cols_by_group = TRUE,
      cols          = c("Early" = "text_early", "Late" = "text_late")
    )
  )
})

test_that("forrest() cols_by_group = TRUE ignored when dodge = FALSE", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  dat$txt <- dat$or_text
  expect_no_error(
    forrest(
      dat,
      estimate      = "estimate",
      lower         = "lower",
      upper         = "upper",
      label         = "label",
      dodge         = FALSE,
      cols_by_group = TRUE,
      cols          = c("OR" = "txt")
    )
  )
})

test_that("forrest() dodge = FALSE (default) is unchanged", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower    = "lower",
      upper    = "upper",
      label    = "label",
      dodge    = FALSE
    )
  )
})

test_that("forrest() dodge works with stripe = TRUE", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- dodge_dat()
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower    = "lower",
      upper    = "upper",
      label    = "label",
      group    = "period",
      dodge    = TRUE,
      stripe   = TRUE
    )
  )
})

test_that("forrest() dodge works with header rows (NA estimates)", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    label    = c("Group 1", "A", "A", "Group 2", "B", "B"),
    period   = c(NA, "early", "late", NA, "early", "late"),
    estimate = c(NA,  0.42,  0.30, NA, -0.18, -0.10),
    lower    = c(NA,  0.22,  0.12, NA, -0.38, -0.28),
    upper    = c(NA,  0.62,  0.48, NA,  0.02,  0.08),
    is_sum   = rep(FALSE, 6)
  )
  expect_no_error(
    forrest(
      dat,
      estimate   = "estimate",
      lower      = "lower",
      upper      = "upper",
      label      = "label",
      is_summary = "is_sum",
      dodge      = TRUE
    )
  )
})

# ── shape ──────────────────────────────────────────────────────────────────────

test_that("forrest() renders with shape column", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  dat$sex <- c("F", "M", "F", "M")
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower    = "lower",
      upper    = "upper",
      label    = "label",
      shape    = "sex"
    )
  )
})

test_that("forrest() renders with dodge + group + shape", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    label    = rep(c("A", "B"), each = 4),
    period   = rep(rep(c("early", "late"), each = 2), 2),
    sex      = rep(c("F", "M"), 4),
    estimate = c( 0.42,  0.30,  0.22,  0.15, -0.18, -0.10, -0.25, -0.20),
    lower    = c( 0.22,  0.12,  0.05,  0.00, -0.38, -0.28, -0.45, -0.38),
    upper    = c( 0.62,  0.48,  0.39,  0.30,  0.02,  0.08, -0.05, -0.02)
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower    = "lower",
      upper    = "upper",
      label    = "label",
      group    = "period",
      shape    = "sex",
      dodge    = TRUE
    )
  )
})

test_that("forrest() shape legend can be suppressed", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  dat$sex <- c("F", "M", "F", "M")
  expect_no_error(
    forrest(
      dat,
      estimate         = "estimate",
      lower            = "lower",
      upper            = "upper",
      label            = "label",
      shape            = "sex",
      legend_shape_pos = NULL
    )
  )
})

test_that("forrest() errors on missing shape column", {
  dat <- basic_dat()
  expect_error(
    forrest(
      dat,
      estimate = "estimate",
      lower    = "lower",
      upper    = "upper",
      shape    = "nonexistent"
    ),
    "Column 'nonexistent' not found"
  )
})

# ── save_forrest() ─────────────────────────────────────────────────────────────

test_that("save_forrest() writes a PDF file", {
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp))
  dat <- basic_dat()
  result <- save_forrest(
    tmp,
    function() {
      forrest(dat, estimate = "estimate", lower = "lower", upper = "upper")
    }
  )
  expect_equal(result, tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0L)
})

test_that("save_forrest() writes a PNG file", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  dat <- basic_dat()
  expect_no_error(
    save_forrest(
      tmp,
      function() {
        forrest(dat, estimate = "estimate", lower = "lower", upper = "upper")
      },
      width = 6,
      height = 4,
      dpi = 72
    )
  )
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0L)
})

test_that("save_forrest() errors on unsupported extension", {
  dat <- basic_dat()
  expect_error(
    save_forrest(
      tempfile(fileext = ".bmp"),
      function() {
        forrest(dat, estimate = "estimate", lower = "lower", upper = "upper")
      }
    ),
    "Supported extensions"
  )
})
