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
