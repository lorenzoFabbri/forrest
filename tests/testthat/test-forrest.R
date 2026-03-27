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

# ── build_sections() unit tests (no graphics device needed) ──────────────────

test_that("build_sections() inserts header rows at section boundaries", {
  df <- data.frame(
    region = c("Asia", "Asia", "Europe", "Europe"),
    study = c("S1", "S2", "S3", "S4"),
    estimate = c(0.1, 0.2, 0.3, 0.4),
    lower = c(0.0, 0.1, 0.2, 0.3),
    upper = c(0.2, 0.3, 0.4, 0.5)
  )
  res <- build_sections(
    df = df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    label = "study",
    is_summary = NULL,
    weight = NULL,
    section = "region",
    section_spacer = FALSE,
    section_indent = FALSE
  )
  # 2 sections × (1 header + 2 data rows) = 6
  expect_equal(nrow(res$df), 6L)
  expect_equal(sum(res$is_section_header), 2L)
  expect_equal(sum(res$is_subsection_header), 0L)
  expect_equal(sum(res$is_spacer), 0L)
})

test_that("build_sections() section header rows have NA estimate", {
  df <- data.frame(
    region = c("Asia", "Asia", "Europe"),
    study = c("S1", "S2", "S3"),
    estimate = c(0.1, 0.2, 0.3),
    lower = c(0.0, 0.1, 0.2),
    upper = c(0.2, 0.3, 0.4)
  )
  res <- build_sections(
    df = df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    label = "study",
    is_summary = NULL,
    weight = NULL,
    section = "region",
    section_spacer = FALSE,
    section_indent = FALSE
  )
  hdr_rows <- res$df[res$is_section_header, ]
  expect_true(all(is.na(hdr_rows$estimate)))
})

test_that("build_sections() section header label equals section value", {
  df <- data.frame(
    region = c("Asia", "Asia"),
    study = c("S1", "S2"),
    estimate = c(0.1, 0.2),
    lower = c(0.0, 0.1),
    upper = c(0.2, 0.3)
  )
  res <- build_sections(
    df = df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    label = "study",
    is_summary = NULL,
    weight = NULL,
    section = "region",
    section_spacer = FALSE,
    section_indent = FALSE
  )
  hdr_rows <- res$df[res$is_section_header, ]
  expect_equal(hdr_rows$study, "Asia")
})

test_that("build_sections() section_indent = TRUE prepends two spaces", {
  df <- data.frame(
    region = c("Asia", "Asia"),
    study = c("S1", "S2"),
    estimate = c(0.1, 0.2),
    lower = c(0.0, 0.1),
    upper = c(0.2, 0.3)
  )
  res <- build_sections(
    df = df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    label = "study",
    is_summary = NULL,
    weight = NULL,
    section = "region",
    section_spacer = FALSE,
    section_indent = TRUE
  )
  data_rows <- res$df[!res$is_section_header & !res$is_spacer, ]
  expect_true(all(startsWith(data_rows$study, "  ")))
})

test_that("build_sections() section_indent = FALSE leaves labels unchanged", {
  df <- data.frame(
    region = c("Asia", "Asia"),
    study = c("S1", "S2"),
    estimate = c(0.1, 0.2),
    lower = c(0.0, 0.1),
    upper = c(0.2, 0.3)
  )
  res <- build_sections(
    df = df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    label = "study",
    is_summary = NULL,
    weight = NULL,
    section = "region",
    section_spacer = FALSE,
    section_indent = FALSE
  )
  data_rows <- res$df[!res$is_section_header & !res$is_spacer, ]
  expect_equal(data_rows$study, c("S1", "S2"))
})

test_that("build_sections() section_spacer = TRUE adds blank rows", {
  df <- data.frame(
    region = c("Asia", "Asia", "Europe"),
    study = c("S1", "S2", "S3"),
    estimate = c(0.1, 0.2, 0.3),
    lower = c(0.0, 0.1, 0.2),
    upper = c(0.2, 0.3, 0.4)
  )
  res <- build_sections(
    df = df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    label = "study",
    is_summary = NULL,
    weight = NULL,
    section = "region",
    section_spacer = TRUE,
    section_indent = FALSE
  )
  expect_equal(sum(res$is_spacer), 2L)
  spacer_rows <- res$df[res$is_spacer, ]
  expect_true(all(spacer_rows$study == ""))
})

test_that("build_sections() section_spacer = FALSE adds no spacers", {
  df <- data.frame(
    region = c("Asia", "Europe"),
    study = c("S1", "S2"),
    estimate = c(0.1, 0.2),
    lower = c(0.0, 0.1),
    upper = c(0.2, 0.3)
  )
  res <- build_sections(
    df = df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    label = "study",
    is_summary = NULL,
    weight = NULL,
    section = "region",
    section_spacer = FALSE,
    section_indent = FALSE
  )
  expect_equal(sum(res$is_spacer), 0L)
})

test_that("build_sections() preserves row order within sections", {
  df <- data.frame(
    region = c("Asia", "Asia", "Asia"),
    study = c("C", "A", "B"),
    estimate = c(0.3, 0.1, 0.2),
    lower = c(0.2, 0.0, 0.1),
    upper = c(0.4, 0.2, 0.3)
  )
  res <- build_sections(
    df = df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    label = "study",
    is_summary = NULL,
    weight = NULL,
    section = "region",
    section_spacer = FALSE,
    section_indent = FALSE
  )
  data_rows <- res$df[!res$is_section_header & !res$is_spacer, ]
  expect_equal(data_rows$study, c("C", "A", "B"))
})

test_that("build_sections() cols columns are empty for header/spacer rows", {
  df <- data.frame(
    region = c("Asia", "Asia"),
    study = c("S1", "S2"),
    estimate = c(0.1, 0.2),
    lower = c(0.0, 0.1),
    upper = c(0.2, 0.3),
    n_text = c("100", "200")
  )
  res <- build_sections(
    df = df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    label = "study",
    is_summary = NULL,
    weight = NULL,
    section = "region",
    section_spacer = TRUE,
    section_indent = FALSE,
    cols = "n_text"
  )
  struct_rows <- res$df[res$is_section_header | res$is_spacer, ]
  expect_true(all(struct_rows$n_text == ""))
})

test_that("build_sections() section_cols populates header row text columns", {
  df <- data.frame(
    region = c("Asia", "Asia"),
    study = c("S1", "S2"),
    estimate = c(0.1, 0.2),
    lower = c(0.0, 0.1),
    upper = c(0.2, 0.3),
    n_text = c("100", "200"),
    k_text = c("k = 2", "k = 2")
  )
  res <- build_sections(
    df = df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    label = "study",
    is_summary = NULL,
    weight = NULL,
    section = "region",
    section_spacer = FALSE,
    section_indent = FALSE,
    cols = c("n_text", "k_text"),
    section_cols = c(k_text = "k_text")
  )
  hdr_rows <- res$df[res$is_section_header, ]
  expect_equal(hdr_rows$k_text, "k = 2")
  expect_equal(hdr_rows$n_text, "")
})

test_that("build_sections() subsection inserts sub-headers", {
  df <- data.frame(
    domain = c("A", "A", "A", "B", "B"),
    type = c("x", "x", "y", "x", "y"),
    study = paste0("S", 1:5),
    estimate = (1:5) * 0.1,
    lower = (1:5) * 0.1 - 0.05,
    upper = (1:5) * 0.1 + 0.05
  )
  res <- build_sections(
    df = df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    label = "study",
    is_summary = NULL,
    weight = NULL,
    section = "domain",
    subsection = "type",
    section_spacer = FALSE,
    section_indent = FALSE
  )
  # 2 section headers + 4 subsection headers + 5 data = 11
  expect_equal(sum(res$is_section_header), 2L)
  expect_equal(sum(res$is_subsection_header), 4L)
  sub_rows <- res$df[res$is_subsection_header, ]
  # Subsection headers are indented by 2 spaces
  expect_true(all(startsWith(sub_rows$study, "  ")))
})

test_that("build_sections() subsection data rows indented by 4 spaces", {
  df <- data.frame(
    domain = c("A", "A"),
    type = c("x", "x"),
    study = c("S1", "S2"),
    estimate = c(0.1, 0.2),
    lower = c(0.0, 0.1),
    upper = c(0.2, 0.3)
  )
  res <- build_sections(
    df = df,
    estimate = "estimate",
    lower = "lower",
    upper = "upper",
    label = "study",
    is_summary = NULL,
    weight = NULL,
    section = "domain",
    subsection = "type",
    section_spacer = FALSE,
    section_indent = TRUE
  )
  is_struct <- res$is_section_header | res$is_subsection_header | res$is_spacer
  data_rows <- res$df[!is_struct, ]
  expect_true(all(startsWith(data_rows$study, "    ")))
})

# ── Basic rendering ───────────────────────────────────────────────────────────

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

# ── Reference-category rows (NA estimates) ────────────────────────────────────

test_that("forrest() renders reference-category NA rows without error", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    label = c("Q1 (ref)", "Q2", "Q3", "Q4"),
    estimate = c(NA, 0.2, 0.4, 0.6),
    lower = c(NA, 0.1, 0.3, 0.5),
    upper = c(NA, 0.3, 0.5, 0.7)
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

test_that("forrest() ref_label = TRUE appends (Ref.) to NA-estimate rows", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    domain = c("D1", "D1", "D1"),
    label = c("Q1", "Q2", "Q3"),
    estimate = c(NA_real_, 0.2, 0.4),
    lower = c(NA_real_, 0.1, 0.3),
    upper = c(NA_real_, 0.3, 0.5)
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label",
      section = "domain",
      ref_label = TRUE
    )
  )
})

# ── section argument ──────────────────────────────────────────────────────────

test_that("forrest() section renders without error", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    region = c("Asia", "Asia", "Europe", "Europe"),
    study = c("S1", "S2", "S3", "S4"),
    estimate = c(0.1, 0.2, 0.3, 0.4),
    lower = c(0.0, 0.1, 0.2, 0.3),
    upper = c(0.2, 0.3, 0.4, 0.5)
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "study",
      section = "region"
    )
  )
})

test_that("forrest() section + cols renders without error", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    region = c("Asia", "Asia", "Europe"),
    study = c("S1", "S2", "S3"),
    estimate = c(0.1, 0.2, 0.3),
    lower = c(0.0, 0.1, 0.2),
    upper = c(0.2, 0.3, 0.4),
    n_text = c("100", "200", "150")
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "study",
      section = "region",
      cols = c("N" = "n_text")
    )
  )
})

test_that("forrest() section + section_cols renders without error", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    region = c("Asia", "Asia"),
    study = c("S1", "S2"),
    estimate = c(0.1, 0.2),
    lower = c(0.0, 0.1),
    upper = c(0.2, 0.3),
    n_text = c("100", "200"),
    k_text = c("k = 2", "k = 2")
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "study",
      section = "region",
      cols = c("N" = "n_text", "k" = "k_text"),
      section_cols = c("k" = "k_text")
    )
  )
})

test_that("forrest() section + stripe renders without error", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    region = c("Asia", "Asia", "Europe"),
    study = c("S1", "S2", "S3"),
    estimate = c(0.1, 0.2, 0.3),
    lower = c(0.0, 0.1, 0.2),
    upper = c(0.2, 0.3, 0.4)
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "study",
      section = "region",
      stripe = TRUE
    )
  )
})

test_that("forrest() section + is_summary renders without error", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    region = c("Asia", "Asia", "Asia", "Europe", "Europe", "Europe"),
    study = c("S1", "S2", "Asia (pooled)", "S3", "S4", "Europe (pooled)"),
    estimate = c(0.1, 0.2, 0.15, 0.3, 0.4, 0.35),
    lower = c(0.0, 0.1, 0.08, 0.2, 0.3, 0.25),
    upper = c(0.2, 0.3, 0.22, 0.4, 0.5, 0.45),
    is_sum = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "study",
      section = "region",
      is_summary = "is_sum"
    )
  )
})

# ── subsection argument ───────────────────────────────────────────────────────

test_that("forrest() subsection renders without error", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    domain = c("A", "A", "A", "B", "B"),
    type = c("x", "x", "y", "x", "y"),
    study = paste0("S", 1:5),
    estimate = (1:5) * 0.1,
    lower = (1:5) * 0.1 - 0.05,
    upper = (1:5) * 0.1 + 0.05
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "study",
      section = "domain",
      subsection = "type"
    )
  )
})

# ── Input type compatibility ──────────────────────────────────────────────────

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

# ── Error handling ────────────────────────────────────────────────────────────

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
      cols = c("or_text")
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
      widths = c(1, 2)
    ),
    "widths"
  )
})

test_that("forrest() errors when subsection provided without section", {
  dat <- basic_dat()
  expect_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      subsection = "grp"
    ),
    "requires `section`"
  )
})

test_that("forrest() errors when section column not in data", {
  dat <- basic_dat()
  expect_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      section = "nonexistent"
    ),
    "Column 'nonexistent' not found"
  )
})

test_that("forrest() errors when section_cols key not in cols names", {
  dat <- basic_dat()
  dat$region <- "A"
  expect_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      section = "region",
      cols = c("OR" = "or_text"),
      section_cols = c("N" = "or_text")
    ),
    "section_cols"
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
    lower = c(-0.80, -0.05),
    upper = c(0.05, 0.90)
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

# ── dodge ─────────────────────────────────────────────────────────────────────

dodge_dat <- function() {
  data.frame(
    label = rep(c("A", "B", "C"), each = 2),
    period = rep(c("early", "late"), 3),
    estimate = c(0.42, 0.30, -0.18, -0.10, 0.31, 0.22),
    lower = c(0.22, 0.12, -0.38, -0.28, 0.12, 0.04),
    upper = c(0.62, 0.48, 0.02, 0.08, 0.50, 0.40)
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
      lower = "lower",
      upper = "upper",
      label = "label",
      group = "period",
      dodge = TRUE
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
      lower = "lower",
      upper = "upper",
      label = "label",
      dodge = 0.3
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
      lower = "lower",
      upper = "upper",
      label = "label",
      group = "period",
      dodge = TRUE,
      header = "Exposure",
      cols = c("Coef" = "txt")
    )
  )
})

test_that("forrest() dodge works with cols_by_group = TRUE", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- dodge_dat()
  dat$est_ci <- sprintf("%.2f", dat$estimate)
  dat$text_early <- ifelse(dat$period == "early", dat$est_ci, "")
  dat$text_late <- ifelse(dat$period == "late", dat$est_ci, "")
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label",
      group = "period",
      dodge = TRUE,
      cols_by_group = TRUE,
      cols = c("Early" = "text_early", "Late" = "text_late")
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
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label",
      dodge = FALSE,
      cols_by_group = TRUE,
      cols = c("OR" = "txt")
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
      lower = "lower",
      upper = "upper",
      label = "label",
      dodge = FALSE
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
      lower = "lower",
      upper = "upper",
      label = "label",
      group = "period",
      dodge = TRUE,
      stripe = TRUE
    )
  )
})

test_that("forrest() section + dodge renders without error", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    region = c("Asia", "Asia", "Asia", "Asia", "Europe", "Europe"),
    study = c("S1", "S1", "S2", "S2", "S3", "S3"),
    period = c("early", "late", "early", "late", "early", "late"),
    estimate = c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35),
    lower = c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25),
    upper = c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45)
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "study",
      section = "region",
      group = "period",
      dodge = TRUE
    )
  )
})

# ── shape ─────────────────────────────────────────────────────────────────────

test_that("forrest() renders with shape column", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  dat$sex <- c("F", "M", "F", "M")
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label",
      shape = "sex"
    )
  )
})

test_that("forrest() renders with dodge + group + shape", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- data.frame(
    label = rep(c("A", "B"), each = 4),
    period = rep(rep(c("early", "late"), each = 2), 2),
    sex = rep(c("F", "M"), 4),
    estimate = c(0.42, 0.30, 0.22, 0.15, -0.18, -0.10, -0.25, -0.20),
    lower = c(0.22, 0.12, 0.05, 0.00, -0.38, -0.28, -0.45, -0.38),
    upper = c(0.62, 0.48, 0.39, 0.30, 0.02, 0.08, -0.05, -0.02)
  )
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label",
      group = "period",
      shape = "sex",
      dodge = TRUE
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
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      label = "label",
      shape = "sex",
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
      lower = "lower",
      upper = "upper",
      shape = "nonexistent"
    ),
    "Column 'nonexistent' not found"
  )
})

# ── save_forrest() ────────────────────────────────────────────────────────────

test_that("save_forrest() writes a PDF file", {
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp))
  dat <- basic_dat()
  result <- save_forrest(
    tmp,
    function() {
      forrest(
        dat,
        estimate = "estimate",
        lower = "lower",
        upper = "upper"
      )
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
        forrest(
          dat,
          estimate = "estimate",
          lower = "lower",
          upper = "upper"
        )
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
        forrest(
          dat,
          estimate = "estimate",
          lower = "lower",
          upper = "upper"
        )
      }
    ),
    "Supported extensions"
  )
})

# ── Theme support ─────────────────────────────────────────────────────────────

test_that("forrest() accepts built-in theme names", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  for (th in c("default", "minimal", "classic")) {
    expect_no_error(
      forrest(
        dat,
        estimate = "estimate",
        lower = "lower",
        upper = "upper",
        theme = th
      )
    )
  }
})

test_that("forrest() accepts theme as a named list", {
  pdf(nullfile())
  on.exit(dev.off())
  dat <- basic_dat()
  expect_no_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      theme = list(ref_col = "red", grid_lwd = 0.2)
    )
  )
})

test_that("forrest() errors on unknown theme name", {
  dat <- basic_dat()
  expect_error(
    forrest(
      dat,
      estimate = "estimate",
      lower = "lower",
      upper = "upper",
      theme = "nonexistent"
    ),
    "must be one of"
  )
})
