# Development guide

## Daily workflow

```r
devtools::load_all()   # reload package in the current R session
devtools::document()   # run roxygen2 → regenerate man/ and NAMESPACE
devtools::test()       # run testthat test suite (tests/)
devtools::check()      # full R CMD check (equivalent to CRAN check)
```

Run these in order when making non-trivial changes:
`load_all()` → edit code → `document()` (if roxygen changed) → `test()` → `check()`.

## README

The source is `README.Rmd`; `README.md` is generated — do not edit it directly.

```r
devtools::build_readme()   # knit README.Rmd → README.md (also rebuilds figures)
```

## Vignettes

Vignettes live in `vignettes/` and are written in Quarto (`.qmd`).
The vignette engine is `quarto::html` (declared in each file's YAML front-matter).

Render a single vignette during development:

```bash
quarto render vignettes/intro.qmd
quarto render vignettes/meta.qmd
quarto render vignettes/regression.qmd
```

Build all vignettes as part of the package (writes to `inst/doc/`):

```r
devtools::build_vignettes()
```

## Documentation website (altdoc)

The site is built with [altdoc](https://etiennebacher.github.io/altdoc/) using
a Quarto website layout. Config lives in `altdoc/quarto_website.yml`; the
rendered site goes to `docs/`.

```r
# One-time setup (already done — do not re-run unless resetting)
# altdoc::setup_docs(tool = "quarto")

# Rebuild the full site (vignettes + reference pages + index)
altdoc::render_docs()

# Preview the site locally in a browser
altdoc::preview_docs()
```

`altdoc::render_docs()` re-runs vignettes and rebuilds all reference `.qmd`
files from `man/`. Run it before pushing changes that affect the public docs.

## Spell check

```r
devtools::spell_check()
```

## Code coverage

```r
covr::package_coverage()
covr::report()   # open HTML coverage report
```

## Install from source

```r
devtools::install()                    # install current source into the active library
pak::pak("lorenzoFabbri/forrest")      # install latest GitHub version
```

---

## CRAN release workflow

Based on [R Packages (2e) — Chapter 22](https://r-pkgs.org/release.html).
The guiding principle: _"If it hurts, do it more often"_ — run `R CMD check`
throughout development, not only before submission.

### 1. Decide the release type and bump the version

```r
usethis::use_version("minor")   # or "patch" / "major"
```

Update `NEWS.md` to document changes for this version.

### 2. Pre-submission checks

```r
devtools::spell_check()          # flag spelling errors in docs
devtools::build_readme()         # rebuild README.md if README.Rmd changed
urlchecker::url_check()          # find broken or redirecting URLs
```

Run the local CRAN check:

```r
devtools::check(cran = TRUE)
```

Target: **0 errors, 0 warnings, 0 notes** (or document unavoidable notes in
`cran-comments.md`).

### 3. Check on Windows (win-builder) and R-devel

```r
devtools::check_win_devel()      # submit to CRAN's win-builder (~30 min for results)
```

Optionally check on multiple platforms via R-hub:

```r
rhub::check_for_cran()
```

### 4. Reverse dependency checks (if the package has dependents)

```r
revdepcheck::revdep_check(num_workers = 4)
```

Results land in `revdep/`. Fix or notify maintainers of any breakage.

### 5. Update `cran-comments.md`

Document the check results and any unavoidable NOTEs for the CRAN reviewer.
For resubmissions, add a "Resubmission" section describing what was changed.

### 6. Submit

```r
devtools::submit_cran()
```

This builds the bundle, posts it to the CRAN submission form, and writes a
`CRAN-SUBMISSION` file. **Confirm the submission via the email link CRAN sends.**

### 7. After acceptance

```r
usethis::use_github_release()    # tag the release on GitHub
usethis::use_dev_version(push = TRUE)   # bump to dev version (e.g. 0.3.0.9000)
```

Monitor CRAN check results across flavors for a few days after acceptance.

### CRAN check notes policy

| Result  | Action                                                                                |
| ------- | ------------------------------------------------------------------------------------- |
| Error   | Must fix before submission                                                            |
| Warning | Must fix before submission                                                            |
| NOTE    | Document in `cran-comments.md`; some are harmless (e.g. first submission, time check) |

### First submission — extra items

- `Authors@R` must include a copyright holder (`[cph]` role) ✓
- `Title` uses Title Case, ≤ 65 characters, no "for R" / "A Toolkit for" ✓
- `Description` does not start with "This package" or the package name ✓
- All exported functions have `@return` and `@examples` ✓
- No `\dontrun{}` unless example truly cannot be run
- `cran-comments.md` exists ✓
- `NEWS.md` exists ✓
