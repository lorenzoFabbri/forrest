# Development guide

Design decisions and the rule against new Imports are in the project memory (`project_forrest.md`); this file covers the commands.

## Daily workflow

```r
devtools::load_all()   # reload the package in the current R session
devtools::document()   # run roxygen2: regenerate man/ and NAMESPACE
devtools::test()       # run the testthat suite in tests/
devtools::check()      # full R CMD check
lintr::lint_package()  # lint with the settings in .lintr
```

For a non-trivial change, run them in that order: `load_all()`, edit, `document()` if roxygen changed, `test()`, `check()`. `lintr`'s object-usage check reads the installed copy of the package, so run `devtools::install()` first when a function's signature has changed, or it reports false "unused argument" warnings.

## README

`README.md` is generated from `README.Rmd`; edit only the `.Rmd`. `devtools::build_readme()` knits it and rebuilds the figures in `man/figures/`.

## Vignettes

The vignettes in `vignettes/` are Quarto (`.qmd`) files using the `quarto::html` engine declared in each file's front matter. To render one while editing:

```bash
quarto render vignettes/intro.qmd
```

`devtools::build_vignettes()` builds all of them into `doc/` and `Meta/` at the package root. Both directories are build output, ignored by git and by `R CMD build`.

## Documentation website

The site is built with [altdoc](https://etiennebacher.github.io/altdoc/) as a Quarto website, configured in `altdoc/quarto_website.yml` and rendered to `docs/`. The `altdoc` GitHub workflow rebuilds and deploys it on every push to `main`.

```r
altdoc::render_docs()    # re-run the vignettes and rebuild the reference pages from man/
altdoc::preview_docs()   # open the rendered site in a browser
```

## Other checks

```r
devtools::spell_check()
urlchecker::url_check()
covr::package_coverage()   # covr::report() opens the HTML report
```

## CRAN release

The steps follow [R Packages (2e), chapter 22](https://r-pkgs.org/release.html).

1. Choose the release type and bump the version with `usethis::use_version("minor")` (or `"patch"`, `"major"`). Turn the development heading in `NEWS.md` into the version heading.
2. Run `devtools::spell_check()`, `devtools::build_readme()` and `urlchecker::url_check()`.
3. Run `devtools::check(cran = TRUE)`. The target is 0 errors, 0 warnings and 0 notes; any note that cannot be removed is explained in `cran-comments.md`.
4. Run `devtools::check_win_devel()`; win-builder emails the result to the maintainer in about 30 minutes. For other platforms, R-hub v2 runs checks as GitHub Actions: `rhub::rhub_setup()` once to add its workflow, then `rhub::rhub_check()`.
5. Reverse dependencies, once the package has any: `revdepcheck::revdep_check(num_workers = 4)`, with results in `revdep/`.
6. Update `cran-comments.md` with the check results and, for a resubmission, what changed.
7. Submit with `devtools::submit_cran()`, which writes `CRAN-SUBMISSION`, then confirm through the link in CRAN's email. Submitting sends the maintainer's email address to CRAN, so ask Lorenzo before running it.
8. After acceptance, run `usethis::use_github_release()` to tag the release and `usethis::use_dev_version(push = TRUE)` to move to the next development version. Check the CRAN results page for the package over the following days.

Errors and warnings must be fixed before submitting. A note is either fixed or explained in `cran-comments.md`; "New submission" and a failure to verify the current time are routine.
