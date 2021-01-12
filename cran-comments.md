## Resubmission
This is a resubmission. The previous version had a WARNING because plotly was orphaned -- this was reported at https://github.com/ropensci/plotly/issues/1906 and has now been resolved.

## Test environments
* Local OS X install, R 4.0.3, `x86_64-apple-darwin17.0`.
* Windows via `devtools::check_win_devel`.

## R CMD check results
There were no ERRORs or WARNINGs.

styler and lintr also report no issues, covr reports 100% test coverage.

A few things that might pop in the validation check:

* This is my first submission, so you might see a note for that.
* There's some oddity around ggmap -- it's not orphaned, but was at some point, and that is only showing up on Windows builds? https://community.rstudio.com/t/orphaned-package-on-windows-build/84165/6 discusses a similar issue, and it seems to be a false positive.
* The description references GPS and GPX, which show up in a spell check as "misspelled words". The former is an obvious term of art in the context of this package, and the latter is defined on first use.

Finally, the GPX/TCX files I include as examples were recorded by me, so there's no license concerns there.

## Downstream dependencies
This is a new release, so there are no downstream dependencies.
