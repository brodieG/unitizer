## Submission Notes:

Dear CRAN Maintainers,

I recently resubmitted unitizer at Professor Ripley's request to
remove the not-cran.Rout.save file that caused unnecessary  output in
the test results.

Warnings of the following form remain **only** on MacOS release and
oldrel, but I believe them to be false positives:

    'library' or 'require' calls not declared from:
     ‘unitizerdummypkg1’ ‘unitizerdummypkg2’

These are fake packages used by unitizer's own integration tests.
R CMD check normally white-lists such packages to avoid this type of
false positive, but the checks must be run on R-devel, or with
--as-cran, or the tests must be in subfolders like those used by
`testthat`.  The warnings are occurring because I dropped `testthat`
in favor of the .Rout/.Rout.save test method, and most likely the
macos release/oldrel test runs are without the --as-cran flag (I can
reproduce the warning locally on MacOS by omitting --as-cran).

I apologize for any inconvenience this might be causing.

Brodie.
