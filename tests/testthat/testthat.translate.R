
library("unitizer")

res <- testthat_to_unitizer("helper/translate/testthat/test-one.R")

expect_equal(
  res,
  c("# for translate unitizer tests", "# context(\"testthat to unitizer\")", "# random non-sectioned", "# blah blah", "rev(10:1)", "unitizer_sect(\"simple tests\", {\n    # first internal\n    fun0(a)\n    # internal comment\n    fun1(a, b, c, d, e, f)\n    # \"external\" comment\n    fun1(a)\n})", "# a test for errors", "unitizer_sect(\"errors\", {\n    # Making up sections\n    stop(\"hello\")\n    warning(\"yoyo\")\n})")
)
expect_error(  # Can't do this twice in a row since files now exist
  testthat_to_unitizer("helper/translate/testthat/test-one.R"),
  "Unable to proceed"
)
expect_equal(
  readRDS("helper/translate/unitizer/one.unitizer/data.rds")@items.ref.calls.deparse,
  c("rev(10:1)", "fun0(a)", "fun1(a, b, c, d, e, f)", "fun1(a)", "stop(\"hello\")", "warning(\"yoyo\")")
)
unlink("helper/translate/unitizer", recursive=TRUE)
