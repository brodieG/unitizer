
context('random')

test_that('random seed', {
  dir <- tempfile()
  file <- file.path(dir, 'randtest.R')
  on.exit(unlink(dir, recursive=TRUE))

  dir.create(dir)

  cat('sample(1:100)\n', file=file)

  set.seed(1)
  capture.output(unitize(file, auto.accept='new'))

  # changing seed should have no effect on result
  set.seed(23)
  capture.output(res <- unitize(file))

  expect_equal(as.character(res$status), "Passed")
})
