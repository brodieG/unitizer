local({
  mx.1 <- matrix(1:9, nrow=3)
  mx.2 <- matrix(1:100, ncol=5)

  test_that("obj_out", {
    expect_identical(
      c("@@ .new @@", "+      [,1] [,2] [,3]", "+ [1,]    1    4    7", "+ [2,]    2    5    8", "+ [3,]    3    6    9"),
      unitizer:::obj_out(mx.1, ".new", 80L, c(5L, 3L))
    )
    expect_identical(
      c("@@ .new @@", "+      [,1] [,2]", "+ [1,]    1    4", "+ [2,]    2    5", "+ ... truncated 5 lines, use `.new` to see full object."),
      unitizer:::obj_out(mx.1, ".new", 17L, c(5L, 3L))
    )
    expect_identical(
      c("@@ .ref @@", "-      [,1] [,2] [,3]", "- [1,]    1    4    7", "- [2,]    2    5    8", "- [3,]    3    6    9"),
      unitizer:::obj_out(mx.1, ".ref", 80L, c(5L, 3L))
    )
  } )
})
