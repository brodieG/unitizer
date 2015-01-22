local({
  mx.1 <- matrix(1:9, nrow=3)
  mx.2 <- matrix(1:100, ncol=5)

  test_that("obj_out", {
    expect_identical(
      c("+      [,1] [,2] [,3]", "+ [1,]    1    4    7", "+ [2,]    2    5    8", "+ [3,]    3    6    9"),
      unitizer:::obj_out(mx.1, TRUE, 80L, c(5L, 3L), extra=".new")
    )
    expect_identical(
      c("+      [,1] [,2]", "+ [1,]    1    4", "+ [2,]    2    5", "+ ... truncated 5 lines, use `.new` to see full object."),
      unitizer:::obj_out(mx.1, TRUE, 17L, c(5L, 3L), extra=".new")
    )
    expect_identical(
      c("-         [,1] [,2] [,3]", "-    [1,]    1    4    7", "-    [2,]    2    5    8", "-    [3,]    3    6    9"),
      unitizer:::obj_out(mx.1, FALSE, 80L, c(5L, 3L), extra=".ref")
    )
  } )
  test_that("cap_first", {
    set.seed(1)
    words <- replicate(2, paste0(sample(letters, 5), collapse=""))
    WORDS <- toupper(words)
    expect_identical(
      c("", "A", "B", "Y", "Z", "Gjnue", "Xzpob", "GJNUE", "XZPOB"),
      unitizer:::cap_first(c("", letters[1:2], letters[25:26], words, WORDS))
    )
    expect_identical(
      c("", "a", "b", "y", "z", "gjnue", "xzpob", "gJNUE", "xZPOB"),
      unitizer:::decap_first(c("", letters[1:2], letters[25:26], words, WORDS))
    )
  })
  test_that("word_wrap", {
    lorem1 <- "Today, with Kiernan on the stand offering confirmation, Howard walked the jury through the enormous amount of data pulled from Ulbricht's computer. Defense lawyers haven't had a chance yet to respond to this evidence—that will likely come tomorrow. The mountain they have to climb looks higher than ever, though. Last week, Ulbricht's lawyer outlined a defense in which Ulbricht walked away from the marketplace he created and was \"lured back.\" But what will explain the dozens of folders of data on this laptop, with data from the upper echelons of Silk Road management—mixed with the most intimate details of Ulbricht's personal life?"
    lorem2 <- "/Volumes/FIXED/folder1/folder2/folder.2345/folderabac/file.text.batch"

    expect_equal(
      c(18L, 25L), range(nchar(head(unitizer:::word_wrap(lorem1, 25L), -1L)))
    )
    expect_equal(
      c(23L, 25L), range(nchar(head(unitizer:::word_wrap(lorem1, 25L, 3L), -1L)))
    )
    expect_identical(
      c("Today, with Kiernan on the stand offering co-", "nfirmation, Howard walked the jury through ", "the enormous amount of data pulled from Ulb-", "richt's computer."),
      unitizer:::word_wrap(substr(lorem1, 1, 147), 45L, 3L)
    )
    expect_identical(
      c("Today, with Kiernan on the stand offering con", "firmation, Howard walked the jury through the", "enormous amount of data pulled from Ulbricht'", "s computer."),
      unitizer:::word_wrap(substr(lorem1, 1, 147), 45L, 3L, FALSE)
    )
    expect_identical(
      c("/Volumes/FIXED/", "folder1/folder2", "/folder.2345/", "folderabac/file", ".text.batch"),
      unitizer:::word_wrap(lorem2, 15L, 3L)
    )
    expect_identical(
      c("/Volumes/FIXED/", "folder1/", "folder2/folder.", "2345/folderabac", "/file.text.", "batch"),
      unitizer:::word_wrap(lorem2, 15L, 8L)
    )
  })

})
