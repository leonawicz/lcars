context("plot")

test_that("lcars_border returns as expected", {
  file <- file.path(tempdir(), "test-plot.png")
  if(require(png)){
    png(file)
    expect_is(lcars_border(), "NULL")
    unlink(file, recursive = TRUE, force = TRUE)
  }
})
