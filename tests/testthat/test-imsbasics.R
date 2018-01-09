context("test imsbasics")
test_that("ims_match", {
  y <- c("Test", "a", "b", "t", "T", "Te", "Test", "hurz")
  # browser()
  testthat::expect_equal(ims_match("T", y), 5)
  testthat::expect_equal(y[ims_match("T", y)], "T")

  testthat::expect_equal(ims_match("T", y, encoding_match_type="base_r"), 5)
  testthat::expect_equal(y[ims_match("T", y, encoding_match_type="base_r")], "T")

  testthat::expect_equal(ims_match("T", y, encoding_match_type="sqc"), 5)
  testthat::expect_equal(y[ims_match("T", y, encoding_match_type="sqc")], "T")

  testthat::expect_equal(all(grep("T", y) == c(1, 5, 6, 7)), T)
  testthat::expect_equal(all(grepl("T", y) == c(T, F, F, F, T, T, T, F)), T)
})

test_that("replace_by_lookuptable replaces values in df as expected", {
  source_df <- data.frame(a = 1:3, b = 5:7)
  lookup_table <- data.frame(old = 7:5, new = c("maus", "laus", "haus"),
                             stringsAsFactors = FALSE)

  dest_df <- replace_by_lookuptable(df = source_df, col = "b", lookup = lookup_table)

  testthat::expect_equal(dest_df[1,2], "haus")
  testthat::expect_equal(dest_df[2,2], "laus")
  testthat::expect_equal(dest_df[3,2], "maus")
})
