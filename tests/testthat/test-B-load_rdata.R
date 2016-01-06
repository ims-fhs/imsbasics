# File Testroutes.RData in /data. This path is searched automatically.
# File has not to be load to environment manually.
# Needs folder /imsbasics/data/

test_that("load_rdata() can load \"Testroutes.Rdata\" w/o path argument", {
  # print("test-load_rdata")

  # Load file in data and check some values:
  myroutes <- load_rdata("Testroutes.RData", "H:/07 Rprogress/01 Packages/imsbasics/data/")
  expect_equal(nrow(myroutes), 25244)
  expect_equal(myroutes$time[2], 1343)
  # Use round to get rid of displayed precision:
  expect_equal(round(myroutes$lat1[2], 5), 47.27197)
  expect_equal(round(myroutes$lng1[2], 5), 7.68593)
  expect_equal(round(myroutes$lat2[2], 5), 47.16292)
  expect_equal(round(myroutes$lng2[2], 5), 7.84218)

  # Check whether .RData is completed correctly:
  myroutes2 <- load_rdata("Testroutes", "H:/07 Rprogress/01 Packages/imsbasics/data/")
  expect_equal(nrow(myroutes2), 25244)

  # Check error message, when file does not exist:
  expect_error(load_rdata("Wrongfile.RData", "H:/07 Rprogress/01 Packages/imsbasics/data/"), )
  # Leave error message blank as statement depends on language.

  # Check, warning, when default path is used:
  # Needs Testcache.RData in folder /testthat/data/.
  myroutes3 <- load_rdata("Testroutes") # Also creates a warning...
  expect_equal(nrow(myroutes3), 25244)
  expect_equal(myroutes3$time[2], 1343)
  expect_warning(load_rdata("Testroutes"), )
})

