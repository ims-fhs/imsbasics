context("decimalplaces")
test_that("decimalplaces...", {
  # skip("Only for test purpose")
  # Three warnings are displayed: Precision worse than about 10 km...
  old <- getOption("warn")
  options(warn = -1) # suppress warnings
  expect_equal(decimalplaces(12), 0)
  expect_equal(decimalplaces(1e5), 0)
  expect_equal(decimalplaces(1.4), 1)
  expect_equal(decimalplaces(12.234), 3)

  expect_equal(decimalplaces(-12), 0)
  expect_equal(decimalplaces(-1e5), 0)
  expect_equal(decimalplaces(-1.4), 1)
  expect_equal(decimalplaces(-12.234), 3)

  expect_equal(is.integer(decimalplaces(12)), T)
  expect_equal(is.integer(decimalplaces(1.345)), T)
  expect_equal(is.integer(decimalplaces(-12)), T)
  expect_equal(is.integer(decimalplaces(-1.345)), T)
  options(warn = old) # Set old settings again

  expect_error(decimalplaces("test"), )
})


context("load_rdata")
test_that("load_rdata works", {
  # skip("Only for test purpose")

  # Load file in data and check some values:
  myroutes <- load_rdata(fileRData, path)
  expect_equal(nrow(myroutes), 25244)
  expect_equal(myroutes$time[2], 1343)
  # Use round to get rid of displayed precision:
  expect_equal(round(myroutes$lat1[2], 5), 47.27197)
  expect_equal(round(myroutes$lng1[2], 5), 7.68593)
  expect_equal(round(myroutes$lat2[2], 5), 47.16292)
  expect_equal(round(myroutes$lng2[2], 5), 7.84218)

  # Check whether .RData is completed correctly:
  myroutes2 <- load_rdata(file, path)
  expect_equal(nrow(myroutes2), 25244)

  # Check error message, when file does not exist:
  expect_error(load_rdata("Wrongfile.RData", path), )
  # Leave error message blank as statement depends on language.

  # Check what happens, if file or path is missing
  expect_error(load_rdata(file), )
  expect_error(load_rdata(path), )
})


context("save_rdata")
test_that("save_rdata", {
  # skip("Only for test purpose")

  # Delete test files if necessary:
  if (file.exists(filename_withoutpath))
    file.remove(filename_withoutpath)
  if (file.exists(filename_withpath))
    file.remove(filename_withpath)
  expect_equal(file.exists(filename_withoutpath), F)
  expect_equal(file.exists(filename_withpath), F)

  # Throw error when less than two args are given:
  expect_error(save_rdata(testroutes), )
  expect_error(save_rdata(savename), )

  # save variable with using the given path:
  save_rdata(testroutes, savename, path) # Warning cannot be switched off
  expect_equal(file.exists(paste0(path, savename)), T)

  # Check warning, when file already exists:
  expect_warning(save_rdata(testroutes, savename, path), )

  # Check overwriting:
  expect_warning(save_rdata(testroutes, savename, path, force = T), )

  # Check errors if parameters are missing
  expect_error(save_rdata(testroutes, savename, force = T), ) # Warning cannot be switched off

  # Delete test files if necessary:
  if (file.exists(filename_withoutpath))
    file.remove(filename_withoutpath)
  if (file.exists(filename_withpath))
    file.remove(filename_withpath)
})


context("archive data")
test_that("archive data works", {
  # skip("Skip for development of tests")

  t0 <- lubridate::now()
  prefix <- paste(lubridate::year(t0), lubridate::month(t0), lubridate::day(t0), sep = "-")
  new_file <- paste0("../../data/", prefix, "-Z-testdata_hs_SG2014_", short_uuid, ".RData")
  new_file2 <- paste0("../../data/", prefix, "-helper-data_000.R")
  new_file3 <- paste0("../../data/", prefix, "-test-AA-basicfunctions_000.R")
  new_file4 <- paste0("../../data/", prefix, "-Z-testdata_routes_000.RData")

  expect_equal(file.exists(new_file), F)
  # browser()
  archive_data(path, "Z-testdata_hs_SG2014.RData", "../../data/")
  expect_equal(file.exists(new_file), T)
  file.remove(new_file)

  expect_equal(file.exists(new_file), F)
  archive_data(path, "all", "../../data/")
  expect_equal(all(file.exists(new_file, new_file2, new_file3, new_file4)), T)
  file.remove(new_file, new_file2, new_file3, new_file4)
  expect_equal(all(file.exists(new_file, new_file2, new_file3, new_file4)), F)
})


context("zero_n")
test_that("zero_n works", {
  # skip("Skip for development of tests")
#   browser()
#   zero_n(c(1:10), position = 3)
#   zero_n(c(1:10), position = 2)
  expect_equal(zero_n(c(1:10), position = 3),
    c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010"))
  expect_equal(zero_n(c(1:10), position = 2),
    c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"))
})

context("rectangle")
test_that("rectangle works", {
  # skip("Skip for development of tests")
  t <- seq(0,10,1)
  t0 <- 3
  t1 <- 6
  expect_equal(rectangle(t, t0, t1), c(0, 0, 0, 0.5, 1, 1, 0.5, 0, 0, 0, 0))
  expect_equal(rectangle(t, t0, t1, 0), c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0))
  expect_equal(rectangle(t, t0, t1, 1), c(0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0))
  expect_error(rectangle(t, t0, t1, 2), "at_step = 0, 0.5 or 1")
})

context("language and dictionary")
test_that("language and dictionary", {
  # skip("Skip for development of tests")

  expect_equal(g2e("So"), "Sun")
  expect_equal(g2e(c("So", "Di")), c("Sun", "Tues"))
  expect_equal(var <- g2e(c("So, Mo, Mi", "Di, Mi, Do, Fr")),
               c("Sun, Mon, Wed", "Tues, Wed, Thurs, Fri"))
  expect_equal(e2g(g2e("So")), "So")
  expect_equal(e2g(g2e(c("So", "Di"))), c("So", "Di"))
  expect_equal(e2g(var), c("So, Mo, Mi", "Di, Mi, Do, Fr"))
})

