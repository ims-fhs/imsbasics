test_that("decimalplaces...", {
  # skip("Because three warnings are displayed: Precision worse than about 10 km")
  print("Test deciamlplaces")

  options(warn = -1) # turn warnings off
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
  options(warn = 0) # turn warnings on

  expect_error(decimalplaces("test"), )
})

