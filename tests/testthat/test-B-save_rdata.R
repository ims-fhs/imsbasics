# File Testroutes.RData in /data. This path is searched automatically.
# File has not to be load to environment manually.


test_that("save_rdata can save variables in environment to folder", {
  # print("test-save_rdata")
  savename <- "save_rdata_Testroutes.RData"
  savepath <- "H:/07 Rprogress/01 Packages/imsbasics/data/"
  filename_withoutpath <- paste0(getwd(), "/", savename)
  filename_withpath <- paste0(savepath, savename)

  # Delete test files if necessary:
  if (file.exists(filename_withoutpath))
    file.remove(filename_withoutpath)
  if (file.exists(paste0(savepath, savename)))
    file.remove(paste0(savepath, savename))

  # Save variable without path:
  expect_equal(file.exists(filename_withoutpath), FALSE)
  expect_equal(save_rdata(Testroutes, savename), paste0(getwd(),"/"))

  # save variable with using the given path:
  expect_equal(file.exists(paste0(savepath, savename)), FALSE)
  expect_equal(save_rdata(Testroutes, savename, savepath), savepath)

  # Check warning, when file already exists:
  expect_warning(save_rdata(Testroutes, savename, savepath), )
  expect_equal(save_rdata(Testroutes, savename, savepath), NULL)

  # Delete all created files:
  file.remove(filename_withoutpath)
  file.remove(paste0(savepath, savename))
})

