# File Testroutes.RData in /data. This path is searched automatically.
# File has not to be load to environment manually.
# Needs folder /imsbasics/data/

test_that("save_rdata can save variables in environment to folder", {
  # print("test-save_rdata")
  savename <- "save_rdata_Testroutes.RData"
  # savepath <- "H:/07 Rprogress/01 Packages/imsbasics/data/"
  savepath <- paste0(dirname(dirname(getwd())), "/data/")
  filename_withoutpath <- paste0(getwd(), "/", savename)
  filename_withpath <- paste0(savepath, savename)

  # Delete test files if necessary:
  if (file.exists(filename_withoutpath))
    file.remove(filename_withoutpath)
  if (file.exists(paste0(savepath, savename)))
    file.remove(paste0(savepath, savename))

  # Throw error when less than two args are given:
  expect_equal(file.exists(filename_withoutpath), FALSE)
  expect_error(save_rdata(Testroutes), "in save_radata: data and name argument required")
  expect_error(save_rdata(savename), "in save_radata: data and name argument required")

  # Save variable without path:
  expect_warning(save_rdata(Testroutes, savename),
    paste0("in save_rdata: path is set to ", getwd(), "/", ", force = F"))

  # save variable with using the given path:
  expect_equal(file.exists(paste0(savepath, savename)), FALSE)
  expect_equal(save_rdata(Testroutes, savename, savepath), savepath)

  # Check warning, when file already exists:
  expect_warning(save_rdata(Testroutes, savename, savepath), )

  # Check overwriting:
  expect_warning(save_rdata(Testroutes, savename, savepath, force = T),
    "in save_rdata: old file overwritten")
  expect_error(save_rdata(Testroutes, savename, force = T), )
  # save_rdata(Testroutes, savename, T) throws error which cannot be identified

  # Delete all created files:
  file.remove(filename_withoutpath)
  file.remove(paste0(savepath, savename))
})

