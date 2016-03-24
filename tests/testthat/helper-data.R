# Helper file. Set global test parameters available in all testthat sections


# Load file in data and check some values:
path <- "../testthat/"
savepath <- "../../data/"

file <- "Z-testdata_routes"
fileRData <- paste0(file, ".RData")

savename <- "save_rdata_Testroutes.RData"
filename_withoutpath <- paste0("../testthat/", savename)
filename_withpath <- paste0(savepath, savename)
testroutes <- load_rdata(fileRData, path)

