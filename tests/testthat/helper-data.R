# Helper file. Set global test parameters available in all testthat sections


# Load file in data and check some values:
path <- ""

file <- "Z-testdata_routes"
fileRData <- paste0(file, ".RData")

savename <- "ZZ-routes.RData"
filename_withoutpath <- paste0("../testthat/", savename)
filename_withpath <- paste0(path, savename)
testroutes <- load_rdata(fileRData, path)

