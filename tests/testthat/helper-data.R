# Helper file. Set global test parameters available in all testthat sections


# Load file in data and check some values:
path <- ""
data <- load_rdata("Z-testdata_hs_SG2014", path)
short_uuid <- substr(data$uuid, 1, 8)

filename <- "Z-testdata_routes"
fileRData <- paste0(filename, ".RData")

savename <- "ZZ-routes.RData"
filename_withoutpath <- paste0("../testthat/", savename)
filename_withpath <- paste0(path, savename)
testroutes <- load_rdata(fileRData, path)

