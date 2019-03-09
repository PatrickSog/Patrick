# set otb path
otbPath <- "D:/Programs/OTB-6.6.0-Win64/OTB-6.6.0-Win64/bin"
otbSeg <- list.files(otbPath, pattern = "otbcli_Segmentation")
# set ifile
ioPath <- "D:/Patrick/Documents/Dokumente/Landklif/Landklif1/Landklif1/P1/"
filedet <- "1_5.tif"
ifile <- paste0(ioPath, filedet)
# set mode
mode <- "vector" #otb cannot handle large input files in raster mode
# set ofile
ofile <- paste0(ioPath, "Seg", ".sqlite")
# set segmentation function
segfun <- "meanshift"
otbCall <- paste0(otbPath, '/', otbSeg, 
                  ' -in ', ifile, 
                  ' -mode ', mode, ' -mode.', mode, '.out ', 
                  ofile, ' uint16', 
                  ' -filter ', segfun)
otbCall # optional control

system(otbCall)

# check result
library(rgdal)
myseg <- readOGR(ofile)
plot(myseg)