# Combined code to merge large raster objects, segment them and classify segments based on pseudo-NDVI
# Author: Patrick Sogno
# E-mail address: patrick.sogno@stud-mail.uni-wuerzburg.de
# Latest version on github: https://github.com/PatrickSog/Patrick/blob/Landklif/completeRLSegClass.R
# Written on Windows (8.1 & 10) machines.
# RStudio version: 1.1.456
# R version: 3.5.1

#----------------------------------------------#

# Please note that the merging is done with gdal, and the segmentation is done with otb, both need to be installed prior to running this code.
# Both can be found via this link: https://live.osgeo.org/en/overview/overview.html

#----------------------------------------------#
# Background: As part of the Landklif project     ---> (https://www.biozentrum.uni-wuerzburg.de/en/zoo3/forschung/verbundprojekte/landklif/)
# a number of airborne images had to be merged.
# Segmenting and classifying the imagery was
# just for my personal fun and for trying out
# neat little tricks like system calls from R
# and parallel processing.
# Hopefully you get some use out of the code,
# if you have any suggestions or found an error
# just contact me via e-mail! :)
#----------------------------------------------#



## packages
install.packages(c('naturalsort', 'gdalUtils', 'rgdal', 'raster', 'RStoolbox', 'MASS'))
library('naturalsort')
library('gdalUtils')
library('rgdal')
library('raster')
library('RStoolbox')
library('foreach')
library('doParallel')
#library('parallel') Package only needed for mclapply, which is not too useful on windows.
#library('MASS') Similar as above

## check out processing power to use parallel processing

numCores <- detectCores()
numCores

## paths

# For Step 1:
AirborneDir <- "C:/DeleteMe/Landklif1" # path to imagery

QuadrantDir <- "C:/DeleteMe/Quadrants" # path to quadrants

oDir <- "C:/DeleteMe" # output directory

# path to gdal
  #gdal_setInstallation() # set gdal installation options (not always needed)
  gdalPath <- getOption("gdalUtils_gdalPath")[[1]]$path # find gdal installation  # Does not always work, if complications hard code gdalPath as string
  gdalPath # optional control
  # hard coding gdalPath (needed if preferred gdal is on another drive)
  gdalPath <- "C:/OSGeo4W64/bin/" # hard code gdalPath with "/" in the end! (comment this command if getOption(...) works for you)

# For Step 2:
otbPath <- "C:/OTB-6.6.0-Win64/OTB-6.6.0-Win64/bin" # path to Orfeo Toolbox
otbSeg <- list.files(otbPath, pattern = "otbcli_Segmentation") # otb segmentation algorithm
segfileNr <- c(1:2) # Select fileNr for segmentation


## crs
mycrsEPSG_GK <- "EPSG:31468"
mycrsEPSG_UTM <- "EPSG:25832"

#################################

## read list of rasters
dirlist <- list.dirs(AirborneDir)
Pdirlist <- grep(paste0("/P"), dirlist, value = T)
# Optonal control
# Pdirlist
Pdir <- Pdirlist[naturalorder(Pdirlist)]
# Optinal control
#Pdir # natural order of P-directories

## read files in each directory into a list
allfilesCK <- lapply(Pdirlist, function(x) list.files(x, pattern="jpg$", full.names = T))
allfilesCK <- allfilesCK[naturalorder(allfilesCK)]
# Optional control
#allfilesCK # natural ordered list of files in directories


#################################

## read shapefiles of quadrants into spdf
final60 <- readOGR(paste0(QuadrantDir, "/Final60Quadrants.shp"))

#################################

## Mosaic rasters
# Credit to Ruben Remelgado for the gdal-based mosaic()-function

ipath <- paste0(Pdir, "/")          # path should end with '/'
out.res <- as.character(c(0.2,0.2)) # set output resolution
ofile <- list()
for (i in 1:length(ipath)) {        # full path of output
  ofile[i] <- paste0(oDir, "/P", i, "_merge_", final60$QUADRANT[i], ".jpg")
}

## Add Raster names to spdf
final60$RasterName <- NA
final60$RasterName[c(1:length(ofile))] <- unlist(ofile)
final60$RasterName # optional control

# optional control
ipath
ofile
out.res

mosaicPS <- function(ipath, ofile, out.res) {
  
  #-------------------------------------------------------------------------------------------------------#
  # make virtual raster (pre-step)
  #-------------------------------------------------------------------------------------------------------#
  
  vrt <-  file.path(ipath, 'mosaic.vrt') # temporary VRT file
  #vrt
  rasterFun <- list.files(gdalPath)[grep('vrt', list.files(gdalPath))] # find vrt function
  gdalCall <- paste0(gdalPath, rasterFun, ' ', vrt, ' ', file.path(ipath, '*.jpg'))
  # optional control:
  #gdalCall
  lapply(gdalCall, function(x) system(x)) # mclapply might be an idea here for non-windows machines...
  
  #-------------------------------------------------------------------------------------------------------#
  # make mosaic
  #-------------------------------------------------------------------------------------------------------#
  
  rasterFun <- list.files(gdalPath)[grep('translate', list.files(gdalPath))] # find "mosaic" function
  basePar <- paste0('-ot Byte -of JPEG -a_srs ', mycrsEPSG_UTM, ' -tr ', out.res[1], ' ', out.res[2])
  gdalCall <- paste0(gdalPath, rasterFun, ' ', basePar, ' ', vrt, ' ', ofile)
  #gdalCall
  lapply(gdalCall, function(x) system(x)) # again, mclapply might be an idea here for non-windows machines...
  
  rm(vrt) # clean up
}
#gdalPath # optional control
mosaicPS(ipath = ipath, ofile = ofile, out.res = out.res)

writeOGR(final60, dsn = paste0(oDir, "/Final60QuadrantsRN.shp"), layer = "Final60QuadrantsRN", driver = 'ESRI Shapefile', overwrite_layer = T)

##########################################################################
# End of data pre-processing
##########################################################################

         
         
## Step 2: Classification

## 1. Segmentation
# -> Let's use the Orfeo Toolbox for segmenting

# set input
ifile <- list()
for (i in 1:length(segfileNr)) {
  ifile[i] <- final60$RasterName[segfileNr[i]]
}
ifile # optional control

ifileR <- list()
for (i in 1:length(ifile)) {
  ifileR[[i]] <- stack(ifile[[i]])
}
ifileR # optonal control


#################################

## Optional:
# set segmentation boundaries and mask raster to save time

#ifileR # optional control
lapply(ifileR, function(x) extent(x)) # show extend
e <- extent(4489000, 4489500, 5575000, 5575500) # set extend of AOI
ifileN <- paste0(oDir, "/", "ifilecrop.grd") # set filename
ifilecrop <- crop(ifileR[[2]], e, filename = ifileN, overwrite = T) # crop, in this case only entry 2 of the list
#plot(ifilecrop) # optional control

#################################


## Prepare command:
# make list of files to be segmented
segfile <- list()
for (i in 1:length(ifile)) {
  segfile[i] <- gsub("merge", "seg", unlist(ifile[i]))
  segfile[i] <- gsub("jpg", "sqlite", unlist(segfile[i]))
}
segfile # optinal control
segfile <- segfile[[2]]

# set mode
mode <- "vector" #otb cannot handle large input files in raster mode

# set segmentation function
segfun <- "meanshift"

# formulate call
otbCall <- paste0(otbPath, '/', otbSeg, 
                  ' -in ', ifileN, 
                  ' -mode ', mode, ' -mode.', mode, '.out ', 
                  segfile, ' uint16', 
                  ' -filter ', segfun)

#otbCall # optional control
lapply(otbCall, function(x) system(x))

ifileSeg <- readOGR(unlist(segfile))
plot(ifileSeg) # optional control <- Nice to look at ;)

## End of segmentation ##


## 2. pNDVI calculation

PseudoNDVI <- function(G, R) {
  (G - R)/(G + R)
}
pNDVI <- PseudoNDVI(ifilecrop[[2]], ifilecrop[[1]])
plot(pNDVI)


## 3. Classification

ifileSeg$C_ID <- NA

registerDoParallel(numCores-1) # parallel processing with all but 1 cores available...
r <- foreach (i=1:length(ifileSeg)) %dopar%{
  tempmask <- raster::mask(pNDVI, ifileSeg[i,])
  tempC_ID <- names(which.max(table(raster::values(tempmask))))
  if (tempC_ID <= 0) {
    tempC_ID = 3
  } else {
    if (tempC_ID <= 0.2) {
      tempC_ID = 2
    } else {
      tempC_ID =1
    }
  }
}
stopImplicitCluster() # don't forget to clean up the cores after processing.

ifileSeg$C_ID <- unlist(r)
head(ifileSeg) # optional control


## 4. Output

writeOGR(ifileSeg, gsub("sqlite", "shp", unlist(segfile)), "Classified Segments", driver = "ESRI Shapefile", overwrite_layer = T)


plot(ifileSeg[which(ifileSeg$C_ID == 3),], col = "greenyellow")
plot(ifileSeg[which(ifileSeg$C_ID == 2),], col = "yellow3", add = T)
plot(ifileSeg[which(ifileSeg$C_ID == 1),], col = "green4", add = T)

##########################################################################
# End of script
##########################################################################
