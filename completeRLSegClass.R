# Combined code of RasterList.R & otbSegmentation.R

## packages
install.packages(c('naturalsort', 'gdalUtils', 'rgdal', 'raster'))
library('naturalsort')
library('gdalUtils')
library('rgdal')
library('raster')

## paths

# For Step 1:
AirborneDir <- "C:/DeleteMe/Landklif1" # path to imagery

QuadrantDir <- "C:/DeleteMe/Quadrants" # path to quadrants

oDir <- "C:/DeleteMe" # output directory

# path to gdal
  #gdal_setInstallation() # set gdal installation options (not always needed)
  gdalPath <- getOption("gdalUtils_gdalPath")[[1]]$path # find gdal installation  # Does not always work, if complications hard code gdalPath as string
  # hard coding gdalPath (needed if preferred gdal is on another drive)
  #gdalPath <- "C:/OSGeo4W64/bin" # hard code gdalPath (comment this command if getOption(...) works for you)

# For Step 2:
otbPath <- "C:/OTB-6.6.0-Win64/OTB-6.6.0-Win64/bin" # path to Orfeo Toolbox
otbSeg <- list.files(otbPath, pattern = "otbcli_Segmentation") # otb segmentation algorithm
segfileNr <- c(1:3) # Select fileNr for segmentation

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
  lapply(gdalCall, function(x) system(x))
  
  #-------------------------------------------------------------------------------------------------------#
  # make mosaic
  #-------------------------------------------------------------------------------------------------------#
  
  rasterFun <- list.files(gdalPath)[grep('translate', list.files(gdalPath))] # find "mosaic" function
  basePar <- paste0('-ot Byte -of JPEG -a_srs ', mycrsEPSG_UTM, ' -tr ', out.res[1], ' ', out.res[2])
  gdalCall <- paste0(gdalPath, rasterFun, ' ', basePar, ' ', vrt, ' ', ofile)
  #gdalCall
  lapply(gdalCall, function(x) system(x))
  
  rm(vrt) # clean up
}
gdalPath # optional control
mosaicPS(ipath = ipath, ofile = ofile, out.res = out.res)

writeOGR(final60, dsn = paste0(oDir, "/Final60QuadrantsRN.shp"), layer = "Final60QuadrantsRN", driver = 'ESRI Shapefile')

##########################################################################
# End of data pre-processing
##########################################################################

## Step 2: Classification

## 1. Segmentation
# -> Let's use the Orfeo Toolbox for segmenting because it is fast

# set i/o
ifile <- list()
for (i in 1:length(segfileNr)) {
  ifile[i] <- ofile[segfileNr[i]]
}
ifile # optional control

segfile <- list()
for (i in 1:length(ifile)) {
  segfile[i] <- gsub("merge", "seg", unlist(ifile[i]))
  segfile[i] <- gsub("jpg", "sqlite", unlist(segfile[i]))
}
segfile # optinal control

# set mode
mode <- "vector" #otb cannot handle large input files in raster mode

# set segmentation function
segfun <- "meanshift"

# formulate call
otbCall <- paste0(otbPath, '/', otbSeg, 
                  ' -in ', ifile, 
                  ' -mode ', mode, ' -mode.', mode, '.out ', 
                  segfile, ' uint16', 
                  ' -filter ', segfun)

otbCall # optional control
lapply(otbCall, function(x) system(x))

## 2. NDVI calculation
PseudoNDVI <- function(G, R) {
  (G - R)/(G + R)
}
segfile
pNDVI <- brick(unlist(ifile[1]))
pNDVI <- PseudoNDVI(pNDVI[[2]], pNDVI[[3]])

## 3. Classification


## 4. Output


