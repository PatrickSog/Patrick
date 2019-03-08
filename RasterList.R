## packages
require('naturalsort', 'gdalUtils')

## paths
LandklifDir <- "D:/Patrick/Documents/Dokumente/Landklif/Landklif1/"
tmpdir <- "D:/Patrick/Zeug"

## crs
mycrsEPSG <- as.character("EPSG:31468")

#################################

## read list of rasters
dirlist <- list.dirs(LandklifDir)
Pdirlist <- grep("*Landklif1/P", dirlist, value = T)
# Optonal control
#Pdirlist
Pdir <- Pdirlist[naturalorder(Pdirlist)]
# Optinal control
Pdir # natural order of P-directories

## read files in each directory
allfilesCK <- lapply(Pdirlist, function(x) list.files(x, pattern="jpg$", full.names = T))
allfilesCK <- allfilesCK[naturalorder(allfilesCK)]
allfilesCK # natural ordered list of files in directories

#################################

## Mosaic rasters
# Credit to Ruben Remelgado for the gdal-based mosaic()-function

ipath <- paste0(Pdir, "/")          # path should end with '/'
out.res <- as.character(c(0.2,0.2)) # set output resolution
ofile <- list()
for (i in 1:length(ipath)) {        # full path of output
  filelist[[i]][2] <- paste0(ipath[i], "P", i, "_merge.jpg")
  }

# optional control on input
ipath
ofile
out.res

mosaicPS <- function(ipath, ofile, out.res) {
  
  #-------------------------------------------------------------------------------------------------------#
  # find valid gdal instalation
  #-------------------------------------------------------------------------------------------------------#
  
  # base requirements to rasterize files with GDAL
  gdal_setInstallation() # sets gdal installation (not always needed)
  gdalPath <- getOption("gdalUtils_gdalPath")[[1]]$path # find gdal installation 
                                                        # Does not always work, if complications hard code gdalPath as string
  # hard coding gdalPath (not always needed)
  gdalPath <- 'D:/Programs/OSGeo4W/bin/' # hard code gdalPath (comment this command if getOption(...) works for you)
  
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
  basePar <- paste0('-ot Byte -of JPEG -a_srs ', mycrsEPSG, ' -tr ', out.res[1], ' ', out.res[2])
  gdalCall <- paste0(gdalPath, rasterFun, ' ', basePar, ' ', vrt, ' ', ofile)
  #gdalCall
  lapply(gdalCall, function(x) system(x))
  
  rm(vrt) # clean up
}
mosaicPS(ipath = ipath, ofile = ofile, out.res = out.res)

