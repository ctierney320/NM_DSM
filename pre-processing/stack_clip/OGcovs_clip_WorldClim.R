## Clipping, checking projection, and reprojecting OG covariates
## Charlotte Tierney
## November 23, 2021
## Edited for Colby's covariates June 2022

################################################################################
required.packages <- c("rgdal","sf","terra","tidyverse","beepr") 
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

#Set memory options for your machine:
memory.limit(500000)
# rasterOptions(maxmemory = 1e+09, chunksize = 1e+08)
terraOptions(memfrac=0.9, memmax=3)


#Read in files:
# OG.fold <- "V:/PROJECTS/TRAVIS_NAUMAN/GIS_Archive/NED30m_CONUSmosaic_Covs"
OG.fold <- "D:/CPT_work/NM_DSM/NCSS_covs_Colby/" #"D:/CPT_work/NM_DSM/WorldClim30m/"
OG.covs <- list.files(path = OG.fold, pattern=".tif$", full.names = T, recursive = F)
# #remove geomorphons tif
# OG.covs <- OG.covs[-6]

#Check if you want:
plot(rast(OG.covs[2]))
test <- rast(OG.covs[2])

#Where do you want clipped files saved?
save.folder <- "D:/CPT_work/NM_DSM/NCSS_covs_Colby/Clipped/"

#Load clip boundary shapefile:
bound <- read_sf("G:/DSM_TWN/covar_sets/NM_extentSHP/nm_dsm_NMextent_reprojected.shp")
bound <- st_transform(bound, CRS("EPSG:5070"))
# bound <- st_transform(bound, CRS("EPSG:4326")) #WorldClim CRS, convert to this. reprojecting global raster takes too long

#create subset rectangle for terra::crop()
g <- terra::vect(bound) #converts og polygon geometry
plot(g)

# boundOG <- read_sf("G:/DSM_TWN/covar_sets/NM_extentSHP/nm_dsm_NMextent_reprojected.shp")
# boundOG <- st_transform(boundOG, CRS("EPSG:5070")) #GOAL CRS
# boundOG <- terra::vect(boundOG)

snaprast <- terra::rast("D:/CPT_work/NM_DSM/COVS_compressed/ca_mosaic.tif") #D:/CPT_work/NM_DSM/OG_covs/ca_mosaic.tif")

#crop:
for (i in 1:length(OG.covs)){ #i <- 8
  starttime <- Sys.time()
  
  file <- rast(OG.covs[i])#read in file
  # file <- terra::project(x = file, y=g, mask=TRUE) #"EPSG:5070" #TAKES FOREVER
  # filename <- file@ptr[["filenames"]]
  file <- crop(file, g)#;beep(sound=1) #this step takes a few minutes
  file <- mask(file, g)#;beep(sound=1) #plot(file)
  # terra::crs(file) <- "EPSG:5070"
  file <- terra::project(x = file, y=snaprast, 
                         method="cubicspline", mask=TRUE)#;beep(sound=1) #TAKES A WHILE
  file <- crop(file, snaprast)#again to make it fit snapraster
  file <- mask(file, snaprast)
  filename <- OG.covs[i]
  filename <- gsub(".tif","", filename)
  filename <- gsub(".*/","",filename)
  filename <- paste(save.folder,filename,".tif",sep="")
  terra::writeRaster(file,filename,overwrite=TRUE)#;beep(sound=1)
  
  endtime <- Sys.time()
  elapsed <- endtime - starttime
  print(paste(filename,"took",elapsed,sep=" "))
};beep(sound=1)

#check on one file:
snaprast <- terra::rast("D:/CPT_work/NM_DSM/OG_covs/ca_mosaic.tif")
plot(snaprast)

##Try stacking with other covs:
newcov <- terra::rast("D:/CPT_work/NM_DSM/WorldClim30m/Clipped/wc2.0_bio_30s_01.tif")
plot(newcov)

testcov <- terra::rast("D:/CPT_work/NM_DSM/NCSS_covs_Colby/processed/covs30m_sl_2.tif")
plot(testcov)

test2cov<- terra::rast("D:/CPT_work/NM_DSM/COVS_compressed/Sent2_stdev_9.tif")
plot(test2cov) #this one stacks with ca_mosaic

rasterlist <- c("D:/CPT_work/NM_DSM/OG_covs/ca_mosaic.tif",
                "D:/CPT_work/NM_DSM/NCSS_covs_Colby/Clipped/covs30m_sl_2.tif")
                #"D:/CPT_work/NM_DSM/GEE_Sent2med/Bands_stacked/NM_Sent2_med_fixed_23.tif")
                #"D:/CPT_work/NM_DSM/WorldClim30m/Clipped/wc2.0_bio_30s_01.tif")
stack <- terra::rast(rasterlist)

stack <- (c(file,snaprast))
