######################
## Travis Nauman
## marked  up by Charlotte Tierney, January 2022

## Script to rescale and compress
## rasters for more efficient storage
## and computation
## Converts all rasters in folder
## to 16-bit integer scale and compresses

## Workspace setup
# Install packages if not already installed
required.packages <- c("terra","sp", "rgdal","beepr","tidyverse") #"raster","parallel"
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
## Increase active memory useable by raster package
memory.limit(500000)
terraOptions(memfrac=0.9, memmax=3)
# rasterOptions(maxmemory = 1e+09, chunksize = 1e+08) #raster package isn't used


## Define folders #two folders for two different datasets
# GEEcovFold <- "D:/CPT_work/NM_DSM/covs_misc/Reprojected/"
OGcovFold <- "D:/CPT_work/NM_DSM/OG_covs/"
# Sent2Fold <- "D:/CPT_work/NM_DSM/GEE_Sent2med/Bands/"
ColbyFold <- "D:/CPT_work/NM_DSM/NCSS_covs_Colby/Clipped/"
# WorldClimFold <- "D:/CPT_work/NM_DSM/WorldClim30m/Clipped/"

# savefolder <- "D:/CPT_work/NM_DSM/GEE_Sent2med/Bands_compressed/" 
savefolder <- "D:/CPT_work/NM_DSM/COVS_compressed/"

## List files ## RUN ONE AT A TIME
# file.list <- list.files(path = GEEcovFold, pattern=".tif$", full.names = T, recursive = F)
# file.list <- list.files(path = OGcovFold, pattern=".tif$", full.names = T, recursive = F)
file.list <- list.files(path = ColbyFold, pattern=".tif$", full.names = T, recursive = F)
# file.list <- list.files(path = WorldClimFold, pattern=".tif$", full.names = T, recursive = F)

# file.list <- file.list[31:76]

################# Grab Min and Max data #################

## Set up MinMax table (outside of loop)
MinMaxAll <- matrix(c("test",1,1), ncol=3, byrow=TRUE)
colnames(MinMaxAll) <- c('covname','min','max')

for (i in 1:length(file.list)){
  file <- terra::rast(file.list[i])
  file.name <- file@ptr[["filenames"]] #get file name for covariate column
  file.name <- gsub(".tif","", file.name)
  file.name <- gsub(".*/","",file.name)
  mm <- minmax(file) # Grab Min and Max values of original file
  mm <- as.data.frame(t(mm)) #transpose
  row.names(mm) <- 1:nrow(mm)
  mm <- mm %>% 
    add_column(covname=file.name, #add cols for file name
               # bandname=c(file@ptr$names), #add col for band number/name, assumes Terra package
               .before = "V1") %>%
    rename(min=V1,max=V2)
  MinMaxAll <- rbind(MinMaxAll,mm)
}
MinMaxAll <- MinMaxAll[-1,] #remove test row

## Save MinMax Data
write.csv(MinMaxAll, 
          paste(savefolder,"OriginalMinMaxData_Colby_new.csv",sep=""),
          row.names = FALSE)





################# Grid Prep with Terra package#################
## Get list of grids ##RUN ONE FOLDER AT A TIME
cov.grids <- file.list 

for(r in 1:length(cov.grids)){
  rast <- rast(cov.grids[r])#[[1]]
  rminmax <- terra::minmax(rast)
  dtype <- ifelse(rminmax[1] < 0, "INT2S","INT2U")
  scalar <- 65530/(rminmax[2]-rminmax[1]) #where rminmax[1] is the min and rminmax[2] is the max
  intercept <- ifelse(dtype == "INT2S", rminmax[1]*scalar-(-32765), rminmax[1]*scalar-2)
  scaleFn <- function(rast){
    ind <- (rast*scalar)-intercept
    return(ind)
  }
  
  rastsc <- scaleFn(rast)#;beep(sound=1)
  
  file.name <- rast@ptr[["filenames"]] #get file name for covariate column
  file.name <- gsub(".tif","", file.name)
  file.name <- gsub(".*/","",file.name)
  
  terra::writeRaster(rastsc, filename=paste(savefolder,file.name,".tif",sep=""),
              overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES"), 
              datatype=dtype)#;beep(sound=1), rastsc@ptr[["names"]]
  gc()
  print(paste("done with ",r,": ",file.name,sep=""))
  rm(rast,rminmax,dtype,scalar,intercept,rastsc)
};beep(sound=1) 



## read in to test
test1 <- rast(paste(savefolder,"wc2.0_bio_30s_01.tif",sep=""))  #cc_mosaic_1.tif
plot(test1)
og1 <- rast(paste(OGcovFold,"cc_mosaic_1.tif",sep=""))
plot(og1)












