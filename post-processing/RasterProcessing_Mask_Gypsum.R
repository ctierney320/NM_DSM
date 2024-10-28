## Raster Processing
## Charlotte Tierney
## created: November 2022
## last edited: December 2022

## Read in pre-made binary raster with elevation and surface water mask
## For gypsum, clip to NM state boundary
## Mask all rasters for all properties
## Note min and max values 
## Compress to 16-bit
## Save



## Workspace setup
# Install packages if not already installed
required.packages <- c("terra","sp", "rgdal","beepr","tidyverse","sf") #"raster","parallel"
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
## Increase active memory useable by raster package
# memory.limit(500000)
terraOptions(memfrac=0.9, memmax=3)
# rasterOptions(maxmemory = 1e+09, chunksize = 1e+08) #raster package isn't used




################# Define Folders & Files #################
# CovFold <- "/mnt/disks/sped/NMDSMactiveCopy/" #processed on GCP (SAR, sand, clay)
CovFold <- "/mnt/covs/NM_DSM/NMpreds/Final/" #processed on office machine (Gypsum, CaCO3, EC)
savefolder <- "/mnt/disks/sped/NMDSMactiveCopy/Masked/"

mask <- rast("/mnt/disks/sped/NMDSMactiveCopy/3000m_water_mask.tif") #water and elevation mask 

nmext <- "/mnt/disks/sped/NMDSMactiveCopy/NewMexico.shp" #new mexico extent for GYPSUM ONLY

property <- "Gypsum" #property name for file saving and list extracting--case sensitive 



################# Property Prep ################# 
file.list <- list.files(path = CovFold, pattern=".tif$", full.names = T, recursive = F)

## choose your property
# file.list.prop <- file.list[95] #gypsum #CHOOSE FILE MANUALLY #the property actually doesn't mateter
# file.list.prop <- file.list[1] #caco3
# file.list.prop <- file.list[42] #ec
# file.list.prop <- file.list[70] #sar
# file.list.prop <- file.list[50] #sand
file.list.prop <- file.list[10] #clay


for (i in 1:length(file.list)){
  if (str_detect(file.list[i],property)==TRUE) {
    # print(file.list[i])
    file <- file.list[i]
    file.list.prop <- rbind(file.list.prop,file)
  }
}
file.list.prop <- file.list.prop[-1,] #remove test row
file.list <- file.list.prop

### For Gypsum only ###

#read in NM state boundary
testrast <- rast(file.list[1])
rcrs <- crs(testrast)
bound <- read_sf(nmext)
bound <- st_transform(bound, CRS(rcrs)) #CRS("EPSG:5070"))
g <- terra::vect(bound)
rm(testrast,bound)
#clip mask raster to NM
maskNM <- crop(mask, g)
maskNM <- mask(maskNM, g)
maskNM[maskNM==0] <- NA


################# Let's Loop It ################# 
## Set up MinMax table (outside of loop)
MinMaxAll <- matrix(c("test","test",1,1), ncol=4, byrow=TRUE)
colnames(MinMaxAll) <- c('property','file','min','max')

## Prepare mask
# mask[mask==0] <- NA #not needed for Gypsum 
       
#i <- 1
start <- Sys.time()
for (i in 1:length(file.list)){
  
  ### Clip ### GYPSUM ONLY ###
  # clip with NM boundary (g)
  file <- terra::rast(file.list[i])
  file <- crop(file, g)
  file <- mask(file, g)
  mask <- maskNM
  
  
  ### Mask ###
  # file <- terra::rast(file.list[i]) ###  COMMENT OUT FOR GYPSUM!!!!!!! **
  masked <- mask(file, mask) #inverse=FALSE, maskvalues=NA, updatevalue=NA, filename="", ...
  #plot(masked)
  
  file <- masked  #terra::rast(file.list[i])
  file.name <- gsub(".tif","", file.list[i])
  file.name <- gsub(".*/","",file.name)
  
  ### Grab Min and Max data ###
  file <- masked  #terra::rast(file.list[i])
  file.name <- gsub(".tif","", file.list[i])
  file.name <- gsub(".*/","",file.name)
  mm <- minmax(file) # Grab Min and Max values of original file
  mm <- as.data.frame(t(mm)) #transpose
  row.names(mm) <- 1:nrow(mm)
  mm <- mm %>% 
    add_column(property=property, #add cols for file name
               .before = "min") %>%
    add_column(file=file.name, #add cols for file name
               .before = "min") #%>%
  #rename(min=V1,max=V2)
  MinMaxAll <- rbind(MinMaxAll,mm)
  
  
  ### Compress ###
  rast <- masked
  rminmax <- terra::minmax(rast)
  dtype <- ifelse(rminmax[1] < 0, "INT2S","INT2U")
  scalar <- 65530/(rminmax[2]-rminmax[1]) #where rminmax[1] is the min and rminmax[2] is the max
  intercept <- ifelse(dtype == "INT2S", rminmax[1]*scalar-(-32765), rminmax[1]*scalar-2)
  scaleFn <- function(rast){
    ind <- (rast*scalar)-intercept
    return(ind)
  }
  rastsc <- scaleFn(rast)#;beep(sound=1)
  #plot(rastsc)
  
  terra::writeRaster(rastsc, filename=paste(savefolder,file.name,"_MC.tif",sep=""), #MC = masked and compressed
                     overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES"), 
                      datatype=dtype)#;beep(sound=1), rastsc@ptr[["names"]]
  
  #  terra::writeRaster(masked, filename=paste(savefolder,file.name,"_Mask.tif",sep=""), 
  #                    overwrite=TRUE, options=c("COMPRESS=DEFLATE", "TFW=YES"),
  #                    progress="text")#;beep(sound=1), rastsc@ptr[["names"]]
  #  
  #  terra::writeRaster(masked,overwrite=TRUE,
  #                     filename=paste(savefolder,file.name,"_Mask.tif",sep=""),
  #                     gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
  
  gc()
  print(paste("done with ",i,": ",file.name,sep=""))
  # rm(rast,rminmax,dtype,scalar,intercept)
  
}
end <- Sys.time()
elapsed <- end-start
elapsed
Sys.time()

# MinMaxAll <- MinMaxAll[-1,] #remove test row[] 
# write.csv(MinMaxAll, 
#           paste(savefolder,"OriginalMinMaxData_",property,".csv",sep=""),
#           row.names = FALSE)

### AFTER ALL PROPERTIES ARE DONE: ###
# #read in all CSVs and make one OG min/max file
# file.list <- list.files(path = savefolder, pattern=".csv$", full.names = T, recursive = F)
# 
# allprops <- read.csv(file.list[1])
# 
# for (i in 2:length(file.list)){
#   oneprop <- read.csv(file.list[i])
#   allprops <- rbind(allprops,oneprop)
# }
# write.csv(allprops, 
#           paste(savefolder,"OriginalMinMaxData_AllProps.csv",sep=""),
#           row.names = FALSE)




#################################################################################










