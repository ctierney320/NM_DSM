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
CovFold1 <- "/mnt/disks/sped/NMDSMactiveCopy/" #processed on GCP (SAR, sand, clay)
CovFold2 <- "/mnt/covs/NM_DSM/NMpreds/Final/" #processed on office machine (Gypsum, CaCO3, EC)
savefolder <- "/mnt/disks/sped/NMDSMactiveCopy/Masked/"

mask <- rast("/mnt/disks/sped/NMDSMactiveCopy/3000m_water_mask.tif") #water and elevation mask 

# nmext <- "/mnt/disks/sped/NMDSMactiveCopy/NewMexico.shp" #new mexico extent for GYPSUM ONLY

# property <- "CaCO3" #property name for file saving and list extracting--case sensitive 



################# Property Prep ################# 
file.list1 <- list.files(path = CovFold1, pattern=".tif$", full.names = T, recursive = F)
file.list2 <- list.files(path = CovFold2, pattern=".tif$", full.names = T, recursive = F)

## choose your property
# file.list.prop <- file.list[95] #gypsum #CHOOSE FILE MANUALLY #the property actually doesn't mateter
# file.list.prop <- file.list[1] #caco3
# file.list.prop <- file.list[42] #ec
# file.list.prop <- file.list[70] #sar
# file.list.prop <- file.list[50] #sand
file.list.prop <- file.list1[10] #clay


for (i in 1:length(file.list2)){
  if (str_detect(file.list2[i],"CaCO3")==TRUE) {
    # print(file.list[i])
    file <- file.list2[i]
    file.list.prop <- rbind(file.list.prop,file)
  }
  if (str_detect(file.list2[i],"EC")==TRUE) {
    # print(file.list[i])
    file <- file.list2[i]
    file.list.prop <- rbind(file.list.prop,file)
  }
}

for (i in 1:length(file.list1)){
  if (str_detect(file.list1[i],"SAR")==TRUE) {
    # print(file.list[i])
    file <- file.list1[i]
    file.list.prop <- rbind(file.list.prop,file)
  }
  if (str_detect(file.list1[i],"Sand")==TRUE) {
    # print(file.list[i])
    file <- file.list1[i]
    file.list.prop <- rbind(file.list.prop,file)
  }
  if (str_detect(file.list1[i],"Clay")==TRUE) {
    # print(file.list[i])
    file <- file.list1[i]
    file.list.prop <- rbind(file.list.prop,file)
  }
}

file.list.prop <- file.list.prop[-1,] #remove test row
file.list <- file.list.prop



################# Let's Loop It ################# 

## Prepare mask
mask[mask==0] <- NA #not needed for Gypsum 

#i <- 1
start <- Sys.time()
for (i in 1:length(file.list)){

  ### Mask ###
  file <- terra::rast(file.list[i]) 
  masked <- mask(file, mask) #inverse=FALSE, maskvalues=NA, updatevalue=NA, filename="", ...
  #plot(masked)
  
  file <- masked  #terra::rast(file.list[i])
  file.name <- gsub(".tif","", file.list[i])
  file.name <- gsub(".*/","",file.name)
  
  
  terra::writeRaster(masked,overwrite=TRUE,
              filename=paste(savefolder,file.name,"_Mask.tif",sep=""),
              gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
  
  gc()
  print(paste("done with ",i,": ",file.name,sep=""))
  # rm(rast,rminmax,dtype,scalar,intercept)
  
}
end <- Sys.time()
elapsed <- end-start
elapsed
Sys.time()




################################################################################




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

#### DO NOT USE!
#### CODE ABOVE IS DERRIVED FROM:


# ### Grab Min and Max data ###
# ## Set up MinMax table (outside of loop)
# MinMaxAll <- matrix(c("test",1,1), ncol=3, byrow=TRUE)
# colnames(MinMaxAll) <- c('file','min','max')
#   
# for (i in 1:length(file.list)){
#   file <- terra::rast(file.list[i])
#   file.name <- file@ptr[["filenames"]] #get file name for covariate column
#   file.name <- gsub(".tif","", file.name)
#   file.name <- gsub(".*/","",file.name)
#   mm <- minmax(file) # Grab Min and Max values of original file
#   mm <- as.data.frame(t(mm)) #transpose
#   row.names(mm) <- 1:nrow(mm)
#   mm <- mm %>% 
#     add_column(file=file.name, #add cols for file name
#                # bandname=c(file@ptr$names), #add col for band number/name, assumes Terra package
#                .before = "min") #%>%
#     #rename(min=V1,max=V2)
#   MinMaxAll <- rbind(MinMaxAll,mm)
# }
# MinMaxAll <- MinMaxAll[-1,] #remove test row
# 
# ## Save MinMax Data
# write.csv(MinMaxAll, 
#           paste(savefolder,"OriginalMinMaxData_",property,".csv",sep=""),
#           row.names = FALSE)
# 
# 
# ################# Mask ################# 
# mask[mask==0] <- NA
# for (i in 1:length(file.list)){
#   file <- rast(file.list[i])
#   masked <<- mask(file, mask) #inverse=FALSE, maskvalues=NA, updatevalue=NA, filename="", ...
# }
# 
# 
# 
# 
# 
# 
# ################# Compress with Terra package#################
# ## Get list of grids ##RUN ONE FOLDER AT A TIME
# cov.grids <- file.list 
# 
# for(r in 1:length(cov.grids)){
#   rast <- rast(cov.grids[r])#[[1]]
#   rminmax <- terra::minmax(rast)
#   dtype <- ifelse(rminmax[1] < 0, "INT2S","INT2U")
#   scalar <- 65530/(rminmax[2]-rminmax[1]) #where rminmax[1] is the min and rminmax[2] is the max
#   intercept <- ifelse(dtype == "INT2S", rminmax[1]*scalar-(-32765), rminmax[1]*scalar-2)
#   scaleFn <- function(rast){
#     ind <- (rast*scalar)-intercept
#     return(ind)
#   }
#   
#   rastsc <- scaleFn(rast)#;beep(sound=1)
#   
#   file.name <- rast@ptr[["filenames"]] #get file name for covariate column
#   file.name <- gsub(".tif","", file.name)
#   file.name <- gsub(".*/","",file.name)
#   
#   terra::writeRaster(rastsc, filename=paste(savefolder,file.name,".tif",sep=""),
#                      overwrite=TRUE, gdal=c("COMPRESS=DEFLATE", "TFW=YES"), 
#                      datatype=dtype)#;beep(sound=1), rastsc@ptr[["names"]]
#   gc()
#   print(paste("done with ",r,": ",file.name,sep=""))
#   rm(rast,rminmax,dtype,scalar,intercept,rastsc)
# };beep(sound=1) 
# 
# 
# 
# ## read in to test
# test1 <- rast(paste(savefolder,"wc2.0_bio_30s_01.tif",sep=""))  #cc_mosaic_1.tif
# plot(test1)
# og1 <- rast(paste(OGcovFold,"cc_mosaic_1.tif",sep=""))
# plot(og1)
# 
# 
# 
# ############## tests
# 
# r <- rast(ncols=10, nrows=10)
# m <- rast(ncols=10, nrows=10)
# values(r) <- 1:100
# set.seed(1965)
# x <- round(3 * runif(ncell(r)))
# x[x==0] <- NA
# values(m) <- x
# mr <- mask(r, m)
# 
# plot(m)
# plot(r)
# plot(mr)
# 
# rm(r,m,x,mr)








