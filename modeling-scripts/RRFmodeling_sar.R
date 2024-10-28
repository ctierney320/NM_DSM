#### Modeling SAR
#### Charlotte Tierney
#### created: September 2022
#### last edit: October 2022

required.packages <- c("ranger","plyr","raster", "sp", "rgdal", 
                       "randomForest", "quantregForest","snowfall",
                       "dplyr", "ggplot2","hexbin","snow","doParallel") #"terra",parallel") # might need "snow", "snowfall",
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
# memory.limit(500000)
# terraOptions(memfrac=0.9) #, memmax=3)

# #linux memory suff
# library("pryr")
# mem_used()
# # install.packages("ulimit")
# # library("ulimit")
# # ulimit::memory_limit()

# system('grep MemTotal /proc/meminfo')
# system('free -m')

# # install.packages("unix") 
# library(unix)
# rlimit_as(6e+10)  #increases to ~60GB #uses bytes
# rlimit_all()



######## Inputs & Outputs ######## 
## Pedon point data
pedons <- read.csv("/mnt/disks/sped/NMDSMactive/PEDONS_SAR.csv") #must be csv, rds requires more manipulation

## Covariate raster data
covfolder <- "/mnt/disks/sped/NMcovs" #"G:/DSM_TWN/covar_sets/NM_compressed/" #CHANGE 
covs <-list.files(path = covfolder, pattern=".tif$", full.names = T, recursive = F)

## Snap raster for CRS info
snaprast <- raster("/mnt/disks/sped/NMcovs/ca_mosaic.tif") #"U:/2022/NM_DSM/ca_mosaic.tif") #grab snap raster #backed up on U:/ drive
ProjectCRS <- crs(snaprast)

## Study Extent shapefile
ProjExtent <- readOGR("/mnt/disks/sped/NMDSMactive/nm_dsm_NMextent_reprojected.shp") #G:/DSM_TWN/covar_sets/NM_extentSHP/nm_dsm_NMextent_reprojected.shp")
spTransform(ProjExtent,CRSobj=ProjectCRS) ##**FIX**#####

## Save folder
savefolder <- "/mnt/disks/sped/NMDSMactiveCopy/"

## Property information
property <- "SAR"    #common name for file saving
prop <- "sar_FINAL" #column name in pedon table
version <- "Fullqt" #test version name for file saving

## Set transformation 
trans <- "log" # none, log10, log, or sqrt






######## Extract Points  ########
## Without parallel: each covariate takes about 1 minute
##94 covariates took 64 minutes
#ProjectCRS2 <- as.character(ProjectCRS)
pedons.v <- terra::vect(pedons, geom=c("utm_east", "utm_north"), crs=ProjectCRS@projargs)
pedons$ID <- seq.int(nrow(pedons))
pts.ext <- as.data.frame(pedons)

for (i in 1:length(covs)){
  rast <- terra::rast(covs[i])
  pts.ext.indv <- terra::extract(rast, pedons.v)
  file.name <- covs[i] #rast@ptr[["filenames"]] #get file name for covariate column
  file.name <- gsub(".tif","", file.name)
  file.name <- gsub(".*/","",file.name)
  names(pts.ext.indv)[2] <- file.name 
  pts.ext <<- merge(pts.ext,pts.ext.indv, by="ID")
  print(paste("Done with",file.name,"at",Sys.time(),sep=" "))
}; #beep(sound=1)

pts.ext <- pts.ext[,! names(pts.ext) %in% c("X","optional")]
rm(pts.ext.indv,rast)

## Save points
# saveRDS(pts.ext, paste(savefolder,property,"_",version,"_ExtractedPts.rds",sep=""))

# pts.ext <- readRDS("/mnt/disks/sped/NMDSMactive/SAR_Fullq_ExtractedPts.rds")

#rm(pts.ext,pedons,pedons.v,file.name,i)

# #### SUBSET TO JUST NEW MEXICO:
{
  # NMbound <- readOGR("G:/DSM_TWN/covar_sets/NM_extentSHP/NewMexico.shp")
  # spTransform(NMbound,CRSobj=ProjectCRS)
  # NMbound <- st_as_sf(NMbound)
  # 
  # # pts.ext.v <- terra::vect(pts.ext, geom=c("utm_east", "utm_north"), crs=ProjectCRS)
  # # pts.ext.nm <- clip(pts.ext.v, NMbound)
  # 
  # 
  # coordinates(pts.ext) <- ~utm_east+utm_north #assign coordinates
  # pts.ext <- st_as_sf(pts.ext)
  # pts.ext <- st_set_crs(pts.ext,5070)
  # 
  # pts.ext.nm <- st_intersection(pts.ext,NMbound)
  # 
  # plot(pts.ext.nm, max.plot = 2)
  # plot(NMbound)
  # 
  # # pts.ext <- spTransform(pts.ext, CRS(projargs = "EPSG:5070"))
  # # # pts.ext <- st_transform(pts.ext, CRS("EPSG:5070"))
  # # proj4string(pts.ext) <- CRS(projargs=ProjectCRS)
  # 
  # # pts.ext.nm <- pts.ext[ProjExtent,] 
  # 
  # saveRDS(pts.ext.nm, paste(savefolder,property,"_",d,"cm_",version,"_ExtractedPts_NMonly.rds",sep=""))
  # pts.ext <- pts.ext.nm
}

######## Model  ########
#read in extracted points if you didn't just run them:
pts.ext <- readRDS("/mnt/disks/sped/NMDSMactive/SAR_Fullq_ExtractedPts.rds")

# pts.ext$sar_FINAL <- as.numeric(pts.ext$sar_FINAL)
# pts.ext <- na.omit(pts.ext)

#might need to re-rast vectorize?

#separate by depth
depths <- c(5,15,30,60,100,150) #(0,
# d <- 0 #d <- 5
# detach("package:terra", unload = TRUE)
for(d in depths){
  print(paste("Depth",d,"started at",Sys.time(),sep=" "))
  pts.extd <- subset(pts.ext, as.numeric(pts.ext$topdepth_cm) <= d & as.numeric(pts.ext$botdepth_cm) > d) # subset to chosen depth
  
  covs.list <- covs #get file name for covariate column
  covs.list <- gsub(".tif","", covs.list)
  covs.list <- gsub(".*/","",covs.list)
  
  pts.extd <- pts.extd[c(covs.list,prop)]## create a specific list of dependent variable and covariate names to use 
  # pts.extd <- na.omit(as.numeric(pts.extd$sar_FINAL))
  pts.extd <- na.omit(pts.extd)# Remove any record with NA's (in any column - be careful)
  
  # #Transform data
  ## Apply transformation
  if(trans=="log10") {pts.extd$transformed <- log10(pts.extd$sar_FINAL + 0.1)}
  if(trans=="log") {pts.extd$transformed <- log(pts.extd$sar_FINAL + 1)}
  if(trans=="sqrt") {pts.extd$transformed <- sqrt(pts.extd$sar_FINAL)}
  if(trans=="none") {pts.extd$transformed <- pts.extd$sar_FINAL}
  
  # pts.extd$transformed <- log(pts.extd$caco3_FINAL + 1) #gypsum
  # pts.extd$transformed <- (exp(pts.extd$caco3_FINAL))-1 #ucrb caco3
  # pts.extd$transformed <- sqrt(pts.extd$caco3_FINAL) #eh
  # pts.extd$transformed <- (pts.extd$caco3_FINAL)^2 #deffo not
  # pts.extd <- na.omit(pts.extd)
  
  saveRDS(pts.extd, paste(savefolder,property,"_",d,"cm_",version,"_ExtractedPtsObj.rds",sep=""))
  
  #### Ranger Random Forest:
  xtrain <- as.matrix(pts.extd[c(covs.list)]) #you don't even need this
  
  # ## Not Transformed:
  # ytrain <- c(as.matrix(pts.extd[c(prop)]))
  # formula <- as.formula(paste(prop,'~',paste(covs.list, collapse="+"))) #uses non-transformed property col
  # varrange <- as.numeric(quantile(pts.extd$sar_FINAL, probs=c(0.975), na.rm=T)-quantile(pts.extd$sar_FINAL, probs=c(0.025), na.rm=T))
  
  
  ## Transformed
  ytrain <- c(as.matrix(pts.extd[c("transformed")])) #same with this
  varrange <- as.numeric(quantile(pts.extd$transformed, probs=c(0.975), na.rm=T)-quantile(pts.extd$transformed, probs=c(0.025),na.rm=T)) ## TRANSFORMED
  formula <- as.formula(paste("transformed",'~',paste(covs.list, collapse="+")))
  
  
  covs.stack.raster <- raster::stack(covs) #build raster stack
  # covs.stack.raster2 <- readRDS("/mnt/disks/sped/NMDSMactive/RasterStackObj.rds") #raster obj from CaCO3 run, def works
  
  
  ################ Predictions 
  
  #Build model
  rangerQmodel <- ranger(formula, 
                         data=pts.extd, 
                         num.trees = 100, 
                         quantreg = T, 
                         importance = "permutation", 
                         write.forest = TRUE)
  
  saveRDS(rangerQmodel, paste(savefolder,property,"_",d,"cm_",version,"_RangerModelObj.rds",sep=""))
  # imp <- data.frame(var=names(rangerQmodel$variable.importance),imp_meas = unname(rangerQmodel$variable.importance))
  # imp[order(imp$imp_meas, decreasing = T),][1:30,]
  
  #Parallelize Predictions
  # cpus <- parallel::detectCores()# - 2
  rasterOptions(maxmemory = 5e+11, chunksize = 1e+09)
  beginCluster(110,type='SOCK') #,filename = paste(savefolder,property,"_",version,"_clusterRoutput.rds",sep=""))
  
  predfun <- function(model, ...) predict(model, ...)$predictions
  start <- Sys.time()
  #standard prediction:
  Sys.time()
  pred <- clusterR(covs.stack.raster, predict,
                   args=list(model=rangerQmodel, fun=predfun), 
                   # filename = paste(savefolder,property,"_",version,"_clusterRoutput.rds",sep=""),
                   progress="text")
  Sys.time()
  #uncertainty predictions:
  predl <- clusterR(covs.stack.raster, predict, 
                    args=list(model=rangerQmodel, fun=predfun,type = "quantiles", quantiles = c(0.025)),
                    progress="text")
  Sys.time()
  predh <- clusterR(covs.stack.raster, predict, 
                    args=list(model=rangerQmodel, fun=predfun,type = "quantiles", quantiles = c(0.975)),
                    progress="text")
  
  Sys.time()
  
  # ## Backtransform if needed
  # bt.fn <- function(x) {
  #   # #ind <- (exp(x))-1 #If a backtransform is needed 10^(x) or exp(x) or ^2
  #   ind <- (x)^2
  #   return(ind)
  # }
 
  
  ## Back transformation Stuff
  if(trans=="log10"){
    bt.fn <- function(x) {
      ind <-  (10^(x))-0.1
      return(ind)
    }
  }
  if(trans=="log"){
    bt.fn <- function(x) {
      ind <-  (exp(x))-1
      return(ind)
    }
  }
  if(trans=="sqrt"){
    bt.fn <- function(x) {
      ind <-  x^2
      return(ind)
    }
  }
  
  
  #back transform if necessary!!
  pred_bt <- clusterR(pred, calc, args=list(fun=bt.fn),progress='text')
  predh_bt <- clusterR(predh, calc, args=list(fun=bt.fn),progress='text')
  predl_bt <- clusterR(predl, calc, args=list(fun=bt.fn),progress='text')
  
  end <- Sys.time()
  endCluster()
  
  elapsed <- end-start
  
  #Save everything
  
  # ## Not transformed:
  # writeRaster(pred,overwrite=TRUE,
  #             filename=paste(savefolder,property,"_",d,"cm_",version,"_Prediction.tif",sep=""),
  #             options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
  # writeRaster(predl,overwrite=TRUE,
  #             filename=paste(savefolder,property,"_",d,"cm_",version,"_Prediction_l.tif",sep=""),
  #             options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
  # writeRaster(predh,overwrite=TRUE,
  #             filename=paste(savefolder,property,"_",d,"cm_",version,"_Prediction_h.tif",sep=""),
  #             options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
  # 
  # saveRDS(pred, paste(savefolder,property,"_",d,"cm","_PredObj.rds",sep=""))
  # saveRDS(predl, paste(savefolder,property,"_",d,"cm","_PredLObj.rds",sep=""))
  # saveRDS(predh, paste(savefolder,property,"_",d,"cm","_PredHObj.rds",sep=""))
  
  
  ## If transformed:
  writeRaster(pred_bt,overwrite=TRUE,
              filename=paste(savefolder,property,"_",d,"cm_",version,"_Prediction_bt.tif",sep=""),
              options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
  writeRaster(predl_bt,overwrite=TRUE,
              filename=paste(savefolder,property,"_",d,"cm_",version,"_Prediction_lbt.tif",sep=""),
              options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
  writeRaster(predh_bt,overwrite=TRUE,
              filename=paste(savefolder,property,"_",d,"cm_",version,"_Prediction_hbt.tif",sep=""),
              options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")

  saveRDS(pred, paste(savefolder,property,"_",d,"cm_",version,"_PredObj.rds",sep=""))
  saveRDS(pred_bt, paste(savefolder,property,"_",d,"cm_",version,"_PredBTobj.rds",sep=""))
  saveRDS(predl, paste(savefolder,property,"_",d,"cm_",version,"_PredLobj.rds",sep=""))
  saveRDS(predh, paste(savefolder,property,"_",d,"cm_",version,"_PredHobj.rds",sep=""))
  saveRDS(predl_bt, paste(savefolder,property,"_",d,"cm_",version,"_PredLBTobj.rds",sep=""))
  saveRDS(predh_bt, paste(savefolder,property,"_",d,"cm_",version,"_PredHBTobj.rds",sep=""))
  
  
  ############### Uncertainty
  
  # # #If you don't have "varrange" objects already: load points data 
  # pts.extd <- readRDS(paste(savefolder,"ExtractedPoints_0cm_Gypsum_Aug5NMqt.rds",sep=""))
  # varrange <- as.numeric(quantile(pts.extd$prop, probs=c(0.975), na.rm=T)-quantile(pts.extd$prop, probs=c(0.025),na.rm=T)) 
  # # varrange <- as.numeric(quantile(pts.extd$transformed, probs=c(0.975), na.rm=T)-quantile(pts.extd$transformed, probs=c(0.025),na.rm=T)) ## TRANSFORMED
  # 
  # # #read in predicted objects if needed
  # predh_bt <- readRDS(paste(savefolder,"Predh_bt_object_0cm_Gypsum_Aug5NMqt.rds",sep=""))
  # predl_bt <- readRDS(paste(savefolder,"Predl_bt_object_0cm_Gypsum_Aug5NMqt.rds",sep=""))
  
  
  ##RUN UNCERTAINTY:
  # #not transformed:
  # pred.uncert.data <- (predh - predl)/varrange #*100
  
  #transformed:
  pred.uncert.data <- (predh_bt - predl_bt)/varrange #*100
  # plot(pred.uncert.data)
  
  
  saveRDS(pred.uncert.data,paste(savefolder,property,"_",d,"cm_",version,"_ModUncertRPIdata.rds",sep=""))
  
  writeRaster(pred.uncert.data,overwrite=TRUE,
              filename=paste(savefolder,property,"_",d,"cm_",version,"_ModUncertRPI.tif",sep=""), 
              options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
  
  # #Ignore this
  # varrange_nt <- as.numeric(quantile(pts.extd$gyp_FINAL, probs=c(0.975), na.rm=T)-quantile(pts.extd$gyp_FINAL, probs=c(0.025),na.rm=T))
  # pred.uncert.data_nt <- (predh_bt - predl_bt)/varrange_nt
  # saveRDS(pred.uncert.data_nt,paste(savefolder,"ModelUncertaintyRPIdata_",d,"cm_",
  #                                property,"_",version,"_nt.rds",sep=""))
  # writeRaster(pred.uncert.data_nt,overwrite=TRUE,
  #             filename=paste(savefolder,"ModelUncertaintyRPI",d,"cm_",property,"_",version,"_nt.tif",sep=""), 
  #             options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
  
  
  
  
  
  ################### Manual Cross validation ################################
  # pts.extd2 <- subset(pts.ext, as.numeric(pts.ext$topdepth_cm) <= d & as.numeric(pts.ext$botdepth_cm) > d) # subset to chosen depth
  # pts.extd2 <- pts.extd2[c(covs.list,prop)]## create a specific list of dependent variable and covariate names to use 
  # pts.extd2 <- na.omit(pts.extd2)# Remove any record with NA's (in any column - be careful)
  
  
  pts.extcvm <- pts.extd
  nfolds <- 10
  pts.extcvm$folds <- sample.int(nfolds,size =length(pts.extcvm[,1]),replace=T)
  
  # #if not transformed:
  # pts.extcvm$prop_t <- pts.extd$sar_FINAL ## UPDATE: transform if needed else just create new version of prop #log(   +1) or sqrt()
  # pts.extcvm <- pts.extcvm %>% rename(prop_t = sar_FINAL)
  
  # if transformed:
  pts.extcvm$prop_t <- pts.extcvm$transformed ##TRANSFORMED
  
  formulaStringCVm <- as.formula(paste('prop_t ~', paste(covs.list, collapse="+")))
  
  #for (g in seq(nfolds)){
  CV_factorRF <- function(g,pts.extcvm, formulaStringCVm){
    traindf <- subset(pts.extcvm, pts.extcvm$folds != g)
    testdf <- subset(pts.extcvm, pts.extcvm$folds == g)
    # xtrain.t <- as.matrix(traindf[c(cov.names)])
    # ytrain.t <- c(as.matrix(traindf$prop_t))
    rangerQmodel_cv <- ranger(formulaStringCVm, 
                              data=traindf, 
                              num.trees = 100, 
                              quantreg = T, 
                              importance = "permutation", 
                              write.forest = TRUE)
    
    rf.pcvc <- rangerQmodel_cv
    # class(rf.pcvc) <- "randomForest"
    # traindf$pcvpredpre <- predict(rf.pcvc, data=traindf)
    traindfpred <- predict(rf.pcvc, data=traindf)
    traindf$pcvpredpre <-traindfpred[["predictions"]]
    
    testdfpred <- predict(rf.pcvc, data=testdf)
    testdf$pcvpredpre <- testdfpred[["predictions"]]
    
    #traindf$pcvpredpre <- predict(rf.pcv, newdata=traindf, what=c(0.5)) ## If median is desired
    #testdf$pcvpredpre <- predict(rf.pcv, newdata=testdf,, what=c(0.5)) ## If median is desired
    
    testpred025 <- predict(rf.pcvc, data=testdf, type = "quantiles", quantiles =c(0.025)) #quantile model object
    testdf$pcvpredpre.025 <- testpred025[["predictions"]]
    
    testpred975 <- predict(rf.pcvc, data=testdf, type = "quantiles", quantiles =c(0.975)) #quantile model object
    testdf$pcvpredpre.975 <- testpred975[["predictions"]]
    
    attach(traindf)
    lm.pcv <- lm(prop_t~pcvpredpre)
    detach(traindf)
    testdf$pcvpred <- predict(lm.pcv, newdata=testdf)
    return(testdf)
  }
  snowfall::sfInit(parallel=TRUE, cpus=nfolds)
  snowfall::sfExport("pts.extcvm","formulaStringCVm","CV_factorRF","covs.list")
  snowfall::sfLibrary(ranger)
  snowfall::sfLibrary(randomForest)
  snowfall::sfLibrary(quantregForest)
  pts.extpcv <- snowfall::sfLapply(1:nfolds, function(g){CV_factorRF(g, pts.extcvm=pts.extcvm,formulaStringCVm=formulaStringCVm)})
  snowfall::sfStop()
  pts.extpcv <- plyr::rbind.fill(pts.extpcv)
  pts.extpcv$pcvpred <- as.numeric(pts.extpcv$pcvpred)
  
  ## PCV statistics
  
  
  cvp.RMSE <- sqrt(mean((pts.extpcv$prop_t - pts.extpcv$pcvpred)^2, na.rm=TRUE))
  cvp.Rsquared <- 1-var(pts.extpcv$prop_t - pts.extpcv$pcvpred, na.rm=TRUE)/var(pts.extpcv$prop_t, na.rm=TRUE)
  
  ## Back transformed: create pcvpred_bt even if not tranformed for cv.depth function
  pts.extpcv$prop <- pts.extpcv$sar_FINAL #                                            ########CHECK THIS :)
  
  ## Back transform CV data
  if(trans=="log10"){
    pts.extpcv$pcvpred_bt <- (10^(pts.extpcv$pcvpred))-0.1  
  }
  if(trans=="log"){
    pts.extpcv$pcvpred_bt <- (exp(pts.extpcv$pcvpred))-1 
  }
  if(trans=="sqrt"){
    pts.extpcv$pcvpred_bt <- (pts.extpcv$pcvpred)^2  
  }
  if(trans=="none"){
    pts.extpcv$pcvpred_bt <- pts.extpcv$pcvpred
  }
  
  
  cvp.RMSE_bt = sqrt(mean((pts.extpcv$prop - pts.extpcv$pcvpred_bt)^2, na.rm=TRUE))
  cvp.Rsquared_bt = 1-var(pts.extpcv$prop - pts.extpcv$pcvpred_bt, na.rm=TRUE)/var(pts.extpcv$prop, na.rm=TRUE)
  
  # #We're not using SCD points here obvi but we could use this for KSSL or NASIS analysis  
  # ## PCV stats for scd points
  # pts.extpcv.scd <- subset(pts.extpcv, pts.extpcv$tid == "scd")
  # cvp.RMSE.scd <- sqrt(mean((pts.extpcv.scd$prop_t - pts.extpcv.scd$pcvpred)^2, na.rm=TRUE))
  # cvp.Rsquared.scd <- 1-var(pts.extpcv.scd$prop_t - pts.extpcv.scd$pcvpred, na.rm=TRUE)/var(pts.extpcv.scd$prop_t, na.rm=TRUE)
  # ## PCV stats for scd points: backtransformed
  # cvp.RMSE.scd_bt <- sqrt(mean((pts.extpcv.scd$prop - pts.extpcv.scd$pcvpred_bt)^2, na.rm=TRUE))
  # cvp.Rsquared.scd_bt <- 1-var(pts.extpcv.scd$prop - pts.extpcv.scd$pcvpred_bt, na.rm=TRUE)/var(pts.extpcv.scd$prop, na.rm=TRUE)
  # ## Number of SCD samples
  # n_scd <- length(pts.extpcv.scd[,1])
  
  
  ## RPI NO NEED TO DO THESE... SEE IF STATEMENTS BELOW
  # #backtransform (or exp()-1):
  # pts.extpcv$prop_bt <- (pts.extpcv$prop_t)^2 # UPDATE: backtransform if necessary. Used for PICP and to characterize backtransformation bias
  # pts.extpcv$pcvpredpre.025_bt <- (pts.extpcv$pcvpredpre.025)^2 # UPDATE: backtransform if necessary
  # pts.extpcv$pcvpredpre.975_bt <- (pts.extpcv$pcvpredpre.975)^2 # UPDATE: backtransform if necessary
  
  # #don't backtransform:
  # pts.extpcv$prop_bt <- pts.extpcv$prop_t
  # pts.extpcv$pcvpredpre.025_bt <- pts.extpcv$pcvpredpre.025
  # pts.extpcv$pcvpredpre.975_bt <- pts.extpcv$pcvpredpre.975
  
  
  ## Back transform quantiles
  if(trans=="log10"){
    pts.extpcv$prop_bt <- (10^(pts.extpcv$prop_t))-0.1  
    pts.extpcv$pcvpredpre.025_bt <- (10^(pts.extpcv$pcvpredpre.025))-0.1 
    pts.extpcv$pcvpredpre.975_bt <- (10^(pts.extpcv$pcvpredpre.975))-0.1 
  }
  if(trans=="log"){
    pts.extpcv$prop_bt <- (exp(pts.extpcv$prop_t))-1  
    pts.extpcv$pcvpredpre.025_bt <- (exp(pts.extpcv$pcvpredpre.025))-1 
    pts.extpcv$pcvpredpre.975_bt <- (exp(pts.extpcv$pcvpredpre.975))-1 
  }
  if(trans=="sqrt"){
    pts.extpcv$prop_bt <- (pts.extpcv$prop_t)^2  
    pts.extpcv$pcvpredpre.025_bt <- (pts.extpcv$pcvpredpre.025)^2 
    pts.extpcv$pcvpredpre.975_bt <- (pts.extpcv$pcvpredpre.975)^2 
  }
  
  
  
  pts.extpcv$abs.resid <- abs(pts.extpcv$prop - pts.extpcv$pcvpred_bt)
  pts.extpcv$RPI <- (pts.extpcv$pcvpredpre.975_bt - pts.extpcv$pcvpredpre.025_bt)/varrange
  # plot(pts.extpcv$abs.resid~pts.extpcv$RPI) # Quick look at relationship
  ## Summarize RPI and residuals
  pts.extpcv$rel.abs.resid <- pts.extpcv$abs.resid/varrange
  RPI.cvave <- mean(pts.extpcv$RPI)
  RPI.cvmed <- median(pts.extpcv$RPI)
  rel.abs.res.ave <- mean(pts.extpcv$rel.abs.resid)
  rel.abs.res.med <- median(pts.extpcv$rel.abs.resid)
  pts.extpcv$BTbias <- pts.extpcv$prop_bt - pts.extpcv$prop
  BTbias.abs.max <- max(abs(pts.extpcv$BTbias))
  BTbias.ave <- mean(pts.extpcv$BTbias)
  PICP <- sum(ifelse(pts.extpcv$prop_bt <= pts.extpcv$pcvpredpre.975_bt & pts.extpcv$prop_bt >= pts.extpcv$pcvpredpre.025_bt,1,0))/length(pts.extpcv[,1])
  pts.n <- length(pts.extd$sar_FINAL)
  # new:
  proponly <- na.omit(as.numeric(pts.ext$sar_FINAL))
  prop.ave <- mean(proponly)
  prop.med <- median(proponly)
  pts.n <- length(proponly)
  prop.min <- min(proponly)
  prop.max <- max(proponly)
  
  stats <- table(pts.ext$sar_tier)
  
  ## Create PCV table
  # if you do analysis by data source ("scd"), add it here
  CVdf <- data.frame(d,cvp.RMSE, cvp.Rsquared, cvp.RMSE_bt, cvp.Rsquared_bt, 
                     RPI.cvave, RPI.cvmed,PICP,rel.abs.res.ave,rel.abs.res.med,
                     BTbias.abs.max,BTbias.ave,trans,
                     prop.ave,prop.med,pts.n,prop.min,prop.max)
  names(CVdf) <- c("d","cvp.RMSE","cvp.Rsquared","cvp.RMSE_bt", "cvp.Rsquared_bt", 
                   "RPI.CVave","RPI.CVmed","PICP","rel.abs.res.ave","rel.abs.res.med",
                   "BTbias.abs.max","BTbias.ave","transf",
                   "prop.ave (all d)","prop.med","pts.n","prop.min","prop.max")
  CVdf <- cbind(CVdf, stats)
  
  write.table(CVdf, paste(savefolder,property,"_",d,"cm_",version,"_ModelCV.csv",
                          sep=""), sep = "\t", row.names = FALSE)
  
  
  # # plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt)
  # # plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt, xlim=c(0,10),ylim=c(0,10))
  # # plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt, xlim=c(0,5),ylim=c(0,5))
  # # plot(pts.extpcv$prop~pts.extpcv$pcvpred_bt, xlim=c(0,1),ylim=c(0,1))
  # # plot(pts.extpcv$prop_t~pts.extpcv$pcvpred)
  # # lines(x1,y1, col = 'red')#1:1 line
  # # ## CV plots
  # # viri <- c("#440154FF", "#39568CFF", "#1F968BFF", "#73D055FF", "#FDE725FF") # color ramp
  # # gplt.dcm.2D.CV <- ggplot(data=pts.extpcv, aes(prop_t, pcvpred)) +
  # #   stat_binhex(bins = 30) + geom_abline(intercept = 0, slope = 1,lwd=1)  + #xlim(0,100) + ylim(0,100) + 
  # #   theme(axis.text=element_text(size=8), legend.text=element_text(size=10), axis.title=element_text(size=10),plot.title = element_text(size=10,hjust=0.5)) + 
  # #   xlab("Measured") + ylab("CV Prediction") + scale_fill_gradientn(name = "log(Count)", trans = "log", colours = rev(viri)) +
  # #   ggtitle(paste("Cross val", prop, d, "cm",sep=" "))
  # # gplt.dcm.2D.CV
  
  # Save Cross validation graph and data for future plotting
  # saveRDS(pts.extpcv, paste(prop, "cvlm_preds_2D", d, "cm_nasisSSURGO_ART_SG100.rds", sep="_"))
  saveRDS(pts.extpcv,paste(savefolder,property,"_",d,"cm_",version,"_ModelCVobj.rds",sep=""))
  
  
  
  
  print(paste("Depth ",d," done at ",Sys.time()," after ",elapsed))
}






#END 



##############################################################





