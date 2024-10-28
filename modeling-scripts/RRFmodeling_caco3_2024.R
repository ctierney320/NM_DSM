#### Ranger Random Forest Modeling Carbonate Equivalents
#### Charlotte Tierney
#### created: September 2022
#### last edit: April 2024

############################# 1. Set up #################################

required.packages <- c("terra","ranger","plyr","raster", "sp", "rgdal", 
                       "randomForest", "quantregForest","snowfall", "tidyr",
                       "dplyr", "ggplot2","hexbin","beepr","parallel") # might need "snow", "snowfall",
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)
# memory.limit(500000)
terraOptions(memfrac=0.9) #, memmax=3)


######## Inputs & Outputs 
## Save folder
savefolder <- "data/" #"final/", "processed/", or "GIS/" #old: "D:/CPT_work/NM_DSM/NM_predictions/Final/"

## Pedon point data
pedons <- read.csv(paste(savefolder,"processed/pedons_ALL_est.csv",sep=""))#"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/PEDONS_carbonates.csv") #must be csv, rds requires more manipulation

## Covariate raster data
covfolder <- "D:/CPT_work/NM_DSM/COVS_compressed/" #"G:/DSM_TWN/covar_sets/NM_compressed/" #NOT on git
covs <-list.files(path = covfolder, pattern=".tif$", full.names = T, recursive = F)
# covfolder2 <- "G:/DSM_TWN/covar_sets/NM_compressed/" #for GDAL error testing 1/18/24
# covs2 <-list.files(path = covfolder2, pattern=".tif$", full.names = T, recursive = F)

## Snap raster for CRS info
snaprast <- raster("D:/CPT_work/NM_DSM/COVS_compressed/ca_mosaic.tif") #grab snap raster #backed up on G:/ drive
ProjectCRS <- snaprast@crs@projargs #crs(snaprast) #sometimes crs() works

## Study Extent shapefile
ProjExtent <- readOGR(paste(savefolder,"GIS/nm_dsm_NMextent_reprojected.shp",sep=""))#"G:/DSM_TWN/covar_sets/NM_extentSHP/nm_dsm_NMextent_reprojected.shp")
spTransform(ProjExtent,CRSobj=ProjectCRS) 

## Property information
property <- "CaCO3"    #common name for file saving
prop <- "caco3_FINAL" #column name in pedon table
tier <- "caco3_tier" #column name for property tier info
version <- "Fullqt240326" #version name for file saving #full dataset, quantiles, transformed, 2024

trans <- "sqrt" #transformation type for table, around line 170


#### 1.e GRAB CARBONATE DATA *if needed*
# Wantedcaco3 <- c("pedon_id","utm_east","utm_north","topdepth_cm","botdepth_cm",
#                  "caco3_FINAL","caco3_tier") #
# ALL_caco3 <- subset(pedons, select = Wantedcaco3)
# ALL_caco3 <- ALL_caco3 %>% drop_na(c("utm_east","utm_north","caco3_FINAL"))
# 
# coordinates(ALL_caco3) <- ~utm_east+utm_north #assign coordinates
# proj4string(ALL_caco3) <- CRS(projargs=ProjectCRS)
# plot(ProjExtent)
# plot(ALL_caco3, add=TRUE)
# 
# ALL_caco3 <- as.data.frame(ALL_caco3)
# 
# saveRDS(ALL_caco3,paste(savefolder,"processed/","CaCO3_Pedons.rds",sep=""))#"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PEDONS_caco3.rds")
# write.csv(ALL_caco3,paste(savefolder,"processed/","CaCO3_Pedons.csv",sep=""))#"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PEDONS_caco3.csv")
# 
# pedons <- ALL_caco3
# rm(ALL_caco3)




### READ IN ALL THE FINISHED FILES if you've done this already
# pedons <- readRDS(paste(savefolder,"processed/CaCO3_Pedons.rds",sep=""))

# # d <- 0
# version2 <- "Fullqt24TEST" #the version where things were most recently made, for loading in objects

# pts.ext <- readRDS(paste(savefolder,"final/",property,"_ExtractedPts_",version2,".rds",sep=""))#"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/CaCO3_Fullq_ExtractedPts.rds")
# # pts.extd <- readRDS(paste(savefolder,"final/",property,"_",d,"cm","_ExtractedPtsObj_",version2,".rds",sep=""))
# # ytrain <- c(as.matrix(pts.extd[c("transformed")])) #same with this
# # varrange <- as.numeric(quantile(pts.extd$transformed, probs=c(0.975), na.rm=T)-quantile(pts.extd$transformed, probs=c(0.025),na.rm=T)) ## TRANSFORMED




########################## 2. Extract Points  ##############################
#ProjectCRS2 <- as.character(ProjectCRS)
pedons <- readRDS(paste(savefolder,"processed/","CaCO3_Pedons.rds",sep=""))
pedons.v <- terra::vect(pedons, geom=c("utm_east", "utm_north"), crs=ProjectCRS) #crs=ProjectCRS@projargs) #make pedons into spatVector
pedons$ID <- seq.int(nrow(pedons))
pts.ext <- as.data.frame(pedons) #create Points Extracted (pts.ext) object


# i <- 102 #test
starttime <- Sys.time()
for (i in 1:length(covs)){ #1:length(covs)){ #3){ #103:106
  rast <- terra::rast(covs[i])
  pts.ext.indv <- terra::extract(rast, pedons.v)
  file.name <- covs[i] #rast@ptr[["filenames"]] #get file name for covariate column
  file.name <- gsub(".tif","", file.name)
  file.name <- gsub(".*/","",file.name)
  names(pts.ext.indv)[2] <- file.name 
  pts.ext <<- merge(pts.ext,pts.ext.indv, by="ID") #c("utm_east","utm_north"))
  print(paste("Done with",file.name,"at",Sys.time(),sep=" "))
}; #beep(sound=1)
endtime <- Sys.time()
print(endtime - starttime)
#1/18/24 note: ran wc2.0_bio_30s_15 (cov 102) from the G drive bc the U drive version was glitching in GDAL
#3/19/24 note: switched out cov 102 with G drive version, original version moved to "2024errors" folder

rm(pts.ext.indv,rast)#,starttime,endtime)

# #check if we need this clean up:
# pts.extd <- subset(pts.extd,select = -c(ca_mosaic.y))
# colnames(pts.extd)[colnames(pts.extd) == 'ca_mosaic.x'] <- 'ca_mosaic'
# pts.ext <- pts.ext[,! names(pts.ext) %in% c("X","optional")] #clean out cols that might not exist

## Save points
saveRDS(pts.ext, paste(savefolder,"final/",property,"_ExtractedPts_",version,".rds",sep=""))

# pts.ext <- readRDS(paste(savefolder,"final/",property,"_ExtractedPts_",version,".rds",sep=""))#pts.ext <- readRDS("data/final/CaCO3_ExtractedPts_Fullqt24TEST.rds")


# #### SUBSET TO JUST NEW MEXICO:
# only needed for gypsum testing
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
  # saveRDS(pts.ext.nm, paste(savefolder,property,"_",d,"cm","_ExtractedPts_NMonly_",version,".rds",sep=""))
  # pts.ext <- pts.ext.nm
}

############################### 3. Model  ######################################
#read in extracted points if you didn't just run them:
# pts.ext <- readRDS(paste(savefolder,"final/",property,"_ExtractedPts_",version2,".rds",sep=""))

pts.ext$caco3_FINAL <- as.numeric(pts.ext$caco3_FINAL)
# pts.ext <- na.omit(pts.ext$caco3_FINAL) #CHECK FOR NAs THOUGH
stats.all <- data.frame(matrix(ncol = 18, nrow = 0))
names(stats.all) <- c("d","cvp.RMSE","cvp.Rsquared","cvp.RMSE_bt", "cvp.Rsquared_bt", 
                 "RPI.CVave","RPI.CVmed","PICP","rel.abs.res.ave","rel.abs.res.med",
                 "BTbias.abs.max","BTbias.ave","transf",
                 "prop.ave","prop.med","pts.n","prop.min","prop.max")

#might need to re-rast vectorize?

#separate by depth
# depths <- c(0,5,15,30,60,100,150) #(0,5, #depths <- 0
depths <- c(100,150) #for April 2024
# d <- 0
detach("package:terra", unload = TRUE)
for(d in depths){
  print(paste("Depth",d,"started at",Sys.time(),sep=" "))
  pts.extd <- subset(pts.ext, as.numeric(pts.ext$topdepth_cm) <= d & as.numeric(pts.ext$botdepth_cm) > d) # subset to chosen depth
  
  covs.list <- covs #get file name for covariate column
  covs.list <- gsub(".tif","", covs.list)
  covs.list <- gsub(".*/","",covs.list)
  
  pts.extd2 <- pts.extd[c(covs.list,prop,tier)] #make table for stats later that has tier information
  pts.extd <- pts.extd[c(covs.list,prop)] ## create a specific list of dependent variable and covariate names to use 
  pts.extd <- na.omit(pts.extd)# Remove any record with NA's (in any column - be careful)
  
  #Transform data
  # pts.extd$transformed <- log(pts.extd$caco3_FINAL + 1) #gypsum, ec
  # pts.extd$transformed <- (exp(pts.extd$caco3_FINAL))-1 #ucrb caco3
  pts.extd$transformed <- sqrt(pts.extd$caco3_FINAL) #eh caco3
  # pts.extd$transformed <- (pts.extd$caco3_FINAL)^2 #deffo not
  
  pts.extd <- na.omit(pts.extd)
  
  saveRDS(pts.extd, paste(savefolder,"final/",property,"_",d,"cm","_ExtractedPtsObj_",version,".rds",sep=""))
  
  #### Ranger Random Forest:
  # xtrain <- as.matrix(pts.extd[c(covs.list)]) #you don't even need this
  
  # ## Not Transformed:
  # ytrain <- c(as.matrix(pts.extd[c(prop)]))
  # formula <- as.formula(paste(prop,'~',paste(covs.list, collapse="+"))) #uses non-transformed property col
  # varrange <- as.numeric(quantile(pts.extd$caco3_FINAL, probs=c(0.975), na.rm=T) - quantile(pts.extd$caco3_FINAL, probs=c(0.025),na.rm=T))
  
  
  ## Transformed
  ytrain <- c(as.matrix(pts.extd[c("transformed")])) #same with this
  varrange <- as.numeric(quantile(pts.extd$transformed, probs=c(0.975), na.rm=T)-quantile(pts.extd$transformed, probs=c(0.025),na.rm=T)) ## TRANSFORMED
  formula <- as.formula(paste("transformed",'~',paste(covs.list, collapse="+")))
  
  
  covs.stack.raster <- stack(covs) #build raster stack
  
  
  
  ################ Predictions 
  
  #Build model
  rangerQmodel <- ranger(formula, 
                         data=pts.extd, 
                         num.trees = 100, 
                         quantreg = T, 
                         importance = "permutation", 
                         write.forest = TRUE)
  
  saveRDS(rangerQmodel, paste(savefolder,"final/",property,"_",d,"cm","_RangerModelObj_",version,".rds",sep=""))
  imp <- data.frame(var=names(rangerQmodel$variable.importance),imp_meas = unname(rangerQmodel$variable.importance))
  imp[order(imp$imp_meas, decreasing = T),][1:30,]

  ##Parallelize Predictions
  rasterOptions(maxmemory = 1e+09, chunksize = 1e+08)
  beginCluster(30,type='SOCK')

  predfun <- function(model, ...) predict(model, ...)$predictions
  start <- Sys.time()
  #standard prediction:
  pred <- clusterR(covs.stack.raster, predict,
                   args=list(model=rangerQmodel, fun=predfun),
                   progress="text")
  #uncertainty predictions:
  predl <- clusterR(covs.stack.raster, predict,
                    args=list(model=rangerQmodel, fun=predfun,type = "quantiles", quantiles = c(0.025)),
                    progress="text")
  # Sys.time()
  predh <- clusterR(covs.stack.raster, predict,
                    args=list(model=rangerQmodel, fun=predfun,type = "quantiles", quantiles = c(0.975)),
                    progress="text")

  # Sys.time()

  ### Backtransform if needed
  bt.fn <- function(x) {
    # ind <- (exp(x))-1   #If a backtransform is needed. 10^(x) or exp(x) or ^2
    ind <- (x)^2
    return(ind)
  }

  pred_bt <- clusterR(pred, calc, args=list(fun=bt.fn),progress='text')
  predh_bt <- clusterR(predh, calc, args=list(fun=bt.fn),progress='text')
  predl_bt <- clusterR(predl, calc, args=list(fun=bt.fn),progress='text')

  end <- Sys.time()
  endCluster()

  elapsed <- end-start

  #Save everything

  # ## Not transformed:
  # writeRaster(pred,overwrite=TRUE,
  #             filename=paste(savefolder,"final/preds/",property,"_",d,"cm","_Prediction_",version,".tif",sep=""),
  #             options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
  # writeRaster(predl,overwrite=TRUE,
  #             filename=paste(savefolder,"final/preds/",property,"_",d,"cm","_Prediction_l_",version,".tif",sep=""),
  #             options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
  # writeRaster(predh,overwrite=TRUE,
  #             filename=paste(savefolder,"final/preds/",property,"_",d,"cm","_Prediction_h_",version,".tif",sep=""),
  #             options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
  #
  # saveRDS(pred, paste(savefolder,"final/",property,"_",d,"cm","_PredObj.rds",sep=""))
  # saveRDS(predl, paste(savefolder,"final/",property,"_",d,"cm","_PredLObj.rds",sep=""))
  # saveRDS(predh, paste(savefolder,"final/",property,"_",d,"cm","_PredHObj.rds",sep=""))


  ## If transformed:
  writeRaster(pred_bt,overwrite=TRUE,
              filename=paste(savefolder,"final/preds/",property,"_",d,"cm","_Prediction_bt_",version,".tif",sep=""),
              options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
  writeRaster(predl_bt,overwrite=TRUE,
              filename=paste(savefolder,"final/preds/",property,"_",d,"cm","_Prediction_lbt_",version,".tif",sep=""),
              options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")
  writeRaster(predh_bt,overwrite=TRUE,
              filename=paste(savefolder,"final/preds/",property,"_",d,"cm","_Prediction_hbt_",version,".tif",sep=""),
              options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")

  saveRDS(pred, paste(savefolder,"final/",property,"_",d,"cm","_PredObj_",version,".rds",sep=""))
  saveRDS(pred_bt, paste(savefolder,"final/",property,"_",d,"cm","_PredBTobj_",version,".rds",sep=""))
  saveRDS(predl, paste(savefolder,"final/",property,"_",d,"cm","_PredLobj_",version,".rds",sep=""))
  saveRDS(predh, paste(savefolder,"final/",property,"_",d,"cm","_PredHobj_",version,".rds",sep=""))
  saveRDS(predl_bt, paste(savefolder,"final/",property,"_",d,"cm","_PredLBTobj_",version,".rds",sep=""))
  saveRDS(predh_bt, paste(savefolder,"final/",property,"_",d,"cm","_PredHBTobj_",version,".rds",sep=""))


  ############### Uncertainty

  # #If you don't have "varrange" objects already: load points data
  # # pts.extd <- readRDS(paste(savefolder,"final/","ExtractedPoints_0cm_CaCO3_Aug5NMqt.rds",sep=""))
  # varrange <- as.numeric(quantile(pts.extd$prop, probs=c(0.975), na.rm=T)-quantile(pts.extd$prop, probs=c(0.025),na.rm=T)) ## NOT TRANSFORMED
  # varrange <- as.numeric(quantile(pts.extd$transformed, probs=c(0.975), na.rm=T)-quantile(pts.extd$transformed, probs=c(0.025),na.rm=T)) ## TRANSFORMED

  # #read in predicted objects if needed
  # predh_bt <- readRDS(paste(savefolder,"final/","Predh_bt_object_0cm_CaCO3_Aug5NMqt.rds",sep=""))
  # predl_bt <- readRDS(paste(savefolder,"final/","Predl_bt_object_0cm_CaCO3_Aug5NMqt.rds",sep=""))


  ##RUN UNCERTAINTY:
  pred.uncert.data <- (predh_bt - predl_bt)/varrange #*100                  #####CHECK TRANSFORMATION (use predh_bt and predl_bt if so) #(predh - predl)/varrange
  # pred.uncert.data <- (predh - predl)/varrange #IF NOT TRANSFORMED
  # plot(pred.uncert.data)


  saveRDS(pred.uncert.data,paste(savefolder,"final/",property,"_",d,"cm","_ModUncertRPIdata_",version,".rds",sep=""))

  writeRaster(pred.uncert.data,overwrite=TRUE,
              filename=paste(savefolder,"final/preds/",property,"_",d,"cm","_ModUncertRPI_",version,".tif",sep=""),
              options=c("COMPRESS=DEFLATE", "TFW=YES"), progress="text")

  
  
  
  ################### 3.c Manual Cross validation ################################
  pts.extcvm <- pts.extd
  nfolds <- 10
  pts.extcvm$folds <- sample.int(nfolds,size =length(pts.extcvm[,1]),replace=T)
  
  # #if not transformed:
  # pts.extcvm$prop_t <- pts.extcvm$caco3_FINAL ## UPDATE: transform if needed else just create new version of prop #log(   +1) or sqrt()
  
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
  
  #### PCV statistics
  
  
  cvp.RMSE <- sqrt(mean((pts.extpcv$prop_t - pts.extpcv$pcvpred)^2, na.rm=TRUE))
  cvp.Rsquared <- 1-var(pts.extpcv$prop_t - pts.extpcv$pcvpred, na.rm=TRUE)/var(pts.extpcv$prop_t, na.rm=TRUE)
  
  ## Back transformed: create pcvpred_bt even if not tranformed for cv.depth function
  pts.extpcv$prop <- pts.extpcv$caco3_FINAL                                     ## UPDATE HERE FOR EVERY PROPERTY, TRANSFORM TYPE
  pts.extpcv$pcvpred_bt <- (pts.extpcv$pcvpred)^2  #^2   #(exp(x))-1            ## UPDATE EVERY TIME #BACKTRANSFORM HERE
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
  
  
  ## RPI
  #backtransform ((x)^2 or exp()-1)
  pts.extpcv$prop_bt <- (pts.extpcv$prop_t)^2 #^2 #exp()-1)                     # UPDATE: backtransform if necessary. Used for PICP and to characterize backtransformation bias
  pts.extpcv$pcvpredpre.025_bt <- (pts.extpcv$pcvpredpre.025)^2                 # UPDATE: backtransform if necessary
  pts.extpcv$pcvpredpre.975_bt <- (pts.extpcv$pcvpredpre.975)^2                 # UPDATE: backtransform if necessary
  
  # #don't backtransform:
  # pts.extpcv$prop_bt <- pts.extpcv$prop_t
  # pts.extpcv$pcvpredpre.025_bt <- pts.extpcv$pcvpredpre.025
  # pts.extpcv$pcvpredpre.975_bt <- pts.extpcv$pcvpredpre.975
  
  pts.extpcv$abs.resid <- abs(pts.extpcv$prop - pts.extpcv$pcvpred_bt)
  pts.extpcv$RPI <- (pts.extpcv$pcvpredpre.975_bt - pts.extpcv$pcvpredpre.025_bt)/varrange
  # plot(pts.extpcv$abs.resid~pts.extpcv$RPI) # Quick look at relationship
  ## Summarize RPI and residuals
  pts.extpcv$rel.abs.resid <- pts.extpcv$abs.resid/varrange #relative absolute residuals
  RPI.cvave <- mean(pts.extpcv$RPI)
  RPI.cvmed <- median(pts.extpcv$RPI)
  rel.abs.res.ave <- mean(pts.extpcv$rel.abs.resid) 
  rel.abs.res.med <- median(pts.extpcv$rel.abs.resid)
  pts.extpcv$BTbias <- pts.extpcv$prop_bt - pts.extpcv$prop
  BTbias.abs.max <- max(abs(pts.extpcv$BTbias))
  BTbias.ave <- mean(pts.extpcv$BTbias)
  PICP <- sum(ifelse(pts.extpcv$prop_bt <= pts.extpcv$pcvpredpre.975_bt & pts.extpcv$prop_bt >= pts.extpcv$pcvpredpre.025_bt,1,0))/length(pts.extpcv[,1])
  # new:
  proponly <- na.omit(as.numeric(pts.extd$caco3_FINAL))
  prop.ave <- mean(proponly)
  prop.med <- median(proponly)
  pts.n <- length(proponly)
  prop.min <- min(proponly)
  prop.max <- max(proponly)
  
  # stats <- table(pts.ext$caco3_tier)
  #this is so messy, sorry everyone
  stats.indv <- table(pts.extd2$caco3_tier)
  stats.indv.t <- as.data.frame(stats.indv) #t(as.matrix(stats.indv[-1]))
  stats.indv.t <- t(stats.indv.t) 
  colnames(stats.indv.t) <- stats.indv.t[1,]
  stats.indv.t <- stats.indv.t[2,]
  stats.indv.t <- as.data.frame(stats.indv.t)
  stats.indv.t <- t(stats.indv.t)
  
  ## Create PCV table
  # if you do analysis by data source ("scd"), add it here
  CVdf <- data.frame(d,cvp.RMSE, cvp.Rsquared, cvp.RMSE_bt, cvp.Rsquared_bt, 
                     RPI.cvave, RPI.cvmed,PICP,rel.abs.res.ave,rel.abs.res.med,
                     BTbias.abs.max,BTbias.ave,trans,
                     prop.ave,prop.med,pts.n,prop.min,prop.max)
  names(CVdf) <- c("d","cvp.RMSE","cvp.Rsquared","cvp.RMSE_bt", "cvp.Rsquared_bt", 
                   "RPI.CVave","RPI.CVmed","PICP","rel.abs.res.ave","rel.abs.res.med",
                   "BTbias.abs.max","BTbias.ave","transf",
                   "prop.ave","prop.med","pts.n","prop.min","prop.max")
  # CVdf <- cbind(CVdf, stats.indv)
  depth.stats <- cbind(CVdf, stats.indv.t)
  stats.all <<- rbind(stats.all, depth.stats)
 
  write.table(depth.stats, paste(savefolder,"final/",property,"_",d,"cm","_ModelCV_",version,".csv",
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
  
  # Save Cross validation data 
  saveRDS(pts.extpcv,paste(savefolder,"final/",property,"_",d,"cm","_ModelCVobj_",version,".rds",sep=""))
  
  
  
  print(paste("Depth ",d," done at ",Sys.time()," after ",elapsed))
}

write.table(stats.all, paste(savefolder,"final/",property,"_ALL_ModelCV_stats",version,".csv",
                               sep=""), sep = "\t", row.names = FALSE)

# rm(stats.all,stats.indv.t)


#END 



##############################################################





