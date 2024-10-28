#### Pedon data organization for SAR
#### Charlotte Tierney
#### July 2022
#### Last edit: January 2024




#### Workspace set up
required.packages <- c("raster","terra","sp", "rgdal", "caret",  
                       "dplyr", "ggplot2","hexbin",
                       "aqp","Hmisc","spatstat","maptools","DSMprops","RSQLite",
                       "lubridate","stringr","DBI","tidyr","readxl","tidyverse",
                       "naniar",'soilDB','mapview','sf','soiltexture','scales') 

new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)




#### Data locations


## Read in complete NMSU datasets
SiteData <- read.csv("data/preprocessed/legacy_site.csv")
HorizonData <- read.csv("data/preprocessed/legacy_horizon.csv")

#NASIS DATA:
datafolder <- "V:/PROJECTS/TRAVIS_NAUMAN/NM_DSM/Data_Spatial/Natl_Pedons/" #do i still need this?


savefolder <- "data/processed/" #full path: C:/Users/cptierney/OneDrive - DOI/2024_OneDrive/nm-dsm/ #old: C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/

snaprast <- terra::rast("data/GIS/ca_mosaic.tif") #grab snap raster 
ProjectCRS <- crs(snaprast)

ProjExtent <- readOGR("data/GIS/nm_dsm_NMextent_reprojected.shp") #old: "G:/DSM_TWN/covar_sets/NM_extentSHP/nm_dsm_NMextent_reprojected.shp"
plot(ProjExtent) #check
spTransform(ProjExtent,CRSobj=ProjectCRS) #make sure project extent shapefile matches CRS of the snapraster








### Clean up and join NMSU Sites and Horizons
## Select columns we need
# WantedSiteData <- c("utm_id","utm_east","utm_north","date",
#                     "classification","notes") #we want all cols now

WantedHorizonData <- c("Pedon_ID","UTM_ID","H_number",
                       "Horizon","topDepth_cm","bottomDepth_cm",
                       "texture","text_modifier","Gyp_meas","Gyp_vis",
                       "CaCO3_meas","CaCO3_vis","Eff.","clay","EC","notes",
                       "sand") #

#make new dataframes
SiteDataWANT <- SiteData #subset(SiteData, select = WantedSiteData)
HorizonDataWANT <- subset(HorizonData, select = WantedHorizonData)

#### CLEAN UP DATA 
##Run this again
na_strings <- c("N/A","n/a","na","-","ILLEGIBLE","illegible"," ","","'","`")
SiteDataWANT <- replace_with_na_all(SiteDataWANT, condition = ~.x %in% na_strings) 
HorizonDataWANT <- replace_with_na_all(HorizonDataWANT, condition = ~.x %in% na_strings)

##*****RUN THIS ONE OBJECT AT A TIME MANUALLY ### sorry
# table <- SiteDataWANT
# table <- HorizonDataWANT
if ("utm_id" %in% colnames(table)==TRUE){
  cell <- grep("illegible",table$utm_id,ignore.case=TRUE,value=TRUE) 
  cell <- union(cell,c(grep("missing",table$utm_id,ignore.case=TRUE,value=TRUE)))
  cell <- union(cell,c(grep("no",table$utm_id,ignore.case=TRUE,value=TRUE)))
  cell <- union(cell,grep("illegible",table$utm_east,ignore.case=TRUE,value=TRUE))
  cell <- union(cell,grep("missing",table$utm_east,ignore.case=TRUE,value=TRUE))
  cell <- union(cell,grep("illegible",table$utm_north,ignore.case=TRUE,value=TRUE))
  cell <- union(cell,grep("missing",table$utm_north,ignore.case=TRUE,value=TRUE))
  
  if (length(cell) != 0){
    table <- table %>% replace_with_na_at(.vars = c("utm_id","utm_east","utm_north"),
                                          condition = ~.x %in% cell)
  }
}
# SiteDataWANT <- table
# HorizonDataWANT <- table
# rm(table)


#delete rows with NA and organize headers
SiteDataWANT <- SiteDataWANT[rowSums(is.na(SiteDataWANT))<ncol(SiteDataWANT),] #removes empty rows
HorizonDataWANT <- HorizonDataWANT[rowSums(is.na(HorizonDataWANT))<ncol(HorizonDataWANT),]
HorizonDataWANT <- HorizonDataWANT %>% rename(utm_ID=UTM_ID)


PedonData <- inner_join(SiteDataWANT,HorizonDataWANT,by=c("utm_ID"))
PedonData <- distinct(PedonData) #remove duplicates

## Save new files
write.csv(PedonData,paste(savefolder,"legacy_pedons.csv",sep=""))
saveRDS(PedonData,paste(savefolder,"legacy_pedons.rds",sep=""))

# write.csv(SiteDataWANT, 
#           paste(savefolder,"NMDSM_PedonData_Sites_USABLE.csv",sep=""),
#           row.names = FALSE)
# write.csv(HorizonDataWANT, 
#           paste(savefolder,"NMDSM_PedonData_Horizons_USABLE.csv",sep=""),
#           row.names = FALSE)




##############################

# data <- PedonData #read.csv(paste(savefolder,"legacy_pedons.csv",sep=""))
# 
# 
# ## clean up columns
# data$date <- as.character(data$date) #no date info
# 
# #eff
# data$eff <- toupper(data$eff) #unique(PedonData$Eff.)
# data[data == "VSL"] <- "VS"
# 
# #texture
# data$texture <- toupper(data$texture) #unique(PedonData$texture)
# data[data == "STVE"] <- "ST"
# data[data == "CLS"] <- "LCOS"
# data[data == "CSL"] <- "COSL"
# 
# #clay 
# data[data =="illeligable"] <- NA #unique(PedonData$clay)
# data[data =="ineligible"] <- NA
# data[data =="> 40"] <- 40
# 
# #text_mod
# data$text_mod <- toupper(data$text_mod) #unique(PedonData$text_modifier) 
# 
# 
# # dataCLEAN <- data
# # data <- dataCLEAN
# 
# nmsudata <- data

data <- PedonData
nmsudata <- PedonData
#

################## Calculate estimated CaCO3 data ################## 
##for new 2024 data checks:
unique(data$texture)

## calcium carbonate equivalent %
for (i in 1:nrow(data)){
  
  ##add clay estimation column 
  data$clayest[i] <- data$clay[i]
  if (is.na(data$clay[i]) & !is.na(data$texture[i])){ #if no clay data but has texture class
    #sand, loamy sand, sandy loam, silt, silty loam, loam
    if (data$texture[i] == "S" |
        data$texture[i] == "LS" |
        data$texture[i] == "SL" |
        data$texture[i] == "SI" |
        data$texture[i] == "SIL" |
        data$texture[i] == "L" |
        data$texture[i] == "FSL" |
        data$texture[i] == "LFS"){
      data$clayest[i] <- 15 #set clay estimate based on texture
    }
    #sandy clay loam, clay loam, silty clay loam, silty clay, sandy clay, clay
    else if (data$texture[i] == "SCL" |
             data$texture[i] == "CL" |
             data$texture[i] == "SICL" |
             data$texture[i] == "SIC" |
             data$texture[i] == "SC" |
             data$texture[i] == "C"){
      data$clayest[i] <- 20 #set clay estimate based on texture
    }
    # else if (data$texture[i] == "PETROGYPSIC"){
    #   data$clayest[i] <- NA #set random textures to NA
    # }
    else {
      data$clayest[i] <- 15
    }
  }
  #with clay estimations, now look at CaCO3 
  if (is.na(data$clayest[i]) | is.na(data$eff[i]) | is.na(data$caco3_vis[i])){
    data$cce_l[i] <- NA
    data$cce_rv[i] <- NA
    data$cce_h[i] <- NA
    # print(paste(i," has NAs",sep=""))
  } 
  # LOW CLAY
  else if (data$clayest[i] < 18){
    # print(paste(i," row SHOULD WORK",sep=""))
    if (data$eff[i] =="NE" & data$caco3_vis[i] < 5){
      data$cce_l[i] <- 0
      data$cce_rv[i] <- 0
      data$cce_h[i] <- 2
    } 
    else if (data$eff[i]=="VS"|data$eff[i]=="SL" & data$caco3_vis[i] < 5){
      data$cce_l[i] <- 0
      data$cce_rv[i] <- 1
      data$cce_h[i] <- 2
    } 
    else if (data$eff[i]=="ST"|data$eff[i]=="VE" & data$caco3_vis[i] < 5){
      data$cce_l[i] <- 0
      data$cce_rv[i] <- 2
      data$cce_h[i] <- 4
    } 
    else if (data$eff[i]=="ST"|data$eff[i]=="VE" & data$caco3_vis[i] > 5){
      data$cce_l[i] <- 5
      data$cce_rv[i] <- 10
      data$cce_h[i] <- 15
    }
  }
  #HIGH CLAY
  else if (data$clayest[i] >= 18){
    # print(paste(i," is greater than 18% clay",sep=""))
    if (data$eff[i] =="NE" & data$caco3_vis[i] < 5){
      data$cce_l[i] <- 0
      data$cce_rv[i] <- 0
      data$cce_h[i] <- 2
    } 
    else if (data$eff[i]=="VS"|data$eff[i]=="SL" & data$caco3_vis[i] < 5){
      data$cce_l[i] <- 0
      data$cce_rv[i] <- 2
      data$cce_h[i] <- 4
    } 
    else if (data$eff[i]=="ST"|data$eff[i]=="VE" & data$caco3_vis[i] < 5){
      data$cce_l[i] <- 0
      data$cce_rv[i] <- 5
      data$cce_h[i] <- 10
    } 
    else if (data$eff[i]=="ST"|data$eff[i]=="VE" & data$caco3_vis[i] > 5){
      data$cce_l[i] <- 15
      data$cce_rv[i] <- 20
      data$cce_h[i] <- 25
    }
    else if (data$eff[i]=="ST"|data$eff[i]=="VE" & data$horizon[i]=="Bkk"){
      data$cce_l[i] <- 15
      data$cce_rv[i] <- 20
      data$cce_h[i] <- 25
      #print(paste(i," has Bkk horizon",sep=""))
    }
  } 
  
  ##REGARDLESS OF CLAY
  if (is.na(data$horizon[i])){
    count <- 1
  }
  else if (data$horizon[i]=="Bkm" | data$horizon[i]=="Kkm"){
    data$cce_l[i] <- 40
    data$cce_rv[i] <- 50
    data$cce_h[i] <- 60
  }
  else if (data$horizon[i]!="Bkm"){
    count <- 1
  }
  else if(data$horizon[i]!="Kkm"){
    count <- 1
  }
  if (is.na(data$notes[i])){
    count <- 1
  }
  else if (grepl("petrocalcic",data$notes[i],ignore.case = T)){
    data$cce_l[i] <- 40
    data$cce_rv[i] <- 50
    data$cce_h[i] <- 60
  }
  
  ## SET UP TIERS
  if (is.na(data$caco3_meas[i]) & is.na(data$caco3_vis[i]) & is.na(data$cce_rv[i])){
    data$caco3_tier[i] <- NA
    data$caco3_FINAL[i] <- NA
  }
  else if (!is.na(data$caco3_meas[i])){
    data$caco3_tier[i] <- "field_lab"
    data$caco3_FINAL[i] <- data$caco3_meas[i]
  }
  else if (!is.na(data$caco3_meas[i]) & !is.na(data$caco3_vis[i])){
    data$caco3_tier[i] <- "field_lab"
    data$caco3_FINAL[i] <- data$caco3_meas[i]
  }
  else if (is.na(data$caco3_meas[i]) & !is.na(data$caco3_vis[i])){
    data$caco3_tier[i] <- "estimated" #visible
    data$caco3_FINAL[i] <- data$caco3_vis[i]
  }
  else if (!is.na(data$cce_rv[i]) & is.na(data$caco3_tier[i]) & is.na(data$caco3_FINAL[i])){
    data$caco3_tier[i] <- "estimated"
    data$caco3_FINAL[i] <- data$cce_rv[i]
  }
}

# ## Save if needed
# write.csv(data, 
#           "C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/NMDSM_PedonData_CaCO3_estimated.csv",
#           row.names = FALSE)


################## Calculate estimated Gypsum data ##################

for (i in 1:nrow(data)){
  if (is.na(data$gyp_vis[i])){
    data$gyp_est_l[i] <- NA
    data$gyp_est_rv[i] <- NA
    data$gyp_est_h[i] <- NA
    # print(paste(i," has NAs",sep=""))
    if (is.na(data$horizon[i]) | is.na(data$text_mod[i])){
      count <- 1
    }
    else if (grepl("yy",data$horizon[i],ignore.case = T)){
      data$gyp_est_l[i] <- 40
      data$gyp_est_rv[i] <- 70
      data$gyp_est_h[i] <- 100
    }
    else if (grepl("y",data$horizon[i],ignore.case = T) & data$text_mod[i]=="GYP"){
      data$gyp_est_l[i] <- 15
      data$gyp_est_rv[i] <- 28
      data$gyp_est_h[i] <- 40
    }
    if (is.na(data$texture[i]) | is.na(data$text_mod[i])){
      count <- 1
    }
    else if (data$texture[i]=="M" & data$text_mod[i]=="GYP"){
      data$gyp_est_l[i] <- 40
      data$gyp_est_rv[i] <- 70
      data$gyp_est_h[i] <- 100
    }
    if (is.na(data$notes[i])){
      count <- 1
    }
    else if (grepl("petrogypsic",data$notes[i],ignore.case = T)){
      data$gyp_est_l[i] <- 40
      data$gyp_est_rv[i] <- 70
      data$gyp_est_h[i] <- 100
    }
  } 
  #IF WE DO HAVE VISIBLE DATA
  else if (!is.na(data$gyp_vis[i])){
    if (data$gyp_vis[i] == 0){
      data$gyp_est_l[i] <- 0
      data$gyp_est_rv[i] <- 0
      data$gyp_est_h[i] <- 2
    } 
    else if (data$gyp_vis[i] < 1){
      data$gyp_est_l[i] <- 0
      data$gyp_est_rv[i] <- 1
      data$gyp_est_h[i] <- 2
    } 
    else if (data$gyp_vis[i] >= 1 & data$gyp_vis[i] < 2){
      data$gyp_est_l[i] <- 0
      data$gyp_est_rv[i] <- 2
      data$gyp_est_h[i] <- 4
    } 
    else if (data$gyp_vis[i] >= 2 & data$gyp_vis[i] < 5){
      data$gyp_est_l[i] <- 5
      data$gyp_est_rv[i] <- 10
      data$gyp_est_h[i] <- 15
    }
  }
}
# dataGYP <- data
# data <- dataGYP

for (i in 1:nrow(data)){
  ## SET UP TIERS
  if (is.na(data$gyp_meas[i]) & is.na(data$gyp_vis[i]) & is.na(data$gyp_est_rv[i])){
    data$gyp_tier[i] <- NA
    data$gyp_FINAL[i] <- NA
  } else if (!is.na(data$gyp_meas[i]) & !is.na(data$gyp_vis[i])){
    data$gyp_tier[i] <- "field_lab"
    data$gyp_FINAL[i] <- data$gyp_meas[i]
  } else if (is.na(data$gyp_meas[i]) & !is.na(data$gyp_vis[i])){
    data$gyp_tier[i] <- "estimated" #visible
    data$gyp_FINAL[i] <- data$gyp_vis[i]
  } else if (!is.na(data$gyp_meas[i]) & is.na(data$gyp_est_rv[i])){
    data$gyp_tier[i] <- "field_lab"
    data$gyp_FINAL[i] <- data$gyp_meas[i]
  } else if (!is.na(data$gyp_est_rv[i]) & is.na(data$gyp_meas[i])){ #& is.na(data$gyp_FINAL[i])
    data$gyp_tier[i] <- "estimated"
    data$gyp_FINAL[i] <- data$gyp_est_rv[i]
  }
}




# rename cols to match NASIS and KSSL processed data
nmsudata <- data
nmsudata <- nmsudata %>% rename(clay_FINAL=clay,
                                ec_FINAL=EC,
                                sand_FINAL=sand) #no SAR
nmsudata$ec_tier <- "NMSU"
nmsudata$sand_tier <- "NMSU"
nmsudata$clay_tier <- "NMSU"

saveRDS(nmsudata,paste(savefolder,"legacy_pedons_est.rds",sep="")) #legacy pedons with estimates


rm(data, PedonData)
## save here?

# ################## Grab CRS info ##################
# snaprast <- terra::rast("U:/2022/NM_DSM/ca_mosaic.tif") #grab snap raster 
# ProjectCRS <- crs(snaprast)
# 
# ProjExtent <- readOGR("G:/DSM_TWN/covar_sets/NM_extentSHP/nm_dsm_NMextent_reprojected.shp")
# plot(ProjExtent)
# spTransform(ProjExtent,CRSobj=ProjectCRS)



################## Prep NASIS Data ################## 
NASIS <- readRDS(paste(datafolder,"NASIS_all_component_horizon_match_SPC_ssurgo20.rds",sep=""))
nationaldataproj <- NASIS@sp@proj4string@projargs
load(paste(datafolder,"nasis_sites_20210325.RData",sep="")) #s

##grab date data
nationaldates <- as.data.frame(s$peiid)
nationaldates <- nationaldates %>% rename(pedon_id=`s$peiid`)
nationaldates$pedon_id <- as.character(nationaldates$pedon_id)
nationaldates$date <- s$obsdate
nationaldates$obskind <- s$obsdatekind

##grab site data
nationalsites <- as.data.frame(NASIS@site[["peiid"]])
nationalsites <- nationalsites %>% rename(pedon_id=`NASIS@site[["peiid"]]`)
nationalsites$utm_east <- NASIS@site[["x_std"]]
nationalsites$utm_north <- NASIS@site[["y_std"]]

##grab horizon data and properties
nationaldata <- as.data.frame(NASIS@horizons[["peiid"]])
nationaldata <- nationaldata %>% rename(pedon_id=`NASIS@horizons[["peiid"]]`)
nationaldata$horizon <- NASIS@horizons[["hzID"]]
nationaldata$topdepth_cm <- NASIS@horizons[["hzdept_r"]]
nationaldata$botdepth_cm <- NASIS@horizons[["hzdepb_r"]]
#properties:
nationaldata$caco3_FINAL<- NASIS@horizons[["caco3_r"]]
nationaldata$caco3_tier <- "NASIS"

nationaldata$gyp_FINAL <- NASIS@horizons[["gypsum_r"]]
nationaldata$gyp_tier <- "NASIS"

nationaldata$sar_FINAL<- NASIS@horizons[["sar_r"]]
nationaldata$sar_tier <- "NASIS"

nationaldata$ec_FINAL<- NASIS@horizons[["ec_r"]]
nationaldata$ec_tier <- "NASIS"

nationaldata$sand_FINAL<- NASIS@horizons[["sandtotal_r"]]
nationaldata$sand_tier <- "NASIS"

nationaldata$clay_FINAL<- NASIS@horizons[["claytotal_r"]]
nationaldata$clay_tier <- "NASIS"


nationalsites <- left_join(nationalsites, nationaldates)
nationaldata <- full_join(nationalsites, nationaldata)

nationaldata <- nationaldata %>% mutate(horizon = as.character(horizon))

##grab only data past 2000
nationaldata <- nationaldata[nationaldata$obskind == "actual site observation date",]
nationaldata <- nationaldata[nationaldata$date >= "2000-01-01",]


rm(nationalsites,NASIS,s,nationaldates)

##align CRS with CRS of NM data, subset to study area
nationaldata <- nationaldata[,colSums(is.na(nationaldata))<nrow(nationaldata)]
nationaldata <- nationaldata[rowSums(is.na(nationaldata))<ncol(nationaldata),] #removes empty rows
coordinates(nationaldata) <- ~utm_east+utm_north #assign coordinates
proj4string(nationaldata) <- CRS(projargs=nationaldataproj) #assign known CRS from data collection

nationaldata <- spTransform(nationaldata,CRSobj=ProjectCRS) # transform

nationaldata <- nationaldata[ProjExtent,] #subset to project extent
# mapview(nationaldata)

# writeOGR(nationaldata,dsn = "D:/CPT_work/NM_DSM/pedons_misc/NASISpedons.shp",
#                   layer = "NASISpedons2",
#                   driver = "ESRI Shapefile")



saveRDS(nationaldata, paste(savefolder,"national_pedons.rds",sep="")) # "C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/nationalpedons.rds")

# nationaldata <- readRDS("D:/CPT_work/NM_DSM/pedons_misc/nationalpedons.rds")




################## Prep KSSL Data ################## 
#### https://ncss-tech.github.io/lab-data-delivery/SDA.html

# setwd("D:/CPT_work/NM_DSM/NCSS_labpedons/KSSL_Export/")
setwd("G:/DSM_TWN/datasets/NMDSM30m/PreProcessed/KSSL_Export")

sql <- "
SELECT
pedon_key, site_key, pedlabsampnum, pedoniid, upedonid, longitude_decimal_degrees, latitude_decimal_degrees
FROM
lab_combine_nasis_ncss
;"

pedons <- SDA_query(sql)


#### Subset to study area!
pedons <- subset(pedons, subset = !is.na(longitude_decimal_degrees) & !is.na(latitude_decimal_degrees))
coordinates(pedons) <- ~longitude_decimal_degrees+latitude_decimal_degrees #assign coordinates
proj4string(pedons) <- CRS(projargs='EPSG:4326') #assign known CRS from data collection

pedons <- spTransform(pedons,CRSobj=ProjectCRS) # transform

pedons <- pedons[ProjExtent,] #subset to project extent
plot(pedons)



pedons.sf <- st_as_sf(pedons, 
                      coords = c('longitude_decimal_degrees', 'latitude_decimal_degrees'), 
                      crs = 4326)
# mapview(ProjExtent) + pedons.sf


#### Grab data we want
pedon.keys <- format_SQL_in_statement(pedons$pedon_key)

sql <- sprintf("
SELECT
l.pedon_key, l.labsampnum, hzn_top, hzn_bot, hzn_desgn,
sand_total, silt_total, clay_total, caco3_lt_2_mm,
caco3_lt_20_mm, gypsum_lt_20_mm, corrected_gypsum_lt_2_mm, 
sodium_absorption_ratio, electrical_conductivity_satx
FROM
lab_layer AS l
JOIN lab_physical_properties AS p ON l.labsampnum = p.labsampnum
JOIN lab_chemical_properties AS c ON l.labsampnum = c.labsampnum
WHERE l.pedon_key IN %s
ORDER BY l.pedon_key, hzn_top
;", pedon.keys)

hz <- SDA_query(sql)
pedons <- as.data.frame(pedons)

kssldata <- full_join(pedons, hz, by = "pedon_key")

kssldata <- st_as_sf(kssldata, 
                     coords = c('longitude_decimal_degrees', 'latitude_decimal_degrees'), 
                     crs = 5070)
# mapview(ProjExtent) + kssldata




#### NEXT: change column names to match NASIS and NMSU datasets

kssldata <- kssldata %>% rename(pedon_ID=pedoniid, #updated for new 2024 data
                                topDepth_cm=hzn_top, #updated for new 2024 data
                                botDepth_cm=hzn_bot, #updated for new 2024 data
                                Horizon=hzn_desgn,#updated for new 2024 data
                                clay_FINAL=clay_total,
                                gyp_FINAL=corrected_gypsum_lt_2_mm,
                                caco3_FINAL=caco3_lt_2_mm,
                                sar_FINAL=sodium_absorption_ratio,
                                ec_FINAL=electrical_conductivity_satx,
                                sand_FINAL=sand_total)

kssldata$pedon_id <- as.character(kssldata$pedon_ID)

kssldata$utm_id <- gsub("c(","",kssldata$geometry,fixed=T)
kssldata$utm_id <- gsub(")","",kssldata$utm_id,fixed=T)
kssldata <- separate(kssldata,utm_id,
                     into=c("utm_E","utm_N"),sep=",",
                     remove = FALSE)
kssldata$utm_east <- as.numeric(kssldata$utm_E) #updated for new 2024 data
kssldata$utm_north <- as.numeric(kssldata$utm_N) #updated for new 2024 data

kssldata$gyp_tier <- "KSSL"
kssldata$caco3_tier <- "KSSL"
kssldata$sar_tier <- "KSSL"
kssldata$ec_tier <- "KSSL"
kssldata$sand_tier <- "KSSL"
kssldata$clay_tier <- "KSSL"


rm(hz,pedons,pedons.sf)

setwd("C:/Users/cptierney/OneDrive - DOI/2024_OneDrive/nm-dsm/") #back to project folder ***WHEREVER GIT IS LINKED***

saveRDS(kssldata,paste(savefolder,"kssl_pedons.rds",sep="")) #"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/kssldata.rds")
# kssldata <- readRDS("D:/CPT_work/NM_DSM/pedons_misc/kssldata.rds")









################## Coordinate Sync & Join All ##################  
nationaldata <- readRDS(paste(savefolder,"national_pedons.rds",sep="")) #"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/nationalpedons.rds")
kssldata <- readRDS(paste(savefolder,"kssl_pedons.rds",sep="")) #"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/kssldata.rds")
nmsudata <- readRDS(paste(savefolder,"legacy_pedons.rds",sep="")) #"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/NM_pedons10.rds")

#### Coordinate sync NMSU data
# nmsudata <- read.csv("C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/NM_pedons_est.csv")



#regardless of when you loaded the NMSU data:
nmsudata$utm_north <- ifelse(nchar(nmsudata$utm_north) != 7,
                             NA,nmsudata$utm_north) #make sure utm_north has 7 characters
nmsudata$utm_east <- ifelse(nchar(nmsudata$utm_east) != 6,
                            NA,nmsudata$utm_east) #make sure utm_east has 6 characters
nmsudata <- nmsudata %>% drop_na(c("utm_east","utm_north"))

coordinates(nmsudata) <- ~utm_east+utm_north #assign coordinates
proj4string(nmsudata) <- CRS(projargs="+init=EPSG:26913") #assign known CRS from data collection
plot(nmsudata, col="blue") #, add=TRUE)

nmsudata <- spTransform(nmsudata,CRSobj=ProjectCRS) #reproject to Project CRS

## Quick fixes
nationaldata <- as.data.frame(nationaldata)

nmsudata <- as.data.frame(nmsudata)

st_geometry(kssldata) <- NULL

nationaldata[] <- lapply(nationaldata, as.character)
kssldata[] <- lapply(kssldata, as.character)
nmsudata[] <- lapply(nmsudata, as.character)

kssldata$utm_east <- as.numeric(kssldata$utm_east)
kssldata$utm_north <- as.numeric(kssldata$utm_north)
nationaldata$utm_east <- as.numeric(nationaldata$utm_east)
nationaldata$utm_north <- as.numeric(nationaldata$utm_north)
nmsudata$utm_east <- as.numeric(nmsudata$utm_east)
nmsudata$utm_north <- as.numeric(nmsudata$utm_north)


mapview(ProjExtent, color = "black") + mapview(nmsudata, col.regions = "white") + 
  mapview(kssldata, col.regions = "red") + mapview(nationaldata, col.regions = "green")

plot(ProjExtent)
plot(kssldata, col="red", add=TRUE)
plot(nationaldata, col="black", add=TRUE)
plot(nmsudata, col="blue", add=TRUE)


PedonDataAll <- bind_rows(nmsudata,nationaldata,kssldata)

saveRDS(PedonDataAll,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/PEDONS_all1.rds")
write.csv(PedonDataAll,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PEDONS_natlNASIS_nmsu.csv" )


# #### GRAB GYPSUM DATA
# WantedGypsum <- c("pedon_id","utm_east","utm_north","topdepth_cm","botdepth_cm",
#                   "gyp_FINAL","gyp_tier") #
# ALL_gypsum <- subset(PedonDataAll, select = WantedGypsum)
# ALL_gypsum <- ALL_gypsum %>% drop_na(c("utm_east","utm_north","gyp_FINAL"))
# 
# 
# coordinates(ALL_gypsum) <- ~utm_east+utm_north #assign coordinates
# proj4string(ALL_gypsum) <- CRS(projargs=ProjectCRS)
# plot(ProjExtent)
# plot(ALL_gypsum, add=TRUE)
# 
# saveRDS(ALL_gypsum,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PEDONS_gypsum.rds")
# write.csv(ALL_gypsum,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PEDONS_gypsum.csv")
# 
# 
# #### GRAB CARBONATE DATA
# Wantedcaco3 <- c("pedon_id","utm_east","utm_north","topdepth_cm","botdepth_cm",
#                  "caco3_FINAL","caco3_tier") #
# ALL_caco3 <- subset(PedonDataAll, select = Wantedcaco3)
# ALL_caco3 <- ALL_caco3 %>% drop_na(c("utm_east","utm_north","caco3_FINAL"))
# 
# coordinates(ALL_caco3) <- ~utm_east+utm_north #assign coordinates
# proj4string(ALL_caco3) <- CRS(projargs=ProjectCRS)
# plot(ProjExtent)
# plot(ALL_caco3, add=TRUE)
# 
# saveRDS(ALL_caco3,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PEDONS_caco3.rds")
# write.csv(ALL_caco3,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PEDONS_caco3.csv")


# #### GRAB SAR DATA
# Wantedsar <- c("pedon_id","utm_east","utm_north","topdepth_cm","botdepth_cm",
#                  "sar_FINAL","sar_tier") #
# ALL_SAR <- subset(PedonDataAll, select = Wantedsar)
# ALL_SAR <- ALL_SAR %>% drop_na(c("utm_east","utm_north","sar_FINAL"))
# 
# coordinates(ALL_SAR) <- ~utm_east+utm_north #assign coordinates
# proj4string(ALL_SAR) <- CRS(projargs=ProjectCRS)
# plot(ProjExtent)
# plot(ALL_SAR, add=TRUE)
# 
# saveRDS(ALL_SAR,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/PEDONS_SAR.rds")
# write.csv(ALL_SAR,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/PEDONS_SAR.csv")


#### GRAB EC DATA
WantedEC <- c("pedon_id","utm_east","utm_north","topdepth_cm","botdepth_cm",
               "ec_FINAL","ec_tier") #
ALL_EC <- subset(PedonDataAll, select = WantedEC)
ALL_EC <- ALL_EC %>% drop_na(c("utm_east","utm_north","ec_FINAL"))

coordinates(ALL_EC) <- ~utm_east+utm_north #assign coordinates
proj4string(ALL_EC) <- CRS(projargs=ProjectCRS)
plot(ProjExtent)
plot(ALL_EC, add=TRUE)
ALL_EC <- as.data.frame(ALL_EC)

saveRDS(ALL_EC,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/PEDONS_EC.rds")
write.csv(ALL_EC,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/PEDONS_EC.csv")


# #### GRAB SAND DATA
# Wantedsar <- c("pedon_id","utm_east","utm_north","topdepth_cm","botdepth_cm",
#                  "sar_FINAL","sar_tier") #
# ALL_SAR <- subset(PedonDataAll, select = Wantedsar)
# ALL_SAR <- ALL_SAR %>% drop_na(c("utm_east","utm_north","sar_FINAL"))
# 
# coordinates(ALL_SAR) <- ~utm_east+utm_north #assign coordinates
# proj4string(ALL_SAR) <- CRS(projargs=ProjectCRS)
# plot(ProjExtent)
# plot(ALL_SAR, add=TRUE)
# 
# saveRDS(ALL_SAR,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/PEDONS_SAR.rds")
# write.csv(ALL_SAR,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/PEDONS_SAR.csv")


# #### GRAB CLAY DATA
# Wantedsar <- c("pedon_id","utm_east","utm_north","topdepth_cm","botdepth_cm",
#                  "sar_FINAL","sar_tier") #
# ALL_SAR <- subset(PedonDataAll, select = Wantedsar)
# ALL_SAR <- ALL_SAR %>% drop_na(c("utm_east","utm_north","sar_FINAL"))
# 
# coordinates(ALL_SAR) <- ~utm_east+utm_north #assign coordinates
# proj4string(ALL_SAR) <- CRS(projargs=ProjectCRS)
# plot(ProjExtent)
# plot(ALL_SAR, add=TRUE)
# 
# saveRDS(ALL_SAR,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/PEDONS_SAR.rds")
# write.csv(ALL_SAR,"C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/PedonData/PEDONS_SAR.csv")
















