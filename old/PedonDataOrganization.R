#### Pedon Data organization
#### Charlotte Tierney
#### created: August 27, 2021
#### last edit: January 2022


#### Notes
## site table, coordinates, pedon level info
## child table (horizons), linked one-to-many to horizon table


#### Walkthrough
## Enter first pedon data folder at lines 28 
## Run through Line 410 ish
## Enter all pedon data folders at line 415-430ish, run functions, join, save
## Grab useful columns around line 446, join, save


required.packages <- c("readxl","tidyverse","stringr","rgdal","raster") 
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only=T)
rm(required.packages, new.packages)

memory.limit(500000)

#### SET UP ####
pedfolder <- "C:/Users/cptierney/New Mexico State University/Colby Brungard - LC_NRCS_completed/Gordon Files/"
ped.list <- list.files(path = pedfolder, pattern=".xlsx$", 
                       full.names = T, recursive = F)


#### Clean Up Functions ####
NameCleaning.fn <- function(table){ #MUST RUN MakeNA.fn() FIRST
  # table <- SiteData #for testing
  ## Deal with funky columns
  if ((("elevation_m" %in% colnames(table)) &
       ("Elevation_m" %in% colnames(table)))==TRUE){  
    table[table == "N/A"] <- NA
    table <- rename(table, elevation_m_lower = elevation_m)
    table$Elevation_m <- paste(table$Elevation_m,table$elevation_m_lower,sep="")
    table$Elevation_m <- gsub("NA","",table$Elevation_m)
    table$Elevation_m[table$Elevation_m == ""] <- NA
    table$elevation_m_lower <- NULL
  }
  
  ## Streamline formatting
  table <- table %>%
    rename_with(tolower) %>% #make all lowercase
    rename_all(~ gsub(" ","_", .)) #convert all spaces to underscores
  
  ## Coordinate columns
  if ("utm_zone" %in% colnames(table)==TRUE){  
    table <- table %>% rename(utm_zo = utm_zone) #rename so it doesn't catch on N or E
  }
  utm <- grep("utm",names(table),ignore.case=TRUE,value=TRUE)
  id <- grep("i",utm,ignore.case=TRUE,value=TRUE)
  north <- grep("n",utm,ignore.case=TRUE,value=TRUE)
  east <- grep("e",utm,ignore.case=TRUE,value=TRUE)
  if ("utm_id" %in% colnames(table)==FALSE){  
    table <- table %>% rename(utm_id = as.name(id)) 
  }
  if ("utm_north" %in% colnames(table)==FALSE){  
    table <- table %>% rename(utm_north = as.name(north)) 
  }
  if ("utm_east" %in% colnames(table)==FALSE){  
    table <- table %>% rename(utm_east = as.name(east)) 
  }
  if ("utm_zo" %in% colnames(table)==TRUE){  
    table <- table %>% rename(utm_zone = utm_zo) #re-rename so it makes sense again
  }
  
  ## Continue with other columns
  if ("elevation_meters" %in% colnames(table)==TRUE){
    table <- table %>% rename(elevation_m=elevation_meters) #new=old
  }
  if ("p_m" %in% colnames(table)==TRUE){
    table <- table %>% rename(parent_material=p_m) 
  }
  if ("pm" %in% colnames(table)==TRUE){
    table <- table %>% rename(parent_material=pm) 
  }
  if ("%_ground_cover" %in% colnames(table)==TRUE){
    table <- table %>% rename(ground_cover=as.character("%_ground_cover")) 
  }
  if ("%cover" %in% colnames(table)==TRUE){
    table <- table %>% rename(ground_cover=as.character("%cover")) 
  }
  if ("%cryptogamic_crust" %in% colnames(table)==TRUE){
    table <- table %>% rename(cryptogamic_crust=as.character("%cryptogamic_crust")) 
  }
  if ("%cryptogam_crust" %in% colnames(table)==TRUE){
    table <- table %>% rename(cryptogamic_crust=as.character("%cryptogam_crust")) 
  }
  if ("%cryptogram_crust" %in% colnames(table)==TRUE){
    table <- table %>% rename(cryptogamic_crust=as.character("%cryptogram_crust")) 
  }
  if ("geo._component" %in% colnames(table)==TRUE){
    table <- table %>% rename(geomorph_compo=as.character("geo._component"))
  }
  if ("geomorphic_comp" %in% colnames(table)==TRUE){
    table <- table %>% rename(geomorph_compo=geomorphic_comp)
  }
  if("surf_frag" %in% colnames(table)==TRUE){
    table <- table %>% rename(surface_frags=surf_frag)
  }
  if("surf_frags" %in% colnames(table)==TRUE){
    table <- table %>% rename(surface_frags=surf_frags)
  }
  if("up.down" %in% colnames(table)==TRUE){
    table <- table %>% rename(up_down=up.down)
  }
  if("up/down" %in% colnames(table)==TRUE){
    table <- table %>% rename(up_down=as.character("up/down"))
  }
  if("veg_note" %in% colnames(table)==TRUE){
    table <- table %>% rename(veg_notes=veg_note)
  }
  if("veg_codes" %in% colnames(table)==TRUE){
    table <- table %>% rename(veg_code=veg_codes)
  }
  if("mapunit" %in% colnames(table)==TRUE){
    table <- table %>% rename(map_unit=mapunit)
  }
  if("hillslope" %in% colnames(table)==TRUE){
    table <- table %>% rename(slope_position=hillslope)
  }
  if("hillslope_profile" %in% colnames(table)==TRUE){
    table <- table %>% rename(slope_position=hillslope_profile)
  }
  if("quad_n/ame" %in% colnames(table)==TRUE){
    table <- table %>% rename(topo_quad=as.character("quad_n/ame"))
  }
  if("quad_name" %in% colnames(table)==TRUE){
    table <- table %>% rename(topo_quad=quad_name)
  }
  # if("gps_id" %in% colnames(table)==TRUE){
  #   table <- table %>% rename(pedon_id=gps_id)
  # }
  if("name" %in% colnames(table)==TRUE){
    table <- table %>% rename(pedon_id=name)
  }
  if("w_p" %in% colnames(table)==TRUE){
    table <- table %>% rename(waypoint=w_p)
  }
  return(table)
}

H.NameCleaning.fn <- function(table){ #MUST RUN MakeNA.fn() FIRST
  # table <- HD HorizonData #for testing
  ## Streamline formatting
  table <- table %>%
    rename_with(tolower) %>% #make all lowercase
    rename_all(~ gsub(" ","_", .)) #convert all spaces to underscores
  
  ## Coordinate columns
  if ("utm_zone" %in% colnames(table)==TRUE){  
    table <- table %>% rename(utm_zo = utm_zone) #rename so it doesn't catch on N or E
  }
  utm <- grep("utm",names(table),ignore.case=TRUE,value=TRUE)
  id <- grep("i",utm,ignore.case=TRUE,value=TRUE)
  north <- grep("n",utm,ignore.case=TRUE,value=TRUE)
  east <- grep("e",utm,ignore.case=TRUE,value=TRUE)
  if ("utm_id" %in% colnames(table)==FALSE){  
    table <- table %>% rename(utm_id = as.name(id)) 
  }
  if (((length(north)!=0)==TRUE) &                         #if there is a utm north column AND
       ("utm_north" %in% colnames(table))==FALSE){         #the current utm north column is not named "utm_north"
    table <- table %>% rename(utm_north = as.name(north)) 
  }
  if (((length(east)!=0)==TRUE) & 
      ("utm_east" %in% colnames(table))==FALSE){  
    table <- table %>% rename(utm_east = as.name(east)) 
  }
  if ("utm_zo" %in% colnames(table)==TRUE){  
    table <- table %>% rename(utm_zone = utm_zo) #re-rename so it makes sense again
  }
  
  ## Continue with other columns
  # if ("gps_id" %in% colnames(table)==TRUE){
  #   table <- table %>% rename(pedon_id=gps_id) #new=old
  # }
  if ("name" %in% colnames(table)==TRUE){
    table <- table %>% rename(pedon_id=name) 
  }
  
  if ("text_modifier" %in% colnames(table)==TRUE){
    table <- table %>% rename(text_mod=text_modifier) 
  }
  if("w_p" %in% colnames(table)==TRUE){
    table <- table %>% rename(waypoint=w_p)
  }
  if("gyps_type" %in% colnames(table)==TRUE){
    table <- table %>% rename(gyp_type=gyps_type)
  }
  if("dry_cons" %in% colnames(table)==TRUE){
    table <- table %>% rename(dry_con=dry_cons)
  }
  if("dryconsistence" %in% colnames(table)==TRUE){
    table <- table %>% rename(dry_con=dryconsistence)
  }
  if("color_dry" %in% colnames(table)==TRUE){
    table <- table %>% rename(dry_color=color_dry)
  }
  if("drycolor" %in% colnames(table)==TRUE){
    table <- table %>% rename(dry_color=drycolor)
  }
  if("moist_cons" %in% colnames(table)==TRUE){
    table <- table %>% rename(moist_con=moist_cons)
  }
  if("moistconsistence" %in% colnames(table)==TRUE){
    table <- table %>% rename(moist_con=moistconsistence)
  }
  if("color_moist" %in% colnames(table)==TRUE){
    table <- table %>% rename(moist_color=color_moist)
  }
  if("moistcolor" %in% colnames(table)==TRUE){
    table <- table %>% rename(moist_color=moistcolor)
  }
  if("depth_inch" %in% colnames(table)==TRUE){
    table <- table %>% rename(depth_in=depth_inch)
  }
  if("eff." %in% colnames(table)==TRUE){
    table <- table %>% rename(eff=as.character("eff."))
  }
  if("efferves" %in% colnames(table)==TRUE){
    table <- table %>% rename(eff=efferves)
  }
  if("plas_con" %in% colnames(table)==TRUE){
    table <- table %>% rename(plasticity=plas_con)
  }
  if("stick_con" %in% colnames(table)==TRUE){
    table <- table %>% rename(stickiness=stick_con)
  }
  if("root" %in% colnames(table)==TRUE){
    table <- table %>% rename(roots=root)
  }
  
  return(table)
}

MakeNA.fn <- function(table){
  table[table == "N/A"] <- NA
  table[table == "n/a"] <- NA
  table[table == "na"] <- NA
  table[table == "-"] <- NA
  table[table == "ILLEGIBLE"] <- NA
  table[table == "illegible"] <- NA
  
  cell <- grep("illegible",table$utm_id,ignore.case=TRUE,value=TRUE) 
  cell <- union(cell,c(grep("missing",table$utm_id,ignore.case=TRUE,value=TRUE)))
  cell <- union(cell,grep("illegible",table$utm_east,ignore.case=TRUE,value=TRUE))
  cell <- union(cell,grep("missing",table$utm_east,ignore.case=TRUE,value=TRUE))
  cell <- union(cell,grep("illegible",table$utm_north,ignore.case=TRUE,value=TRUE))
  cell <- union(cell,grep("missing",table$utm_north,ignore.case=TRUE,value=TRUE))
  if (length(cell) > 0){
    table[table == cell] <- NA
  }
  
  return(table)
}

ColTypes.fn <- function(table){
  ## Create lists of columns that should be a certain data type
  cols.num <- c("utm_east","utm_north","elevation_m","elevation_ft","slope",
                "cyano","lichen","moss","crypto","salt","gyp","canopy","woody",
                "bare","cryptogramic_crust","t_number","w_p","t_number") #"ground_cover" #characters TO numeric
  cols.char <- c("pedon_id","aspect","ground_cover") #stay characters (if not already)
  
  ## Create lists based on current table's columns
  names <- colnames(table)
  nums <- intersect(cols.num, names) #columns that are in BOTH cols.num and table
  chars <- intersect(cols.char,names) 
  non_nums <- setdiff(names,nums) #columns that are in the table and don't need to be numeric
  
  ## Convert
  table <- table %>% 
    mutate_at(nums,as.numeric) %>%
    # mutate_at(chars,as.character) %>%
    mutate_at(non_nums,as.character)

  # #MAYBE LATER FIGURE OUT HOW TO CONVERT DATES NICELY
  # if ("date" %in% colnames(SD)==TRUE){
  #   SD <- SD %>% mutate(date==as.character(date))
  # }
  # #For now, just delete the date data
  
  return(table)
}

#### Data Set Up ####

#Site Data Set Up
#Load first known SD file (grab both sheets) and clean up names
SiteData <- read_excel(path=ped.list[[2]],sheet=1)
SiteData <- NameCleaning.fn(SiteData)
SiteData <- MakeNA.fn(SiteData)

SD2 <- read_excel(path=ped.list[[2]],sheet=2)
SD2 <- NameCleaning.fn(SD2)
SD2 <- MakeNA.fn(SD2)
SiteData <- full_join(SiteData,SD2)
SiteData$date <- NULL
rm(SD2)

#decide how we're mutating the columns today:
SiteData <- ColTypes.fn(SiteData)
# #OR
# SiteData <- SiteData %>% mutate_all(as.character)



##Horizon Data Set Up
#First, load first known HD file:
HorizonData <- read_excel(path=ped.list[[1]])
HorizonData <- H.NameCleaning.fn(HorizonData)
HorizonData <- MakeNA.fn(HorizonData)


#decide how we're mutating the columns today:
HorizonData <- ColTypes.fn(HorizonData)
#OR
# HorizonData <- HorizonData %>% mutate_all(as.character)


#### Functions for looping in the rest of the data, in multiple folders ####
SiteOrganization.fn <- function(folder){
  pedfolder <- folder #folder <- pedfolder2 3
  ped.list <- list.files(path = pedfolder, pattern=".xlsx$", 
                         full.names = T, recursive = F)
  #Loop Site Data:
  for (i in 1:length(ped.list)){ #30:50){ ##SHOULD BE 1:length(ped.list)){
    if (str_detect(ped.list[i],"SD")==TRUE){
      sn <- length(excel_sheets(ped.list[i]))
        for (n in 1:sn){
          SD <- read_excel(path=ped.list[[i]],sheet=n)
          SD$Date <- NULL
          SD$date <- NULL
          SD <- NameCleaning.fn(SD)
          SD <- MakeNA.fn(SD)
          
          #decide how we're mutating the columns today:
          SiteData <- ColTypes.fn(SiteData)
          SD <- ColTypes.fn(SD)
          # #OR
          # SD <- SD %>% mutate_all(as.character)
          
          SiteData <- suppressMessages(full_join(SiteData, SD))
          rm(SD)
        }
    }
  }
  #Delete columns of all NA values
  SiteData <- SiteData[,colSums(is.na(SiteData))<nrow(SiteData)] #removes empty columns
  SiteData <- SiteData[rowSums(is.na(SiteData))<ncol(SiteData),] #removes empty rows
  return(SiteData)
}

HorizonOrganization.fn <- function(folder){
  pedfolder <- folder #folder <- pedfolder1 32
  ped.list <- list.files(path = pedfolder, pattern=".xlsx$", 
                         full.names = T, recursive = F)

  #Loop Horizon Data:
  for (i in 1:length(ped.list)){
    if (str_detect(ped.list[i],"HD")==TRUE){
      HD <- read_excel(path=ped.list[[i]])
      
      HD$Date <- NULL
      HD$date <- NULL
      HD <- H.NameCleaning.fn(HD)
      HD <- MakeNA.fn(HD)
      HD <- ColTypes.fn(HD) #OR:
      # HD <- HD %>% mutate_all(as.character)
      
      HorizonData <- suppressMessages(full_join(HorizonData, HD)) 
    }
  }
  
  #More clean up
  HorizonData$utm_east <- gsub("'","",HorizonData$utm_east)
  
  table <- HorizonData
  cell <- grep("illegible",table$utm_id,ignore.case=TRUE,value=TRUE) #maybe make this apply to all?
  cell <- union(cell,c(grep("missing",table$utm_id,ignore.case=TRUE,value=TRUE)))
  cell <- union(cell,grep("illegible",table$utm_east,ignore.case=TRUE,value=TRUE))
  cell <- union(cell,grep("missing",table$utm_east,ignore.case=TRUE,value=TRUE))
  cell <- union(cell,grep("illegible",table$utm_north,ignore.case=TRUE,value=TRUE))
  cell <- union(cell,grep("missing",table$utm_north,ignore.case=TRUE,value=TRUE))
  if (length(cell) > 0){
    table[table == cell] <- NA
  }
  HorizonData <- table
  HorizonData <- ColTypes.fn(HorizonData)
  
  #Delete columns of all NA values
  HorizonData <- HorizonData[,colSums(is.na(HorizonData))<nrow(HorizonData)]
  HorizonData <- HorizonData[rowSums(is.na(HorizonData))<ncol(HorizonData),] #removes empty rows
  return(HorizonData)
} 

Check.fn <- function(site,horizon){
  #check, should have same number of unique values
  print(length(unique(site$pedon_id)))
  print(length(unique(horizon$pedon_id)))
  
  #clear out objects for next folder
  rm(pedfolder, envir = globalenv())
  rm(ped.list, envir = globalenv())
}



#### Run Folders
##First folder
pedfolder1 <- "C:/Users/cptierney/New Mexico State University/Colby Brungard - LC_NRCS_completed/Gordon Files/"
SiteData1 <- SiteOrganization.fn(pedfolder1)
HorizonData1 <- HorizonOrganization.fn(pedfolder1)
Check.fn(SiteData1,HorizonData1)

##Second Folder
pedfolder2 <- "C:/Users/cptierney/New Mexico State University/Colby Brungard - LC_NRCS_completed/Greg's Office/"
SiteData2 <- SiteOrganization.fn(pedfolder2)
HorizonData2 <- HorizonOrganization.fn(pedfolder2)
Check.fn(SiteData2,HorizonData2)

#Third Folder
pedfolder3 <- "C:/Users/cptierney/New Mexico State University/Colby Brungard - LC_NRCS_completed/Holloman Perimeter/"
SiteData3 <- SiteOrganization.fn(pedfolder3)
HorizonData3 <- HorizonOrganization.fn(pedfolder3)
Check.fn(SiteData3,HorizonData3)


#Join
#***

#### Save data
write.csv(SiteData, 
            "C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/NMDSM_PedonData_Sites_DRAFT3.csv",
            row.names = FALSE)
write.csv(HorizonData, 
            "C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/NMDSM_PedonData_Horizons_DRAFT3.csv",
            row.names = FALSE)




############ Make It Usable ############ 

## Select columns we need
WantedSiteData <- c("pedon_id","utm_id","utm_east","utm_north","class","notes")

WantedHorizonData <- c("pedon_id","utm_id","utm_east","utm_north","h_number",
                       "horizon","topdepth_cm","botdepth_cm","texture",
                       "text_mod","gyp_meas","gyp_vis","gyp_type","caco3_meas",
                       "caco3_vis","eff","clay","ec","notes") #

#make new dataframes
SiteData2 <- subset(SiteData, select = WantedSiteData)
HorizonData2 <- subset(HorizonData, select = WantedHorizonData)

## Check data cleanliness 
#delete rows with NA coordinates
SiteData2 <- SiteData2[rowSums(is.na(SiteData2))<ncol(SiteData2),] #removes empty rows
HorizonData2 <- HorizonData2[rowSums(is.na(HorizonData2))<ncol(HorizonData2),]
HorizonData2 <- HorizonData2 %>% rename(notes_h=notes)
#clean col types again for some reason
HorizonData2 <- ColTypes.fn(HorizonData2)


## Join tables based on pedon_id 
JoinedPedonData <- suppressMessages(inner_join(SiteData2,HorizonData2))
#Delete columns of all NA values
JoinedPedonData <- JoinedPedonData[,colSums(is.na(JoinedPedonData))<nrow(JoinedPedonData)]
JoinedPedonData <- JoinedPedonData[rowSums(is.na(JoinedPedonData))<ncol(JoinedPedonData),] #removes empty rows
#remove repeats
#check join method

## Clean joined data
#if utm_id is NA remove
#if utm_east or?? utm_north
#remove repeat columns 



## Save new files
write.csv(SiteData2, 
          "C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/NMDSM_PedonData_Sites_USABLE2.csv",
          row.names = FALSE)
write.csv(HorizonData2, 
          "C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/NMDSM_PedonData_Horizons_USABLE4.csv",
          row.names = FALSE)
write.csv(JoinedPedonData, 
          "C:/Users/cptierney/OneDrive - DOI/2022_OneDrive/NMDSM_PedonData_JOINED1.csv",
          row.names = FALSE)





data <- readRDS("V:/PROJECTS/TRAVIS_NAUMAN/NM_DSM/Data_Spatial/Natl_Pedons/NASIS_all_component_horizon_match_SPC_ssurgo20.rds")







############ Nice! ############ 
