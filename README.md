# Welcome to New Mexico Digital Soil Mapping
- Charlotte Tierney cptierney@usgs.gov or ctierney320@gmail.com
- Project created: 2021
- Repository created: December 2023
- Last update: April 2024

##
This Git hosts code and point data for the New Mexico Digital Soil Mapping project. The project uses pedon data from three sources (Legacy data, KSSL, and NASIS) and 106 rasters* to build a random forest model that predicts the values of six soil properties at seven depths. 

Predicted properties are gypsum, calcium carbonate equivalents, electrical conductivity, sodium absorption ratio, sand, and clay. Predictions are modeled at 0, 5, 15, 30, 60, 100, and 150cm depths.

Code was developed with the help of Travis Nauman (travis.nauman@usda.gov).

## Project status
- [x] compile raster covariates
- [x] compile pedon training data
- [ ] predict maps for each property, at each depth (see table below)
- [ ] mask and compress raster files 
- [ ] side task for Travis: transfer GCP files nm_dsm_exp/NM_DSM/NMDSMactiveCopy/Processed to G:\Base Layers\Soils\NM_30m_mc_2022

#### 2024 Predictions Rendered: 
x = predicted<br/>
/ = masked, compressed, saved to network
| | 0 | 5 | 15 | 30 | 60 | 100 | 150cm |
| ------ | ------ |------ | ------ |------ | ------ |------ | ------ |
|CaCO3| x | x | x | x | x |  |  |
|Gypsum| |  |  |  |  |  |  |
|EC| |  |  |  |  |  |  |
|Sand| |  |  |  |  |  |  |
|Clay| |  |  |  |  |  |  |

Table last updated on 4/6/2024

- Finished predictions for carbonates 0-60cm are saved here: C:\CPT_WORK\nm-dsm_cpt\data\GIS\caco3final
- as of 3/26/2024, models are being redone with new legacy data
- ~~as of 3/6/2024, all the prediction code for "2_RRFmodeling_..." files is commented out to allow the workflow to focus on getting cross validation staticstics.~~ 
- SAR was not included in the legacy data and did not need to be updated in 2024. Final versions of the SAR predictions are saved to G:/Base Layers/Soils/NM_30m_mc_2022


## To run the code for each property:
You need...
- Ranger Random Forest [script for each property](https://code.usgs.gov/sbsc/duniway-lab/dart-dsm/nm-dsm/-/tree/main/modeling-scripts)
- Shapefile of [project study area](https://code.usgs.gov/sbsc/duniway-lab/dart-dsm/nm-dsm/-/tree/main/data/GIS/nm_dsm_NMextent_reprojected.shp)
- Folder of covariates (saved locally)
- Snap raster from covariate stack (first covariate, ca_mosaic.tif) (saved locally)
- [Pedon data](https://code.usgs.gov/sbsc/duniway-lab/dart-dsm/nm-dsm/-/blob/main/data/processed/pedons_ALL_est.csv) from all sources

Make sure:
- Confirm in the gitignore that the **savefolder** location (specifically the raster outputs) is not going to be pushed to the git. Rasters are set to save in their own folder to avoid large files being pushed.
- The transformation math is correct, backtransformation is happening, the CV uses the right transformation objects. Each property's script is tailored to its transformation method, but there might be errors.
- The **depths** object before the loop reflects what you need to render
- After every depth or property is completed, copy all files to the G:/ drive
- Update the **version** string to reflect what you're doing (date, testing, transfroming, final, etc.)

### After Predictions are done
The original prediction and RPI maps were masked above 3000m and where there is surface water, then compressed to 16-bit files. The combined code for these processes is [here](https://code.usgs.gov/sbsc/duniway-lab/dart-dsm/nm-dsm/-/blob/main/post-processing).




## NM DSM Project History
July 2021
- Multivariate Random Forest Modeling tests. Saved to the V:/ drive here:  V:\PROJECTS\TRAVIS_NAUMAN\NM_DSM\Analysis\MultiResponse_Tests

September 2021 
- GEE work for downloading covariates is saved [here](https://code.usgs.gov/sbsc/duniway-lab/dart-dsm/nm-dsm/-/tree/main/pre-processing/earth-engine).
(Requires [study area shapefile](https://code.usgs.gov/sbsc/duniway-lab/dart-dsm/nm-dsm/-/tree/main/data/GIS).)

July 2022
- Stacking and compression of covariates from different sources. Code is [here](https://code.usgs.gov/sbsc/duniway-lab/dart-dsm/nm-dsm/-/tree/main/pre-processing/stack_clip). 
- Processed *rasters are not stored on GitLab, but are accessible on the G:/ drive and locally on the D:/ drive of WM1701


Fall 2022
- Ranger Random Forest modeling tests begin
- [Pedon gathering](https://code.usgs.gov/sbsc/duniway-lab/dart-dsm/nm-dsm/-/blob/main/1_PedonOrg_AllProps_2024.R) and organization
- 2022 version of predictions is completed

Spring 2024
- Legacy pedon data is [updated](https://code.usgs.gov/sbsc/duniway-lab/dart-dsm/nm-dsm/-/tree/main/data/preprocessed/legacy_horizon.csv)
- Final models run with [updated code](https://code.usgs.gov/sbsc/duniway-lab/dart-dsm/nm-dsm/-/tree/main/modeling-scripts) for the updated data.

## 
### Property information
| common name | column name | tier column name|transformation method | trans. notation | back-trans. notation |
| ------ | ------ | ------ | ------ |------ | ------ |
|CaCO3|caco3_FINAL|caco3_tier|sqrt|sqrt(x) |(x)^2|
|Gypsum|gyp_FINAL|gyp_tier|log|log(x+1)|exp(x)-1|
|SAR|sar_FINAL|sar_tier|log|log(x+1)|exp(x)-1|
|EC|ec_FINAL|ec_tier|log|log(x+1)|exp(x)-1|
|Sand|sand_FINAL|sand_tier|none|NA|NA|
|Clay|clay_FINAL|clay_tier|none|NA|NA|

## Figures
The Map Project for this project's figures is saved to V:/PROJECTS/TRAVIS_NAUMAN/NM_DSM/Reports/NMDSM_Figures.aprx
- Layers for figures should be available on USGS network drives, either at V:\PROJECTS\TRAVIS_NAUMAN\NM_DSM\Data_Spatial or G:\DSM_TWN
- Masked, compressed predictions from the 2022 data is stored at V:\PROJECTS\TRAVIS_NAUMAN\NM_DSM\Data_Spatial\2022_maskedrastersforfigures


##
Have a great day!