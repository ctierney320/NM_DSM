## Welcome
The goal of these scripts is to create continuous maps of six soil properties at seven depths. For each property and depth, this code will create a raster of predicted soil property values, a raster of the prediction’s RPI data, and a table of the model’s cross validation and central tendencies.

The first drafts of these codes were written in 2021 by Charlotte Tierney (ctierney320@gmail.com), based on processes by Travis Nauman (travis.nauman@usda.gov). Latests updates were made in April 2024.

All six properties were modeled in 2022. New data was added in December 2023 so CaCO3, Gypsum, EC, Sand, and Clay are updated for 2024. *Soil absorption ratio (SAR) was not measured in the new dataset, so the 2022 predictions are the final versions.  


## Code outline 
1. 	Set up<br/>
  **a.** Load packages <br/>
  **b.**	Set up all the required inputs and outputs<br/>
    - A folder where generated files and final predictions can go (should be local, should NOT be backed up to the git, should be copied to G drive after every property)
    - File of all processed pedon training data (not property specific)
    - Folder file path for all the raster covariates (should run with the local folder but it is backed up on the G drive)
    - the “Snap Raster” or the blueprint for the raster covariates (the first tif in the covariate folder)
    - Study area extent shapefile (saved locally and on G drive) <br/>
    
    **c.** Input the strings this property uses for column headers and saving. The version name is up to you!<br/>
  **d.**	Name the type of transformation this property uses (this is for tables; the actual transformation code must be manually checked)<br/>
  **e.**	If the data for this property hasn’t been isolated (such as CaCO3_Pedons.rds) from the full pedon dataset (pedons_ALL_est.csv), make a new table with just coordinate data, depth data, and property data. *many of these are already made, but they can be remade quickly
2. Extract covariate data at each pedon location
- This takes 15-20 minutes and only needs to be done once for every property. Check if a “_ExtractedPts_” rds file exists for this property, it will be faster to read it in here. 
3. Modeling!!
- I apologize for all the transformation stuff. Each property’s code is tailored for its transformation needs. I have double checked that the correct lines are uncommented for each property’s script, but if something doesn’t come out right, that’s the first thing to check. (Also, see notes about SAR below.)
- The following areas require **manually typing the property name** (table$caco3_FINAL instead of table$prop): first line in section 3 (line 150ish), at the start of cross validation (line 340ish), at the PCV stats (line 400ish), and making the stats table (440, 450) . These are all tailored to each code already—just keep an eye out if you’re ever editing or reusing. 

  **a.** The set up requires standardizing the property data and building an empty data table for the cross validation stats.<br/>
  **b.** Now it’s time for the Model Loop™!!! For each depth (0, 5, 15, 30, 60, 100, and 150cm), this loop does the following:
    - Prepares lists and tables for later processing
    - **Transforming the data!!** If necessary!! Each property’s code should have this properly set up, but still double check the transformation method is correct with table below. If transformation is not necessary, all four transformation lines should be commented out.
    - The code saves a depth specific file of extracted points here—the only purpose of this is for double checks, tests, or small sections re-runs.
    - Set up data for the Ranger Random Forest model: build quantile data and create the modeling formula with the property and every covariate. Check to make sure the **transformed or not transformed** lines are correctly commented out.
    - Create and save the model object
    - Predictions!! This step takes about 24 hours for each depth (on WM1701)—models for a whole property will take a full week to run, even parallelized. The model creates a prediction for the data and for the high and low quantile data.
    - If **transformed**, all the prediction rasters need to be back-transformed here. Again, check the method.
    - The lines to save the predictions are different for **transformed** and non-transformed. Double check the correct lines are running.
    - Check the uncertainty of the predictions using Travis’s RPI statistic! and save it. Again, used the **transformed** or not transformed lines.<br/>

  **c.** We’re still in the Model Loop but now we’re doing a manual cross validation! *This step can be run without running the model predictions! <br/>
    - The code here randomly divides the extracted point data into 10 groups (folds), then runs a function to create a model object that uses 90% of the data as training data and runs a prediction based on 10% of the data, 10 times. Each group of 10% is used as test data once.
    - The prediction cross validation (PCV) data is then used to generate the statistics (R2, RMSE, etc.) for the models we used. There are some things for transformation to check here.
    Line 388(ish) has a manually coded property mention. It should be correct, but if something isn’t working, check here, too.
    - The last step of the loop is to calculate and build a table with cross validation and property data. The loop saves a cross validation table for each depth, and a combined table is collected last.

  **d.** The last step in this code is to save the table written with stats for ALL the depths! This happens outside the loop and needs to be saved while the prediction session is still open. If you start the loop, make sure this table gets saved before restarting anything.

**REMEMBER TO [MASK, COMPRESS](https://code.usgs.gov/sbsc/duniway-lab/dart-dsm/nm-dsm/-/tree/main/post-processing), AND SAVE ALL OF THE OUTPUT FILES TO THE V:/ DRIVE**

## Property information
Each property's script is tailored to these strings and transformation methods. If you're editing or reusing, make sure the strings and transformations are  consistently updated. 
| common name | column name | tier column name|transformation method | trans. notation | back-trans. notation |
| ------ | ------ | ------ | ------ |------ | ------ |
|CaCO3|caco3_FINAL|caco3_tier|sqrt|sqrt(x) |(x)^2|
|Gypsum|gyp_FINAL|gyp_tier|log|log(x+1)|exp(x)-1|
|SAR|sar_FINAL|sar_tier|log|log(x+1)|exp(x)-1|
|EC|ec_FINAL|ec_tier|log|log(x+1)|exp(x)-1|
|Sand|sand_FINAL|sand_tier|none|NA|NA|
|Clay|clay_FINAL|clay_tier|none|NA|NA|


## 

#### *Sodium Absorption Ratio

*SAR was originally modeled on a Google Cloud Platform virtual machine. It has not been edited to sync with the folders on the git. The script for SAR's predictions includes the same steps with some changes for GCP. Also, the transformation method is more organized on this code... but none of the others. 

: )
##