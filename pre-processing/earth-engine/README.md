Charlotte Tierney (ctierney320@gmail.com)
<br /> Code updated: September 2021
<br /> Git updated: April 2024

## Google Earth Engine raster processing

This folder contains code written to download raster data on Google Earth Engine. 

Code is written to collect images from various satellites and clip them to the boundary for the New Mexico Digital Soil Mapping project. The rasters generated from these codes made up most of the covariates used in modeling. Not all rasters generated here were used in the final models; images with artifacts from the satellite paths, cloud cover, or other errors were excluded (removed rasters are saved to G:\DSM_TWN\covar_sets\NM_compressed\have_artifacts).

* Landsat 8 images were collected during the growing seasons (May-July) of 2015-2021. 
* Sentinel 1 images were collected for all 12 months of 2015-2020. 
* Sentinel 2 images were collected during the growing seasons (May-July) of 2019-2021.

Sentinel 1 and 2 codes were modified from code written by Dave White (NRCS).
All of these processes require a [shapefile](https://code.usgs.gov/sbsc/duniway-lab/dart-dsm/nm-dsm/-/tree/main/data/GIS/nm_dsm_NMextent_reprojected.shp) of the NM DSM project outline. 

