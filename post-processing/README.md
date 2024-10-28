Charlotte Tierney (ctierney320@gmail.com)
<br /> Code updated: 2022
<br /> Git updated: April 2024

## Code for Compressing and Masking 

The raster-processing code documents in this folder were used in the NM DSM project before 2023. They have not been especially cleaned or organized for sharing or 2024 re-processing.

- **MaskCompress.R** reads in finished prediction rasters, masks out elevation areas above 3000m and with surface water, then compresses the rasters to 16-bit. <br/>
This code requires the mask raster found below.
- **Mask_Gypsum.R** reads in finished prediction rasters, _clips Gypsum rasters to the extent of New Mexico_, masks out elevation areas above 3000m and with surface water, then compresses the rasters to 16-bit. <br/>
This code requires the mask raster and the NM extent shapefile

**Processing Layers** are found here: <br/> V:/PROJECTS/TRAVIS_NAUMAN/NM_DSM/Data_Spatial/Processing_Data