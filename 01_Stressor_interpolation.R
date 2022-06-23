##############################################################################################################
#################################### 01_Stressor_interpolation ###############################################

library(tidyverse)
library(terra)
library(gstat)
library(fields)
library(spatialEco)
library(raster)

Stressor_layers <- read.csv("Stressor_layers.csv")
 
# Change "Inf" values to NA
Stressor_layers [Stressor_layers ==Inf] <- NA

# Create a SpatVector 
spatVector_stressors <- vect(Stressor_layers, geom=c("lon", "lat"), 
                                  crs="+proj=longlat +datum=WGS84 +no_defs +type=crs")

## Create a template raster with the same dimensions as our current stressor layers
template_SpatRaster <- rast("T:/Boating.asc") # Boating is a stressor layer from Jones et al (2018)
values(template_SpatRaster) <- NA # Rasterising is more efficient if template raster does not contain values
crs(template_SpatRaster)

# Transform the crs to that of the stressor layers from Jones et al (2018)
trnsfrm_vect <- project(spatVector_stressors, template_SpatRaster)


## Rasterise variables using the transformed spatial data
Nitrate_rasterised <- terra::rasterize(trnsfrm_vect, template_SpatRaster, field="nitrate", fun=sum, 
                                       background=NA)

Ammonium_rasterised <- terra::rasterize(trnsfrm_vect, template_SpatRaster, field="ammonium", 
                                        background=NA)

Phytoplankton_rasterised <- terra::rasterize(trnsfrm_vect, template_SpatRaster, field="phytoplankton", 
                                             background=NA)

Zooplankton_rasterised <- terra::rasterize(trnsfrm_vect, template_SpatRaster, field="zooplankton", 
                                           background=NA)

Small_detritus_rasterised <- terra::rasterize(trnsfrm_vect, template_SpatRaster, field="small_detritus", 
                                              background=NA)

Large_detritus_rasterised <- terra::rasterize(trnsfrm_vect, template_SpatRaster, field="large_detritus", 
                                              background=NA)

Temperature_rasterised <- terra::rasterize(trnsfrm_vect, template_SpatRaster, field="Temp_max", 
                                              background=NA)

Hypersalinity_rasterised <- terra::rasterize(trnsfrm_vect, template_SpatRaster, field="Sal_max", 
                                            background=NA)

Hyposalinity_rasterised <- terra::rasterize(trnsfrm_vect, template_SpatRaster, field="Sal_min", 
                                              background=NA)

Oxygen_rasterised <- terra::rasterize(trnsfrm_vect, template_SpatRaster, field="Oxy_min", 
                                        background=NA)

## Resample using focal method - the mean of a specified matrix to fill neighbouring cells

# NITRATE
Nitrate_focal <- focal(Nitrate_rasterised, w = 25, fun=mean, fillvalue=NA, 
                       na.rm=TRUE, na.only=TRUE) 
# AMMONIUM
Ammonium_focal <- focal(Ammonium_rasterised, w = 25, fun=mean, fillvalue=NA,  
                        na.rm=TRUE, na.only=TRUE) 

# PHYTOPLANKTON
Phytoplankton_focal <- focal(Phytoplankton_rasterised, w = 25, fun=mean, fillvalue=NA, 
                             na.rm=TRUE, na.only=TRUE) 

# ZOOPLANKTON
Zooplankton_focal <- focal(Zooplankton_rasterised, w = 25, fun=mean, fillvalue=NA,  
                          na.rm=TRUE, na.only=TRUE) 

# SMALL DETRITUS
Small_detritus_focal <- focal(Small_detritus_rasterised, w = 25, fun=mean, fillvalue=NA, 
                              na.rm=TRUE, na.only=TRUE) 

# LARGE DETRITUS
Large_detritus_focal <- focal(Large_detritus_rasterised, w = 25, fun=mean, fillvalue=NA,  
                              na.rm=TRUE, na.only=TRUE) 

# Temperature
Temperature_focal <- focal(Temperature_rasterised, w = 25, fun=mean, fillvalue=NA,  
                              na.rm=TRUE, na.only=TRUE)

# Hypersalinity
Hypersalinity_focal <- focal(Hypersalinity_rasterised, w = 25, fun=mean, fillvalue=NA,  
                            na.rm=TRUE, na.only=TRUE)

# Hyposalinity
Hyposalinity_focal <- focal(Hyposalinity_rasterised, w = 25, fun=mean, fillvalue=NA,  
                              na.rm=TRUE, na.only=TRUE)

# Oxygen
Oxygen_focal <- focal(Oxygen_rasterised, w = 25, fun=mean, fillvalue=NA,  
                              na.rm=TRUE, na.only=TRUE)


## Rescale
## The stressor layers from Jones et al (2018) have been scaled to between 0 and 1

Nitrate_minmax <- minmax(Nitrate_focal) 
Nitrate <- (Nitrate_focal - Nitrate_minmax[1, ]) / (Nitrate_minmax[2, ] - Nitrate_minmax[1, ])

Ammonium_minmax <- minmax(Ammonium_focal) 
Ammonium <- (Ammonium_focal - Ammonium_minmax[1, ]) / 
  (Ammonium_minmax[2, ] - Ammonium_minmax[1, ])

Phytoplankton_minmax <- minmax(Phytoplankton_focal) 
Phytoplankton <- (Phytoplankton_focal - Phytoplankton_minmax[1, ]) / 
  (Phytoplankton_minmax[2, ] - Phytoplankton_minmax[1, ])

Zooplankton_minmax <- minmax(Zooplankton_focal) 
Zooplankton <- (Zooplankton_focal - Zooplankton_minmax[1, ]) / 
  (Zooplankton_minmax[2, ] - Zooplankton_minmax[1, ])

Small_detritus_minmax <- minmax(Small_detritus_focal) 
Small_detritus <- (Small_detritus_focal - Small_detritus_minmax[1, ]) / 
  (Small_detritus_minmax[2, ] - Small_detritus_minmax[1, ])

Large_detritus_minmax <- minmax(Large_detritus_focal) 
Large_detritus <- (Large_detritus_focal - Large_detritus_minmax[1, ]) / 
  (Large_detritus_minmax[2, ] - Large_detritus_minmax[1, ])

Temperature_minmax <- minmax(Temperature_focal) 
Temperature <- (Temperature_focal - Temperature_minmax[1, ]) / 
  (Temperature_minmax[2, ] - Temperature_minmax[1, ])

Hypersalinity_minmax <- minmax(Hypersalinity_focal) 
Hypersalinity <- (Hypersalinity_focal - Hypersalinity_minmax[1, ]) / 
  (Hypersalinity_minmax[2, ] - Hypersalinity_minmax[1, ])

Hyposalinity_minmax <- minmax(Hyposalinity_focal) 
Hyposalinity <- (Hyposalinity_focal - Hyposalinity_minmax[1, ]) / 
  (Hyposalinity_minmax[2, ] - Hyposalinity_minmax[1, ])

Oxygen_minmax <- minmax(Oxygen) 
Oxygen <- (Oxygen_focal - Oxygen_minmax[1, ]) / 
  (Oxygen_minmax[2, ] - Oxygen_minmax[1, ])


## Invert "Oxygen" and "Hyposalinity" 
Oxygen_invert <- raster(Oxygen)
Oxygen_invert <- raster.invert(Oxygen_invert)
Oxygen_invert <- rast(Oxygen_invert)

Oxygen_minmax <- minmax(Oxygen_invert) 
Oxygen_invert <- (Oxygen_invert - Oxygen_minmax[1, ]) / 
  (Oxygen_minmax[2, ] - Oxygen_minmax[1, ])


Hyposalinity_invert <- raster(Hyposalinity)
Hyposalinity_invert <- raster.invert(Hyposalinity_invert)
Hyposalinity_invert <- rast(Hyposalinity_invert)

Hyposalinity_minmax <- minmax(Hyposalinity_invert) 
Hyposalinity_invert <- (Hyposalinity_invert - Hyposalinity_minmax[1, ]) / 
  (Hyposalinity_minmax[2, ] - Hyposalinity_minmax[1, ])










