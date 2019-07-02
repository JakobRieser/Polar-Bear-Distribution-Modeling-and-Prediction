#' ====================================================================================================================================
#' Species distribution models (SDM) to predict polar bear distribution using occurrence data from GBIF
#' ====================================================================================================================================


#' Author: Jakob Rieser (University of Würzburg, EAGLE MSc. Program)
#' E-Mail: jakob.rieser@t-online.de
#' GitHub: JakobRieser

#' Date: 
#' This script was tested on Windows 10 64Bit, RStudio version 1.1.463, R version 3.5.1
#' Developed based on a scrpt by B. Reineking and M. Wegmann (download at https://bitbucket.org/rsbiodiv/species_distribution_model/src/master/)  

#' Occurrence data source: [gbif](http://www.gbif.org)  
#' Environmental data source: [worldclim](http://www.worldclim.org)  
#' Algorithms: GAM, RF


#' ====================================================================================================================================
#' Prepare your environment:
#' ====================================================================================================================================

#' Install and load the needed packages:
#' ------------------------------------------------------------------------------------------------

Packages <- c("rms", "raster", "mgcv", "randomForest", "sdmpredictors", "dismo", "rgdal", "ellipse", "rJava", "XML")

install.packages(Packages)

lapply(Packages, library, character.only = TRUE)


#' set the working directory to a path of your choice:
#' ------------------------------------------------------------------------------------------------

#' you have to use your own path: setwd("/set/path/to/your/script/")
getwd()
setwd("D:\\Programme\\OneDrive\\EAGLE M.Sc\\Term 2 (Summer 2019)\\MET1 - Spatial Modeling and Prediction\\Polar Bear Distribution Modeling and Prediction\\Data")


#' ====================================================================================================================================
#' Import data: Environment and species occurrences
#' ====================================================================================================================================

#' Read/generate vector layer with study area
#' ------------------------------------------------------------------------------------------------

if (file.exists("./GIS/study_area.shp")) {
  study_area <- readOGR("./GIS/study_area.shp")
  #' './' refers to the current working directory, i.e. we are specifying as the first argument of readOGR, the dsn, the subdirectory "GIS" within the current working directory
  #' make sure there is no trailing '/' in the value of the dsn argument, i.e. do **not** use "./GIS/"
  #' when importing shapefiles, drop the suffix from the layer argument, i.e. do **not** use "africa_dissolved.shp"
} else {
  #' First, get the coordinates into a 2-column matrix:
  x_coord <- c(-180, -180, 180, 180)
  y_coord <- c(90,  50,  50, 90)
  xym <- cbind(x_coord, y_coord)
  xym
  #' Then create a Polygon, wrap that into a Polygons object, then wrap that into a SpatialPolygons object:
  p <- Polygon(xym)
  ps <- Polygons(list(p),1)
  sps <- SpatialPolygons(list(ps))
  sps.df <- data.frame( ID=1) 
  study_area <- SpatialPolygonsDataFrame(sps, sps.df)
  #' export the study area as a shapefile to disk:
  writeOGR(study_area, "./GIS", "study_area", driver="ESRI Shapefile", overwrite_layer=TRUE)
}

#' Download and read bioclim variables
#' ------------------------------------------------------------------------------------------------

#' The function getData will load the data from the working directory, if available. If they are not available, the function will attempt to download the data from the internet.
#' For a definition of the variables, see [http://www.worldclim.org/bioclim]

#' Variable | Description
#' -------- | -----------
#' BIO1     | Annual Mean Temperature
#' BIO2     | Mean Diurnal Range (Mean of monthly (max temp - min temp))
#' BIO3     | Isothermality (BIO2/BIO7) (* 100)
#' BIO4     | Temperature Seasonality (standard deviation *100)
#' BIO5     | Max Temperature of Warmest Month
#' BIO6     | Min Temperature of Coldest Month
#' BIO7     | Temperature Annual Range (BIO5-BIO6)
#' BIO8     | Mean Temperature of Wettest Quarter
#' BIO9     | Mean Temperature of Driest Quarter
#' BIO10    | Mean Temperature of Warmest Quarter
#' BIO11    | Mean Temperature of Coldest Quarter
#' BIO12    | Annual Precipitation
#' BIO13    | Precipitation of Wettest Month
#' BIO14    | Precipitation of Driest Month
#' BIO15    | Precipitation Seasonality (Coefficient of Variation)
#' BIO16    | Precipitation of Wettest Quarter
#' BIO17    | Precipitation of Driest Quarter
#' BIO18    | Precipitation of Warmest Quarter
#' BIO19    | Precipitation of Coldest Quarter

variables_bioclim <- raster::getData("worldclim", var = "bio", res = 2.5)

#' Plot the first raster layer, i.e. annual mean temperature
plot(raster(variables_bioclim, 1))

# Add the outline of the study area
plot(study_area, add=TRUE)

#' Crop to study area extent (with a 5 degree buffer in each direction)
variables_bioclim_crop <- crop(variables_bioclim, extent(study_area) + 10)

#' Plot the first raster layer of the cropped climate data
plot(raster(variables_bioclim_crop, 1))
plot(study_area, add=TRUE)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' definition of polar projection:
polar_projection = CRS("+init=epsg:3995")

#' reproject the raster:
variables_bioclim_crop <- projectRaster(variables_bioclim_crop, crs=polar_projection, method="bilinear")

plot(raster(variables_bioclim_crop, 1))

#' expoert to drive:
writeRaster(variables_bioclim_crop, filename="variables_bioclim_crop", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

#' if existing, just do:
variables_bioclim_crop <- stack("variables_bioclim_crop.tif")
names(variables_bioclim_crop) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

#' Download and read Bio-ORACLE variables
#' ------------------------------------------------------------------------------------------------

#' In the case of polar bears, the bioclim variabloes are not enough due to the fact that they are very dependent on sea ice.
#' That's why we have to add layers that contain information on sea ice conditions

# Explore datasets in the package 
datasets <- list_datasets() 
View(datasets)

# Explore layers in a dataset and decide which one are useful for the analysis 
layers <- list_layers("Bio-ORACLE", marine=T) 
View(layers)

# Download the specific layers to the current directory 
variables_biooracle <- load_layers(c("BO2_icethickmean_ss", "BO2_icethickrange_ss", "BO2_icecovermean_ss", "BO2_icecoverrange_ss", "BO2_curvelmean_ss", "BO2_tempmean_ss"), equalarea = FALSE) 

plot(variables_biooracle)

#' crop to the extent of the study area:
variables_biooracle_crop <- crop(variables_biooracle, extent(study_area) + 10)

#' reprojection and resampling to fit the bioclim variables:
variables_biooracle_crop <- projectRaster(variables_biooracle_crop, crs=polar_projection, method="bilinear")
variables_biooracle_crop <- resample(variables_biooracle_crop, variables_bioclim_crop)

# names(variables_full) <-c(names(variables_bioclim_crop), "bio1", "bio2", "bio3", "bio4")
# names(variables_ice_crop)

#' export to drive:
writeRaster(variables_biooracle_crop, filename="variables_biooracle_crop", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

variables_biooracle_crop <- stack("variables_biooracle_crop.tif")
names(variables_biooracle_crop) <- c("BO2_icethickmean_ss", "BO2_icethickrange_ss", "BO2_icecovermean_ss", "BO2_icecoverrange_ss", "BO2_curvelmean_ss", "BO2_tempmean_ss")

#' Download and read occurrence data
#' ------------------------------------------------------------------------------------------------

#' Read occurrence points. If they exist locally, use the local file.
#' If they do not exist, download from [gbif](http://www.gbif.org)

if (file.exists("./GIS/Occurrence Data/Ursus maritimus.shp")) {
  Ursus_maritimus <- readOGR("./GIS/Occurrence Data/Ursus maritimus.shp", layer = "Ursus maritimus")
} else {
  #' Download species location data from gbif
  Ursus_maritimus_0 <- gbif("Ursus", "maritimus")
  Ursus_maritimus <- subset(Ursus_maritimus_0,select=c("lat","lon"))
  Ursus_maritimus <- na.omit(Ursus_maritimus)
  coordinates(Ursus_maritimus) <- c("lon", "lat")  # set spatial coordinates
  #' Add projection information
  proj4string(Ursus_maritimus) <- CRS("+proj=longlat +datum=WGS84")
  #' convert to the polar projection
  Ursus_maritimus <- spTransform(Ursus_maritimus, CRS=polar_projection)
  #' Convert to Spatial Polygons Data Frame
  Ursus_maritimus.df <- data.frame( ID=1:length(Ursus_maritimus)) 
  Ursus_maritimus <- SpatialPointsDataFrame(Ursus_maritimus, Ursus_maritimus.df)
  #' Save species records in mif-format (preserves full column names)
  writeOGR(Ursus_maritimus, "./GIS/Occurrence Data", 
           "Ursus maritimus", driver="MapInfo File", dataset_options="FORMAT=MIF", overwrite_layer = TRUE)
  writeOGR(Ursus_maritimus, "./GIS/Occurrence Data", 
           "Ursus maritimus", driver="ESRI Shapefile", dataset_options="FORMAT=SHP", overwrite_layer = TRUE)
  
}

dev.off ()
plot(raster(variables_bioclim_crop, 1))
plot(Ursus_maritimus, add = TRUE)
