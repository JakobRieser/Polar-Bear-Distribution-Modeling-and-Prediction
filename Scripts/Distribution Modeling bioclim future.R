#' ====================================================================================================================================
#' RCP 85
#' ====================================================================================================================================



#' Download and read future bioclim variables
#' ------------------------------------------------------------------------------------------------

#' year 2050, model BC, rcp85:
variables_bioclim_2050_rcp85 <- raster::getData("CMIP5", var="bio", res=2.5, rcp=85, model="BC", year=50)

#' Plot the first raster layer, i.e. annual mean temperature
plot(raster(variables_bioclim_2050_rcp85, 1))

# Add the outline of the study area
plot(study_area, add=TRUE)

#' Crop to study area extent (with a 5 degree buffer in each direction)
variables_bioclim_2050_rcp85_crop <- crop(variables_bioclim_2050_rcp85, extent(study_area) + 10)

#' Plot the first raster layer of the cropped climate data
plot(raster(variables_bioclim_2050_rcp85_crop, 1))
plot(study_area, add=TRUE)

#' export to drive:
writeRaster(variables_bioclim_2050_rcp85_crop, filename="variables_bioclim_2050_rcp85_crop", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

#' if existing, just do:
variables_bioclim_2050_rcp85_crop <- stack("variables_bioclim_2050_rcp85_crop.tif")
names(variables_bioclim_2050_rcp85_crop) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

#' Select an uncorrelated subset of environmental variables
subset_var_bioclim_2050_rcp85 <- subset(variables_bioclim_2050_rcp85_crop, c("bio1", "bio2", "bio5", "bio13", "bio15", "bio19"))


#' Generalized Additive Models
#' ------------------------------------------------------------------------------------------------

#' Prediction map
gammap_bioclim_2050_rcp85 <- predict(subset_var_bioclim_2050_rcp85, gammodel_bioclim, type = "response")

plot(gammap_bioclim_2050_rcp85)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' reproject the raster:
gammap_bioclim_2050_rcp85 <- projectRaster(gammap_bioclim_2050_rcp85, crs=polar_projection, method="bilinear")

plot(gammap_bioclim_2050_rcp85)

#' export prediction map to disk:
writeRaster(gammap_bioclim_2050_rcp85, filename="gammap_bioclim_2050_rcp85", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' Random Forest
#' ------------------------------------------------------------------------------------------------

#' Prediction map
rfmap_bioclim_2050_rcp85 <- predict(subset_var_bioclim_2050_rcp85, rfmodel_bioclim, type = "prob", index = 2)

plot(rfmap_bioclim_2050_rcp85)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' reproject the raster:
rfmap_bioclim_2050_rcp85 <- projectRaster(rfmap_bioclim_2050_rcp85, crs=polar_projection, method="bilinear")

plot(rfmap_bioclim_2050_rcp85)

#' export prediction map to disk:
writeRaster(rfmap_bioclim_2050_rcp85, filename="rfmap_bioclim_2050_rcp85", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' combine both models to one by using the mean:
map_bioclim_2050_rcp85_combined <- mean(rfmap_bioclim_2050_rcp85, gammap_bioclim_2050_rcp85)

plot(map_bioclim_2050_rcp85_combined)
writeRaster(map_bioclim_2050_rcp85_combined, filename="map_bioclim_2050_rcp85_combined", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' ====================================================================================================================================
#' RCP 60
#' ====================================================================================================================================



#' Download and read future bioclim variables
#' ------------------------------------------------------------------------------------------------

#' year 2050, model BC, rcp60:
variables_bioclim_2050_rcp60 <- raster::getData("CMIP5", var="bio", res=2.5, rcp=60, model="BC", year=50)

#' Plot the first raster layer, i.e. annual mean temperature
plot(raster(variables_bioclim_2050_rcp60, 1))

# Add the outline of the study area
plot(study_area, add=TRUE)

#' Crop to study area extent (with a 5 degree buffer in each direction)
variables_bioclim_2050_rcp60_crop <- crop(variables_bioclim_2050_rcp60, extent(study_area) + 10)

#' Plot the first raster layer of the cropped climate data
plot(raster(variables_bioclim_2050_rcp60_crop, 1))
plot(study_area, add=TRUE)

#' export to drive:
writeRaster(variables_bioclim_2050_rcp60_crop, filename="variables_bioclim_2050_rcp60_crop", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

#' if existing, just do:
variables_bioclim_2050_rcp60_crop <- stack("variables_bioclim_2050_rcp60_crop.tif")
names(variables_bioclim_2050_rcp60_crop) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

#' Select an uncorrelated subset of environmental variables
subset_var_bioclim_2050_rcp60 <- subset(variables_bioclim_2050_rcp60_crop, c("bio1", "bio2", "bio5", "bio13", "bio15", "bio19"))


#' Generalized Additive Models
#' ------------------------------------------------------------------------------------------------

#' Prediction map
gammap_bioclim_2050_rcp60 <- predict(subset_var_bioclim_2050_rcp60, gammodel_bioclim, type = "response")

plot(gammap_bioclim_2050_rcp60)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' reproject the raster:
gammap_bioclim_2050_rcp60 <- projectRaster(gammap_bioclim_2050_rcp60, crs=polar_projection, method="bilinear")

plot(gammap_bioclim_2050_rcp60)

#' export prediction map to disk:
writeRaster(gammap_bioclim_2050_rcp60, filename="gammap_bioclim_2050_rcp60", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' Random Forest
#' ------------------------------------------------------------------------------------------------

#' Prediction map
rfmap_bioclim_2050_rcp60 <- predict(subset_var_bioclim_2050_rcp60, rfmodel_bioclim, type = "prob", index = 2)

plot(rfmap_bioclim_2050_rcp60)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' reproject the raster:
rfmap_bioclim_2050_rcp60 <- projectRaster(rfmap_bioclim_2050_rcp60, crs=polar_projection, method="bilinear")

plot(rfmap_bioclim_2050_rcp60)

#' export prediction map to disk:
writeRaster(rfmap_bioclim_2050_rcp60, filename="rfmap_bioclim_2050_rcp60", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' combine both models to one by using the mean:
map_bioclim_2050_rcp60_combined <- mean(rfmap_bioclim_2050_rcp60, gammap_bioclim_2050_rcp60)

plot(map_bioclim_2050_rcp60_combined)
writeRaster(map_bioclim_2050_rcp60_combined, filename="map_bioclim_2050_rcp60_combined", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))



#' ====================================================================================================================================
#' RCP 25
#' ====================================================================================================================================



#' Download and read future bioclim variables
#' ------------------------------------------------------------------------------------------------

#' year 2050, model BC, rcp26:
variables_bioclim_2050_rcp26 <- raster::getData("CMIP5", var="bio", res=2.5, rcp=26, model="BC", year=50)

#' Plot the first raster layer, i.e. annual mean temperature
plot(raster(variables_bioclim_2050_rcp26, 1))

# Add the outline of the study area
plot(study_area, add=TRUE)

#' Crop to study area extent (with a 5 degree buffer in each direction)
variables_bioclim_2050_rcp26_crop <- crop(variables_bioclim_2050_rcp26, extent(study_area) + 10)

#' Plot the first raster layer of the cropped climate data
plot(raster(variables_bioclim_2050_rcp26_crop, 1))
plot(study_area, add=TRUE)

#' export to drive:
writeRaster(variables_bioclim_2050_rcp26_crop, filename="variables_bioclim_2050_rcp26_crop", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

#' if existing, just do:
variables_bioclim_2050_rcp26_crop <- stack("variables_bioclim_2050_rcp26_crop.tif")
names(variables_bioclim_2050_rcp26_crop) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

#' Select an uncorrelated subset of environmental variables
subset_var_bioclim_2050_rcp26 <- subset(variables_bioclim_2050_rcp26_crop, c("bio1", "bio2", "bio5", "bio13", "bio15", "bio19"))


#' Generalized Additive Models
#' ------------------------------------------------------------------------------------------------

#' Prediction map
gammap_bioclim_2050_rcp26 <- predict(subset_var_bioclim_2050_rcp26, gammodel_bioclim, type = "response")

plot(gammap_bioclim_2050_rcp26)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' reproject the raster:
gammap_bioclim_2050_rcp26 <- projectRaster(gammap_bioclim_2050_rcp26, crs=polar_projection, method="bilinear")

plot(gammap_bioclim_2050_rcp26)

#' export prediction map to disk:
writeRaster(gammap_bioclim_2050_rcp26, filename="gammap_bioclim_2050_rcp26", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' Random Forest
#' ------------------------------------------------------------------------------------------------

#' Prediction map
rfmap_bioclim_2050_rcp26 <- predict(subset_var_bioclim_2050_rcp26, rfmodel_bioclim, type = "prob", index = 2)

plot(rfmap_bioclim_2050_rcp26)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' reproject the raster:
rfmap_bioclim_2050_rcp26 <- projectRaster(rfmap_bioclim_2050_rcp26, crs=polar_projection, method="bilinear")

plot(rfmap_bioclim_2050_rcp26)

#' export prediction map to disk:
writeRaster(rfmap_bioclim_2050_rcp26, filename="rfmap_bioclim_2050_rcp26", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' combine both models to one by using the mean:
map_bioclim_2050_rcp26_combined <- mean(rfmap_bioclim_2050_rcp26, gammap_bioclim_2050_rcp26)

plot(map_bioclim_2050_rcp26_combined)
writeRaster(map_bioclim_2050_rcp26_combined, filename="map_bioclim_2050_rcp26_combined", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#'create layerstack from all three rcp models:

map_bioclim_2050_all <- stack(map_bioclim_2050_rcp26_combined, map_bioclim_2050_rcp60_combined, map_bioclim_2050_rcp85_combined)
plotRGB(map_bioclim_2050_all)
names(map_bioclim_2050_all) <- c("RCP 2.6", "RCP 6.0", "RCP 8.5")
map_bioclim_2050_all

writeRaster(map_bioclim_2050_all, filename="map_bioclim_2050_all", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))