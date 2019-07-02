#' ====================================================================================================================================
#' RCP 85
#' ====================================================================================================================================



#' Import and future Bio-ORACLE variables
#' ------------------------------------------------------------------------------------------------

#' unfortunately, there is no implementation of these future variables in R currently
#' that's why we have to download the from the Bio-ORACLE website (www.bio-oracle.org) manually

#' import the data from folders:
setwd("D:\\Programme\\OneDrive\\EAGLE M.Sc\\Term 2 (Summer 2019)\\MET1 - Spatial Modeling and Prediction\\Polar Bear Distribution Modeling and Prediction\\Data\\BioORACLE_future\\rcp85")
files_biooracle_2050_rcp85 <- list.files(pattern="\\.tif")
variables_biooracle_2050_rcp85 <- stack(files_biooracle_2050_rcp85)

setwd("D:\\Programme\\OneDrive\\EAGLE M.Sc\\Term 2 (Summer 2019)\\MET1 - Spatial Modeling and Prediction\\Polar Bear Distribution Modeling and Prediction\\Data")

#' Plot the first raster layer:
plot(raster(variables_biooracle_2050_rcp85, 1))

# Add the outline of the study area
plot(study_area, add=TRUE)

#' Crop to study area extent (with a 5 degree buffer in each direction)
variables_biooracle_2050_rcp85_crop <- crop(variables_biooracle_2050_rcp85, extent(study_area) + 10)

#' Plot the first raster layer of the cropped climate data
plot(raster(variables_biooracle_2050_rcp85_crop, 1))
plot(study_area, add=TRUE)

#' export to drive:
writeRaster(variables_biooracle_2050_rcp85_crop, filename="variables_biooracle_2050_rcp85_crop", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

#' if existing, just do:
variables_biooracle_2050_rcp85_crop <- stack("variables_biooracle_2050_rcp85_crop.tif")
names(variables_biooracle_2050_rcp85_crop) <- c("BO2_curvelmean_ss", "BO2_icethickmean_ss", "BO2_icethickrange_ss", "BO2_tempmean_ss")


#' Select an uncorrelated subset of environmental variables
subset_var_biooracle_2050_rcp85 <- subset(variables_biooracle_2050_rcp85_crop, c("BO2_curvelmean_ss", "BO2_icethickmean_ss", "BO2_icethickrange_ss", "BO2_tempmean_ss"))


#' Generalized Additive Models
#' ------------------------------------------------------------------------------------------------

#' Prediction map
gammap_biooracle_2050_rcp85 <- predict(subset_var_biooracle_2050_rcp85, gammodel_biooracle_noic, type = "response")

plot(gammap_biooracle_2050_rcp85)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' reproject the raster:
gammap_biooracle_2050_rcp85 <- projectRaster(gammap_biooracle_2050_rcp85, crs=polar_projection, method="bilinear")

plot(gammap_biooracle_2050_rcp85)

#' export prediction map to disk:
writeRaster(gammap_biooracle_2050_rcp85, filename="gammap_biooracle_2050_rcp85", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' Random Forest
#' ------------------------------------------------------------------------------------------------

#' Prediction map
rfmap_biooracle_2050_rcp85 <- predict(subset_var_biooracle_2050_rcp85, rfmodel_biooracle_noic, type = "prob", index = 2)

plot(rfmap_biooracle_2050_rcp85)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' reproject the raster:
rfmap_biooracle_2050_rcp85 <- projectRaster(rfmap_biooracle_2050_rcp85, crs=polar_projection, method="bilinear")

plot(rfmap_biooracle_2050_rcp85)

#' export prediction map to disk:
writeRaster(rfmap_biooracle_2050_rcp85, filename="rfmap_biooracle_2050_rcp85", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' combine both models to one by using the mean:
map_biooracle_2050_rcp85_combined <- mean(rfmap_biooracle_2050_rcp85, gammap_biooracle_2050_rcp85)

plot(map_biooracle_2050_rcp85_combined)
writeRaster(map_biooracle_2050_rcp85_combined, filename="map_biooracle_2050_rcp85_combined", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))




#' ====================================================================================================================================
#' RCP 60
#' ====================================================================================================================================



#' import the data from folders:
setwd("D:\\Programme\\OneDrive\\EAGLE M.Sc\\Term 2 (Summer 2019)\\MET1 - Spatial Modeling and Prediction\\Polar Bear Distribution Modeling and Prediction\\Data\\BioORACLE_future\\rcp60")
files_biooracle_2050_rcp60 <- list.files(pattern="\\.tif")
variables_biooracle_2050_rcp60 <- stack(files_biooracle_2050_rcp60)

setwd("D:\\Programme\\OneDrive\\EAGLE M.Sc\\Term 2 (Summer 2019)\\MET1 - Spatial Modeling and Prediction\\Polar Bear Distribution Modeling and Prediction\\Data")

#' Plot the first raster layer:
plot(raster(variables_biooracle_2050_rcp60, 1))

# Add the outline of the study area
plot(study_area, add=TRUE)

#' Crop to study area extent (with a 5 degree buffer in each direction)
variables_biooracle_2050_rcp60_crop <- crop(variables_biooracle_2050_rcp60, extent(study_area) + 10)

#' Plot the first raster layer of the cropped climate data
plot(raster(variables_biooracle_2050_rcp60_crop, 1))
plot(study_area, add=TRUE)

#' export to drive:
writeRaster(variables_biooracle_2050_rcp60_crop, filename="variables_biooracle_2050_rcp60_crop", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

#' if existing, just do:
variables_biooracle_2050_rcp60_crop <- stack("variables_biooracle_2050_rcp60_crop.tif")
names(variables_biooracle_2050_rcp60_crop) <- c("BO2_curvelmean_ss", "BO2_icethickmean_ss", "BO2_icethickrange_ss", "BO2_tempmean_ss")


#' Select an uncorrelated subset of environmental variables
subset_var_biooracle_2050_rcp60 <- subset(variables_biooracle_2050_rcp60_crop, c("BO2_curvelmean_ss", "BO2_icethickmean_ss", "BO2_icethickrange_ss", "BO2_tempmean_ss"))


#' Generalized Additive Models
#' ------------------------------------------------------------------------------------------------

#' Prediction map
gammap_biooracle_2050_rcp60 <- predict(subset_var_biooracle_2050_rcp60, gammodel_biooracle_noic, type = "response")

plot(gammap_biooracle_2050_rcp60)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' reproject the raster:
gammap_biooracle_2050_rcp60 <- projectRaster(gammap_biooracle_2050_rcp60, crs=polar_projection, method="bilinear")

plot(gammap_biooracle_2050_rcp60)

#' export prediction map to disk:
writeRaster(gammap_biooracle_2050_rcp60, filename="gammap_biooracle_2050_rcp60", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' Random Forest
#' ------------------------------------------------------------------------------------------------

#' Prediction map
rfmap_biooracle_2050_rcp60 <- predict(subset_var_biooracle_2050_rcp60, rfmodel_biooracle_noic, type = "prob", index = 2)

plot(rfmap_biooracle_2050_rcp60)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' reproject the raster:
rfmap_biooracle_2050_rcp60 <- projectRaster(rfmap_biooracle_2050_rcp60, crs=polar_projection, method="bilinear")

plot(rfmap_biooracle_2050_rcp60)

#' export prediction map to disk:
writeRaster(rfmap_biooracle_2050_rcp60, filename="rfmap_biooracle_2050_rcp60", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' combine both models to one by using the mean:
map_biooracle_2050_rcp60_combined <- mean(rfmap_biooracle_2050_rcp60, gammap_biooracle_2050_rcp60)

plot(map_biooracle_2050_rcp60_combined)
writeRaster(map_biooracle_2050_rcp60_combined, filename="map_biooracle_2050_rcp60_combined", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))




#' ====================================================================================================================================
#' RCP 26
#' ====================================================================================================================================



#' import the data from folders:
setwd("D:\\Programme\\OneDrive\\EAGLE M.Sc\\Term 2 (Summer 2019)\\MET1 - Spatial Modeling and Prediction\\Polar Bear Distribution Modeling and Prediction\\Data\\BioORACLE_future\\rcp26")
files_biooracle_2050_rcp26 <- list.files(pattern="\\.tif")
variables_biooracle_2050_rcp26 <- stack(files_biooracle_2050_rcp26)

setwd("D:\\Programme\\OneDrive\\EAGLE M.Sc\\Term 2 (Summer 2019)\\MET1 - Spatial Modeling and Prediction\\Polar Bear Distribution Modeling and Prediction\\Data")

#' Plot the first raster layer:
plot(raster(variables_biooracle_2050_rcp26, 1))

# Add the outline of the study area
plot(study_area, add=TRUE)

#' Crop to study area extent (with a 5 degree buffer in each direction)
variables_biooracle_2050_rcp26_crop <- crop(variables_biooracle_2050_rcp26, extent(study_area) + 10)

#' Plot the first raster layer of the cropped climate data
plot(raster(variables_biooracle_2050_rcp26_crop, 1))
plot(study_area, add=TRUE)

#' export to drive:
writeRaster(variables_biooracle_2050_rcp26_crop, filename="variables_biooracle_2050_rcp26_crop", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

#' if existing, just do:
variables_biooracle_2050_rcp26_crop <- stack("variables_biooracle_2050_rcp26_crop.tif")
names(variables_biooracle_2050_rcp26_crop) <- c("BO2_curvelmean_ss", "BO2_icethickmean_ss", "BO2_icethickrange_ss", "BO2_tempmean_ss")


#' Select an uncorrelated subset of environmental variables
subset_var_biooracle_2050_rcp26 <- subset(variables_biooracle_2050_rcp26_crop, c("BO2_curvelmean_ss", "BO2_icethickmean_ss", "BO2_icethickrange_ss", "BO2_tempmean_ss"))


#' Generalized Additive Models
#' ------------------------------------------------------------------------------------------------

#' Prediction map
gammap_biooracle_2050_rcp26 <- predict(subset_var_biooracle_2050_rcp26, gammodel_biooracle_noic, type = "response")

plot(gammap_biooracle_2050_rcp26)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' reproject the raster:
gammap_biooracle_2050_rcp26 <- projectRaster(gammap_biooracle_2050_rcp26, crs=polar_projection, method="bilinear")

plot(gammap_biooracle_2050_rcp26)

#' export prediction map to disk:
writeRaster(gammap_biooracle_2050_rcp26, filename="gammap_biooracle_2050_rcp26", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' Random Forest
#' ------------------------------------------------------------------------------------------------

#' Prediction map
rfmap_biooracle_2050_rcp26 <- predict(subset_var_biooracle_2050_rcp26, rfmodel_biooracle_noic, type = "prob", index = 2)

plot(rfmap_biooracle_2050_rcp26)

#' As you can clearly see, the projection is not very good for visualisation in the polar regions.
#' Therefore we change it to a polar projection

#' reproject the raster:
rfmap_biooracle_2050_rcp26 <- projectRaster(rfmap_biooracle_2050_rcp26, crs=polar_projection, method="bilinear")

plot(rfmap_biooracle_2050_rcp26)

#' export prediction map to disk:
writeRaster(rfmap_biooracle_2050_rcp26, filename="rfmap_biooracle_2050_rcp26", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' combine both models to one by using the mean:
map_biooracle_2050_rcp26_combined <- mean(rfmap_biooracle_2050_rcp26, gammap_biooracle_2050_rcp26)

plot(map_biooracle_2050_rcp26_combined)
writeRaster(map_biooracle_2050_rcp26_combined, filename="map_biooracle_2050_rcp26_combined", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))



#'create layerstack from all three rcp models:

map_biooracle_2050_all <- stack(map_biooracle_2050_rcp26_combined, map_biooracle_2050_rcp60_combined, map_biooracle_2050_rcp85_combined)
names(map_biooracle_2050_all) <- c("RCP 2.6", "RCP 6.0", "RCP 8.5")
map_biooracle_2050_all

writeRaster(map_biooracle_2050_all, filename="map_biooracle_2050_all", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
