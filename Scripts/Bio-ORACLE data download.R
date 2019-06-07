#' ====================================================================================================================================
#' Bio ORACLE data download:
#' ====================================================================================================================================

#' Install and load the needed packages:
#' ------------------------------------------------------------------------------------------------

install.packages("sdmpredictors")
library(sdmpredictors)

# Explore datasets in the package 
datasets <- list_datasets() 
View(datasets)

# Explore layers in a dataset and decide which one are useful for the analysis 
layers <- list_layers("Bio-ORACLE", marine=T) 
View(layers)

# Download the specific layers to the current directory 
Sea_ice <- load_layers(c("BO2_icethickmean_ss", "BO2_icethickrange_ss", "BO2_icecovermean_ss", "BO2_icecoverrange_ss"), equalarea = FALSE) 
rm(Sea_ice_thickness_mean)

# Check layer statistics 
layer_stats("BO2_icethickmean_ss") 

plot(Sea_ice)

Sea_ice_crop <- crop(Sea_ice, extent(study_area) + 10)

#reprojection and resampling necessary for stacking:
Sea_ice_crop <- projectRaster(Sea_ice_crop, crs=polar_projection, method="bilinear")
Sea_ice_crop <- resample(Sea_ice_crop, biocrop)

#the stack:
combined_data_test <- raster::stack(biocrop, Sea_ice_crop)


combined_data_test
names(combined_data_test) <-c(names(biocrop), "bio20", "bio21", "bio22", "bio23")

names(combined_data_test)
