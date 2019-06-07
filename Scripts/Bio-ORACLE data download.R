#' ====================================================================================================================================
#' Bio ORACLE data download:
#' ====================================================================================================================================

#' Install and load the needed packages:
#' ------------------------------------------------------------------------------------------------

install.packages("sdmpredictors")
install.packages("leaflet")

library(sdmpredictors)

# Explore datasets in the package 
list_datasets() 

# Explore layers in a dataset 
list_layers() 

# Download specific layers to the current directory 
bathy <- load_layers(c("BO_bathymin", "BO_bathymean", "BO_bathymax")) 

# Check layer statistics 
layer_stats() 

# Check Pearson correlation coefficient between layers 
layers_correlation() 

list_datasets(marine=TRUE)
Bio_ORACLE_layers <- list_layers("Bio-ORACLE")
