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
#' Algorithms: GAM, RF, MaxEnt


#' ====================================================================================================================================
#' Prepare your environment:
#' ====================================================================================================================================

#' Install and load the needed packages:
#' ------------------------------------------------------------------------------------------------

Packages <- c("rms", "raster", "mgcv", "randomForest", "dismo", "rgdal", "ellipse", "rJava", "XML")

install.packages(Packages)

options(java.parameters = "-Xmx8g" )

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
  atudy_area <- readOGR("./GIS/study_area.shp")
  #' './' refers to the current working directory, i.e. we are specifying as the first argument of readOGR, the dsn, the subdirectory "GIS" within the current working directory
  #' make sure there is no trailing '/' in the value of the dsn argument, i.e. do **not** use "./GIS/"
  #' when importing shapefiles, drop the suffix from the layer argument, i.e. do **not** use "africa_dissolved.shp"
} else {
  #' First, get the coordinates into a 2-column matrix:
  x_coord <- c(-180, -180, 180, 180)
  y_coord <- c(90,  40,  40, 90)
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

bio <- raster::getData("worldclim", var = "bio", res = 2.5)

#' Plot the first raster layer, i.e. annual mean temperature
plot(raster(bio, 1))

# Add the outline of the study area
plot(study_area, add=TRUE)

#' Crop to study area extent (with a 5 degree buffer in each direction)
biocrop <- crop(bio, extent(study_area) + 10)

#' Plot the first raster layer of the cropped climate data
plot(raster(biocrop, 1))
plot(sps, add=TRUE)


#' Download and read occurrence data
#' ------------------------------------------------------------------------------------------------

#' Read occurrence points. If they exist locally, use the local file.
#' If they do not exist, download from [gbif](http://www.gbif.org)

if (file.exists("./GIS/Occurrence Data/Ursus maritimus.mif")) {
  Ursus_maritimus <- readOGR("./GIS/Occurrence Data/Ursus maritimus.mif", layer = "Ursus maritimus")
} else {
  #' Download species location data from gbif
  Ursus_maritimus_0 <- gbif("Ursus", "maritimus")
  Ursus_maritimus <- subset(Ursus_maritimus_0,select=c("lat","lon"))
  Ursus_maritimus <- na.omit(species)
  coordinates(Ursus_maritimus) <- c("lon", "lat")  # set spatial coordinates
  #' Add projection information
  proj4string(Ursus_maritimus) <- CRS("+proj=longlat +datum=WGS84")
  #' Convert to Spatial Polygons Data Frame
  Ursus_maritimus.df <- data.frame( ID=1:length(Ursus_maritimus)) 
  Ursus_maritimus <- SpatialPointsDataFrame(Ursus_maritimus, Ursus_maritimus.df)
  #' Save species records in mif-format (preserves full column names)
  writeOGR(Ursus_maritimus, "./GIS/Occurrence Data", 
           "Ursus maritimus", driver="MapInfo File", dataset_options="FORMAT=MIF", overwrite_layer = TRUE)
}

dev.off ()
plot(raster(biocrop, 1))
plot(Ursus_maritimus, add = TRUE)


#' ====================================================================================================================================
#' Data preprocessing
#' ====================================================================================================================================

#' Select species records for which environmental information is available
#' ------------------------------------------------------------------------------------------------
Ursus_maritimus <- Ursus_maritimus[complete.cases(extract(biocrop, Ursus_maritimus)), ]

#' Collinearity
#' ------------------------------------------------------------------------------------------------

##' Visual inspection of collinearity:

cm <- cor(getValues(bio), use = "complete.obs")

#' with plotcorr:
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

#' with ggcorr:
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")

ggcorr(cm,
  label = TRUE,
  label_alpha = TRUE)

ggcorr(
  cm,
  method = c("pairwise", "pearson"),
  cor_matrix = NULL,
  nbreaks = NULL,
  digits = 2,
  name = "p",
  low = "#3B9AB2",
  mid = "#EEEEEE",
  high = "#F21A00",
  midpoint = 0,
  palette = NULL,
  geom = "tile",
  min_size = 2,
  max_size = 6,
  label = T,
  label_alpha = FALSE,
  label_color = "gray20",
  label_round = 1,
  label_size = 3,
  limits = TRUE,
  drop = !limits,
  layout.exp = ,
  legend.position = "right",
  legend.size = 9)

#' with corrplot:
install.packages("corrplot")
library(corrplot)

corrplot(cm, method = "circle")

##' Select an uncorrelated subset of environmental variables
env <- subset(biocrop, c("bio2", "bio13", "bio14", "bio15", "bio19"))

#' Sampling of (pseudo-)absence points
#' ------------------------------------------------------------------------------------------------

#' The function randomPoints in package dismo allows to randomly select a certain number of random points,
#' and to adjust the probability of selecting a cell according to its size, which is relevant in lat-lon-grids,
#' where cells are of differing size

#' Selecting 2000 random background points, excluding cells where the species is present
set.seed(2)
background <- randomPoints(env, 2000, Ursus_maritimus)

plot(background)


#' A title
#' ------------------------------------------------------------------------------------------------

#' Select only one presence record in each cell of the environmental layer
presence <- gridSample(Ursus_maritimus, env, n = 1)

plot(presence)

#' Now we combine the presence and background points, adding a column "species" that contains the information about presence (1) and background (0)
fulldata <- SpatialPointsDataFrame(rbind(presence, background),
                                   data = data.frame("species" = rep(c(1,0), 
                                                                     c(nrow(presence), nrow(background)))),
                                   match.ID = FALSE,
                                   proj4string = CRS(projection(env)))

#' Add information of environmental conditions at point locations
fulldata@data <- cbind(fulldata@data, extract(env, fulldata))

# Split data set into a training and test data set
set.seed(2)
fold <- kfold(fulldata, k = 5)
traindata <- fulldata[fold != 1, ]
testdata <- fulldata[fold == 1, ]


#' ====================================================================================================================================
#' Apply different SDMs
#' ====================================================================================================================================

#' We can now use a range of statistical methods to estimate the probability of species occurrence.
#' Unfortunately, there are often subtle differences in how the models are specified and in which data formats are useable

varnames <- c("bio2", "bio13", "bio14", "bio15", "bio19")


#' Generalized Additive Models
#' ------------------------------------------------------------------------------------------------

gammodel <- gam(species ~ s(bio2) + s(bio13) + s(bio14) + s(bio15) + s(bio19),
                family="binomial", data=traindata)
summary(gammodel)

plot(gammodel)

#' Now we should do model selection: bio[14] does not contribute to the fit

##' Evaluate model on test data

#' a) Predict to test data
gamtest <- predict(gammodel, newdata = testdata, type = "response")

#' b) Calculate performance indices
val.prob(gamtest, testdata[["species"]])

#' Variable importance
source("varImpBiomod.R") #We assume, that this file is in the working directory (download at https://bitbucket.org/rsbiodiv/species_distribution_model/src/master/)

gamimp <- varImpBiomod(gammodel, varnames,
                       traindata)

barplot(100 * gamimp/sum(gamimp), ylab = "Variable importance (%)")

#' Response functions
plot(gammodel, pages = 1)

#' png("gammodel_resp.png", 800, 800)
#' plot(gammodel, pages = 1)
dev.off()

#' Prediction map
gammap <- predict(env, gammodel, type = "response")

plot(gammap)


#' Random Forest
#' ------------------------------------------------------------------------------------------------

#' randomForest requires the dependent variable to be a factor
#' if we want to do classification
rftraindata <- as(traindata, "data.frame")
rftraindata$species <- factor(rftraindata$species)

#' TODO: check proper settings of random forest algorithm
rfmodel <- randomForest(species ~ bio2 + bio13 + bio14 + bio15 + bio19, data = rftraindata)

#' Evaluate model on test data

#' a) Predict to test data
rftest <- predict(rfmodel, newdata = testdata, type = "prob")[,2]

#' b) Calculate performance indices
val.prob(rftest, testdata[["species"]])

#' Variable importance
rfImp <- importance(rfmodel)
varImpPlot(rfmodel)

#' Response functions
par(mfrow=c(3,2))

for (i in seq_along(varnames)) {
  partialPlot(rfmodel, rftraindata, varnames[i], xlab = varnames[i], main="")  
}

#' Prediction map
rfmap <- predict(env, rfmodel, type = "prob", index = 2)

par(mfrow=c(1, 1))
plot(rfmap)


##############WIP

#' Maxent
#' ------------------------------------------------------------------------------------------------

#' The following code assumes that the column with the species informatio is in the first position
maxentmodel <- maxent(traindata@data[, -1], traindata[["species"]], 
                      args = c("nothreshold", 
                               "nohinge"))

#' Model evaluation on test data
maxenttest <- predict(maxentmodel, testdata)
val.prob(maxenttest, testdata[["species"]])

#' Alternatively, we can use the evaluate function
maxente <- evaluate(p = maxenttest[testdata[["species"]] == 1],
                    a = maxenttest[testdata[["species"]] == 0])

#' Show variable importance
plot(maxentmodel)

#' Plot response functions
response(maxentmodel)

#' Prediction map
maxentmap <- predict(maxentmodel, env)
plot(maxentmap)

#' Plot predictions of several methods, using the same
#' colour scheme
par(mfrow = c(3, 1), mar = c(3, 3, 1, 1))
brks <- seq(0, 1, by = 0.1)
arg <- list(at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2))
col <- rev(terrain.colors(length(brks) - 1))
plot(gammap, breaks = brks, col = col, axis.args = arg)
plot(rfmap, breaks = brks, col = col, axis.args = arg)
plot(maxentmap, breaks = brks, col = col, axis.args = arg)
