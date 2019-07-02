#' Download and read Bio-ORACLE variables
#' ------------------------------------------------------------------------------------------------


# Download the specific layers to the current directory 
variables_biooracle_noic <- load_layers(c("BO2_icethickmean_ss", "BO2_icethickrange_ss", "BO2_curvelmean_ss", "BO2_tempmean_ss"), equalarea = FALSE) 

plot(variables_biooracle_noic)

#' crop to the extent of the study area:
variables_biooracle_noic_crop <- crop(variables_biooracle_noic, extent(study_area) + 10)

#' reprojection and resampling to fit the bioclim variables:
variables_biooracle_noic_crop <- projectRaster(variables_biooracle_noic_crop, crs=polar_projection, method="bilinear")
variables_biooracle_noic_crop <- resample(variables_biooracle_noic_crop, variables_biooracle_noic_crop)

#' export to drive:
writeRaster(variables_biooracle_noic_crop, filename="variables_biooracle_noic_crop", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

variables_biooracle_noic_crop <- stack("variables_biooracle_noic_crop.tif")
names(variables_biooracle_noic_crop) <- c("BO2_icethickmean_ss", "BO2_icethickrange_ss", "BO2_curvelmean_ss", "BO2_tempmean_ss")


#' ====================================================================================================================================
#' Data preprocessing
#' ====================================================================================================================================

#' Select species records for which environmental information is available
#' ------------------------------------------------------------------------------------------------
Ursus_maritimus_biooracle_noic <- Ursus_maritimus[complete.cases(extract(variables_biooracle_noic_crop, Ursus_maritimus)), ]

#' Collinearity
#' ------------------------------------------------------------------------------------------------

##' Visual inspection of collinearity:

cm_biooracle_noic <- cor(getValues(variables_biooracle_noic_crop), use = "complete.obs")

#' with plotcorr:
plotcorr(cm_biooracle_noic, col=ifelse(abs(cm) > 0.7, "red", "grey"))


##' Select an uncorrelated subset of environmental variables
subset_var_biooracle_noic <- subset(variables_biooracle_noic_crop, c(names(variables_biooracle_noic_crop)))

#' Sampling of (pseudo-)absence points
#' ------------------------------------------------------------------------------------------------

#' The function randomPoints in package dismo allows to randomly select a certain number of random points,
#' and to adjust the probability of selecting a cell according to its size, which is relevant in lat-lon-grids,
#' where cells are of differing size

#' Selecting 2000 random background points, excluding cells where the species is present
set.seed(2)
background_biooracle_noic <- randomPoints(subset_var_biooracle_noic, 2000, Ursus_maritimus_biooracle_noic)

plot(background_biooracle_noic)


#' select only good records
#' ------------------------------------------------------------------------------------------------

#' Select only one presence record in each cell of the environmental layer
presence_biooracle_noic <- gridSample(Ursus_maritimus_biooracle_noic, subset_var_biooracle_noic, n = 1)

plot(presence_biooracle_noic)

#' Now we combine the presence and background points, adding a column "species" that contains the information about presence (1) and background (0)
fulldata_biooracle_noic <- SpatialPointsDataFrame(rbind(presence_biooracle_noic, background_biooracle_noic),
                                             data = data.frame("species_biooracle_noic" = rep(c(1,0), 
                                                                                         c(nrow(presence_biooracle_noic), nrow(background_biooracle_noic)))),
                                             match.ID = FALSE,
                                             proj4string = CRS(projection(subset_var_biooracle_noic)))

#' Add information of environmental conditions at point locations
fulldata_biooracle_noic@data <- cbind(fulldata_biooracle_noic@data, extract(subset_var_biooracle_noic, fulldata_biooracle_noic))

# Split data set into a training and test data set
set.seed(2)
fold_biooracle_noic <- kfold(fulldata_biooracle_noic, k = 5)
traindata_biooracle_noic <- fulldata_biooracle_noic[fold_biooracle_noic != 1, ]
testdata_biooracle_noic <- fulldata_biooracle_noic[fold_biooracle_noic == 1, ]


#' ====================================================================================================================================
#' Apply different SDMs
#' ====================================================================================================================================

#' We can now use a range of statistical methods to estimate the probability of species occurrence.
#' Unfortunately, there are often subtle differences in how the models are specified and in which data formats are useable

varnames_biooracle_noic <- c(names(variables_biooracle_noic_crop))


#' Generalized Additive Models
#' ------------------------------------------------------------------------------------------------

gammodel_biooracle_noic <- gam(species_biooracle_noic ~ s(BO2_icethickmean_ss) + s(BO2_icethickrange_ss) + s(BO2_curvelmean_ss) +s(BO2_tempmean_ss),
                          family="binomial", data=traindata_biooracle_noic)
summary(gammodel_biooracle_noic)

plot(gammodel_biooracle_noic)

#' Now we should do model selection: bio[15] does not contribute to the fit

##' Evaluate model on test data

#' a) Predict to test data
gamtest_biooracle_noic <- predict(gammodel_biooracle_noic, newdata = testdata_biooracle_noic, type = "response")

#' b) Calculate performance indices
val.prob(gamtest_biooracle_noic, testdata_biooracle_noic[["species_biooracle_noic"]])

#' Variable importance
source("varImpBiomod.R") #We assume, that this file is in the working directory (download at https://bitbucket.org/rsbiodiv/species_distribution_model/src/master/)

gamimp_biooracle_noic <- varImpBiomod(gammodel_biooracle_noic, varnames_biooracle_noic,
                                 traindata_biooracle_noic)

barplot(100 * gamimp_biooracle_noic/sum(gamimp_biooracle_noic), ylab = "Variable importance (%)")

#' Response functions
plot(gammodel_biooracle_noic, pages = 1)

#export to png:
png("gammodel_biooracle_noic_response_functions.png", 800, 800)
plot(gammodel_biooracle_noic, pages = 1)
dev.off()

#' Prediction map
gammap_biooracle_noic <- predict(subset_var_biooracle_noic, gammodel_biooracle_noic, type = "response")

plot(gammap_biooracle_noic)

#' export prediction map to disk:
writeRaster(gammap_biooracle_noic, filename="gammap_biooracle_noic", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' Random Forest
#' ------------------------------------------------------------------------------------------------

#' randomForest requires the dependent variable to be a factor if we want to do classification
rftraindata_biooracle_noic <- as(traindata_biooracle_noic, "data.frame")
rftraindata_biooracle_noic$species_biooracle_noic <- factor(rftraindata_biooracle_noic$species_biooracle_noic)

#' TODO: check proper settings of random forest algorithm
rfmodel_biooracle_noic <- randomForest(species_biooracle ~ BO2_icethickmean_ss + BO2_icethickrange_ss + BO2_curvelmean_ss + BO2_tempmean_ss, data = rftraindata_biooracle)

#' Evaluate model on test data

#' a) Predict to test data
rftest_biooracle_noic <- predict(rfmodel_biooracle_noic, newdata = testdata_biooracle_noic, type = "prob")[,2]

#' b) Calculate performance indices
val.prob(rftest_biooracle_noic, testdata_biooracle_noic[["species_biooracle_noic"]])

#' Variable importance
rfImp_biooracle_noic <- importance(rfmodel_biooracle_noic)
varImpPlot(rfmodel_biooracle_noic)

#' Response functions
par(mfrow=c(3,2))

for (i in seq_along(varnames_biooracle_noic)) {
  partialPlot(rfmodel_biooracle_noic, rftraindata_biooracle_noic, varnames_biooracle_noic[i], xlab = varnames_biooracle_noic[i], main="")  
}

#' Prediction map
rfmap_biooracle_noic <- predict(subset_var_biooracle_noic, rfmodel_biooracle_noic, type = "prob", index = 2)

par(mfrow=c(1, 1))
plot(rfmap_biooracle_noic)

#' export prediction map to disk:
writeRaster(rfmap_biooracle_noic, filename="rfmap_biooracle_noic", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' combine both models to one by using the mean:
map_biooracle_noic_combined <- mean(rfmap_biooracle_noic, gammap_biooracle_noic)

plot(map_biooracle_noic_combined)
writeRaster(map_biooracle_noic_combined, filename="map_biooracle_noic_combined", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

#' Plot predictions of several methods, using the same colour scheme
par(mfrow = c(3, 1), mar = c(3, 3, 1, 1))
brks <- seq(0, 1, by = 0.1)
arg <- list(at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2))
col <- rev(terrain.colors(length(brks) - 1))
plot(gammap_biooracle_noic, breaks = brks, col = col, axis.args = arg)
plot(rfmap_biooracle_noic, breaks = brks, col = col, axis.args = arg)
plot(map_biooracle_noic_combined, breaks = brks, col = col, axis.args = arg)
