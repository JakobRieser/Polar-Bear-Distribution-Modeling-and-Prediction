#' ====================================================================================================================================
#' Data preprocessing
#' ====================================================================================================================================

#' Select species records for which environmental information is available
#' ------------------------------------------------------------------------------------------------
Ursus_maritimus_bioclim <- Ursus_maritimus[complete.cases(extract(variables_bioclim_crop, Ursus_maritimus)), ]

#' Collinearity
#' ------------------------------------------------------------------------------------------------

##' Visual inspection of collinearity:

cm <- cor(getValues(variables_bioclim_crop), use = "complete.obs")

#' with plotcorr:
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))


##' Select an uncorrelated subset of environmental variables
subset_var_bioclim <- subset(variables_bioclim_crop, c("bio1", "bio2", "bio5", "bio13", "bio15", "bio19"))

#' Sampling of (pseudo-)absence points
#' ------------------------------------------------------------------------------------------------

#' The function randomPoints in package dismo allows to randomly select a certain number of random points,
#' and to adjust the probability of selecting a cell according to its size, which is relevant in lat-lon-grids,
#' where cells are of differing size

#' Selecting 2000 random background points, excluding cells where the species is present
set.seed(2)
background_bioclim <- randomPoints(subset_var_bioclim, 2000, Ursus_maritimus_bioclim)

plot(background_bioclim)


#' select only records where the full data is available
#' ------------------------------------------------------------------------------------------------

#' Select only one presence record in each cell of the environmental layer
presence_bioclim <- gridSample(Ursus_maritimus_bioclim, subset_var_bioclim, n = 1)

plot(presence_bioclim)

#' Now we combine the presence and background points, adding a column "species" that contains the information about presence (1) and background (0)
fulldata_bioclim <- SpatialPointsDataFrame(rbind(presence_bioclim, background_bioclim),
                                           data = data.frame("species_bioclim" = rep(c(1,0), 
                                                                                     c(nrow(presence_bioclim), nrow(background_bioclim)))),
                                           match.ID = FALSE,
                                           proj4string = CRS(projection(subset_var_bioclim)))

#' Add information of environmental conditions at point locations
fulldata_bioclim@data <- cbind(fulldata_bioclim@data, extract(subset_var_bioclim, fulldata_bioclim))

# Split data set into a training and test data set
set.seed(2)
fold_bioclim <- kfold(fulldata_bioclim, k = 5)
traindata_bioclim <- fulldata_bioclim[fold_bioclim != 1, ]
testdata_bioclim <- fulldata_bioclim[fold_bioclim == 1, ]


#' ====================================================================================================================================
#' Apply different SDMs
#' ====================================================================================================================================

#' We can now use a range of statistical methods to estimate the probability of species occurrence.
#' Unfortunately, there are often subtle differences in how the models are specified and in which data formats are useable

varnames_bioclim <- c("bio1", "bio2", "bio5", "bio13", "bio15", "bio19")


#' Generalized Additive Models
#' ------------------------------------------------------------------------------------------------

gammodel_bioclim <- gam(species_bioclim ~ s(bio1) + s(bio2) + s(bio5) + s(bio13) + s(bio15) + s(bio19),
                        family="binomial", data=traindata_bioclim)
summary(gammodel_bioclim)

plot(gammodel_bioclim)

#' Now we should do model selection: bio[15] does not contribute to the fit

##' Evaluate model on test data

#' a) Predict to test data
gamtest_bioclim <- predict(gammodel_bioclim, newdata = testdata_bioclim, type = "response")

#' b) Calculate performance indices
val.prob(gamtest_bioclim, testdata_bioclim[["species_bioclim"]])

#' Variable importance
source("varImpBiomod.R") #We assume, that this file is in the working directory (download at https://bitbucket.org/rsbiodiv/species_distribution_model/src/master/)

gamimp_bioclim <- varImpBiomod(gammodel_bioclim, varnames_bioclim,
                               traindata_bioclim)

barplot(100 * gamimp_bioclim/sum(gamimp_bioclim), ylab = "Variable importance (%)")

#' Response functions
plot(gammodel_bioclim, pages = 1)

#export to png:
png("gammodel_bioclim_response_functions.png", 800, 800)
plot(gammodel_bioclim, pages = 1)
dev.off()

#' Prediction map
gammap_bioclim <- predict(subset_var_bioclim, gammodel_bioclim, type = "response")

plot(gammap_bioclim)

#' export prediction map to disk:
writeRaster(gammap_bioclim, filename="gammap_bioclim", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))



#' Random Forest
#' ------------------------------------------------------------------------------------------------

#' randomForest requires the dependent variable to be a factor if we want to do classification
rftraindata_bioclim <- as(traindata_bioclim, "data.frame")
rftraindata_bioclim$species_bioclim <- factor(rftraindata_bioclim$species_bioclim)

#' TODO: check proper settings of random forest algorithm
rfmodel_bioclim <- randomForest(species_bioclim ~ bio1 + bio2 + bio5 + bio13 + bio15 + bio19, data = rftraindata_bioclim)

#' Evaluate model on test data

#' a) Predict to test data
rftest_bioclim <- predict(rfmodel_bioclim, newdata = testdata_bioclim, type = "prob")[,2]

#' b) Calculate performance indices
val.prob(rftest_bioclim, testdata_bioclim[["species_bioclim"]])

#' Variable importance
rfImp_bioclim <- importance(rfmodel_bioclim)
varImpPlot(rfmodel_bioclim)

#' Response functions
par(mfrow=c(3,2))

for (i in seq_along(varnames_bioclim)) {
  partialPlot(rfmodel_bioclim, rftraindata_bioclim, varnames_bioclim[i], xlab = varnames_bioclim[i], main="")  
}

#' Prediction map
rfmap_bioclim <- predict(subset_var_bioclim, rfmodel_bioclim, type = "prob", index = 2)

par(mfrow=c(1, 1))
plot(rfmap_bioclim)

#' export prediction map to disk:
writeRaster(rfmap_bioclim, filename="rfmap_bioclim", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' combine both models to one by using the mean:
map_bioclim_combined <- mean(rfmap_bioclim, gammap_bioclim)

plot(map_bioclim_combined)
writeRaster(map_bioclim_combined, filename="map_bioclim_combined", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

#' Plot predictions of several methods, using the same colour scheme
par(mfrow = c(2, 1), mar = c(3, 3, 1, 1))
brks <- seq(0, 1, by = 0.1)
arg <- list(at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2))
col <- rev(terrain.colors(length(brks) - 1))
plot(gammap_bioclim, breaks = brks, col = col, axis.args = arg)
plot(rfmap_bioclim, breaks = brks, col = col, axis.args = arg)
#plot(maxentmap, breaks = brks, col = col, axis.args = arg)

