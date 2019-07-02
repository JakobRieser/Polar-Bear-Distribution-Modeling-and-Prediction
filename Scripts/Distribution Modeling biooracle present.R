#' ====================================================================================================================================
#' Data preprocessing
#' ====================================================================================================================================

#' Select species records for which environmental information is available
#' ------------------------------------------------------------------------------------------------
Ursus_maritimus_biooracle <- Ursus_maritimus[complete.cases(extract(variables_biooracle_crop, Ursus_maritimus)), ]

#' Collinearity
#' ------------------------------------------------------------------------------------------------

##' Visual inspection of collinearity:

cm_biooracle <- cor(getValues(variables_biooracle_crop), use = "complete.obs")

#' with plotcorr:
plotcorr(cm_biooracle, col=ifelse(abs(cm) > 0.7, "red", "grey"))


##' Select an uncorrelated subset of environmental variables
subset_var_biooracle <- subset(variables_biooracle_crop, c(names(variables_biooracle_crop)))

#' Sampling of (pseudo-)absence points
#' ------------------------------------------------------------------------------------------------

#' The function randomPoints in package dismo allows to randomly select a certain number of random points,
#' and to adjust the probability of selecting a cell according to its size, which is relevant in lat-lon-grids,
#' where cells are of differing size

#' Selecting 2000 random background points, excluding cells where the species is present
set.seed(2)
background_biooracle <- randomPoints(subset_var_biooracle, 2000, Ursus_maritimus_biooracle)

plot(background_biooracle)


#' select only good records
#' ------------------------------------------------------------------------------------------------

#' Select only one presence record in each cell of the environmental layer
presence_biooracle <- gridSample(Ursus_maritimus_biooracle, subset_var_biooracle, n = 1)

plot(presence_biooracle)

#' Now we combine the presence and background points, adding a column "species" that contains the information about presence (1) and background (0)
fulldata_biooracle <- SpatialPointsDataFrame(rbind(presence_biooracle, background_biooracle),
                                           data = data.frame("species_biooracle" = rep(c(1,0), 
                                                                                     c(nrow(presence_biooracle), nrow(background_biooracle)))),
                                           match.ID = FALSE,
                                           proj4string = CRS(projection(subset_var_biooracle)))

#' Add information of environmental conditions at point locations
fulldata_biooracle@data <- cbind(fulldata_biooracle@data, extract(subset_var_biooracle, fulldata_biooracle))

# Split data set into a training and test data set
set.seed(2)
fold_biooracle <- kfold(fulldata_biooracle, k = 5)
traindata_biooracle <- fulldata_biooracle[fold_biooracle != 1, ]
testdata_biooracle <- fulldata_biooracle[fold_biooracle == 1, ]


#' ====================================================================================================================================
#' Apply different SDMs
#' ====================================================================================================================================

#' We can now use a range of statistical methods to estimate the probability of species occurrence.
#' Unfortunately, there are often subtle differences in how the models are specified and in which data formats are useable

varnames_biooracle <- c(names(variables_biooracle_crop))


#' Generalized Additive Models
#' ------------------------------------------------------------------------------------------------

gammodel_biooracle <- gam(species_biooracle ~ s(BO2_icethickmean_ss) + s(BO2_icethickrange_ss) + s(BO2_icecovermean_ss) + s(BO2_icecoverrange_ss) + s(BO2_curvelmean_ss) +s(BO2_tempmean_ss),
                        family="binomial", data=traindata_biooracle)
summary(gammodel_biooracle)

plot(gammodel_biooracle)

#' Now we should do model selection: bio[15] does not contribute to the fit

##' Evaluate model on test data

#' a) Predict to test data
gamtest_biooracle <- predict(gammodel_biooracle, newdata = testdata_biooracle, type = "response")

#' b) Calculate performance indices
val.prob(gamtest_biooracle, testdata_biooracle[["species_biooracle"]])

#' Variable importance
source("varImpBiomod.R") #We assume, that this file is in the working directory (download at https://bitbucket.org/rsbiodiv/species_distribution_model/src/master/)

gamimp_biooracle <- varImpBiomod(gammodel_biooracle, varnames_biooracle,
                               traindata_biooracle)

barplot(100 * gamimp_biooracle/sum(gamimp_biooracle), ylab = "Variable importance (%)")

#' Response functions
plot(gammodel_biooracle, pages = 1)

#export to png:
png("gammodel_biooracle_response_functions.png", 800, 800)
plot(gammodel_biooracle, pages = 1)
dev.off()

#' Prediction map
gammap_biooracle <- predict(subset_var_biooracle, gammodel_biooracle, type = "response")

plot(gammap_biooracle)

#' export prediction map to disk:
writeRaster(gammap_biooracle, filename="gammap_biooracle", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' Random Forest
#' ------------------------------------------------------------------------------------------------

#' randomForest requires the dependent variable to be a factor if we want to do classification
rftraindata_biooracle <- as(traindata_biooracle, "data.frame")
rftraindata_biooracle$species_biooracle <- factor(rftraindata_biooracle$species_biooracle)

#' TODO: check proper settings of random forest algorithm
rfmodel_biooracle <- randomForest(species_biooracle ~ BO2_icethickmean_ss + BO2_icethickrange_ss + BO2_icecovermean_ss + BO2_icecoverrange_ss + BO2_curvelmean_ss + BO2_tempmean_ss, data = rftraindata_biooracle)

#' Evaluate model on test data

#' a) Predict to test data
rftest_biooracle <- predict(rfmodel_biooracle, newdata = testdata_biooracle, type = "prob")[,2]

#' b) Calculate performance indices
val.prob(rftest_biooracle, testdata_biooracle[["species_biooracle"]])

#' Variable importance
rfImp_biooracle <- importance(rfmodel_biooracle)
varImpPlot(rfmodel_biooracle)

#' Response functions
par(mfrow=c(3,2))

for (i in seq_along(varnames_biooracle)) {
  partialPlot(rfmodel_biooracle, rftraindata_biooracle, varnames_biooracle[i], xlab = varnames_biooracle[i], main="")  
}

#' Prediction map
rfmap_biooracle <- predict(subset_var_biooracle, rfmodel_biooracle, type = "prob", index = 2)

par(mfrow=c(1, 1))
plot(rfmap_biooracle)

#' export prediction map to disk:
writeRaster(rfmap_biooracle, filename="rfmap_biooracle", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))


#' combine both models to one by using the mean:
map_biooracle_combined <- mean(rfmap_biooracle, gammap_biooracle)

plot(map_biooracle_combined)
writeRaster(map_biooracle_combined, filename="map_biooracle_combined", format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

#' Plot predictions of several methods, using the same colour scheme
par(mfrow = c(3, 1), mar = c(3, 3, 1, 1))
brks <- seq(0, 1, by = 0.1)
arg <- list(at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2))
col <- rev(terrain.colors(length(brks) - 1))
plot(gammap_biooracle, breaks = brks, col = col, axis.args = arg)
plot(rfmap_biooracle, breaks = brks, col = col, axis.args = arg)
plot(map_biooracle_combined, breaks = brks, col = col, axis.args = arg)
