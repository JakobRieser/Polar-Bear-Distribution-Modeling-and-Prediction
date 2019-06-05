
##############WIP##############

#' Maxent
#' ------------------------------------------------------------------------------------------------

#' The following code assumes that the column with the species informatio is in the first position
#maxentmodel <- maxent(traindata@data[, -1], traindata[["species"]], 
#                      args = c("nothreshold", 
#                               "nohinge"))

#' Model evaluation on test data
#maxenttest <- predict(maxentmodel, testdata)
#val.prob(maxenttest, testdata[["species"]])

#' Alternatively, we can use the evaluate function
#maxente <- evaluate(p = maxenttest[testdata[["species"]] == 1],
#                    a = maxenttest[testdata[["species"]] == 0])

#' Show variable importance
#plot(maxentmodel)

#' Plot response functions
#response(maxentmodel)

#' Prediction map
#maxentmap <- predict(maxentmodel, env)
#plot(maxentmap)