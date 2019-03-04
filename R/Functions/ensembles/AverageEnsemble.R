##This function will accept predictions on a single data set from a variety of base classifiers, and combine their predictions into a single prediction by averaging.

##Each column of the array of base Classifier predictions should represent the predictions of a single Classifier over all of the data set, with the rows representing the different Classifiers' predictions for a single data point .

AverageEnsemble <- function(basePredictions) {

	##Assertions about the inputs
	stopifnot(is.matrix(basePredictions) || is.data.frame(basePredictions))
	
	nrow <- nrow(basePredictions)
	ncol <- ncol(basePredictions)
	avgPredictions <- vector(mode = "numeric", length = nrow)

	##Fill avgPredictions with average of each row of basePredictions	
	for (i in 1:nrow) {
		avgPredictions[i] = sum(basePredictions[i,]) / ncol
	}
		
	return(avgPredictions)	
}