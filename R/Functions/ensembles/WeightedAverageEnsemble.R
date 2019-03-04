##This function will accept predictions on a single data set from a variety of base classifiers, and combine their predictions into a single prediction by weighted averaging (based on the accuracy of each classifier on the data set as quantified by the AUC metric).

##Each column of the array of base Classifier predictions should represent the predictions of a single Classifier over all of the data set, with the rows representing the different Classifiers' predictions for a single data point. baseClassifierAUC is a vector with each entry representing the AUC score of the base classifiers, IN THE SAME ORDER AS THEY APPEAR IN basePredictions.

WeightedAverageEnsemble <- function(basePredictions, baseClassifierAUC) {

	##Assertions about the inputs
	stopifnot(is.matrix(basePredictions) || is.data.frame(basePredictions))
	stopifnot(is.vector(baseClassifierAUC))

	nrow <- nrow(basePredictions)
	ncol <- ncol(basePredictions)
	avgPredictions <- vector(mode = "numeric", length = nrow)
	
	##Check the number of classifiers giving predictions is the same as the number of classifiers whos AUC has been provided.
	stopifnot(ncol == length(baseClassifierAUC))

	
	for (i in 1:nrow) {
		avgPredictions[i] = sum(basePredictions[i,] * baseClassifierAUC) / sum(baseClassifierAUC)
	}
		
	return(avgPredictions)	
}