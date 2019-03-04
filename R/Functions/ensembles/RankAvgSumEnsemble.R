# This function takes a matrix of base predictions, 'rank averages' each set of predictions, and takes the average of the rank averaged predictions for each data point (would have taken sum but PLOTROCCURVE needs values between 0-1).

RankAvgSumEnsemble <- function(basePredictions) {
	
	stopifnot(is.data.frame(basePredictions) || is.matrix(basePredictions))
	
	rankedPredictions <- matrix(nrow=nrow(basePredictions), ncol=ncol(basePredictions))
	for(i in seq(1,ncol(basePredictions),1)) {
		rankedPredictions[,i] = ((rank(basePredictions[,i], ties.method="first") - rep(1, length(basePredictions[,i]))) / (length(basePredictions[,i]) - 1))
	}
	
	finalPredictions <- vector(mode="numeric", length=nrow(rankedPredictions))
	divider <- ncol(rankedPredictions)
	for(j in seq(1,nrow(rankedPredictions),1)) {
		finalPredictions[j] = (sum(rankedPredictions[j,]) / divider)
	}
	
	return(finalPredictions)
}