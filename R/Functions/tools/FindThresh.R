##Function to find the best probability threshold above which to consider mail as spam, based the ROC & AUC statistics.

FindThresh <- function(RAWpredictions, targetValues){
	
	##----------------------------------------------------------------------
	## ASSERTIONS ABOUT THE INPUT ------------------------------------------
	##----------------------------------------------------------------------
	stopifnot(is.logical(targetValues))
	stopifnot(is.vector(RAWpredictions))

	##initialise vector of thresholds to be tested
	thresholds <- seq(0,1,0.5)

##21 for 0.05
	for (i in 1:3){
		##apply the threshold to finalise the predictions
		predictions = RAWpredictions
	
		for (j in 1:length(predictions)){
			if (predictions[j] < thresholds[i]){
				predictions[j] = FALSE
			
			} else {
				predictions[j] = TRUE
			}
		}
	cat("At threshold ", thresholds[i], " the predictions are:")
	print(predictions)
	
	}
	
	return()
}