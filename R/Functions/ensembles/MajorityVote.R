## This function accepts a matrix of base predictions from Base Learners, wherein columns represent probabilistic predictions from each BL and rows represent points from the data set on which we are predicting.
## The function then applies a threshold above which to consider each prediction as TRUE or FALSE, and then for each data point, takes a vote of the learners to return either a majority TRUE/FALSE vote or the proportion of learners which voted for TRUE.

MajorityVote <- function(basePredictions, thresh=0.5, prob=FALSE) {
	
	##Assertions about the inputs
	stopifnot(is.matrix(basePredictions) || is.data.frame(basePredictions))
	stopifnot(is.numeric(thresh))
	stopifnot(thresh >= 0 && thresh <= 1)
	
	rows = nrow(basePredictions)
	cols = ncol(basePredictions)
	
	VotePredictions = vector(mode="integer", length=rows)

	##Apply the specified threshold to each entry of the prediction matrix (default=0.5) and vote!
	for (i in seq(1,rows,1)) {
		for (j in seq(1,cols,1)) {
			
			if (basePredictions[i,j] < thresh) {
				basePredictions[i,j]=0
				
			} else {
				basePredictions[i,j]=1
			}
			
		}	
		
		if(prob=='FALSE') {
			if(sum(basePredictions[i,]) < (cols/2)) VotePredictions[i]=0
			else VotePredictions[i] = 1			
		}
		else{
			VotePredictions[i] = sum(basePredictions[i,]/cols)
		}	
	}
	
	return(VotePredictions)

}