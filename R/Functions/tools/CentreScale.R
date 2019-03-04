## This function will take a matrix of variables, and process the columns to leave them with median of 0 and variance of 1.

CentreScale <- function(data.raw) {
	
	## Assertions about the input
	stopifnot(is.data.frame(data.raw) || is.matrix(data.raw))
	
	## Get useful values
	nrow = nrow(data.raw)
	ncol = ncol(data.raw)
	
	## Scale the columns
	data.scaled = matrix(nrow=nrow, ncol=ncol)
	
	for (i in 1:ncol) {
		data.scaled[,i] <- scale(data.raw[,i])
	}
	
	return(data.scaled)
	
} 