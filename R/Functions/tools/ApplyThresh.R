ApplyThresh <- function(unthresh) {
	
	stopifnot(is.data.frame(unthresh))
	
	nrow = nrow(unthresh)
	ncol = ncol(unthresh)
	
	thresh = matrix(nrow=nrow, ncol=ncol)
	
	for (i in seq(1,nrow,1)) {
		for (j in seq(1,ncol,1)) {
			
			if(unthresh[i,j]<0.5) {
				thresh[i,j] = 0
			} else thresh[i,j] = 1
			
		}
		
	}
	
	return(thresh)
}