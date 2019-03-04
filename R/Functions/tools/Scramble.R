Scramble <- function(data.in) {
	
	stopifnot(is.data.frame(data.in) || is.vector(data.in))
	
	set.seed(1234)
	
	if(is.data.frame(data.in)) {
	
		nrow=nrow(data.in)
	
		c = sample(seq(1,nrow,1))
	
		return(data.in[c,])
	
	}
	
	if(is.vector(data.in)) {
		
		length=length(data.in)
		
		c = sample(seq(1,length,1))
		
		return(data.in[c])
	}
	
}