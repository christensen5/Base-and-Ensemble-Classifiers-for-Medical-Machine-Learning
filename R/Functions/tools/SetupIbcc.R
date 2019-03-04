SetupIbcc <- function(FolderName, super=TRUE) {
	
	source("functions/tools/ApplyThresh.R")
	
	readpath <- paste0("datasets/",FolderName,"/Simpson/")
	savepath <- paste0("datasets/",FolderName,"/Simpson/")
	
	if(super==TRUE) {
	
		predictions.Mtrain = read.table(paste0(readpath,"predictions-Mtrain.data"), sep=",")
		predictions.Mtest  = read.table(paste0(readpath,"predictions-Mtest.data"), sep=",")
		
		input			= rbind(predictions.Mtrain, predictions.Mtest)
#		input.unthresh	= rbind(predictions.Mtrain, predictions.Mtest)
#		input			= ApplyThresh(input.unthresh)
	
		tv				= scan(paste0(readpath,"tv-Mtrain.data"))
		subjects			= seq(1,length(tv),1)
		gold			= cbind(subjects, tv)
		write.table(input, paste0(savepath,"Input.csv"), quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)

		write.table(gold, paste0(savepath,"Gold.csv"), quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
	}
	
	if(super==FALSE) {
		predictions.Mtest = read.table(paste0(readpath,"predictions-bigTrain.data"), sep=",")

		input 			= predictions.Mtest	
#		input			= ApplyThresh(predictions.Mtest)
		
		write.table(input, paste0(savepath,"Input-unsup.csv"), quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
		
	}
	cat("CHANGE CONFIG FILE\n \n")	
}