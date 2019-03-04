##This function will accept training and testing data for base and meta learners, then run the base classifiers and combine their predictions using various ensemble methods, returning the ensemble method predictions on the data.Mtest set, as well as the AUC metric for each ensemble method's predictions on said set.

## The function assumes as input:
## data.Btrain, data.Mtrain, data.Mtest : 3 (nItems * nFeatures) data frames
## targetVal.btrain, targetVal.Mtrain, targetVal.Mtest : 3 binary vectors (representing correct classifications for each item in the above data frames)
## output : a variable controlling whether to return a dataframe of all the ensemble model predictions, or a dataframe of AUC scores and 95% CI for each ensemble model.
## save : a boolean variable controlling whether the AUC scores & 95%CI values will be written to a file directly by this function

##It returns a data frame containing predictions probabilities for each ensemble method. These are the predicted probability of the outcome==1 as defined in the file Data Set Info.docx

EnsembleModels2 <- function(data.Btrain, data.Mtrain, data.Mtest, targetVal.Btrain, targetVal.Mtrain, targetVal.Mtest, output=c('predictions','AUC'), save=FALSE) {
	
	##----------------------------------------------------------------------
  	## LOAD LIBRARIES & FUNCTIONS ------------------------------------------
 	##----------------------------------------------------------------------
 	library("pROC")
 	
 	source("functions/base classifiers/ClassifierModels.R")
	
	source("functions/ensembles/AverageEnsemble.R")
	source("functions/ensembles/MajorityVote.R")
	source("functions/ensembles/RankAvgSumEnsemble.R")
	
	source("functions/tools/PlotRocCurve.R")
  	
  	##----------------------------------------------------------------------
  	## ASSERTIONS ABOUT THE INPUT ------------------------------------------
  	##----------------------------------------------------------------------
	stopifnot(is.data.frame(data.Btrain))
	stopifnot(is.data.frame(data.Mtrain))
	stopifnot(is.data.frame(data.Mtest))
	
	stopifnot(is.vector(targetVal.Btrain))
	stopifnot(is.vector(targetVal.Mtrain))
	stopifnot(is.vector(targetVal.Mtest))
	
	##----------------------------------------------------------------------
	## OBTAIN COMBINED TRAINING SET FOR BASE LEARNERS
	##----------------------------------------------------------------------
	stopifnot(ncol(data.Btrain)==ncol(data.Mtrain))
	data.train = rbind(data.Btrain, data.Mtrain)
	data.test  = data.Mtest
	
	targetVal.train = c(targetVal.Btrain, targetVal.Mtrain)
	targetVal.test = targetVal.Mtest
	
	##----------------------------------------------------------------------
	## OBTAIN BASE CLASSIFIER PREDICTIONS (& their accuracies for Weighted Avg Ensemble)
	##----------------------------------------------------------------------
	cat('OBTAINING BASE CLASSIFIER PREDICTIONS small training set\n')
	predictions.base.small <- ClassifierModels(data.Btrain, targetVal.Btrain, data.Mtest)
	
	cat('OBTAINING BASE CLASSIFIER PREDICTIONS for large training set\n')
	predictions.base.large <- ClassifierModels(data.train, targetVal.train, data.test)
	
	
	cat('OBTAINING ENSEMBLE PREDICTIONS\n')
	##----------------------------------------------------------------------
	## Average Ensemble ----------------------------------------------------
	##----------------------------------------------------------------------
	predictions.small.Avg = AverageEnsemble(predictions.base.small)
	predictions.large.Avg = AverageEnsemble(predictions.base.large)
	cat('Average Ensemble complete\n')
  	
  	
  	##----------------------------------------------------------------------
	## Majority Vote -------------------------------------------------------
	##----------------------------------------------------------------------
	predictions.small.MajVote = MajorityVote(predictions.base.small, prob=TRUE)
	predictions.large.MajVote = MajorityVote(predictions.base.large, prob=TRUE)
	cat('Majority Vote complete\n')
  	
  	
  	##----------------------------------------------------------------------
	## Average of Rank Averages --------------------------------------------
	##----------------------------------------------------------------------
	predictions.small.RankAvg = RankAvgSumEnsemble(predictions.base.small)
	predictions.large.RankAvg = RankAvgSumEnsemble(predictions.base.large)
	cat('Rank Avg complete\n')

  	
  	##----------------------------------------------------------------------
  	## CONSTRUCT A DATA FRAME TO HOLD THE OUTPUT PREDICTIONS ---------------
  	##----------------------------------------------------------------------
  	predictions = data.frame(
                           SmallAvg=predictions.small.Avg,
                           LargeAvg=predictions.large.Avg,
                           SmallmVote=predictions.small.MajVote,
                           LargemVote=predictions.large.MajVote,
                           SmallRankAvg=predictions.small.RankAvg,
                           LargeRankAvg=predictions.large.RankAvg)
                           
    row.names(predictions) = row.names(data.Mtest)
    
  	##----------------------------------------------------------------------
  	## DETERMINE ACCURACY OF EACH ENSEMBLE METHOD --------------------------
  	##----------------------------------------------------------------------
  	if(output=="AUC") {
  		AUC.ensemble = matrix(nrow=3, ncol=ncol(predictions))
		colnames(AUC.ensemble) <- colnames(predictions)
		rownames(AUC.ensemble) = c("AUC", "CI-lower", "CI-upper")
		for(i in seq(1,ncol(predictions),1)) {
			rocCurve.ensemble = roc(as.numeric(targetVal.Mtest), predictions[,i], ci=TRUE, direction="auto")
			AUC.ensemble[1,i] = as.numeric(rocCurve.ensemble$auc)
			AUC.ensemble[2,i] = as.numeric(rocCurve.ensemble$ci)[1]
			AUC.ensemble[3,i] = as.numeric(rocCurve.ensemble$ci)[3]
		}
	
		if(save==TRUE) {
			savepath <- paste0("results/EnsembleModels/small&large/",readline("Save to (include extension!) results/EnsembleModels/small&large/"))
			write.table(AUC.ensemble, savepath, quote=FALSE)
		}
	}
	
  
  	if(output=='predictions') return(predictions)
  	else return(AUC.ensemble)

	
}