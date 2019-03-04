##This function will accept training and testing data for base and meta learners, then run the base classifiers and combine their predictions using various ensemble methods, returning the ensemble method predictions on the data.Mtest set, as well as the AUC metric for each ensemble method's predictions on said set.

## The function assumes as input:
## data.Btrain, data.Mtrain, data.Mtest : 3 (nItems * nFeatures) data frames
## targetVal.btrain, targetVal.Mtrain, targetVal.Mtest : 3 binary vectors (representing correct classifications for each item in the above data frames)
## output : a variable controlling whether to return a dataframe of all the ensemble model predictions, or a dataframe of AUC scores and 95% CI for each ensemble model.
## save : a boolean variable controlling whether the AUC scores & 95%CI values will be written to a file directly by this function

##It returns a data frame containing predictions probabilities for each ensemble method. These are the predicted probability of the outcome==1 as defined in the file Data Set Info.docx

EnsembleModels <- function(data.Btrain, data.Mtrain, data.Mtest, targetVal.Btrain, targetVal.Mtrain, targetVal.Mtest, output=c('predictions','AUC'), save=FALSE) {
	
	##----------------------------------------------------------------------
  	## LOAD LIBRARIES & FUNCTIONS ------------------------------------------
 	##----------------------------------------------------------------------
 	library(pROC)
 	library(MASS)
 	
 	source("functions/base classifiers/ClassifierModels.R")
	
	source("functions/ensembles/AverageEnsemble.R")
	source("functions/ensembles/MajorityVote.R")
	source("functions/ensembles/RankAvgSumEnsemble.R")
	source("functions/ensembles/Stacking.RandFor.R")
	source("functions/ensembles/Stacking.SparseLogReg.R")
	source("functions/ensembles/Stacking.LogReg.R")
	source("functions/ensembles/WeightedAverageEnsemble.R")
	
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
	
	savepath <- paste0("datasets/",readline("Save predictions to datasets/???????/Simpson/"),"/Simpson/")
	write.table(targetVal.Mtrain, paste0(savepath,"tv-Mtrain.data"), quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)

	
	##----------------------------------------------------------------------
	## OBTAIN BASE CLASSIFIER PREDICTIONS (& their accuracies for Weighted Avg Ensemble)
	##----------------------------------------------------------------------
	cat('OBTAINING BASE CLASSIFIER PREDICTIONS & AUC for data.Mtrain\n')
	predictions.base.Mtrain 	 <- ClassifierModels(data.Btrain, targetVal.Btrain, data.Mtrain)
	write.table(predictions.base.Mtrain, paste0(savepath,'predictions-Mtrain.data'), quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
	
	AUC.base.Mtrain 			= vector(mode="numeric", length=ncol(predictions.base.Mtrain)) #this will be used by WAvg to learn how to weight the base classifiers
	names(AUC.base.Mtrain) 		<- colnames(predictions.base.Mtrain)
	
	for(i in seq(1,ncol(predictions.base.Mtrain),1)) {
		rocCurve.base.Mtrain 	= roc(as.numeric(targetVal.Mtrain), predictions.base.Mtrain[,i], ci=TRUE, direction="auto")

		AUC.base.Mtrain[i] 		= as.numeric(rocCurve.base.Mtrain$auc)
	}
	
	cat('OBTAINING BASE CLASSIFIER PREDICTIONS & AUC for data.Mtest\n')
	predictions.base.Mtest <- ClassifierModels(data.Btrain, targetVal.Btrain, data.Mtest)
	write.table(predictions.base.Mtest, paste0(savepath,'predictions-Mtest.data'), quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
	
	AUC.base.Mtest				= vector(mode="numeric", length=ncol(predictions.base.Mtest)) #this will be used to include the best performing base classifier in the output
	names(AUC.base.Mtest)		<- colnames(predictions.base.Mtest)
	
	for(j in seq(1,ncol(predictions.base.Mtest),1)) {
		rocCurve.base.Mtest  	= roc(as.numeric(targetVal.Mtest), predictions.base.Mtest[,j], ci=TRUE, direction="auto")
		
		AUC.base.Mtest[j]		= as.numeric(rocCurve.base.Mtest$auc)
	}
	
	if(output=="AUC" && save==TRUE) {
  		savepath1 <- paste0("results/EnsembleModels/base/",readline("Save base classifier AUCs to (include extension!) results/EnsembleModels/base/"))
			write.table(AUC.base.Mtest, savepath1, quote=FALSE)
  		
  	}
	index.bestBase = which.max(AUC.base.Mtest)
	
	cat('OBTAINING BASE CLASSIFIER PREDICTIONS & AUC for large Btrain + Mtrain training set\n')
	predictions.base.bigTrain 	 <- ClassifierModels(rbind(data.Btrain, data.Mtrain), c(targetVal.Btrain, targetVal.Mtrain), data.Mtest)
	write.table(predictions.base.bigTrain, paste0(savepath,'predictions-bigTrain.data'), quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
	
	numEnsembles = 0
	cat('OBTAINING ENSEMBLE PREDICTIONS\n')
	##----------------------------------------------------------------------
	## Average Ensemble ----------------------------------------------------
	##----------------------------------------------------------------------
	predictions.Avg = AverageEnsemble(predictions.base.bigTrain)
	cat('Average Ensemble complete\n')
  	numEnsembles <- numEnsembles + 1
  	
  	
  	##----------------------------------------------------------------------
	## Weighted Average Ensemble -------------------------------------------
	##----------------------------------------------------------------------
	predictions.WeightedAvg = WeightedAverageEnsemble(predictions.base.Mtest, AUC.base.Mtrain)
	cat('Weighted Average Ensemble complete\n')
  	numEnsembles <- numEnsembles + 1
  	
  	
  	##----------------------------------------------------------------------
	## Majority Vote -------------------------------------------------------
	##----------------------------------------------------------------------
	predictions.MajVote = MajorityVote(predictions.base.bigTrain, prob=TRUE)
	cat('Majority Vote complete\n')
  	numEnsembles <- numEnsembles + 1
  	
  	
  	##----------------------------------------------------------------------
	## Average of Rank Averages --------------------------------------------
	##----------------------------------------------------------------------
	predictions.RankAvg = RankAvgSumEnsemble(predictions.base.bigTrain)
	cat('Rank Avg complete\n')
  	numEnsembles <- numEnsembles + 1
  	
  	
  	##----------------------------------------------------------------------
	## Stacking (various meta L) -------------------------------------------
	##----------------------------------------------------------------------
	predictions.StackLR  = Stacking.LogReg(predictions.base.Mtrain, targetVal.Mtrain, predictions.base.Mtest)
	cat('Stacking (LR) complete\n')
	numEnsembles <- numEnsembles + 1
	predictions.StackSLR = Stacking.SparseLogReg(predictions.base.Mtrain,targetVal.Mtrain,predictions.base.Mtest)
	cat('Stacking (SLR) complete\n')
  	numEnsembles <- numEnsembles + 1
	predictions.StackRF = Stacking.RandFor(predictions.base.Mtrain, targetVal.Mtrain, predictions.base.Mtest)
	cat('Stacking (RF) complete\n')
  	numEnsembles <- numEnsembles + 1
  	
  	
  	##----------------------------------------------------------------------
	## 'Grand' Average -----------------------------------------------------
	##----------------------------------------------------------------------
	predictions.GrandAvg = AverageEnsemble(data.frame(
                           Avg=predictions.Avg,
                           wAvg=predictions.WeightedAvg,
                           mVote=predictions.MajVote,
                           rankAvg=predictions.RankAvg,
                           stackLR=predictions.StackLR,
                           stackSLR=predictions.StackSLR,
                           stackRF=predictions.StackRF))
    cat('\'Grand\' Avg complete\n')
  	numEnsembles <- numEnsembles + 1

  	
  	
  	##----------------------------------------------------------------------
  	## CONSTRUCT A DATA FRAME TO HOLD THE OUTPUT PREDICTIONS ---------------
  	##----------------------------------------------------------------------
  	predictions = data.frame(
                           Avg=predictions.Avg,
                           wAvg=predictions.WeightedAvg,
                           mVote=predictions.MajVote,
                           rankAvg=predictions.RankAvg,
                           stackLR=predictions.StackLR,
                           stackSLR=predictions.StackSLR,
                           stackRF=predictions.StackRF,
                           #gAvg=predictions.GrandAvg,
                           bestBase=predictions.base.Mtest[,index.bestBase])
                           
    row.names(predictions) = row.names(data.Mtest)
  
  	cat(numEnsembles,'/8 Ensembles active.\n \n', sep="")
  	
  	if(output=="predictions" && save==TRUE) {
  		savepath2 <- paste0("results/EnsembleModels/ensembles",readline("Save to (include extension!) results/EnsembleModels/ensembles/"))
			write.table(predictions, savepath2, quote=FALSE)
  		
  	}
  	
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
			savepath3 <- paste0("results/EnsembleModels/ensembles/",readline("Save to (include extension!) results/EnsembleModels/ensembles/"))
			write.table(AUC.ensemble, savepath3, quote=FALSE)
		}
	}
	
  
  	if(output=='predictions') return(predictions)
  	else return(AUC.ensemble)

	
}