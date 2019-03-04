##This function will accept a set of base classifier models, a training set on which to train them (using CrossValidation.R) it will then train a meta-learner using the same training set and the CrossValidation predictions, and return predictions made by the meta-learner on a testing set.

Stacking2 <- function(base.train, targetValues) {

	##--------------------------------------------------------------------------------
	## ASSERTIONS ABOUT THE INPUTS
	##--------------------------------------------------------------------------------
	stopifnot(is.data.frame(base.train))
	stopifnot(is.logical(targetValues))
	
	##--------------------------------------------------------------------------------
	## LOAD LIBRARIES
	##--------------------------------------------------------------------------------
	library(glmnet)
	source("~/Dropbox/Documents/University/URSS/R/functions/base classifiers/CrossValidation.R")
	source("~/Dropbox/Documents/University/URSS/R/functions/base classifiers/CVSparseLogisticRegression.R")
  	
	##--------------------------------------------------------------------------------
	## Run CrossValidation.R to get separate predictions for the base learners on their training set and on the testing set
	##--------------------------------------------------------------------------------
	cat('Obtaining BL predictions to train ML.')
	meta.train <- CrossValidation(base.train, targetValues)
	
	##----------------------------------------------------------------------
  	##  Run CROSS-VALIDATED SPARSE LOGISTIC REGRESSION as a meta-learner, learning on the cross-validated predictions for the base learners, and predicting sequentially on the unseen folds of the same predictions.  	##----------------------------------------------------------------------
  	cat('Running ML.')
  	predictions.meta <- CVSparseLogisticRegression(meta.train, targetValues)
  	
  	return(predictions.meta)
}