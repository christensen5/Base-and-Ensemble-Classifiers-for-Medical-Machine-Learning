##This function will accept a set of base classifier models, a training set on which to train them (using CrossValidation.R) it will then train a meta-learner using the same training set and the CrossValidation predictions, and return predictions made by the meta-learner on a testing set.

Stacking.LogReg <- function(predictions.base, targetValues.Mtrain, predictions.metaTest) {

	##--------------------------------------------------------------------------------
	## ASSERTIONS ABOUT THE INPUTS
	##--------------------------------------------------------------------------------
	stopifnot(is.data.frame(predictions.base))
	stopifnot(is.vector(targetValues.Mtrain))
	stopifnot(is.data.frame(predictions.metaTest))
	
		
	##--------------------------------------------------------------------------------
	## LOAD LIBRARIES
	##--------------------------------------------------------------------------------
	library(glmnet)
	source("~/Dropbox/Documents/University/URSS/R/functions/base classifiers/ClassifierModels.R")
	source("~/Dropbox/Documents/University/URSS/R/functions/base classifiers/CrossValidation.R")
  	
	##--------------------------------------------------------------------------------
	## Get base learner predictions on the training set and testing set.
	##--------------------------------------------------------------------------------
	print('Obtaining BL predictions to train ML')
	meta.train <- predictions.base
	print('ML training predictions obtained')
	
	print('Obtaining BL predictions to test ML')
##	meta.test <- ClassifierModels(base.train, targetValues, data.test)
	meta.test <- predictions.metaTest
	print('ML testing predictions obtained')
	
	##----------------------------------------------------------------------
  	##  Run SPARSE LOGISTIC REGRESSION as a meta-learner, learning on the cross-validated predictions for the base learners, and predicting on a separate training set.
  	##----------------------------------------------------------------------
  	print('Running ML')
  	lr.meta           = glm(targetValues.Mtrain ~ ., data=meta.train, family=binomial)
  	predictions.meta = predict(lr.meta, meta.test, type="response")
 	predictions.meta = as.numeric(predictions.meta)
 	print('ML predictions complete')
  
  	return(predictions.meta)
  	
}


