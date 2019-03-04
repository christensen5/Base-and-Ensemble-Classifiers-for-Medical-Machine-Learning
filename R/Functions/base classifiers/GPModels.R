GPModels <- function(data.train, targetValues.train, data.test, targetValues.test) {

	library(kernlab)
	library(pROC)
	stopifnot(is.vector(targetValues.train))
	stopifnot(is.vector(targetValues.test))
  	stopifnot(is.data.frame(data.train))
 	stopifnot(is.data.frame(data.test))
 	
 	source("GP/classification.R")
		
	AUC = matrix(nrow=5, ncol=3)
	colnames(AUC) = c("initial GaussPr", "default GaussPr", "Jim")
	
	for(i in seq(1,5,1)) {
		
		## Normal GP
		model1.gp       = gausspr(data.train, as.factor(targetValues.train), kernel="rbfdot", scaled=FALSE, var=1, tol=0.07)
  		predictions1.gp = predict(model1.gp, data.test, type="probabilities")
  		predictions1.gp = as.numeric(predictions1.gp[, 2])
  		rocCurve			= roc(as.numeric(targetValues.test), predictions1.gp, ci=TRUE, direction="auto")
  		AUC[i,1]			= as.numeric(rocCurve$auc)
	
		## Default GaussPr
		model2.gp       = gausspr(data.train, as.factor(targetValues.train), kernel="rbfdot")
  		predictions2.gp = predict(model2.gp, data.test, type="probabilities")
  		predictions2.gp = as.numeric(predictions2.gp[, 2])
  		rocCurve			= roc(as.numeric(targetValues.test), predictions2.gp, ci=TRUE, direction="auto")
  		AUC[i,2]			= as.numeric(rocCurve$auc)
	
		## Jim GP
#		model3.gp 		= GPC(data.train, as.factor(targetValues.train))
#		predictions3.gp = predict(model3.gp, data.test)
#		predictions3.gp = as.numeric(predictions3.gp)
#		rocCurve			= roc(as.numeric(targetValues.test), predictions3.gp, ci=TRUE, direction="auto")
#  		AUC[i,3]			= as.numeric(rocCurve$auc)
	
	}
	
	return(AUC)
	
}