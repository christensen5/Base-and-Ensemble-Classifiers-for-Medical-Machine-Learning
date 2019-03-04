Tuner <- function(data.train, targetValues.train, data.test, targetValues.test) {

	library(caret)
	library(pROC)

	AUCvalues <- vector(mode="numeric", length=100)

	row=0
	col=0

	for(i in seq(1,length(AUCvalues),1)) {

	model.knn = knn3(data.train, as.factor(targetValues.train), k=i)
  	output.knn = predict.knn3(model.knn, data.test, type="prob")[, 2]
  	predictions.knn = as.numeric(output.knn)

	AUCvalues[i] = PlotRocCurve(predictions.knn, targetValues.test)

	}

	return(AUCvalues)
}


