##Function to run a set of classification models on input training, test data sets
##
##This function assumes as input:
##  data.train:   a (nItems * nFeatures) data frame 
##  data.test:    a (nItems * nFeatures) data frame 
##  targetValues: a vector of values 0 & 1 (representing FALSE and TRUE)
##
##It returns a data frame containing prediction probabilities for each classification algorithm.
##These are the predicted probabililty of outcome==TRUE
##
ClassifierModels <- function(data.train, targetValues, data.test, rank=FALSE){
  ##----------------------------------------------------------------------
  ## LOAD LIBRARIES ------------------------------------------------------
  ##----------------------------------------------------------------------
  library(glmnet)
  library(randomForest)
  library(kernlab)
  library(gbm)
  library(neuralnet)
  library(C50)
  library(caret)
  library(klaR)
  library(MASS)
  
  source("functions/tools/CentreScale.R")
  source("functions/base classifiers/GP/classification.R")
  ##----------------------------------------------------------------------
  ## ASSERTIONS ABOUT THE INPUT ------------------------------------------
  ##----------------------------------------------------------------------
  stopifnot(is.vector(targetValues))
  stopifnot(is.data.frame(data.train))
  stopifnot(is.data.frame(data.test))
  
  numClassifiers = 0
  
  if(rank==TRUE) cat('PREDICTIONS WILL BE RANKED\n')
  
  ##----------------------------------------------------------------------
  ##  SPARSE LOGISTIC REGRESSION -----------------------------------------
  ##----------------------------------------------------------------------
  cv.fit          = cv.glmnet(as.matrix(data.train), targetValues, family="binomial", maxit=1e5, alpha=1)
  predictions.glm = predict(cv.fit, as.matrix(data.test), type="response", s="lambda.min")
  predictions.glm = as.numeric(predictions.glm)
  
  if(rank=='TRUE') {
  	predictions.glm.ranked = ((rank(predictions.glm, ties.method="first") - rep(1, length(predictions.glm))) / (length(predictions.glm) - 1))	
  }
  
  cat('Sparse Logistic Regression complete\n')
  numClassifiers <- numClassifiers + 1
  
  ##----------------------------------------------------------------------
  ##  RANDOM FOREST ------------------------------------------------------
  ##----------------------------------------------------------------------
  rf.model       = randomForest(data.train, as.factor(targetValues), importance=TRUE, ntree=1500)
  predictions.rf = predict(rf.model, data.test, type="prob")[,2]
  
  if(rank=='TRUE') {
  	predictions.rf.ranked = ((rank(predictions.rf, ties.method="first") - rep(1, length(predictions.rf))) / (length(predictions.rf) - 1))	
  }
  
  cat('Random Forest complete\n')
  numClassifiers <- numClassifiers + 1
  
   ##----------------------------------------------------------------------
   ##  GENERALISED BOOSTING MODEL -----------------------------------------
   ##----------------------------------------------------------------------
   cvFolds         = 3
   nMinObs         = floor(length(targetValues) *0.33 / cvFolds)  ##this is a bit ad hoc
   nMinObs         = min(nMinObs, 10)
   model.gbm       = gbm(targetValues~., data=data.train, distribution="bernoulli",
                         n.trees=3000, interaction.depth=4, cv.folds=cvFolds, n.cores=2, n.minobsinnode=nMinObs)
   best.iter       = gbm.perf(model.gbm, method="cv")
   predictions.gbm = predict(model.gbm, data.test, n.trees=best.iter, type="response")
  
   if(rank=='TRUE') {
   	predictions.gbm.ranked = ((rank(predictions.gbm, ties.method="first") - rep(1, length(predictions.gbm))) / (length(predictions.gbm) - 1))	
   }
  
   cat('Generalised Boosting Method complete\n')
   numClassifiers <- numClassifiers + 1
  
  ##----------------------------------------------------------------------
  ## GAUSSIAN PROCESS ----------------------------------------------------
  ##----------------------------------------------------------------------
  set.seed(123)
  model.gp       = gausspr(data.train, as.factor(targetValues), kernel="rbfdot")
  predictions.gp = predict(model.gp, data.test, type="probabilities")
  predictions.gp = as.numeric(predictions.gp[, 2])
  
  if(rank=='TRUE') {
  	predictions.gp.ranked = ((rank(predictions.gp, ties.method="first") - rep(1, length(predictions.gp))) / (length(predictions.gp) - 1))	
  }
  
  cat('Gaussian Process complete\n')
  numClassifiers <- numClassifiers + 1
  
  ##----------------------------------------------------------------------
  ##  SUPPORT VECTOR MACHINE ---------------------------------------------
  ##----------------------------------------------------------------------
  filter          = ksvm(as.factor(targetValues)~., data=data.train, kernel="rbfdot",
                         prob.model=TRUE, kpar=list(sigma=0.05),C=5,cross=3)
  predictions.svm = predict(filter, data.test, type="probabilities")[, 2]
  
  if(rank=='TRUE') {
  	predictions.svm.ranked = ((rank(predictions.svm, ties.method="first") - rep(1, length(predictions.svm))) / (length(predictions.svm) - 1))	
  }
  
  cat('Support Vector Machine complete\n')
  numClassifiers <- numClassifiers + 1
  
  ##----------------------------------------------------------------------
  ## NEURAL NETWORK ------------------------------------------------------
  ##----------------------------------------------------------------------
  inputFormula     = as.formula(paste("targetValues ~ ", paste(names(data.train), collapse="+")))
  model.nnet       = neuralnet(inputFormula, data=scale(data.train), hidden=1, threshold=0.01, linear.output=FALSE)
  output.nnet      = compute(model.nnet, scale(data.test)) 
  predictions.nnet = as.numeric(output.nnet$net.result)
  
  if(rank=='TRUE') {
  	predictions.nnet.ranked = ((rank(predictions.nnet, ties.method="first") - rep(1, length(predictions.nnet))) / (length(predictions.nnet) - 1))	
  }
  
  cat('Neural Network complete\n')
  numClassifiers <- numClassifiers + 1
  
  ##----------------------------------------------------------------------
  ## Decision Tree -------------------------------------------------------
  ##----------------------------------------------------------------------
  model.dt = C5.0(data.train, as.factor(targetValues), trials=1, rules=FALSE)
  output.dt = predict.C5.0(model.dt, data.test, type="prob")[, 2]
  predictions.dt = as.numeric(output.dt)
  
  if(rank=='TRUE') {
  	predictions.dt.ranked = ((rank(predictions.dt, ties.method="first") - rep(1, length(predictions.dt))) / (length(predictions.dt) - 1))	
  }
  
  cat('Decision Tree complete\n')
  numClassifiers <- numClassifiers + 1
  
  ##----------------------------------------------------------------------
  ## Rule-Based Method ---------------------------------------------------
  ##----------------------------------------------------------------------
  model.rbm = C5.0(data.train, as.factor(targetValues), trials=1, rules=TRUE)
  output.rbm = predict.C5.0(model.rbm, data.test, type="prob")[, 2]
  predictions.rbm = as.numeric(output.rbm)
  
  if(rank=='TRUE') {
  	predictions.rbm.ranked = ((rank(predictions.rbm, ties.method="first") - rep(1, length(predictions.rbm))) / (length(predictions.rbm) - 1))	
  }
  
  cat('Rule-Based Method complete\n')
  numClassifiers <- numClassifiers + 1
  
  ##----------------------------------------------------------------------
  ## k-Nearest Neighbour -------------------------------------------------
  ##----------------------------------------------------------------------
  data.train.scaled = CentreScale(data.train)
  data.test.scaled = CentreScale(data.test)
  
  model.knn = knn3(data.train.scaled, as.factor(targetValues), k=40)
  output.knn = predict.knn3(model.knn, data.test.scaled, type="prob")[, 2]
  predictions.knn = as.numeric(output.knn)
  
  if(rank=='TRUE') {
  	predictions.knn.ranked = ((rank(predictions.knn, ties.method="first") - rep(1, length(predictions.knn))) / (length(predictions.knn) - 1))	
  }
  
  cat('k-Nearest Neighbour complete\n')
  numClassifiers <- numClassifiers + 1
  
  ##----------------------------------------------------------------------
  ## Naive Bayes ---------------------------------------------------------
  ##----------------------------------------------------------------------
  model.nb = NaiveBayes(data.train, as.factor(targetValues))
  output.nb = predict(model.nb, data.test)
  predictions.nb = as.numeric(output.nb$posterior[,2])
  
  if(rank=='TRUE') {
  	predictions.nb.ranked = ((rank(predictions.nb, ties.method="first") - rep(1, length(predictions.nb))) / (length(predictions.nb) - 1))	
  }
  
  cat('Naive Bayes complete\n')
  numClassifiers <- numClassifiers + 1
 
  ##----------------------------------------------------------------------
  ## Linear Discriminant Analysis ----------------------------------------
  ##----------------------------------------------------------------------
  model.lda = lda(data.train, as.factor(targetValues))
  output.lda = predict(model.lda, data.test)
  predictions.lda = as.numeric(output.lda$posterior[,2])
  
  if(rank=='TRUE') {
  	predictions.lda.ranked = ((rank(predictions.lda, ties.method="first") - rep(1, length(predictions.lda))) / (length(predictions.lda) - 1))	
  }
  
  cat('LDA complete\n')
  numClassifiers <- numClassifiers + 1
  
  ##----------------------------------------------------------------------
  ## CONSTRUCT A DATA FRAME TO HOLD THE OUTPUT PREDICTIONS ---------------
  ##----------------------------------------------------------------------
  if(rank=='FALSE') {
  	
  	predictions = data.frame(
                           glmnet=predictions.glm,
                           rf=predictions.rf,
                           gbm=predictions.gbm,
                           gp=predictions.gp,
                           svm=predictions.svm,
                           nnet=predictions.nnet,
                           dt=predictions.dt,
                           rbm=predictions.rbm,
                           knn=predictions.knn,
                           nb=predictions.nb,
                           lda=predictions.lda)
  }
                           
  else {
  	predictions = data.frame(
  						   glmnet=predictions.glm.ranked,
                           rf=predictions.rf.ranked,
                           gbm=predictions.gbm.ranked,
                           gp=predictions.gp.ranked,
                           svm=predictions.svm.ranked,
                           nnet=predictions.nnet.ranked,
                           dt=predictions.dt.ranked,
                           rbm=predictions.rbm.ranked,
                           knn=predictions.knn.ranked,
                           nb=predictions.nb.ranked,
                           lda=predictions.lda.ranked)
  	
  }
                          
  row.names(predictions) = row.names(data.test)
  
  cat(numClassifiers,'/11 Classifiers active.\n', 'Ranking is ', rank,'\n \n', sep="")
  
  return(predictions)
}
##*****************************************************************************
##*****************************************************************************
##----------------------------------------------------------------------
## ----------------------------------------
##----------------------------------------------------------------------



