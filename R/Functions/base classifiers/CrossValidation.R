##Function to run cross-validation for classification models
##
CrossValidation <- function(data.train, targetValues, nFolds=10, threshold=NULL, nKeep=NULL, verbose=FALSE, heatmap=FALSE, PCA=FALSE, extraData=NULL){
  ##----------------------------------------------------------------------
  ## LOAD LIBRARIES ------------------------------------------------------
  ##----------------------------------------------------------------------
  source("~/Dropbox/Documents/University/URSS/R/functions/base classifiers/ClassifierModels.R")
  source("~/Dropbox/Documents/University/URSS/R/functions/tools/FeatureSelection.R")
  library(gplots)
  ##----------------------------------------------------------------------
  ## ASSERTIONS ABOUT THE INPUT ------------------------------------------
  ##----------------------------------------------------------------------
  stopifnot(is.logical(targetValues))
  stopifnot(is.data.frame(data.train))
  if (is.null(threshold)==FALSE)
    stopifnot(is.null(nKeep))  ##if a threshold is set, don't also allow nKeep
  ##----------------------------------------------------------------------
  ## FIND USEFUL VALUES --------------------------------------------------
  ##----------------------------------------------------------------------
  nDataItems   = length(targetValues)
  featureNames = colnames(data.train)
  ##----------------------------------------------------------------------
  ## RUN THE CROSS-VALIDATION --------------------------------------------
  ##----------------------------------------------------------------------
  for (i in 1:nFolds){
    cat('Starting fold ', i, " of ", nFolds, fill=TRUE)
    ##SEPARATE DATA INTO TRAINING, TEST SETS
    index           = seq(i, nDataItems, nFolds)
    testData        = data.train[index,]
    trainingData    = data.train[-index,]
    trainingTargets = targetValues[-index]
    ##OPTION TO RUN SIMPLE FEATURE PRE-SELECTION USING THE CURRENT TRAINING SET
    keep = NULL
    if (is.null(threshold)==FALSE) keep = FeatureSelection(trainingData, trainingTargets, threshold)
    if (is.null(nKeep)==FALSE)     keep = FeatureSelection(trainingData, trainingTargets, nKeep=nKeep)
      
    if (is.null(keep)==FALSE){
      trainingData = trainingData[, keep]
      testData     = testData[, keep]
      if (verbose)
        print(featureNames[keep])
      if (heatmap){
        working           = as.matrix(trainingData)
        rownames(working) = trainingTargets
        heatmap.2(t(working), trace="none")
        readline("pause...")
      }
    }
    ##OPTION TO ADD IN EXTRA FEATURES, AFTER THE SELECTION
    if (is.null(extraData)==FALSE){
      trainingData = cbind(trainingData, extraData[index,])
      testData     = cbind(testData,     extraData[-index,])
    }
    ##OPTION TO RUN PCA ON THE FEATURES
    if (PCA){
          model.pca = princomp(trainingData)
          cum.var   = 0
          k         = 1            
          while (cum.var<0.95) {  ## Choose the principal components that explain 95% of the variance
            k       = k+1        # ALWAYS KEEP AT LEAST TWO PCs
            cum.var = sum(model.pca$sdev[1:k]^2)/sum(model.pca$sdev^2)
          }
          trainingData = as.data.frame(model.pca$scores[,1:k])
          cat('No. Principal Components= ', k, fill=TRUE)
          ## TRANSFORM TEST DATA USING PCA MODEL
          object.testData = predict(model.pca, testData)
          testData        = as.data.frame(object.testData[,1:k])
    }    
    ##TRAIN MODELS; MAKE PREDICTIONS
    working = ClassifierModels(trainingData, trainingTargets, testData)
    ##(1st iteration) CONSTRUCT AN OUTPUT DATA FRAME
    ##only at this point will we know how many classification models have been run
    ##might be a smarter way to do this....
    if (i==1){
      nModels                = ncol(working)
      predictions            = matrix(0, nDataItems, nModels)
      predictions            = as.data.frame(predictions)
      names(predictions)     = names(working)
      row.names(predictions) = row.names(data.train)
    }
    ##STORE PREDICTIONS IN OUTPUT FRAME
    predictions[index, ] = working    
  }
  return(predictions)
}
##*****************************************************************************
##*****************************************************************************
##----------------------------------------------------------------------
## ----------------------------------------
##----------------------------------------------------------------------
