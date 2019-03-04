##Function to perform a feature selection using a Wilcoxon rank-sum test
##
FeatureSelection <- function(dataMatrix, targetValues, threshold=NULL, nKeep=2){
  ##----------------------------------------------------------------------
  ## LOAD LIBRARIES ------------------------------------------------------
  ##----------------------------------------------------------------------
  ##This is a library from MD Anderson's Oompa suite of R tools
  #library(ClassComparison)
  ##----------------------------------------------------------------------
  ## FIND USEFUL VALUES --------------------------------------------------
  ##----------------------------------------------------------------------
  nDataItems = nrow(dataMatrix)
  nFeatures  = ncol(dataMatrix)
  nMinKeep   = 2
  startTime  = Sys.time()
  ##----------------------------------------------------------------------
  ## USE FASTER CODE TO PRE-SELECT CANDIDATE FEATURES --------------------
  ##----------------------------------------------------------------------
  ##Should be able to also use this function to assess p-values...
#  working = MultiWilcoxonTest(t(dataMatrix), targetValues)
#  keep    = which(selectSignificant(working, prior=0.5, signif=0.25))

  keep = 1:nFeatures
  
  ##----------------------------------------------------------------------
  ## COMPUTE P-VALUES FOR WILCOXON RANK-SUM TEST -------------------------
  ##----------------------------------------------------------------------
  pValues       = numeric(nFeatures)
  pValues[]     = NA
  index.disease = which(targetValues==TRUE)
  index.control = which(targetValues==FALSE)
  data.disease  = as.matrix(dataMatrix[index.disease,])
  data.control  = as.matrix(dataMatrix[index.control,])
  for (i in keep){
    current.disease = data.disease[, i]
    current.control = data.control[, i]
    outputObject    = wilcox.test(current.disease, current.control, exact=FALSE)
    pValues[i]      = outputObject$p.value
  }
  pValues = pValues * nFeatures ##Bonferroni correction
  ##----------------------------------------------------------------------
  ## DISCARD ANY FEATURES SCORING "NA" -----------------------------------
  ##----------------------------------------------------------------------
  ##assume that this is a zero-variance feature, or rejected  using MultiWilcoxonTest
  pValues[is.na(pValues)] = max(pValues, na.rm=TRUE)
  ##----------------------------------------------------------------------
  ## DECIDE WHCH FEATURES TO KEEP ----------------------------------------
  ##----------------------------------------------------------------------
  if (is.null(threshold))
    threshold = quantile(pValues, nKeep/nFeatures)
  keep = which(pValues < threshold)
  ##----------------------------------------------------------------------
  ## ALWAYS RETURN AT LEAST A COUPLE OF FEATURES -------------------------
  ##----------------------------------------------------------------------
  if (length(keep)<nMinKeep){
    cat("Always return at least ", nMinKeep," features....", fill=TRUE)
    index = order(pValues)
    keep  = index[1:nMinKeep]
    print(pValues[keep])    
  }
  else cat("nFeatures=", length(keep), fill=TRUE)
  if (length(keep)<10)
    print(sort(pValues[keep]))    
  print(Sys.time() - startTime)
  return(keep)
}
##*****************************************************************************
##*****************************************************************************
##----------------------------------------------------------------------
## ----------------------------------------
##----------------------------------------------------------------------
