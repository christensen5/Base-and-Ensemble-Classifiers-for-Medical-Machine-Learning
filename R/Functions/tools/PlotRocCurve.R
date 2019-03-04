##Function to plot a ROC curve and AUC statistic, given input class probabilities
PlotRocCurve <- function(predictions, targetValues, titleString="", ci=FALSE, lr=FALSE) {
  library(pROC)
  ##----------------------------------------------------------------------
  ## GENERATE A ROC CURVE ------------------------------------------------
  ##----------------------------------------------------------------------
  rocCurve          = roc(as.numeric(targetValues), predictions, ci=TRUE, direction="auto")
  auc               = as.numeric(rocCurve$auc)
  confidence        = as.numeric(rocCurve$ci)
  sensitivityValues = rocCurve$sensitivities
  specificityValues = rocCurve$specificities
  ##----------------------------------------------------------------------
  ## PLOT THE ROC CURVE --------------------------------------------------
  ##----------------------------------------------------------------------
  if (!ci) {
    plot(rocCurve, lwd=4, cex=4, mar=c(5, 4, 4, 2)+.1)
  } else {
    rocobj = plot.roc(rocCurve, lwd=4, cex=4, mar=c(5, 4, 4, 2)+.1,
                      ci=TRUE, print.auc=FALSE)
    ciobj  = ci.se(rocobj, specificities=seq(0, 1, length=100))
    plot(ciobj, type="shape", col="gray")
  }

  title(paste(titleString, " (auc=", format(auc, digits=2), ") (95% CI: ",
              format(confidence[1], digits=2), ", ", format(confidence[3], digits=2), ")", sep=""))
  ##----------------------------------------------------------------------
  ## FIND A THRESHOLD TO BALANCE SENSITIVITY, SPECIFICITY ----------------
  ##----------------------------------------------------------------------
  ##use distance between null line and actual curve
  ##Obviously, this simplifies... :-)
#  metricScore = (sensitivityValues - 1 + specificityValues)^2 +
#                (specificityValues - 1 + sensitivityValues)^2
#  index       = which.max(metricScore)

  ##this finds sens/spec values that are approximately equal
  metricScore = abs(sensitivityValues - specificityValues)
  index       = which.min(metricScore)
  
  sensitivity = sensitivityValues[index]
  specificity = specificityValues[index]
  threshold   = rocCurve$thresholds[index]
  ##----------------------------------------------------------------------
  ## FIND THE POSITIVE, NEGATIVE LIKELIHOOD RATIOS -----------------------
  ##----------------------------------------------------------------------
  lr.pos = sensitivity     / (1-specificity)
  lr.neg = (1-sensitivity) / specificity
  ##----------------------------------------------------------------------
  ## COMPUTE CONFIDENCE INTERVALS FOR SENSITIVITY, SPECIFICITY -----------
  ##----------------------------------------------------------------------
  if (rocCurve$direction=="<")  
    predictedClass = predictions>threshold
  else
    predictedClass = predictions<threshold
   
  truePositive   = sum((predictedClass==TRUE)  & (targetValues==TRUE))
  trueNegative   = sum((predictedClass==FALSE) & (targetValues==FALSE))
  falsePositive  = sum((predictedClass==TRUE)  & (targetValues==FALSE))
  falseNegative  = sum((predictedClass==FALSE) & (targetValues==TRUE))
  sensitivity.ci = as.numeric(binom.test(truePositive, truePositive + falseNegative)$conf.int)
  specificity.ci = as.numeric(binom.test(trueNegative, trueNegative + falsePositive)$conf.int)
  precision      = truePositive / (truePositive + falsePositive)
  recall         = sensitivity
  f.measure      = 2 * precision*recall / (precision + recall)
  ##also recompute sens/spec, as a sanity check
  sens.2         = truePositive / (truePositive + falseNegative)
  spec.2         = trueNegative / (trueNegative + falsePositive)  
  ##----------------------------------------------------------------------
  ## PRINT USEFUL INFO TO SCREEN -----------------------------------------
  ##----------------------------------------------------------------------
  cat(paste(titleString, "(ROC)"), fill=TRUE)
  cat("------------------", fill=TRUE)
  cat(paste("AUC         = ", format(auc,         digits=2)), "   (", format(confidence[1], digits=2), " - ",
      format(confidence[3], digits=2), ")", fill=TRUE, sep="")
  cat(paste("sensitivity = ", format(sensitivity, digits=2)), "   (", format(sensitivity.ci[1], digits=2), " - ",
      format(sensitivity.ci[2], digits=2), ")", fill=TRUE, sep="")
  cat(paste("specificity = ", format(specificity, digits=2)), "   (", format(specificity.ci[1], digits=2), " - ",
      format(specificity.ci[2], digits=2), ")", fill=TRUE, sep="")
  cat("\n")

  if (lr==TRUE){
    cat(paste("LR+ = ", format(lr.pos, digits=2)), fill=TRUE, sep="")
    cat(paste("LR- = ", format(lr.neg, digits=2)), fill=TRUE, sep="")
    cat("\n")
  }

  return(auc)
}
##*****************************************************************************
##*****************************************************************************
##----------------------------------------------------------------------
## ----------------------------------------
##----------------------------------------------------------------------
