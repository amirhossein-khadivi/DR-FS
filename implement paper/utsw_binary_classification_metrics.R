# =========================================================================
#
# Shaode Yu (yushaodemia@163.com)
# -------------------------------------------------------------------------
# performance metrics for binary classification problems
#
# %  input parameters
#      groundTruth: the ground truth labels ([n, 1])
#      predictedResult: the predicted results ([n, 1])
#
# %  output parameters
#      metric: the 7 parameters computed for performance evaluation
# -------------------------------------------------------------------------
# v01 02/28/2019
# v02 11/16/2020
# -------------------------------------------------------------------------



utsw_binary_classification_metrics <- function(groundTruth, predictedResult) {
  # (1) to check input parameters
  if (missing(groundTruth) || missing(predictedResult)) {
    cat("WRONG: Not enough parameters ...\n")
    metric <- NULL
    return(metric)
  }
  
  # (2) to check whether the numbers of labels match
  if (length(groundTruth) != length(predictedResult)) {
    metric <- NULL
    return(metric)
  }
  
  # (3) performance evaluation
  testLabel <- groundTruth
  preLab <- predictedResult
  
  # (3.1) to check whether Labels are consistent
  # according to random data splitting, there are two labels in certain
  #     ben 0; mal 1
  predictLabelx <- unique(preLab)
  if (length(predictLabelx) == 1) {
    if (predictLabelx == 0) {
      cat("CAUTION: benign labels only ...\n")
    } else if (predictLabelx == 1) {
      cat("CAUTION: malignant labels only ...\n")
    } else {
      cat("CAUTION: wrong labels ", predictLabelx, " ...\n")
      metric <- NULL
      return(metric)
    }
  } else {
    if ((min(predictLabelx) != 0) || (max(predictLabelx) != 1)) {
      cat("CAUTION: benign (", min(predictLabelx), "); malignant (", max(predictLabelx), ") ", predictLabelx, " ...\n")
      metric <- NULL
      return(metric)
    }
  }
  
  # (3.2) to baseline (ben, 0; mal, 1)
  posLab <- 1
  negLab <- 0
  
  tp <- sum((preLab == posLab) & (testLabel == posLab))
  fn <- sum((preLab == negLab) & (testLabel == posLab))
  tn <- sum((preLab == negLab) & (testLabel == negLab))
  fp <- sum((preLab == posLab) & (testLabel == negLab))
  
  #install.packages('Metrics')
  library(Metrics)
  auc <- auc(as.double(testLabel), as.double(preLab))
  
  acc <- (tp + tn) / (tp + fn + tn + fp)
  sen <- tp / (tp + fn)
  spe <- tn / (tn + fp)
  ppv <- tp / (tp + fp)
  npv <- tn / (tn + fn)
  f1s <- (2 * tp) / (2 * tp + fp + fn)
  
  metric <- c(auc, acc, sen, spe, ppv, npv, f1s)
  
  
  
  
}
