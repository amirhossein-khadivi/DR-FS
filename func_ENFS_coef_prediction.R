# -------------------------------------------------------------------------
# Shaode Yu (yushaodemia AT 163 DOT com)
#   Purpose
#       elastic net based feature selection and weighting
# -------------------------------------------------------------------------
#  Input parameters
#      in_data_ben: the input beni cases
#      in_data_mal: the input mali cases
#         Note, benign 0; malignant 1
#     alpha: the value to balance L1 and L2 penalty
#     num_iteration: the number of iteration
# -------------------------------------------------------------------------
# Output parameters
#   data_out
#      data_out.coef, the coefficient matrix
#      data_out.enfs, ENFS based prediction
#                       
# -------------------------------------------------------------------------
# v01 05/28/2020
# v02 11/16/2020
# v03 11/26/2021
# V04 01/22/2022
# -------------------------------------------------------------------------



#install.packages('glmnet')
func_ENFS_coef_prediction <- function(in_data_ben, in_data_mal, alpha = 0.8, num_iteration = 30) {
  library(glmnet)
  # (1) to check input parameters
  # (1.1) to check input parameters
  if (missing(num_iteration)) {
    num_iteration <- 30
  }
  
  if (missing(alpha)) {
    alpha <- 0.8
  }
  
  if (missing(in_data_mal) | missing(in_data_ben)) {
    cat("ERROR: insufficient input parameters ...\n")
    data_out <- list(coef = NULL, enfs = NULL)
    return(data_out)
  }
  
  # (1.2) to check whether input data is correct
  if (ncol(in_data_ben) != ncol(in_data_mal)) {
    cat("ERROR: feature dimension not match ...\n")
    data_out <- list(coef_matrix = NULL, enfs = NULL)
    return(data_out)
  }
  
  
  # (2) to start offline elastic net based feature ranking and prediction
  cat("... start elastic net based feature selection \n")
  
  # (2.1) to define the output matrix
  metric <- matrix(0, nrow = num_iteration, ncol = 7)
  coeffx <- matrix(0, nrow = num_iteration, ncol = ncol(in_data_ben))
  
  # (2.2) to retrieve the source data
  data_ben <- in_data_ben
  data_mal <- in_data_mal
  
  # (2.3) to start the iteration
  for (i in 1:num_iteration) {
    # (2.3.1) random data splitting
    result <- utsw_random_data_spliting_train_test(data_ben, data_mal)
    train_ben <- result$train_ben
    train_mal <- result$train_mal
    ttest_ben <- result$test_ben
    ttest_mal <- result$test_mal
    
    # (2.3.2) data re-organization
    XTrain <- rbind(train_ben, train_mal)
    yTrain <- c(rep(0, nrow(train_ben)), rep(1, nrow(train_mal)))
    XTest <- rbind(ttest_ben, ttest_mal)
    yTest <- c(rep(0, nrow(ttest_ben)), rep(1, nrow(ttest_mal)))
    
    # (2.3.3) 10-folder cross-validation of elastic net
    result <- cv.glmnet(as.matrix(XTrain), yTrain, family="binomial",alpha = alpha, nfolds = 10)
    idxLambda1SE <- result$lambda.1se
    result1 <- glmnet(as.matrix(XTrain), yTrain, family="binomial",alpha = alpha, lambda= idxLambda1SE, nfolds = 10)
    coef <- coef(result1, s = idxLambda1SE)[-1]
    coef0 <- coef(result1, s = idxLambda1SE)[1]
    
    
    # (2.3.4) on the testing
    #yhat <- as.matrix(XTest) %*% coef + coef0
    #yhat <- as.numeric(yhat > 0.5)
    yhat <- predict(result1, XTest)
    yhat <- ifelse(yhat > 0.5, '1', '0')
    metric[i, ] <- utsw_binary_classification_metrics(yTest, yhat)
    
    # (2.3.5) on the coefficients
    coeffx[i, ] <- coef
    cat("...... (", i, ")/(", num_iteration, ") \n")
  }
  coef <- coeffx
  enfs <- metric
  data <- list(coef =coef,enfs= enfs)
  return(data)
}




