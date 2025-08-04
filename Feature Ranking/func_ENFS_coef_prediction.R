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



# نیاز به نصب پکیج‌ها در صورت عدم نصب
# install.packages("glmnet")
# install.packages("ncvreg")

func_ENFS_coef_prediction <- function(in_data_ben, in_data_mal, 
                                      alpha = 0.5, num_iteration = 50,
                                      method = "elasticnet") {
  library(glmnet)
  library(ncvreg)
  
  # بررسی پارامترها
  if (missing(num_iteration)) num_iteration <- 30
  if (missing(alpha)) alpha <- 0.8
  if (missing(in_data_mal) | missing(in_data_ben)) {
    cat("ERROR: insufficient input parameters ...\n")
    return(list(coef = NULL, enfs = NULL))
  }
  if (ncol(in_data_ben) != ncol(in_data_mal)) {
    cat("ERROR: feature dimension not match ...\n")
    return(list(coef = NULL, enfs = NULL))
  }
  
  cat("... start feature selection method:", method, "\n")
  
  metric <- matrix(0, nrow = num_iteration, ncol = 7)
  coeffx <- matrix(0, nrow = num_iteration, ncol = ncol(in_data_ben))
  
  data_ben <- in_data_ben
  data_mal <- in_data_mal
  
  for (i in 1:num_iteration) {
    result <- utsw_random_data_spliting_train_test(data_ben, data_mal)
    train_ben <- result$train_ben
    train_mal <- result$train_mal
    ttest_ben <- result$test_ben
    ttest_mal <- result$test_mal
    
    XTrain <- rbind(train_ben, train_mal)
    yTrain <- c(rep(0, nrow(train_ben)), rep(1, nrow(train_mal)))
    XTest <- rbind(ttest_ben, ttest_mal)
    yTest <- c(rep(0, nrow(ttest_ben)), rep(1, nrow(ttest_mal)))
    
    if (method == "elasticnet") {
      cvfit <- cv.glmnet(as.matrix(XTrain), yTrain, family = "binomial", alpha = alpha)
      best_lambda <- cvfit$lambda.1se
      model <- glmnet(as.matrix(XTrain), yTrain, family = "binomial", alpha = alpha, lambda = best_lambda)
      coef <- coef(model)[-1]
      coef0 <- coef(model)[1]
      
      # پیش‌بینی احتمالات و تبدیل به برچسب
      yhat_prob <- predict(model, XTest, type = "response")
      yhat <- ifelse(yhat_prob > 0.5, '1', '0')
      
    } else if (method == "mcp" || method == "scad") {
      penalty_type <- ifelse(method == "mcp", "MCP", "SCAD")
      cvfit <- cv.ncvreg(XTrain, yTrain, family = "binomial", penalty = penalty_type, nfolds = 10, max.iter = 10000)
      lambda_opt <- cvfit$lambda.min
      model <- cvfit$fit
      coef <- model$beta[-1, which(model$lambda == lambda_opt)]
      coef0 <- model$beta[1, which(model$lambda == lambda_opt)]
      yhat_prob <- predict(model, XTest, lambda = lambda_opt, type = "response")
      yhat <- ifelse(yhat_prob > 0.5, '1', '0')
      
    } else if (method == "adaptivelasso") {
      cv_model <- cv.glmnet(as.matrix(XTrain), yTrain, family = "binomial", alpha = 0)
      lambda_best <- cv_model$lambda.min  # مقدار بهینه lambda
      
      # گرفتن ضرایب بدون بایاس
      init_coef <- as.vector(coef(cv_model, s = lambda_best))[-1]
      
      if (length(init_coef) != ncol(XTrain)) {
        stop("Dimensions of init_coef and XTrain do not match")
      }
      weights <- 1 / (abs(init_coef) + 1e-6)  # جلوگیری از تقسیم بر صفر
      
      model <- ncvreg(XTrain, yTrain, family = "binomial", penalty = "lasso", penalty.factor = weights)
      lambda_idx <- which.min(model$loss)
      coef <- model$beta[-1, lambda_idx]
      coef0 <- model$beta[1, lambda_idx]
      yhat_prob <- predict(model, XTest, lambda = model$lambda[lambda_idx], type = "response")
      yhat <- ifelse(yhat_prob > 0.5, '1', '0')
      
    } else {
      stop("Unsupported method. Choose from: 'elasticnet', 'mcp', 'scad', 'adaptivelasso'")
    }
    
    metric[i, ] <- utsw_binary_classification_metrics(yTest, yhat)
    coeffx[i, ] <- coef
    cat("...... (", i, ")/(", num_iteration, ") \n")
  }
  
  return(list(coef = coeffx, enfs = metric))
}





