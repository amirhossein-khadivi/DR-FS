# -------------------------------------------------------------------------
# Shaode Yu, yushaodemia@163.com
# -------------------------------------------------------------------------
# v01 06/24/2021
# v02 06/30/2021
# v03 11/06/2021
# v04 11/25/2021
# -------------------------------------------------------------------------
#   If there is only two parts, such as training and testing,
#       we can define the validation set the same as the testing set
# -------------------------------------------------------------------------
# to integrate classifiers (in R) for data prediction
# input
#   Xtrain, the data samples for model training
#           [m p] m samples and each with p features
#   Ytrain, the corresponding labels
#           [m 1] m samples and each with 1 labels in {0, 1}
#           NOTE: 0 is bad (benign) and 1 is good (malignant)
#   Xtest,  the data samples for model training
#           [k p] k samples and each with p features
#   Ytest,  the corresponding labels
#           [k 1] k samples and each with 1 labels in {0, 1}
# ml_classifier,
#          including KNN, Random Forest, Naive Bayes, Ensembles
#                    and discriminant analysis classifier
# -------------------------------------------------------------------------



utsw_machine_learning_classifiers_train_test <- function(Xtrain, Ytrain, Xtest, Ytest, ml_classifier) {
  if (missing(ml_classifier)) {
    ml_classifier <- 'knn'
  }
  
  if (missing(Ytest)) {
    cat('ERROR_ysd: not enough inputs ...\n')
    metric_test <- NULL
    return(metric_test)
  }
  
  # machine learning based prediction on the validation and the testing set
  ml_classifier <- tolower(ml_classifier)
  if (ml_classifier == 'knn') {
    library(class)
    knn <- knn(train = as.matrix(Xtrain), test = as.matrix(Xtest), cl = Ytrain, k = 5) # a trick
    predict_label_test <- knn
    metric_test <- utsw_binary_classification_metrics(Ytest, as.numeric(as.character(predict_label_test)))
  } else if (ml_classifier == 'rf') {
    library(randomForest)
    nTree <- 200 # a trick
    rf <- randomForest(x = as.matrix(Xtrain), y = as.factor(Ytrain), ntree = nTree)
    predict_label_test <- predict(rf, as.matrix(Xtest))
    metric_test <- utsw_binary_classification_metrics(Ytest, as.numeric(as.character(predict_label_test)))
  } else if (ml_classifier == 'nb') {
    library(e1071)
    nb <- naiveBayes(x = as.matrix(Xtrain), y = Ytrain)
    predict_label_test <- predict(nb, as.matrix(Xtest))
    metric_test <- utsw_binary_classification_metrics(Ytest, as.matrix(as.character(predict_label_test)))
  } else if (ml_classifier == 'adaboost') {
    library(ada)
    ens <- ada(as.data.frame(Xtrain), as.factor(Ytrain), iter = 100, type = "discrete") # 100 learning cycles
    Xtestd <- as.data.frame(Xtest)
    colnames(Xtestd) <- colnames(as.data.frame(Xtrain))
    predict_label_test <- predict(ens, Xtestd)
    metric_test <- utsw_binary_classification_metrics(Ytest, as.numeric(as.character(predict_label_test)))
  } else if (ml_classifier == 'lda') {
    library(MASS)
    obj <- lda(as.matrix(Xtrain), Ytrain)
    predict_label_test <- predict(obj, as.matrix(Xtest))
    metric_test <- utsw_binary_classification_metrics(Ytest, predict_label_test)
  } else if (ml_classifier == 'lsvm') {
    #install.packages('e1071')
    library('e1071')
    lsvmModel <- svm(Xtrain, Ytrain, kernel = "linear")
    predict_label_test <- predict(lsvmModel, Xtest)
    predict_label_test <- ifelse(predict_label_test > 0.5, '1', '0')
    metric_test <- utsw_binary_classification_metrics(Ytest, predict_label_test)
  } else if (ml_classifier == "rbfsvm") {
    rbfsvmModel <- svm(Xtrain, Ytrain, kernel = "radial")
    predict_label_test <- predict(rbfsvmModel, Xtest)
    predict_label_test <- ifelse(predict_label_test > 0.5, '1', '0')
    metric_test <- utsw_binary_classification_metrics(Ytest, predict_label_test)
  } else if (ml_classifier == "lr") {
    df_train <- data.frame(y = as.factor(Ytrain), Xtrain)
    model <- glm(y ~ ., data = df_train, family = binomial)
    X <- as.data.frame(Xtest)
    colnames(X) <- paste0("X", seq_len(ncol(X)))
    prob <- predict(model, newdata = X, type = "response")
    predict_label_test <- ifelse(prob > 0.5, 1, 0)
    metric_test <- utsw_binary_classification_metrics(Ytest, predict_label_test)
  } else {
    
    metric_test <- NULL
    cat("ERROR_ysd: an unseen classifier (knn, rf, nb, ens, lda) ...\n")
    return(metric_test)
  }
  # elseif strcmp(ml_classifier, 'enfs')
  #alpha <- 0.80 # If necessary
  #lasso_fit <- glmnet(Xtrain, Ytrain, alpha = alpha, nfolds = 10)
  #idxLambda1SE <- which.min(lasso_fit$lambda - lasso_fit$cvm)
  #coef <- coef(lasso_fit, s = lasso_fit$lambda[idxLambda1SE])
  #coef0 <- lasso_fit$beta0[idxLambda1SE]
  
  #predict_label_test <- as.numeric(predict(lasso_fit, newx = Xtest, s = lasso_fit$lambda[idxLambda1SE]) > 0.5)
  #metric_test <- utsw_binary_classification_metrics(Ytest, predict_label_test)
  
}

