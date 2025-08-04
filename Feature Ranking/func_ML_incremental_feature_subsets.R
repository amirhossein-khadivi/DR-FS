# Shaode Yu, yushaodemia@163.com
#   Purpose
#       To do incremental feature subsets for machine learning based disease prediction
# -------------------------------------------------------------------------
#   Input
#       freq_rank_index, the feature rank index
#       feature_beni, benign cases [m, p], m cases with p features
#       feature_mali, malignant cases [n, p], n cases with p features
#       num_iteration, the number of iteration
#       num_top_feature, the number of top candidate features
# -------------------------------------------------------------------------
#   Output
#       metric_svm_freqy,  svm performance
# -------------------------------------------------------------------------


func_ML_incremental_feature_subsets <- function(freq_rank_index, feature_beni, feature_mali, num_iteration, num_top_feature, classifier='lsvm') {
  # Check input parameters
  if (missing(num_top_feature)) {
    num_top_feature <- 10
  }
  
  if (missing(num_iteration)) {
    cat("Error: No sufficient input parameters \n")
    enfr_svm <- NULL
    return(enfr_svm)
  }
  
  if (ncol(feature_beni) != ncol(feature_mali)) {
    cat("Error: Feature dimensions not match \n")
    enfr_svm <- NULL
    return(enfr_svm)
  }
  
  
  # (2) to do machine learning with incremental feature subsets
  metric_svm_freqy <- array(0, dim = c(num_iteration, num_top_feature, 7)) # svm performance
  
  for (i in 1:num_iteration) {
    # (4.1.1) random data splitting
    result <- utsw_random_data_spliting_train_test(feature_beni, feature_mali)
    train_nfog <- result$train_ben
    train_fog <- result$train_mal
    test_nfog <- result$test_ben
    test_fog <- result$test_mal
    Xtrain <- rbind(train_nfog, train_fog)
    Ytrain <- c(rep(0, nrow(train_nfog)), rep(1, nrow(train_fog)))
    Xtest <- rbind(test_nfog, test_fog)
    Ytest <- c(rep(0, nrow(test_nfog)), rep(1, nrow(test_fog)))
    # (4.1.2) incremental feature selection for classification
    for (j in 1:num_top_feature) {
      # re-organize the dataset
      tmp_feature_index <- freq_rank_index[1:j]
      tmp_Xtrain <- Xtrain[, tmp_feature_index]
      tmp_Xtest <- Xtest[, tmp_feature_index]
      # re-run the machine learning classifiers
      metric_svm_freqy[i, j, ] <- utsw_machine_learning_classifiers_train_test(tmp_Xtrain, Ytrain, tmp_Xtest, Ytest, ml_classifier=classifier)
    }
    rm(tmp_feature_index, tmp_Xtrain, tmp_Xtest)
  }
  return(metric_svm_freqy)
  
}
