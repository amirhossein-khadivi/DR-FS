# Shaode Yu (yushaodemia@163.com)
# -------------------------------------------------------------------------
# random data splitting
#
# %  input parameters
#      data_ben: benign samples ([m, p]) m samples and each with p features
#      data_mal: malignant samples ([n, p]) n samples and each with p features
#      case_ratio: the ratio of each groups for training (default 80% for training)
#      flag_ratio: 1 (the same number of cases in each group for training, default)
#                  0 (the sampe ratio of each group for training)
#
# %  output parameters
#      train_ben: the benign samples in the training set
#      train_mal: the malignant samples in the training set
#      ttest_ben: the benign samples in the testing set
#      ttest_mal: the malignant samples in the testing set
#      ttest_ben_index: the index of benign samples in the testing set
#      ttest_mal_index: the index of malignant samples in the testing set
# -------------------------------------------------------------------------
# v01 10/08/2020
# v02 11/16/2020
# v03 04/10/2021
# -------------------------------------------------------------------------



utsw_random_data_spliting_train_test <- function(data_ben, data_mal, case_ratio = 0.8, flag_ratio = 1) {
  # (1) to check input parameters
  if (missing(flag_ratio)) {
    flag_ratio <- 1
    # 1, the same number of cases in each group for training
    # 0, the ratio of each group
  }
  if (missing(case_ratio)) {
    case_ratio <- 0.8
    # 80% for training
  }
  
  if (missing(data_mal) || missing(data_ben)) {
    cat('ERROR: insufficient input parameters ...\n')
    train_ben <- NULL
    train_mal <- NULL
    ttest_ben <- NULL
    ttest_mal <- NULL
    ttest_ben_index <- NULL
    ttest_mal_index <- NULL
    return(list(train_ben = train_ben, train_mal = train_mal, ttest_ben = ttest_ben, ttest_mal = ttest_mal, ttest_ben_index = ttest_ben_index, ttest_mal_index = ttest_mal_index))
  }
  
  # (2) to check whether feature dimensions match
  if (ncol(data_ben) != ncol(data_mal)) {
    cat('ERROR: feature dimension not match ...\n')
    train_ben <- NULL
    train_mal <- NULL
    ttest_ben <- NULL
    ttest_mal <- NULL
    ttest_ben_index <- NULL
    ttest_mal_index <- NULL
    return(list(train_ben = train_ben, train_mal = train_mal, ttest_ben = ttest_ben, ttest_mal = ttest_mal, ttest_ben_index = ttest_ben_index, ttest_mal_index = ttest_mal_index))
  }
  
  # (3) random data splitting
  num_ben <- nrow(data_ben)
  num_mal <- nrow(data_mal)
  # (3.1) to determine the number of cases for training
  if (flag_ratio == 1) {
    case_base_ben <- round(case_ratio * min(num_ben, num_mal))
    # to select the group with fewer cases as the baseline
    case_base_mal <- case_base_ben
    # to keep the number of cases from each group the same
  } else {
    case_base_ben <- round(case_ratio * num_ben)
    case_base_mal <- round(case_ratio * num_mal)
    # to keep the ratio of cases from each group the same
  }
  
  # (3.2) to split the cases into training and testing sets
  rand_ben <- sample(num_ben)
  rand_mal <- sample(num_mal)
  
  # Rest of the code...
  # (3.2.1) the training set
  train_ben <- data_ben[sample(rand_ben[1:case_base_ben]), ]
  train_mal <- data_mal[sample(rand_mal[1:case_base_mal]), ]
  
  # (3.2.2) the testing set
  test_ben <- data_ben[sample(rand_ben[(1+case_base_ben):length(rand_ben)]), ]
  test_mal <- data_mal[sample(rand_mal[(1+case_base_mal):length(rand_mal)]), ]
  
  # (3.2.3) the index for the testing set
  #             if the wrong classification cases are interested for analysis
  test_ben_index <- rand_ben[(1+case_base_ben):length(rand_ben)]
  test_mal_index <- rand_mal[(1+case_base_mal):length(rand_mal)]
  data <- list(train_ben= train_ben, train_mal= train_mal, test_ben= test_ben, test_mal= test_mal)
  return(data)
}

