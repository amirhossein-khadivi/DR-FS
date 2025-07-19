# Shaode Yu, yushaodemia@163.com
#
#   Purpose
#       High-dimensional small-sample-size data analysis
#           using elastic net based feature ranking
#
#   Main steps
#       (1) import dataset
#       (2) z-score data normalization
#               using
#                   pre_data_normalization.R
#       (3) elastic net based feature selection (ENFS)
#               using 
#                   func_ENFS_coef_prediction.R
#       (4) elastic net based feature ranking   (ENFR)
#               using
#                   func_ENFR_rank_importance.R
#       (5) incremental feature subsets for classification
#               using
#                   utsw_machine_learning_classifiers_train_test.R
#       (*) random data spliting
#               using
#                   utsw_random_data_spliting_train_test.R
#       (*) evaluation of binary classification performance
#               using
#                   utsw_binary_classification_metrics.R
#
# -------------------------------------------------------------------------

# (1) load data and get gene name (fa_gene_name, fb_patID_data, fc_patID_label)

#install.packages('R.matlab')
library(R.matlab)
data <- readMat("F:\\Desktop2\\ENFR-main\\data\\gse10810data.mat")

# data <- readRDS("data/gse15852data.rds") # another gene dataset
feature_name <- t(data$fa.gene.name)
feature_beni <- data$fb.patID.data[data$fc.patID.label[,2] == 0, 2:ncol(data$fb.patID.data)]
feature_mali <- data$fb.patID.data[data$fc.patID.label[,2] == 1, 2:ncol(data$fb.patID.data)]







####### ERROR
#rm(data$fa.gene.name, data$fb.patID.data, data$fc.patID.label)
###########




# (2) normalize the data using z-score
tmpX <- pre_data_normalization(rbind(feature_beni, feature_mali), 'zscore')
feature_beni <- tmpX[1:nrow(feature_beni), ]
feature_mali <- tmpX[(nrow(feature_beni) + 1):nrow(tmpX), ]
rm(tmpX)



# (3) ENFS
alpha <- 0.8
num_iteration <- 50
data_out <- func_ENFS_coef_prediction(feature_beni, feature_mali, alpha, num_iteration)
coef <- data_out$coef
enfs <- data_out$enfs
#data_out
rm(data_out)

# (4) ENFR ranks
enfr_rank <- func_ENFR_rank_importance(coef, num_iteration)

# (5) ENFR rank based linear SVM prediction
num_top_feature <- 20
# (5.1) ENFRq
enfr_svm_freq <- func_ML_incremental_feature_subsets(enfr_rank$freq_rank$index, 
                                                     feature_beni, feature_mali, num_iteration, num_top_feature)
# (5.2) ENFRw
enfr_svm_weit <- func_ML_incremental_feature_subsets(enfr_rank$weit_rank$index, 
                                                     feature_beni, feature_mali, num_iteration, num_top_feature)
# (5.3) ENFRwq
enfr_svm_wetq <- func_ML_incremental_feature_subsets(enfr_rank$weit_freq_rank$index, 
                                                     feature_beni, feature_mali, num_iteration, num_top_feature)
rm(feature_beni, feature_mali)


# (6) post-processing
cat("\014") # clear console
# (6.1) coef analysis and ENFS
cat("\n------------------------------------------------------------------------------------\n")
cat("(1) ENFS \n")
coef_bin <- as.matrix(abs(coef) > 0)
p_feature_each_iter <- colSums(coef_bin)
p_feature_involved <- sum(colSums(coef_bin) > 0)
cat(paste("... max (", max(p_feature_each_iter), "), min (", min(p_feature_each_iter), "), mean (", round(mean(p_feature_each_iter)), "), std (", round(sd(p_feature_each_iter)), "), involve (", p_feature_involved, ") ...\n", sep = ""))

cat(paste("... auc (", mean(enfs[,1]), " + ", sd(enfs[,1]), "), acc (", mean(enfs[,2]), " + ", sd(enfs[,2]), "), sen (", mean(enfs[,3]), " + ", sd(enfs[,3]), "), spe (", mean(enfs[,4]), " + ", sd(enfs[,4]), ")\n", sep = ""))
rm(coef_bin, coef, p_feature_each_iter, p_feature_involved)



# (6.2) ENFR_q
cat("\n------------------------------------------------------------------------------------\n")
cat("(2) ENFR_q \n")
freqy <- enfr_rank$freq_rank$freqy
frind <- enfr_rank$freq_rank$index
for (i in 1:num_top_feature) {
  #cat(paste("... top-(", i, "), \t freqy (", freqy[i], "), \t gene (", feature_name[frind[i]], ")\n"))
  
  tmp_enfr_freq <- enfr_svm_freq[,i,]
  cat("\t \t \t \t \t \t auc (", mean(tmp_enfr_freq[,1]), " + ", sd(tmp_enfr_freq[,1]), "), acc (", mean(tmp_enfr_freq[,2]), " + ", sd(tmp_enfr_freq[,2]), "), 
      sen (", mean(tmp_enfr_freq[,3]), " + ", sd(tmp_enfr_freq[,3]), "), spe (", mean(tmp_enfr_freq[,4]), " + ", sd(tmp_enfr_freq[,4]), ")\n")
}
rm(i, freqy, frind, tmp_enfr_freq)



# (6.3) ENFR_w
cat("\n------------------------------------------------------------------------------------\n")
cat("(3) ENFR_w \n")
weits <- enfr_rank$weit_rank$weits
wtind <- enfr_rank$weit_rank$index
for (i in 1:num_top_feature) {
  cat(paste("... top-(", i, "), \t weits (", weits[i], "), \t gene (", feature_name[wtind[i]], ")\n"))
  #
  tmp_enfr_weit <- enfr_svm_weit[,i,]
  cat("\t \t \t \t \t \t auc (", mean(tmp_enfr_weit[,1]), " + ", sd(tmp_enfr_weit[,1]), "), acc (", mean(tmp_enfr_weit[,2]), " + ", sd(tmp_enfr_weit[,2]), "), sen (", mean(tmp_enfr_weit[,3]), " + ", sd(tmp_enfr_weit[,3]), "), spe (", mean(tmp_enfr_weit[,4]), " + ", sd(tmp_enfr_weit[,4]), ")\n")
}
rm(i, weits, wtind, tmp_enfr_weit)



# (6.4) ENFR_wq
cat("\n------------------------------------------------------------------------------------\n")
cat("(4) ENFR_wq \n")
weit_freq <- enfr_rank$weit_freq_rank$weit_freq
wqind <- enfr_rank$weit_freq_rank$index
for (i in 1:num_top_feature) {
  cat(paste("... top-(", i, "), \t weit_freq (", weit_freq[i], "), \t gene (", feature_name[wqind[i]], ")\n"))
  #
  tmp_enfr_wetq <- enfr_svm_wetq[,i,]
  cat("\t \t \t \t \t \t auc (", mean(tmp_enfr_wetq[,1]), " + ", sd(tmp_enfr_wetq[,1]), "), acc (", mean(tmp_enfr_wetq[,2]), " + ", sd(tmp_enfr_wetq[,2]), "), sen (", mean(tmp_enfr_wetq[,3]), " + ", sd(tmp_enfr_wetq[,3]), "), spe (", mean(tmp_enfr_wetq[,4]), " + ", sd(tmp_enfr_wetq[,4]), ")\n")
}
rm(i, weit_freq, wqind, tmp_enfr_wetq)

