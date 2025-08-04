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
data <- readMat("F:\\Desktop2\\ENFR-main\\data\\gse15852data.mat")

# data <- readRDS("data/gse15852data.rds") # another gene dataset
feature_name <- t(data$fa.gene.name)
feature_beni <- data$fb.patID.data[data$fc.patID.label[,2] == 0, 2:ncol(data$fb.patID.data)]
feature_mali <- data$fb.patID.data[data$fc.patID.label[,2] == 1, 2:ncol(data$fb.patID.data)]







####### ERROR
#rm(data$fa.gene.name, data$fb.patID.data, data$fc.patID.label)
###########




# (2) normalize the data using z-score
source("F:/Desktop2/Elastic R/normalization.R", echo = FALSE)
tmpX <- pre_data_normalization(rbind(feature_beni, feature_mali), 'zscore')
feature_beni <- tmpX[1:nrow(feature_beni), ]
feature_mali <- tmpX[(nrow(feature_beni) + 1):nrow(tmpX), ]
rm(tmpX)



# (3) ENFS
alpha <- 0.5
num_iteration <- 50
source('F:/Desktop2/Elastic R/func_ENFS_coef_prediction.R', echo = FALSE)
source('F:/Desktop2/Elastic R/utsw_binary_classification_metrics.R', echo = FALSE)
source('F:/Desktop2/Elastic R/utsw_random_data_spliting_train_test.R', echo = FALSE)
method <- 'adaptivelasso'
data_out <- func_ENFS_coef_prediction(feature_beni, feature_mali, alpha=alpha, num_iteration=num_iteration, method = method)
coef <- data_out$coef
enfs <- data_out$enfs
#data_out
#rm(data_out)

# (4) ENFR ranks
source('F:/Desktop2/Elastic R/func_ENFR_rank_importance.R', echo = FALSE)
enfr_rank <- func_ENFR_rank_importance(coef, num_iteration)

# (5) ENFR rank based linear SVM prediction
num_top_feature <- 20
modell <- 'rbfsvm'
source('F:/Desktop2/Elastic R/func_ML_incremental_feature_subsets.R', echo = FALSE)
source('F:/Desktop2/Elastic R/utsw_machine_learning_classifiers_train_test.R', echo = FALSE)
# (5.1) ENFRq
enfr_svm_freq <- func_ML_incremental_feature_subsets(enfr_rank$freq_rank$index, 
                                                     feature_beni, feature_mali, num_iteration, num_top_feature, classifier=modell)
# (5.2) ENFRw
enfr_svm_weit <- func_ML_incremental_feature_subsets(enfr_rank$weit_rank$index, 
                                                     feature_beni, feature_mali, num_iteration, num_top_feature, classifier=modell)
# (5.3) ENFRwq
enfr_svm_wetq <- func_ML_incremental_feature_subsets(enfr_rank$weit_freq_rank$index, 
                                                     feature_beni, feature_mali, num_iteration, num_top_feature, classifier = modell)
#rm(feature_beni, feature_mali)


# (6) post-processing
cat("\014") # clear console
# (6.1) coef analysis and ENFS
cat("\n------------------------------------------------------------------------------------\n")
cat("(1) ENFS \n")
coef_bin <- as.matrix(abs(coef) > 0)
p_feature_each_iter <- rowSums(coef != 0)
p_feature_involved <- sum(colSums(coef_bin) > 0)
cat(paste("... max (", max(p_feature_each_iter), "), min (", min(p_feature_each_iter), "), mean (", round(mean(p_feature_each_iter)), "), std (", round(sd(p_feature_each_iter)), "), involve (", p_feature_involved, ") ...\n", sep = ""))

cat(paste("... auc (", mean(enfs[,1]), " + ", sd(enfs[,1]), "), acc (", mean(enfs[,2]), " + ", sd(enfs[,2]), "), sen (", mean(enfs[,3]), " + ", sd(enfs[,3]), "), spe (", mean(enfs[,4]), " + ", sd(enfs[,4]), ")\n", sep = ""))
#rm(coef_bin, coef, p_feature_each_iter, p_feature_involved)



# (6.2) ENFR_q
cat("\n------------------------------------------------------------------------------------\n")
cat("(2) ENFR_q \n")
result_df1 <- data.frame(
  rank = integer(),
  freq = numeric(),
  gene = character(),
  auc_mean = numeric(),
  auc_sd = numeric(),
  acc_mean = numeric(),
  acc_sd = numeric(),
  sen_mean = numeric(),
  sen_sd = numeric(),
  spe_mean = numeric(),
  spe_sd = numeric(),
  stringsAsFactors = FALSE
)
freqy <- enfr_rank$freq_rank$freqy
frind <- enfr_rank$freq_rank$index
for (i in 1:num_top_feature) {
  tmp_enfr_freq <- enfr_svm_freq[, i, ]  # ماتریس n_iter * 4
  result_df1[i, ] <- list(
    rank = i,
    freq = freqy[i],
    gene = feature_name[frind[i]],
    auc_mean = mean(tmp_enfr_freq[, 1]),
    auc_sd   = sd(tmp_enfr_freq[, 1]),
    acc_mean = mean(tmp_enfr_freq[, 2]),
    acc_sd   = sd(tmp_enfr_freq[, 2]),
    sen_mean = mean(tmp_enfr_freq[, 3]),
    sen_sd   = sd(tmp_enfr_freq[, 3]),
    spe_mean = mean(tmp_enfr_freq[, 4]),
    spe_sd   = sd(tmp_enfr_freq[, 4])
  )
}
#rm(i, freqy, frind, tmp_enfr_freq)
result_df1

# محاسبه مقادیر موردنظر
auc_mean_val <- mean(enfs[,1])
auc_sd_val <- sd(enfs[,1])
mean_feature_count <- mean(p_feature_each_iter)

# ساخت ردیف جدید با فقط مقادیر موردنظر و سایر ستون‌ها NA
summary_row <- data.frame(
  rank = mean_feature_count,
  freq = NA,
  gene = NA,
  auc_mean = auc_mean_val,
  auc_sd = auc_sd_val,
  acc_mean = NA,
  acc_sd = NA,
  sen_mean = NA,
  sen_sd = NA,
  spe_mean = NA,
  spe_sd = NA,
  stringsAsFactors = FALSE
)

# افزودن ردیف به دیتا فریم اصلی
result_df1 <- rbind(result_df1, summary_row)
result_df1


# (6.3) ENFR_w
cat("\n------------------------------------------------------------------------------------\n")
cat("(3) ENFR_w \n")
weits <- enfr_rank$weit_rank$weits
wtind <- enfr_rank$weit_rank$index
result_df2 <- data.frame(
  rank = integer(),
  freq = numeric(),
  gene = character(),
  auc_mean = numeric(),
  auc_sd = numeric(),
  acc_mean = numeric(),
  acc_sd = numeric(),
  sen_mean = numeric(),
  sen_sd = numeric(),
  spe_mean = numeric(),
  spe_sd = numeric(),
  stringsAsFactors = FALSE
)
for (i in 1:num_top_feature) {
  tmp_enfr_weit <- enfr_svm_weit[, i, ]  # n_iter × 4
  
  result_df2[i, ] <- list(
    rank = i,
    weit = weits[i],
    gene = feature_name[wtind[i]],
    auc_mean = mean(tmp_enfr_weit[, 1]),
    auc_sd   = sd(tmp_enfr_weit[, 1]),
    acc_mean = mean(tmp_enfr_weit[, 2]),
    acc_sd   = sd(tmp_enfr_weit[, 2]),
    sen_mean = mean(tmp_enfr_weit[, 3]),
    sen_sd   = sd(tmp_enfr_weit[, 3]),
    spe_mean = mean(tmp_enfr_weit[, 4]),
    spe_sd   = sd(tmp_enfr_weit[, 4])
  )
}
result_df2
#rm(i, weits, wtind, tmp_enfr_weit)



# (6.4) ENFR_wq
cat("\n------------------------------------------------------------------------------------\n")
cat("(4) ENFR_wq \n")
weit_freq <- enfr_rank$weit_freq_rank$weit_freq
wqind <- enfr_rank$weit_freq_rank$index
result_df3 <- data.frame(
  rank = integer(),
  freq = numeric(),
  gene = character(),
  auc_mean = numeric(),
  auc_sd = numeric(),
  acc_mean = numeric(),
  acc_sd = numeric(),
  sen_mean = numeric(),
  sen_sd = numeric(),
  spe_mean = numeric(),
  spe_sd = numeric(),
  stringsAsFactors = FALSE
)
for (i in 1:num_top_feature) {
  tmp_enfr_wetq <- enfr_svm_wetq[, i, ]  # n_iter × 4
  
  result_df3[i, ] <- list(
    rank = i,
    weit_freq = weit_freq[i],
    gene = feature_name[wqind[i]],
    auc_mean = mean(tmp_enfr_wetq[, 1]),
    auc_sd   = sd(tmp_enfr_wetq[, 1]),
    acc_mean = mean(tmp_enfr_wetq[, 2]),
    acc_sd   = sd(tmp_enfr_wetq[, 2]),
    sen_mean = mean(tmp_enfr_wetq[, 3]),
    sen_sd   = sd(tmp_enfr_wetq[, 3]),
    spe_mean = mean(tmp_enfr_wetq[, 4]),
    spe_sd   = sd(tmp_enfr_wetq[, 4])
  )
}
result_df3
#rm(i, weit_freq, wqind, tmp_enfr_wetq)



########## Freq plot
# install.packages("ggplot2")

library(ggplot2)

df_top15 <- head(result_df1, 15)
df_top15$rank <- as.factor(df_top15$rank)
jpeg("F:/Desktop2/Elastic R/Result files/gse15852/adaptive lasso/freq plot/with_rbfsvm.jpeg", quality = 100)
ggplot(df_top15, aes(x = rank, y = freq, group = 1)) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Adaptive Lasso with RBF SVM") +
  theme_minimal() + 
theme(
  plot.title = element_text(hjust = 0.5),
  axis.title.x = element_blank(),
  axis.title.y = element_blank() 
)
dev.off()


################## AUC plot
jpeg("F:/Desktop2/Elastic R/Result files/gse15852/adaptive lasso/auc plot/with_rbfsvm.jpeg", quality = 100)
result_df1$index <- 1:nrow(result_df1)
x_labels <- rep("", nrow(result_df1))
x_labels[1:nrow(result_df1)] <- as.character(result_df1$rank[1:nrow(result_df1)])
# افزودن ستون index و is_last به داده
result_df1$index <- 1:nrow(result_df1)
result_df1$is_last <- result_df1$index == nrow(result_df1)

ggplot(result_df1, aes(x = index, y = auc_mean, group = 1)) +
  geom_line(color = "black", size = 1) +
  geom_point(aes(color = is_last), size = 3) +
  geom_errorbar(aes(ymin = auc_mean - auc_sd, ymax = auc_mean + auc_sd, color = is_last),
                width = 0.2) +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  scale_x_continuous(breaks = result_df1$index, labels = x_labels) +
  labs(title = "Adaptive Lasso with RBF SVM") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none" # حذف legend مربوط به رنگ‌ها
  )
dev.off()

