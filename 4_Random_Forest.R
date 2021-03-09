# Training of a random forest classifier to distinguish between sound clips containing 
# nocturnal flight calls (NFCs) and sound clips not containing NFCs;

# Input: features extracted from sound clips;

# Training sets are artificially balanced to contain 50% sound clips with NFCs;
# Evaluation is based on a "leave-one-test group-out" cross-validation procedure


# PACKAGES: ==========================================================================

library(ranger)


# MAIN PART ==========================================================================

# load data:
# extracted features = input for the random forest classifier:
load(file = "~/Master/Masterarbeit/Code/Code_final/df_max_freq_spec_spectro_feat_final.RData")


# Random Forest:

# "leave-one-group-out" cross-validation procedure; 
# each group = test group in one cross-validation run
groups <- sort(unique(df_max_freq_spec_spectro_feat_tot$group))

# store Random Forest predictions:
rfs_preds_final1 <- vector(mode = "list", length = length(groups))

# store importance of input variables:
importance_preds_final1 <- as.data.frame(matrix(nrow = length(groups), ncol = 281))
colnames(importance_preds_final1) <- c(paste0("bw1_f_", 1:256), 
                                       paste0(c("f_25_ampl_bw1", "f_50_ampl_bw1", 
                                                "f_75_ampl_bw1", "f_90_ampl_bw1")),
                                       paste0(c("ampl_sum_0_1kHz", "ampl_sum_1_2kHz", 
                                                "ampl_sum_2_3kHz", "ampl_sum_3_6kHz", 
                                                "ampl_sum_6_8kHz", "ampl_sum_8_11kHz")),
                                       paste0("pks_", 0:10, "_", 1:11, "kHz_bw1"),
                                       paste0(c("event_duration", "event_freq_range", 
                                                "event_min_freq", "event_max_freq")))


# train Random Forest:

## formula:
fmla <- as.formula(paste("as.factor(bird_event) ~ ", 
                         paste0("bw1_f_", 1:256, collapse= "+"), "+", 
                         paste0(c("f_25_ampl_bw1", "f_50_ampl_bw1", "f_75_ampl_bw1", 
                                  "f_90_ampl_bw1"), collapse = "+"), "+", 
                         paste0(c("ampl_sum_0_1kHz", "ampl_sum_1_2kHz", "ampl_sum_2_3kHz", 
                                  "ampl_sum_3_6kHz", "ampl_sum_6_8kHz", "ampl_sum_8_11kHz"),
                                collapse = "+"), "+",
                         paste0("pks_", 0:10, "_", 1:11, "kHz_bw1", collapse= "+"), "+",
                         paste0(c("event_duration", "event_freq_range", "event_min_freq", 
                                  "event_max_freq"), collapse= "+")))

set.seed(123)

for(n in c(1:length(groups))) {
  
  print(n) # test group
  
  # artificially balance training set:
  
  ## sound clips containing only background noise:
  df_max_freq_spec_spectro_feat_tot_bal <- df_max_freq_spec_spectro_feat_tot[which(df_max_freq_spec_spectro_feat_tot$bird_event == 0 & df_max_freq_spec_spectro_feat_tot$group != groups[n]),]
  
  ## number of sound clips containing only background noise:
  n_noise <- nrow(df_max_freq_spec_spectro_feat_tot_bal)
  
  ## sound clips containing NFCs:
  row_be <- which(df_max_freq_spec_spectro_feat_tot$bird_event == 1 & 
                    df_max_freq_spec_spectro_feat_tot$group != groups[n])
  
  ## upsampling of sound clips containing NFCs, until 50% of training set contains NFCs:
  row_be_upsmpl <- sample(row_be, size = n_noise, replace = T)
  df_max_freq_spec_spectro_feat_tot_bal <- rbind(df_max_freq_spec_spectro_feat_tot_bal, 
                                                 df_max_freq_spec_spectro_feat_tot[row_be_upsmpl,])
  
  # run Random Forest:
  
  rf <- ranger(fmla , 
               data = df_max_freq_spec_spectro_feat_tot_bal, 
               num.threads = 7, 
               importance="impurity", 
               probability = T, 
               num.trees = 500, 
               oob.error = F) 
  
  # predict to test group:
  
  rfs_preds_final1[[n]] <- predict(rf, 
                                   data = df_max_freq_spec_spectro_feat_tot[which(df_max_freq_spec_spectro_feat_tot$group == groups[n]),])$predictions[,2]
  
  # variable importance:
  importance_preds_final1[n,] <- as.numeric(importance(rf)) 
  
  # remove RF:
  rm(rf)
  
  # remove artificially balanced training set:
  rm(df_max_freq_spec_spectro_feat_tot_bal)

}


# store Random Forest predictions and input variable importance:
save(rfs_preds_final1,
     importance_preds_final1,
     file = "~/Master/Masterarbeit/Code/Code_final/rfs_preds.RData")