# Evaluation of performance of the random forest classifier and BirdVoxDetect 
# regarding the detection of nocturnal flight calls in audio recordings;

# Evaluation is based on a "leave-one-test group-out" cross-validation procedure


# PACKAGES: ==========================================================================

library(tidymodels)
library(yardstick)
library(lubridate)
library(viridis)
library(gridExtra)
library(tuneR)
library(seewave)

# FUNCTIONS (load from scripts): =====================================================

source("~/Master/Masterarbeit/Code/Code_final/1_Functions.R") 

# MAIN PART ==========================================================================

# load data:
# audiofile metadata:
load(file = "~/Master/Masterarbeit/Code/Code_final/audio_metadata.RData")
# recorded species:
load(file = "~/Master/Masterarbeit/Code/Code_final/recorded_species.RData")
# grouping of recorded nights:
load(file = "~/Master/Masterarbeit/Code/Code_final/grouping_table.RData")
# extracted features = input for the random forest classifier:
load(file = "~/Master/Masterarbeit/Code/Code_final/df_max_freq_spec_spectro_feat_final.RData")
# predictions of the random forest classifier:
load(file = "~/Master/Masterarbeit/Code/Code_final/rfs_preds.RData")
# processed predictions of BirdVoxDetect:
load(file = "~/Master/Masterarbeit/Code/Code_final/BVD_preds.RData")

# Merge input features and predictions: ----------------------------------------------

# groups:
groups <- sort(unique(df_max_freq_spec_spectro_feat_tot$group))

# RF:
dt_preds <- data.frame(df_max_freq_spec_spectro_feat_tot[which(df_max_freq_spec_spectro_feat_tot$group == 1),],
                       "rf_preds_1" = unlist(rfs_preds_final1[[1]]))

for(i in 2:length(groups)){
  dt_preds <- rbind(dt_preds, 
                    data.frame(df_max_freq_spec_spectro_feat_tot[which(df_max_freq_spec_spectro_feat_tot$group == i),],
                               "rf_preds_1" = unlist(rfs_preds_final1[[i]])))
}

# add BVD:

dt_preds2 <- merge(dt_preds, BVD_conf_df, by = c("audiofile", "time_in_record_s"), all = F, sort = F)

# if channel= 1 or NA, merge left, if channel = 2 merge right:
dt_preds2$max_conf_BVD <- ifelse(dt_preds2$channel == 1 | is.na(dt_preds2$channel), 
                                 dt_preds2$max_conf_BVD_left, 
                                 dt_preds2$max_conf_BVD_right)



# Sensitivity, specificity, precision: -----------------------------------------------
##  Boxplots of evaluation measures (Fig. 3.1)


# RF:

# threshold to convert probabilistic output to binary class predictions: prevalence of positive class in respective training data:

# prevalence of training data per test group:

prev_pos_train <- data.frame("group" = 1:21, prev_pos_train = NA)

for(g in 1:length(prev_pos_train$group)){
  n_pos <- length(which(dt_preds2$bird_event == 1 & dt_preds2$group != prev_pos_train$group[g]))
  n_neg <- length(which(dt_preds2$bird_event == 0 & dt_preds2$group != prev_pos_train$group[g]))
  prev_pos_train$prev_pos_train[g] <- n_pos/(n_pos + n_neg)
}

# add prevalence to data frame:
dt_preds2 <- merge(dt_preds2, prev_pos_train, sort = F)
colnames(dt_preds2)[which(colnames(dt_preds2) == "prev_pos_train")] <- "prev_pos_RF_train"

# convert prediction scores to binary predictions:
dt_preds2$RF_pred_thresh_prev_train <- factor(ifelse(dt_preds2$rf_preds_1 >= dt_preds2$prev_pos_RF_train, 1, 0), 
                                              levels = c("1","0"))


# sensitivity, specitivity and precision for each group:

# change factor level order:
dt_preds2 <- dt_preds2 %>% 
  mutate_at(vars(bird_event), list(. %>% factor() %>% forcats::fct_relevel("1")))

group_recall <- dt_preds2 %>% 
  group_by(group) %>%
  recall(truth = bird_event, estimate = RF_pred_thresh_prev_train)

group_specificity <- dt_preds2 %>% 
  group_by(group) %>%
  specificity(truth = bird_event, estimate = RF_pred_thresh_prev_train)

group_precision <- dt_preds2 %>% 
  group_by(group) %>%
  precision(truth = bird_event, estimate = RF_pred_thresh_prev_train)

group_eval_measures_prev_train_RF <- data.frame("group" = group_recall$group, 
                                                "Sensitivity" = group_recall$.estimate, 
                                                "Specificity" = group_specificity$.estimate, 
                                                "Precision" = group_precision$.estimate)


# BVD:

# threshold: prevalence of whole data set: 3 %

# convert prediction scores to binary predictions:
dt_preds2$BVD_pred_thresh_all_dt <- factor(ifelse(dt_preds2$max_conf_BVD >= 0.03, 1, 0), 
                                           levels = c("1","0"))


# sensitivity, specitivity and precision for each group:

group_recall <- dt_preds2 %>% 
  group_by(group) %>%
  recall(truth = bird_event, estimate = BVD_pred_thresh_all_dt)

group_specificity <- dt_preds2 %>% 
  group_by(group) %>%
  specificity(truth = bird_event, estimate = BVD_pred_thresh_all_dt)

group_precision <- dt_preds2 %>% 
  group_by(group) %>%
  precision(truth = bird_event, estimate = BVD_pred_thresh_all_dt)

group_eval_measures_all_data_prev_BVD <- data.frame("group" = group_recall$group, 
                                                    "Sensitivity" = group_recall$.estimate, 
                                                    "Specificity" = group_specificity$.estimate, 
                                                    "Precision" = group_precision$.estimate)


# Plot: 

# RF: change to long format:

group_eval_measures_prev_train_RF_long <- group_eval_measures_prev_train_RF %>% 
  group_by(group) %>%
  gather(key = "measure", 
         value = "value", 
         -group,
         factor_key = T)

# sensitivity, specificity and precision for entire data set:

group_eval_measures_prev_train_RF_long$value_all_groups <- NA
group_eval_measures_prev_train_RF_long$value_all_groups[which(group_eval_measures_prev_train_RF_long$measure == "Sensitivity")] <- 
  as.numeric(recall(data = dt_preds2, truth = bird_event, estimate = RF_pred_thresh_prev_train)[3])
group_eval_measures_prev_train_RF_long$value_all_groups[which(group_eval_measures_prev_train_RF_long$measure == "Specificity")] <- 
  as.numeric(specificity(data = dt_preds2, truth = bird_event, estimate = RF_pred_thresh_prev_train)[3])
group_eval_measures_prev_train_RF_long$value_all_groups[which(group_eval_measures_prev_train_RF_long$measure == "Precision")] <- 
  as.numeric(precision(data = dt_preds2, truth = bird_event, estimate = RF_pred_thresh_prev_train)[3])


# BVD: change to long format:

group_eval_measures_all_data_prev_BVD_long <- group_eval_measures_all_data_prev_BVD %>% 
  group_by(group) %>%
  gather(key = "measure", 
         value = "value", 
         -group,
         factor_key = T)

# sensitivity, specificity and precision for entire data set:

group_eval_measures_all_data_prev_BVD_long$value_all_groups <- NA
group_eval_measures_all_data_prev_BVD_long$value_all_groups[which(group_eval_measures_all_data_prev_BVD_long$measure == "Sensitivity")] <- as.numeric(recall(data = dt_preds2, truth = bird_event, estimate = BVD_pred_thresh_all_dt)[3])
group_eval_measures_all_data_prev_BVD_long$value_all_groups[which(group_eval_measures_all_data_prev_BVD_long$measure == "Specificity")] <- as.numeric(specificity(data = dt_preds2, truth = bird_event, estimate = BVD_pred_thresh_all_dt)[3])
group_eval_measures_all_data_prev_BVD_long$value_all_groups[which(group_eval_measures_all_data_prev_BVD_long$measure == "Precision")] <- as.numeric(precision(data = dt_preds2, truth = bird_event, estimate = BVD_pred_thresh_all_dt)[3])


# Boxplots for BVD and RF in one plot:

# merge data frames for the RF and BVD:

RF_BVD_preds_thresh <- rbind(group_eval_measures_prev_train_RF_long, 
                             group_eval_measures_all_data_prev_BVD_long)

# add model:
RF_BVD_preds_thresh$model <- factor(c(rep("RF", nrow(group_eval_measures_prev_train_RF_long)), 
                                      rep("BVD", nrow(group_eval_measures_all_data_prev_BVD_long))), 
                                    levels = c("RF", "BVD"))

# Plot:
ggplot(data = RF_BVD_preds_thresh, aes(y = value, x = model, fill = model)) +
  stat_boxplot(geom = "errorbar", width = 0.2) + 
  geom_boxplot(width = 0.5) +
  facet_grid(~ measure) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 15, colour = "black"),
        legend.position = "bottom",
        text = element_text(size = 15),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm"),
        strip.background = element_rect(fill = c("grey80"),
                                        colour = "black"),
        strip.text = element_text(size = 15)) +
  xlab("") +
  ylab("") +
  labs(fill = "") +
  geom_text(aes(y = value_all_groups, x = model), label="*", size = 5) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), expand = expansion(mult = c(0.05, 0.05))) + 
  scale_fill_manual(values = c("#d7191c", "#2c7bb6"),labels=c("Random Forest", "BirdVoxDetect")) +
  scale_x_discrete(expand=c(0.8,0)) 

# extract values:
median(RF_BVD_preds_thresh$value[which(RF_BVD_preds_thresh$model == "RF" 
                                       & RF_BVD_preds_thresh$measure == "Sensitivity")])
median(RF_BVD_preds_thresh$value[which(RF_BVD_preds_thresh$model == "BVD" 
                                       & RF_BVD_preds_thresh$measure == "Sensitivity")])
median(RF_BVD_preds_thresh$value[which(RF_BVD_preds_thresh$model == "RF" 
                                       & RF_BVD_preds_thresh$measure == "Specificity")])
median(RF_BVD_preds_thresh$value[which(RF_BVD_preds_thresh$model == "BVD" 
                                       & RF_BVD_preds_thresh$measure == "Specificity")])
median(RF_BVD_preds_thresh$value[which(RF_BVD_preds_thresh$model == "RF" 
                                       & RF_BVD_preds_thresh$measure == "Precision")])
median(RF_BVD_preds_thresh$value[which(RF_BVD_preds_thresh$model == "BVD" 
                                       & RF_BVD_preds_thresh$measure == "Precision")])




# Variable importance: ---------------------------------------------------------------

importance_preds_final1

View(sort(colMeans(importance_preds_final1), decreasing = T))

# Importance of frequency bands:

freq <- seq(0, (22050/2) - (22050/512), length.out = 256)/1000
plot(freq,
     importance_preds_final1[1,1:256], 
     type = "l", 
     ylim = c(0, 2200),
     xlab = "Frequency [kHz]",
     ylab = "Variable importance [no unit]",
     las = 1,
     yaxt = "n")
for(i in 2:nrow(importance_preds_final1)){
  lines(freq,importance_preds_final1[i,1:256])
}



# RF False Positives: ----------------------------------------------------------------

# manually check false positives of each group:
View(dt_preds2[which(dt_preds2$group == 21), c(1:7, 545)]) # xx

# categories typical content of a sound clip predicted false positive:
fps_RF <- data.frame("group" = 1:21, 
                     "actual_TPs" = 0, 
                     "other_bird_sounds" = 0, 
                     "other_animal_sounds" = 0, 
                     "human_voices" = 0,
                     "other_sounds" = 0)
fps_RF[1,4] <- 5
fps_RF[1,6] <- 5
fps_RF[2,4] <- 4
fps_RF[2,6] <- 6
fps_RF[3,2] <- 3
fps_RF[3,3] <- 1
fps_RF[3,6] <- 6
fps_RF[4,2] <- 1
fps_RF[4,3] <- 2
fps_RF[4,4] <- 3
fps_RF[4,6] <- 4
fps_RF[5,2] <- 2
fps_RF[5,3] <- 6
fps_RF[5,6] <- 2
fps_RF[6,3] <- 6
fps_RF[6,4] <- 4
fps_RF[6,3] <- 6
fps_RF[6,4] <- 4
fps_RF[7,5] <- 9
fps_RF[7,6] <- 1
fps_RF[8,6] <- 8
fps_RF[8,3] <- 2
fps_RF[9,6] <- 6
fps_RF[9,3] <- 4
fps_RF[10,6] <- 6
fps_RF[10,3] <- 1
fps_RF[10,5] <- 3
fps_RF[11,3] <- 6
fps_RF[11,6] <- 3
fps_RF[11,5] <- 1
fps_RF[12,2] <- 1
fps_RF[12,3] <- 2
fps_RF[12,6] <- 7
fps_RF[13,6] <- 10
fps_RF[14,6] <- 9
fps_RF[14,3] <- 1
fps_RF[15,2] <- 1
fps_RF[15,3] <- 1
fps_RF[15,6] <- 8
fps_RF[16,2] <- 2
fps_RF[16,3] <- 2
fps_RF[16,5] <- 3
fps_RF[16,6] <- 3
fps_RF[17,2] <- 1
fps_RF[17,5] <- 1
fps_RF[17,6] <- 8
fps_RF[18,2] <- 2
fps_RF[18,3] <- 2
fps_RF[18,6] <- 6
fps_RF[19,2] <- 6
fps_RF[19,6] <- 4
fps_RF[20,2] <- 3
fps_RF[20,6] <- 7
fps_RF[21,2] <- 5
fps_RF[21,6] <- 5

# Plot (Fig. C1):

# change to long format:

fps_RF_long <- gather(fps_RF, key = "sound_type", value = "count", -group)
fps_RF_long$sound_type <- factor(fps_RF_long$sound_type, 
                                 levels = c("actual_TPs", "other_bird_sounds", 
                                            "other_animal_sounds", "human_voices", "other_sounds"))
fps_RF_long$group <- factor(fps_RF_long$group, levels= 1:21)

ggplot(data = fps_RF_long, aes(x = group, y = count)) +
  geom_col(aes(fill = sound_type), width = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15, colour = "black"), 
        axis.text.y = element_text(size = 15, colour = "black"),
        legend.position = "right",
        text = element_text(size = 15),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  ylab("Count") + 
  xlab("Test group") +
  labs(fill = "Sound type") +
  ylim(c(0, 10)) +
  coord_equal() +
  scale_fill_viridis(discrete=T, begin = 0, end = 1, direction = -1, 
                     labels = c("NFC", "other bird sound", "non-bird animal sound", 
                                "human voices", "other")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 10, 1)) +
  geom_label(aes(x, y, label=lab),
             data=data.frame(x=fps_RF_long$group, 
                             y=10.5,
                             lab= c("SP", "WM", "S", "FR", "FR", "R", "FR", "M", "FR", 
                                    "FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", "FR", 
                                    "FR", "FR", "FR" )),
             size = 5,
             show.legend = F, 
             fontface = 1, 
             label.size = 0,
             vjust = 0.8)


# RF False Negatives: ----------------------------------------------------------------

# compare heuristically 3 calls with highest prediction score and 3 calls with lowest 
# prediction score for certain species:
View(dt_preds2[which(grepl(pattern = "Amsel", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Feldlerche", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Rotkehlchen", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Ortolan", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Rotdrossel", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Singdrossel", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Grauschnäpper", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Baumpieper", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Lachmöwe", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Flussuferläufer", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Bekassine", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Flussregenpfeifer", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Teichhuhn", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Bläßhuhn", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Wasserralle", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Krickente", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Rohrdommel", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Graureiher", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Zwergdommel", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Nachtreiher", x = dt_preds2$species)), c(1:7, 545)])
View(dt_preds2[which(grepl(pattern = "Purpurreiher", x = dt_preds2$species)), c(1:7, 545)])


# Export example plot:

# Blackbird:

audio_dir <- "D:/Masterarbeit/Tonaufnahmen/Originalaufnahmen/"
#audio_file <- "200930_07_08_04_Freiburg_BW_D_NM_RM_LS_36632" # 3460 s, channel 2, faint blackbird NFC
audio_file <- "201002_07_13_26_Freiburg_BW_D_NM_RM_LS_36637" # 5720 s, channel 2, loud blackbird NFC

# read sound file:
s <- readWave(file = paste0(audio_dir, audio_file, ".wav") , 
              from = 5720, 
              to = 5730, 
              units = "seconds")

# preprocessing:

# lowpass filter:
s_filtered <- fir(s, f = 44100, to = 22050/2, output="Wave", channel = 2)
# downsample to 22050 Hz (e.g. Lasseck 2018):
s_22050Hz <- resamp(s_filtered, f = 44100, g = 22050, output = "Wave")
# butterworth filter (decrease noise):
s_bwfilter <- bwfilter(s_22050Hz, n = 2, from = 1000, output="Wave")

# spectrogram:
spec_filter<- spectro(s_bwfilter, 
                      channel = 1,
                      f = 22050, 
                      wl = 512, 
                      wn = "hanning", 
                      ovlp = 75,
                      fftw = T, 
                      plot = F,
                      norm = T,
                      dB = "max0")
# Plot:
image(x = spec_filter$time, y = spec_filter$freq, z = t(spec_filter$amp),
      xlab = "", ylab = "",
      col = gray.colors(512, start = 0.2, end = 1, rev = T),
      las = 1,
      cex.axis = 1.4)
title(ylab = "Frequency [kHz]", xlab = "Time [s]", line = 2, cex.lab=1.4)

# faint NFC:
#text(x = 2, y = 10.2, "RF prediction score =  0.01", cex = 1.4) # png
#rect(xleft = 1.6, ybottom = 6.3, xright = 2.2, ytop = 7.1, col = NA, border = "red3")

# loud NFC:
text(x = 2, y = 10.2, "RF prediction score =  0.37", cex = 1.4) # png
rect(xleft = 6.5, ybottom = 6.4, xright = 7, ytop = 10.2, col = NA, border = "red3")

# png export: cex = 1.4, width = 1000, height = 545
# pdf export: cex = 1, size = A6, 4.13 x 7



# ROC curves: ------------------------------------------------------------------------
## per test group

# RF:

# ROC:
roc_dat_rf1 <- dt_preds2 %>%
  group_by(group) %>% 
  roc_curve(bird_event, rf_preds_1)

# AUC - ROC:
auroc_rf1 <- dt_preds2 %>%
  group_by(group) %>% 
  roc_auc(bird_event, rf_preds_1)


# BVD:

# ROC:
roc_dat_BVD <- dt_preds2 %>%
  group_by(group) %>% 
  roc_curve(bird_event, max_conf_BVD)

# AUC - ROC:
auroc_bvd <- dt_preds2 %>%
  group_by(group) %>% 
  roc_auc(bird_event, max_conf_BVD)


# Plot ROC curves:

# ROC curve RF:

# colour by recording location:
roc_dat_rf1_loc <- merge(roc_dat_rf1, 
                         bes_per_night_loc[!duplicated(bes_per_night_loc$group),], 
                         by = "group")
roc_dat_rf1_loc$group <- factor(roc_dat_rf1_loc$group, 
                                levels= c(9:21, 7, 4, 5, 1:3, 6, 8)) # sets plotting order

roc_plot_rf <- roc_dat_rf1_loc %>%
  arrange(.threshold) %>%
  mutate(location = factor(location, levels=c("Freiburg (SW-Germany)", "Schauinsland (SW-Germany)", "Wildes Moor (N-Germany)", "Mellum (N-Germany)", "Spain", "Romania"))) %>%
  ggplot(aes(colour = as.factor(location))) +
  geom_path(aes(1 - specificity, sensitivity, group = as.factor(group)), size = 1) + # connect the points in the order in which they appear in the data to form a curve
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") + # add a reference line by convention
  coord_equal() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 20, colour = "black"),
        axis.text.y = element_text(size = 20, colour = "black"),
        legend.position = "right",
        text = element_text(size = 20),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(size = 20),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -2),
  )+
  labs(col = "Location") +
  xlab("False positive rate") +
  ylab("Sensitivity") +
  scale_color_viridis(discrete=T, begin = 0, end = 1, direction = -1, option = "D") +
  ggtitle("Random Forest")


# ROC curve BVD:

# colour by recording location:
roc_dat_BVD_loc <- merge(roc_dat_BVD, 
                         bes_per_night_loc[!duplicated(bes_per_night_loc$group),], 
                         by="group")
roc_dat_BVD_loc$group <- factor(roc_dat_BVD_loc$group, 
                                levels= c(9:21, 7, 4, 5, 1:3, 6, 8)) # sets plotting order


roc_plot_bvd <- roc_dat_BVD_loc %>%
  arrange(.threshold) %>%
  mutate(location = factor(location, levels=c("Freiburg (SW-Germany)", "Schauinsland (SW-Germany)", "Wildes Moor (N-Germany)", "Mellum (N-Germany)", "Spain", "Romania"))) %>%
  ggplot(aes(colour = as.factor(location))) + # location
  geom_path(aes(1 - specificity, sensitivity, group = as.factor(group)), size = 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") + 
  coord_equal() +
  theme_bw() +
  labs(col = "") +
  xlab("False positive rate") +
  ylab("") +
  scale_color_viridis(discrete=T, begin = 0, end = 1, direction = -1) +
  theme(axis.text.x = element_text(size = 20, colour = "black"),
        axis.text.y = element_blank(),
        #axis.text.y = element_text(size = 15, colour = "black"),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(size = 20, margin = margin(r = 2, unit = 'cm')),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -2),
  )+
  ggtitle("BirdVoxDetect")


# ROC for RF and BVD in one plot with one legend (Fig. D1):

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

extr_legend <- g_legend(roc_plot_bvd)

grid.arrange(arrangeGrob(roc_plot_rf + theme(legend.position="none"),
                         roc_plot_bvd + theme(legend.position="none"),
                         nrow=1),
             extr_legend, nrow=2, heights=c(10, 3))

# export: 8.27 x 13


# AUC scatterplot RF & BVD: ----------------------------------------------------------

auc_df <- data.frame("group" = 1:21, "auc_rf" = auroc_rf1$.estimate, "auc_bvd" = auroc_bvd$.estimate)
# add recording night and location
auc_df2 <- merge(auc_df, bes_per_night_loc[,c("group", "location", "nights", "n_specs")], by = "group") 
# add year-month
auc_df2$year_month <- format(as.Date(auc_df2$nights), "%Y-%m") 
auc_df3 <- auc_df2[which(duplicated(auc_df2[,-5]) == F),] # remove duplicates
auc_df3 <- auc_df3[-c(12, 15),] # if night pool spans several months use last one only

# Correlation - Spearmans rho:
cor.test(auc_df3$auc_rf, auc_df3$auc_bvd, method = "s")
cor.test(auc_df3$auc_rf[auc_df3$location == "Freiburg (SW-Germany)"], 
         auc_df3$auc_bvd[auc_df3$location == "Freiburg (SW-Germany)"], method = "s")

# Plot, coloured by location and year-month (for FR):
auc_df3 %>%
  ggplot() + 
  geom_abline(intercept = 0, slope = 1, linetype = 2, col = "grey60") +
  geom_point(aes(auc_rf, auc_bvd, colour = factor(location, levels=c("Freiburg (SW-Germany)", "Schauinsland (SW-Germany)", "Wildes Moor (N-Germany)", "Mellum (N-Germany)", "Spain", "Romania"))), size = 3) +
  geom_point(aes(auc_rf, auc_bvd, shape = format(as.Date(nights), "%Y-%m")), size = 3,
             fill = "#fde725ff",
             data = auc_df3[which(auc_df3$location == "Freiburg (SW-Germany)"),]) + 
  geom_text(aes(auc_rf, auc_bvd, label = group), color = "grey20", vjust = -0.75, size = 3.5) + 
  coord_equal() +
  theme_bw() +
  labs(col = "Location", shape = "Year and month of recording") +
  xlab("AUC Random Forest") +
  ylab("AUC BirdVoxDetect") +
  xlim(c(0.5,1))+
  ylim(c(0.5,1))+
  theme(axis.text.x = element_text(size = 15,colour = "black"),
        axis.text.y = element_text(size = 15, colour = "black"),
        legend.position = "right",
        text = element_text(size = 15),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_colour_viridis(discrete = T, begin = 0, end = 1, direction = -1) +
  scale_shape_manual(values = c(21:25, 10))



# PR curves: -------------------------------------------------------------------------

# RF:

pr_dat_rf1 <- dt_preds2 %>%
  group_by(group) %>% 
  pr_curve(bird_event, rf_preds_1)

# BVD: 

pr_dat_bvd <- dt_preds2 %>%
  group_by(group) %>% 
  pr_curve(bird_event, max_conf_BVD)

# AUC - PR: not calculated because not comparable for different underlying datasets

# Subplot for each test group with RF and BVD PR curve (Fig. E1): 

# baseline: prevalence bird events:
prev_pos_group <- dt_preds2 %>% 
  group_by(group) %>% 
  summarise(n_neg = length(which(bird_event == 0)),
            n_pos = length(which(bird_event == 1)),
            prev_pos = n_pos/(n_neg+n_pos))

pr_dat_rf1 <- merge(pr_dat_rf1, prev_pos_group, by = "group")

# new facet label names. 
group.labs <- paste0(c(1:3, 6, 8, 4, 5, 7, 9:21), ": ", c("Spain", "Wildes Moor", 
                                                          "Schauinsland", "Romania",
                                                          "Mellum", rep("Freiburg", 16)))
names(group.labs) <- as.character(c(1:3, 6, 8, 4, 5, 7, 9:21))

# group order:
pr_dat_rf1$group <- factor(pr_dat_rf1$group, levels= c(1:3, 6, 8, 4, 5, 7, 9:21))
pr_dat_bvd$group <- factor(pr_dat_bvd$group, levels= c(1:3, 6, 8, 4, 5, 7, 9:21))

# colour dummy:
pr_dat_rf1$col <- "col"
pr_dat_bvd$col <- "col_bvd"

pr_dat_rf1 %>%
  arrange(.threshold) %>%
  ggplot() +
  geom_path(aes(recall, precision, group = as.factor(group), colour = col), 
            data = pr_dat_bvd, 
            size = 0.5) +
  geom_path(aes(recall, precision, group = as.factor(group), colour = col), 
            size = 0.5) +
  geom_hline(aes(yintercept = prev_pos), linetype = "dotted", show.legend = F, 
             colour = "grey60") +
  labs(col = "") +
  xlab("Recall") +
  ylab("Precision") +
  facet_wrap(vars(group), 
             labeller = labeller(group = group.labs)) +
  scale_color_manual(values = c("#d7191c", "#2c7bb6"), 
                     labels=c("Random Forest", "BirdVoxDetect")) +
  coord_equal() +
  theme_bw() +
  theme(text = element_text(size = 15), 
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.background = element_rect(fill = c("grey80"),
                                        colour = "black"),
        strip.text = element_text(size = 12),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.text.x = element_text(size = 11, colour = "black"),
        axis.text.y = element_text(size = 11, colour = "black"),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text()) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = c("0", "0.25", "0.5", "0.75", "1"))




# Sensitivity with regard to species: ------------------------------------------------


# Sensitivity with regard to species groups: ----

## (specificity and precision not meaningful when looking at single species while 
## prediction aim is species-agnostic)

# boxplots based on cross-validation: determine sensitivity for each species group 
# within each test group. -> 21 sensitivity values

# calculation:
## with regard to single species: true positives = detected NFCs of this species, 
## true negatives = correctly predicted sound clip without NFCs, 
## false negatives = missed NFCs of respective species, 
## no false positives since detection is species agnostic
## exclude all NFCs of other than respectove focal species


# summarise cases with certain and probable species identification for plotting, 
# ignore cases where wither species could not be identified or only a rough category could be determined:

species_sum <- c("Alpenstrandläufer", "Amsel", "Austernfischer", "Baumpieper", "Bekassine",
                 "Bläßhuhn", "Bruchwasserläufer", "Feldlerche", "Feldsperling",
                 "Flussregenpfeifer", "Flussuferläufer", "Graugans", "Graureiher",
                 "Grauschnäpper", "Großer Brachvogel", "Grünschenkel",
                 "Haubentaucher", "Hausrotschwanz", "Haussperling", "Kernbeißer", 
                 "Kiebitz", "Kiebitzregenpfeifer", "Krickente", "Lachmöwe", "Mauersegler",
                 "Nachtreiher", "Neuntöter", "Ortolan", "Purpurreiher", "Rohrammer",
                 "Rohrdommel", "Rosaflamingo", "Rotdrossel", "Rotkehlchen", "Schafstelze",
                 "Singdrossel", "Stockente", "Teichhuhn", "Trauerschnäpper", "Triel", "Tüpfelsumpfhuhn",
                 "Turmfalke", "Waldwasserläufer", "Wasserralle", "Wiesenpieper", "Zwergdommel")


# Species occurrences across groups, English names, NFC count, species category:

spec_group_occ <- data.frame("species" = species_sum, "n_groups" = NA)

for(i in 1:nrow(spec_group_occ)){
  
  # rows containing target species:
  species_rows <- dt_preds2[grepl(pattern = as.character(spec_group_occ$species[i]), 
                                  x = dt_preds2$species),]
  spec_group_occ$n_groups[i] <- length(unique(species_rows$group))
  
}
# correction for Kiebitz and Kiebitzregenpfeifer:
spec_group_occ$n_groups[21] <- spec_group_occ$n_groups[21] - 1

# add English names:
spec_group_occ <- merge(spec_group_occ, 
                        species_names_df[,1:2], 
                        by.x = "species", 
                        by.y = "German", 
                        all.x = T)
spec_group_occ$English[which(is.na(spec_group_occ$English))] <- c("Eurasian tree sparrow", 
                                                                  "House sparrow", 
                                                                  "Hawfinch", 
                                                                  "Eurasian teal", 
                                                                  "Spotted crake")

# add species category:
spec_group_occ$category <- c("Wader", "Passerine", "Wader", "Passerine", "Wader", 
                             "Rail", "Wader", "Passerine", "Passerine", "Wader", 
                             "Wader", "Duck/Goose", "Heron", "Passerine", "Wader", 
                             "Wader", "Other", "Passerine", "Passerine", "Passerine", 
                             "Wader", "Wader", "Duck/Goose", "Wader", "Other", 
                             "Heron", "Passerine", "Passerine", "Heron", "Passerine", 
                             "Heron", "Other", "Passerine", "Passerine", "Passerine", 
                             "Passerine", "Duck/Goose", "Rail", "Passerine", "Wader", 
                             "Rail", "Other", "Wader", "Rail", "Passerine", "Heron")

spec_group_occ$category <- factor(spec_group_occ$category, 
                                  levels = c("Passerine", "Wader", "Rail", "Duck/Goose", 
                                             "Heron", "Other"))



# which sound clips contain species of which group:
## attention: sometimes there are NFCs of more than one species group in the same sound clip

# group species into categories:
passerine_specs <- spec_group_occ$species[which(spec_group_occ$category == "Passerine")]
wader_specs <- spec_group_occ$species[which(spec_group_occ$category == "Wader")]
rail_specs <- spec_group_occ$species[which(spec_group_occ$category == "Rail")]
duck_specs <- spec_group_occ$species[which(spec_group_occ$category == "Duck/Goose")]
heron_specs <- spec_group_occ$species[which(spec_group_occ$category == "Heron")]
other_specs <- spec_group_occ$species[which(spec_group_occ$category == "Other")]


# sensitivity for RF and BVD, respectively, per test group and species category:

spec_group_recall_CV <- data.frame("group" = unique(dt_preds2$group),
                                   "Passerines_RF" = NA,
                                   "Waders_RF" = NA,
                                   "Rails_RF" = NA,
                                   "Ducks_RF" = NA,
                                   "Herons_RF" = NA,
                                   "Other_RF" = NA,
                                   "Passerines_BVD" = NA,
                                   "Waders_BVD" = NA,
                                   "Rails_BVD" = NA,
                                   "Ducks_BVD" = NA,
                                   "Herons_BVD" = NA,
                                   "Other_BVD" = NA)

for(g in 1:21){
  
  print(g)
  
  # indexes of species occurrence on group level:
  
  ## passerines:
  passerines_ind <- vector(mode = "list", length = length(passerine_specs))
  for(p in 1:length(passerines_ind)){
    passerines_ind[[p]] <- which(grepl(pattern = passerine_specs[p], 
                                       x = dt_preds2$species[which(dt_preds2$group == g)]))
  }
  passerines_ind_all <- unlist(passerines_ind)[!duplicated(unlist(passerines_ind))]
  
  ## waders:
  waders_ind <- vector(mode = "list", length = length(wader_specs))
  for(p in 1:length(waders_ind)){
    waders_ind[[p]] <- which(grepl(pattern = wader_specs[p], 
                                   x = dt_preds2$species[which(dt_preds2$group == g)]))
  }
  waders_ind_all <- unlist(waders_ind)[!duplicated(unlist(waders_ind))]
  
  ## rails:
  rails_ind <- vector(mode = "list", length = length(rail_specs))
  for(p in 1:length(rails_ind)){
    rails_ind[[p]] <- which(grepl(pattern = rail_specs[p], 
                                  x = dt_preds2$species[which(dt_preds2$group == g)]))
  }
  rails_ind_all <- unlist(rails_ind)[!duplicated(unlist(rails_ind))]
  
  ## ducks:
  ducks_ind <- vector(mode = "list", length = length(duck_specs))
  for(p in 1:length(ducks_ind)){
    ducks_ind[[p]] <- which(grepl(pattern = duck_specs[p], 
                                  x = dt_preds2$species[which(dt_preds2$group == g)]))
  }
  ducks_ind_all <- unlist(ducks_ind)[!duplicated(unlist(ducks_ind))]
  
  ## herons:
  herons_ind <- vector(mode = "list", length = length(heron_specs))
  for(p in 1:length(herons_ind)){
    herons_ind[[p]] <- which(grepl(pattern = heron_specs[p], 
                                   x = dt_preds2$species[which(dt_preds2$group == g)]))
  }
  herons_ind_all <- unlist(herons_ind)[!duplicated(unlist(herons_ind))]
  
  ## other:
  other_ind <- vector(mode = "list", length = length(other_specs))
  for(p in 1:length(other_ind)){
    other_ind[[p]] <- which(grepl(pattern = other_specs[p], 
                                  x = dt_preds2$species[which(dt_preds2$group == g)]))
  }
  others_ind_all <- unlist(other_ind)[!duplicated(unlist(other_ind))]
  
  
  # Sensitivity for each species group:
  
  ## which sound clips of respective test group contain NFC of certain species group (=1):
  
  spec_group_events <- data.frame(matrix(0, 
                                         nrow = nrow(dt_preds2[dt_preds2$group == g,]), 
                                         ncol = length(unique(spec_group_occ$category))))
  colnames(spec_group_events) <- unique(spec_group_occ$category)
  
  spec_group_events$Passerine[passerines_ind_all] <- 1
  spec_group_events$Wader[waders_ind_all] <- 1
  spec_group_events$Rail[rails_ind_all] <- 1
  spec_group_events$`Duck/Goose`[ducks_ind_all] <- 1
  spec_group_events$Heron[herons_ind_all] <- 1
  spec_group_events$Other[others_ind_all] <- 1
  
  
  ## sensitivity:
  
  ### RF:
  
  recall_passerines_rf <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,], 
                                            truth = factor(spec_group_events$Passerine, 
                                                           levels = c("1", "0")),
                                            estimate = RF_pred_thresh_prev_train)[3])
  
  recall_waders_rf <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,], 
                                        truth = factor(spec_group_events$Wader, 
                                                       levels = c("1", "0")), 
                                        estimate = RF_pred_thresh_prev_train)[3])
  
  recall_rails_rf <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,], 
                                       truth = factor(spec_group_events$Rail, 
                                                      levels = c("1", "0")), 
                                       estimate = RF_pred_thresh_prev_train)[3])
  
  recall_ducks_rf <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,], 
                                       truth = factor(spec_group_events$`Duck/Goose`, 
                                                      levels = c("1", "0")), 
                                       estimate = RF_pred_thresh_prev_train)[3])
  
  recall_herons_rf <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,], 
                                        truth = factor(spec_group_events$Heron, 
                                                       levels = c("1", "0")), 
                                        estimate = RF_pred_thresh_prev_train)[3])
  
  recall_others_rf <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,], 
                                        truth = factor(spec_group_events$Other, 
                                                       levels = c("1", "0")), 
                                        estimate = RF_pred_thresh_prev_train)[3])
  
  spec_group_recall_CV[g, 2:7] <- c(recall_passerines_rf, 
                                    recall_waders_rf, 
                                    recall_rails_rf, 
                                    recall_ducks_rf, 
                                    recall_herons_rf, 
                                    recall_others_rf)
  
  ### BVD:
  
  recall_passerines_bvd <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,], 
                                             truth = factor(spec_group_events$Passerine, 
                                                            levels = c("1", "0")), 
                                             estimate = BVD_pred_thresh_all_dt)[3])
  
  recall_waders_bvd <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,], 
                                         truth = factor(spec_group_events$Wader, 
                                                        levels = c("1", "0")), 
                                         estimate = BVD_pred_thresh_all_dt)[3])
  
  recall_rails_bvd <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,], 
                                        truth = factor(spec_group_events$Rail, 
                                                       levels = c("1", "0")), 
                                        estimate = BVD_pred_thresh_all_dt)[3])
  
  recall_ducks_bvd <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,], 
                                        truth = factor(spec_group_events$`Duck/Goose`, 
                                                       levels = c("1", "0")), 
                                        estimate = BVD_pred_thresh_all_dt)[3])
  
  recall_herons_bvd <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,], 
                                         truth = factor(spec_group_events$Heron, 
                                                        levels = c("1", "0")), 
                                         estimate = BVD_pred_thresh_all_dt)[3])
  
  recall_others_bvd <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,], 
                                         truth = factor(spec_group_events$Other, 
                                                        levels = c("1", "0")), 
                                         estimate = BVD_pred_thresh_all_dt)[3])
  
  spec_group_recall_CV[g, 8:13] <- c(recall_passerines_bvd, 
                                     recall_waders_bvd, 
                                     recall_rails_bvd, 
                                     recall_ducks_bvd, 
                                     recall_herons_bvd, 
                                     recall_others_bvd)
}



# sensitivity for RF and BVD, respectively, per entire dataset and species category:

spec_group_recall_entire_dt <- data.frame("Passerines_RF" = NA,
                                          "Waders_RF" = NA,
                                          "Rails_RF" = NA,
                                          "Ducks_RF" = NA,
                                          "Herons_RF" = NA,
                                          "Other_RF" = NA,
                                          "Passerines_BVD" = NA,
                                          "Waders_BVD" = NA,
                                          "Rails_BVD" = NA,
                                          "Ducks_BVD" = NA,
                                          "Herons_BVD" = NA,
                                          "Other_BVD" = NA)

# indices of species occurrence:

## passerines:
passerines_ind <- vector(mode = "list", length = length(passerine_specs))
for(p in 1:length(passerines_ind)){
  passerines_ind[[p]] <- which(grepl(pattern = passerine_specs[p], x = dt_preds2$species))
}
passerines_ind_all <- unlist(passerines_ind)[!duplicated(unlist(passerines_ind))]

## waders:
waders_ind <- vector(mode = "list", length = length(wader_specs))
for(p in 1:length(waders_ind)){
  waders_ind[[p]] <- which(grepl(pattern = wader_specs[p], x = dt_preds2$species))
}
waders_ind_all <- unlist(waders_ind)[!duplicated(unlist(waders_ind))]

## rails:
rails_ind <- vector(mode = "list", length = length(rail_specs))
for(p in 1:length(rails_ind)){
  rails_ind[[p]] <- which(grepl(pattern = rail_specs[p], x = dt_preds2$species))
}
rails_ind_all <- unlist(rails_ind)[!duplicated(unlist(rails_ind))]

## ducks:
ducks_ind <- vector(mode = "list", length = length(duck_specs))
for(p in 1:length(ducks_ind)){
  ducks_ind[[p]] <- which(grepl(pattern = duck_specs[p], x = dt_preds2$species))
}
ducks_ind_all <- unlist(ducks_ind)[!duplicated(unlist(ducks_ind))]

## herons:
herons_ind <- vector(mode = "list", length = length(heron_specs))
for(p in 1:length(herons_ind)){
  herons_ind[[p]] <- which(grepl(pattern = heron_specs[p], x = dt_preds2$species))
}
herons_ind_all <- unlist(herons_ind)[!duplicated(unlist(herons_ind))]

## other:
other_ind <- vector(mode = "list", length = length(other_specs))
for(p in 1:length(other_ind)){
  other_ind[[p]] <- which(grepl(pattern = other_specs[p], x = dt_preds2$species))
}
others_ind_all <- unlist(other_ind)[!duplicated(unlist(other_ind))]


# Sensitivity for each species group:

## which sound clips contain NFC of certain species group (=1):

spec_group_events <- data.frame(matrix(0, 
                                       nrow = nrow(dt_preds2), 
                                       ncol = length(unique(spec_group_occ$category))))
colnames(spec_group_events) <- unique(spec_group_occ$category)

spec_group_events$Passerine[passerines_ind_all] <- 1
spec_group_events$Wader[waders_ind_all] <- 1
spec_group_events$Rail[rails_ind_all] <- 1
spec_group_events$`Duck/Goose`[ducks_ind_all] <- 1
spec_group_events$Heron[herons_ind_all] <- 1
spec_group_events$Other[others_ind_all] <- 1

## sensitivity:

### RF:

recall_passerines_rf <- as.numeric(recall(data = dt_preds2, 
                                          truth = factor(spec_group_events$Passerine, 
                                                         levels = c("1", "0")), 
                                          estimate = RF_pred_thresh_prev_train)[3])

recall_waders_rf <- as.numeric(recall(data = dt_preds2, 
                                      truth = factor(spec_group_events$Wader, 
                                                     levels = c("1", "0")), 
                                      estimate = RF_pred_thresh_prev_train)[3])

recall_rails_rf <- as.numeric(recall(data = dt_preds2, 
                                     truth = factor(spec_group_events$Rail, 
                                                    levels = c("1", "0")), 
                                     estimate = RF_pred_thresh_prev_train)[3])

recall_ducks_rf <- as.numeric(recall(data = dt_preds2, 
                                     truth = factor(spec_group_events$`Duck/Goose`, 
                                                    levels = c("1", "0")), 
                                     estimate = RF_pred_thresh_prev_train)[3])

recall_herons_rf <- as.numeric(recall(data = dt_preds2, 
                                      truth = factor(spec_group_events$Heron, 
                                                     levels = c("1", "0")), 
                                      estimate = RF_pred_thresh_prev_train)[3])

recall_others_rf <- as.numeric(recall(data = dt_preds2, 
                                      truth = factor(spec_group_events$Other, 
                                                     levels = c("1", "0")), 
                                      estimate = RF_pred_thresh_prev_train)[3])

spec_group_recall_entire_dt[1, 1:6] <- c(recall_passerines_rf, 
                                         recall_waders_rf, 
                                         recall_rails_rf, 
                                         recall_ducks_rf, 
                                         recall_herons_rf, 
                                         recall_others_rf)

### BVD:

recall_passerines_bvd <- as.numeric(recall(data = dt_preds2, 
                                           truth = factor(spec_group_events$Passerine, 
                                                          levels = c("1", "0")), 
                                           estimate = BVD_pred_thresh_all_dt)[3])

recall_waders_bvd <- as.numeric(recall(data = dt_preds2, 
                                       truth = factor(spec_group_events$Wader, 
                                                      levels = c("1", "0")), 
                                       estimate = BVD_pred_thresh_all_dt)[3])

recall_rails_bvd <- as.numeric(recall(data = dt_preds2, 
                                      truth = factor(spec_group_events$Rail, 
                                                     levels = c("1", "0")), 
                                      estimate = BVD_pred_thresh_all_dt)[3])

recall_ducks_bvd <- as.numeric(recall(data = dt_preds2, 
                                      truth = factor(spec_group_events$`Duck/Goose`, 
                                                     levels = c("1", "0")), 
                                      estimate = BVD_pred_thresh_all_dt)[3])

recall_herons_bvd <- as.numeric(recall(data = dt_preds2, 
                                       truth = factor(spec_group_events$Heron, 
                                                      levels = c("1", "0")), 
                                       estimate = BVD_pred_thresh_all_dt)[3])

recall_others_bvd <- as.numeric(recall(data = dt_preds2, 
                                       truth = factor(spec_group_events$Other, 
                                                      levels = c("1", "0")), 
                                       estimate = BVD_pred_thresh_all_dt)[3])

spec_group_recall_entire_dt[1, 7:12] <- c(recall_passerines_bvd, 
                                          recall_waders_bvd, 
                                          recall_rails_bvd, 
                                          recall_ducks_bvd, 
                                          recall_herons_bvd, 
                                          recall_others_bvd)


# change format:
spec_group_recall_entire_dt_long <- data.frame("Passerines" = as.numeric(spec_group_recall_entire_dt[1, c(1,7)]),
                                               "Waders" = as.numeric(spec_group_recall_entire_dt[1, c(2,8)]),
                                               "Rails" = as.numeric(spec_group_recall_entire_dt[1, c(3,9)]),
                                               "Ducks" = as.numeric(spec_group_recall_entire_dt[1, c(4,10)]),
                                               "Herons" = as.numeric(spec_group_recall_entire_dt[1, c(5,11)]),
                                               "Other" = as.numeric(spec_group_recall_entire_dt[1, c(6,12)]),
                                               "model" = factor(c("RF", "BVD"),levels = c("RF", "BVD")))

# Plot: sensitivity per species group (Fig. F1):

# change to long format:

spec_group_recall_CV_long <- gather(spec_group_recall_CV,
                                    key = "specgroup_model", 
                                    value = "sensitivity", 
                                    -group,
                                    factor_key = T)

spec_group_recall_entire_dt_long2 <- gather(spec_group_recall_entire_dt_long,
                                            key = "species_group",
                                            value = "sensitivity",
                                            -model)

spec_group_recall_CV_long$model <- c(rep("RF", 126), rep("BVD", 126))
spec_group_recall_CV_long$model <- factor(spec_group_recall_CV_long$model, 
                                          levels = c("RF", "BVD"))
spec_group_recall_CV_long$species_group <- sub("\\_.*", "", spec_group_recall_CV_long$specgroup_model)
spec_group_recall_CV_long$species_group <- factor(spec_group_recall_CV_long$species_group, 
                                                  levels = c("Passerines", "Waders", "Rails", 
                                                             "Ducks", "Herons", "Other"))


spec_group_recall_CV_long %>% 
  filter(species_group != "Other") %>% 
  ggplot() +
  stat_boxplot(aes(x = species_group, y = sensitivity, fill = model), 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.6)) +
  geom_boxplot(aes(x = species_group, y = sensitivity, fill = model), width = 0.6) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, colour = "black"), 
        axis.text.y = element_text(size = 15, colour = "black"),
        legend.position = "top",
        text = element_text(size = 15),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  ylab("Sensitivity") + 
  xlab("Species group") +
  labs(colour = "", fill = "") +
  scale_fill_manual(values = c("#d7191c", "#2c7bb6"), label = c("Random Forest", "BirdVoxDetect")) +
  scale_x_discrete(labels = c("Passerines", "Waders", "Rails", "Ducks / Geese", "Herons")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1), add = 0)) +
  geom_label(x = 1.03, y = 1.05, label = "N = 19", show.legend = F, fontface = 1, 
             label.size = 0, size = 5, fill = "white") +
  geom_label(x = 2.03, y = 1.05, label = "N = 11", show.legend = F, fontface = 1, 
             label.size = 0, size = 5, fill = "white") +
  geom_label(x = 3.03, y = 1.05, label = "N = 12", show.legend = F, fontface = 1, 
             label.size = 0, size = 5, fill = "white") +
  geom_label(x = 4, y = 1.05, label = "N = 8", show.legend = F, fontface = 1, 
             label.size = 0, size = 5, fill = "white") +
  geom_label(x = 5.03, y = 1.05, label = "N = 13", show.legend = F, fontface = 1, 
             label.size = 0, size = 5,fill = "white") +
  geom_text(data = spec_group_recall_entire_dt_long2[1:10,], 
            aes(y = sensitivity, x = species_group, group = model), 
label="*", size = 5, position = position_dodge(width = 0.6)) 



# Sensitivity with regard to single species: ----

## which sound clips contain NFC of certain species (=1):
species_events <- data.frame(matrix(0, 
                                    nrow = nrow(dt_preds2), 
                                    ncol = length(species_sum)))
colnames(species_events) <- species_sum

for(s in 1:length(species_sum)){
  
  print(s)
  
  # set all rows to 1 where clip contains NFC of respective species:
  species_events[grepl(pattern = species_sum[s], x = dt_preds2$species), s] <- 1
  
}

# correction for Schnäpper:
# delete "Grauschnäpper/Trauerschnäpper" from "Grauschnäpper" and "Trauerschnäpper"
species_events[which(dt_preds2$species == "Grauschnäpper/Trauerschnäpper"), 14] <- 0
species_events[which(dt_preds2$species == "Grauschnäpper/Trauerschnäpper"), 39] <- 0
# correction for Kiebitz and Kiebitzregenpfeifer:
species_events[grepl(pattern = "Kiebitzregenpfeifer", x = dt_preds2$species), 21] <- 0


# add for each species the total number of sound clips with associated NFCs:
n_soundclips <- data.frame("species" = names(colSums(species_events)), 
                           "n_soundclips" = colSums(species_events))
spec_group_occ <- merge(spec_group_occ, n_soundclips)
spec_group_occ$English_n_soundclips <- paste0(spec_group_occ$English, " (",spec_group_occ$n_soundclips,")")

save(spec_group_occ, file = "~/Master/Masterarbeit/Code/Code_final/species_occurrences.RData")

# Sensitivity:
# for species-specific figure show only species with > 10 NFCs 

spec_recall_CV <- data.frame(matrix(0, 
                                    nrow = 2*length(unique(dt_preds2$group)), 
                                    ncol = 2 + length(which(colSums(species_events) > 10)))) # consider only species with > 10 NFCs
colnames(spec_recall_CV) <- c("group", 
                              "model", 
                              names(which(colSums(species_events) > 10)))
spec_recall_CV$group <- rep(1:21, 2)
spec_recall_CV$model <- c(rep("RF", 21), rep("BVD", 21))


# species events only for species considered:
species_events_min11 <- species_events[, which(names(species_events) %in% names(which(colSums(species_events) > 10)))]

# add group:
species_events_min11$group <- dt_preds2$group

# sensitivity for RF and BVD, respectively, per test group and species:

for(g in 1:21){
  
  print(g)
  
  for(s in 1:(ncol(species_events_min11)-1)){
    
    print(s)
    
    # for RF:
    spec_recall_CV[g,(s+2)] <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,],
                                                 truth = factor(species_events_min11[which(species_events_min11$group == g),s], levels = c("1", "0")),
                                                 estimate = RF_pred_thresh_prev_train)[3])
    
    # for BVD:
    spec_recall_CV[(g+21),(s+2)] <- as.numeric(recall(data = dt_preds2[dt_preds2$group == g,],
                                                      truth = factor(species_events_min11[which(species_events_min11$group == g),s], levels = c("1", "0")),
                                                      estimate = BVD_pred_thresh_all_dt)[3])
  }
}


# Plot (Fig. 3.4):

# change to long format:
spec_recall_CV_long <- gather(data = spec_recall_CV, 
                              key = "species", 
                              value = "sensitivity", 
                              -group, 
                              -model)

spec_recall_CV_long$model <- factor(spec_recall_CV_long$model, 
                                    levels = c("RF", "BVD"))


# add English species names and number of NFCs: 
spec_recall_CV_long2 <- merge(spec_recall_CV_long, spec_group_occ, by = "species", sort = F)

# facet grid labels:
cat.labs <- c("Passerines", "Waders", "Rails", "Ducks/G.", "Herons", "Other")
names(cat.labs) <- c("Passerine", "Wader", "Rail", "Duck/Goose", "Heron", "Other")


spec_recall_CV_long2 %>% 
  #filter(category != "Other") %>% 
  ggplot(mapping = aes(x = English_n_soundclips)) +
  facet_grid(cols = vars(category), 
             scales = "free", 
             space = "free_x", 
             labeller = labeller(category = cat.labs))+
  stat_boxplot(aes(y = sensitivity, fill = model), 
               geom = "errorbar", 
               width = 0.2, 
               position = position_dodge(width = 0.8)) +
  geom_boxplot(aes(y = sensitivity, fill = model), 
               width = 0.8) + 
  ylab("Sensitivity") + 
  xlab("Species") +
  theme_bw() + 
  theme(legend.position = "top",
        text = element_text(size = 15),
        legend.key.size = unit(1.7, 'lines'),
        axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1, size = 15, colour = "black"), 
        axis.text.y = element_text(size = 15, colour = "black"),
        axis.title.y = element_text(vjust = 3, size = 15),
        axis.title.x = element_text(vjust = -2, size = 15),
        plot.margin = unit(c(1,1,1,1), "cm"),
        strip.background = element_rect(fill = c("grey80"), colour = "black"),
        strip.text = element_text(size = 15)) +
  labs(fill = "") +
  scale_fill_manual(values = c("#d7191c", "#2c7bb6"), 
                    label = c("Random Forest", "BirdVoxDetect")) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), 
                     labels = c("0.00", "0.25", "0.50", "0.75", "1.00"), 
                     expand = expansion(mult = c(0.05, 0.06))) +
  geom_label(aes(x, y, label=lab),
             data = data.frame(x = spec_recall_CV_long2$English_n_soundclips,
                               y = 1.09,
                               lab = paste0(spec_recall_CV_long2$n_groups),
                               category = spec_recall_CV_long2$category),
             size = 5,
             show.legend = F,
             fontface = 1,
             label.size = 0,
             vjust = 0.8)




# AUC scatterplot RF & BVD regarding species composition / N song thrush NFCs: -------

# percentage of song thrush NFCs of all NFCs per test group:

df_n_song_thrush_per_group <- data.frame("group" = 1:21, 
                                         n_song_thrush_per_group = NA, 
                                         perc_song_thrush_per_group = NA)

for(g in 1:length(groups)){
  
  specs_per_group <- dt_preds2$species[which(dt_preds2$group == g & dt_preds2$bird_event == 1)]
  
  df_n_song_thrush_per_group$n_song_thrush_per_group[g] <- length(grep(pattern = "Singdrossel", 
                                                                       x = specs_per_group, 
                                                                       fixed = F))
  df_n_song_thrush_per_group$perc_song_thrush_per_group[g] <- df_n_song_thrush_per_group$n_song_thrush_per_group[g]/length(specs_per_group)
  
}


# Plot (Fig. 3.5):
auc_df3 <- merge(auc_df3, df_n_song_thrush_per_group, by = "group")

auc_df3 %>%
  ggplot(aes(colour = perc_song_thrush_per_group)) + 
  geom_abline(intercept = 0, slope = 1, linetype = 2, col = "grey60") +
  geom_point(aes(auc_rf, auc_bvd, size = n_specs)) + 
  geom_text(aes(auc_rf, auc_bvd, label = group), color = "grey20", vjust = -1, size = 3.5) + 
  coord_equal() +
  theme_bw() +
  labs(size = "Number of species", col = "Song thrush NFCs [% of total NFCs]") +
  xlab("AUC Random Forest") +
  ylab("AUC BirdVoxDetect") +
  xlim(c(0.5,1))+
  ylim(c(0.5,1))+
  scale_color_viridis(discrete=F, begin = 0, end = 1, direction = -1) +
  theme(axis.text.x = element_text(size = 13,colour = "black"),
        axis.text.y = element_text(size = 13, colour = "black"),
        legend.position = "right",
        text = element_text(size = 13),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(size = 13),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_size_binned() +
  guides(size = guide_bins(order = 1), colourbar = guide_legend(order = 2))



# Reduction in amount of data to inspect manually for certain sensitivity: -----------

# threshold for each test group corresponding to a sensitivity value of 50%, 75%, 90% and 95%:

## RF:

sens_threshold_group_df <- roc_dat_rf1 %>% 
  group_by(group) %>% 
  summarise(RF_thresh_50 = max(.threshold[sensitivity > 0.5]),
            RF_thresh_75 = max(.threshold[sensitivity > 0.75]),
            RF_thresh_90 = max(.threshold[sensitivity > 0.90]),
            RF_thresh_95 = max(.threshold[sensitivity > 0.95]))


# reduction in what has to be manually inspected per test group
#(= percentage which does not has to be inspected):

sens_threshold_group_df$redu_sens_50 <- NA
sens_threshold_group_df$redu_sens_75 <- NA
sens_threshold_group_df$redu_sens_90 <- NA
sens_threshold_group_df$redu_sens_95 <- NA

for(g in 1:length(groups)){
  
  n_all_clips <- length(which(dt_preds2$group == g))
  
  # percentage of positive predictions per test group:
  n_pos_pred_50 <- length(which(dt_preds2$group == g & 
                                  dt_preds2$rf_preds_1 >= sens_threshold_group_df$RF_thresh_50[sens_threshold_group_df$group == g]))
  # reduction:
  sens_threshold_group_df$redu_sens_50[sens_threshold_group_df$group == g] <- (n_all_clips - n_pos_pred_50)/n_all_clips 
  
  n_pos_pred_75 <- length(which(dt_preds2$group == g & 
                                  dt_preds2$rf_preds_1 >= sens_threshold_group_df$RF_thresh_75[sens_threshold_group_df$group == g]))
  
  sens_threshold_group_df$redu_sens_75[sens_threshold_group_df$group == g] <- (n_all_clips - n_pos_pred_75)/n_all_clips 
  
  n_pos_pred_90 <- length(which(dt_preds2$group == g & 
                                  dt_preds2$rf_preds_1 >= sens_threshold_group_df$RF_thresh_90[sens_threshold_group_df$group == g]))
  
  sens_threshold_group_df$redu_sens_90[sens_threshold_group_df$group == g] <- (n_all_clips - n_pos_pred_90)/n_all_clips
  
  n_pos_pred_95 <- length(which(dt_preds2$group == g & 
                                  dt_preds2$rf_preds_1 >= sens_threshold_group_df$RF_thresh_95[sens_threshold_group_df$group == g]))
  
  sens_threshold_group_df$redu_sens_95[sens_threshold_group_df$group == g] <- (n_all_clips - n_pos_pred_95)/n_all_clips
  
}


# reduction for entire dataset corresponding to a sensitivity value of 50%, 75%, 90% and 95%:

thresh_50 <- max(roc_dat_rf1$.threshold[roc_dat_rf1$sensitivity > 0.5])
thresh_75 <- max(roc_dat_rf1$.threshold[roc_dat_rf1$sensitivity > 0.75])
thresh_90 <- max(roc_dat_rf1$.threshold[roc_dat_rf1$sensitivity > 0.90])
thresh_95 <- max(roc_dat_rf1$.threshold[roc_dat_rf1$sensitivity > 0.95])

n_all_clips <- nrow(dt_preds2)

n_pos_pred_50 <- length(which(dt_preds2$rf_preds_1 >= thresh_50))
sens_threshold_group_df$red_all_dt_50 <- (n_all_clips - n_pos_pred_50)/n_all_clips

n_pos_pred_75 <- length(which(dt_preds2$rf_preds_1 >= thresh_75))
sens_threshold_group_df$red_all_dt_75 <- (n_all_clips - n_pos_pred_75)/n_all_clips

n_pos_pred_90 <- length(which(dt_preds2$rf_preds_1 >= thresh_90))
sens_threshold_group_df$red_all_dt_90 <- (n_all_clips - n_pos_pred_90)/n_all_clips

n_pos_pred_95 <- length(which(dt_preds2$rf_preds_1 >= thresh_95))
sens_threshold_group_df$red_all_dt_95 <- (n_all_clips - n_pos_pred_95)/n_all_clips


## BVD:

sens_threshold_group_df_BVD <- roc_dat_BVD %>% 
  group_by(group) %>% 
  summarise(BVD_thresh_50 = max(.threshold[sensitivity > 0.5]),
            BVD_thresh_75 = max(.threshold[sensitivity > 0.75]),
            BVD_thresh_90 = max(.threshold[sensitivity > 0.90]),
            BVD_thresh_95 = max(.threshold[sensitivity > 0.95]))

# reduction in what has to be manually inspected per test group
#(= percentage which does not has to be inspected):

sens_threshold_group_df_BVD$redu_sens_50 <- NA
sens_threshold_group_df_BVD$redu_sens_75 <- NA
sens_threshold_group_df_BVD$redu_sens_90 <- NA
sens_threshold_group_df_BVD$redu_sens_95 <- NA

for(g in 1:length(groups)){
  
  n_all_clips <- length(which(dt_preds2$group == g))
  
  n_pos_pred_50 <- length(which(dt_preds2$group == g &
                                  dt_preds2$max_conf_BVD >= sens_threshold_group_df_BVD$BVD_thresh_50[sens_threshold_group_df_BVD$group == g]))
  
  sens_threshold_group_df_BVD$redu_sens_50[sens_threshold_group_df_BVD$group == g] <- (n_all_clips - n_pos_pred_50)/n_all_clips 
  
  n_pos_pred_75 <- length(which(dt_preds2$group == g &
                                  dt_preds2$max_conf_BVD >= sens_threshold_group_df_BVD$BVD_thresh_75[sens_threshold_group_df_BVD$group == g]))
  
  sens_threshold_group_df_BVD$redu_sens_75[sens_threshold_group_df_BVD$group == g] <- (n_all_clips - n_pos_pred_75)/n_all_clips 
  
  n_pos_pred_90 <- length(which(dt_preds2$group == g &
                                  dt_preds2$max_conf_BVD >= sens_threshold_group_df_BVD$BVD_thresh_90[sens_threshold_group_df_BVD$group == g]))
  
  sens_threshold_group_df_BVD$redu_sens_90[sens_threshold_group_df_BVD$group == g] <- (n_all_clips - n_pos_pred_90)/n_all_clips
  
  n_pos_pred_95 <- length(which(dt_preds2$group == g &
                                  dt_preds2$max_conf_BVD >= sens_threshold_group_df_BVD$BVD_thresh_95[sens_threshold_group_df_BVD$group == g]))
  
  sens_threshold_group_df_BVD$redu_sens_95[sens_threshold_group_df_BVD$group == g] <- (n_all_clips - n_pos_pred_95)/n_all_clips
  
}


# reduction  for entire dataset corresponding to a sensitivity value of 50%, 75%, 90% and 95%:
n_all_clips <- nrow(dt_preds2)

thresh_50 <- max(roc_dat_BVD$.threshold[roc_dat_BVD$sensitivity > 0.5])
thresh_75 <- max(roc_dat_BVD$.threshold[roc_dat_BVD$sensitivity > 0.75])
thresh_90 <- max(roc_dat_BVD$.threshold[roc_dat_BVD$sensitivity > 0.90])
thresh_95 <- max(roc_dat_BVD$.threshold[roc_dat_BVD$sensitivity > 0.95])

n_pos_pred_50 <- length(which(dt_preds2$max_conf_BVD >= thresh_50))
sens_threshold_group_df_BVD$red_all_dt_50 <- (n_all_clips - n_pos_pred_50)/n_all_clips

n_pos_pred_75 <- length(which(dt_preds2$max_conf_BVD >= thresh_75))
sens_threshold_group_df_BVD$red_all_dt_75 <- (n_all_clips - n_pos_pred_75)/n_all_clips

n_pos_pred_90 <- length(which(dt_preds2$max_conf_BVD >= thresh_90))
sens_threshold_group_df_BVD$red_all_dt_90 <- (n_all_clips - n_pos_pred_90)/n_all_clips

n_pos_pred_95 <- length(which(dt_preds2$max_conf_BVD >= thresh_95))
sens_threshold_group_df_BVD$red_all_dt_95 <- (n_all_clips - n_pos_pred_95)/n_all_clips


# Plot:

# change to long format:

# RF:
sens_threshold_group_df_long <- sens_threshold_group_df %>% 
  gather(key = "sensitivity", 
         value = "reduction", 
         redu_sens_50, redu_sens_75, redu_sens_90, redu_sens_95,
         factor_key = F)
sens_threshold_group_df_long$model <- rep("RF", nrow(sens_threshold_group_df_long))

# BVD:
sens_threshold_group_df_BVD_long <- sens_threshold_group_df_BVD %>% 
  gather(key = "sensitivity", 
         value = "reduction", 
         redu_sens_50, redu_sens_75, redu_sens_90, redu_sens_95,
         factor_key = F)
sens_threshold_group_df_BVD_long$model <- rep("BVD", nrow(sens_threshold_group_df_BVD_long))

# combine RF and BVD dataset:
sens_threshold_group_df_long_RF_BVD <- rbind(sens_threshold_group_df_long[,c(1,6:12)], 
                                             sens_threshold_group_df_BVD_long[,c(1,6:12)])


sens_threshold_group_df_long_RF_BVD$model <- factor(sens_threshold_group_df_long_RF_BVD$model, 
                                                    levels = c("RF", "BVD"))

# Plot (Fig. 3.6):
ggplot(data = sens_threshold_group_df_long_RF_BVD) +
  stat_boxplot(aes(y = reduction*100, x = sensitivity, fill = model), 
               geom = "errorbar", 
               width = 0.2, 
               position = position_dodge(width = 0.6)) +
  geom_boxplot(aes(y = reduction*100, x = sensitivity, fill = model), 
               width = 0.6) +
  theme_bw() +
  theme(axis.text.x=element_text(size = 15, colour = "black"),
        axis.text.y = element_text(size = 15, colour = "black"),
        legend.position = "top",
        text = element_text(size = 15),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  xlab("Sensitivity") +
  ylab("Reduction [%]") +
  labs(fill = "") +
  geom_text(aes(y = red_all_dt_50*100, x = 1, group = model), 
            label="*", size = 5, position = position_dodge(width = 0.6)) +
  geom_text(aes(y = red_all_dt_75*100, x = 2, group = model), 
            label="*", size = 5, position = position_dodge(width = 0.6)) +
  geom_text(aes(y = red_all_dt_90*100, x = 3, group = model), 
            label="*", size = 5, position = position_dodge(width = 0.6)) +
  geom_text(aes(y = red_all_dt_95*100, x = 4, group = model), 
            label="*", size = 5, position = position_dodge(width = 0.6)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), breaks = seq(0, 100, 25)) + 
  scale_fill_manual(values = c("#d7191c", "#2c7bb6"),labels=c("Random Forest", "BirdVoxDetect")) +
  scale_x_discrete(labels = c("50%", "75%", "90%", "95%"), expand=c(0.2,0))



# extract values:

median(sens_threshold_group_df$redu_sens_50)
median(sens_threshold_group_df$redu_sens_75)
median(sens_threshold_group_df$redu_sens_90)
median(sens_threshold_group_df$redu_sens_95)
median(sens_threshold_group_df_BVD$redu_sens_50)
median(sens_threshold_group_df_BVD$redu_sens_75)
median(sens_threshold_group_df_BVD$redu_sens_90)
median(sens_threshold_group_df_BVD$redu_sens_95)


# Discussion: comparison to BVD performance reported by Lostanlen 2019: -------------- 

# RF:

# precision reached for 75 % and 85% recall:

recall_precision_group_df_RF <- pr_dat_rf1 %>% 
  group_by(group) %>% 
  summarise(RF_prec_75_recall = max(precision[recall > 0.75]),
            RF_prec_85_recall = max(precision[recall > 0.85]))

# chance level:
recall_precision_group_df_RF$prev_pos <- prev_pos_group$prev_pos #xx

# BVD:

# precision reached for 75 % and 85% recall:


recall_precision_group_df_BVD <- pr_dat_bvd %>% 
  group_by(group) %>% 
  summarise(BVD_prec_75_recall = max(precision[recall > 0.75]),
            BVD_prec_85_recall = max(precision[recall > 0.85]))

# chance level:
recall_precision_group_df_BVD$prev_pos <- prev_pos_group$prev_pos