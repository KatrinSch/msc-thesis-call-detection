# Pre-processing and extraction of features from sound clips derived from audio recordings,
# features are used to train a random forest classifier to distinguish between sound 
# clips containing nocturnal flight calls (NFCs) and sound clips not containing NFCs;

# Grouping of recorded nights for cross validation


# PACKAGES: ==========================================================================

library(seewave)
library(tuneR)
library(doParallel)
library(robustbase)
library(mmand)
library(raster)

# FUNCTIONS (load from scripts): =====================================================

source("~/Master/Masterarbeit/Code/Code_final/1_Functions.R") 

# MAIN PART ==========================================================================

# load data:
# audiofile metadata:
load(file = "~/Master/Masterarbeit/Code/Code_final/audio_metadata.RData")

# register cores for parallel computation:
#registerDoParallel(cores = 7) 

# directory of selection tables:
directory <- "~/Master/Masterarbeit/Selection_tables/Selection_tables_species_flight_calls_only_both_channels_annot/"
# list of selection tables:
file.list <- list.files(path = directory, pattern = "\\.txt$")

# directory of audiofiles:
audio_dir <- "D:/Masterarbeit/Tonaufnahmen/Originalaufnahmen/"
# list of audio files:
audiofile.list <- gsub("Table.1.selections.txt", "wav", file.list)


# relevant audiofiles (audio recordings with at least one relevant NFC in the respective night):
file.list.rel <- file.list[ind_file_used]
audiofile.list.rel <- audiofile.list[ind_file_used]


# Maximum frequency spectrum of each sound clip from each audio recording: -----------

for(f in c(1:length(ind_file_used))){
  
  print(f) # audio recording
  
  # start and end of sound clips derived from audio recording:
  start_end <- segs_start_end(sel_tbl_name = file.list.rel[[f]], segment_length_s = 10)
  
  # maximum frequency spectrum of clips from left channel:
  df_max_freq_spec <- par_max_freq_spec(file = paste0(audio_dir, audiofile.list.rel[[f]]), 
                                        start = start_end$start, 
                                        end = start_end$end,
                                        channel = 1)

  # merge with selection table, NFCs in left channel:
  df_max_freq_spec_with_sel_1 <- merge_sel_tbl_with_ch(file =  paste0(directory, file.list.rel[[f]]), 
                                                       df = df_max_freq_spec, 
                                                       channel = 1)
  
  # maximum frequency spectrum of clips from right channel only if NFCs are contained:
  # merge with selection table to identify clips with NFCs in right channel:
  df_max_freq_spec_with_sel_2 <- merge_sel_tbl_with_ch(file = paste0(directory, file.list.rel[[f]]), 
                                                       df = df_max_freq_spec, 
                                                       channel = 2)
  
  # maximum frequency spectrum of clips of right channel with NFCs:
  
  ## sound clips with NFCs:
  be_segs <- df_max_freq_spec_with_sel_2[which(df_max_freq_spec_with_sel_2$bird_event == 1), 1:4]
  
  # if NFCs in right channel:
  if(nrow(be_segs) != 0){
    
    # maximum frequency spectrum:
    df_max_freq_spec_2 <- par_max_freq_spec(file = paste0(audio_dir, be_segs$audiofile[1],".wav"), 
                                            start = be_segs$time_in_record_s, 
                                            end = be_segs$time_in_record_s + 10,
                                            channel = 2)
    # add to dataframe:
    df_max_freq_spec_2 <- cbind(be_segs, df_max_freq_spec_2[5:ncol(df_max_freq_spec_2)])
    df_max_freq_spec_2$channel <- 2
    df_max_freq_spec_with_sel_1 <- rbind(df_max_freq_spec_with_sel_1, df_max_freq_spec_2)
  }
  
  if(f == 1){
    df_max_freq_spec_with_sel_tot <- df_max_freq_spec_with_sel_1
  }else{
    df_max_freq_spec_with_sel_tot <- rbind(df_max_freq_spec_with_sel_tot, 
                                           df_max_freq_spec_with_sel_1)
  }
}

# store dataframe:
save(df_max_freq_spec_with_sel_tot, file = "~/Master/Masterarbeit/Code/df_max_freq_spec_with_sel_tot.RData")


# Features derived from maximum frequency spectrum: ----------------------------------

# 1) Sum of amplitude values in frequency bands from 0 - 1 kHz, >1 - 2 kHz, >2 - 3 kHz, >3 - 6 kHz, >6 - 8 kHz, >8 - 11.025 kHz;
# 2) Frequencies at which 25%, 50% , 75% and 90% of the total amplitude in the maximum frequency spectrum is reached;
# 3) Amplitude peaks in the corresponding maximum frequency spectrum for which both slopes were greater than 0.05

freq_bands_mfs <- seq(0, (22050/2) - (22050/512), length.out = 256)/1000

# amplitude sum in frequency bands:
df_max_freq_spec_with_sel_tot$ampl_sum_0_1kHz <-NA
df_max_freq_spec_with_sel_tot$ampl_sum_1_2kHz <-NA
df_max_freq_spec_with_sel_tot$ampl_sum_2_3kHz <-NA
df_max_freq_spec_with_sel_tot$ampl_sum_3_6kHz <-NA
df_max_freq_spec_with_sel_tot$ampl_sum_6_8kHz <-NA
df_max_freq_spec_with_sel_tot$ampl_sum_8_11kHz <-NA

# frequency where 25%-, 50%-, 75%- and 90% of amplitude sum is reached:
df_max_freq_spec_with_sel_tot$f_25_ampl_bw1 <- NA
df_max_freq_spec_with_sel_tot$f_50_ampl_bw1 <- NA
df_max_freq_spec_with_sel_tot$f_75_ampl_bw1 <- NA
df_max_freq_spec_with_sel_tot$f_90_ampl_bw1 <- NA

# number of amplitude peaks > 0.05 in 1 kHz wide frequency bands:
freq_bands <- seq(0,11,1)

n_peaks_freq_band1kHz <- as.data.frame(matrix(ncol = length(freq_bands)-1, 
                                              nrow = nrow(df_max_freq_spec_with_sel_tot)))
colnames(n_peaks_freq_band1kHz) <- paste0("pks_", 0:10, "_", 1:11, "kHz_bw1")
df_max_freq_spec_with_sel_tot <- cbind(df_max_freq_spec_with_sel_tot, n_peaks_freq_band1kHz)


for(i in 1:nrow(df_max_freq_spec_with_sel_tot)){
  
  print(paste(i, "/", nrow(df_max_freq_spec_with_sel_tot)))
 
  # maximum frequency spectrum as matrix:
  mfspec <- as.matrix(data.frame(freq_bands_mfs, as.numeric(df_max_freq_spec_with_sel_tot[i, 5:260])))
  
  # amplitude sum in frequency bands:
  df_max_freq_spec_with_sel_tot$ampl_sum_0_1kHz[i] <- sum(mfspec[which(mfspec[,1] >= 0 & mfspec[,1] <= 1 ),2])
  df_max_freq_spec_with_sel_tot$ampl_sum_1_2kHz[i] <- sum(mfspec[which(mfspec[,1] > 1 & mfspec[,1] <= 2 ),2])
  df_max_freq_spec_with_sel_tot$ampl_sum_2_3kHz[i] <- sum(mfspec[which(mfspec[,1] > 2 & mfspec[,1] <= 3 ),2])
  df_max_freq_spec_with_sel_tot$ampl_sum_3_6kHz[i] <- sum(mfspec[which(mfspec[,1] > 3 & mfspec[,1] <= 6 ),2])
  df_max_freq_spec_with_sel_tot$ampl_sum_6_8kHz[i] <- sum(mfspec[which(mfspec[,1] > 6 & mfspec[,1] <= 8 ),2])
  df_max_freq_spec_with_sel_tot$ampl_sum_8_11kHz[i] <- sum(mfspec[which(mfspec[,1] > 8 & mfspec[,1] <= 11.025 ),2])
  
  
  # frequency where 25%, 50%, 75% and 90% of amplitude sum is reached:
  total_energy <- sum(mfspec[,2])
  cs_energy <- cumsum(mfspec[,2])
  df_max_freq_spec_with_sel_tot$f_25_ampl_bw1[i] <- as.numeric(mfspec[min(which(cs_energy >= 0.25*total_energy)),1])
  df_max_freq_spec_with_sel_tot$f_50_ampl_bw1[i] <- as.numeric(mfspec[min(which(cs_energy >= 0.50*total_energy)),1])
  df_max_freq_spec_with_sel_tot$f_75_ampl_bw1[i] <- as.numeric(mfspec[min(which(cs_energy >= 0.75*total_energy)),1])
  df_max_freq_spec_with_sel_tot$f_90_ampl_bw1[i] <- as.numeric(mfspec[min(which(cs_energy >= 0.90*total_energy)),1])

  
  # number of amplitude peaks > 0.05 in 1 kHz wide frequency bands:
  
  n_peaks <- numeric(length(freq_bands)-1)
  
  peaks <- fpeaks(mfspec, f = 22050, amp = c(0.05,0.05), plot = F)
  
  for(j in 1:(length(freq_bands)-1)){
    n_peaks[j] <- length(which(peaks[,'freq'] >= freq_bands[j] & peaks[,'freq'] < freq_bands[j+1]))
  }
  
  df_max_freq_spec_with_sel_tot[i, 272:282] <- n_peaks #xx
}

# store dataframe:
save(df_max_freq_spec_with_sel_tot, file = "~/Master/Masterarbeit/Code/df_max_freq_spec_with_sel_tot2.RData")


# Most prominent sound event in each sound clip: -------------------------------------
# max. frequency, min. frequency, frequency range, time duration


for (f in c(1:length(ind_file_used))){
  
  print(f) # audio recording
  
  # start and end of sound clips derived from audio recording:
  start_end <- segs_start_end(sel_tbl_name = file.list.rel[[f]], segment_length_s = 10)
  
  # calculate spectrogram and extract features of sound clips from left channel:
  df_spectro_event_feat <- par_spectro_segs_features(file = paste0(audio_dir, audiofile.list.rel[[f]]), 
                                                     start = start_end$start, 
                                                     end = start_end$end,
                                                     segment_length_s = 10,
                                                     wl_freq = 512, 
                                                     ovlp_freq = 75,
                                                     channel = 1)
  
  # merge with selection table, NFCs in left channel:
  df_spectro_event_feat_with_sel_1 <- merge_sel_tbl_with_ch(file = paste0(directory, file.list.rel[[f]]), 
                                                            df = df_spectro_event_feat, 
                                                            channel = 1)
  
  # spectrogram features of clips from right channel only if NFCs are contained:
  # merge with selection table to identify clips with NFCs in right channel:
  df_spectro_event_feat_with_sel_2 <- merge_sel_tbl_with_ch(file = paste0(directory, file.list.rel[[f]]), 
                                                            df = df_spectro_event_feat, 
                                                            channel = 2)
  
  # spectrogram event features of clips of right channel with NFCs:
  
  ## sound clips with NFCs:
  be_segs <- df_spectro_event_feat_with_sel_2[which(df_spectro_event_feat_with_sel_2$bird_event == 1), 1:4]
  
  # if NFCs in right channel:
  if(nrow(be_segs) != 0){
    
    df_spectro_event_feat_2 <- par_spectro_segs_features(file = paste0(audio_dir, be_segs$audiofile[1],".wav"), 
                                                         start = be_segs$time_in_record_s, 
                                                         end = be_segs$time_in_record_s + 10,
                                                         segment_length_s = 10,
                                                         wl_freq = 512, 
                                                         ovlp_freq = 75,
                                                         channel = 2)
    
    # add to dataframe:
    df_spectro_event_feat_2 <- cbind(be_segs, df_spectro_event_feat_2[5:ncol(df_spectro_event_feat_2)])
    df_spectro_event_feat_2$channel <- 2
    df_spectro_event_feat_with_sel_1 <- rbind(df_spectro_event_feat_with_sel_1, df_spectro_event_feat_2)
    
  }
  
  if(f == 1){
    df_spectro_event_feat_bw1_with_sel_tot <- df_spectro_event_feat_with_sel_1
  }else{
    df_spectro_event_feat_bw1_with_sel_tot <- rbind(df_spectro_event_feat_bw1_with_sel_tot, 
                                                    df_spectro_event_feat_with_sel_1)
  }
}

# store dataframe:
save(df_spectro_event_feat_bw1_with_sel_tot, file = "~/Master/Masterarbeit/Code/df_spectro_event_feat_bw1_with_sel_tot.RData")


# merge dataframe with features derived from maximum frequency spectrum with those derived from spectrogram:
df_max_freq_spec_spectro_feat_tot <- merge(df_max_freq_spec_with_sel_tot, 
                                           df_spectro_event_feat_bw1_with_sel_tot[,-c(3:4)], 
                                           by = c("audiofile", "time_in_record_s", "channel"), 
                                           sort = F)

# add recording night:
df_max_freq_spec_spectro_feat_tot <- merge(df_max_freq_spec_spectro_feat_tot, 
                                           audio_metadata[, 1:2], 
                                           by.x = "audiofile", 
                                           by.y = "filename", 
                                           sort = F)


# Grouping of the audio recordings: --------------------------------------------------

## pool nights considering location and dates:

nights <- unique(audio_metadata$date)

# sound clips with NFCs per night:
bes_per_night <- numeric(length(nights))

for(i in 1:length(nights)){
  
  bes_per_night[i] <- nrow(df_max_freq_spec_spectro_feat_tot[which(df_max_freq_spec_spectro_feat_tot$bird_event == 1 & df_max_freq_spec_spectro_feat_tot$date == nights[i]),])
  
}

# add location
bes_per_night_loc <- unique(merge(cbind(nights, bes_per_night), 
                                  audio_metadata[,c("date", "location")], 
                                  by.x = "nights", 
                                  by.y = "date"))

# grouping:
bes_per_night_loc$group <- c(1, 2, 3, 4, rep(5,5), 6, 7, 8, rep(9,3), rep(10,3), 
                             rep(11,5), rep(12,5), rep(13,3), rep(14,2), rep(15, 3), 
                             rep(16,3), 17, 18, 19, rep(20,2), 21, 20)


# number of species per group:

## audio files belonging to each group:
filenames_groups <- merge(audio_metadata[,c("filename","date")], bes_per_night_loc[, c("nights", "group")], by.x = "date", by.y = "nights")

## selection tables with NFC annotations in both channels:
directory = "~/Master/Masterarbeit/Selection_tables/Selection_tables_species_flight_calls_only_both_channels_annot/"

n_specs_group <- data.frame("group" = groups, "n_specs" = NA)

for(g in 1:length(groups)){
  
  files <- filenames_groups$filename[which(filenames_groups$group == g)]
  
  for(f in 1:length(files)){
    if(f == 1){
      specs <- recorded_species(directory = directory, file = paste0(files[f], ".Table.1.selections.txt"))
    }else{
      specs <- c(specs, recorded_species(directory = directory, file = paste0(files[f], ".Table.1.selections.txt")))
    }
  }
  specs2 <- unique(specs)
  
  # check against species names: 
  for(s in 1:length(specs2)){
    spec_names <- spec_group_occ$species[which(stringr::str_detect(string = specs2[s], 
                                                                   pattern = as.character(spec_group_occ$species)))]
    if(s == 1){
      specs3 <- as.character(spec_names)
    }else{
      specs3 <- c(specs3, as.character(spec_names))
    }
  }
  n_specs_group$n_specs[g] <- length(unique(specs3))
}

bes_per_night_loc <- merge(bes_per_night_loc, n_specs_group)


# store grouping table:
save(bes_per_night_loc, file = "~/Master/Masterarbeit/Code/Code_final/grouping_table.RData") 


# add group to feature dataset:
df_max_freq_spec_spectro_feat_tot <- merge(df_max_freq_spec_spectro_feat_tot, 
                                           bes_per_night_loc[,c(1,4)], 
                                           by.x = "date", 
                                           by.y = "nights", 
                                           sort = F)

# store feature dataset:
save(df_max_freq_spec_spectro_feat_tot, file = "~/Master/Masterarbeit/Code/Code_final/df_max_freq_spec_spectro_feat_final.RData")


# store columns with dataset:
df_sound_clips <- df_max_freq_spec_spectro_feat_tot[, c("date", "audiofile", "time_in_record_s", "channel", "bird_event", "species", "group")]
save(df_sound_clips, file = "~/Master/Masterarbeit/Code/Code_final/df_sound_clips.RData")