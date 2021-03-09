# Processing of audio recordings to fit input requirements of BirdVoxDetect 
# (https://github.com/BirdVox/birdvoxdetect); 
# Processing output of BirdVoxDetect to make predictions comparable to those of the 
# random forest classifier
# = determining most confident BirdVoxDetect prediction in each sound clip used to 
# train and test the random forest classifier


# PACKAGES: ==========================================================================

library(tuneR)
library(lubridate)

# MAIN ===============================================================================

# load data:
# metadata on audio recordings:
load(file = "~/Master/Masterarbeit/Code/Code_final/audio_metadata.RData")

# directory where audio recordings are stored:
audio_dir <- "D:/Masterarbeit/Tonaufnahmen/Originalaufnahmen/"


# Preparing input for BVD: -----------------------------------------------------------

## needs to be monophonic -> store each channel of stereo recordings separately

## only for annotated time period because BVDs computation time is rather long (approx. 30 min for 3 hr recording)
## -> attention: sometimes several disjoint parts of a single recording annotated

for(n in 1:nrow(audio_metadata)){
  
  print(n)

  # read audiofile:
  
  # recordings where a single time period is annotated:
  
  if(n %in% c(1:14, 16, 18: 73, 75:81)){
    
    s <- readWave(paste0(audio_dir, audio_metadata$filename[n], ".WAV"), 
                  from = audio_metadata$start1_s[n], 
                  to = audio_metadata$end1_s[n], 
                  units = "seconds")
  }
  
  
  # recordings where a two disjoint time periods are annotated:
  
  if(n %in% c(17, 74)){

    s1 <- readWave(paste0(audio_dir, audio_metadata$filename[n], ".WAV"), 
                   from = audio_metadata$start1_s[n], 
                   to = audio_metadata$end1_s[n], 
                   units = "seconds")
    s2 <- readWave(paste0(audio_dir, audio_metadata$filename[n], ".WAV"), 
                   from = audio_metadata$start2_s[n], 
                   to = audio_metadata$end2_s[n], 
                   units = "seconds")
    s <- bind(s1, s2)
    rm(s1)
    rm(s2)
  }
  
  # recording where a three disjoint time periods are annotated:
  
  if(n == 15){
    
    s1 <- readWave(paste0(audio_dir, audio_metadata$filename[n], ".WAV"), 
                   from = audio_metadata$start1_s[n], 
                   to = audio_metadata$end1_s[n], 
                   units = "seconds")
    s2 <- readWave(paste0(audio_dir, audio_metadata$filename[n], ".WAV"), 
                   from = audio_metadata$start2_s[n], 
                   to = audio_metadata$end2_s[n], 
                   units = "seconds")
    s3 <- readWave(paste0(audio_dir, audio_metadata$filename[n], ".WAV"), 
                   from = audio_metadata$start3_s[n], 
                   to = audio_metadata$end3_s[n], 
                   units = "seconds")
    s <- bind(s1, s2, s3)
    rm(s1)
    rm(s2)
    rm(s3)
  }
  
  # extract left channel:
  s_mono_left <- mono(s, which = "left")
  
  # write left channel to monophonic audio file:
  monofile_name_left <- paste0(audio_metadata$filename[n], "_mono_left")
  writeWave(s_mono_left, 
            filename = paste0(audio_dir, "/Recordings_in_mono/", monofile_name_left, ".wav"), 
            extensible = F)
  rm(s_mono_left)
  
  # extract right channel:
  s_mono_right <- mono(s, which = "right")
  rm(s)
  
  # write right channel to monophonic audio file:
  monofile_name_right <- paste0(audio_metadata$filename[n], "_mono_right")
  writeWave(s_mono_right, 
            filename = paste0(audio_dir, "/Recordings_in_mono/", monofile_name_right, ".wav"), 
            extensible = F)
  rm(s_mono_right)

}



# Run BVD: ---------------------------------------------------------------------------

# see https://github.com/BirdVox/birdvoxdetect
# from the command line on Windows e.g. with:
# >python -m birdvoxdetect "D:\Masterarbeit\Tonaufnahmen\Originalaufnahmen\Recordings_in_mono\190905_11_42_40_Romania_NM_RM_LS_35966_1_mono_left.wav" -C -t=0 -v



# Postprocessing of BirdVoxDetect output: --------------------------------------------

# directory BVD output is stored:
BVD_directory <- "D:/Masterarbeit/Tonaufnahmen/Originalaufnahmen/Recordings_in_mono/"



# BVD checklist files:
BVD_checklist.list <- list.files(path = BVD_directory, pattern = "checklist")

# sort audio metadata:
audio_metadata <- audio_metadata[order(audio_metadata$filename, decreasing = F),]
rownames(audio_metadata) <- as.character(1:81)

# audio recording numbers:
audio_metadata_n <- c(1:81)
# corresponding BVD checklist file number:
checklist_n <- seq(1, 164, 2)


# connect BVD checklists to the 10 s audio clips used with RF:

for(j in 1:length(checklist_n)){
  
  print(j)
  
  # left channel:
  BVD_checklist_left <- read.csv(paste0(BVD_directory, BVD_checklist.list[checklist_n[j]]))
  
  # convert time unit:
  BVD_checklist_left$time_s <- as.numeric(hms(BVD_checklist_left$Time..hh.mm.ss.))
  
  # convert confidence:
  BVD_checklist_left$preds <- BVD_checklist_left$Confidence..../100
  
  # right channel:
  BVD_checklist_right <- read.csv(paste0(BVD_directory, BVD_checklist.list[checklist_n[j]+1]))
  
  # convert time unit:
  BVD_checklist_right$time_s <- as.numeric(hms(BVD_checklist_right$Time..hh.mm.ss.))
  
  # convert confidence:
  BVD_checklist_right$preds <- BVD_checklist_right$Confidence..../100
  
  
  
  # merge to the 10 s audio clips, use for each sound clip BVD prediction with highest confidence:

  # match timestamps of BVD predictions, which refer to annotated period only, to time within the full audio recording:
  
  # time adjustment left channel:
  
  # adjust time when annotation doesn't start at the beginning of audio file:
  BVD_checklist_left$time_s_korr <- BVD_checklist_left$time_s + audio_metadata$start1_s[audio_metadata_n[j]]
  
  # 10 s audio clips in recording used with the RF:
  time_segs <- seq(from = audio_metadata$start1_s[audio_metadata_n[j]], 
                   to = audio_metadata$end1_s[audio_metadata_n[j]]-10, 
                   by = 10)
  
  # time adjustment for second annotated period within one audio file:
  if(audio_metadata$start2_s[audio_metadata_n[j]] != 0){
    
    # BVD predictions refering to second annotation period:
    annot_segs2_ind <- which(BVD_checklist_left$time_s_korr > audio_metadata$end1_s[audio_metadata_n[j]])
    # set these predictions to correct time within audio recording 
    # (add non-annotated time period to predicted time stamps):
    BVD_checklist_left$time_s_korr2 <- BVD_checklist_left$time_s_korr
    BVD_checklist_left$time_s_korr2[annot_segs2_ind] <- BVD_checklist_left$time_s_korr[annot_segs2_ind] + 
      (audio_metadata$start2_s[audio_metadata_n[j]] - audio_metadata$end1_s[audio_metadata_n[j]])
    BVD_checklist_left$time_s_korr <- BVD_checklist_left$time_s_korr2
    
    # add to 10 s audio clips in recording used with the RF:
    time_segs <- seq(from = audio_metadata$start1_s[audio_metadata_n[j]], 
                     to = audio_metadata$end1_s[audio_metadata_n[j]], 
                     by = 10)
    
    time_segs2 <- seq(from = audio_metadata$start2_s[audio_metadata_n[j]], 
                      to = audio_metadata$end2_s[audio_metadata_n[j]]-10, 
                      by = 10)
    time_segs <- c(time_segs, time_segs2)
  }else{}
  
  # time adjustment for third annotated period within one audio file:
  if(audio_metadata$start3_s[audio_metadata_n[j]] != 0){
    
    # BVD predictions refering to third annotation period:
    annot_segs3_ind <- which(BVD_checklist_left$time_s_korr2 > audio_metadata$end2_s[audio_metadata_n[j]])
    # set these predictions to correct time within audio recording 
    # (add non-annotated time period to predicted time stamps):
    BVD_checklist_left$time_s_korr3 <- BVD_checklist_left$time_s_korr2
    BVD_checklist_left$time_s_korr3[annot_segs3_ind] <- BVD_checklist_left$time_s_korr2[annot_segs3_ind] +
      (audio_metadata$start3_s[audio_metadata_n[j]] - audio_metadata$end2_s[audio_metadata_n[j]])
    BVD_checklist_left$time_s_korr <- BVD_checklist_left$time_s_korr3
    
    # add to 10 s audio clips in recording used with the RF:
    time_segs <- seq(from = audio_metadata$start1_s[audio_metadata_n[j]], 
                     to = audio_metadata$end1_s[audio_metadata_n[j]], 
                     by = 10)
    time_segs2 <- seq(from = audio_metadata$start2_s[audio_metadata_n[j]], 
                      to = audio_metadata$end2_s[audio_metadata_n[j]], 
                      by = 10)
    time_segs3 <- seq(from = audio_metadata$start3_s[audio_metadata_n[j]], 
                      to = audio_metadata$end3_s[audio_metadata_n[j]]-10, 
                      by = 10)
    time_segs <- c(time_segs, time_segs2, time_segs3)
  }else{}
  
  
  # time adjustment right channel:
  
  # adjust time when annotation doesn't start at the beginning of audio file:
  BVD_checklist_right$time_s_korr <- BVD_checklist_right$time_s + audio_metadata$start1_s[audio_metadata_n[j]]
  
  # time adjustment for second annotated period within one audio file:
  if(audio_metadata$start2_s[audio_metadata_n[j]] != 0){
    # BVD predictions refering to second annotation period:
    annot_segs2_ind <- which(BVD_checklist_right$time_s_korr > audio_metadata$end1_s[audio_metadata_n[j]])
    # set these predictions to correct time within audio recording 
    # (add non-annotated time period to predicted time stamps):
    BVD_checklist_right$time_s_korr2 <- BVD_checklist_right$time_s_korr
    BVD_checklist_right$time_s_korr2[annot_segs2_ind] <- BVD_checklist_right$time_s_korr[annot_segs2_ind] + 
      (audio_metadata$start2_s[audio_metadata_n[j]] - audio_metadata$end1_s[audio_metadata_n[j]])
    BVD_checklist_right$time_s_korr <- BVD_checklist_right$time_s_korr2
  }
  
  # time adjustment for third annotated period within one audio file:
  if(audio_metadata$start3_s[audio_metadata_n[j]] != 0){
    
    # BVD predictions refering to third annotation period:
    annot_segs3_ind <- which(BVD_checklist_right$time_s_korr2 > audio_metadata$end2_s[audio_metadata_n[j]])
    # set these predictions to correct time within audio recording 
    # (add non-annotated time period to predicted time stamps):
    BVD_checklist_right$time_s_korr3 <- BVD_checklist_right$time_s_korr2
    BVD_checklist_right$time_s_korr3[annot_segs3_ind] <- BVD_checklist_right$time_s_korr2[annot_segs3_ind] + 
      (audio_metadata$start3_s[audio_metadata_n[j]] - audio_metadata$end2_s[audio_metadata_n[j]])
    BVD_checklist_right$time_s_korr <- BVD_checklist_right$time_s_korr3
  }
  
  
  # match BVD predictions to time within the full audio recording:
  BVD_conf_both <- data.frame("audiofile" = audio_metadata$filename[audio_metadata_n[j]], 
                              "time_in_record_s" = time_segs, 
                              "max_conf_BVD_left" = NA,  
                              "max_conf_BVD_right" = NA)
  
  
  for(i in 1:length(time_segs)){
    
    # left channel:
    segs_left <- which(BVD_checklist_left$time_s_korr >= time_segs[i] &
                         BVD_checklist_left$time_s_korr < (time_segs[i] + 10))
    
    BVD_conf_both$max_conf_BVD_left[i] <- ifelse(length(segs_left) == 0, 
                                                 0, 
                                                 max(BVD_checklist_left$preds[segs_left]))
    
    # right channel:
    segs_right <- which(BVD_checklist_right$time_s_korr >= time_segs[i] &
                          BVD_checklist_right$time_s_korr < (time_segs[i] + 10))
    
    BVD_conf_both$max_conf_BVD_right[i] <- ifelse(length(segs_right) == 0, 
                                                  0, 
                                                  max(BVD_checklist_right$preds[segs_right]))
  }
  
  if(j == 1) {
    BVD_conf_df <- BVD_conf_both}
  else{
    BVD_conf_df <- rbind(BVD_conf_df, BVD_conf_both)
  }
  
}


# store BVD predictions matched to 10 s audio clip used with RF:
save(BVD_conf_df, file = "~/Master/Masterarbeit/Code/Code_final/BVD_preds.RData")