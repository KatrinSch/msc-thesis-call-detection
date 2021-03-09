# Functions used with regard to the detection of nocturnal flight calls (NFCs) 
# in audio recordings:


# GENERAL HELPER FUNCTIONS: ==========================================================


# Species in a single selection table or in all selection tables in a folder: ----
# input:  directory
#         file
#         count: T/F, number of occurrences
# output: character vector

recorded_species <- function(directory = "~/Master/Masterarbeit/Selection_tables/Selection_tables_species_flight_calls_only", 
                             file = "", 
                             count = F){
  
  if (file == ""){
    
    # selection tables in working directory:
    file.list <- list.files(path = directory, pattern = "\\.txt$")
    
    species <- numeric()
    
    for(i in 1:length(file.list)){
      # read selection table:
      Sel_tbl <- read.delim(paste0(directory, "/",file.list[[i]]), stringsAsFactors = F)
      # extract species:
      species <- c(species, Sel_tbl$Species)
    }
    
  }else{
    
    # read selection table:
    Sel_tbl <- read.delim(paste0(directory, "/", file), stringsAsFactors = F)
    # extract species:
    species <- Sel_tbl$Species
    
  }
  
  species_all <- unique(species)
  
  result <- sort(species_all)
  
  if(count == T){
    
    species_count <- numeric()
    
    # occurrence of each species:
    for(j in 1:length(species_all)){
      species_count[j] <- length(which(species == species_all[j]))
    }
    species_all_count <- paste0(species_all, " (", species_count, ")")
    
    result <- sort(species_all_count)
  }
  
  return(result)
}



# Selection tables in which a certain species is contained: ----
# input:  directory
#         species
# output: character vector

files_with_species <- function(directory = "~/Master/Masterarbeit/Selection_tables/Selection_tables_species_flight_calls_only",
                               species){
  
  file.list <- list.files(path = directory, pattern = "\\.txt$")
  
  files_with_species <- NULL
  
  for(f in 1:length(file.list)){
    
    species_contained <- species %in% recorded_species(file = file.list[[f]])
    
    if(species_contained){
      files_with_species <- c(files_with_species, file.list[[f]])
    }
  }
  
  return(files_with_species)
}



# Frequency range for NFCs of certain species/in specific selection table/in selection tables in a folder: ----
# input:  directory
#         species
#         file
# output: data frame

freq_range <- function(directory = "~/Master/Masterarbeit/Selection_tables/Selection_tables_species_flight_calls_only", 
                       species = "", 
                       file = ""){
  
  if (file == ""){
    
    # selection tables in working directory:
    file.list <- list.files(path = directory, pattern = "\\.txt$")
    
    # frequency range of all calls in entire folder:
    if (species == ""){
      
      # store intermediate results:
      min_freq <- numeric(length(file.list))
      max_freq <- numeric(length(file.list))
      
      for(i in 1:length(file.list)){
        # read selection table:
        Sel_tbl <- read.delim(paste0(directory, "/",file.list[[i]]))
        # min. and max. frequency:
        min_freq[i] <- min(Sel_tbl$Low.Freq..Hz.)
        max_freq[i] <- max(Sel_tbl$High.Freq..Hz.)
      }
      
      result <- c(min(min_freq), max(max_freq))
    }
    
    # frequency range of all calls of a certain species in entire folder:
    if (species != ""){
      
      # store intermediate results:
      species_min_freqs <- numeric()
      species_max_freqs <- numeric()
      
      for(i in 1:length(file.list)){
        # read selection table:
        Sel_tbl <- read.delim(paste0(directory, "/",file.list[[i]]))
        # min. and max. frequency:
        species_min_freqs <- c(species_min_freqs, Sel_tbl$Low.Freq..Hz.[which(Sel_tbl$Species == species)])
        species_max_freqs <- c(species_max_freqs, Sel_tbl$High.Freq..Hz.[which(Sel_tbl$Species == species)])
      }
      
      result <- data.frame("min_freq" = range(species_min_freqs), "max_freq" = range(species_max_freqs))
    }
  }
  
  
  if (file != ""){
    
    # read selection table:
    Sel_tbl <- read.delim(paste0(directory, "/", file))
    
    # frequency range of all calls in a selection table:
    if (species == ""){
      
      min_freq <- min(Sel_tbl$Low.Freq..Hz.)
      max_freq <- max(Sel_tbl$High.Freq..Hz.)
      
      result <- c(min_freq, max_freq)
    }
    
    # frequency range of calls of a certain species in a selection table:
    if (species != ""){
      
      # store intermediate results:
      species_min_freqs <- numeric()
      species_max_freqs <- numeric()
      
      # min. and max. frequency:
      if (!is.element(species, Sel_tbl$Species)) stop(paste("Species not in selection table."))
      
      species_min_freqs <- Sel_tbl$Low.Freq..Hz.[which(Sel_tbl$Species == species)]
      species_max_freqs <- Sel_tbl$High.Freq..Hz.[which(Sel_tbl$Species == species)]
      
      result <- data.frame("min_freq" = range(species_min_freqs), "max_freq" = range(species_max_freqs))
    }
  }
  
  return(result)
}



# Duration range of calls of certain species/in specific selection table/in selection tables in a folder: ----
# input:  directory
#         species
#         file
# output: data frame

call_duration <- function(directory = "~/Master/Masterarbeit/Selection_tables/Selection_tables_species_flight_calls_only", 
                          file = "", 
                          species = ""){
  
  if (file == ""){
    
    # selection tables in working directory:
    file.list <- list.files(path = directory, pattern = "\\.txt$")
    
    # duration range of all calls in entire folder:
    if (species == ""){
      
      # store intermediate results:
      min_duration <- numeric(length(file.list))
      max_duration <- numeric(length(file.list))
      
      for(i in 1:length(file.list)){
        # read selection table:
        Sel_tbl <- read.delim(paste0(directory, "/",file.list[[i]]))
        # min. and max. duration:
        min_duration[i] <- min(Sel_tbl$End.Time..s. - Sel_tbl$Begin.Time..s.)
        max_duration[i] <- max(Sel_tbl$End.Time..s. - Sel_tbl$Begin.Time..s.)
      }
      
      result <- c(min(min_duration[which(!is.infinite(min_duration))]), 
                  max(max_duration[which(!is.infinite(max_duration))]))
    }
    
    # duration range of all calls of a certain species in entire folder:
    if (species != ""){
      
      # store intermediate results:
      species_duration <- numeric()
      
      for(i in 1:length(file.list)){
        # read selection table:
        Sel_tbl <- read.delim(paste0(directory, "/",file.list[[i]]))
        # min. and max. duration:
        species_duration <- c(species_duration, Sel_tbl$End.Time..s.[which(Sel_tbl$Species == species)] - Sel_tbl$Begin.Time..s.[which(Sel_tbl$Species == species)])
      }
      
      result <- species_duration
    }
  }
  
  if (file != ""){
    
    # read selection table:
    Sel_tbl <- read.delim(paste0(directory, "/", file))
    
    # duration range of all calls in a selection table:
    if (species == ""){
      
      # min. and max. duration:
      min_duration <- min(Sel_tbl$End.Time..s. - Sel_tbl$Begin.Time..s.)
      max_duration <- max(Sel_tbl$End.Time..s. - Sel_tbl$Begin.Time..s.)
      
      result <- c(min_duration, max_duration)
    }
    
    # duration range of calls of a certain species in a selection table:
    if (species != ""){
      
      if (!is.element(species, Sel_tbl$Species)) stop(paste("Species not in selection table."))
      
      # store intermediate results:
      species_duration <- numeric()
      
      # min. and max. duration:
      species_duration <- c(species_duration, Sel_tbl$End.Time..s.[which(Sel_tbl$Species == species)] - Sel_tbl$Begin.Time..s.[which(Sel_tbl$Species == species)])
      
      result <- species_duration
    }
  }
  
  return(result)
}



# Convert seconds to hour-minute-seconds: --------------------------------------------
# input: seconds
# output: character - "hours:minutes:seconds"

time_conv_hr_min_s <- function(sec){
  hr <- floor(sec/(60*60))
  min <- floor(sec%%(60*60)/60)
  sec <- sec%%(60*60)%%60
  return(paste(hr, ":", min, ":", sec))
}



# FUNCTIONS FOR AUDIO FILE AND SELECTION TABLE MANAGEMENT: ===========================


# Start and end time of sound clips derived from annotated time period of an audio recording: ----
# input: name of selection table
#        length of clips to split audio file into [s]
#        metadata on audio recordings including annotated time period
# output: data frame

segs_start_end <- function(sel_tbl_name, 
                           segment_length_s,
                           metadata = audio_metadata){
  
  metadata_ind <- which(paste0(metadata$filename, ".Table.1.selections.txt") == sel_tbl_name)
  
  
  if(metadata[metadata_ind, 'start2_s'] == 0){
    start <- seq(from = metadata[metadata_ind, 'start1_s'], 
                 to = metadata[metadata_ind, 'end1_s'] - segment_length_s, 
                 by = segment_length_s)
  }else{
    start <- seq(from = metadata[metadata_ind, 'start1_s'], 
                 to = metadata[metadata_ind, 'end1_s'], 
                 by = segment_length_s)
  }
  
  # if second time period was annotated in the same audio file:
  if(metadata[metadata_ind, 'start2_s'] != 0){
    start2 <- seq(from = metadata[metadata_ind, 'start2_s'], 
                  to = metadata[metadata_ind, 'end2_s'], 
                  by = segment_length_s)
    start <- c(start, start2)
  }
  # if third time period was annotated in the same audio file:
  if(metadata[metadata_ind, 'start3_s'] != 0){
    start3 <- seq(from = metadata[metadata_ind, 'start3_s'], 
                  to = metadata[metadata_ind, 'end3_s'] - segment_length_s, 
                  by = segment_length_s)
    start <- c(start, start3)
  }
  
  end <- start + segment_length_s
  start_end_df <- data.frame("start" = start, "end" = end)
  
  return(start_end_df)
  
}



# Merge annotations from selection tables with sound clips derived from audio recordings: ----
# input: selection table,
#        data frame with start and end time of sound clips
#        channel in which NFCs are annotated
# output: data frame


merge_sel_tbl_with_ch <- function(file, df, channel = 1){
  
  df$bird_event <- numeric(nrow(df))
  df$species <- numeric(nrow(df))
  df$channel = NA
  
  sel_tbl <- read.delim(file)
  
  # find time segments with sound event -> include species:
  
  df$next_segment <- c(df[-1, "time_in_record_s"], NA) # necessary for intermediate calculation
  
  # use only annotations from respective channel:
  sel_tbl <- sel_tbl[which(sel_tbl$Channel == channel),]
  
  func <- function(segment, next_segment){
    sel_tbl$Species[
      which((segment <= sel_tbl$Begin.Time..s. & next_segment >= sel_tbl$Begin.Time..s.)
            | (segment > sel_tbl$Begin.Time..s. & segment < sel_tbl$End.Time..s.))
      ]
  }
  
  species <- apply(df[, c("time_in_record_s", "next_segment")], MARGIN = 1, 
                   FUN = function(x) func(x["time_in_record_s"], x["next_segment"]))
  
  species_vec <- unlist(lapply(species, FUN = paste, collapse = ", ")) # enables string of several species in one sound segment
  if(!is.null(species_vec)){
    df$species <- species_vec
  }else{
    df$species <- ""
  }
  
  df$bird_event[which(df$species != "")] <- 1
  df$channel[which(df$species != "")] <- channel
  
  df$bird_event <- as.factor(df$bird_event)
  
  df$next_segment <- NULL # remove intermediate result
  
  return(df)
}


# FUNCTIONS FOR PRE-PROCESSING AND FEATURE EXTRACTION FROM SOUND CLIPS: ==============


# Maximum frequency spectrum of sound clips: ----
# input: path to audio file
#        start time of sound clip
#        end time of sound clip
#        window length with regard to the short-time fourier transformation
#        overlap between successive analysis windows
#        channel of the audio file to consider
# output: data frame

par_max_freq_spec <- function(file,
                              start, 
                              end,
                              wl_freq = 512, 
                              ovlp_freq = 75,
                              channel = 1) {
  
  # number of frequency bands:
  n_freq_bands <- wl_freq/2
  
  # create data frame:
  df_max_freq_spec <- data.frame(matrix(ncol = 4 + n_freq_bands, nrow = length(end)))
  
  colnames(df_max_freq_spec) <- c("audiofile", "time_in_record_s", "bird_event", "species", 
                                  paste0("bw1_f_", 1:n_freq_bands))
  
  df_max_freq_spec$time_in_record_s <- start
  df_max_freq_spec$audiofile <- rep(gsub(".wav", "", gsub(audio_dir, "", file)), nrow(df_max_freq_spec))
  
  # maximum frequency spectrum of each sound clip:
  
  max_freq_spec_values <- foreach(i = 1:length(end), .packages=c("seewave", "tuneR"), 
                                  .combine = "rbind", .verbose = T) %dopar%{
                                    
                                    # read clip of audio file:
                                    s <- readWave(file, 
                                                  from = start[i], 
                                                  to = end[i], 
                                                  units = "seconds")
                                    
                                    # preprocessing:
                                    
                                    # first filter, then downsampling to avoid aliasing:
                                    # lowpass filter:
                                    s_filtered <- fir(s, 
                                                      f = 44100, 
                                                      to = 22050/2, 
                                                      output = "Wave", 
                                                      channel = channel)
                                    
                                    # downsample to 22050 Hz:
                                    s_22050Hz <- resamp(s_filtered, 
                                                        f = 44100, 
                                                        g = 22050, 
                                                        output = "Wave")
                                    
                                    # butterworth filter (decrease noise):
                                    s_bwfilter1 <- bwfilter(s_22050Hz, 
                                                            n = 2, 
                                                            from = 1000, 
                                                            output="Wave")
                                    
                                    # feature extraction:
                                    
                                    # max. frequency spectrum:
                                    max_freq_spec1 <- meanspec(s_bwfilter1, 
                                                               f = 22050, 
                                                               norm = T, 
                                                               fftw = T, 
                                                               dB = NULL, 
                                                               wn = 'hanning', 
                                                               wl = wl_freq, 
                                                               ovlp = ovlp_freq, 
                                                               plot = F, 
                                                               FUN = max)
                                    
                                    all_values_one_seg <- max_freq_spec1[,2]
                                  }
  
  df_max_freq_spec[, 5:ncol(df_max_freq_spec)] <- max_freq_spec_values
  
  return(df_max_freq_spec)
}


# Duration and frequency range of most prominent sound event in the spectrogram of a sound clip. ----
# input: path to audio file
#        start time of sound clip
#        end time of sound clip
#        window length with regard to the short-time fourier transformation
#        overlap between successive analysis windows
#        channel of the audio file to consider
# output: data frame

par_spectro_segs_features <- function(file, 
                                      start, 
                                      end,
                                      wl_freq = 512, 
                                      ovlp_freq = 75,
                                      channel = 1) {
  
  
  # create data frame:
  df_spectro_event_feat <- data.frame(matrix(ncol = 4 + 4, nrow = length(end)))
  
  
  colnames(df_spectro_event_feat) = c("audiofile", "time_in_record_s", "bird_event", 
                                      "species", "event_duration", "event_freq_range", 
                                      "event_min_freq", "event_max_freq")
  
  df_spectro_event_feat$time_in_record_s <- start
  df_spectro_event_feat$audiofile <- rep(gsub(".wav", "", gsub(audio_dir, "", file)), 
                                         nrow(df_spectro_event_feat))
  
  # calculate freatures for segments:
  
  spectro_event_feat <- foreach(i = 1:length(end), .packages=c("seewave", "tuneR", 
                                                               "robustbase", "mmand", "raster"), 
                                .combine = "rbind", .verbose = T) %dopar%{
                                  
                                  # read clip of audio file:
                                  s <- readWave(file, 
                                                from = start[i], 
                                                to = end[i], 
                                                units = "seconds")
                                  
                                  # preprocessing:
                                  
                                  # first filter, then downsampling to avoid aliasing:
                                  # lowpass filter:
                                  s_filtered <- fir(s, 
                                                    f = 44100, 
                                                    to = 22050/2,
                                                    output="Wave", 
                                                    channel = channel)
                                  
                                  # downsample to 22050 Hz:
                                  s_22050Hz <- resamp(s_filtered, 
                                                      f = 44100, 
                                                      g = 22050, 
                                                      output = "Wave")
                                  
                                  # butterworth filter (decrease noise):
                                  s_bwfilter1 <- bwfilter(s_22050Hz, 
                                                          n = 2, 
                                                          from = 1000, 
                                                          output="Wave")
                                  
                                  # feature extraction:
                                  
                                  # normalised spectrogram:
                                  sf_spec_bw1 <- spectro(s_bwfilter1, 
                                                         f = 22050, 
                                                         wl = wl_freq, 
                                                         wn = "hanning", 
                                                         ovlp = ovlp_freq,
                                                         fftw = T, 
                                                         plot = F,
                                                         norm = T,
                                                         dB = NULL,
                                                         noisereduction = F)
                                  
                                  # frequency and time resolution:
                                  freq_res <- 0.04306641 # kHz
                                  time_res <- 0.005820722 # s
                                  
                                  # roughly following Lasseck 2015: 
                                  # row and column medians for clipping:
                                  cm <- colMedians(sf_spec_bw1$amp)
                                  rm <- rowMedians(sf_spec_bw1$amp)
                                  
                                  # initialise binary matrices:
                                  bin_mat <- matrix(0, 
                                                    nrow = nrow(sf_spec_bw1$amp), 
                                                    ncol = ncol(sf_spec_bw1$amp))
                                  bin_mat2 <- matrix(0, 
                                                     nrow = nrow(sf_spec_bw1$amp), 
                                                     ncol = ncol(sf_spec_bw1$amp))
                                  
                                  # rows above 3*rowmedian:
                                  
                                  for(i in 1:nrow(sf_spec_bw1$amp)){
                                    bin_mat[i,] <- ifelse(sf_spec_bw1$amp[i,] > 3* rm[i],1,0)
                                  }
                                  
                                  # columns above 3*colmedian:
                                  
                                  for(j in 1:ncol(sf_spec_bw1$amp)){
                                    bin_mat2[,j] <- ifelse(sf_spec_bw1$amp[,j] > 3* cm[j],1,0)
                                  }
                                  
                                  # add matrices: set to 1 where both matrices are 1, set to 0 otherwise
                                  bin_mat3 <- bin_mat + bin_mat2
                                  bin_mat4 <- bin_mat3
                                  bin_mat4[bin_mat3 == 1] <- 0
                                  bin_mat4[bin_mat3 == 2] <- 1
                                  
                                  # closing - dilation - median filter:
                                  
                                  # parameters:
                                  k <- shapeKernel(c(6,6), type = "box")
                                  k2 <- shapeKernel(c(10,10), type = "box")
                                  
                                  bin_mat5 <- closing(bin_mat4, kernel = k) # removes small holes
                                  bin_mat6 <- dilate(bin_mat5, kernel = k)
                                  bin_mat7 <- medianFilter(bin_mat6, k2)
                                  
                                  # convert to raster to find clusters of connected pixels of value 1:
                                  Rmat <- raster(bin_mat7)
                                  Clumps <- as.matrix(clump(Rmat, directions = 8)) 
                                  
                                  # largest cluster:
                                  largest_clump <- as.numeric(names(which.max(table(Clumps))))
                                  
                                  # cells belonging to largest cluster:
                                  res <- which(Clumps == largest_clump, arr.ind = T)
                                  
                                  # duration = extent over columns:
                                  duration_largest_cluster <- (max(res[,2]) - min(res[,2])) * time_res # s
                                  
                                  # freq_span_largest_cluster:
                                  dom_col_in_se <- as.numeric(names(which.max(table(res[,2]))))
                                  freq_span_largest_cluster <- length(which(res[,2] == dom_col_in_se)) * freq_res # kHz
                                  
                                  # min. freq:
                                  min_freq_largest_cluster <- min(res[which(res[,2] == dom_col_in_se)]) * freq_res
                                  
                                  # max. freq.:
                                  max_freq_largest_cluster <- max(res[which(res[,2] == dom_col_in_se)]) * freq_res

                                  all_values_one_seg <- c(duration_largest_cluster,
                                                          freq_span_largest_cluster,
                                                          min_freq_largest_cluster,
                                                          max_freq_largest_cluster)
                                }
  
  df_spectro_event_feat[, 5:ncol(df_spectro_event_feat)] <- spectro_event_feat
  
  return(df_spectro_event_feat)
}