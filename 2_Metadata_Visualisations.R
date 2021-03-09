# Assembling of metadata on audio recordings and recorded species;
# Visualisations with regard to 
# - number of recorded nocturnal flight calls (NFCs) per audio recording
# - frequency range of NFCs per species, 
# - species composition in audio recordings,
# - feature extraction from sound clips
# - spectrograms of the NFCs of selected species


# PACKAGES: ==========================================================================

library(lubridate)
library(tuneR)
library(ggplot2)
library(viridis)
library(gg.gap)
library(tidyr)
library(dplyr)
library(seewave)
library(robustbase)
library(mmand)
library(raster)

# FUNCTIONS (load from scripts): =====================================================

source("~/Master/Masterarbeit/Code/Code_final/1_Functions.R")


# MAIN PART ==========================================================================

# load data:
# recorded species:
load(file = "~/Master/Masterarbeit/Code/Code_final/species_occurrences.RData")
# grouping of recorded nights:
load(file = "~/Master/Masterarbeit/Code/Code_final/grouping_table.RData")
# table on sound clips, which were derived from audio recordings:
load(file = "~/Master/Masterarbeit/Code/Code_final/df_sound_clips.RData")


# Dataframe on recorded species: German name - English name - number of NFCs: --------

# all species occuring in selection tables:
all_species <- recorded_species() 
# with number of occurrences:
all_species_count <- recorded_species(count = T)

species_names_df <- data.frame("German" = all_species, "English" = NA)

species_names_df$English <- c("Dunlin", 
                              "Common blackbird", 
                              "Common blackbird?",
                              "Eurasian oystercatcher",
                              "Tree pipit",
                              "Common snipe",
                              "Eurasian coot",
                              "Eurasian coot?",
                              "Wood sandpiper",
                              "Duck spec.",
                              "Eurasian skylark",
                              "Eurasian tree sparrow?",
                              "Little ringed plover",
                              "Common sandpiper",
                              "Greylag goose",
                              "Grey heron",
                              "Spotted flycatcher",
                              "Spotted flycatcher/ European pied flycatcher",
                              "Spotted flycatcher?",
                              "Eurasian curlew",
                              "Common greenshank",
                              "Great crested grebe",
                              "Black redstart",
                              "House sparrow?",
                              "Hawfinch?",
                              "Northern lapwing",
                              "Northern lapwing?",
                              "Grey plover",
                              "Eurasian teal",
                              "Black-headed gull",
                              "Black-headed gull?",
                              "Common swift",
                              "Night heron",
                              "Red-backed shrike",
                              "Ortolan bunting",
                              "Ortolan bunting?",
                              "Purple heron",
                              "Heron spec.",
                              "Common reed bunting",
                              "Eurasian bittern",
                              "Greater flamingo",
                              "Redwing",
                              "European robin",
                              "European robin?",
                              "Western yellow wagtail",
                              "Flycatcher spec.",
                              "Song thrush",
                              "Song thrush?",
                              "Mallard",
                              "Mallard Male",
                              "Mallard Female",
                              "Mallard?",
                              "Common moorhen",
                              "Common moorhen?",
                              "European pied flycatcher",
                              "Eurasian stone-curlew",
                              "Spotted crake?",
                              "Common kestrel",
                              "Bird - spec. unknown",
                              "Green sandpiper",
                              "Water rail",
                              "Water rail?",
                              "Meadow pipit",
                              "Little bittern",
                              "Little bittern?")

# number of recorded NFCs per species:
species_names_df$count <- all_species_count

save(species_names_df, file = "~/Master/Masterarbeit/Code/Code_final/recorded_species.RData")

# Dataframe on audio recordings metadata: --------------------------------------------

# all recordings considered where at least one NFC was recorded in the corresponding night:
# (= include recordings without NFCs if NFCs occurred in another recording of the same night and location

# directory where selection tables are stored:
directory = "~/Master/Masterarbeit/Selection_tables/Selection_tables_species_flight_calls_only/"
# list of selection tables:
file.list <- list.files(path = directory, pattern = "\\.txt$")

# directory where audio files are stored:
audio_dir <- "D:/Masterarbeit/Tonaufnahmen/Originalaufnahmen/"

# recordings containing at least one NFC:

ind_file_used <- NULL
for(f in 1:length(file.list)){
  specs <- recorded_species(directory = directory, file = file.list[[f]])
  if (length(specs) != 0) {
    ind_file_used <- c(ind_file_used, f)
  }
}

# recordings not containing NFCs (17):

for(f in 1:length(file.list)){
  specs <- recorded_species(directory = directory, file = file.list[[f]])
  if (length(specs) == 0) {
    print(paste(f, ":", file.list[[f]]))}
}

# nights containing NFCs:

# manually compare date:
# whole night without NFC: file 20, 28, 38, 39, 40, 41, 59, 62, 66, 67, 68, 69, 70
# NFC in night but not in all corresponding recordings: file 48, 52, 54, 81
ind_file_used <- c(ind_file_used, 48, 52, 54, 81)

# Metadata dataframe:

## filename:
audio_metadata <- data.frame("filename" = gsub(".Table.1.selections.txt", "", file.list[ind_file_used]))

## recording date:
audio_metadata$date <- as.character(ymd(substr(audio_metadata$filename, 1, 6))) 
audio_metadata$date[75] <- as.character(ymd(180801))
audio_metadata$date[76] <- paste0(ymd(200904), ".2")
audio_metadata$date[77] <- paste0(ymd(200904), ".2")

## recording day and month:
audio_metadata$day_month <- format(ymd(audio_metadata$date), format = "%m-%d")

## recording location:
audio_metadata$location <- c("Spain", 
                             "Schauinsland (SW-Germany)", 
                             rep("Freiburg (SW-Germany)", 12), 
                             rep("Romania", 3), 
                             "Freiburg (SW-Germany)", 
                             "Mellum (N-Germany)", 
                             rep("Freiburg (SW-Germany)", 55), 
                             "Wildes Moor (N-Germany)", 
                             rep("Freiburg (SW-Germany)",2), 
                             rep("Freiburg (SW-Germany)", 4))


# annotated time spans of audio recording:
## directory of original selection tables:
directory_raw_sel_tbl <- "~/Master/Masterarbeit/Selection_tables"

## annotated time span 1:
audio_metadata$start1_s <- numeric(nrow(audio_metadata))
audio_metadata$end1_s <- numeric(nrow(audio_metadata))

## duration of sound files:

for(i in 1:nrow(audio_metadata)){
  
  audiofile <- paste0(audio_metadata$filename[i], ".wav")
  
  if(file.exists(paste0(audio_dir, audiofile))){
    
    af_metadt <- readWave(paste0(audio_dir, audiofile), header = T)
    
    duration_s <- round(af_metadt$samples / af_metadt$sample.rate)

    audio_metadata$end1_s[i] <- duration_s
  }
}

## audio recordings of which several time spans were annotated:
## which of selection tables with relevant species contain "Auswertung" or (Start der) "Aufnahme": #34
ind_not_fully_annotated <- NULL

for(f in 1:nrow(audio_metadata)){
  
  specs <- recorded_species(directory = directory_raw_sel_tbl, 
                            file = paste0(audio_metadata$filename[f], ".Table.1.selections.txt"))
  
  if (any(grepl(pattern = "Auswertung", x = specs, ignore.case = T)) | any(grepl(pattern = "Aufnahme", x = specs, ignore.case = T))) {
    ind_not_fully_annotated <- c(ind_not_fully_annotated, f)
  }
}

## check annotated time spans manually:
### annotated time span 2:
audio_metadata$start2_s <- numeric(nrow(audio_metadata))
audio_metadata$end2_s <- numeric(nrow(audio_metadata))
### annotated time span 3:
audio_metadata$start3_s <- numeric(nrow(audio_metadata))
audio_metadata$end3_s <- numeric(nrow(audio_metadata))

#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[1])))
audio_metadata$start1_s[1] <- 288
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[6])))
audio_metadata$end1_s[6] <- 8509
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[8])))
audio_metadata$end1_s[8] <- 7778
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[10])))
audio_metadata$end1_s[10] <- 8414
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[12])))
audio_metadata$end1_s[12] <- 9565
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[14])))
audio_metadata$end1_s[14] <- 5266
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[15])))
audio_metadata$start1_s[15] <- 345
audio_metadata$end1_s[15] <- 	547
audio_metadata$start2_s[15] <- 1624
audio_metadata$end2_s[15] <- 1679
audio_metadata$start3_s[15] <- 3653
audio_metadata$end3_s[15] <- 	3823
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[17])))
audio_metadata$end1_s[17] <- 	10776
audio_metadata$start2_s[17] <- 	11328
audio_metadata$end2_s[17] <- 	12109
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[19])))
audio_metadata$end1_s[19] <- 	526
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[21])))
audio_metadata$end1_s[21] <- 	2663
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[26])))
audio_metadata$end1_s[26] <- 6870
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[28])))
audio_metadata$end1_s[28] <- 	8013
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[30])))
audio_metadata$end1_s[30] <- 11311
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[31])))
audio_metadata$end1_s[31] <- 10320
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[33])))
audio_metadata$end1_s[33] <- 11700
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[35])))
audio_metadata$end1_s[35] <- 10794
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[38])))
audio_metadata$end1_s[38] <- 9814
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[39])))
audio_metadata$end1_s[39] <- 8547
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[41])))
audio_metadata$end1_s[41] <- 10615
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[42])))
audio_metadata$end1_s[42] <- 8207
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[44])))
audio_metadata$end1_s[44] <- 10690
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[45])))
audio_metadata$end1_s[45] <- 7912
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[46])))
audio_metadata$end1_s[46] <- 10742
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[48])))
audio_metadata$end1_s[48] <- 10348
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[54])))
audio_metadata$start1_s[54] <- 5554
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[57])))
audio_metadata$end1_s[57] <- 5174
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[67])))
audio_metadata$end1_s[67] <- 8036
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[68])))
audio_metadata$end1_s[68] <- 7167
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[69])))
audio_metadata$end1_s[69] <- 6951
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[70])))
audio_metadata$end1_s[70] <- 10184
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[71])))
audio_metadata$start1_s[71] <-	1162
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[72])))
audio_metadata$start1_s[72] <- 6850
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[73])))
audio_metadata$end1_s[73] <- 9857
#View(read.delim(paste0(directory_raw_sel_tbl, "/",audio_metadata$filename[74])))
audio_metadata$end1_s[74] <- 4145
audio_metadata$start2_s[74] <- 7832
audio_metadata$end2_s[74] <- 8638 

## total time annotated:
audio_metadata$time_annotated_s <- (audio_metadata$end1_s - audio_metadata$start1_s) + 
  (audio_metadata$end2_s - audio_metadata$start2_s) + 
  (audio_metadata$end3_s - audio_metadata$start3_s) 

## seconds with NFCs per recording:
audio_metadata$seconds_with_NFCs <- NA

for(f in 1:nrow(audio_metadata)){
  
  sel_tbl <- read.delim(paste0(directory, audio_metadata$filename[f], ".Table.1.selections.txt"))
  
  audio_metadata$seconds_with_NFCs[f] <- sum(ceiling(sel_tbl$End.Time..s.) - floor(sel_tbl$Begin.Time..s.))
}

## seconds with only background noise per recording:
audio_metadata$seconds_noise_only <- audio_metadata$time_annotated_s - audio_metadata$seconds_with_NFCs

# store metadata table:
save(audio_metadata, ind_file_used, file = "~/Master/Masterarbeit/Code/Code_final/audio_metadata.RData")


# Stackplot number of segments with NFCs / without NFCs per night: -------------------

# change to long format (for plotting):
audio_metadata_long <- gather(data = audio_metadata, 
                              be_ne, time_span_s, seconds_with_NFCs:seconds_noise_only, 
                              factor_key = T)

audio_metadata_long$be_ne <- factor(audio_metadata_long$be_ne, 
                                    levels = c("seconds_noise_only", "seconds_with_NFCs"))
audio_metadata_long$day_of_year <- yday(audio_metadata_long$date)
audio_metadata_long$day_month <- format(ymd(audio_metadata_long$date), format = "%m-%d")

p <- ggplot(data = audio_metadata_long, 
            aes(x = as.factor(paste(day_month, location)), y = time_span_s, fill = factor(be_ne))) +
  geom_bar(stat="identity") +
  scale_fill_viridis(discrete = T, begin = 0, end = 0.9, 
                     labels = c("noise", "bird calls"), name = "") +
  ylab("time [s]")  +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3), 
        legend.position = "top") 

pgg <- gg.gap(plot = p,
              segments = c(700,5000),
              tick_width = c(100,2000), 
              ylim = c(0, 33000))
pgg
add.legend(p, margin = c(top = 0, right = 200, bottom = 650, left = 200))




# Extract general information on data: -----------------------------------------------

# number of nights:
length(unique(audio_metadata$date))

# bird sound events and number of occurrences:
rec_bird_events <- recorded_species(directory = "~/Master/Masterarbeit/Selection_tables/Selection_tables_species_song_call_only" , count = F)
rec_bird_events_cnt <- recorded_species(directory = "~/Master/Masterarbeit/Selection_tables/Selection_tables_species_song_call_only" , count = T)
cnts <- as.numeric(gsub("[\\(\\)]", "", regmatches(rec_bird_events_cnt, gregexpr("\\(.*?\\)", rec_bird_events_cnt))))
be_occ_df <- data.frame("bird_event" = rec_bird_events, "occ" = cnts)
sum(be_occ_df$occ) 

# number of songs:
sum(be_occ_df$occ[which(grepl(pattern = "Gesang", x = be_occ_df$bird_event, ignore.case = T))])
# number of wing-flapping sounds:
sum(be_occ_df$occ[which(grepl(pattern = "Flügel", x = be_occ_df$bird_event, ignore.case = T))])
# number of corvid sounds:
sum(be_occ_df$occ[which(grepl(pattern = "Rabenkrähe", x = be_occ_df$bird_event, ignore.case = T))])
sum(be_occ_df$occ[which(grepl(pattern = "Saatkrähe", x = be_occ_df$bird_event, ignore.case = T))])
sum(be_occ_df$occ[which(grepl(pattern = "Elster", x = be_occ_df$bird_event, ignore.case = T))])

# number of NFCs:
rec_rel_bird_calls <- recorded_species(directory = "~/Master/Masterarbeit/Selection_tables/Selection_tables_species_flight_calls_only" , count = F)
rec_rel_bird_calls_cnt <- recorded_species(directory = "~/Master/Masterarbeit/Selection_tables/Selection_tables_species_flight_calls_only" , count = T)
cnts <- as.numeric(gsub("[\\(\\)]", "", regmatches(rec_rel_bird_calls_cnt, gregexpr("\\(.*?\\)", rec_rel_bird_calls_cnt))))
rel_be_df <- data.frame("bird_event" = rec_rel_bird_calls, "occ" = cnts)
sum(rel_be_df$occ) 

# total annotated time period:
time_conv_hr_min_s(sum(audio_metadata$time_annotated_s))

# annotated time period per night:
annotated_time_per_date <- audio_metadata %>% 
                              group_by(day_month) %>% 
                              summarise(time_annotated = sum(time_annotated_s))
time_conv_hr_min_s(range(annotated_time_per_date$time_annotated))
time_conv_hr_min_s(mean(annotated_time_per_date$time_annotated))
time_conv_hr_min_s(median(annotated_time_per_date$time_annotated))

# percentage of NFCs of recorded time:
sum(audio_metadata$seconds_with_NFCs) / sum(audio_metadata$time_annotated_s)


# Approximate frequency range of each species (based on annotated time-frequency boxes): ------

# combine all selection tables:
for(i in 1:length(file.list)){
  # read selection table:
  Sel_tbl <- read.delim(paste0(directory, file.list[[i]]))
  # add filename to data frame:
  Sel_tbl$audiofile <- rep(file.list[[i]], nrow(Sel_tbl))
  # rbind selection tables:
  if(i == 1){
    all_sel_tbls <- Sel_tbl
  }else{
    all_sel_tbls <- rbind(all_sel_tbls, Sel_tbl)
  }
}
levels(all_sel_tbls$Species)[6] <- "Krickente"

# add English species names and categories; # spec_group_occ from 6_Evaluation.R
all_sel_tbls <- merge(all_sel_tbls, spec_group_occ, by.x = "Species", by.y = "species") 

# change to long format:
data_long <- gather(all_sel_tbls, frequency_type, frequency_value, 
                    Low.Freq..Hz.:High.Freq..Hz., factor_key = T)

# facet grid labels:
cat.labs <- c("Passerines", "Waders", "Rails", "Ducks/G.", "Herons", "Other")
names(cat.labs) <- c("Passerine", "Wader", "Rail", "Duck/Goose", "Heron", "Other")

# Plot:
data_long %>% 
  filter(category != "Other") %>% 
  filter(n_soundclips > 10) %>% 
  ggplot(mapping = aes(x = English_n_soundclips, y = frequency_value/1000, colour = frequency_type)) +
  facet_grid(cols = vars(category), 
             scales = "free", 
             space = "free_x", 
             labeller = labeller(category = cat.labs)) +
  stat_boxplot(aes(y = frequency_value/1000, colour = frequency_type), 
               geom = "errorbar", 
               width = 0.2, 
               position = position_dodge(width = 0.7)) +
  geom_boxplot(width = 0.7, 
               lwd = 0.6) + 
  scale_color_manual(values = c("royalblue3", "red2"), 
                     name = "", 
                     labels = c("min. freq.", "max. freq.")) +
  ylab("Frequency [kHz]") + 
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
        strip.background = element_rect(fill = c("grey80"),colour = "black"),
        strip.text = element_text(size = 15)) +
  ylim(c(0, 11.025)) +
  scale_y_continuous(breaks = seq(0, 11.025, by = 2))


# Composition of the recorded NFCs regarding species categories per group: ----

# Function: proportion of bird species recorded per group:
## species_vec = considered species
## group vec = considered groups


bird_spec_prop <- function(species_vec, group_vec){
  
  ind_specs <- vector(mode = "list", length = length(species_vec))
  
  for(s in 1:length(species_vec)){
    
    ind_specs[[s]] <- which(grepl(pattern = species_vec[s], 
                                  df_sound_clips$species[which(df_sound_clips$group %in% group_vec & df_sound_clips$bird_event == 1)]))
  }
  
  n_specs <- length(unique(unlist(ind_specs)))
  
  return(n_specs/length(df_sound_clips$species[which(df_sound_clips$group %in% group_vec & df_sound_clips$bird_event == 1)]))
}


# proportion of each species category per group:
spec_comp_group <- data.frame("group" = 1:21, 
                              passerine_perc = NA, 
                              wader_perc = NA, 
                              rail_perc = NA,
                              duck_perc = NA,
                              heron_perc = NA,
                              other_perc = NA,
                              unknown_perc = NA)

# species of each category:
passerine_specs_ch <- as.character(spec_group_occ$species[which(spec_group_occ$category == "Passerine")])
wader_specs_ch <- as.character(spec_group_occ$species[which(spec_group_occ$category == "Wader")])
rail_specs_ch <- as.character(spec_group_occ$species[which(spec_group_occ$category == "Rail")])
duck_specs_ch <- as.character(spec_group_occ$species[which(spec_group_occ$category == "Duck/Goose")])
heron_specs_ch <- as.character(spec_group_occ$species[which(spec_group_occ$category == "Heron")])
other_specs_ch <- as.character(spec_group_occ$species[which(spec_group_occ$category == "Other")])


for(g in 1:21){
  spec_comp_group[g,2] <- bird_spec_prop(passerine_specs_ch, g)
  spec_comp_group[g,3] <- bird_spec_prop(wader_specs_ch, g)
  spec_comp_group[g,4] <- bird_spec_prop(rail_specs_ch, g)
  spec_comp_group[g,5] <- bird_spec_prop(duck_specs_ch, g)
  spec_comp_group[g,6] <- bird_spec_prop(heron_specs_ch, g)
  spec_comp_group[g,7] <- bird_spec_prop(other_specs_ch, g)
}
# unknown:
spec_comp_group[,8] <- 1-rowSums(spec_comp_group[,2:7])
spec_comp_group$unknown_perc[which(spec_comp_group$unknown_perc < 0)] <- 0

rowSums(spec_comp_group[,2:7]) # does not sum to 1 because either species could not be identified for some calls or because several calls of different species groups occured in the same sound clip

# merge month:
spec_comp_group2 <- merge(spec_comp_group, bes_per_night_loc[, c("group", "nights")])
spec_comp_group2$year_month <- format(as.Date(spec_comp_group2$nights), "%Y-%m")
spec_comp_group2$nights <- NULL
spec_comp_group2 <- spec_comp_group2[!duplicated(spec_comp_group2),]
spec_comp_group2 <- spec_comp_group2[-c(12, 15),]

# change to long format:
spec_comp_group_long <- gather(spec_comp_group2, 
                               key = "species_group", 
                               value = "percent", 
                               -c(group, year_month))

spec_comp_group_long$species_group <- factor(spec_comp_group_long$species_group, 
                                             levels = c("passerine_perc", "wader_perc", "rail_perc", "duck_perc",
                                                        "heron_perc", "other_perc", "unknown_perc"))

spec_comp_group_long$group <- factor(spec_comp_group_long$group)

# Plot (Fig. B1)
spec_comp_group_long %>% 
  ggplot(aes(x = group, y = percent*100)) +
  geom_col(aes(fill = species_group), width = 0.7) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 15, colour = "black"),
        legend.position = "right",
        text = element_text(size = 15),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1, size = 15, colour = "black")) +
  ylab("Share [%]") + 
  xlab("Recording year and month") +
  labs(fill = "Species group") +
  scale_fill_manual(values = c("#FDE725FF", "#7AD151FF", "#22A884FF", "#2A788EFF", "#414487FF", "#440154FF", "grey80"),labels = c("Passerines", "Waders", "Rails", "Ducks / Geese", "Herons", "Other", "Unknown")) +
  scale_x_discrete(labels = spec_comp_group_long$year_month) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 100, 25)) +
  geom_text(aes(x = group, label = group), y = 107, fontface = 1)




# Plots for thesis - methods and discussion: -----------------------------------------

# Effect of bw-filter: 

# audiofile:
rohrdommel_file <- "200404_09_23_30_Freiburg_BW_D_NM_RM_LS_36358"

# read sound clip:
s <- readWave(file = paste0(audio_dir, rohrdommel_file, ".wav"), 
              from = 7430, 
              to = 7440, 
              units = "seconds")

# preprocessing:
# first filter, then downsampling to avoid aliasing:
# lowpass filter:
s_filtered <- fir(s, f = 44100, to = 22050/2, output="Wave", channel = 1)
# downsample to 22050 Hz (e.g. Lasseck 2018):
s_22050Hz <- resamp(s_filtered, f = 44100, g = 22050, output = "Wave")
# butterworth filter (decrease noise):
s_bwfilter <- bwfilter(s_22050Hz, n = 2, from = 1000, output="Wave")

spec_filter<- spectro(s_bwfilter, 
                      channel = 1,
                      f = 22050, 
                      wl = 512, 
                      wn = "hanning", 
                      ovlp = 75,
                      fftw = T, 
                      plot = F,
                      norm = T,
                      fastdisp = F,
                      dB = "max0")

# no filter:
spec_nofilter <- spectro(s_22050Hz, 
                         channel = 1,
                         f = 22050, 
                         wl = 512, 
                         wn = "hanning", 
                         ovlp = 75,
                         fftw = T, 
                         plot = F,
                         norm = T,
                         fastdisp = F,
                         dB = "max0")


image(x=spec_nofilter$time, y=spec_nofilter$freq, z=t(spec_nofilter$amp),
      xlab="", ylab="",
      col= gray.colors(512, start = 0, end = 0.8, rev = T),
      las = 1,
      cex.axis = 1.4)
title(ylab="Frequency [kHz]", xlab="Time [s]", line=2, cex.lab=1.4)

image(x=spec_filter$time, y=spec_filter$freq, z=t(spec_filter$amp),
      xlab="", ylab="",
      col= gray.colors(512, start = 0, end = 1, rev = T),
      las = 1,
      cex.axis = 1.4)
title(ylab="Frequency [kHz]", xlab="Time [s]", line=2, cex.lab=1.4)

# png: cex = 1.4, width = 1000, height = 545
# pdf: cex = 1, size = A6, 4.13 x 7


# Maximum frequency spectrum:

# audiofile:
rotdrossel_file <- "190321_10_12_42_Freiburg_BW_D_NM_RM_LS_36311"

# read sound clip:
s <- readWave(file = paste0(audio_dir, rotdrossel_file, ".wav"), 
              from = 7400, 
              to = 7410, 
              units = "seconds")

# preceeding sound clip with background noise:
s_noise <- readWave(file = paste0(audio_dir, rotdrossel_file, ".wav"),
                    from = 7390, 
                    to = 7400, 
                    units = "seconds")

# preprocessing:
# first filter, then downsampling to avoid aliasing:
# lowpass filter:
s_filtered <- fir(s, f = 44100, to = 22050/2, output="Wave", channel = 1)
s_filtered_noise <- fir(s_noise, f = 44100, to = 22050/2, output="Wave", channel = 1)
# downsample to 22050 Hz (e.g. Lasseck 2018):
s_22050Hz <- resamp(s_filtered, f = 44100, g = 22050, output = "Wave")
s_22050Hz_noise <- resamp(s_filtered_noise, f = 44100, g = 22050, output = "Wave")
# butterworth filter (decrease noise):
s_bwfilter <- bwfilter(s_22050Hz, n = 2, from = 1000, output="Wave")
s_bwfilter_noise <- bwfilter(s_22050Hz_noise, n = 2, from = 1000, output="Wave")

# spectrogram:
spec_filter <- spectro(s_bwfilter, 
                      channel = 1,
                      f = 22050, 
                      wl = 512, 
                      wn = "hanning", 
                      ovlp = 75,
                      fftw = T, 
                      plot = F,
                      norm = T,
                      fastdisp = F,
                      dB = "max0")

image(x=spec_filter$time, y=spec_filter$freq, z=t(spec_filter$amp),
      xlab="", ylab="",
      col= gray.colors(512, start = 0, end = 1, rev = T),
      las = 1,
      cex.axis = 1.4)
title(ylab="Frequency [kHz]", xlab="Time [s]", line=2, cex.lab=1.4)

# maximum frequency spectrum:
max_freq_spec_RD <- meanspec(s_bwfilter, 
                             f = 22050, 
                             norm = T, 
                             fftw = T, 
                             dB = NULL, 
                             wn = 'hanning', 
                             wl = 512, 
                             ovlp = 75, 
                             plot = T, 
                             FUN = max,
                             type = "h",
                             alim = c(0,1.05),
                             yaxt="s",
                             alab = "",
                             flab = "",
                             cex.lab = 1,
                             cex.axis = 1,
                             xaxt = "n")
title(xlab="Frequency [kHz]", line=2, cex.lab=1)
title(ylab="Amplitude [no unit]", line=2.5, cex.lab=1)

max_freq_spec_RD_noise <- meanspec(s_bwfilter_noise, 
                                   f = 22050, 
                                   norm = T, 
                                   fftw = T, 
                                   dB = NULL, 
                                   wn = 'hanning', 
                                   wl = 512, 
                                   ovlp = 75, 
                                   plot = T, 
                                   FUN = max,
                                   type = "h",
                                   alim = c(0,1.05),
                                   yaxt="s",
                                   alab = "",
                                   flab = "",
                                   cex.lab = 1,
                                   cex.axis = 1)
title(xlab="Frequency [kHz]", line=2, cex.lab=1)
title(ylab="Amplitude [no unit]", line=2.5, cex.lab=1)



# Amplitude peaks in the corresponding maximum frequency spectrum for which both slopes were greater than 0.05: 

freq_bands <- seq(0,11,1)
n_peaks <- numeric(length(freq_bands)-1)

peaks <- fpeaks(max_freq_spec_RD, 
                f = 22050, 
                amp = c(0.05,0.05), 
                plot = T, 
                legend = F, 
                ylim = c(0,1.08),
                yaxt="s",
                xaxt = "n",
                ylab = "",
                xlab = "",
                cex.lab = 1,
                cex.axis = 1,
                title = F,
                labels = F,
                las = 1)
#title(xlab="Frequency [kHz]", line=2, cex.lab=1)
title(ylab="Amplitude [no unit]", line=2.5, cex.lab=1)
abline(v = seq(1,10,1), lwd = 1, col = "grey60", lty = 2)

text(x = 0.5, y = 1.06, "n = 1") 
text(x = 1.5, y = 1.06, "n = 2") 
text(x = 2.5, y = 1.06, "n = 1") 
text(x = 3.5, y = 1.06, "n = 0") 
text(x = 4.5, y = 1.06, "n = 0") 
text(x = 5.5, y = 1.06, "n = 0") 
text(x = 6.5, y = 1.06, "n = 1") 
text(x = 7.5, y = 1.06, "n = 0") 
text(x = 8.5, y = 1.06, "n = 0") 
text(x = 9.5, y = 1.06, "n = 0") 
text(x = 10.5, y = 1.06, "n = 0") 

# peaks noise:
peaks <- fpeaks(max_freq_spec_RD_noise, 
                f = 22050, 
                amp = c(0.05,0.05), 
                plot = T, 
                legend = F, 
                ylim = c(0,1.08),
                yaxt="s",
                ylab = "",
                xlab = "",
                cex.lab = 1,
                cex.axis = 1,
                title = F,
                labels = F,
                las = 1)
title(xlab="Frequency [kHz]", line=2, cex.lab=1)
title(ylab="Amplitude [no unit]", line=2.5, cex.lab=1)
abline(v = seq(1,10,1), lwd = 1, col = "grey60", lty = 2)

text(x = 0.5, y = 1.06, "n = 0") 
text(x = 1.5, y = 1.06, "n = 3") 
text(x = 2.5, y = 1.06, "n = 0") 
text(x = 3.5, y = 1.06, "n = 0") 
text(x = 4.5, y = 1.06, "n = 0") 
text(x = 5.5, y = 1.06, "n = 0") 
text(x = 6.5, y = 1.06, "n = 0") 
text(x = 7.5, y = 1.06, "n = 0") 
text(x = 8.5, y = 1.06, "n = 0") 
text(x = 9.5, y = 1.06, "n = 0") 
text(x = 10.5, y = 1.06, "n = 0") 


# Extraction features of the most prominent sound event in spectrogram:

k <- shapeKernel(c(6,6), type = "box")
k2 <- shapeKernel(c(10,10), type = "box")

# frequency and time resolution:
freq_res <- 0.04306641 # kHz
time_res <- 0.005820722 # s

# use normalised spectrogram:
sf_spec_bw <- spectro(s_bwfilter, 
                      channel = 1,
                      f = 22050, #22050 
                      wl = 512, 
                      wn = "hanning", 
                      ovlp = 75,
                      fftw = T, 
                      plot = F,
                      norm = T,
                      fastdisp = T,
                      dB = NULL)

# medians for clipping:
cm <- colMedians(sf_spec_bw$amp)
rm <- rowMedians(sf_spec_bw$amp)

# initialise binary matrices:
bin_mat <- matrix(0, nrow = nrow(sf_spec_bw$amp), ncol = ncol(sf_spec_bw$amp))
bin_mat2 <- matrix(0, nrow = nrow(sf_spec_bw$amp), ncol = ncol(sf_spec_bw$amp))

# rows above 3*rowmedian:
for(i in 1:nrow(sf_spec_bw$amp)){
  bin_mat[i,] <- ifelse(sf_spec_bw$amp[i,] > 3* rm[i],1,0)
}

# columns above 3*colmedian:
for(j in 1:ncol(sf_spec_bw$amp)){
  bin_mat2[,j] <- ifelse(sf_spec_bw$amp[,j] > 3* cm[j],1,0)
}

# add matrices: set to 1 where both matrices are 1, set to 0 otherwise
bin_mat3 <- bin_mat + bin_mat2
bin_mat4 <- bin_mat3
bin_mat4[bin_mat3 == 1] <- 0
bin_mat4[bin_mat3 == 2] <- 1

# Plot:
image(sf_spec_bw$time, 
      sf_spec_bw$freq,
      t(bin_mat4),
      xlab = "",
      ylab = "",
      col = c(rgb(1,1,1,alpha = 0), viridis_pal(begin = 0, end = 0.8, direction = -1)(2)),
      las = 1,
      yaxt = "n",
      xaxt = "n")
axis(side = 2, las = 1)
title(ylab="Frequency [kHz]", line=2, cex.lab=1)
text(x = 9.5, y = 10, "A", cex = 2)

# Lasseck 2015: closing - dilation - median filter:
bin_mat5 <- closing(bin_mat4, kernel = k) # removes small holes
bin_mat6 <- dilate(bin_mat5, kernel = k)
bin_mat7 <- medianFilter(bin_mat6, k2)

# convert to raster to find clusters of connected pixels of value 1:
Rmat <- raster(bin_mat7)
Clumps <- as.matrix(clump(Rmat, directions=8)) 

# find largest cluster:
largest_clump <- as.numeric(names(which.max(table(Clumps))))
Clumps2 <- Clumps
Clumps2[Clumps == largest_clump] <- 1
Clumps2[Clumps != largest_clump] <- 0

# Plot:
image(sf_spec_bw$time, 
      sf_spec_bw$freq,
      t(Clumps2),
      xlab = "",
      ylab = "",
      col = viridis_pal(begin = 0, end = 0.8, direction = -1)(2),
      las = 1)
axis(side = 2, las = 1)
title(ylab="Frequency [kHz]", xlab="Time [s]", line=2, cex.lab=1)
box(col = "black")
text(x = 9.5, y = 10, "B", cex = 2)

# Same for preceeding soundclip with background noise only: 

# use normalised spectrogram:
sf_spec_bw_noise <- spectro(s_bwfilter_noise, 
                            channel = 1,
                            f = 22050, #22050 
                            wl = 512, 
                            wn = "hanning", 
                            ovlp = 75,
                            fftw = T, 
                            plot = F,
                            norm = T,
                            fastdisp = T,
                            dB = NULL)

# medians for clipping:
cm <- colMedians(sf_spec_bw_noise$amp)
rm <- rowMedians(sf_spec_bw_noise$amp)

# initialise binary matrices:
bin_mat <- matrix(0, nrow = nrow(sf_spec_bw_noise$amp), ncol = ncol(sf_spec_bw_noise$amp))
bin_mat2 <- matrix(0, nrow = nrow(sf_spec_bw_noise$amp), ncol = ncol(sf_spec_bw_noise$amp))

# rows above 3*rowmedian:
for(i in 1:nrow(sf_spec_bw_noise$amp)){
  bin_mat[i,] <- ifelse(sf_spec_bw_noise$amp[i,] > 3* rm[i],1,0)
}

# columns above 3*colmedian:
for(j in 1:ncol(sf_spec_bw_noise$amp)){
  bin_mat2[,j] <- ifelse(sf_spec_bw_noise$amp[,j] > 3* cm[j],1,0)
}

# add matrices: set to 1 where both matrices are 1, set to 0 otherwise
bin_mat3 <- bin_mat + bin_mat2
bin_mat4 <- bin_mat3
bin_mat4[bin_mat3 == 1] <- 0
bin_mat4[bin_mat3 == 2] <- 1

# Plot:
image(sf_spec_bw_noise$time, 
      sf_spec_bw_noise$freq,
      t(bin_mat4),
      xlab = "",
      ylab = "",
      col = c(rgb(1,1,1,alpha = 0), viridis_pal(begin = 0, end = 0.8, direction = -1)(2)),
      las = 1,
      yaxt = "n",
      xaxt = "n")
axis(side = 2, las = 1)
title(ylab="Frequency [kHz]", line=2, cex.lab=1)
text(x = 9.5, y = 10, "C", cex = 2)

# Lasseck 2015: closing - dilation - median filter:

bin_mat5 <- closing(bin_mat4, kernel = k) # removes small holes
bin_mat6 <- dilate(bin_mat5, kernel = k)
bin_mat7 <- medianFilter(bin_mat6, k2)

# convert to raster to find clusters of connected pixels of value 1:
Rmat <- raster(bin_mat7)
Clumps <- as.matrix(clump(Rmat, directions=8)) 

# find largest cluster:
largest_clump <- as.numeric(names(which.max(table(Clumps))))
Clumps2 <- Clumps
Clumps2[Clumps == largest_clump] <- 1
Clumps2[Clumps != largest_clump] <- 0

# Plot:
image(sf_spec_bw_noise$time, 
      sf_spec_bw_noise$freq,
      t(Clumps2),
      xlab = "",
      ylab = "",
      col = viridis_pal(begin = 0, end = 0.8, direction = -1)(2),
      las = 1)
axis(side = 2, las = 1)
title(ylab="Frequency [kHz]", xlab="Time [s]", line=2, cex.lab=1)
text(x = 9.5, y = 10, "D", cex = 2)


# Spectrograms for discussion: 
## NFCs of songthrush, European robin, tree pipit and grey heron:

# Songthrush NFCs:

sound_file <- "181022_10_15_00_Schauinsland_FR_BW_D_RM_LS_75366" 
# read sound clip:
s <- readWave(file = paste0(audio_dir, sound_file, ".wav"), 
              from = 10662, 
              to = 10664, 
              units = "seconds")
# preprocessing:
# first filter, then downsampling to avoid aliasing:
# lowpass filter:
s_filtered <- fir(s, f = 44100, to = 22050/2, output = "Wave", channel = 1)
# downsample to 22050 Hz (e.g. Lasseck 2018):
s_22050Hz <- resamp(s_filtered, f = 44100, g = 22050, output = "Wave")
# butterworth filter (decrease noise):
s_bwfilter <- bwfilter(s_22050Hz, n = 2, from = 1000, output = "Wave")
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
                      fastdisp = F,
                      dB = "max0")

image(x=spec_filter$time, y=spec_filter$freq, z=t(spec_filter$amp),
      xlab="", ylab="",
      col= gray.colors(512, start = 0.3, end = 1, rev = T),
      las = 1,
      cex.axis = 2)
title(ylab="Frequency [kHz]", xlab="Time [s]", line=3, cex.lab=2)
box(col = "black") # border
# rectangles:
rect(xleft = 0.2, 
     ybottom = 7.3, 
     xright = 0.4, 
     ytop = 9,
     col = NA, border = "red3")
rect(xleft = 1.65, 
     ybottom = 6.5, 
     xright = 1.85, 
     ytop = 9.5,
     col = NA, border = "red3")
text(x = 1, y = 10.5, "Song thrush", cex = 2)


# European robin NFC:

sound_file <- "190321_10_12_42_Freiburg_BW_D_NM_RM_LS_36311"
# read sound clip:
s <- readWave(file = paste0(audio_dir, sound_file, ".wav"), 
              from = 10327, 
              to = 10329, 
              units = "seconds")
# preprocessing:
# first filter, then downsampling to avoid aliasing:
# lowpass filter:
s_filtered <- fir(s, f = 44100, to = 22050/2, output = "Wave", channel = 1)
# downsample to 22050 Hz (e.g. Lasseck 2018):
s_22050Hz <- resamp(s_filtered, f = 44100, g = 22050, output = "Wave")
# butterworth filter (decrease noise):
s_bwfilter <- bwfilter(s_22050Hz, n = 2, from = 1000, output = "Wave")
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
                      fastdisp = F,
                      dB = "max0")

image(x=spec_filter$time, y=spec_filter$freq, z=t(spec_filter$amp),
      xlab="", ylab="",
      col= gray.colors(512, start = 0, end = 1, rev = T),
      las = 1,
      cex.axis = 2)
title(ylab="Frequency [kHz]", xlab="Time [s]", line=3, cex.lab=2)
box(col = "black") # border
# rectangles:
rect(xleft = 1.05, 
     ybottom = 5.6, 
     xright = 1.35, 
     ytop = 8.6,
     col = NA, border = "red3")
text(x = 1, y = 10.5, "European robin", cex = 2)


# Tree pipit NFC:
sound_file <- "190905_11_42_40_Romania_NM_RM_LS_35968"
# read sound clip:
s <- readWave(file = paste0(audio_dir, sound_file, ".wav"), 
              from = 7837, 
              to = 7839, 
              units = "seconds")
# preprocessing:
# first filter, then downsampling to avoid aliasing:
# lowpass filter:
s_filtered <- fir(s, f = 44100, to = 22050/2, output = "Wave", channel = 1)
# downsample to 22050 Hz (e.g. Lasseck 2018):
s_22050Hz <- resamp(s_filtered, f = 44100, g = 22050, output = "Wave")
# butterworth filter (decrease noise):
s_bwfilter <- bwfilter(s_22050Hz, n = 2, from = 1000, output = "Wave")
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
                      fastdisp = F,
                      dB = "max0")

image(x=spec_filter$time, y=spec_filter$freq, z=t(spec_filter$amp),
      xlab="", ylab="",
      col= gray.colors(512, start = 0.4, end = 1, rev = T),
      las = 1,
      cex.axis = 2)
title(ylab="Frequency [kHz]", xlab="Time [s]", line=3, cex.lab=2)
box(col = "black") # border
# rectangles:
rect(xleft = 1.2, 
     ybottom = 4.5, 
     xright = 1.5, 
     ytop = 7.1,
     col = NA, border = "red3")
text(x = 1, y = 10.5, "Tree pipit", cex = 2)

# Grey heron NFC:

sound_file <- "200428_06_00_00_Freiburg_BW_D_NM_RM_LS_36425"
# read sound clip:
s <- readWave(file = paste0(audio_dir, sound_file, ".wav"), 
              from = 4065, 
              to = 4067, 
              units = "seconds")
# preprocessing:
# first filter, then downsampling to avoid aliasing:
# lowpass filter:
s_filtered <- fir(s, f = 44100, to = 22050/2, output = "Wave", channel = 1)
# downsample to 22050 Hz (e.g. Lasseck 2018):
s_22050Hz <- resamp(s_filtered, f = 44100, g = 22050, output = "Wave")
# butterworth filter (decrease noise):
s_bwfilter <- bwfilter(s_22050Hz, n = 2, from = 1000, output = "Wave")
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
                      fastdisp = F,
                      dB = "max0")

image(x=spec_filter$time, y=spec_filter$freq, z=t(spec_filter$amp),
      xlab="", ylab="",
      col= gray.colors(512, start = 0, end = 1, rev = T),
      las = 1,
      cex.axis = 2)
title(ylab="Frequency [kHz]", xlab="Time [s]", line=3, cex.lab=2)
box(col = "black") # border
# rectangles:
rect(xleft = 0.7, 
     ybottom = 0.7, 
     xright = 1.7, 
     ytop = 10,
     col = NA, border = "red3")
text(x = 1, y = 10.5, "Grey heron", cex = 2)