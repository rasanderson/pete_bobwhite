# MFCC analysis of clipped calls
# This is a development package https://github.com/DenaJGibbon/behaviouR
library(behaviouR)
library(ggplot2)
library(ggfortify)
library(tuneR)
library(fftw)
library(seewave)
library(signal)

# On Ubuntu 20.04. Probably different on Windoze
setWavPlayer("/usr/bin/aplay")

# Create a list of all .WAV files in the subfolders
wav_files <- list.files(path = "calls", recursive = TRUE, pattern = "\\.wav$", full.names = TRUE)

# Read in all .WAV files into a list
wav_list <- lapply(wav_files, readWave)

# # Play and visualise spectrogram of one sound
# sound_no <- 621 # This recording has good BobWhite call in it
# SpectrogramSingle(wav_files[sound_no], Colors = "Colors", min.freq = 1000, max.freq = 4000)
# play(wav_list[[sound_no]])
# 
# # Filter out just sounds between 1500 and 3000 Hz
# myWave <- wav_list[[sound_no]]
# lower <- 1500
# upper <- 3000
# filter_order <- 3
# lp <- bwfilter(myWave, f = myWave@samp.rate, n = filter_order,
#                from = lower, to = upper, output = "Wave")
# spectro(lp)

# Before pushing through MFCC, filter out the 1500 to 3000 where BobWhites call
# Create names of filtered files
filt_files <- sub("calls", "filtered", wav_files)

# Create the new folders to receive the filtered data
# Extract the unique directory names from the new_filenames vector
dirs <- unique(dirname(filt_files))

# Create each directory
for (dir in dirs) {
  # Check if the directory already exists
  if (!dir.exists(dir)) {
    # Create the directory
    dir.create(dir, recursive = TRUE)
    print(paste("Directory created: ", dir))
  } else {
    print(paste("Directory already exists: ", dir))
  }
}

# Apply bandpass filter and write files
lower <- 1500
upper <- 3000
filter_order <- 3
for(call in 1:length(wav_list)){
  myWave <- wav_list[[call]]
  filtered_call <- bwfilter(myWave, f = myWave@samp.rate, n = filter_order,
                            from = lower, to = upper, output = "Wave")
  writeWave(filtered_call, filt_files[call])
}

wav_folders <- list.files(path = "filtered")
mfcc_list <- list()
for(site in wav_folders){
  print(site)
  mfcc_list[[site]] <- MFCCFunction(input.dir = paste0("filtered/", site))
}
# Merge into a dataframe
mfcc_df <- data.frame()
for(site in 1:length(wav_folders)){
  this_site <- data.frame(mfcc_list[[site]])
  site_name <- rep(wav_folders[[site]], nrow(this_site))
  this_site$Class <- site_name
  mfcc_df <- rbind(mfcc_df, this_site)
}

saveRDS(mfcc_df, "mfcc_df.RDS")

# min_row <- 1
# max_row <- 573 # max is nrow(mfcc_df)
# pca_res <- prcomp(mfcc_df[min_row:max_row, -c(1)], scale. = TRUE)
# ggplot2::autoplot(pca_res, data = mfcc_df[min_row:max_row,], colour = "Class")
# min_row <- 574
# max_row <- nrow(mfcc_df) # max is nrow(mfcc_df)
# pca_res <- prcomp(mfcc_df[min_row:max_row, -c(1)], scale. = TRUE)
# ggplot2::autoplot(pca_res, data = mfcc_df[min_row:max_row,], colour = "Class")
# 
# # Vegan equivalent
# pca_res_vgn <- vegan::rda(mfcc_df[min_row:max_row, -c(1)], scale = TRUE)
# plot(pca_res_vgn, display="sites")
# 
# library(Rtsne)
# mfcc_tsne <- Rtsne(mfcc_df[1:max_row, -1], perplexity = 10)
# mfcc_tsne_df <- data.frame(tSNE1 = mfcc_tsne$Y[,1],
#                            tSNE2 = mfcc_tsne$Y[,2],
#                            site_name = mfcc_df[1:max_row,1])
# ggplot(mfcc_tsne_df, aes(tSNE1, tSNE2, colour = site_name)) +
#   geom_point()
