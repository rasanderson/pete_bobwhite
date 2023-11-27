# MFCC analysis of clipped calls
# This is a development package https://github.com/DenaJGibbon/behaviouR
library(behaviouR)
library(ggplot2)
library(ggfortify)
library(tuneR)
library(fftw)

# On Ubuntu 20.04. Probably different on Windoze
setWavPlayer("/usr/bin/aplay")

# Create a list of all .WAV files in the subfolders
wav_files <- list.files(path = "calls", recursive = TRUE, pattern = "\\.wav$", full.names = TRUE)

# Read in all .WAV files into a list
wav_list <- lapply(wav_files, readWave)

# Play and visualise spectrogram of one sound
sound_no <- 621
SpectrogramSingle(wav_files[sound_no], Colors = "Colors", min.freq = 1000, max.freq = 4000)
play(wav_list[[sound_no]])

# Filter out just sounds between 1500 and 3000 Hz
# Perform FFT
myWave <- wav_list[[sound_no]]
myFFT <- fft(myWave@left)

# Get the frequencies
freqs <- seq(0, myWave@samp.rate/2, length.out=length(myFFT)/2+1)

# Zero out the frequencies between 1500 and 3000 Hz
myFFT[freqs >= 1500 & freqs <= 3000] <- 0

# Perform inverse FFT
filtered <- Re(fft(myFFT, inverse=TRUE))

# Create a new Wave object with the filtered data
filteredWave <- Wave(filtered, samp.rate=myWave@samp.rate)

# Normalize the data to the required range
filteredWave <- normalize(filteredWave, "16")
play(normalize(myWave, "16"))
play(filteredWave)

# Write the result to a new wave file. Compare sith normalized version of original
writeWave(filteredWave, "filtered.wav")
writeWave(normalize(myWave, "16"), "original.wav")
SpectrogramSingle("original.wav", Colors = "Colors", min.freq = 1000, max.freq = 4000)
SpectrogramSingle("filtered.wav", Colors = "Colors", min.freq = 1000, max.freq = 4000)


wav_folders <- list.files(path = "calls")
mfcc_list <- list()
for(site in wav_folders){
  print(site)
  mfcc_list[[site]] <- MFCCFunction(input.dir = paste0("calls/", site))
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
