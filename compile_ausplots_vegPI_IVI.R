#Following 'compile_ausplots_vegPI.R' and , 'compile_ausplots_vegPI_FREQ.R' this script generates a species~sites occurrence matrix with scores representing IVI (Importance Value Index), calculated as the sum of percent cover and percent frequency in within-plot transects, and writes it to file. 
#
#Authors:
#Greg Guerin

#setwd()

freq_matrix_percents <- freq_matrix*100 #freq_matrix defined in 'compile_ausplots_vegPI_FREQ.R'
which(!colnames(freq_matrix_percents) %in% colnames(cover_matrix)) #all species names the same
IVI <- freq_matrix_percents + cover_matrix #can also calculate mean to get a 0-100% value

#write to file
write.csv(freq_matrix_percents, file="sitesVspecies_freq_transects_%.txt")
write.csv(IVI, file="IVI_%freqPLUS%cover.txt")
