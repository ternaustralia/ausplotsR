#Following 'compile_ausplots_vegPI.R' and , 'compile_ausplots_vegPI_FREQ.R' this script generates a species~sites occurrence matrix with scores representing IVI (Importance Value Index), calculated as the sum of percent cover and percent frequency in within-plot transects, and writes it to file. 
#
#Authors:
#Greg Guerin

freq_matrix_percents <- freq_matrix*100 #convert to %; freq_matrix defined in 'compile_ausplots_vegPI_FREQ.R'
#the cover and frequency matrices may have different numbers of species, so need to trim to match
freq_matrix_percents <- freq_matrix_percents[,-which(!colnames(freq_matrix_percents) %in% colnames(cover_matrix))] 
IVI <- cover_matrix[,-which(!colnames(cover_matrix) %in% colnames(freq_matrix_percents))]
IVI <- (IVI + freq_matrix_percents)/2 

#write to file
write.csv(freq_matrix_percents, file="sitesVspecies_freq_transects_%.txt")
write.csv(IVI, file="IVI_%freqPLUS%cover.txt")
