optim _species <- function (speciesVsitesMatrix, n.plt=250, start=”fixed”, plot=FALSE, richness=TRUE, RRR=TRUE, CWE=TRUE, shannon=TRUE, simpson=TRUE, simpson_beta=TRUE){
  if(richness){
    speciesVsitesMatrix_binary <- speciesVsitesMatrix
    speciesVsitesMatrix_binary[speciesVsitesMatrix_binary > 0] <- 1 #convert abundances to presences
  } #end binary conversion
  if(RRR){
    speciesVsitesMatrix_binary <- speciesVsitesMatrix
    speciesVsitesMatrix_binary[speciesVsitesMatrix_binary > 0] <- 1 #convert abundances to presences
  } #end binary conversion
  if(CWE){
    speciesVsitesMatrix_binary <- speciesVsitesMatrix
    speciesVsitesMatrix_binary[speciesVsitesMatrix_binary > 0] <- 1 #convert abundances to presences
  } #end binary conversion
  if(simpson_beta){
    speciesVsitesMatrix_binary <- speciesVsitesMatrix
    speciesVsitesMatrix_binary[speciesVsitesMatrix_binary > 0] <- 1 #convert abundances to presences
  } #end binary conversion
  
}


########################
Richness.opt <- function(speciesVsitesMatrix_binary, n.plt) {
  Richness <- rowSums(speciesVsitesMatrix_binary) #simple species richness (sum) per plot
  RichnessSort <- rev(sort(Richness))[1:n.plt] #inverse of the sort function to get decreasing order
  RichnessMCP <- RichnessSort[1:n.plt] #get top n.plt plots based on Richness
  RichnessMCPaccum <- specaccum(speciesVsitesMatrix_binary[names(RichnessMCP),], method="collector") #get a species accumulation curve for these selected plots
  plot (RichnessMCPaccum, col="magenta", lwd=2,  main="Richness optimiser", xlab = "Number of plots", ylab = "Cumulative species")
  return(RichnessMCPaccum)
}  

Richness.opt(speciesVsitesMatrix_binary) #check

########################
RRR.opt <- function(speciesVsitesMatrix_binary, n.plt) {
  RRR <- rowSums(speciesVsitesMatrix_binary/colSums(speciesVsitesMatrix_binary)) #presence/absence matrix with presences divided by frequency of that species
  RRRSort <- rev(sort(RRR)) #inverse of the sort function to get decreasing order
  RRRMCP <- RRRSort[1:n.plt] #get top n.plt plots based on RRR
  RRRMCPaccum <- specaccum(speciesVsitesMatrix_binary[names(RRRMCP),], method="collector") #get a species accumulation curve for these selected plots
  plot(RRRMCPaccum, col="cyan", lwd=2, main="RRR optimiser", xlab = "Number of plots", ylab = "Cumulative species")
  return(RRRMCPaccum)
}

RRR.opt(speciesVsitesMatrix_binary, n.plt) #check

########################
CWE.opt <- function(speciesVsitesMatrix_binary, n.plt) {
  CWE <- rowSums(speciesVsitesMatrix_binary/colSums(speciesVsitesMatrix_binary))/rowSums(speciesVsitesMatrix_binary) #It's RRR divided by richness
  CWESort <- rev(sort(CWE)) #inverse of the sort function to get decreasing order
  CWEMCP <- CWESort[1:n.plt] #get top n.plt plots based on CWE
  CWEMCPaccum <- specaccum(PAdataGood[names(CWEMCP),], method="collector") #get a species accumulation curve for these selected plots
  plot(CWEMCPaccum, col="gold", lwd=2,main="CWE optimiser", xlab = "Number of plots", ylab = "Cumulative species")
  return(CWEMCPaccum)
}

CWE.opt(speciesVsitesMatrix_binary, n.plt) #check

########################
Shannon.opt <- function(speciesVsitesMatrix, n.plt) {
  Shannon <- diversity(speciesVsitesMatrix, index = "shannon") #Normal Shannon diversity index using vegan package
  ShannonSort <- rev(sort(Shannon)) #inverse of the sort function to get decreasing order
  ShannonMCP <- ShannonSort[1:n.plt] #get top n.plt plots based on Shannon-Wienner diversity index
  ShannonMCPaccum <- specaccum(speciesVsitesMatrix[names(ShannonMCP),], method="collector") #get a species accumulation curve for these selected plots
  plot(ShannonMCPaccum, col="mediumseagreen", lwd=2, main="Sannon-Wienner optimiser", xlab = "Number of plots", ylab = "Cumulative species")
  return(ShannonMCPaccum)
}

Shannon.opt(speciesVsitesMatrix) #check

########################
Simpson.opt <- function(speciesVsitesMatrix, n.plt) {
  Simpson <- diversity(speciesVsitesMatrix, index = "simpson") #Normal Simpson diversity index using vegan package
  SimpsonSort <- rev(sort(Simpson)) #inverse of the sort function to get decreasing order
  SimpsonMCP <- SimpsonSort[1:n.plt] #get top n.plt plots based on Shannon-Wienner diversity index
  SimpsonMCPaccum <- specaccum(speciesVsitesMatrix[names(SimpsonMCP),], method="collector") #get a species accumulation curve for these selected plots
  plot(SimpsonMCPaccum, col="purple", lwd=2, main="Gini-Simpson optimiser", xlab = "Number of plots", ylab = "Cumulative species")
  return(SimpsonMCPaccum)
}

Simpson.opt(speciesVsitesMatrix, n.plt) #check

########################

simpson_beta.opt <- function(speciesVsitesMatrix_binary, n.plt, start) {
  richest.plot<- which.max(rowSums(speciesVsitesMatrix_binary))
  if (start =="fixed"){
    start.plot<- speciesVsitesMatrix_binary[richest.plot,] #fixed seed: this is the richest plot
  }
  if (start=="random"){
    start.plot <- speciesVsitesMatrix_binary[sample(nrow(speciesVsitesMatrix_binary), 1), ] #get a random seed plot
  }
    result <- list() 
  n <- 1
  result[n] <- row.names(start.plot)
  for(i in 1:(n.plt-1)) {
    n <- n + 1 
    simpson <- as.data.frame(as.matrix(beta.pair(speciesVsitesMatrix_binary)$beta.sim)) #simpson beta diversity between all pairs (excludes species nestedness)
    sort.diss <- rev(sort(simpson[row.names(simpson) %in% row.names(start.plot),])) # creates vector of dissimilarity comparisons
    next.plot.name <- names(sort.diss[1]) #selected the first plot in the vector, which after sorting is the most dissimilar
    result[n] <- next.plot.name #add it to the list of plots to save
    cat(next.plot.name, " ", sort.diss[[1]], "\n") #Outputs the objects, concatenating the representations in a chain or series
    start.plot <- start.plot + speciesVsitesMatrix_binary[next.plot.name,] #Merge the seed plot with the first selected plot and get all occurrences into one virtual plot
    start.plot[start.plot>0] <- 1 #set all values to 1 so it is PA data
    speciesVsitesMatrix_binary[row.names(speciesVsitesMatrix_binary) %in% row.names(start.plot),] <- start.plot 
  }
  dissimilarplots <- unlist(result)
  return(dissimilarplots)
}

simpson_beta.opt(speciesVsitesMatrix_binary, n.plt) #check

########################

Frequent_simpson_beta.opt<- simpson_beta.opt(speciesVsitesMatrix_binary, n.plt, start = "random", iterations = 1000, freq = TRUE){
   if (iterations == TRUE){
    opt.runs <- list()
    n <- 0
    for(i in 1:iterations){
      n <- n + 1
      opt.run <- simpson_beta.opt(speciesVsitesMatrix_binary, n.plt, start = "random")
      opt.runs.freq[[n]]<-opt.run
      cat("Rep ", n, "\n")
      Sppaccum_freq[[n]]<- specaccum(speciesVsitesMatrix_binary[sample(nrow(speciesVsitesMatrix_binary), n.plt),], method="collector")
      }  
    combined.rand_specaccum <- Sppaccum_freq[[1]] #copy one for format
    combined_matrix_rand <- do.call(rbind, lapply(Sppaccum_freq, function(x) {return(x$richness)})) #compile the  cumulative richness results from reps above into a matrix
    combined.rand_specaccum$richness <- apply(combined_matrix_rand, 2, mean) #using the matrix, calculate the mean for each additional plot, and add that to the richness slot inthe specaccum object
    combined.rand_specaccum$sd <- apply(combined_matrix_rand, 2, sd) #same for standard deviation
    combined.rand_specaccum$method <- "random" #assign it as random not collector so it plots correctly
    plot(combined.rand_specaccum, bty="l", las=1, col="beige", lwd=2, main="Random seed for Simpson dissimilarity optimisation", xlab = "Number of plots", ylab = "Cumulative species")
    return(combined.rand_specaccum) #mean line with bars for sd
   
    if (freq == TRUE){
    freq_list <- unlist(opt.runs.freq)
    freq_table = table(freq_list)
    freq_dataframe = as.data.frame(freq_table)
    sorted_freq_plots<-freq_dataframe[order(freq_dataframe$Freq, decreasing=T),]
    top_freq_plots<-sorted_freq_plots %>% dplyr::slice(1:n.plt) # most frequent plots
    top_freq_plots_PAdata<-speciesVsitesMatrix_binary[top_freq_plots$freq_list,] #subset the most frequent plots 
    SPPaccum_freq<- specaccum(top_freq_plots_PAdata, method="collector") 
    plot(SPPaccum_freq, lwd=2, add= T, col="blue")
    return(top_freq_plots)
    }
}  
  
Frequent_simpson_beta.opt(speciesVsitesMatrix, n.plt, start = "random", iterations = 1000, freq = TRUE) #check  
 