optim_species <- function(speciesVsitesMatrix, n.plt=250, start="fixed", plot_name=NULL, plot=TRUE, richness=TRUE, RRR=TRUE, CWE=TRUE, shannon=TRUE, simpson=TRUE, simpson_beta=TRUE, frequent=TRUE, random=TRUE, iterations=10) {
	
############################
#check inputs
  #NB (GRG) this would be better as guidance in the help/Rd file as site names as the first column is non-standard and doesn't match the species table from ausplotsR nor the matrices in the dune and mite example datasets.
  #rownames(speciesVsitesMatrix) <- speciesVsitesMatrix[,1]
  #speciesVsitesMatrix <- speciesVsitesMatrix[,-1] #remove the ffirst column
  
  if(!(start %in% c("fixed", "defined", "random"))) {stop("Argument 'start' must be character and one of: 'fixed', 'defined' or 'random'")}
  
  if(!is.null(plot_name) && (!plot_name %in% rownames(speciesVsitesMatrix))) {stop("Selected plot_name must match a site/row name in the species~sites data.")}
  
	if(n.plt > nrow(speciesVsitesMatrix)) {
		cat("You are attempting to optimise more sites that exist in the dataset - trimming to maximum. \n")
		n.plt <- nrow(speciesVsitesMatrix)
	}
  
	if(any(c(random, richness, RRR, CWE, simpson_beta, frequent))) {
	  speciesVsitesMatrix_binary <- speciesVsitesMatrix
		speciesVsitesMatrix_binary[speciesVsitesMatrix_binary > 0] <- 1 #convert abundances to presences
	}
  
########################
#calls

	result <- list()
	
	if(richness){
    result$Richness <- Richness.opt(speciesVsitesMatrix_binary, n.plt) #
  } #end if richness
  
  if(RRR){
    result$RRR <- RRR.opt(speciesVsitesMatrix_binary, n.plt) #
  } #end if RRR
  
  if(CWE){
    result$CWE <- CWE.opt(speciesVsitesMatrix_binary, n.plt) #
  } #end if CWE
  
  if(simpson_beta){
    result$SimpsonBeta <- simpson_beta.opt(speciesVsitesMatrix_binary, n.plt, start=start, plot_name) #
  } #end if simpson beta
  
  if(shannon) {
  	result$Shannon <- Shannon.opt(speciesVsitesMatrix, n.plt) #
  } #end if shannon
  
  if(simpson) {
  	result$Simpson <- Simpson.opt(speciesVsitesMatrix, n.plt) #
  } #end if simpson
   
  if(frequent) {
  	hold <- Frequent_simpson_beta.opt(speciesVsitesMatrix_binary, n.plt, iterations) 
    result$Frequent <- hold$Freq
    result$SimpsonBeta_randSeed <- hold$simspon_rand
  } #end if freqent plots
  
   if(random) {
  	result$Random <- Random.opt(speciesVsitesMatrix_binary, n.plt, iterations)
  } #end if random
  
  
  ##########################
  #wrap up:
	
  if(plot) {
  	plot.opt(result)
  	} #end if plot
    
  return(result)
  
} #end function


########################
Richness.opt <- function(speciesVsitesMatrix_binary, n.plt) {
  Richness <- rowSums(speciesVsitesMatrix_binary) #simple species richness (sum) per plot
  RichnessSort <- rev(sort(Richness))[1:n.plt] #inverse of the sort function to get decreasing order
  RichnessMCP <- RichnessSort[1:n.plt] #get top n.plt plots based on Richness
  RichnessMCPaccum <- specaccum(speciesVsitesMatrix_binary[names(RichnessMCP),], method="collector") #get a species accumulation curve for these selected plots
   return(RichnessMCPaccum)
}  


########################
RRR.opt <- function(speciesVsitesMatrix_binary, n.plt) {
  RRR <- rowSums(speciesVsitesMatrix_binary/colSums(speciesVsitesMatrix_binary)) #presence/absence matrix with presences divided by frequency of that species
  RRRSort <- rev(sort(RRR)) #inverse of the sort function to get decreasing order
  RRRMCP <- RRRSort[1:n.plt] #get top n.plt plots based on RRR
  RRRMCPaccum <- specaccum(speciesVsitesMatrix_binary[names(RRRMCP),], method="collector") #get a species accumulation curve for these selected plots
  return(RRRMCPaccum)
}

########################
CWE.opt <- function(speciesVsitesMatrix_binary, n.plt) {
  CWE <- rowSums(speciesVsitesMatrix_binary/colSums(speciesVsitesMatrix_binary))/rowSums(speciesVsitesMatrix_binary) #It's RRR divided by richness
  CWESort <- rev(sort(CWE)) #inverse of the sort function to get decreasing order
  CWEMCP <- CWESort[1:n.plt] #get top n.plt plots based on CWE
  CWEMCPaccum <- specaccum(speciesVsitesMatrix_binary[names(CWEMCP),], method="collector") #get a species accumulation curve for these selected plots
  #plot(CWEMCPaccum, col="gold", lwd=2,main="CWE optimiser", xlab = "Number of plots", ylab = "Cumulative species")
  return(CWEMCPaccum)
}


########################
Shannon.opt <- function(speciesVsitesMatrix, n.plt) {
  Shannon <- diversity(speciesVsitesMatrix, index = "shannon") #Normal Shannon diversity index using vegan package
  ShannonSort <- rev(sort(Shannon)) #inverse of the sort function to get decreasing order
  ShannonMCP <- ShannonSort[1:n.plt] #get top n.plt plots based on Shannon-Wienner diversity index
  ShannonMCPaccum <- specaccum(speciesVsitesMatrix[names(ShannonMCP),], method="collector") #get a species accumulation curve for these selected plots
  return(ShannonMCPaccum)
}


########################
Simpson.opt <- function(speciesVsitesMatrix, n.plt) {
  Simpson <- diversity(speciesVsitesMatrix, index = "simpson") #Normal Simpson diversity index using vegan package
  SimpsonSort <- rev(sort(Simpson)) #inverse of the sort function to get decreasing order
  SimpsonMCP <- SimpsonSort[1:n.plt] #get top n.plt plots based on Shannon-Wienner diversity index
  SimpsonMCPaccum <- specaccum(speciesVsitesMatrix[names(SimpsonMCP),], method="collector") #get a species accumulation curve for these selected plots
  return(SimpsonMCPaccum)
}


########################
simpson_beta.opt <- function(speciesVsitesMatrix_binary, n.plt, start, plot_name) { #I added plot_name in case the user wants to define a fixed seed which does not correspond to the richest site
	 original_matrix <- speciesVsitesMatrix_binary
  if (start == "fixed"){
  	start.plot <- rownames(speciesVsitesMatrix_binary)[which.max(rowSums(speciesVsitesMatrix_binary))] #fixed seed: this is the richest plot
  } #end if fixed
	if (start == "defined") {
	  start.plot <- plot_name #defined seed: this a specific plot chose by the user
	} #end if defined
	if (start == "random") {
    start.plot <- sample(rownames(speciesVsitesMatrix_binary), 1) #get a random seed plot
  }
  result <- list() 
  n <- 1
  result[n] <- start.plot
  for(i in 1:(n.plt-1)) {
    n <- n + 1 
    simpson <- as.data.frame(as.matrix(beta.pair(speciesVsitesMatrix_binary)$beta.sim)) #simpson beta diversity between all pairs (excludes species nestedness)
    sort.diss <- rev(sort(simpson[start.plot,])) #creates single row data frame holding the vector of dissimilarity comparisons to the seed/start.plot
    equal_plots <- length(which(sort.diss == max(sort.diss)))
    if(equal_plots == 1) {
      next.plot.name <- names(sort.diss)[1] #select the first plot in the vector, which after sorting is the most dissimilar
    }
    if(equal_plots > 1) {
      next.plot.name <- sample(names(sort.diss[,1:equal_plots])[(!names(sort.diss[,1:equal_plots]) %in% start.plot)],1) #select the first plot in the vector, which after sorting is the most dissimilar
    }
    #if(next.plot.name == start.plot) {next.plot.name <- names(sort.diss)[2]}
    result[n] <- next.plot.name #add it to the list of plots to save
    cat(next.plot.name, " ", sort.diss[,1], "\n") #Print out the chosen plot and its dissimilarity score
    speciesVsitesMatrix_binary[start.plot,] <- speciesVsitesMatrix_binary[start.plot,] + speciesVsitesMatrix_binary[next.plot.name,] #Merge the seed plot with the latest selected plot to get all occurrences into one virtual plot
    speciesVsitesMatrix_binary[start.plot,][speciesVsitesMatrix_binary[start.plot,] > 0] <- 1 #set all values to 1 so it is PA data in case species are shared
    speciesVsitesMatrix_binary <- speciesVsitesMatrix_binary[(!rownames(speciesVsitesMatrix_binary) %in% next.plot.name),]
  }
  dissimilarplots <- unlist(result) #vector of plot names in order selected
  return(specaccum(original_matrix[dissimilarplots,], method="collector"))
}


########################
Frequent_simpson_beta.opt <- function(speciesVsitesMatrix_binary, n.plt, iterations) {
    opt.runs.freq <- list()
    n <- 0
    for(i in 1:iterations) {
      n <- n + 1
      cat("Rep ", n, "\n")
      opt.runs.freq[[n]] <- simpson_beta.opt(speciesVsitesMatrix_binary, n.plt, start = "random")
      } #end iterations
    freq_plots <- plyr::count(unlist(lapply(opt.runs.freq, FUN=function(x) as.character(names(x$richness)))))
    freq_plots <- freq_plots[rev(order(freq_plots$freq)),]
    freq_plots <- freq_plots[1:n.plt,] 
    freq_accum <- specaccum(speciesVsitesMatrix_binary[as.character(freq_plots$x),], method="collector")
    
    #create mean/sd accumulation for simpson iterations with random seed
    combined.rand_specaccum <- opt.runs.freq[[1]] #copy one specaccum object in the list of random starts for format
    combined_matrix_rand <- do.call(rbind, lapply(opt.runs.freq, function(x) {return(x$richness)})) #compile the cumulative richness results from reps above into a matrix
    combined.rand_specaccum$richness <- apply(combined_matrix_rand, 2, mean) #using the matrix, calculate the mean for each additional plot, and add that to the richness slot in the specaccum object
    combined.rand_specaccum$sd <- apply(combined_matrix_rand, 2, sd) #same for standard deviation
    combined.rand_specaccum$method <- "random" #assign it as random not collector so it plots correctly as mean and SD
    
    freq_accum_lst <- list()
    freq_accum_lst$simspon_rand <- combined.rand_specaccum
    freq_accum_lst$Freq <- freq_accum
    return(freq_accum_lst)
}  
  
  
####################################
Random.opt <- function(speciesVsitesMatrix_binary, n.plt, iterations) {
    Sppaccum_freq <- list() #
    n <- 0
    for(i in 1:iterations) {
      n <- n + 1
      cat("Rep ", n, "\n")
      Sppaccum_freq[[n]]<- specaccum(speciesVsitesMatrix_binary[sample(nrow(speciesVsitesMatrix_binary), n.plt),], method="collector")
      }  
    combined.rand_specaccum <- Sppaccum_freq[[1]] #copy one for format
    combined_matrix_rand <- do.call(rbind, lapply(Sppaccum_freq, function(x) {return(x$richness)})) #compile the cumulative richness results from reps above into a matrix
    combined.rand_specaccum$richness <- apply(combined_matrix_rand, 2, mean) #using the matrix, calculate the mean for each additional plot, and add that to the richness slot in the specaccum object
    combined.rand_specaccum$sd <- apply(combined_matrix_rand, 2, sd) #same for standard deviation
    combined.rand_specaccum$method <- "random" #assign it as random not collector so it plots correctly as mean and SD
    return(combined.rand_specaccum) #object that can be plotted with mean line with bars for sd
} 


#################################
plot.opt <- function(result, choices=c("Richness", "RRR", "CWE", "Shannon", "Simpson", "SimpsonBeta", "Frequent", "SimpsonBeta_randSeed", "Random")) {
  	result <- result[names(result) %in% choices]
  	plot(1, ylim=c(0, max(unlist(lapply(result, FUN=function(x) max(x$richness))))), xlim=c(0, length(result[[1]]$richness)), type="n", xlab = "Number of plots", ylab = "Cumulative species", main="Site optimisation applying the Maximum Coverage Problem", las=1, bty="l", cex.main=1.2) #blank template plot
    opt.col <- sample(rainbow(8), length(which(names(result) %in% choices[1:8]))) #random colours for the plot lines of optimisers, but exclude random for now
    zzz <- 0
    for(j in result) { #for each optimiser result in the list
      zzz <- zzz + 1
      if(names(result)[zzz] != "Random") {
      	plot(result[[zzz]], col=opt.col[zzz], lwd=1.5, add=TRUE) #add line for each optimiser
      	} #end if not random
      if(names(result)[zzz] == "Random") {
      	plot(result[[zzz]], col="gray", lwd=1, lty=2, add=TRUE) #add line for random
      } #end if Random
    } #close for j in ...
    if("Random" %in% names(result)) {
    	legend("bottomright", legend=names(result), lty=c(rep(1, (length(result)-1)), 2), col=c(opt.col[1:(length(result)-1)], "gray"), cex=1, bty='n', lwd=rep(3, length(result))) 	
    } #end if Random
    if(!"Random" %in% names(result)) {
    	legend("bottomright", legend=names(result), lty=rep(1, length(result)), col=opt.col[1:length(result)], cex=1, bty='n', lwd=rep(3, length(result))) 	
    } #end if not Random
  } #end plot.opt function

 