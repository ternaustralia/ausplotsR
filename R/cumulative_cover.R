cumulative_cover <- function(veg.PI) {
	
  plot.hits.temp <- veg.PI[, c("herbarium_determination", "hits_unique", "in_canopy_sky")]
  
  transect.order <- c("N1-S1", "S1-N1", "S2-N2", "N2-S2", "N3-S3", "S3-N3", "S4-N4", "N4-S4", "N5-S5", "S5-N5", "W1-E1", "E1-W1", "E2-W2", "W2-E2", "W3-E3", "E3-W3", "E4-W4", "W4-E4", "W5-E5", "E5-W5") #for reference later to re-order the hits into a logical progression

  
  plot.hits.temp <- plot.hits.temp[gtools::mixedorder(plot.hits.temp$hits_unique),] #re-order hits for that site in a logical transect order with hits 0-100 rather than some plots being in alphabetical species order (default mixedorder is alphabetical then numeric) ####Note: not redundant even with additional sorting step below - we need to get the hits within transect in 0-100 order first, and then reorder the transects, otherwise hits can still be in species alpha order within transects, resulting in zigzag in the plot:
  trans.ord.vec <- plot.hits.temp$hits_unique[order(match(gsub(" .*$", "", plot.hits.temp$hits_unique), transect.order))]
  
  plot.hits.temp <- plot.hits.temp[match(trans.ord.vec, plot.hits.temp$hits_unique),]
  
  
  uniq.hits.plot.i <- length(unique(plot.hits.temp$hits_unique)) #number of unique intercepts for plot i
  x.ca <- list()
  if(length(na.omit(unique(plot.hits.temp$herbarium_determination))) > 0) { #needed because there are plots with no herbarium_determination, only growth form, so we want to skip all of the below for these, means if more than zero species
    
    #calculate cover scores, over increasing number of points, for each species in turn
    spp.num <- 0
    for(x in na.omit(unique(plot.hits.temp$herbarium_determination))) { #for each species... #the na.omit means that it won't attempt to count cover for herb_det== NA which results from substrate-only hits
      #print(x) #print the current species
      spp.num <- spp.num + 1
      x.ca[[spp.num]] <- rep(NA, floor(uniq.hits.plot.i/10)) #10 hit intervals are used to score cumulative cover
      n <- 0 # to subset x.ca[[spp.num]] vector to save the score
      
      for(k in 10*c(1:(uniq.hits.plot.i/10))) { #this is for each of 10, 20, 30 etc up to 1010 or whatever was taken
        n <- n+1
        CA.temp <- subset(plot.hits.temp, hits_unique %in% unique(plot.hits.temp$hits_unique)[1:k]) #subset to all the hits for that species up to the current k (10, 20, 30,... 1000) ## gives all rows of hits for the first k unique hits (so there may be more than k rows if double+ species hit in some intercepts)
        CA.temp.x <- subset(CA.temp, herbarium_determination==x)
        CA.temp.x <- CA.temp.x[which(as.character(CA.temp.x$in_canopy_sky) == "FALSE"),] #remove in canopy sky hits as we do not want to calculate opaque canopy cover here
        temp.cover.score <- as.numeric((count(CA.temp.x, vars="herbarium_determination")$freq)/k*100) #count the number of hits so far, divide by the total unique hits and make a percent. OUTDATED?::##dividing by k here is a placeholder as it should really be unique hits not unique rows from 'hits' that may be from the same hit... JUST NEED TO DIVIDE by length(unique(plot.hits.temp$hits_unique)) when all rows have a value for this (at present substrate only hits have NA) BUT ABOVE ALREADY GETS ALL HITS FROM FIRST K UNIQUE HITS so dividing by k should be fine now, e.g. if first 100 hits have 125 species hits, the covers are still divided by 100 not 125...
        if(length(temp.cover.score) != 0) {x.ca[[spp.num]][n] <-temp.cover.score} #assign the cumulative cover to the storage vector for plotting that species
      } #close for k
     # print(temp.cover.score)
    }#close for x (loop over species in a plot)
    max.cover.plot.i <- max(na.omit(unlist(x.ca))) #use this instead of 100 for ylim below to make neat but have changed to 100 to make default room for the legend within the plot panel and to make plots comparable in size.
    
    plot(1, ylim=c(0, 100), xlim=c(0, 1100), type="n", xlab="Number of point intercepts", ylab="Estimated %CA", main=unique(veg.PI$site_unique), las=1, bty="l", cex.main=1.2) #blank template plot
    spp.col <- sample(colors(), length(x.ca)) #random colour for the plot line of species x
    zzz <- 0
    
    for(j in x.ca) {
      zzz <- zzz + 1 #just to match a colour to cover data for a species
      points(10*c(1:(uniq.hits.plot.i/10))[1:length(j)], j, type="l", col=spp.col[zzz]) #add the cumulative cover for species x to the plot for plot i
    } #close for j in x.ca
    
    #plot(0,type='n',axes=FALSE,ann=FALSE)
    legend.data <- data.frame(cover = sapply(x.ca, FUN = function(x) {tail(x, n=1)}), species = na.omit(unique(plot.hits.temp$herbarium_determination)), colour = spp.col)
    legend.data <- na.omit(legend.data[order(legend.data[,"cover"], decreasing=TRUE),][1:5,])
    legend("topright", legend=legend.data$species, lty=rep(1, length(legend.data$species)), col=as.character(legend.data$colour), cex=0.8, bty='n', lwd=rep(3, length(legend.data$species)))
    
  }#close if length... > 0, where plots with no herbarium determinations are excluded

	
	
	
}#cls function