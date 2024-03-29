species_list <- function(veg.vouch, grouping=c("by_site", "by_visit", "collapse"), species_name=c("SN","HD","GS"), strip_bryophytes=FALSE, append_family=FALSE, writefile=FALSE, outfile="my_species_lists.txt") {
  
  #input checks and set defaults
  if(!inherits(veg.vouch, "data.frame")) {stop("veg.vouch must be a data.frame")}
  if(any(!c("site_location_name", "herbarium_determination") %in% names(veg.vouch))) {stop("Can't match names of veg.vouch; data frame should be returned from get_ausplots")}
  
  if(any(!is.logical(c(strip_bryophytes, append_family, writefile)))) {
    stop("Arguments 4:6 must be TRUE or FALSE")
  }

  if(missing(species_name)) { #if no species_name supplied, default to SN
    species_name <- "SN"
    warning("No species_name supplied, defaulting to species_name='SN'")
  } #end missing
  
  if(missing(grouping)) {
    grouping <- "by_site"
    warning("No grouping supplied, defaulting to 'by_site'")
  } #end missing
  
  if(!grouping %in% c("by_site", "by_visit", "collapse")){
    stop("Grouping argument does not match available choices")
  }
  
  if(!species_name %in% c("SN", "GS", "HD")){
    stop("Species_name argument does not match available choices")
  }
  
    
  #if(strip_bryophytes) {veg.vouch <- subset(veg.vouch, taxa_group != "bryophytes")}
  if(strip_bryophytes) {warning("Argument 'strip_bryophytes' is deprecated. species_name = 'HD' returns all determinations, whereas 'SN' returns matches with the Australian Plant Census, which excludes bryophytes.")}
  
  if(species_name == "SN") {
    veg.vouch <- subset(veg.vouch, !is.na(standardised_name) & genus != "NA")
    gather_names <- function(x) {
      noquote(sort(unique(na.omit(x[,"standardised_name"]))))
      }
  }
  
  if(species_name == "HD") {
    veg.vouch <- subset(veg.vouch, !is.na(herbarium_determination))
    gather_names <- function(x) {
      noquote(sort(unique(na.omit(x[,"herbarium_determination"]))))
      }
    }
  
  if(species_name == "GS") {
    veg.vouch <- subset(veg.vouch, !is.na(genus)  & genus != "NA")
    gather_names <- function(x) {
      noquote(sort(unique(na.omit(x[,"genus_species"]))))
      }
    }
  
  
  
  if(grouping == "by_site") {
    final_list <- lapply(split(veg.vouch, veg.vouch$site_location_name, drop=TRUE), gather_names)
  }
  
  if(grouping == "by_visit") {
    final_list <- lapply(split(veg.vouch, veg.vouch$site_unique, drop=TRUE), gather_names)
    }

  if(grouping == "collapse") {
    final_list <- gather_names(veg.vouch)
  }
  

  if(append_family) {
    if(species_name == "SN") {
      if(grouping != "collapse"){
        final_list <- lapply(final_list, function(x) {noquote(sort(paste(x, veg.vouch[match(x, veg.vouch$standardised_name), "family"], sep="--")))})
        }
      if(grouping == "collapse") {
        final_list <- noquote(sort(paste(final_list, veg.vouch[match(final_list, veg.vouch$standardised_name), "family"], sep="--")))
      }
    }
    
    if(species_name == "GS") {
      if(grouping != "collapse"){
        final_list <- lapply(final_list, function(x) {noquote(sort(paste(x, veg.vouch[match(x, veg.vouch$genus_species), "family"], sep="--")))})
      }
      if(grouping == "collapse") {
        final_list <- noquote(sort(paste(final_list, veg.vouch[match(final_list, veg.vouch$genus_species), "family"], sep="--")))
      }
    }
    
    if(species_name == "HD") {
      if(grouping != "collapse"){
        final_list <- lapply(final_list, function(x) {noquote(sort(paste(x, veg.vouch[match(x, veg.vouch$herbarium_determination), "family"], sep="--")))})
      }
      if(grouping == "collapse") {
        final_list <- noquote(sort(paste(final_list, veg.vouch[match(final_list, veg.vouch$herbarium_determination), "family"], sep="--")))
      }
    }
  }
  
  if(writefile) {
    write.table(do.call(rbind, lapply(final_list, function(x) paste(x,collapse=", "))), file=outfile)
  }
  
  return(final_list)
}