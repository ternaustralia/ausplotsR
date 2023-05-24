# This is a version of the 'Trajectory plots' functions trajectoryPCoA and trajectoryPlots from the R package ecotraj that has been modified here under GPL-3 for use with ausplotsR, specifically to alter cosmetic features of the output plots.
# 
# De Cáceres, M., Coll, L., Legendre, P., Allen, R.B., Wiser, S.K., Fortin, M.J., Condit, R. &
#   Hubbell, S. (2019). Trajectory analysis in community ecology. Ecological Monographs 89: e01350
# 
# Sturbois, A., De Cáceres, M., Sánchez-Pinillos, M., Schaal, G., Gauthier, O., Le Mao, P.,
# Ponsero, A., & Desroy, N. (2021). Extending community trajectory analysis : New metrics and
# representation. Ecological Modelling 440: 109400

trajectoryPCoA_ausplots <- function(d, sites, surveys = NULL, selection = NULL, traj.colors = NULL, axes=c(1,2), survey.labels = FALSE, ...) {
  siteIDs <- unique(sites)
  nsite <- length(siteIDs)
  
  #Apply site selection
  
  if(is.null(selection)) selection = 1:nsite 
  else {
    if(is.character(selection)) selection = (siteIDs %in% selection)
  }
  selIDs = siteIDs[selection]
  
  D2 <- as.dist(as.matrix(d)[sites %in% selIDs, sites %in% selIDs])
  cmd_D2 <- cmdscale(D2,eig=TRUE, add=TRUE, k=nrow(as.matrix(D2))-1)
  
  x <- cmd_D2$points[,axes[1]]
  y <- cmd_D2$points[,axes[2]]
  plot(x,y, type="n", asp=1, xlab=paste0("PCoA ",axes[1]," (", round(100*cmd_D2$eig[axes[1]]/sum(cmd_D2$eig)),"%)"), 
       ylab=paste0("PCoA ",axes[2]," (", round(100*cmd_D2$eig[axes[2]]/sum(cmd_D2$eig)),"%)"), las=1)
  
  sitesred <- sites[sites %in% selIDs]
  if(!is.null(surveys)) surveysred = surveys[sites %in% selIDs]
  else surveysred = NULL
  #Draw arrows
  for(i in 1:length(selIDs)) {
    ind_surv <- which(sitesred==selIDs[i])
    #Surveys may not be in order
    if(!is.null(surveysred)) ind_surv <- ind_surv[order(surveysred[sitesred==selIDs[i]])]
    for(t in 1:(length(ind_surv)-1)) {
      niini <- ind_surv[t]
      nifin <- ind_surv[t+1]
      if(!is.null(traj.colors)) arrows(x[niini],y[niini],x[nifin],y[nifin], col = traj.colors[i], ...)
      else arrows(x[niini],y[niini],x[nifin],y[nifin], ...)
      if(survey.labels) {
        text(x[niini],y[niini], labels = ifelse(!is.null(surveysred), surveysred[niini],t), pos = 3)
        if(t==(length(ind_surv)-1)) {
          text(x[nifin],y[nifin], labels = ifelse(!is.null(surveysred), surveysred[nifin],t+1), pos = 3)
        }
      }
    }
  }
  #Return cmdscale result
  invisible(cmd_D2)
}

trajectoryPlot_ausplots <- function(x, sites, surveys = NULL, selection = NULL, traj.colors = NULL, axes=c(1,2), 
                         survey.labels = FALSE, axis.x="Species richness", axis.y="Shannon diversity", ...) {
  siteIDs <- unique(sites)
  nsite <- length(siteIDs)
  
  #Apply site selection
  
  if(is.null(selection)) selection = 1:nsite 
  else {
    if(is.character(selection)) selection = (siteIDs %in% selection)
  }
  selIDs = siteIDs[selection]
  
  xp = x[sites %in% selIDs, axes[1]]
  yp <- x[sites %in% selIDs,axes[2]]
  plot(xp, yp, type="n", xlab=axis.x, 
       ylab=axis.y, las=1)
  
  sitesred <- sites[sites %in% selIDs]
  if(!is.null(surveys)) surveysred = surveys[sites %in% selIDs]
  else surveysred <- NULL
  #Draw arrows
  for(i in 1:length(selIDs)) {
    ind_surv <- which(sitesred==selIDs[i])
    #Surveys may not be in order
    if(!is.null(surveysred)) ind_surv <- ind_surv[order(surveysred[sitesred==selIDs[i]])]
    for(t in 1:(length(ind_surv)-1)) {
      niini <- ind_surv[t]
      nifin <- ind_surv[t+1]
      if(!is.null(traj.colors)) arrows(xp[niini],yp[niini],xp[nifin],yp[nifin], col = traj.colors[i], ...)
      else arrows(xp[niini],yp[niini],xp[nifin],yp[nifin], ...)
      if(survey.labels) {
        text(xp[niini],yp[niini], labels = ifelse(!is.null(surveysred), surveysred[niini],t), pos = 3)
        if(t==(length(ind_surv)-1)) {
          text(xp[nifin],yp[nifin], labels = ifelse(!is.null(surveysred), surveysred[nifin],t+1), pos = 3)
        }
      }
    }
  }
}