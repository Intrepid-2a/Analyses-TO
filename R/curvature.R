

# also source("R/utilities.R")


loadCurvatureData <- function() {
  
  # get participants in this task:
  participants <- read.table(file = 'data/participants.tsv', header = TRUE, sep = "\t", skip = 0, stringsAsFactors = FALSE)
  participants <- participants$ID[which(participants$curvature)]
  
  allData <- list()
  for (participant in participants) {
    pdf <- loadParticipantTaskData(ID=participant,task='curvature')
    pdf$Reversal  <- NULL
    pdf$AllTrials <- NULL
    pdf$StairsOngoing <- NULL
    # green filter is on the left eye, so green stimuli will be visible to the right eye
    pdf$Eye      <- c('left', 'right')[pdf$GreenStim+1]
    # position was 0 = away from blind spot; 1 is right next to blind spot
    pdf$Location <- c('out',  'bsa')[pdf$Stimulus_position+1]
    
    # print( pdf$Response == pdf$HemiField )
    pdf$OutwardResponse <- as.integer( pdf$Response == pdf$HemiField )
    
    
    # original curvature is with positive values = toward blind spot / away from fovea, negative values = away from blind spot / toward fovea
    
    allData[[participant]] <- pdf
  }
  
  return(allData)
  
}



processCurvatureData <- function() {
  
  allData <- loadCurvatureData()
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out') )
  
  allConditionData <- list()
  
  for (cond_no in c(1:nrow(info))) {
    
    # select rows that correspond to one of the 4 conditions:
    eye <- info$eye[cond_no]
    area <- info$area[cond_no]
    
    condition_data <- NA
    
    for (participant in names(allData)) {
      
      df <- allData[[participant]]
      
      if (eye == 'ipsi') {
        e_idx <- which(df$HemiField == df$Eye)
      } else {
        e_idx <- which(df$HemiField != df$Eye)
      }
      
      if (area == 'bsa') {
        a_idx <- which(df$Stimulus_position == 1)
      } else {
        a_idx <- which(df$Stimulus_position == 0)
      }
      
      idx <- intersect(e_idx, a_idx)
      
      # print(length(idx))
      
      cdf <- df[idx,]
      cdf$count <- 1
      
      agcdf  <- aggregate(OutwardResponse ~ Curvature, data=cdf, FUN=mean)
      agccdf <- aggregate(count           ~ Curvature, data=cdf, FUN=sum)
      
      agcdf$participant <- participant
      agcdf$count       <- agccdf$count
      
      
      if (is.data.frame(condition_data)) {
        condition_data <- rbind(condition_data, agcdf)
      } else {
        condition_data <- agcdf
      }
      
    }
    
    allConditionData[[sprintf('%s_%s', eye, area)]] <- condition_data
    
  }
  
  return(allConditionData)
  
}


logisticCurvatureModels <- function(allConditionData=NULL) {
  
  if (is.null(allConditionData)) {
    allConditionData <- processCurvatureData(scale=0)
  }
  
  allConditionModels <- list()
  
  for (condition in names(allConditionData)) {
    
    cond_df <- allConditionData[[condition]]
    
    # going to assign weights to participants
    # based on how much data they are contributing
    # should be the same for everyone, but this 
    cond_df$weights <- NA
    
    
    # this is without weights (by counted observations per distance-difference)
    # cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="logit"), data=cond_df)
    
    # here it is with weights:
    # cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="probit"), data=cond_df, weights=cond_df$counts)
    cond_mod <- glm(OutwardResponse ~ Curvature, family=quasibinomial(link="probit"), data=cond_df, weights=cond_df$counts)
    
    allConditionModels[[condition]] <- cond_mod
    
  }
  
  return(allConditionModels)
  
}

plotCurvaturePsychometricCurve <- function(target='inline') {
  
  setupFigureFile( target = target, 
                   width = 5, 
                   height = 5, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/curvature_psychometric.%s', target, target))
  
  
  allConditionData <- processCurvatureData()
  
  N <- length(unique(allConditionData[[1]]$participant))
  
  allConditionModels <- logisticCurvatureModels(allConditionData)
  
  plot(-1000,-1000,
       xlim=c(-.4, .4), ylim=c(0,1),
       main=sprintf('curvature perception (N=%d)',N), xlab='difference [dva]', ylab='proportion outward chosen',
       ax=F, bty='n')
  
  lines(x=c(-.4,.4),y=c(0.5,0.5),col='gray',lty=3)
  lines(x=c(0,0),y=c(0,1),col='gray',lty=3)
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out'),
                      col  = c('red', 'orange', 'blue', 'turquoise'))
  
  
  for (cond_no in c(1:nrow(info))) {
    
    scol <- info$col[cond_no]
    tcol <- Reach::colorAlpha(scol, alpha=34)
    
    cdf <- allConditionData[[sprintf('%s_%s', info$eye[cond_no], info$area[cond_no])]]
    
    acdf <- aggregate(OutwardResponse ~ Curvature, data=cdf, FUN=mean)
    lines(acdf, col=Reach::colorAlpha(scol, alpha=100))
    points(acdf, col=Reach::colorAlpha(scol, alpha=100))
    
    mod <- allConditionModels[[sprintf('%s_%s', info$eye[cond_no], info$area[cond_no])]]
    pred <- predict(mod, type = 'response', se.fit = TRUE, newdata=data.frame(Curvature=seq(-0.4,0.4,0.01)))
    
    polygon(c(seq(-0.4,0.4,0.01), rev(seq(-0.4,0.4,0.01))),
            c(pred$fit - pred$se.fit * 1.96, rev(pred$fit + pred$se.fit * 1.96)),
            col = tcol, border = FALSE)
    lines(pred$fit ~ seq(-.4,.4,0.01), lwd = 1, col = scol)
    
  }
  
  legend(x=0, y=0.5,
         legend=c('ipsilateral, at', 'ipsilateral, away', 'contralateral, at', 'contralateral, away'),
         lty=1, col=info$col,
         bty='n')
  
  axis(side=1, at=c(-.4,-.2,0,.2,.4))
  axis(side=2, at=c(0,0.5,1))
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
}

