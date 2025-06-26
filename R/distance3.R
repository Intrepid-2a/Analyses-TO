

# also source("R/utilities.R")


# loads all the distance data:

loadDistanceData <- function() {
  
  # get participants in this task:
  participants <- read.table(file = 'data/participants.tsv', header = TRUE, sep = "\t", skip = 0)
  participants <- participants$ID[which(participants$distance)]
  
  allData <- list()
  for (participant in participants) {
    allData[[participant]] <- loadParticipantTaskData(ID=participant,task='distance')
  }
  
  return(allData)
  
}

# split by foil -----

processSplitDistanceData <- function() {
  
  allData <- loadDistanceData()
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont','ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out', 'bsa', 'out', 'bsa', 'out'),
                      foil = c('top', 'top', 'top', 'top', 'bot', 'bot', 'bot', 'bot'))
  
  allConditionData <- list()
  
  for (cond_no in c(1:nrow(info))) {
    
    # select rows that correspond to one of the 4 conditions:
    eye  <- info$eye[ cond_no]
    area <- info$area[cond_no]
    foil <- info$foil[cond_no]
    
    condition_data <- NA
    
    for (participant in names(allData)) {
      
      df <- allData[[participant]]
      
      if (eye == 'ipsi') {
        e_idx <- which(df$HemiField == df$Eye)
      } else {
        e_idx <- which(df$HemiField != df$Eye)
      }
      
      if (area == 'bsa') {
        a_idx <- which(df$Targ_loc    %in% c('left-mid','righ-mid'))
      } else {
        a_idx <- which(df$Targ_loc %notin% c('left-mid','righ-mid'))
      }
      
      if (foil == 'top') {
        f_idx <- which(df$Foil_loc %in% c('left-top','righ-top'))
      } else {
        f_idx <- which(df$Foil_loc %in% c('left-bot','righ-bot'))
      }
      
      idx <- intersect(intersect(e_idx, a_idx),f_idx)
      
      # print(length(idx))
      
      cdf <- df[idx,]
      cdf$count <- 1

      agcdf  <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
      agccdf <- aggregate(count       ~ Difference, data=cdf, FUN=sum)

      agcdf$participant <- participant
      agcdf$count       <- agccdf$count
      
      # cdf$Targ_chosen <- as.numeric(cdf$Targ_chosen)
      
      # cdf <- cdf[,c('Targ_chosen','Difference')]
      
      # cdf$participant <- participant
      
      if (is.data.frame(condition_data)) {
        condition_data <- rbind(condition_data, agcdf)
      } else {
        condition_data <- agcdf
      }
      
    }
    
    allConditionData[[sprintf('%s_%s_%s', eye, area, foil)]] <- condition_data
    
  }
  
  return(allConditionData)
  
}



# Normal 	Identity 	g(μ)=μ
# μ=Xβ
# gaussian(link="identity")
# 
# Binomial 	Logit 	g(μ)=log(μ1−μ)
# log(μ1−μ)=Xβ
# binomial(link="logit")
# 
# Poisson 	Log 	g(μ)=log(μ)
# −μ−1=Xβ
# poisson(link="log")
# 
# Exponential 	Negative Inverse 	g(μ)=−μ−1
# log(μ)=Xβ
# Gamma(link="inverse")





# mprobitDistanceModels <- function(allConditionData=NULL) {
#   
#   source('R/mprobit.R')
#   
#   if (is.null(allConditionData)) {
#     allConditionData <- processDistanceData()
#   }
#   
#   allConditionModels <- list()
#   
#   for (condition in names(allConditionData)) {
#     
#     cond_df <- allConditionData[[condition]]
#     
#     # going to assign weights to participants
#     # based on how much data they are contributing
#     # should be the same for everyone, but this 
#     cond_df$weights <- NA
#     
#     
#     # this is without weights (by counted observations per distance-difference)
#     # cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="logit"), data=cond_df)
#     
#     # here it is with weights:
#     # cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="probit"), data=cond_df, weights=cond_df$counts)
#     cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="logit"), data=cond_df, weights=cond_df$counts)
#     
#     
#     
#     allConditionModels[[condition]] <- cond_mod
#     
#   }
#   
#   return(allConditionModels)
#   
# }

plotSplitDistancePsychometricCurves <- function(target='inline') {
  
  setupFigureFile( target = target, 
                   width = 10, 
                   height = 5, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/distance_psychometric_foilloc.%s', target, target))
  
  
  allConditionData <- processSplitDistanceData()
  
  N <- length(unique(allConditionData[[1]]$participant))
  
  # allConditionModels <- logisticDistanceModels(allConditionData)
  
  layout(mat=matrix(data=c(1,2), nrow=1, ncol=2, byrow=TRUE))
  
  for (foil in c('top','bot')) {
    
    plot(-1000,-1000,
         xlim=c(-3.5, 3.5), ylim=c(0,1),
         main=sprintf('distance perception (N=%d, %s foil)',N,list('top'='top','bot'='bottom')[[foil]]), xlab='difference [dva]', ylab='proportion target chosen',
         ax=F, bty='n')
    
    lines(x=c(-3.5,3.5),y=c(0.5,0.5),col='gray',lty=3)
    lines(x=c(0,0),y=c(0,1),col='gray',lty=3)
    
    info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                        area = c('bsa', 'out', 'bsa', 'out'),
                        col  = c('red', 'orange', 'blue', 'turquoise'))
    
    
    for (cond_no in c(1:nrow(info))) {
      
      scol <- info$col[cond_no]
      tcol <- Reach::colorAlpha(scol, alpha=34)
      
      cdf <- allConditionData[[sprintf('%s_%s_%s', info$eye[cond_no], info$area[cond_no], foil)]]
      
      acdf <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
      lines(acdf, col=Reach::colorAlpha(scol, alpha=100))
      points(acdf, col=Reach::colorAlpha(scol, alpha=100))
      
      mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference, 
                           start = c( 0,   1, 0   ),
                           lower = c(-3, 0.2, 0   ),
                           upper = c( 3,   3, 0.3 )
      )
      X <- seq(-3.5,3.5,0.1)
      lines(x=X, y=mprobit(p=mprob$par, x=X), lwd = 1, col = scol)
      
      # mod <- allConditionModels[[sprintf('%s_%s', info$eye[cond_no], info$area[cond_no])]]
      # pred <- predict(mod, type = 'response', se.fit = TRUE, newdata=data.frame(Difference=seq(-3.5,3.5,0.1)))
      
      
      
      # polygon(c(seq(-3.5,3.5,0.1), rev(seq(-3.5,3.5,0.1))),
      #         c(pred$fit - pred$se.fit * 1.96, rev(pred$fit + pred$se.fit * 1.96)),
      #         col = tcol, border = FALSE)
      
      # lines(pred$fit ~ seq(-3.5,3.5,0.1), lwd = 1, col = scol)
      
    }
    
    legend(x=0, y=0.5,
           legend=c('ipsi, at', 'ipsi, away', 'contra, at', 'contra, away'),
           lty=1, col=info$col,
           bty='n')
    
    axis(side=1, at=c(-3.5,-2,-1,0,1,2,3.5))
    axis(side=2, at=c(0,0.5,1))
    
  }
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
}

## statistics -----

getAllSplitDistData <- function() {
  
  allConditionData <- processSplitDistanceData()
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont','ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out', 'bsa', 'out', 'bsa', 'out'),
                      foil = c('top', 'top', 'top', 'top', 'bot', 'bot', 'bot', 'bot'))
  
  allData <- NA
  
  for (condition in names(allConditionData)) {
    
    cond_df <- allConditionData[[condition]]
    
    
    if (condition %in% c("ipsi_bsa_top", "ipsi_out_top", "ipsi_bsa_bot", "ipsi_out_bot")) {
      cond_df$Eye <- 'ipsilateral'
    } else {
      cond_df$Eye <- 'contralateral'
    }
    if (condition %in% c("ipsi_bsa_top", "cont_bsa_top", "ipsi_bsa_bot", "cont_bsa_bot")) {
      cond_df$Location <- 'blindspot'
    } else {
      cond_df$Location <- 'away'
    }
    if (condition %in% c("ipsi_bsa_top", "cont_bsa_top", "ipsi_out_top", "cont_out_top")) {
      cond_df$Foil <- 'top'
    } else {
      cond_df$Foil <- 'bottom'
    }
    if (is.data.frame(allData)) {
      allData <- rbind(allData, cond_df)
    } else {
      allData <- cond_df
    }
    
  }
  
  return(allData)
  
}

getSplitDistANOVAdata <- function() {
  
  data <- getAllSplitDistData()
  
  participants <- unique(data$participant)
  eyes <- unique(data$Eye)
  locations <- unique(data$Location)
  foils <- unique(data$Foil)
  
  # create a data frame with all combinations of participants, eyes, and locations
  df <- expand.grid(participant = participants, Eye = eyes, Location = locations, Foil = foils)
  
  df$mean  <- NA
  df$sd    <- NA
  df$margL <- NA
  df$margU <- NA
  df$PSE   <- NA
  df$slope <- NA
  
  for (i in 1:nrow(df)) {
    participant <- df$participant[i]
    eye        <- df$Eye[i]
    location   <- df$Location[i]
    foil       <- df$Foil[i]
    
    # filter the data for the current combination
    subdf <- data[data$participant == participant & data$Eye == eye & data$Location == location & data$Foil == foil, ]
    
    mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
                         start = c(-0.5,   1, 0  ),
                         lower = c(-3,   0.3, 0  ),
                         upper = c( 3,     3, 0.3) )
    
    df$mean[i] <- mod$par[1]
    df$sd[i]   <- mod$par[2]
    df$margL[i] <- mod$par[3]
    df$margU[i] <- mod$par[4]
    
    descr <- descr.mprobit(p=mod$par)
    df$PSE[i]   <- descr$PSE
    df$slope[i] <- descr$slope
    
  }
  
  return(df)
}

doSplitDistanceStats <- function() {
  
  data <- getSplitDistANOVAdata()
  
  # fit the model
  bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = data,
    within = c("Eye", "Location", "Foil"),
  )
  
  cat("==== Distance Bias ANOVA:\n\n")

  print(bias_aov)
  # 
  # bs_data <- data[which(data$Location == "blindspot"),]
  # 
  # bsl_bias_aov <- afex::aov_ez(
  #   id = "participant",
  #   dv = "mean",
  #   data = bs_data,
  #   within = c("Eye"),
  # )
  
  # cat("\n==== At Blindspot Bias Test:\n\n")
  # 
  # print(bsl_bias_aov)
  
  slope_aov <- afex::aov_ez(
    id = "participant",
    dv = "slope",
    data = data,
    within = c("Eye", "Location", "Foil"),
  )
  
  cat("\n==== Distance Slope ANOVA:\n\n")
  
  print(slope_aov)


}

# split by hemifield ----

processHemiDistanceData <- function() {
  
  allData <- loadDistanceData()
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont','ipsi', 'ipsi', 'cont', 'cont'),
                      area = c('bsa', 'out', 'bsa', 'out', 'bsa',  'out',  'bsa',  'out'),
                      hemi = c('left','left','left','left','right','right','right','right'))
  
  allConditionData <- list()
  
  for (cond_no in c(1:nrow(info))) {
    
    # select rows that correspond to one of the 4 conditions:
    eye  <- info$eye[ cond_no]
    area <- info$area[cond_no]
    hemi <- info$hemi[cond_no]
    
    condition_data <- NA
    
    for (participant in names(allData)) {
      
      df <- allData[[participant]]
      
      if (eye == 'ipsi') {
        e_idx <- which(df$HemiField == df$Eye)
      } else {
        e_idx <- which(df$HemiField != df$Eye)
      }
      
      if (area == 'bsa') {
        a_idx <- which(df$Targ_loc    %in% c('left-mid','righ-mid'))
      } else {
        a_idx <- which(df$Targ_loc %notin% c('left-mid','righ-mid'))
      }
      
      h_idx <- which(df$HemiField == hemi)
      
      idx <- intersect(intersect(e_idx, a_idx),h_idx)
      
      # print(length(idx))
      
      cdf <- df[idx,]
      cdf$count <- 1
      
      agcdf  <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
      agccdf <- aggregate(count       ~ Difference, data=cdf, FUN=sum)
      
      agcdf$participant <- participant
      agcdf$count       <- agccdf$count
      
      if (is.data.frame(condition_data)) {
        condition_data <- rbind(condition_data, agcdf)
      } else {
        condition_data <- agcdf
      }
      
    }
    
    allConditionData[[sprintf('%s_%s_%s', eye, area, hemi)]] <- condition_data
    
  }
  
  return(allConditionData)
  
}

plotHemiDistancePsychometricCurves <- function(target='inline') {
  
  setupFigureFile( target = target, 
                   width = 10, 
                   height = 5, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/distance_psychometric_hemifields.%s', target, target))
  
  
  allConditionData <- processHemiDistanceData()
  
  N <- length(unique(allConditionData[[1]]$participant))

  layout(mat=matrix(data=c(1,2), nrow=1, ncol=2, byrow=TRUE))
  
  for (hemi in c('left','right')) {
    
    plot(-1000,-1000,
         xlim=c(-3.5, 3.5), ylim=c(0,1),
         main=sprintf('distance perception (N=%d, %s hemifield)',N,hemi), xlab='difference [dva]', ylab='proportion target chosen',
         ax=F, bty='n')
    
    lines(x=c(-3.5,3.5),y=c(0.5,0.5),col='gray',lty=3)
    lines(x=c(0,0),y=c(0,1),col='gray',lty=3)
    
    info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                        area = c('bsa', 'out', 'bsa', 'out'),
                        col  = c('red', 'orange', 'blue', 'turquoise'))
    
    
    for (cond_no in c(1:nrow(info))) {
      
      scol <- info$col[cond_no]
      tcol <- Reach::colorAlpha(scol, alpha=34)
      
      cdf <- allConditionData[[sprintf('%s_%s_%s', info$eye[cond_no], info$area[cond_no], hemi)]]
      
      acdf <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
      lines(acdf, col=Reach::colorAlpha(scol, alpha=100))
      points(acdf, col=Reach::colorAlpha(scol, alpha=100))
      
      mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference, 
                           start = c( 0, 1,   0,   0   ),
                           lower = c(-3, 0.2, 0,   0   ),
                           upper = c( 3, 3,   0.3, 0.3 )
      )
      X <- seq(-3.5,3.5,0.1)
      lines(x=X, y=mprobit(p=mprob$par, x=X), lwd = 1, col = scol)
      
    }
    
    legend(x=0, y=0.5,
           legend=c('ipsi, at', 'ipsi, away', 'contra, at', 'contra, away'),
           lty=1, col=info$col,
           bty='n')
    
    axis(side=1, at=c(-3.5,-2,-1,0,1,2,3.5))
    axis(side=2, at=c(0,0.5,1))
    
  }
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
}


## statistics -----

getAllHemiDistData <- function() {
  
  allConditionData <- processHemiDistanceData()
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont','ipsi', 'ipsi', 'cont', 'cont'),
                      area = c('bsa', 'out', 'bsa', 'out', 'bsa',  'out',  'bsa',  'out'),
                      hemi = c('left','left','left','left','right','right','right','right'))
  
  allData <- NA
  
  for (condition in names(allConditionData)) {
    
    cond_df <- allConditionData[[condition]]
    
    
    if (condition %in% c("ipsi_bsa_left", "ipsi_out_left", "ipsi_bsa_right", "ipsi_out_right")) {
      cond_df$Eye <- 'ipsilateral'
    } else {
      cond_df$Eye <- 'contralateral'
    }
    if (condition %in% c("ipsi_bsa_left", "cont_bsa_left", "ipsi_bsa_right", "cont_bsa_right")) {
      cond_df$Location <- 'blindspot'
    } else {
      cond_df$Location <- 'away'
    }
    if (condition %in% c("ipsi_bsa_left", "cont_bsa_left", "ipsi_out_left", "cont_out_left")) {
      cond_df$Hemi <- 'left'
    } else {
      cond_df$Hemi <- 'right'
    }
    if (is.data.frame(allData)) {
      allData <- rbind(allData, cond_df)
    } else {
      allData <- cond_df
    }
    
  }
  
  return(allData)
  
}

getHemiDistANOVAdata <- function() {
  
  data <- getAllHemiDistData()
  
  participants <- unique(data$participant)
  eyes <- unique(data$Eye)
  locations <- unique(data$Location)
  hemis <- unique(data$Hemi)
  
  # create a data frame with all combinations of participants, eyes, and locations
  df <- expand.grid(participant = participants, Eye = eyes, Location = locations, Hemi = hemis)
  
  df$mean  <- NA
  df$sd    <- NA
  df$margL <- NA
  df$margU <- NA
  df$PSE   <- NA
  df$slope <- NA
  
  for (i in 1:nrow(df)) {
    participant <- df$participant[i]
    eye        <- df$Eye[i]
    location   <- df$Location[i]
    hemi       <- df$Hemi[i]
    
    # filter the data for the current combination
    subdf <- data[data$participant == participant & data$Eye == eye & data$Location == location & data$Hemi == hemi, ]
    
    mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
                        start = c(-0.5,   1, 0  ),
                        lower = c(-3,   0.3, 0  ),
                        upper = c( 3,     3, 0.3) )
    
    df$mean[i] <- mod$par[1]
    df$sd[i]   <- mod$par[2]
    df$margL[i] <- mod$par[3]
    df$margU[i] <- mod$par[4]
    
    descr <- descr.mprobit(p=mod$par)
    df$PSE[i]   <- descr$PSE
    df$slope[i] <- descr$slope
    
  }
  
  return(df)
}

doHemiDistanceStats <- function() {
  
  data <- getHemiDistANOVAdata()
  
  # fit the model
  bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = data,
    within = c("Eye", "Location", "Hemi"),
  )
  
  cat("==== Distance Bias ANOVA:\n\n")
  
  print(bias_aov)

  slope_aov <- afex::aov_ez(
    id = "participant",
    dv = "slope",
    data = data,
    within = c("Eye", "Location", "Hemi"),
  )
  
  cat("\n==== Distance Slope ANOVA:\n\n")
  
  print(slope_aov)
  
  
}


# split by eye of origin ----

processOrigDistanceData <- function() {
  
  allData <- loadDistanceData()
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont','ipsi', 'ipsi', 'cont', 'cont'),
                      area = c('bsa', 'out', 'bsa', 'out', 'bsa',  'out',  'bsa',  'out'),
                      orig = c('left','left','left','left','right','right','right','right'))
  
  allConditionData <- list()
  
  for (cond_no in c(1:nrow(info))) {
    
    # select rows that correspond to one of the 4 conditions:
    eye  <- info$eye[ cond_no]
    area <- info$area[cond_no]
    orig <- info$orig[cond_no]
    
    condition_data <- NA
    
    for (participant in names(allData)) {
      
      df <- allData[[participant]]
      
      if (eye == 'ipsi') {
        e_idx <- which(df$HemiField == df$Eye)
      } else {
        e_idx <- which(df$HemiField != df$Eye)
      }
      
      if (area == 'bsa') {
        a_idx <- which(df$Targ_loc    %in% c('left-mid','righ-mid'))
      } else {
        a_idx <- which(df$Targ_loc %notin% c('left-mid','righ-mid'))
      }
      
      o_idx <- which(df$Eye == orig)
      
      idx <- intersect(intersect(e_idx, a_idx),o_idx)
      
      # print(length(idx))
      
      cdf <- df[idx,]
      cdf$count <- 1
      
      agcdf  <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
      agccdf <- aggregate(count       ~ Difference, data=cdf, FUN=sum)
      
      agcdf$participant <- participant
      agcdf$count       <- agccdf$count
      
      if (is.data.frame(condition_data)) {
        condition_data <- rbind(condition_data, agcdf)
      } else {
        condition_data <- agcdf
      }
      
    }
    
    allConditionData[[sprintf('%s_%s_%s', eye, area, orig)]] <- condition_data
    
  }
  
  return(allConditionData)
  
}

plotOrigDistancePsychometricCurves <- function(target='inline') {
  
  setupFigureFile( target = target, 
                   width = 10, 
                   height = 5, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/distance_psychometric_origineye.%s', target, target))
  
  
  allConditionData <- processOrigDistanceData()
  
  N <- length(unique(allConditionData[[1]]$participant))
  
  layout(mat=matrix(data=c(1,2), nrow=1, ncol=2, byrow=TRUE))
  
  for (orig in c('left','right')) {
    
    plot(-1000,-1000,
         xlim=c(-3.5, 3.5), ylim=c(0,1),
         main=sprintf('distance perception (N=%d, %s origin)',N,orig), xlab='difference [dva]', ylab='proportion target chosen',
         ax=F, bty='n')
    
    lines(x=c(-3.5,3.5),y=c(0.5,0.5),col='gray',lty=3)
    lines(x=c(0,0),y=c(0,1),col='gray',lty=3)
    
    info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                        area = c('bsa', 'out', 'bsa', 'out'),
                        col  = c('red', 'orange', 'blue', 'turquoise'))
    
    
    for (cond_no in c(1:nrow(info))) {
      
      scol <- info$col[cond_no]
      tcol <- Reach::colorAlpha(scol, alpha=34)
      
      cdf <- allConditionData[[sprintf('%s_%s_%s', info$eye[cond_no], info$area[cond_no], orig)]]
      
      acdf <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
      lines(acdf, col=Reach::colorAlpha(scol, alpha=100))
      points(acdf, col=Reach::colorAlpha(scol, alpha=100))
      
      mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference, 
                           start = c( 0, 1,   0,   0  ),
                           lower = c(-3, 0.2, 0,   0  ),
                           upper = c( 3, 3,   0.3, 0.3)
      )
      X <- seq(-3.5,3.5,0.1)
      lines(x=X, y=mprobit(p=mprob$par, x=X), lwd = 1, col = scol)
      
    }
    
    legend(x=0, y=0.5,
           legend=c('ipsi, at', 'ipsi, away', 'contra, at', 'contra, away'),
           lty=1, col=info$col,
           bty='n')
    
    axis(side=1, at=c(-3.5,-2,-1,0,1,2,3.5))
    axis(side=2, at=c(0,0.5,1))
    
  }
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
}


## statistics -----

getAllOrigDistData <- function() {
  
  allConditionData <- processOrigDistanceData()
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont','ipsi', 'ipsi', 'cont', 'cont'),
                      area = c('bsa', 'out', 'bsa', 'out', 'bsa',  'out',  'bsa',  'out'),
                      orig = c('left','left','left','left','right','right','right','right'))
  
  allData <- NA
  
  for (condition in names(allConditionData)) {
    
    cond_df <- allConditionData[[condition]]
    
    
    if (condition %in% c("ipsi_bsa_left", "ipsi_out_left", "ipsi_bsa_right", "ipsi_out_right")) {
      cond_df$Eye <- 'ipsilateral'
    } else {
      cond_df$Eye <- 'contralateral'
    }
    if (condition %in% c("ipsi_bsa_left", "cont_bsa_left", "ipsi_bsa_right", "cont_bsa_right")) {
      cond_df$Location <- 'blindspot'
    } else {
      cond_df$Location <- 'away'
    }
    if (condition %in% c("ipsi_bsa_left", "cont_bsa_left", "ipsi_out_left", "cont_out_left")) {
      cond_df$Orig <- 'left'
    } else {
      cond_df$Orig <- 'right'
    }
    if (is.data.frame(allData)) {
      allData <- rbind(allData, cond_df)
    } else {
      allData <- cond_df
    }
    
  }
  
  return(allData)
  
}

getOrigDistANOVAdata <- function() {
  
  data <- getAllOrigDistData()
  
  participants <- unique(data$participant)
  eyes <- unique(data$Eye)
  locations <- unique(data$Location)
  origs <- unique(data$Orig)
  
  # print(origs)
  
  # create a data frame with all combinations of participants, eyes, and locations
  df <- expand.grid(participant = participants, Eye = eyes, Location = locations, Orig = origs)
  
  df$mean  <- NA
  df$sd    <- NA
  df$margL <- NA
  df$margU <- NA
  df$PSE   <- NA
  df$slope <- NA
  
  for (i in 1:nrow(df)) {
    participant <- df$participant[i]
    eye        <- df$Eye[i]
    location   <- df$Location[i]
    orig       <- df$Orig[i]
    
    # filter the data for the current combination
    subdf <- data[data$participant == participant & data$Eye == eye & data$Location == location & data$Orig == orig, ]
    
    mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
                        start = c(-0.5,   1, 0,   0  ),
                        lower = c(-3,   0.3, 0,   0  ),
                        upper = c( 3,     3, 0.3, 0.3) )
    
    df$mean[i] <- mod$par[1]
    df$sd[i]   <- mod$par[2]
    df$margL[i] <- mod$par[3]
    df$margU[i] <- mod$par[4]
    
    descr <- descr.mprobit(p=mod$par)
    df$PSE[i]   <- descr$PSE
    df$slope[i] <- descr$slope
    
  }
  
  return(df)
}

doOrigDistanceStats <- function() {
  
  data <- getOrigDistANOVAdata()
  
  # fit the model
  bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = data,
    within = c("Eye", "Location", "Orig"),
  )
  
  cat("==== Distance Bias ANOVA:\n\n")
  
  print(bias_aov)
  
  slope_aov <- afex::aov_ez(
    id = "participant",
    dv = "slope",
    data = data,
    within = c("Eye", "Location", "Orig"),
  )
  
  cat("\n==== Distance Slope ANOVA:\n\n")
  
  print(slope_aov)
  
  
}


# split by first stimulus ----

processFirstDistanceData <- function() {
  
  allData <- loadDistanceData()
  
  info <- data.frame( eye   = c('ipsi',  'ipsi',  'cont',  'cont',  'ipsi', 'ipsi', 'cont', 'cont'),
                      area  = c('bsa',   'out',   'bsa',   'out',   'bsa',  'out',  'bsa',  'out'),
                      first = c('target','target','target','target','foil', 'foil', 'foil', 'foil'))
  
  allConditionData <- list()
  
  for (cond_no in c(1:nrow(info))) {
    
    # select rows that correspond to one of the 4 conditions:
    eye   <- info$eye[  cond_no]
    area  <- info$area[ cond_no]
    first <- info$first[cond_no]
    
    condition_data <- NA
    
    for (participant in names(allData)) {
      
      df <- allData[[participant]]
      
      df$Which_first[which(df$Which_first == 'Targ')] <- 'target'
      df$Which_first[which(df$Which_first == 'Foil')] <- 'foil'
      
      if (eye == 'ipsi') {
        e_idx <- which(df$HemiField == df$Eye)
      } else {
        e_idx <- which(df$HemiField != df$Eye)
      }
      
      if (area == 'bsa') {
        a_idx <- which(df$Targ_loc    %in% c('left-mid','righ-mid'))
      } else {
        a_idx <- which(df$Targ_loc %notin% c('left-mid','righ-mid'))
      }
      
      f_idx <- which(df$Which_first == first)
      
      idx <- intersect(intersect(e_idx, a_idx),f_idx)
      
      # print(length(idx))
      
      cdf <- df[idx,]
      cdf$count <- 1
      
      agcdf  <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
      agccdf <- aggregate(count       ~ Difference, data=cdf, FUN=sum)
      
      agcdf$participant <- participant
      agcdf$count       <- agccdf$count
      
      if (is.data.frame(condition_data)) {
        condition_data <- rbind(condition_data, agcdf)
      } else {
        condition_data <- agcdf
      }
      
    }
    
    allConditionData[[sprintf('%s_%s_%s', eye, area, first)]] <- condition_data
    
  }
  
  return(allConditionData)
  
}

plotFirstDistancePsychometricCurves <- function(target='inline') {
  
  setupFigureFile( target = target, 
                   width = 10, 
                   height = 5, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/distance_psychometric_origineye.%s', target, target))
  
  
  allConditionData <- processFirstDistanceData()
  
  N <- length(unique(allConditionData[[1]]$participant))
  
  layout(mat=matrix(data=c(1,2), nrow=1, ncol=2, byrow=TRUE))
  
  for (first in c('target','foil')) {
    
    plot(-1000,-1000,
         xlim=c(-3.5, 3.5), ylim=c(0,1),
         main=sprintf('distance perception (N=%d, %s first)',N,first), xlab='difference [dva]', ylab='proportion target chosen',
         ax=F, bty='n')
    
    lines(x=c(-3.5,3.5),y=c(0.5,0.5),col='gray',lty=3)
    lines(x=c(0,0),y=c(0,1),col='gray',lty=3)
    
    info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                        area = c('bsa', 'out', 'bsa', 'out'),
                        col  = c('red', 'orange', 'blue', 'turquoise'))
    
    
    for (cond_no in c(1:nrow(info))) {
      
      scol <- info$col[cond_no]
      tcol <- Reach::colorAlpha(scol, alpha=34)
      
      cdf <- allConditionData[[sprintf('%s_%s_%s', info$eye[cond_no], info$area[cond_no], first)]]
      
      acdf <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
      lines(acdf, col=Reach::colorAlpha(scol, alpha=100))
      points(acdf, col=Reach::colorAlpha(scol, alpha=100))
      
      mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference, 
                           start = c( 0, 1,   0,   0  ),
                           lower = c(-3, 0.2, 0,   0  ),
                           upper = c( 3, 3,   0.3, 0.3)
      )
      X <- seq(-3.5,3.5,0.1)
      lines(x=X, y=mprobit(p=mprob$par, x=X), lwd = 1, col = scol)
      
    }
    
    legend(x=0, y=0.5,
           legend=c('ipsi, at', 'ipsi, away', 'contra, at', 'contra, away'),
           lty=1, col=info$col,
           bty='n')
    
    axis(side=1, at=c(-3.5,-2,-1,0,1,2,3.5))
    axis(side=2, at=c(0,0.5,1))
    
  }
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
}


## statistics -----

getAllFirstDistData <- function() {
  
  allConditionData <- processFirstDistanceData()
  
  info <- data.frame( eye   = c('ipsi',  'ipsi',  'cont',  'cont',  'ipsi', 'ipsi', 'cont', 'cont'),
                      area  = c('bsa',   'out',   'bsa',   'out',   'bsa',  'out',  'bsa',  'out'),
                      first = c('target','target','target','target','foil', 'foil', 'foil', 'foil'))
  
  allData <- NA
  
  for (condition in names(allConditionData)) {
    
    cond_df <- allConditionData[[condition]]
    
    
    if (condition %in% c("ipsi_bsa_target", "ipsi_out_target", "ipsi_bsa_foil", "ipsi_out_foil")) {
      cond_df$Eye <- 'ipsilateral'
    } else {
      cond_df$Eye <- 'contralateral'
    }
    if (condition %in% c("ipsi_bsa_target", "cont_bsa_target", "ipsi_bsa_foil", "cont_bsa_foil")) {
      cond_df$Location <- 'blindspot'
    } else {
      cond_df$Location <- 'away'
    }
    if (condition %in% c("ipsi_bsa_foil", "cont_bsa_foil", "ipsi_out_foil", "cont_out_foil")) {
      cond_df$First <- 'foil'
    } else {
      cond_df$First <- 'target'
    }
    if (is.data.frame(allData)) {
      allData <- rbind(allData, cond_df)
    } else {
      allData <- cond_df
    }
    
  }
  
  return(allData)
  
}

getFirstDistANOVAdata <- function() {
  
  data <- getAllFirstDistData()
  
  participants <- unique(data$participant)
  eyes <- unique(data$Eye)
  locations <- unique(data$Location)
  firsts <- unique(data$First)
  
  # print(origs)
  
  # create a data frame with all combinations of participants, eyes, and locations
  df <- expand.grid(participant = participants, Eye = eyes, Location = locations, First = firsts)
  
  df$mean  <- NA
  df$sd    <- NA
  df$margL <- NA
  df$margU <- NA
  df$PSE   <- NA
  df$slope <- NA
  
  for (i in 1:nrow(df)) {
    participant <- df$participant[i]
    eye        <- df$Eye[i]
    location   <- df$Location[i]
    first      <- df$First[i]
    
    # filter the data for the current combination
    subdf <- data[data$participant == participant & data$Eye == eye & data$Location == location & data$First == first, ]
    
    mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
                        start = c(-0.5,   1, 0,   0  ),
                        lower = c(-3,   0.3, 0,   0  ),
                        upper = c( 3,     3, 0.3, 0.3) )
    
    df$mean[i] <- mod$par[1]
    df$sd[i]   <- mod$par[2]
    df$margL[i] <- mod$par[3]
    df$margU[i] <- mod$par[4]
    
    descr <- descr.mprobit(p=mod$par)
    df$PSE[i]   <- descr$PSE
    df$slope[i] <- descr$slope
    
  }
  
  return(df)
}

doFirstDistanceStats <- function() {
  
  data <- getFirstDistANOVAdata()
  
  # fit the model
  bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = data,
    within = c("Eye", "Location", "First"),
  )
  
  cat("==== Distance Bias ANOVA:\n\n")
  
  print(bias_aov)
  
  slope_aov <- afex::aov_ez(
    id = "participant",
    dv = "slope",
    data = data,
    within = c("Eye", "Location", "First"),
  )
  
  cat("\n==== Distance Slope ANOVA:\n\n")
  
  print(slope_aov)
  
  
}
