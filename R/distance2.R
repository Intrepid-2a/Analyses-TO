

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



processDistanceData <- function() {
  
  allData <- loadDistanceData()
  
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
        a_idx <- which(df$Targ_loc    %in% c('left-mid','righ-mid'))
      } else {
        a_idx <- which(df$Targ_loc %notin% c('left-mid','righ-mid'))
      }
      
      idx <- intersect(e_idx, a_idx)
      
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
    
    allConditionData[[sprintf('%s_%s', eye, area)]] <- condition_data
    
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

plotDistancePsychometricCurve <- function(target='inline') {
  
  setupFigureFile( target = target, 
                   width = 5, 
                   height = 5, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/distance_psychometric.%s', target, target))
  
  
  allConditionData <- processDistanceData()
  
  N <- length(unique(allConditionData[[1]]$participant))
  
  # allConditionModels <- logisticDistanceModels(allConditionData)
  
  plot(-1000,-1000,
       xlim=c(-3.5, 3.5), ylim=c(0,1),
       main=sprintf('distance perception (N=%d)',N), xlab='difference [dva]', ylab='proportion target chosen',
       ax=F, bty='n')
  
  lines(x=c(-3.5,3.5),y=c(0.5,0.5),col='gray',lty=3)
  lines(x=c(0,0),y=c(0,1),col='gray',lty=3)
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out'),
                      col  = c('red', 'orange', 'blue', 'turquoise'))
  
  
  for (cond_no in c(1:nrow(info))) {
    
    scol <- info$col[cond_no]
    tcol <- Reach::colorAlpha(scol, alpha=34)
    
    cdf <- allConditionData[[sprintf('%s_%s', info$eye[cond_no], info$area[cond_no])]]
    
    acdf <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
    lines(acdf, col=Reach::colorAlpha(scol, alpha=100))
    points(acdf, col=Reach::colorAlpha(scol, alpha=100))
    
    mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference,
                         start <- c( 0, 1,   0,   0   ),
                         lower <- c(-3, 0.2, 0,   0   ),
                         upper <- c( 3, 3,   0.3, 0.3 )
                         )
    # mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference, 
    #                      start <- c( 0, 1,   0   ),
    #                      lower <- c(-3, 0.2, 0   ),
    #                      upper <- c( 3, 3,   0.3 )
    #                     )
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
         legend=c('ipsilateral, across', 'ipsilateral, away', 'contralateral, across', 'contralateral, away'),
         lty=1, col=info$col,
         bty='n')
  
  axis(side=1, at=c(-3.5,-2,-1,0,1,2,3.5))
  axis(side=2, at=c(0,0.5,1))
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
}

# statistics -----

getAllDistData <- function() {
  
  allConditionData <- processDistanceData()
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out') )
  
  allData <- NA
  
  for (condition in names(allConditionData)) {
    
    cond_df <- allConditionData[[condition]]
    
    
    if (condition %in% c("ipsi_bsa", "ipsi_out")) {
      cond_df$Eye <- 'ipsilateral'
    } else {
      cond_df$Eye <- 'contralateral'
    }
    if (condition %in% c("ipsi_bsa", "cont_bsa")) {
      cond_df$Location <- 'blindspot'
    } else {
      cond_df$Location <- 'away'
    }
    
    if (is.data.frame(allData)) {
      allData <- rbind(allData, cond_df)
    } else {
      allData <- cond_df
    }
    
  }
  
  return(allData)
  
}

getDistANOVAdata <- function() {
  
  data <- getAllDistData()
  
  participants <- unique(data$participant)
  eyes <- unique(data$Eye)
  locations <- unique(data$Location)
  
  # create a data frame with all combinations of participants, eyes, and locations
  df <- expand.grid(participant = participants, Eye = eyes, Location = locations)
  
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
    
    # filter the data for the current combination
    subdf <- data[data$participant == participant & data$Eye == eye & data$Location == location, ]
    
    mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
                         start <- c(-0.5,   1, 0,   0  ),
                         lower <- c(-3,   0.3, 0,   0  ),
                         upper <- c( 3,     3, 0.3, 0.3) )
    # mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
    #                     start <- c(-0.5,   1, 0   ),
    #                     lower <- c(-3,   0.3, 0   ),
    #                     upper <- c( 3,     3, 0.3 ) )
    
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

doDistanceStats <- function() {
  
  data <- getDistANOVAdata()
  
  # fit the model
  bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = data,
    within = c("Eye", "Location"),
  )
  
  cat("==== Distance Bias ANOVA:\n\n")
  
  print(bias_aov)
  
  bs_data <- data[which(data$Location == "blindspot"),]
  
  bsl_bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = bs_data,
    within = c("Eye"),
  )
  
  cat("\n==== At Blindspot Bias Test:\n\n")
  
  print(bsl_bias_aov)
  
  slope_aov <- afex::aov_ez(
    id = "participant",
    dv = "slope",
    data = data,
    within = c("Eye", "Location"),
  )
  
  cat("\n==== Distance Slope ANOVA:\n\n")
  
  print(slope_aov)


}

## parameter distributions -----

plotDistancePSEslope <- function(target='inline') {
  
  setupFigureFile( target = target, 
                   width = 8, 
                   height = 5, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/distance_pse_slope.%s', target, target))
  
  data <- getDistANOVAdata()
  
  layout(mat    = matrix( c(1,2),
                          nrow=1,
                          ncol=2,
                          byrow=TRUE),
         widths = c(1,1))
  
  for (parameter in c('mean','slope')) {
    
    varname <- list('mean'='bias', 'slope'='precision')[[parameter]]
    
    plot(NULL,NULL,
         ylim=c(0.5,4.5),
         xlim=list('mean'=c(-2,1), 'slope'=c(0,1))[[parameter]],
         main=varname, ylab='', 
         xlab=list('mean'='bias [dva]','slope'=expression(paste("precision [", Delta, "%/", Delta, "dva]")))[[parameter]],
         ax=F, bty='n')
    
    if (parameter == 'mean') {
      lines(y=c(0,5),x=c(0,0),col='gray',lty=3)
    } else {
      # lines(x=c(0,5),y=c(0,1),col='gray',lty=3)
    }
    
    
    info <- data.frame( eye  = c('ipsilateral','contralateral','ipsilateral','contralateral'),
                        area = c('blindspot', 'blindspot',     'away',       'away'),
                        col  = c('red',       'blue',          'orange',     'turquoise'))
    
    for (cond_no in c(1:nrow(info))) {
     
      cdat <- data[which(data$Eye == info$eye[cond_no] & data$Location == info$area[cond_no]),] 
      
      v <- cdat[,parameter]
      
      points(y=rep(cond_no, length(v)), 
             x=v, 
             col=Reach::colorAlpha(info$col[cond_no], alpha=100), 
             pch=16)
      
      avg <- mean(v)
      CI <- sd(v) / sqrt(length(v)) * qt(0.975, df=length(v)-1)
      CI <- avg + c(CI*-1,CI)
      
      polygon(y = cond_no + c(-0.4, -0.2, -0.2, -0.4),
              x = c(CI[1], CI[1], CI[2], CI[2]),
              col = Reach::colorAlpha(info$col[cond_no], alpha=34), border=FALSE)
      lines(y = cond_no + c(-0.4, -0.2), 
            x = c(avg, avg), 
            col=info$col[cond_no], lwd=2)
      
      dens <- density( x = v, 
                       bw = 'nrd0', 
                       adjust = 1, 
                       kernel = 'gaussian', 
                       from = list('mean'=-2, 'slope'=0 )[[parameter]],
                       to = list('mean'=1, 'slope'=1 )[[parameter]], 
                       n = 100)
      
      X <- dens$x
      mf <- list('mean'=0.3, 'slope'=0.10)[[parameter]]
      Y <- dens$y * mf + cond_no
      
      # print(c(X, max(X), rev(X), min(X)))
      # print(c(Y, rep(cond_no, 2+length(Y))) + 0.2)

      polygon(x = c(X, max(X), rev(X), min(X)),
              y = c(Y, rep(cond_no, 2+length(Y))) + 0.2,
              col = Reach::colorAlpha(info$col[cond_no], alpha=34), 
              
              border=FALSE)
      lines(x=X, 
            y=Y + 0.2,
            col=info$col[cond_no], lwd=1)
    }
    
    
    axis(side=2, at=c(1,2,3,4), labels=c('ipsi\nat', 'contra\nat', 'ipsi\naway', 'contra\naway'), las=2)
    axis(side=1, at=list('mean'=c(-2,-1,0,1),'slope'=c(0,0.5,1))[[parameter]])
    
  }
  
  if (target %in% c('svg','pdf','png','tiff')) {
    dev.off()
  }
  
}