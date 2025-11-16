
# Horizontal Distance task variant -----

## loading the raw (non-BIDSified) data -----

loadRecDistHorizontalData <- function() {
  
  IDs <- findParticipants(task='recDistHorizontal')
  
  allData <- list()
  
  for (ID in IDs) {
    
    # print(ID)
    
    participantData <- loadRecDistHorizontalParticipant(ID)
    
    if (is.data.frame(participantData)) {
      allData[[ID]] <- participantData
    } else {
      cat(sprintf('Participant %s has no data\n', ID))
    }
    
  }
  
  return(allData)
  
}



findParticipants <- function(task) {
  
  IDs <- c()
  
  taskCCs <- list.files( path = sprintf('../data/%s/',task),
                         pattern = '*.txt')
    
  for (CC in taskCCs) {
    
    us1_idx <- unlist(gregexpr('_', CC))[1]
    
    ppID <- substr( x     = CC, 
                    start = 1, 
                    stop  = us1_idx-1)
    
    IDs <- c(IDs, ppID)
    
  }
  
  
  IDs <- unique(IDs)
  
  IDs <- IDs[which(nchar(IDs) > 8)]
  
  # those are the only ones we want to work on:
  # return(c('tor109f9b'))
  return(IDs)
  
}

findTaskFiles <- function(ID, task) {
  
  alltaskfiles <- list.files( path = sprintf('../data/%s/', task) )
  Pfiles <- alltaskfiles[which(substr(alltaskfiles, 1, nchar(ID)) == ID)]
  
  lastfiles <- list()
  
  # if (task == 'recDistHorizontal') {
  runs <- c('LH','RH')
  # }
  # if (task == 'distBinocular') {
  #   runs <- c('run')
  # }
  
  for (hf in runs) {
    
    # files for this hemifield:
    # print(Pfiles)
    # print(grepl(sprintf('_%s_', hf), Pfiles))
    # 
    Hfiles <- Pfiles[which(grepl(sprintf('_%s_', hf), Pfiles))]
    
    nums <- c()
    
    for (hfile in Hfiles) {
      # find the first period (just before the file extension)
      pidx <- unlist(gregexpr('[.]', hfile))[1]
      # find the last underscore (just before the calibration number)
      uidx <- tail(unlist(gregexpr('_', hfile)), n=1)
      # extract number/index of color calibration file:
      nums <- c(nums, as.numeric(substr(hfile, uidx+1, pidx-1)))
    }
    # for which ever blind spot map is not there, we can skip and not look for task data
    if (length(nums) == 0) {
      # cat(sprintf('no data file found for %s, %s hemifield\n', task, list('RH'='right','LH'='left')[[hf]]))
      lastfiles[[hf]] <- NULL
    } else {
      # if (task == 'distHorizontal') {
      lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, 'disth', hf, min(nums))
      # }
      # if (task == 'distBinocular') {
      #   lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, 'distb', hf, max(nums))
      # }
    }
    
  }
  
  return(lastfiles)
  
}



loadRecDistHorizontalParticipant <- function(ID) {
  
  files <- findTaskFiles(ID, 'recDistHorizontal')
  
  allData <- NA
  
  writeoutput <- TRUE
  
  for (hf in c('LH','RH')) {
    
    if (is.null(files[[hf]])) {
      writeoutput <- FALSE
      next
    } 
    df <- read.delim(
      file = files[[hf]],
      skip=1,
      header=TRUE,
      sep='\t'
    )
    df <- read.csv(
      file = files[[hf]], 
      stringsAsFactors = FALSE
    )
    
    # df <- df[which(df$Resp %in% c("1","2")),]
    df$abort <- 1
    df$abort[which(df$Resp %in% c("1","2"))] <- 0
    df$abort[which(df$Resp == 'auto abort')] <- 1
    df$abort[which(df$Resp == 'abort')]      <- 2
    
    df$Resp[!which(df$Resp %in% c("1","2"))] <- NA
    
    suppressWarnings( df$Resp <- as.numeric(df$Resp) ) # is this smart or the opposite?
    
    df$Targ_chosen[!which(df$Targ_chosen %in% c("False", "True"))] <- NA
    df$Targ_chosen <- as.logical(df$Targ_chosen)
    
    df$Reversal[!which(df$Reversal %in% c("False", "True"))] <- NA
    df$Reversal <- as.logical(df$Reversal)
    
    df$HemiField <- list('LH'='left', 'RH'='right')[[hf]]
    
    if (is.data.frame(allData)) {
      allData <- rbind(allData, df)
    } else {
      allData <- df
    }
    
  }
  
  if (writeoutput) {
    return(allData)
  } else {
    return(FALSE)
  }

}

## processing -----

processRecDistHorizontalData <- function() {
  
  allData <- loadRecDistHorizontalData()
  
  info <- data.frame( eye    = c('ipsi','ipsi','cont','cont','ipsi','ipsi','cont','cont' ),
                      area   = c( 'bsa', 'out', 'bsa', 'out', 'bsa', 'out', 'bsa', 'out' ),
                      disdif = c( 'pos', 'pos', 'pos', 'pos', 'neg', 'neg', 'neg', 'neg' )  )
  
  allConditionData <- list()
  
  for (cond_no in c(1:nrow(info))) {
    
    # select rows that correspond to one of the 4 conditions:
    eye    <- info$eye[   cond_no]
    area   <- info$area[  cond_no]
    disdif <- info$disdif[cond_no]
    
    condition_data <- NA
    
    for (participant in names(allData)) {
      
      df <- allData[[participant]]
      
      df <- df[which(df$Resp %in% c(1,2)),]
      
      df$Difference <- df$recoveredDiffs
      
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
      
      if (disdif == 'pos') {
        d_idx <- which( (df$targOffset - df$foilOffset) > 0 ) # target chosen less?
      } else {
        d_idx <- which( (df$targOffset - df$foilOffset) < 0 )
      }
      
      idx <- intersect(d_idx, intersect(e_idx, a_idx))
      
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
    
    allConditionData[[sprintf('%s_%s_%s', eye, area, disdif)]] <- condition_data
    
  }
  
  return(allConditionData)
  
}



## main plot -----

plotRecDistHorizontalPsychometricCurve <- function(target='inline', cluster=NULL) {
  
  
  # ncores   <- parallel::detectCores()
  # usecores <- max(c(1,floor(ncores*0.5)))
  # # usecores <- 16
  # clust    <- parallel::makeCluster(usecores)
  if (!is.null(cluster)) {
    parallel::clusterEvalQ(cl=cluster, source('R/mprobit.R'))
  }
  
  setupFigureFile( target = target, 
                   width = 8, 
                   height = 5, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/distHorRecOffset_psychometric.%s', target, target))
  
  
  allConditionData <- processRecDistHorizontalData()
  
  N <- length(unique(allConditionData[[1]]$participant))
  
  # allConditionModels <- logisticDistanceModels(allConditionData)
  
  layout(mat=matrix(data=c(1:2),nrow=1))
  
  for (disdif in c('pos', 'neg')) {
    
    plot(-1000,-1000,
         xlim=c(-3.5, 3.5), ylim=c(0,1),
         main=sprintf('hor. dist. perception %s (N=%d)', disdif, N), xlab='difference [dva]', ylab='proportion target chosen',
         ax=F, bty='n')
    
    lines(x=c(-3.5,3.5),y=c(0.5,0.5),col='gray',lty=3)
    lines(x=c(0,0),y=c(0,1),col='gray',lty=3)
    
    info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                        area = c('bsa', 'out', 'bsa', 'out'),
                        col  = c('red', 'orange', 'blue', 'turquoise'))
    
    
    for (cond_no in c(1:nrow(info))) {
      
      scol <- info$col[cond_no]
      tcol <- Reach::colorAlpha(scol, alpha=34)
      
      acdf <- allConditionData[[sprintf('%s_%s_%s', info$eye[cond_no], info$area[cond_no], disdif)]]
      
      temp_acdf <- aggregate(Targ_chosen ~ Difference, data=acdf, FUN=mean)
      # lines(temp_acdf, col=Reach::colorAlpha(scol, alpha=10))
      points(x=temp_acdf$Difference, y=temp_acdf$Targ_chosen,
             pch=16, 
             col=Reach::colorAlpha(scol, alpha=50),
             cex=1.5)
      
      # points(acdf, col=Reach::colorAlpha(scol, alpha=100))
      
      # mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference,
      #                      start = c( 0, 1,   0,   0   ),
      #                      lower = c(-3, 0.2, 0,   0   ),
      #                      upper = c( 3, 3,   0.3, 0.3 )
      #)
      mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference, w=acdf$count,
                           start = c( 0, 1,   0   ),
                           lower = c(-3, 0.2, 0   ),
                           upper = c( 3, 3,   0.3 )
      )
      
      X <- seq(-3.5,3.5,0.1)
      lines(x=X, y=mprobit(p=mprob$par, x=X), lwd = 1, col = scol)
      
      descr <- descr.mprobit(p=mprob$par)
      PSE <- descr$PSE
      lines(x = rep(PSE,2), y=c(0.5,0),
            col=scol, lty=2)
      
      
      # mod <- allConditionModels[[sprintf('%s_%s', info$eye[cond_no], info$area[cond_no])]]
      # pred <- predict(mod, type = 'response', se.fit = TRUE, newdata=data.frame(Difference=seq(-3.5,3.5,0.1)))
      
      if (is.null(cluster)) {
        CI <- CI.mprobit.serial(data=data.frame(ID=acdf$participant,
                                                x=acdf$Difference,
                                                y=acdf$Targ_chosen,
                                                w=acdf$count),
                                # start = c(0, 1, 0, 0),
                                # lower = c(-3, 0.2, 0, 0),
                                # upper = c(3, 3, 0.3, 0.3),
                                start = c( 0, 1,   0   ),
                                lower = c(-3, 0.2, 0   ),
                                upper = c( 3, 3,   0.3 ),
                                iterations = 1000,
                                n = length(X))
      } else {
        CI <- CI.mprobit(data=data.frame(ID=acdf$participant,
                                         x=acdf$Difference,
                                         y=acdf$Targ_chosen,
                                         w=acdf$count),
                         cluster=cluster,
                         # start = c(0, 1, 0, 0),
                         # lower = c(-3, 0.2, 0, 0),
                         # upper = c(3, 3, 0.3, 0.3),
                         start = c( 0, 1,   0   ),
                         lower = c(-3, 0.2, 0   ),
                         upper = c( 3, 3,   0.3 ),
                         iterations = 1000,
                         n = length(X))
      }
      
      
      polygon(x = c(CI$X, rev(CI$X)),
              y = c(CI$lo, rev(CI$hi)),
              col = tcol, border = FALSE)
      
    }
    
    legend(x=0.2, y=0.5,
           legend=c('ipsi, at', 'ipsi, away', 'contra, at', 'contra, away'),
           lty=1, col=info$col,
           bty='n')
    
    axis(side=1, at=c(-3,-2,-1,0,1,2,3))
    axis(side=2, at=c(0,0.5,1))
  }
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
  # parallel::stopCluster(clust)
  
}

plotPSEoverOffsetDifferences <- function(target='inline', task='recDistHorizontal') {
  
  setupFigureFile( target = target, 
                   width = 8, 
                   height = 8, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/shift_PSEs_%s.%s', target, task, target))
  
  layout(mat=matrix(c(1:4),nrow=2,byrow=T))
  
  # made a function in 'utilities.R' to load data:
  allData <- loadTaskData(task=task)
  
  data <- NA 
  for (ID in names(allData)) {
    ppdf <- allData[[ID]]
    ppdf$participant <- ID
    if (is.data.frame(data)) {
      data <- rbind(data, ppdf)
    } else {
      data <- ppdf
    }
  }
  

  # data <- data[which( ! is.na(data$Targ_chosen)) , ]
  data <- data[which( data$abort == 0 ) , ]
  data$offsetDiff <- round(data$targOffset - data$foilOffset,2)
  # print(sort(unique(data$offsetDiff))) # 18 unique values
  
  offsetDiffs <- sort(unique(data$offsetDiff))
  PSEs <- c()
  
  plot(NULL,NULL,
       main='',xlab='difference',ylab='proportion target chosen',
       xlim=c(-3.5, 3.5), ylim=c(0,1),
       ax=F,bty='n')
  
  lines(x=c(0,0),y=c(0,1),col='#999999',lty=3)
  lines(x=c(-3.5,3.5),y=c(0.5,0.5),col='#999999',lty=3)
  
  for (offsetDiff in offsetDiffs) {
    
    odf <- data[which(data$offsetDiff == offsetDiff),]
    
    # print(str(odf))
    
    mprob <- fit.mprobit(y=odf$Targ_chosen, x=odf$Difference, w=NULL,
                         start = c( 0, 1,   0   ),
                         lower = c(-3, 0.2, 0   ),
                         upper = c( 3, 3,   0.3 )
    )
    
    
    
    diffrange <- range(unique(odf$Difference))
    X <- seq(diffrange[1], diffrange[2], length.out=50)
    W <- (offsetDiff - min(data$offsetDiff) ) / diff(range(data$offsetDiff))
    scol <- Reach::colorMix(a='red', b='blue', balance=c(W,1-W))
    lines(x=X, y=mprobit(p=mprob$par, x=X), lwd = 1, col = scol)
    
    descr <- descr.mprobit(p=mprob$par)
    PSEs <- c(PSEs, descr$PSE)
    
    # print(PSE)
    
  }
  
  axis(side=1, at=c(-3,-2,-1,0,1,2,3))
  axis(side=2, at=c(0,0.5,1))
  
  colo <- '#0066FFFF'
  colt <- '#0066FF33'
  
  plot(NULL,NULL, 
       xlim=c(-2,2), ylim=c(-2,2),
       xlab='shift difference [dva]', ylab='PSE [dva]',
       bty='n', ax=F,  asp=1)
  
  lines(x=c(-2,2), y=c(0,0), col='#999999', lty=3)
  lines(y=c(-2,2), x=c(0,0), col='#999999', lty=3)
  lines(x=c(-2,2), y=c(-2,2), col='#999999', lty=3)
  
  
  points(x=offsetDiffs, y=PSEs,
         pch=16, col=colt, cex=2)
  
  lmfit <- lm(PSEs ~ offsetDiffs)
  
  at = range(offsetDiffs)
  
  coef <- lmfit$coefficients
  print(coef)
  print(cor.test(x=offsetDiffs, y=PSEs))
  lines(at, coef[1]+(at*coef[2]), col=colo, lty=2)
  # 
  ci <- predict( lmfit,
                 newdata=data.frame(offsetDiffs=seq(at[1],at[2],length.out=40)),
                 interval = "confidence")

  X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=colt,border=NA)

  
  axis(side=1,at=c(-2,-1,0,1,2))
  axis(side=2,at=c(-2,-1,0,1,2))
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out'),
                      col  = c('red', 'orange', 'blue', 'turquoise'))
  
  
  predPSEs <- offsetDiffs*coef[2]
  
  ncores   <- parallel::detectCores()
  usecores <- max(c(1,floor(ncores*0.5)))
  cluster    <- parallel::makeCluster(usecores)
  parallel::clusterEvalQ(cl=cluster, source('R/mprobit.R'))
  
  for (version in c('original', 'corrected')) {
  
    plot(NULL,NULL,
         main=version,xlab='difference [dva]',ylab='proportion target chosen',
         xlim=c(-3.5, 3.5), ylim=c(0,1),
         ax=F,bty='n')
    
    lines(x=c(0,0),y=c(0,1),col='#999999',lty=3)
    lines(x=c(-3.5,3.5),y=c(0.5,0.5),col='#999999',lty=3)
    
    if (version == 'corrected') {
      df <- data
      # df$Difference <- df$Difference + (df$Difference - df$recoveredDiffs)
      for (idx in c(1:length(offsetDiffs))) {
        offsetDiff <- offsetDiffs[idx]
        PSE <- predPSEs[idx]
        df$Difference[which(df$offsetDiff == offsetDiff)] <- df$Difference[which(df$offsetDiff == offsetDiff)] - PSE
      }
      
    } else {
      df <- data
    }
    
    for (cond_no in c(1:nrow(info))) {
      
      eye    <- info$eye[   cond_no]
      area   <- info$area[  cond_no]
      col    <- info$col[   cond_no]
      
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
      cdf <- df[idx,]
      
      acdf <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
      # lines(acdf, col=Reach::colorAlpha(col, 34))
      
      mprob <- fit.mprobit(y=cdf$Targ_chosen, x=cdf$Difference,
                           start = c( 0, 1,   0   ),
                           lower = c(-3, 0.2, 0   ),
                           upper = c( 3, 3,   0.3 )
      )
      
      X <- seq(-3.5,3.5,0.1)
      lines(x=X, y=mprobit(p=mprob$par, x=X), lwd = 1, col = col)
      
      # bootstrap confidence intervals:
      
      CI <- CI.mprobit(data=data.frame(ID=cdf$participant,
                                       x=cdf$Difference,
                                       y=cdf$Targ_chosen),
                       cluster=cluster,
                       start = c( 0, 1,   0   ),
                       lower = c(-3, 0.2, 0   ),
                       upper = c( 3, 3,   0.3 ),
                       iterations = 1000,
                       n = length(X))
      
      polygon(x = c(CI$X, rev(CI$X)),
              y = c(CI$lo, rev(CI$hi)),
              col = Reach::colorAlpha(col, 34), border = FALSE)
      
      
    }
    
    axis(side=1, at=c(-3,-2,-1,0,1,2,3))
    axis(side=2, at=c(0,0.5,1))  
  
  }
  

  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
  
}

## statistics -----


getAllDistHorData <- function() {
  
  allConditionData <- processDistHorizontalData()
  
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


getDistHorANOVAdata <- function() {
  
  data <- getAllDistHorData()
  
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
                        start = c(-0.5,   1, 0,   0  ),
                        lower = c(-3,   0.3, 0,   0  ),
                        upper = c( 3,     3, 0.3, 0.3) )
    # mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
    #                     start <- c(-0.5,   1, 0   ),
    #                     lower <- c(-3,   0.3, 0   ),
    #                     upper <- c( 3,     3, 0.3 ) )
    
    df$mean[i]  <- mod$par[1]
    df$sd[i]    <- mod$par[2]
    df$margL[i] <- mod$par[3]
    df$margU[i] <- mod$par[4]
    
    descr <- descr.mprobit(p=mod$par)
    df$PSE[i]   <- descr$PSE
    df$slope[i] <- descr$slope
    
  }
  
  return(df)
}


doDistHorizontalANOVA <- function() {
  
  distHorData <- getDistHorANOVAdata()
  distOrgData <- getDistANOVAdata()
  
  distOrgData <- distOrgData[which(distOrgData$participant %in% distHorData$participant),]
  distHorData <- distHorData[which(distHorData$participant %in% distOrgData$participant),]
  
  distHorData$orientation = 'horizontal'
  distOrgData$orientation = 'tilted'
  
  AOVdata <- rbind(distHorData, distOrgData)
  

  # fit the model
  org_bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = distOrgData,
    within = c("Eye", "Location"),
  )
  
  cat("==== Original Distance Bias ANOVA:\n\n")
  
  print(org_bias_aov)
  
  
  
  # fit the model
  org_bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = distHorData,
    within = c("Eye", "Location"),
  )
  
  cat("==== Horizontal Distance Bias ANOVA:\n\n")
  
  print(org_bias_aov)
  
  # fit the model
  bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = AOVdata,
    within = c("Eye", "Location", "orientation"),
  )
  
  cat("==== Distance/Horizontal Bias ANOVA:\n\n")
  
  print(bias_aov)
  
  
  
}

plotDistHorizontalIpsiEffectDistribution <- function() {
  
  distHorData <- getDistHorANOVAdata()
  
  ipsiData <- distHorData[which(distHorData$Location == 'blindspot'),]
  
  participants <- unique(ipsiData$participant)
  
  participant <- c()
  effect <- c()
  
  for (ppID in participants) {
    contra  <- ipsiData$PSE[which(ipsiData$participant == ppID & ipsiData$Eye == 'contralateral')]
    ipsi    <- ipsiData$PSE[which(ipsiData$participant == ppID & ipsiData$Eye == 'ipsilateral')]
    Peffect <- contra - ipsi
    
    participant <- c(participant, ppID)
    effect <- c(effect, Peffect)
  }
  
  df <- data.frame(participant=participant, effect=effect)
  
  effd <- density(df$effect,
                  n    = 351,
                  from =  -2,
                  to   =   2)
  
  plot(main='',xlab='effect [dva]', ylab='density',
       x=NULL, y=NULL,
       xlim=c(-2,2), ylim=c(-0.2, max(effd$y)*1.1),
       ax=F)
  
  lines(effd, col='blue', lwd=2)
  
  points(x=df$effect, y=rep(-0.1, length(df$effect)),
         pch=16, col='#0000FF33', cex=2.5)
  
  axis(side=1, at=c(-2,-1,0,1,2))
  axis(side=2, at=seq(0, 0.6, 0.15))
  
  uno <- Reach::multiModalFit( x = df$effect,
                               n = 1)
  dos <- Reach::multiModalFit( x = df$effect,
                               n = 2)
  # 
  # uno.nll <- Reach::multiModalModelNLL(par=uno, x = df$effect)
  # dos.nll <- Reach::multiModalModelNLL(par=dos, x = df$effect)
  # 
  # print(uno)
  # print(dos)
  

  
  uno.pd <- Reach::multiModalModel(par=uno, x = seq(-2,2,.01))
  
  dos.pd <- Reach::multiModalModel(par=dos, x = seq(-2,2,.01))
  
  lines(x=seq(-2,2,.01), y=uno.pd, col='red', lty=2)
  lines(x=seq(-2,2,.01), y=dos.pd, col='red', lty=3)
  
  print(ks.test(df$effect, 'pnorm', mean=mean(df$effect), sd=sd(df$effect)))
  
}

# Eyetracker calibration sequence -----

extractCalibrationSequence <- function() {
  
  IDs <- findParticipants(task='distHorizontal')
  
  calibration_log <- NA
  
  for (ID in IDs) {
    
    print(ID)
    
    # find the last eyetracking files for each participant and hemifield:
    folder <- sprintf('../data/distHorizontal/eyetracking/%s/', ID)
    alltaskfiles <- list.files( path = folder, pattern='*.csv' )
    alltaskfiles <- alltaskfiles[which(grepl('dstH', alltaskfiles))]
    lastfiles <- list()
    for (hf in c('LH','RH')) {
      Hfiles <- alltaskfiles[which(grepl(sprintf('%s', hf), alltaskfiles))]
      nums <- c()
      for (hfile in Hfiles) {
        # print(hfile)
        pidx <- unlist(gregexpr('[.]', hfile))[1]
        uidx <- tail(unlist(gregexpr(hf, hfile)), n=1)
        # print(pidx)
        # print(uidx)
        nums <- c(nums, as.numeric(substr(hfile, uidx+2, pidx-1)))
      }
      lastfiles[[hf]] <- sprintf('%sdstH%s%d.csv', folder, hf, max(nums))
    }
    
    # print(lastfiles)
    
    for (hf in c('LH','RH')) {
      df <- fixLiveTrack(read.csv( file = lastfiles[[hf]], header=TRUE ))
      # write.csv(df, file = sprintf('../data/distHorizontal/eyetracking/%s/clean_distH_%s.csv', ID, hf), row.names=FALSE)
      
      comments <- df$Comment[which(df$Comment != ' ')]
      
      trial <- c()
      calibrations <- c()
      
      trial_no <- 0
      N_calibrations <- 0
      for (comment in comments) {
        if (grepl(' calibration', comment)) {
          N_calibrations <- N_calibrations + 1
        }
        if (grepl(' start trial', comment)) {
          trial_no <- as.numeric(substr(comment, 14, nchar(comment)))
          cat(sprintf('%s, %s, trial %d, calibrations: %d\n', ID, hf, trial_no, N_calibrations))
          
          trial        <- c(trial, trial_no)
          calibrations <- c(calibrations, N_calibrations)
          
          N_calibrations <- 0
          
        }
        
        
        
      }
      
      phfdf <- data.frame(trial=trial, calibrations=calibrations)
      phfdf$ID <- ID
      phfdf$hemifield <- hf
      phfdf$file <- lastfiles[[hf]] # this is the eye-tracking file... what about the behavioral response file?
      
      if (is.data.frame(calibration_log)) {
        calibration_log <- rbind(calibration_log, phfdf)
      } else {
        calibration_log <- phfdf
      }
      
    }
    
  }
  
  write.csv(calibration_log, file = '../data/distHorizontal/calibration_log.csv', row.names=FALSE)
  
}


checkCalibrationSanity <- function() {
  
  df <- read.csv(file = '../data/distHorizontal/calibration_log.csv', header=TRUE, stringsAsFactors = FALSE)
  
  participants <- unique(df$ID)
  
  for (ID in participants) {
    
    for (hf in c('LH','RH')) {
      
      subdf <- df[which(df$ID == ID & df$hemifield == hf),]
      
      print(c(ID, hf, max(subdf$trial), sum(subdf$calibrations)))
      # if (max(subdf$trial) < 240) {
      #   print(c(ID, hf, max(subdf$trial), sum(subdf$calibrations)))
      # }
      
      
    }
    
  }
  
  
}


fixLiveTrack <- function(df) {
  
  df <- df[which(df$Timestamp != 'Timestamp'),]
  
  df <- df[,which(names(df) != 'Trigger')]
  
  for (colname in names(df)) {
    
    if (colname != 'Comment') {
      df[,colname] <- as.numeric(df[,colname])
    }
    
  }
  
  # find spots where time goes back down (calibration):
  idxs <- which(diff(df$Timestamp) < 0)
  
  # how many samples are there:
  nsamples <- length(df$Timestamp)
  
  # loop through reset points:
  for (idx_no in c(1:length(idxs))) {
    idx <- idxs[idx_no]
    lval <- df$Timestamp[idx]
    if (idx_no == length(idxs)) {
      tidx <- c((idx+1):nsamples)
    } else {
      tidx <- c((idx+1):idxs[idx_no+1])
    }
    df$Timestamp[tidx] <- df$Timestamp[tidx] + lval
  }
  
  # fix instances where timestamps go up too much:
  # find spots where time goes back down (calibration):
  idxs <- which(diff(df$Timestamp) > 2500)
  
  # loop through reset points:
  for (idx_no in c(1:length(idxs))) {
    idx <- idxs[idx_no]
    lval <- df$Timestamp[idx] - df$Timestamp[idx+1] + 2000
    if (idx_no == length(idxs)) {
      tidx <- c((idx+1):nsamples)
    } else {
      tidx <- c((idx+1):idxs[idx_no+1])
    }
    df$Timestamp[tidx] <- df$Timestamp[tidx] + lval
  }
  
  
  return(df)
  
}

