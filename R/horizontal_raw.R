
# Horizontal Distance task variant -----

## loading the raw (non-BIDSified) data -----

loadDistHorizontalData <- function() {
  
  IDs <- findParticipants(task='distHorizontal')
  
  allData <- list()
  
  for (ID in IDs) {
    
    # print(ID)
    
    participantData <- loadDistHorizontalParticipant(ID)
    
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
  
  taskCCs <- list.files( path = sprintf('../data/%s/color/',task),
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
  
  if (task == 'distHorizontal') {
    runs <- c('LH','RH')
  }
  if (task == 'distBinocular') {
    runs <- c('run')
  }
  
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
      if (task == 'distHorizontal') {
        lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, 'disth', hf, min(nums))
      }
      if (task == 'distBinocular') {
        lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, 'distb', hf, max(nums))
      }
    }
    
  }
  
  return(lastfiles)
  
}



loadDistHorizontalParticipant <- function(ID) {
  
  files <- findTaskFiles(ID, 'distHorizontal')
  
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

processDistHorizontalData <- function() {
  
  allData <- loadDistHorizontalData()
  
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



## main plot -----

plotDistHorizontalPsychometricCurve <- function(target='inline', cluster=NULL) {
  
  
  # ncores   <- parallel::detectCores()
  # usecores <- max(c(1,floor(ncores*0.5)))
  # # usecores <- 16
  # clust    <- parallel::makeCluster(usecores)
  if (!is.null(cluster)) {
    parallel::clusterEvalQ(cl=cluster, source('R/mprobit.R'))
  }
  
  setupFigureFile( target = target, 
                   width = 6, 
                   height = 6, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/distHorizontal_psychometric.%s', target, target))
  
  
  allConditionData <- processDistHorizontalData()
  
  N <- length(unique(allConditionData[[1]]$participant))
  
  # allConditionModels <- logisticDistanceModels(allConditionData)
  
  plot(-1000,-1000,
       xlim=c(-3.5, 3.5), ylim=c(0,1),
       main=sprintf('horizontal distance perception (N=%d)',N), xlab='difference [dva]', ylab='proportion target chosen',
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
    # points(acdf, col=Reach::colorAlpha(scol, alpha=100))
    
    # mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference,
    #                      start = c( 0, 1,   0,   0   ),
    #                      lower = c(-3, 0.2, 0,   0   ),
    #                      upper = c( 3, 3,   0.3, 0.3 )
    #)
    mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference,
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
      CI <- CI.mprobit.serial(data=data.frame(ID=cdf$participant,
                                              x=cdf$Difference,
                                              y=cdf$Targ_chosen),
                              # start = c(0, 1, 0, 0),
                              # lower = c(-3, 0.2, 0, 0),
                              # upper = c(3, 3, 0.3, 0.3),
                              start = c( 0, 1,   0   ),
                              lower = c(-3, 0.2, 0   ),
                              upper = c( 3, 3,   0.3 ),
                              iterations = 1000,
                              n = length(X))
    } else {
      CI <- CI.mprobit(data=data.frame(ID=cdf$participant,
                                       x=cdf$Difference,
                                       y=cdf$Targ_chosen),
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
  
  legend(x=1.0, y=0.5,
         legend=c('ipsi, at', 'ipsi, away', 'contra, at', 'contra, away'),
         lty=1, col=info$col,
         bty='n')
  
  axis(side=1, at=c(-3,-2,-1,0,1,2,3))
  axis(side=2, at=c(0,0.5,1))
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
  # parallel::stopCluster(clust)
  
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
  # df$margL <- NA
  # df$margU <- NA
  df$LR    <- NA
  df$PSE   <- NA
  df$slope <- NA
  
  for (i in 1:nrow(df)) {
    participant <- df$participant[i]
    eye         <- df$Eye[i]
    location    <- df$Location[i]
    
    # filter the data for the current combination
    subdf <- data[data$participant == participant & data$Eye == eye & data$Location == location, ]
    
    mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
                        start = c(-0.5,   1, 0  ),
                        lower = c(-3,   0.3, 0  ),
                        upper = c( 3,     3, 0.3) )
    # mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
    #                     start <- c(-0.5,   1, 0   ),
    #                     lower <- c(-3,   0.3, 0   ),
    #                     upper <- c( 3,     3, 0.3 ) )
    
    df$mean[i]  <- mod$par[1]
    df$sd[i]    <- mod$par[2]
    df$LR[i]    <- mod$par[3]
    # df$margU[i] <- mod$par[4]
    
    descr <- descr.mprobit(p=mod$par)
    df$PSE[i]   <- descr$PSE
    df$slope[i] <- descr$slope
    
  }
  
  return(df)
}


doDistHorizontalANOVA <- function(dv="PSE") {
  
  distHorData <- getDistHorANOVAdata()
  distOrgData <- getDistANOVAdata()
  
  # distOrgData <- distOrgData[which(distOrgData$participant %in% distHorData$participant),]
  # distHorData <- distHorData[which(distHorData$participant %in% distOrgData$participant),]
  
  distHorData$orientation = 'horizontal'
  distOrgData$orientation = 'tilted'
  
  distHorData$participant <- sprintf('H_%s', distHorData$participant)
  distOrgData$participant <- sprintf('O_%s', distOrgData$participant)
  
  
  AOVdata <- rbind(distHorData, distOrgData)
  

  # fit the model
  org_bias_aov <- afex::aov_ez(
    id = "participant",
    dv = dv,
    data = distOrgData,
    within = c("Eye", "Location"),
  )
  
  cat(sprintf("==== Original Distance %s ANOVA:\n\n",dv))
  
  print(org_bias_aov)
  
  
  
  # fit the model
  org_bias_aov <- afex::aov_ez(
    id = "participant",
    dv = dv,
    data = distHorData,
    within = c("Eye", "Location"),
  )
  
  cat(sprintf("\n==== Horizontal Distance %s ANOVA:\n\n",dv))
  
  print(org_bias_aov)
  
  # fit the model
  bias_aov <- afex::aov_ez(
    id = "participant",
    dv = dv,
    data = AOVdata,
    within = c("Eye", "Location"),
    between = "orientation"
  )
  
  cat(sprintf("\n==== Distance/Horizontal %s ANOVA:\n\n",dv))
  
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

# Binocular Distance task variant ----

## loading the data -----

loadDistBinocularData <- function() {
  
  IDs <- findParticipants(task='distBinocular')
  
  allData <- list()
  
  for (ID in IDs) {
    
    # print(ID)
    
    participantData <- loadDistBinocularParticipant(ID)
    
    if (is.data.frame(participantData)) {
      allData[[ID]] <- participantData
    } else {
      cat(sprintf('Participant %s has no data\n', ID))
    }
    
  }
  
  return(allData)
  
}


loadDistBinocularParticipant <- function(ID) {
  
  files <- findTaskFiles(ID, 'distBinocular')
  
  allData <- NA
  
  writeoutput <- TRUE
  
  for (hf in c('run')) {
    
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
    
    # df$HemiField <- list('LH'='left', 'RH'='right')[[hf]]
    
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

processDistBinocularData <- function() {
  
  allData <- loadDistBinocularData()
  
  info <- data.frame( area = c('bsa', 'out') )
  
  allConditionData <- list()
  
  for (cond_no in c(1:nrow(info))) {
    
    # select rows that correspond to one of the 4 conditions:
    area <- info$area[cond_no]
    
    condition_data <- NA
    
    for (participant in names(allData)) {
      
      df <- allData[[participant]]
      
      if (area == 'bsa') {
        a_idx <- which(df$Targ_loc    %in% c('left-mid','righ-mid'))
      } else {
        a_idx <- which(df$Targ_loc %notin% c('left-mid','righ-mid'))
      }
      
      idx <- a_idx
      

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
    
    allConditionData[[sprintf('%s', area)]] <- condition_data
    
  }
  
  return(allConditionData)
  
}

## main plot -----

plotDistBinocularPsychometricCurve <- function(target='inline', cluster=NULL) {
  
  
  # ncores   <- parallel::detectCores()
  # usecores <- max(c(1,floor(ncores*0.5)))
  # # usecores <- 16
  # clust    <- parallel::makeCluster(usecores)
  if (!is.null(cluster)) {
    parallel::clusterEvalQ(cl=cluster, source('R/mprobit.R'))
  }
  
  setupFigureFile( target = target, 
                   width = 5, 
                   height = 5, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/distBinocular_psychometric.%s', target, target))
  
  
  allConditionData <- processDistBinocularData()
  
  N <- length(unique(allConditionData[[1]]$participant))
  
  # allConditionModels <- logisticDistanceModels(allConditionData)
  
  plot(-1000,-1000,
       xlim=c(-3.5, 3.5), ylim=c(0,1),
       main=sprintf('binocular distance perception (N=%d)',N), xlab='difference [dva]', ylab='proportion target chosen',
       ax=F, bty='n')
  
  lines(x=c(-3.5,3.5),y=c(0.5,0.5),col='gray',lty=3)
  lines(x=c(0,0),y=c(0,1),col='gray',lty=3)
  
  # info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
  #                     area = c('bsa', 'out', 'bsa', 'out'),
  #                     col  = c('red', 'orange', 'blue', 'turquoise'))
  
  info <- data.frame( area = c('bsa', 'out'),
                      col  = c('red', 'orange'))
  
  for (cond_no in c(1:nrow(info))) {
    
    scol <- info$col[cond_no]
    tcol <- Reach::colorAlpha(scol, alpha=34)
    
    cdf <- allConditionData[[sprintf('%s', info$area[cond_no])]]
    
    acdf <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
    lines(acdf, col=Reach::colorAlpha(scol, alpha=100))
    # points(acdf, col=Reach::colorAlpha(scol, alpha=100))
    
    mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference,
                         start = c( 0, 1,   0,   0   ),
                         lower = c(-3, 0.2, 0,   0   ),
                         upper = c( 3, 3,   0.3, 0.3 )
    )
    # mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference, 
    #                      start <- c( 0, 1,   0   ),
    #                      lower <- c(-3, 0.2, 0   ),
    #                      upper <- c( 3, 3,   0.3 )
    #                     )
    
    X <- seq(-3.5,3.5,0.1)
    lines(x=X, y=mprobit(p=mprob$par, x=X), lwd = 1, col = scol)
    
    descr <- descr.mprobit(p=mprob$par)
    PSE <- descr$PSE
    lines(x = rep(PSE,2), y=c(0.5,0),
          col=scol, lty=2)
    
    
    # mod <- allConditionModels[[sprintf('%s_%s', info$eye[cond_no], info$area[cond_no])]]
    # pred <- predict(mod, type = 'response', se.fit = TRUE, newdata=data.frame(Difference=seq(-3.5,3.5,0.1)))
    
    if (is.null(cluster)) {
      CI <- CI.mprobit.serial(data=data.frame(ID=cdf$participant,
                                              x=cdf$Difference,
                                              y=cdf$Targ_chosen),
                              start = c(0, 1, 0, 0),
                              lower = c(-3, 0.2, 0, 0),
                              upper = c(3, 3, 0.3, 0.3),
                              iterations = 1000,
                              n = length(X))
    } else {
      CI <- CI.mprobit(data=data.frame(ID=cdf$participant,
                                       x=cdf$Difference,
                                       y=cdf$Targ_chosen),
                       cluster=cluster,
                       start = c(0, 1, 0, 0),
                       lower = c(-3, 0.2, 0, 0),
                       upper = c(3, 3, 0.3, 0.3),
                       iterations = 1000,
                       n = length(X))
    }
    
    # print(str(CI))
    
    polygon(x = c(CI$X, rev(CI$X)),
            y = c(CI$lo, rev(CI$hi)),
            col = tcol, border = FALSE)
    
  }
  
  legend(x=1.0, y=0.5,
         legend=c('at', 'away'),
         lty=1, col=info$col,
         bty='n')
  
  axis(side=1, at=c(-3.5,-2,-1,0,1,2,3.5))
  axis(side=2, at=c(0,0.5,1))
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
  # parallel::stopCluster(clust)
  
}

## statistics -----

getAllDistBinoData <- function() {
  
  allConditionData <- processDistBinocularData()
  
  info <- data.frame( area = c('bsa', 'out') )
  
  allData <- NA
  
  for (condition in names(allConditionData)) {
    
    cond_df <- allConditionData[[condition]]
    
    
    if (condition %in% c("bsa")) {
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


getDistBinoANOVAdata <- function() {
  
  data <- getAllDistBinoData()
  
  participants <- unique(data$participant)
  locations <- unique(data$Location)
  
  # create a data frame with all combinations of participants, eyes, and locations
  df <- expand.grid(participant = participants, Location = locations)
  
  df$mean  <- NA
  df$sd    <- NA
  df$margL <- NA
  df$margU <- NA
  df$PSE   <- NA
  df$slope <- NA
  
  for (i in 1:nrow(df)) {
    participant <- df$participant[i]
    location   <- df$Location[i]
    
    # filter the data for the current combination
    subdf <- data[data$participant == participant & data$Location == location, ]
    
    mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
                        start = c(-0.5,   1, 0,   0  ),
                        lower = c(-3,   0.3, 0,   0  ),
                        upper = c( 3,     3, 0.3, 0.3) )
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


doDistBinocularANOVA <- function() {
  
  distBinoData <- getDistBinoANOVAdata()
  distOrgData  <- getDistANOVAdata()
  
  distOrgData  <- distOrgData[ which(distOrgData$participant  %in% distBinoData$participant),]
  distBinoData <- distBinoData[which(distBinoData$participant %in% distOrgData$participant),]
  
  
  distOrgData <- aggregate(cbind(mean,sd,margL,margU,PSE,slope) ~ participant + Location, data=distOrgData, FUN=mean)
  
  
  distBinoData$presentation = 'binocular'
  distOrgData$presentation = 'monocular'
  
  AOVdata <- rbind(distBinoData, distOrgData)
  
  
  # fit the model
  org_bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = distOrgData,
    within = c("Location"),
  )
  
  cat("==== Original Distance Bias ANOVA:\n\n")
  
  print(org_bias_aov)
  
  
  
  # fit the model
  org_bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = distBinoData,
    within = c("Location"),
  )
  
  cat("==== Binocular Distance Bias ANOVA:\n\n")
  
  print(org_bias_aov)
  
  # fit the model
  bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = AOVdata,
    within = c("Location", "presentation"),
  )
  
  cat("==== Distance/Horizontal Bias ANOVA:\n\n")
  
  print(bias_aov)
  
  
  
}


# GLM test -----


testGLM <- function() {
  
  allData <- getAllDistHorData()
  
  # fit the model
  mod <- glm(Targ_chosen ~ Difference * Eye * Location, 
             data = allData,
             family = binomial(link = 'logit'))
  
  print(summary(mod))
  
  # anova(mod, test = "Chisq")
  
  # print(emmeans::emmeans(mod, specs = c('Eye','Location')))
  
  return(mod)
  
}