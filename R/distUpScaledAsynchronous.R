
# Horizontal Distance task variant -----

## loading the raw (non-BIDSified) data -----

loadDistUpScaledAsynchronousData <- function() {
  
  IDs <- findParticipants(task='distUpScaledAsynchronous')
  
  allData <- list()
  
  for (ID in IDs) {
    
    # print(ID)
    
    participantData <- loadDistUpScaledAsynchronousParticipant(ID)
    
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
  #return(c('tora05f8a'))
  return(IDs)
  
}

findTaskFiles <- function(ID, task) {
  
  alltaskfiles <- list.files( path = sprintf('../data/%s/', task) )
  Pfiles <- alltaskfiles[which(substr(alltaskfiles, 1, nchar(ID)) == ID)]
  
  taskID <- list('distAsynchronous'         = 'dista',
                 'distUpScaledAsynchronous' = 'distusa')[[task]]
  
  lastfiles <- list()
  
  runs <- c('LH','RH')
  
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
      lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, taskID, hf, min(nums))
    }
    
  }
  
  return(lastfiles)
  
}



loadDistUpScaledAsynchronousParticipant <- function(ID) {
  
  # cat('loadDistUpScaledAsynchronousParticipant()\n')
  
  files <- findTaskFiles(ID, 'distUpScaledAsynchronous')
  
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

processDistUpScaledAsynchronousData <- function() {
  
  allData <- loadDistUpScaledAsynchronousData()
  
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

plotDistUpScaledAsynchronousPsychometricCurve <- function(target='inline', cluster=NULL) {
  
  
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
                   filename = sprintf('doc/fig/%s/distUpScAsynch_psychometric.%s', target, target))
  
  
  allConditionData <- processDistUpScaledAsynchronousData()
  
  N <- length(unique(allConditionData[[1]]$participant))
  
  # allConditionModels <- logisticDistanceModels(allConditionData)
  
  plot(-1000,-1000,
       xlim=c(-4.2, 4.2), ylim=c(0,1),
       main=sprintf('upsc. async. hor. dist. perception (N=%d)',N), xlab='difference [dva]', ylab='proportion target chosen',
       ax=F, bty='n')
  
  lines(x=c(-4.2,4.2),y=c(0.5,0.5),col='gray',lty=3)
  lines(x=c(0,0),y=c(0,1),col='gray',lty=3)
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out'),
                      col  = c('red', 'orange', 'blue', 'turquoise'))
  
  
  for (cond_no in c(1:nrow(info))) {
    
    scol <- info$col[cond_no]
    tcol <- Reach::colorAlpha(scol, alpha=34)
    
    cdf <- allConditionData[[sprintf('%s_%s', info$eye[cond_no], info$area[cond_no])]]
    
    acdf <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
    cdf$count <- 1
    acdf$count <- aggregate(count ~ Difference, data=cdf, FUN=sum)$count
    
    lines(acdf, col=Reach::colorAlpha(scol, alpha=100))
    # points(acdf, col=Reach::colorAlpha(scol, alpha=100))
    
    # mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference,
    #                      start = c( 0, 1,   0,   0   ),
    #                      lower = c(-3, 0.2, 0,   0   ),
    #                      upper = c( 3, 3,   0.3, 0.3 )
    # )
    mprob <- fit.mprobit(y=acdf$Targ_chosen, x=acdf$Difference,
                         w=acdf$count,
                         start = c( 0, 1,   0   ),
                         lower = c(-3, 0.2, 0   ),
                         upper = c( 3, 3,   0.3 )
                        )
    # print(mprob)
    X <- seq(-4.2,4.2,0.1)
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
    #
    #
    polygon(x = c(CI$X, rev(CI$X)),
            y = c(CI$lo, rev(CI$hi)),
            col = tcol, border = FALSE)
    
  }
  
  legend(x=1.0, y=0.5,
         legend=c('ipsi, at', 'ipsi, away', 'contra, at', 'contra, away'),
         lty=1, col=info$col,
         bty='n')
  
  axis(side=1, at=c(-4,-3,-2,-1,0,1,2,3,4))
  axis(side=2, at=c(0,0.5,1))
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
  # parallel::stopCluster(clust)
  
}

## statistics -----


getAllDistUpsAsynchData <- function() {
  
  allConditionData <- processDistUpScaledAsynchronousData()
  
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


getDistUpsAsynchANOVAdata <- function() {
  
  data <- getAllDistUpsAsynchData()
  
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
    eye        <- df$Eye[i]
    location   <- df$Location[i]
    
    # filter the data for the current combination
    subdf <- data[data$participant == participant & data$Eye == eye & data$Location == location, ]
    
    mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
                        start = c(-0.5,   1, 0   ),
                        lower = c(-3,   0.3, 0   ),
                        upper = c( 3,     3, 0.3 ) )
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


doDistUpsAsynchANOVA <- function() {
  
  distUpsAsynchData <- getDistUpsAsynchANOVAdata()
  
  # distHorData <- getDistHorANOVAdata()
  # distOrgData <- getDistANOVAdata()
  # 
  # distOrgData <- distOrgData[which(distOrgData$participant %in% distHorData$participant),]
  # distHorData <- distHorData[which(distHorData$participant %in% distOrgData$participant),]
  # 
  # distHorData$orientation = 'horizontal'
  # distOrgData$orientation = 'tilted'
  # 
  # AOVdata <- rbind(distHorData, distOrgData)
  
  # fit the model
  org_bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = distUpsAsynchData,
    within = c("Eye", "Location"),
  )
  # 
  # cat("==== Original Distance Bias ANOVA:\n\n")
  # 
  print(org_bias_aov)
  # 
  # 
  # 
  # # fit the model
  # org_bias_aov <- afex::aov_ez(
  #   id = "participant",
  #   dv = "PSE",
  #   data = distHorData,
  #   within = c("Eye", "Location"),
  # )
  # 
  # cat("==== Horizontal Distance Bias ANOVA:\n\n")
  # 
  # print(org_bias_aov)
  # 
  # # fit the model
  # bias_aov <- afex::aov_ez(
  #   id = "participant",
  #   dv = "PSE",
  #   data = AOVdata,
  #   within = c("Eye", "Location", "orientation"),
  # )
  # 
  # cat("==== Distance/Horizontal Bias ANOVA:\n\n")
  # 
  # print(bias_aov)
  # 
  # 
  
}

doDistAsynchVariantsANOVA <- function(type='BF') {
  
  if (!is.vector(type)) {
    type <- c(type)
  }
  
  if ('repeated' %in% type) {
    distUpsAsynchData <- getDistUpsAsynchANOVAdata()
    distAsynchData    <- getDistAsynchANOVAdata()
  
    distUpsAsynchData <- distUpsAsynchData[which(distUpsAsynchData$participant %in% distAsynchData$participant),]
    distAsynchData    <- distAsynchData[   which(distAsynchData$participant %in% distUpsAsynchData$participant),]
  
    distUpsAsynchData$scale <- 'upscaled'
    distAsynchData$scale    <- 'regular'
  
  
    AOVdata <- rbind(distAsynchData, distUpsAsynchData)
  
    # fit the model
    within_bias_aov <- afex::aov_ez(
      id = "participant",
      dv = "PSE",
      data = AOVdata,
      within = c("Eye", "Location", "scale"),
      # within = c("Location", "scale"),
    )
    #
    cat("==== Repeated measures ANOVA (N=8):\n\n")
    #
    print(within_bias_aov)
  }
  
  if ('between' %in% type) {
    distUpsAsynchData <- getDistUpsAsynchANOVAdata()
    distAsynchData    <- getDistAsynchANOVAdata()
    
    distUpsAsynchData$participant <- sprintf('ups_%s', distUpsAsynchData$participant)
    distAsynchData$participant    <- sprintf('reg_%s', distAsynchData$participant)
    
    distUpsAsynchData$scale <- 'upscaled'
    distAsynchData$scale    <- 'regular'
    
    AOVdata <- rbind(distAsynchData, distUpsAsynchData)
    
    # fit the model
    between_bias_aov <- afex::aov_ez(
      id = "participant",
      dv = "PSE",
      data = AOVdata,
      within = c("Eye", "Location"),
      # within = c("Location"),
      between = "scale"
    )
    # 
    cat("\n==== Between-subjects ANOVA (N=8 & N=12):\n\n")
    # 
    print(between_bias_aov)
    
  }
  
  if ('lme' %in% type) {
    distUpsAsynchData <- getDistUpsAsynchANOVAdata()
    distAsynchData    <- getDistAsynchANOVAdata()
    
    distUpsAsynchData$scale <- 'upscaled'
    distAsynchData$scale    <- 'regular'
    
    AOVdata <- rbind(distAsynchData, distUpsAsynchData)
    
    
    my_lmer <- lmerTest::lmer(
                              PSE ~ Eye * Location * scale - (1|participant),
                              # PSE ~ Location * scale - (1|participant),
                              na.action = na.exclude,
                              data = AOVdata,
                              REML = TRUE,
                              control = lme4::lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B'))
    )
    
    # print(my_lmer)
    
    cat("\n==== LME-based ANOVA-like analysis (N=12 with missing data):\n\n")
    
    print(anova(my_lmer,ddf='Satterthwaite',type=3))
  }
  
  if ('BF' %in% type) {
    distUpsAsynchData <- getDistUpsAsynchANOVAdata()
    distAsynchData    <- getDistAsynchANOVAdata()
    
    distUpsAsynchData$scale <- 'upscaled'
    distAsynchData$scale    <- 'regular'
    
    AOVdata <- rbind(distAsynchData, distUpsAsynchData)
    
    AOVdata$Eye      <- as.factor(AOVdata$Eye)
    AOVdata$Location <- as.factor(AOVdata$Location)
    AOVdata$scale    <- as.factor(AOVdata$scale)
    
    BFaov <- BayesFactor::anovaBF(
      PSE ~ Eye + Location + scale,
      data = AOVdata,
      whichRandom = "participant"
    )
    
    print(BFaov)
  }
  # 
  # 
  # 
  # # fit the model
  # org_bias_aov <- afex::aov_ez(
  #   id = "participant",
  #   dv = "PSE",
  #   data = distHorData,
  #   within = c("Eye", "Location"),
  # )
  # 
  # cat("==== Horizontal Distance Bias ANOVA:\n\n")
  # 
  # print(org_bias_aov)
  # 
  # # fit the model
  # bias_aov <- afex::aov_ez(
  #   id = "participant",
  #   dv = "PSE",
  #   data = AOVdata,
  #   within = c("Eye", "Location", "orientation"),
  # )
  # 
  # cat("==== Distance/Horizontal Bias ANOVA:\n\n")
  # 
  # print(bias_aov)
  # 
  # 
  
}



## PSE plot -----

plotDistUpsAsynchPSEs <- function() {
  
  df <- getDistUpsAsynchANOVAdata()
  
  info <- data.frame( Eye      = c('ipsilateral', 'ipsilateral', 'contralateral', 'contralateral'),
                      Location = c(  'blindspot',        'away',     'blindspot',          'away'),
                      col      = c(        'red',      'orange',          'blue',     'turquoise'))
  
  plot(NULL,NULL,
       xlim=c(0,5), ylim=c(-2,1),
       main=sprintf('upsc. async. hor. dist. perception (N=%d)',length(unique(df$participant))),
       xlab='condition',ylab='PSE',
       bty='n',ax=F)
  
  lines(x=c(0,5),y=c(0,0),col='#999999')
  
  for (condno in c(1:nrow(info))) {
    Eye      <- info[condno,'Eye']
    Location <- info[condno,'Location']
    col      <- info[condno,'col']
    
    PSEs <- df[which(df$Eye == Eye & df$Location == Location),'PSE']
    
    points(x=rep(condno-0.2,length(PSEs)),
           y=PSEs,
           col=Reach::colorAlpha(col,34),
           pch=16,
           cex=2)
    
    CI = Reach::getConfidenceInterval(PSEs)
    
    polygon( x = c(0.1,0.3,0.3,0.1) + condno,
             y = rep(CI,each=2),
             col=Reach::colorAlpha(col,34),
             border=NA)
    
    lines( x = c(0.1,0.3) + condno,
           y = rep(mean(PSEs),2),
           col=col)
    
  }
  
  axis(side=2,at=c(1,0,-1,-2))
  
  legend(x=2.5, y=-1.5,
         legend=c('ipsi, at', 'ipsi, away', 'contra, at', 'contra, away'),
         lty=1, col=info$col,
         bty='n',
         xpd=TRUE)
  
}