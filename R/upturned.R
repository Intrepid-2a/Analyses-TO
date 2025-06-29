

# also source("R/utilities.R")


# loads all the distance data:

loadDistUpturnedData <- function() {
  
  IDs <- findParticipants(task='distUpturned')
  
  allData <- list()
  
  for (ID in IDs) {
    
    # print(ID)
    
    participantData <- loadDistUpturnedParticipant(ID)
    
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
  return(IDs)
  
}

findTaskFiles <- function(ID, task) {
  
  alltaskfiles <- list.files( path = sprintf('../data/%s/', task) )
  Pfiles <- alltaskfiles[which(substr(alltaskfiles, 1, nchar(ID)) == ID)]
  
  lastfiles <- list()
  
  # if (task %in% c('distHorizontal', 'distScaled', 'distRotated')) {
  #   runs <- c('LH','RH')
  # }
  if (task == 'distBinocular') {
    runs <- c('run')
  } else {
    runs <- c('LH','RH')
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
      if (task == 'distScaled') {
        lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, 'dists', hf, max(nums))
      }
      if (task == 'distRotated') {
        lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, 'distr', hf, max(nums))
      }
      if (task == 'distCentred') {
        lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, 'distc', hf, max(nums))
      }
      if (task == 'distUpturned') {
        lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, 'distt', hf, max(nums))
      }
    }
    
  }
  
  return(lastfiles)
  
}


loadDistUpturnedParticipant <- function(ID) {
  
  files <- findTaskFiles(ID, 'distUpturned')
  
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



processDistUpturnedData <- function() {
  
  allData <- loadDistUpturnedData()
  
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


plotDistUpturnedPsychometricCurve <- function(target='inline', cluster=NULL) {
  
  
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
                   filename = sprintf('doc/fig/%s/dist_centred_psychometric.%s', target, target))
  
  
  allConditionData <- processDistUpturnedData()
  
  N <- length(unique(allConditionData[[1]]$participant))
  
  # allConditionModels <- logisticDistanceModels(allConditionData)
  
  plot(-1000,-1000,
       xlim=c(-7, 7), ylim=c(0,1),
       main=sprintf('upturned distance perception (N=%d)',N), xlab='difference [dva]', ylab='proportion target chosen',
       ax=F, bty='n')
  
  lines(x=c(-7,7),y=c(0.5,0.5),col='gray',lty=3)
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
    
    X <- seq(-7,7,0.025)
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


    polygon(x = c(CI$X, rev(CI$X)),
            y = c(CI$lo, rev(CI$hi)),
            col = tcol, border = FALSE)
    
  }
  
  legend(x=-5, y=1,
         legend=c('ipsi, at', 'ipsi, away', 'contra, at', 'contra, away'),
         lty=1, col=info$col,
         bty='n')
  
  axis(side=1, at=c(-6,-4,-2,0,2,4,6))
  axis(side=2, at=c(0,0.5,1))
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
  # parallel::stopCluster(clust)
  
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

doDistanceBiasANOVA <- function() {
  
  data <- getDistANOVAdata()
  
  # fit the model
  bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = data,
    within = c("Eye", "Location"),
  )
  
  # print(bias_aov)
  knitr::kable(bias_aov$anova_table, digits=3)
  
}
