

# loading the raw (non-BIDSified) data -----

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
  return(IDs)
  
}

findTaskFiles <- function(ID, task) {
  
  alltaskfiles <- list.files( path = sprintf('../data/%s/', task) )
  Pfiles <- alltaskfiles[which(substr(alltaskfiles, 1, nchar(ID)) == ID)]
  
  lastfiles <- list()
  
  for (hf in c('LH','RH')) {
    
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
      } else {
        lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, task, hf, max(nums))
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

# processing -----

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



# main plot -----

plotDistHorizontalPsychometricCurve <- function(target='inline', cluster=NULL) {
  
  
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
                   filename = sprintf('doc/fig/%s/distance_psychometric.%s', target, target))
  
  
  allConditionData <- processDistHorizontalData()
  
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
      # CI <- CI.mprobit.serial(data=data.frame(ID=cdf$participant,
      #                                         x=cdf$Difference,
      #                                         y=cdf$Targ_chosen),
      #                         start = c(0, 1, 0, 0),
      #                         lower = c(-3, 0.2, 0, 0),
      #                         upper = c(3, 3, 0.3, 0.3),
      #                         iterations = 1000,
      #                         n = length(X))
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
    
    # polygon(x = c(CI$X, rev(CI$X)),
    #         y = c(CI$lo, rev(CI$hi)),
    #         col = tcol, border = FALSE)
    
  }
  
  legend(x=1.0, y=0.5,
         legend=c('ipsi, at', 'ipsi, away', 'contra, at', 'contra, away'),
         lty=1, col=info$col,
         bty='n')
  
  axis(side=1, at=c(-3.5,-2,-1,0,1,2,3.5))
  axis(side=2, at=c(0,0.5,1))
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
  # parallel::stopCluster(clust)
  
}
