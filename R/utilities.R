

`%notin%` <- Negate(`%in%`)

# loads the data for 1 participant:

loadParticipantTaskData <- function(ID, task) {
  
  # both halves of the data should be in one file now:
  data <- read.table(file = sprintf('data/sub-%s/ses-%s/beh/sub-%s_%s.tsv',ID,task,ID,task), header = TRUE, sep = "\t", skip = 0)
  
  # remove lines we don't need:
  data <- data[which(data$abort == FALSE),]
  
  # that's all we want for now:
  return(data)
  
}


setupFigureFile <- function(target='inline',width=8,height=6,dpi=300,filename) {
  
  if (target == 'pdf') {
    pdf(file   = filename, 
        width  = width, 
        height = height)
  }
  if (target == 'svg') {
    svglite::svglite( filename = filename,
                      width = width,
                      height = height,
                      fix_text_size = FALSE) 
    # fix_text_size messes up figures on my machine... 
    # maybe it's better on yours?
  }
  if (target == 'png') {
    png( filename = filename,
         width = width*dpi,
         height = height*dpi,
         res = dpi
    )
  }
  if (target == 'tiff') {
    tiff( filename = filename,
          compression = 'lzw',
          width = width*dpi,
          height = height*dpi,
          res = dpi
    )
  }
}

# luminance of stimuli ----

printStimulusLuminance <- function() {
  
  # CLUT was linearized by PsychoPy,
  # so values are in linear space.
  
  # [ [  0., 135.44739,  2.4203537, np.nan, np.nan, np.nan  ],
  #   [  0.,  27.722954, 2.4203537, np.nan, np.nan, np.nan  ],
  #   [  0.,  97.999275, 2.4203537, np.nan, np.nan, np.nan  ],
  #   [  0.,   9.235623, 2.4203537, np.nan, np.nan, np.nan  ]  ],
  
  maxLums <- c(27.722954, 97.999275, 9.235623)
  
  background <-	(c(0.50000000,0.50000000,-1.00000000)  + 1) / 2
  red        <-	(c(0.50000000,-1.00000000,-1.00000000) + 1) / 2
  green      <- (c(-1.00000000,0.50000000,-1.00000000) + 1) / 2
  
  # luminances
  cat(sprintf('background: %0.3f\n',sum(background * maxLums)))
  cat(sprintf('red:        %0.3f\n',sum(red        * maxLums)))
  cat(sprintf('green:      %0.3f\n',sum(green      * maxLums)))
  
  Rmax = 27.722954
  Gmax = 97.999275
  Rw = Rmax / (Rmax+Gmax)
  Lw = Gmax / (Rmax+Gmax)
  col_binoc <- c()
  for (idx in c(1:3)) {
    col_binoc[idx] <- (Lw*red[idx]) + (Rw*green[idx])
  }
  
  cat(sprintf('binocular:  %0.3f (lum weighted)\n',sum(col_binoc * maxLums)))
  col_binoc <- c()
  for (idx in c(1:3)) {
    col_binoc[idx] <- (.5*red[idx]) + (.5*green[idx])
  }
  
  cat(sprintf('binocular:  %0.3f (50-50)\n',sum(col_binoc * maxLums)))
  cat(sprintf('red&green:  %0.3f\n',mean(c(sum(red        * maxLums),sum(green      * maxLums)))))
  
  
  
}

# data processing -----

listParticipants <- function(task) {
  
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

listTaskFiles <- function(ID, task) {
  
  alltaskfiles <- list.files( path = sprintf('../data/%s/', task) )
  Pfiles <- alltaskfiles[which(substr(alltaskfiles, 1, nchar(ID)) == ID)]
  
  lastfiles <- list()
  
  # if (task == 'recDistHorizontal') {
  runs <- c('LH','RH')
  # }
  # if (task == 'distBinocular') {
  #   runs <- c('run')
  # }
  
  ext <- list(
    'distance'                    = 'dist',
    'distHorizontal'              = 'disth',
    'distAsynchronous'            = 'dista',
    'distUpScaledAsynchronous'    = 'distusa',
    'recDistance'                 = 'dist',
    'recDistHorizontal'           = 'disth',
    'recDistAsynchronous'         = 'dista',
    'recDistUpScaledAsynchronous' = 'distusa'
  )[[task]]
  
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
      lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, ext, hf, min(nums))
      # }
      # if (task == 'distBinocular') {
      #   lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, 'distb', hf, max(nums))
      # }
    }
    
  }
  
  return(lastfiles)
  
}


loadParticipantData <- function(ID, task) {
  
  # print(ID)
  files <- listTaskFiles(ID, task)
  # print(files)
  
  allData <- NA
  
  writeoutput <- TRUE
  
  skip <- 1
  sep <- '\t'
  if (task %in% c('recDistHorizontal', 'recDistAsynchronous', 'recDistUpScaledAsynchronous', 'recDistance')) {
    skip <- 0
    sep <- ','
  }
  
  for (hf in c('LH','RH')) {
    
    if (is.null(files[[hf]])) {
      writeoutput <- FALSE
      next
    } 
    df <- read.delim(
      file = files[[hf]],
      skip=skip,
      header=TRUE,
      sep=sep
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
  
  allData$participant <- ID
  
  if (writeoutput) {
    return(allData)
  } else {
    return(FALSE)
  }
  
}



loadTaskData <- function(task) {
  
  IDs <- listParticipants(task=task)
  
  allData <- list()
  
  for (ID in IDs) {
    
    participantData <- loadParticipantData(ID=ID, task=task)
    
    participantData <- participantData[which(participantData$abort == 0),]
    
    if (is.data.frame(participantData)) {
      allData[[ID]] <- participantData
    } else {
      cat(sprintf('Participant %s has no data\n', ID))
    }
    
  }
  
  return(allData)
  
}

eccentricityCorrection <- function(df) {
  
  df$eccdiff <- round(df$targOffset - df$foilOffset,2)
  
  eccdiffs <- sort(unique(df$eccdiff))
  PSEs <- c()
  
  for (eccdiff in eccdiffs) {
    
    edf <- df[which(df$eccdiff == eccdiff),]
    
    # plot(aggregate(Targ_chosen ~ Difference, data=edf, FUN=mean))
    
    mprob <- fit.mprobit(y=edf$Targ_chosen, x=edf$Difference, w=NULL,
                         start = c( 0, 1,   0   ),
                         lower = c(-3, 0.2, 0   ),
                         upper = c( 3, 3,   0.3 )
    )
    
    descr <- descr.mprobit(p=mprob$par)
    PSEs <- c(PSEs, descr$PSE)
    
  }
  
  print(eccdiffs)
  print(PSEs)
  
  print(cor.test(x=eccdiffs, y=PSEs))
  
  plot(x=eccdiffs, y=PSEs)
  
  return(df)
  
}

processTaskData <- function(task) {
  
  allData <- loadTaskData(task)
  
  IDs <- names(allData)
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out') )
  
  # get condition data frames instead of participant data frames:
  
  allConditionData <- list()
  
  for (cond_no in c(1:nrow(info))) {
    
    eye  <- info$eye[ cond_no]
    area <- info$area[cond_no]
    
    # cat(sprintf('%s %s\n', eye, area))
    
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

      
      if (is.data.frame(condition_data)) {
        condition_data <- rbind(condition_data, cdf)
      } else {
        condition_data <- cdf
      }
      
    }
    
    if (task %in% c('recDistHorizontal', 'recDistAsynchronous', 'recDistUpScaledAsynchronous')) {
      # condition_data <- eccentricityCorrection(condition_data)
    }
    
    # cat(sprintf('%s_%s\n', eye, area))
    allConditionData[[sprintf('%s_%s', eye, area)]] <- condition_data
    
  }
  
  alldata <- NA
  for (name in names(allConditionData)) {
    cdf <- allConditionData[[name]]
    
    if (is.data.frame(alldata)) {
      alldata <- rbind(alldata, cdf)
    } else {
      alldata <- cdf
    }
  }
  
  alldata <- eccentricityCorrection(cdf)
  
  
}




# Eyetracker calibration sequence -----

extractCalibrationSequence <- function(task) {
  
  IDs <- listParticipants(task=task)
  
  calibration_log <- NA
  
  et_file <- list(
    'area'='dist',
    'curvature'='dist',
    'distance'='dist',
    'distAsynchronous'='dstA',
    'distHorizontal'='dstH',
    'distScaled'='dstS',
    'distUpScaledAsynchronous'='dSA'
                  )[[task]]
  
  for (ID in IDs) {
    
    print(ID)
    
    # find the last eyetracking files for each participant and hemifield:
    folder <- sprintf('../data/%s/eyetracking/%s/', task, ID)
    alltaskfiles <- list.files( path = folder, pattern='*.csv' )
    print(alltaskfiles)
    alltaskfiles <- alltaskfiles[which(grepl(et_file, alltaskfiles))]
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
      lastfiles[[hf]] <- sprintf('%s%s%s%d.csv', folder, et_file, hf, max(nums))
    }
    
    # print(lastfiles)
    
    for (hf in c('LH','RH')) {
      # print(lastfiles[[hf]])
      df <- fixLiveTrack(read.csv( file = lastfiles[[hf]], header=TRUE ))
      # write.csv(df, file = sprintf('../data/distHorizontal/eyetracking/%s/clean_distH_%s.csv', ID, hf), row.names=FALSE)
      
      comments <- df$Comment[which(df$Comment != ' ')]
      
      trial <- c()
      calibrations <- c()
      
      trial_no <- 0
      N_calibrations <- 0
      total_calibrations <- 0
      for (comment in comments) {
        if (grepl(' calibration', comment)) {
          N_calibrations <- N_calibrations + 1
        }
        if (grepl(' start trial', comment)) {
          trial_no <- as.numeric(substr(comment, 14, nchar(comment)))
          # cat(sprintf('%s, %s, trial %d, calibrations: %d\n', ID, hf, trial_no, N_calibrations))
          
          trial        <- c(trial, trial_no)
          calibrations <- c(calibrations, N_calibrations)
          total_calibrations <- total_calibrations + N_calibrations
          N_calibrations <- 0
          
        }
        
      }
      
      cat(sprintf('** %s: %d trials, %d calibrations\n', hf, trial_no, total_calibrations))
      
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
  
  write.csv(calibration_log, file = sprintf('../data/%s/calibration_log.csv', task), row.names=FALSE)
  
}


checkCalibrationSanity <- function(task) {
  
  # this currently only prints the number of trials for each participant / hemifield
  
  df <- read.csv(file = sprintf('../data/%s/calibration_log.csv', task), header=TRUE, stringsAsFactors = FALSE)
  
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

