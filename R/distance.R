
`%notin%` <- Negate(`%in%`)

# loads the data for 1 participant:

loadParticipantDistanceData <- function(ID) {
  
  # both halves of the data should be in one file now:
  data <- read.table(file = sprintf('data/sub-%s/ses-distance/beh/sub-%s_distance.tsv',ID,ID), header = TRUE, sep = "\t", skip = 0)
  
  # remove lines we don't need:
  data <- data[which(data$abort == FALSE),]

  # that's all we want for now:
  return(data)
  
}


# loads all the distance data:

loadDistanceData <- function() {
  
  # get participants in this task:
  participants <- read.table(file = 'data/participants.tsv', header = TRUE, sep = "\t", skip = 0)
  participants <- participants$ID[which(participants$distance)]
  
  allData <- list()
  for (participant in participants) {
    allData[[participant]] <- loadParticipantDistanceData(participant)
  }
  
  return(allData)
  
}

# filenames <- c('left'='data/patched/distance/torp24182c_dist_LH_5.txt',
#                'right' ='data/patched/distance/torp24182c_dist_RH_1.txt')

# filenames <- c('left'='data/patched/distance/torpb40694_dist_LH_1.txt',
#                'right' ='data/patched/distance/torpb40694_dist_RH_1.txt')

# getParticipantFilenames <- function() {
#   
#   # get all files in target directory:
#   csvfiles       <- list.files( path = 'data/distance/', 
#                                 pattern = '*.txt')
#   # split filenames by "_":
#   csvsubstrings  <- strsplit(csvfiles, '_')
#   
#   # collect participant IDs here:
#   participants <- c()
#   
#   # add unique participant IDs to vector:
#   for (css in csvsubstrings) {
#     if (css[1] %notin% participants) {participants <- c(participants, css[1])}
#   }
#   
#   # collect participant filenames in list:
#   participant_filenames <- list()
#   
#   for (participant in participants) {
#     # empty filename holder for participant:
#     participant_filenames[[participant]] <- c()
#     
#     for (side in c('left','right')) {
#       
#       # get hemifield txt files:
#       h_files <- Sys.glob( paths = sprintf('data/distance/%s*%s*.txt', participant, list('left'='LH', 'right'='RH')[[side]]) )
#       
#       runs <- c()
#       for (filename in h_files) {
#         parts <- strsplit(fs::path_ext_remove(basename(filename)), '_')[[1]]
#         runs <- c(runs, as.numeric(parts[[length(parts)]]))
#       }
#       participant_filenames[[participant]][side] <- h_files[which.max(runs)]
#     }
#   }
#   
#   return(participant_filenames)
#   
# }


# returns an lm model fit (using a probit binomial link function) on one subset of data:

eval_staircase <- function(data.df) {
  data.df$targ_chosen <- (data.df$Resp == 1) == (data.df$Which_first == 'Targ')
  
  agg_data.df <- data.frame(
    targ_chosen = with(data.df, tapply(targ_chosen, Difference, sum)),
    foil_chosen = with(data.df, tapply(!targ_chosen, Difference, sum))
  )
  
  # print(agg_data.df)
  
  agg_data.df$foil_targ_dif <- as.numeric(rownames(agg_data.df))
  
  rownames(agg_data.df) <- c()
  
  exp_names <- c(seq(-3.5, -0.5, .5), -0.1, 0.1, seq(.5, 3.5,.5))
  for(x in exp_names) {
    if (!(x %in% agg_data.df$foil_targ_dif)) {
      agg_data.df <- rbind(agg_data.df, matrix(c(0, 0, x), ncol = 3, dimnames = list(NULL, colnames(agg_data.df))))
    }
  }
  
  agg_data.df <- agg_data.df[order(agg_data.df$foil_targ_dif),]
  
  mod <- glm(cbind(targ_chosen, foil_chosen) ~ foil_targ_dif,
             family = binomial('probit'), data = agg_data.df)
  
  return(mod)
  
}


participantStaircasePlots <- function(df) {
  
  layout(mat=matrix(c(1,2), nrow=2))
  
  for (eye in c('left','right')) {
    
    edf <- df[which(df$Eye == eye),]
    
    plot(-1000,-1000,
         main=sprintf('%s eye',eye),xlab='trial',ylab='difference [dva]',
         xlim=c(0,max(edf$Trial)+1),ylim=c(-3.5,3.5),
         bty='n',ax=F)
    
    for (sc in sort(unique(edf$Stair))) {
      
      sdf <- edf[which(edf$Stair == sc),]
      X <- sdf$Trial
      Y <- sdf$Difference
      
      lines(X,Y)
      
    }
    
    axis(side=1,at=c(1,seq(50,max(edf$Trial)+1,50)))
    axis(side=2,at=c(-3.5,0,3.5))
    
  }
  
}

plotParticipantPsychometricCurves <- function(df) {
  
  plot(-1000,-1000,
       xlim=c(-3.5, 3.5), ylim=c(0,1),
       main='', xlab='difference [dva]', ylab='proportion target chosen',
       ax=F, bty='n')
  
  lines(x=c(-3.5,3.5),y=c(0.5,0.5),col='gray',lty=3)
  lines(x=c(0,0),y=c(0,1),col='gray',lty=3)
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out'),
                      col  = c('red', 'orange', 'blue', 'turquoise'))
  
  
  for (cond_no in c(1:nrow(info))) {
    
    # select rows that correspond to one fo the 4 conditions:
    eye <- info$eye[cond_no]
    area <- info$area[cond_no]
    scol <- info$col[cond_no]
    tcol <- Reach::colorAlpha(scol, alpha=34)
    
    if (eye == 'ipsi') {
      e_idx <- which(df$Hemifield == df$Eye)
    } else {
      e_idx <- which(df$Hemifield != df$Eye)
    }
    
    if (area == 'bsa') {
      a_idx <- which(df$Targ_loc    %in% c('left-mid','righ-mid'))
    } else {
      a_idx <- which(df$Targ_loc %notin% c('left-mid','righ-mid'))
    }
    
    idx <- intersect(e_idx, a_idx)
    
    cdf <- df[idx,]
    
    acdf <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
    lines(acdf, col=scol)
    points(acdf, col=scol)
    
    # now fit model:
    mod <- eval_staircase(cdf)
    pred <- predict(mod, type = 'response', se.fit = TRUE, newdata=data.frame(foil_targ_dif=seq(-3.5,3.5,0.1)))
    
    
    polygon(c(seq(-3.5,3.5,0.1), rev(seq(-3.5,3.5,0.1))),
            c(pred$fit - pred$se.fit, rev(pred$fit + pred$se.fit)),
            col = tcol, border = FALSE)
    lines(pred$fit ~ seq(-3.5,3.5,0.1), lwd = 1, col = scol)
    
  }
  
  legend(x=-3.5, y=1,
         legend=c('ipsilateral, across', 'ipsilateral, away', 'contralateral, across', 'contralateral, away'),
         lty=1, col=info$col,
         bty='n')
  
  axis(side=1, at=c(-3.5,-2,-1,0,1,2,3.5))
  axis(side=2, at=c(0,0.5,1))
  
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
      
      agcdf <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
      
      agcdf$participant <- participant
      
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

logisticModels <- function(allConditionData=NULL) {
  
  if (is.null(allConditionData)) {
    allConditionData <- processDistanceData()
  }
  
  allConditionModels <- list()
  
  for (condition in names(allConditionData)) {
    
    cond_df <- allConditionData[[condition]]
    
    # going to assign weights to participants
    # based on how much data they are contributing
    # should be the same for everyone, but this 
    cond_df$weights <- NA
    
    # count number of data points per participant:
    counts <- table(cond_df$participant)
    weights <- counts / sum(counts)
    for (pp in names(weights)) {
      cond_df$weights[which(cond_df$participant == pp)] <- weights[pp]
    }
    # of course, these are already averages, so probably this doesn't matter one bit
    
    cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="logit"), data=cond_df)
    
    # skipping the weights for now:
    # cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="logit"), data=cond_df, weights=cond_df$weights)
    
    allConditionModels[[condition]] <- cond_mod
    
  }
  
  return(allConditionModels)
  
  # data.df$targ_chosen <- (data.df$Resp == 1) == (data.df$Which_first == 'Targ')
  # 
  # agg_data.df <- data.frame(
  #   targ_chosen = with(data.df, tapply(targ_chosen, Difference, sum)),
  #   foil_chosen = with(data.df, tapply(!targ_chosen, Difference, sum))
  # )
  # 
  # print(agg_data.df)
  # 
  # agg_data.df$foil_targ_dif <- as.numeric(rownames(agg_data.df))
  # 
  # rownames(agg_data.df) <- c()
  # 
  # exp_names <- c(seq(-3.5, -0.5, .5), -0.1, 0.1, seq(.5, 3.5,.5))
  # for(x in exp_names) {
  #   if (!(x %in% agg_data.df$foil_targ_dif)) {
  #     agg_data.df <- rbind(agg_data.df, matrix(c(0, 0, x), ncol = 3, dimnames = list(NULL, colnames(agg_data.df))))
  #   }
  # }
  # 
  # agg_data.df <- agg_data.df[order(agg_data.df$foil_targ_dif),]
  # 
  # mod <- glm(cbind(targ_chosen, foil_chosen) ~ foil_targ_dif,
  #            family = binomial('probit'), data = agg_data.df)
  # 
  # return(mod)
  
}

plotGroupPsychometricCurve <- function(target='inline') {
  
  setupFigureFile( target = target, 
                   width = 5, 
                   height = 5, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/distance_psychometric.%s', target, target))
  
  
  allConditionData <- processDistanceData()
  
  N <- length(unique(allConditionData[[1]]$participant))
  
  allConditionModels <- logisticModels(allConditionData)
  
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
    
    mod <- allConditionModels[[sprintf('%s_%s', info$eye[cond_no], info$area[cond_no])]]
    pred <- predict(mod, type = 'response', se.fit = TRUE, newdata=data.frame(Difference=seq(-3.5,3.5,0.1)))
    
    polygon(c(seq(-3.5,3.5,0.1), rev(seq(-3.5,3.5,0.1))),
            c(pred$fit - pred$se.fit * 1.96, rev(pred$fit + pred$se.fit * 1.96)),
            col = tcol, border = FALSE)
    lines(pred$fit ~ seq(-3.5,3.5,0.1), lwd = 1, col = scol)
    
  }
  
  legend(x=-3.5, y=1,
         legend=c('ipsilateral, across', 'ipsilateral, away', 'contralateral, across', 'contralateral, away'),
         lty=1, col=info$col,
         bty='n')
  
  axis(side=1, at=c(-3.5,-2,-1,0,1,2,3.5))
  axis(side=2, at=c(0,0.5,1))
  
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
}
