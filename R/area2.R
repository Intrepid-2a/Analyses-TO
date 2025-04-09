
loadAreaData <- function() {
  
  # get participants in this task:
  participants <- read.table(file = 'data/participants.tsv', header = TRUE, sep = "\t", skip = 0, stringsAsFactors = FALSE)
  participants <- participants$ID[which(participants$area)]
  
  allData <- list()
  for (participant in participants) {
    pdf <- loadParticipantTaskData(ID=participant,task='area')
    
    
    # add Eye column ("left", "right")
    # pdf$EyeStim is 0 or 1, 0 is left eye, 1 is right eye
    pdf$Eye <- c('left', 'right')[pdf$EyeStim+1]

    # add AreaDifference column
    # peripheral area - foveal area
    # peripheral area is from the diameter of the peripheral circle:
    # pi * r^2
    
    # PeriOrigSize is the diameter of the peripheral circle
    # FixFinalSize is the diameter of the foveal circle
    # str(pdf)
    # print(pdf$PeriOrigSize)
    per_a <- (pi * (pdf$PeriOrigSize/2)^2)
    fov_a <- (pi * (pdf$FixFinalSize/2)^2)
    
    # pdf$AreaDifference <- ((fov_a - per_a) / per_a) * 100
    pdf$AreaDifference <- fov_a - per_a
    
    
    # # add Location column ('bsa' and 'out)
    # pdf$Location <- c('out',  'bsa')[pdf$StimulusPosition+1]
    
    
    allData[[participant]] <- pdf
    
  }
  
  return(allData)
  
}

# no processing needed, but we do some remixing, and assigning conditions

sortAreaData <- function() {

  allData <- loadAreaData()

  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out') )

  allConditionData <- list()

  for (cond_no in c(1:nrow(info))) {

    # select rows that correspond to one of the 4 conditions:
    eye <- info$eye[cond_no]
    area <- info$area[cond_no]

    condition_data <- NA

    for (participant in names(allData)) {

      # print(participant)

      df <- allData[[participant]]

      if (eye == 'ipsi') {
        e_idx <- which(df$HemiField == df$Eye)
      } else {
        e_idx <- which(df$HemiField != df$Eye)
      }
      # pdf$StimulusPosition
      if (area == 'bsa') {
        a_idx <- which(df$StimulusPosition == 1)
      } else {
        a_idx <- which(df$StimulusPosition == 0)
      }

      idx <- intersect(e_idx, a_idx)

      # print(length(idx))

      cdf <- df[idx,]

      # cdfun <- ecdf(cdf$AreaDifference)
      # X <- seq(-40,60,1)
      # Y <- cdfun(X)
      # cdf <- data.frame(AreaDifference=X,
      #                   proportion=Y)
      # cdf$participant <- participant
      
      
      cdf$participant <- participant
      cdf <- cdf[,c('AreaDifference', 'participant')]


      if (is.data.frame(condition_data)) {
        condition_data <- rbind(condition_data, cdf)
      } else {
        condition_data <- cdf
      }

    }

    allConditionData[[sprintf('%s_%s', eye, area)]] <- condition_data

  }

  return(allConditionData)

}


processAreaData <- function() {
  
  allData <- loadAreaData()
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out') )
  
  allConditionData <- list()
  
  for (cond_no in c(1:nrow(info))) {
    
    # select rows that correspond to one of the 4 conditions:
    eye <- info$eye[cond_no]
    area <- info$area[cond_no]
    
    condition_data <- NA
    
    for (participant in names(allData)) {
      
      # print(participant)
      
      df <- allData[[participant]]
      
      if (eye == 'ipsi') {
        e_idx <- which(df$HemiField == df$Eye)
      } else {
        e_idx <- which(df$HemiField != df$Eye)
      }
      # pdf$StimulusPosition
      if (area == 'bsa') {
        a_idx <- which(df$StimulusPosition == 1)
      } else {
        a_idx <- which(df$StimulusPosition == 0)
      }
      
      idx <- intersect(e_idx, a_idx)
      
      # print(length(idx))
      
      cdf <- df[idx,]
      # cdf$count <- 1
      
      # agcdf  <- aggregate(OutwardResponse ~ Curvature, data=cdf, FUN=mean)
      # agccdf <- aggregate(count           ~ Curvature, data=cdf, FUN=sum)
      # 
      # cdf$participant <- participant
      # agcdf$count       <- agccdf$count
      # 
      
      # cumulative density stuff?
      
      cdfun <- ecdf(cdf$AreaDifference)
      
      # X <- knots(cdfun)
      X <- seq(-40,60,1)
      Y <- cdfun(X)
      # counts <- unname(table(cdf$AreaDifference))
      
      # print(c(length(X), length(Y), length(counts)))
      
      cdf <- data.frame(AreaDifference=X,
                        proportion=Y)
      
      cdf$participant <- participant
      # cdf$counts  <- counts
      
      # counts?
      
      
      if (is.data.frame(condition_data)) {
        condition_data <- rbind(condition_data, cdf)
      } else {
        condition_data <- cdf
      }
      
    }
    
    allConditionData[[sprintf('%s_%s', eye, area)]] <- condition_data
    
  }
  
  return(allConditionData)
  
}


probitAreaModels <- function(allConditionData=NULL) {
  
  if (is.null(allConditionData)) {
    allConditionData <- sortAreaData()
  }
  
  allConditionModels <- list()
  
  for (condition in names(allConditionData)) {
    
    cond_df <- allConditionData[[condition]]
    
    # weights assigned to data points depending on the number of trials?
    cond_df$weights <- NA
    
    
    # this is without weights (by counted observations per distance-difference)
    # cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="logit"), data=cond_df)
    
    # here it is with weights:
    # cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="probit"), data=cond_df, weights=cond_df$counts)

    cdfun <- ecdf(cond_df$AreaDifference)
    X <- seq(-40,60,0.1)
    # print(X)
    Y <- cdfun(X)
    # print(Y)
    
    cond_mod <- glm(Y ~ X, family=quasibinomial(link="probit"), data=data.frame(X=X, Y=Y))
    
    allConditionModels[[condition]] <- cond_mod
    
  }
  
  return(allConditionModels)
  
}


plotAreaDensityCurve <- function(target='inline') {
  
  setupFigureFile( target = target, 
                   width = 5, 
                   height = 5, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/area_gaussian_psychometric.%s', target, target))
  
  
  allConditionData <- sortAreaData()

  N <- length(unique(allConditionData[[1]]$participant))
  
  allConditionModels <- probitAreaModels(allConditionData)
  
  plot(-1000,-1000,
       xlim=c(-40, 60), ylim=c(0,0.03),
       main=sprintf('area perception (N=%d)',N), xlab='area difference [dva^2]', ylab='proportion responded',
       ax=F, bty='n')
  
  lines(x=c(-40,60),y=c(0.5,0.5),col='gray',lty=3)
  lines(x=c(0,0),y=c(0,1),col='gray',lty=3)
  
  info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
                      area = c('bsa', 'out', 'bsa', 'out'),
                      col  = c('red', 'orange', 'blue', 'turquoise'))
  
  
  for (cond_no in c(1:nrow(info))) {
    
    scol <- info$col[cond_no]
    tcol <- Reach::colorAlpha(scol, alpha=34)
    
    cdf <- allConditionData[[sprintf('%s_%s', info$eye[cond_no], info$area[cond_no])]]
    
    binsize <- 2
    lo <- floor(min(cdf$AreaDifference) / binsize) * binsize
    hi <- ceiling(max(cdf$AreaDifference) / binsize) * binsize
    
    
    freqs<- hist(cdf$AreaDifference,
                 breaks=seq(lo,hi,binsize),
                 plot=FALSE,
                 include.lowest = FALSE)
    
    idx <- which(freqs$mids > (-40-binsize) & freqs$mids < 60)
    X <- freqs$mids[idx] + (binsize/2)
    Y <- freqs$density[idx]
    
    lines(x=X, y=Y, lwd = 1, col = tcol)
    
    
    bias <- mean(cdf$AreaDifference)
    width <- sd(cdf$AreaDifference)
    
    fX <- seq(-40,60,0.1)
    fY=dnorm(fX, mean=bias, sd=width)
    lines(x=fX, y=fY, lwd = 1, col = scol)
    
    
    
    # str(cdf)
    # print(cdf$AreaDifference)
    
    # cdfun <- ecdf(cdf$AreaDifference)
    # 
    # X <- knots(cdfun)
    # Y <- cdfun(X)
    # counts <- unname(table(cdf$AreaDifference))
    # 
    # acdf <- data.frame(AreaDifference=X,
    #                    proportion=Y,
    #                    count=counts)
    
    # lines(x = acdf$AreaDifference,
    #       y = acdf$proportion,
    #       col=Reach::colorAlpha(scol, alpha=100))
    
    # points(acdf, col=Reach::colorAlpha(scol, alpha=100)) # too many points
    
    mod <- allConditionModels[[sprintf('%s_%s', info$eye[cond_no], info$area[cond_no])]]
    
    pred <- predict(mod, type = 'response', se.fit = TRUE, newdata=data.frame(AreaDifference=fX))
    # str(pred)
    
    # print(coef(mod))
    
    polygon(c(fX, rev(fX)),
            c(fY - pred$se.fit * 1.96, rev(fY + pred$se.fit * 1.96)),
            col = tcol, border = FALSE)

  }
  
  legend(x=-40, y=0.03,
         legend=c('ipsi, at', 'ipsi, away', 'contra, at', 'contra, away'),
         lty=1, col=info$col,
         bty='n')
  
  axis(side=1, at=c(-40,-20,0,20,40,60))
  # axis(side=2, at=c(0,0.5,1))
  axis(side=2, at=seq(0,0.03,.01))
  
  if (target %in% c('pdf','svg','png','tiff')) {
    dev.off()
  }
  
}


# statistics -----

getAllAreaData <- function() {
  
  allConditionData <- processAreaData()
  
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

getAreaANOVAdata <- function() {
  
  data <- getAllAreaData()
  
  participants <- unique(data$participant)
  eyes <- unique(data$Eye)
  locations <- unique(data$Location)
  
  # create a data frame with all combinations of participants, eyes, and locations
  df <- expand.grid(participant = participants, Eye = eyes, Location = locations)
  
  df$mean  <- NA
  df$sd    <- NA
  df$slope <- NA
  
  for (i in 1:nrow(df)) {
    participant <- df$participant[i]
    eye         <- df$Eye[i]
    location    <- df$Location[i]
    
    # filter the data for the current combination
    subdf <- data[data$participant == participant & data$Eye == eye & data$Location == location, ]
    
    # mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
    #                     start <- c(-0.5,   1, 0  ),
    #                     lower <- c(-3,   0.3, 0  ),
    #                     upper <- c( 3,     3, 0.3) )
    
    prob_mod <- glm(proportion ~ AreaDifference, family=quasibinomial(link="probit"), data=subdf)
    
    par <- coef(prob_mod)
    
    df$mean[i] <- unname(par[1])
    df$sd[i]   <- unname(par[2])
    # df$marg[i] <- mod$par[3]
    
    df$slope[i] <- diff( predict(prob_mod, type = 'response', newdata=data.frame(AreaDifference=unname(par[1])+c(-.0000001, .0000001))) ) / (2*.0000001)
    
    # df$slope[i] <- diff(mprobit(p=mod$par, x=mod$par[1]+c(-.0000001, .0000001))) / (2*.0000001)
    
  }
  
  return(df)
}

doAreaStats <- function() {
  
  data <- getAreaANOVAdata()
  
  # fit the model
  bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "mean",
    data = data,
    within = c("Eye", "Location"),
  )
  
  cat("==== Area Bias ANOVA:\n\n")
  
  print(bias_aov)
  
  # bs_data <- data[which(data$Location == "blindspot"),]
  # 
  # bsl_bias_aov <- afex::aov_ez(
  #   id = "participant",
  #   dv = "mean",
  #   data = bs_data,
  #   within = c("Eye"),
  # )
  # 
  # cat("\n==== At Blindspot Bias Test:\n\n")
  # 
  # print(bsl_bias_aov)
  
  slope_aov <- afex::aov_ez(
    id = "participant",
    dv = "sd",
    data = data,
    within = c("Eye", "Location"),
  )
  
  cat("\n==== Area SD ANOVA:\n\n")
  
  print(slope_aov)
  
  dflist <- processAreaData()
  df <- NA
  for (name in names(dflist)) {
    subdf <- dflist[[name]]

    subdf$Eye <- NA
    subdf$Location <- NA

    if (name %in% c('ipsi_bsa', 'ipsi_out')) {
      subdf$Eye <- '_il'
    } else {
      subdf$Eye <- '_cl'
    }
    if (name %in% c('ipsi_bsa', 'cont_bsa')) {
      subdf$Location <- '_bs'
    } else {
      subdf$Location <- '_ew'
    }
    if (is.data.frame(df)) {
      df <- rbind(df, subdf)
    } else {
      df <- subdf
    }
  }
  
  cond_mod <- glm(proportion ~ AreaDifference * Eye * Location, family=quasibinomial(link="probit"), data=df)
  
  cat("\n==== Area GLM with probit link function:\n")
  
  summary(cond_mod)
  
}