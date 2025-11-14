

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

plotDistancePsychometricCurve <- function(target='inline', cluster=NULL) {
  
  
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
  
  legend(x=0, y=0.5,
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
                         start = c(-0.5,   1, 0  ),
                         lower = c(-3,   0.3, 0  ),
                         upper = c( 3,     3, 0.3) )
    # mod <- fit.mprobit( y=subdf$Targ_chosen, x =subdf$Difference,
    #                     start <- c(-0.5,   1, 0   ),
    #                     lower <- c(-3,   0.3, 0   ),
    #                     upper <- c( 3,     3, 0.3 ) )
    
    df$mean[i] <- mod$par[1]
    df$sd[i]   <- mod$par[2]
    # df$margL[i] <- mod$par[3]
    # df$margU[i] <- mod$par[4]
    df$LR[i]   <- mod$par[3]
    
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

doDistanceBiasANOVAfollowup <- function() {
  
  data <- getDistANOVAdata()
  
  bs_data <- data[which(data$Location == "blindspot"),]
  
  bsl_bias_aov <- afex::aov_ez(
    id = "participant",
    dv = "PSE",
    data = bs_data,
    within = c("Eye"),
  )
  
  # print(bsl_bias_aov)
  knitr::kable(bsl_bias_aov$anova_table, digits=3)
  
  
}

doDistanceSlopeANOVA <- function() {
  
  data <- getDistANOVAdata()
  
  # fit the model
  slope_aov <- afex::aov_ez(
    id = "participant",
    dv = "slope",
    data = data,
    within = c("Eye", "Location"),
  )
  
  # print(slope_aov)
  knitr::kable(slope_aov$anova_table, digits=3)
  
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


plotDistanceIpsiEffectDistribution <- function() {
  
  data <- getDistANOVAdata()
  
  ipsiData <- data[which(data$Location == 'blindspot'),]
  
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
  
  lines(effd, col='red', lwd=2)
  
  points(x=df$effect, y=rep(-0.1, length(df$effect)),
         pch=16, col='#FF000033', cex=2.5)
  
  axis(side=1, at=c(-2,-1,0,1,2))
  axis(side=2, at=seq(0, 0.6, 0.15))
  
  uno <- Reach::multiModalFit( x = df$effect,
                               n = 1)
  dos <- Reach::multiModalFit( x = df$effect,
                               n = 2)
  # 
  # par.uno <- list('m' = unname(uno['m1']),
  #                 's' = unname(uno['s1']),
  #                 'w' = unname(uno['w1']))
  # 
  uno.pd <- Reach::multiModalModel(par=uno, x = seq(-2,2,.01))
  # 
  # par.dos <- list('m' = c(unname(dos['m1']), unname(dos['m2'])),
  #                 's' = c(unname(dos['s1']), unname(dos['s2'])),
  #                 'w' = c(unname(dos['w1']), unname(dos['w2'])))
  # 
  # uno.pd <- Reach::multiModalModel(par=par.uno, x = seq(-2,2,.01))
  dos.pd <- Reach::multiModalModel(par=dos, x = seq(-2,2,.01))
  # 
  lines(x=seq(-2,2,.01), y=uno.pd, col='blue', lty=2)
  lines(x=seq(-2,2,.01), y=dos.pd, col='blue', lty=3)
  
  print(ks.test(df$effect, 'pnorm', mean=mean(df$effect), sd=sd(df$effect)))
  
  
  
}


# test GLM fits -----

testGLMs <- function() {
  
  allData <- getAllDistData()
  
  ipsiData <- allData[which(allData$Eye == 'ipsilateral'),]
  
  # overall fit with GLM:
  mod1 <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="probit"), data=ipsiData)
  cat("\nGLM on all ipsilateral data, only Difference as predictor:\n")
  print(summary(mod1))
  
  # mod2 <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="probit"), data=ipsiData[which(ipsiData$Location == 'blindspot'),])
  # cat("\nGLM on ipsilateral, at blindspot data only, only Difference as predictor:\n")
  # print(summary(mod2)$coefficients)
  # mod3 <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="probit"), data=ipsiData[which(ipsiData$Location == 'away'),])
  # cat("\nGLM on ipsilateral, away from blindspot data only, only Difference as predictor:\n")
  # print(summary(mod3)$coefficients)
  # 
  mod4 <- glm(Targ_chosen ~ Difference * Location, family=quasibinomial(link="probit"), data=ipsiData)
  cat("\nGLM on ipsilateral data only, both Difference and Location as predictors:\n")
  # print(summary(mod4)$coefficients)
  # 
  # # the intercept for mod4 is the same as the one for mod3:
  # print(summary(mod4)$coefficients[1,1] == summary(mod3)$coefficients[1,1])
  # 
  # 
  # # the coefficient for Difference in mod4 should also be the same as the one in mod3:
  # print(summary(mod4)$coefficients[2,1] == summary(mod3)$coefficients[2,1])
  
  
  # print(summary(mod4)$coefficients[3,1])
  # print(summary(mod2)$coefficients[2,1] - summary(mod1)$coefficients[1,1])
  
  # apart for rounding errors, the coefficient for Location in mod4 should be the same as the difference between the coefficients for Difference in mod2 and mod3:
  # print(summary(mod4)$coefficients[4,1])
  # print((summary(mod2)$coefficients[2,1] - summary(mod3)$coefficients[2,1]))
  
  
  
  idx <- which(ipsiData$Location == 'blindspot')
  mod1fit <- fit.mprobit( y=ipsiData$Targ_chosen[idx], x=ipsiData$Difference[idx],
                          start = c( 0,   1),
                          lower = c(-3,   0.01),
                          upper = c( 3, 100)      )
  
  cat(sprintf("mprobit fit bias: %0.8f, slope: %0.8f\n",
              mod1fit$par[1], mod1fit$par[2]))
  cat(sprintf('GLM fit bias: %0.8f, slope: %0.8f\n',
              -coef(mod1)[1]/coef(mod1)[2], 1/coef(mod1)[2]))
  
  
  # psyphy needs a different kind of input:
  
  ipsiData$succ <- ipsiData$Targ_chosen * ipsiData$count
  ipsiData$fail <- (1 - ipsiData$Targ_chosen) * ipsiData$count
  
  succ <- aggregate(succ ~ Difference, data = ipsiData, FUN = sum)
  wght <- aggregate(fail ~ Difference, data = ipsiData, FUN = sum)
  
  Difference <- succ$Difference
  my.resp.mat   <- cbind(succ$succ, wght$fail)
  
  
  
  # fit using the psyphy package:
  ppp.glm <- psyphy::psyfun.2asym(
    my.resp.mat ~ Difference,
    link = psyphy::probit.2asym
  )
  
  #print(ppp.glm)
  
  # fit using mprobit with 2 asymptotes for comparison:
  mod2asymfit <- fit.mprobit( y=ipsiData$Targ_chosen, x=ipsiData$Difference,
                              start = c( 0,   1,   0,   0),
                              lower = c(-3,   0.01, 0,   0),
                              upper = c( 3, 100,   0.3, 0.3)      )
  
  print(mod2asymfit)
  
  # cat("\nrepeated measures ANOVAs on bias and slope\neach with Location (at blindspot or away) as factor\n\n")
  # distAOVdata <- getDistANOVAdata()
  # 
  # # select same sub set of data:
  # distAOVdata <- distAOVdata[which(distAOVdata$Eye == 'ipsilateral'),]
  # 
  # # drop unnecessary columns:
  # distAOVdata <- distAOVdata[,c('participant','PSE','slope','Location')]
  # 
  # 
  # bias_aov <- afex::aov_ez(
  #   id = "participant",
  #   dv = "PSE",
  #   data = distAOVdata,
  #   within = c("Location"),
  # )
  # 
  # print(summary(bias_aov))
  # # knitr::kable(bias_aov$anova_table, digits=3)
  # 
  # 
  # slope_aov <- afex::aov_ez(
  #   id = "participant",
  #   dv = "slope",
  #   data = distAOVdata,
  #   within = c("Location"),
  # )
  # 
  # print(summary(slope_aov))
  # # knitr::kable(slope_aov$anova_table, digits=3)
  
  
}


getStimulusAngles <- function() {

  participants <- read.table(file = 'data/participants.tsv', header = TRUE, sep = "\t", skip = 0)
  participants <- participants$ID[which(participants$distance)]
    
  
  participant       <- c()
  left_foil_top     <- c()
  left_foil_bottom  <- c()
  left_target       <- c()
  right_foil_top    <- c()
  right_foil_bottom <- c()
  right_target      <- c()
  
  
  for (ppID in participants) {
    BS <- jsonlite::read_json(sprintf('data/sub-%s/ses-distance/beh/sub-%s_distance.json', ppID, ppID))$blindspot_mapping
    
    # RIGHT properties
    right_pos <- as.numeric(BS$right$position)
    right_size <- as.numeric(BS$right$size)
    
    
    right_bs_angle <- atan2(right_pos[2], right_pos[1]) * 180 / pi
    
    # right_ang_up = (cart2pol(spot_cart[0], spot_cart[1] + spot_size[1])[0] - spot[0]) + 2
    right_ang_up = (atan2(right_pos[2] + right_size[1], right_pos[1]) * 180 / pi) - right_bs_angle + 2
    
    
    # LEFT properties
    left_pos <- as.numeric(BS$left$position)
    left_size <- as.numeric(BS$left$size)
    
    left_bs_angle <- atan2(left_pos[2], -1*left_pos[1]) * 180 / pi
    
    
    
    # left_ang_up = (cart2pol(spot_cart[0], spot_cart[1] - spot_size[1])[0] - spot[0]) + 2
    left_ang_up = (atan2(left_pos[2] + left_size[1], -left_pos[1]) * 180 / pi) - left_bs_angle + 2
    
    
    right_target      <- c(right_target,      right_bs_angle + 0)
    right_foil_top    <- c(right_foil_top,    right_bs_angle + right_ang_up)
    right_foil_bottom <- c(right_foil_bottom, right_bs_angle - right_ang_up)
    left_target       <- c(left_target,       left_bs_angle  + 0)
    left_foil_top     <- c(left_foil_top,     left_bs_angle  + left_ang_up)
    left_foil_bottom  <- c(left_foil_bottom,  left_bs_angle  - left_ang_up)
    
    participant <- c(participant, ppID)
    
  }
  
  return( data.frame ( participant,
                       left_foil_top,
                       left_foil_bottom,
                       left_target,
                       right_foil_top,
                       right_foil_bottom,
                       right_target) )
}

plotStimulusAngles <- function(target='inline') {
  
  if (target == 'pdf') {
    
    pdf(file = sprintf('stimulus_angles.%s', target),
        width = 8, height = 5, pointsize = 12, paper = 'special', bg = 'white')
    
  }

  df <- getStimulusAngles()
  
  layout(mat = matrix(  c(1:3),
                        nrow=3,
                        ncol=1,
                        byrow=TRUE) 
         )
  
  for (stim_loc in c('foil_top', 'target', 'foil_bottom')) {
    
    plot(NULL, NULL,
         main=list('foil_top'='above blind spot',
                   'target'='at blind spot',
                   'foil_bottom'='below blind spot')[[stim_loc]],
         xlab='stimulus angle',ylab='probability density',
         xlim=c(-45,30),ylim=c(0,0.2),
         ax=F, bty='n')
    
    for (side in c('left','right')) {
      
      col <- list('left'='blue', 'right'='red')[[side]]
      
      pd <- density( df[[sprintf('%s_%s', side, stim_loc)]] ,
                      n = 351,
                      from = -45,
                      to   = 30)
      
      lines(pd, col=col, lwd=2)
      
    }
    
    loc_angles <- c(df[[sprintf('%s_%s', 'left', stim_loc)]], df[[sprintf('%s_%s', 'rightt', stim_loc)]])
    
    cat(sprintf('%s average: %0.2f\n', stim_loc, mean(loc_angles)))
    
    axis(side=1, at=seq(-45,30,15))
    axis(side=2, at=seq(0,0.2,0.1))
    
    if (stim_loc == 'foil_top') {
      legend(x=-40, y=0.2,
             legend=c('left hemifield', 'right hemifield'),
             col=c('blue', 'red'),
             lty=1, bty='n')
    }
    
  }
  
 
 
  if (target %in% c('svg','pdf','png','tiff')) {
    dev.off()
  }
  
}

getBlindSpotEccentricity <- function() {
  
  participants <- read.table(file = 'data/participants.tsv', header = TRUE, sep = "\t", skip = 0)
  participants <- participants$ID[which(participants$distance)]
  
  
  participant  <- c()
  left_x       <- c()
  left_y       <- c()
  left_ecc     <- c()
  right_x      <- c()
  right_y      <- c()
  right_ecc    <- c()

  
  for (ppID in participants) {
    BS <- jsonlite::read_json(sprintf('data/sub-%s/ses-distance/beh/sub-%s_distance.json', ppID, ppID))$blindspot_mapping
    
    # RIGHT properties
    right_pos <- as.numeric(BS$right$position)
    # right_size <- as.numeric(BS$right$size)
    
    
    # right_bs_angle <- atan2(right_pos[2], right_pos[1]) * 180 / pi
    # # right_ang_up = (cart2pol(spot_cart[0], spot_cart[1] + spot_size[1])[0] - spot[0]) + 2
    # right_ang_up = (atan2(right_pos[2] + right_size[1], right_pos[1]) * 180 / pi) - right_bs_angle + 2
    
    
    # LEFT properties
    left_pos <- as.numeric(BS$left$position)
    # left_size <- as.numeric(BS$left$size)
    
    # left_bs_angle <- atan2(left_pos[2], -1*left_pos[1]) * 180 / pi
    # # left_ang_up = (cart2pol(spot_cart[0], spot_cart[1] - spot_size[1])[0] - spot[0]) + 2
    # left_ang_up = (atan2(left_pos[2] + left_size[1], -left_pos[1]) * 180 / pi) - left_bs_angle + 2
    
    
    participant  <- c(participant, ppID)
    left_x       <- c(left_x, left_pos[1])
    left_y       <- c(left_y, left_pos[2])
    left_ecc     <- c(left_ecc, sqrt(left_pos[1]^2 + left_pos[2]^2))
    right_x      <- c(right_x, right_pos[1])
    right_y      <- c(right_y, right_pos[2])
    right_ecc    <- c(right_ecc, sqrt(right_pos[1]^2 + right_pos[2]^2))
    
  }
  
  return( data.frame ( participant,
                       left_x,
                       left_y,
                       left_ecc,
                       right_x,
                       right_y,
                       right_ecc
                       ) )
}