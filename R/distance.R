

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


# participantStaircasePlots <- function(df) {
#   
#   layout(mat=matrix(c(1,2), nrow=2))
#   
#   for (eye in c('left','right')) {
#     
#     edf <- df[which(df$Eye == eye),]
#     
#     plot(-1000,-1000,
#          main=sprintf('%s eye',eye),xlab='trial',ylab='difference [dva]',
#          xlim=c(0,max(edf$Trial)+1),ylim=c(-3.5,3.5),
#          bty='n',ax=F)
#     
#     for (sc in sort(unique(edf$Stair))) {
#       
#       sdf <- edf[which(edf$Stair == sc),]
#       X <- sdf$Trial
#       Y <- sdf$Difference
#       
#       lines(X,Y)
#       
#     }
#     
#     axis(side=1,at=c(1,seq(50,max(edf$Trial)+1,50)))
#     axis(side=2,at=c(-3.5,0,3.5))
#     
#   }
#   
# }

# plotParticipantPsychometricCurves <- function(df) {
#   
#   plot(-1000,-1000,
#        xlim=c(-3.5, 3.5), ylim=c(0,1),
#        main='', xlab='difference [dva]', ylab='proportion target chosen',
#        ax=F, bty='n')
#   
#   lines(x=c(-3.5,3.5),y=c(0.5,0.5),col='gray',lty=3)
#   lines(x=c(0,0),y=c(0,1),col='gray',lty=3)
#   
#   info <- data.frame( eye  = c('ipsi','ipsi','cont','cont'),
#                       area = c('bsa', 'out', 'bsa', 'out'),
#                       col  = c('red', 'orange', 'blue', 'turquoise'))
#   
#   
#   for (cond_no in c(1:nrow(info))) {
#     
#     # select rows that correspond to one fo the 4 conditions:
#     eye <- info$eye[cond_no]
#     area <- info$area[cond_no]
#     scol <- info$col[cond_no]
#     tcol <- Reach::colorAlpha(scol, alpha=34)
#     
#     if (eye == 'ipsi') {
#       e_idx <- which(df$Hemifield == df$Eye)
#     } else {
#       e_idx <- which(df$Hemifield != df$Eye)
#     }
#     
#     if (area == 'bsa') {
#       a_idx <- which(df$Targ_loc    %in% c('left-mid','righ-mid'))
#     } else {
#       a_idx <- which(df$Targ_loc %notin% c('left-mid','righ-mid'))
#     }
#     
#     idx <- intersect(e_idx, a_idx)
#     
#     cdf <- df[idx,]
#     
#     acdf <- aggregate(Targ_chosen ~ Difference, data=cdf, FUN=mean)
#     lines(acdf, col=scol)
#     points(acdf, col=scol)
#     
#     # now fit model:
#     mod <- eval_staircase(cdf)
#     pred <- predict(mod, type = 'response', se.fit = TRUE, newdata=data.frame(foil_targ_dif=seq(-3.5,3.5,0.1)))
#     
#     
#     polygon(c(seq(-3.5,3.5,0.1), rev(seq(-3.5,3.5,0.1))),
#             c(pred$fit - pred$se.fit, rev(pred$fit + pred$se.fit)),
#             col = tcol, border = FALSE)
#     lines(pred$fit ~ seq(-3.5,3.5,0.1), lwd = 1, col = scol)
#     
#   }
#   
#   legend(x=-3.5, y=1,
#          legend=c('ipsilateral, across', 'ipsilateral, away', 'contralateral, across', 'contralateral, away'),
#          lty=1, col=info$col,
#          bty='n')
#   
#   axis(side=1, at=c(-3.5,-2,-1,0,1,2,3.5))
#   axis(side=2, at=c(0,0.5,1))
#   
# }



processDistanceData <- function(scale=0) {
  
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
      
      if (scale == 1) {
        agcdf$Targ_chosen <- agcdf$Targ_chosen - min(agcdf$Targ_chosen)
        agcdf$Targ_chosen <- agcdf$Targ_chosen / max(agcdf$Targ_chosen)
      }
      
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





logisticDistanceModels <- function(allConditionData=NULL) {
  
  if (is.null(allConditionData)) {
    allConditionData <- processDistanceData(scale=1)
  }
  
  allConditionModels <- list()
  
  for (condition in names(allConditionData)) {
    
    cond_df <- allConditionData[[condition]]
    
    # going to assign weights to participants
    # based on how much data they are contributing
    # should be the same for everyone, but this 
    cond_df$weights <- NA
    

    # this is without weights (by counted observations per distance-difference)
    # cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="logit"), data=cond_df)
    
    # here it is with weights:
    # cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="probit"), data=cond_df, weights=cond_df$counts)
    cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="logit"), data=cond_df, weights=cond_df$counts)
    
    allConditionModels[[condition]] <- cond_mod
    
  }
  
  return(allConditionModels)
  
}

plotDistancePsychometricCurve <- function(target='inline') {
  
  setupFigureFile( target = target, 
                   width = 5, 
                   height = 5, 
                   dpi = 300, 
                   filename = sprintf('doc/fig/%s/distance_psychometric.%s', target, target))
  
  
  allConditionData <- processDistanceData(scale=0)
  
  N <- length(unique(allConditionData[[1]]$participant))
  
  allConditionModels <- logisticDistanceModels(allConditionData)
  
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

# export distance data ----

exportDistanceData <- function() {
  
  allConditionData <- processDistanceData(scale=0)
  
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
  
  write.table(allData, 
              file = 'data/distance.tsv', 
              sep = "\t", 
              row.names = FALSE)
  
}

# statistics ----

getGlmANOVAdata <- function() {
  
  allConditionData <- processDistanceData()
  
  allData <- NA
  
  for (location in c('bsa','out')) {
    
    for (eye in c('ipsi','cont')) {
      
      cond_df <- allConditionData[[sprintf('%s_%s', eye, location)]]
      
      if (is.data.frame(allData)) {
        allData <- rbind(allData, cond_df)
      } else {
        allData <- cond_df
      }
      
    }
    
  }
  
  for (participant in unique(allData$participant)) {
    pdf <- allData[which(allData$participant == participant),]
    cond_mod <- glm(Targ_chosen ~ Difference, family=quasibinomial(link="logit"), data=pdf)
    print(cond_mod$coefficients)
  }
  
}


doANOVAonGLM <- function() {
  
  data <- getGlmANOVAdata()
  
 
}