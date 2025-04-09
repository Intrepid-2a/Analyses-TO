
getDistanceCurvatureRelationData <- function(Eye=c('ipsilateral')) {
  
  curvature <- getCurvatureANOVAdata()
  distance <- getDistANOVAdata()
  
  # Merge the two data frames on the 'participant' column
  df <- merge(curvature, distance, by = c("participant", "Eye", "Location"))
  
  df <- df[which(df$Eye %in% Eye), ]
  
  # Rename the columns for clarity
  colnamemap <- list("mean.x" = "PSE_curve",
                     "mean.y" = "PSE_dist")
  for (k in names(colnamemap)) {
    colnames(df)[colnames(df) == k] <- colnamemap[[k]]
  }
  
  df$participant <- as.character(df$participant)
  
  dist_df <- aggregate(PSE_dist ~ Location + participant, data = df, FUN = mean)
  dist_df <- aggregate(PSE_dist ~ participant, data = dist_df, FUN = diff)
  curve_df <- aggregate(PSE_curve ~ Location + participant, data = df, FUN = mean)
  curve_df <- aggregate(PSE_curve ~ participant, data = curve_df, FUN = diff)
  
  out_df <- merge(dist_df, curve_df, by = c("participant"))
  
  return(out_df)
  
}

odregress <- function(x, y) {
  stopifnot(is.numeric(x), is.numeric(y))
  
  Z <- cbind(x, y)  # bind data in one matrix
  n <- nrow(Z)      # no. of data points
  m <- ncol(Z) - 1  # no. of independent variables
  
  # this line is different, because we don't want `repmat()` as dependency
  meanZ <- matrix(1, n, 1) %x% matrix(apply(Z, 2, mean), nrow=1, ncol=m+1)
  
  svdZ <- svd(Z - meanZ) # singular value decomposition
  V <- svdZ$v # eigen vectors
  
  # coefficients (a) and intercept (b)
  a <- -V[1:m, m+1] / V[m+1, m+1]
  b <- mean(Z %*% V[, m+1]) / V[m+1, m+1]
  
  # Fitted values
  yfit <- cbind(x, 1) %*% c(a, b)
  resd <- y - yfit
  
  # orthogonal distance
  normal <- V[, m+1]
  err <- abs((Z - meanZ) %*% normal)
  ssq <- sum(err^2)
  

  
  if (dim(Z)[2] == 2) {
    
    sZ <- scale(Z, scale=FALSE)
    r <-  sum(apply(sZ, 1, prod)) / prod( sqrt( colSums(sZ^2) ) )
    
    return( list(coeff = c(a, b), ssq = ssq, err = err,
                 fitted = yfit, resid = resd, normal = normal,
                 r = r
    ) )
    
  }
  
  return( list(coeff = c(a, b), ssq = ssq, err = err,
               fitted = yfit, resid = resd, normal = normal
  ) )
}



plotEffectStrength <- function() {
  
  df <- getDistanceCurvatureRelationData(Eye = c('ipsilateral'))
  
  # Fit the orthogonal distance regression
  # odr <- odregress(x=df$PSE_curve, y=df$PSE_dist)
  # odr <- odregress(df$PSE_dist, df$PSE_curve)
  
  # Create a scatter plot with the fitted line
  plot(NULL,NULL,
       xlim=c(-1.,1.5), ylim=c(-0.5,2.5),
       xlab = "at-away PSE Curvature [dva-1]", ylab = "at-away PSE Distance [dva]",
       bty='n',ax=F)
  
  points(df$PSE_curve, df$PSE_dist, pch = 19, col = "#AA00AA44", cex = 1.5)
  
  
  # Add the fitted line
  # X <- range(df$PSE_curve, na.rm = TRUE)
  # Y <- (range(df$PSE_curve, na.rm = TRUE)*odr$coeff[1])+odr$coeff[2]
  # lines(x = X,
  #       y = Y + mean(df$PSE_dist), col = "red")
  
  
  # regular regression line
  lm_fit <- lm(PSE_dist ~ PSE_curve, data = df)
  # print(summary(lm_fit))
  abline(lm_fit, col = "blue", lty = 1)
  
  axis(side=1,at=c(-0.5,0,0.5,1.0))
  axis(side=2,at=seq(-0.5,2.5,0.5))
}



regressionEffectStrength <- function() {
  
  df <- getDistanceCurvatureRelationData(Eye = c('ipsilateral'))
  lm_fit <- lm(PSE_dist ~ PSE_curve, data = df)
  print(summary(lm_fit))
  
  blm <- BayesFactor::lmBF(PSE_dist ~ PSE_curve, data=df)
  
  print(blm)
}