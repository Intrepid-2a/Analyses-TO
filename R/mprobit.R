

# probit with min/max margins
mprobit <- function(p, x) {
  
  # p is a vector of parameters
  # the first two are the regular parameters of the normal distribution:
  # p[1] is the mean
  # p[2] is the standard deviation
  
  # the second two parameters set the bounds:
  # p[3] is the lower margin
  # p[4] is the upper margin
  
  # if only 3 parameters are given,
  # it is taken to be both the upper and lower margin
  
  if (length(p) == 3) {
    p[4] <- p[3]
  }
  
  # p[3] + p[4] has to be lower than 1:
  if (p[3] + p[4] >= 1) {
    cat('warning: impossible margins (returning zeroes)\n')
    return(rep(0, length(x))) # might need to have a better value to return in case if invalid bounds
  }
  
  # x is a vector of values
  
  # probit is implemented in R as the pnorm function,
  # but we add bounds:
  # print(p[1])
  # print(p[2])
  # print(x)
  # print(pnorm(x, mean=p[1], sd=p[2] ))
  
  return( p[3] + ((1 - p[3] - p[4]) * pnorm(x, mean=p[1], sd=p[2], lower.tail=TRUE, log.p=FALSE)) )
  
}

mprobit.nll <- function(p, x, y) {
  
  # p is a vector of parameters
  # the first two are the regular parameters of the normal distribution:
  # p[1] is the mean
  # p[2] is the standard deviation
  
  # the second two parameters set the bounds:
  # p[3] is the lower margin
  # p[4] is the upper margin
  
  # x is a vector of values
  # y is a vector of values (0 or 1)
  
  # probit is implemented in R as the pnorm function,
  # but we add bounds:
  
  prob <- mprobit(p, x)
  
  # print(prob)
  
  prob[which((prob-1) == 0)] <- 1-.Machine$double.eps
  
  # this is the negative log likelihood,
  # which (when minimized) should give the maximum likelihood estimate
  return(-sum(y * log(prob) + (1 - y) * log(1 - prob)))
  
}

#' @title Probit regression with min/max margins
#' @description 
#' This function fits a probit regression model with min/max margins to the data.
#' It uses the `mprobit` function to calculate the probit function with margins.
#' It also uses the `mprobit.nll` function to calculate the negative log-likelihood for optimization.
#' @param x A vector of predictor variables.
#' @param y A vector of response variables (0 or 1).
#' @param start A vector of starting values for the parameters.
#' @param lower A vector of lower bounds for the parameters.
#' @param upper A vector of upper bounds for the parameters.
#' @param maxit The maximum number of iterations for the optimization.
#' @export
fit.mprobit <- function(x, y, start, lower, upper, maxit=1000) {
  
  # x is a vector of predictor variables
  # y is a vector of response variables (0 or 1)
  
  # start is a vector of starting values for the parameters
  # lower is a vector of lower bounds for the parameters
  # upper is a vector of upper bounds for the parameters
  
  # maxit is the maximum number of iterations for the optimization
  
  # fit the model using optim
  fit <- optim(par=start,
               fn=mprobit.nll,
               x=x,
               y=y,
               method="L-BFGS-B",
               lower=lower,
               upper=upper,
               control=list(maxit=maxit))
  
  return(fit)
  
}

CI.mprobit <- function(data, start, lower, upper, maxit=1000, bootstraps=1000, n=100, from=NULL, to=NULL, interval=c(0.025, 0.975)) {
  
  # data is a data frame with the predictor and response variables ('x' and 'y')
  #      as well as the observation ID ('ID')
  #      'y' should be raw: an ecdf will be created for each bootstrap iteration
  # start is a vector of starting values for the parameters
  # lower is a vector of lower bounds for the parameters
  # upper is a vector of upper bounds for the parameters
  
  # maxit is the maximum number of iterations for the optimization
  # bootstraps is the number of bootstrap samples to generate
  # n is the number of X values to estimate the CI at
  # from is the minimum value of X (defaults to the lowest value in data)
  # to is the maximum value of X (defaults to the highest value in data)
  # interval is the confidence interval
  
  # fit the model using optim
  fit <- optim(par=start,
               fn=mprobit.nll,
               x=data$Difference,
               y=data$Targ_chosen,
               method="L-BFGS-B",
               lower=lower,
               upper=upper,
               control=list(maxit=maxit))
  
  return(fit)
}


# mprobit.se.fit <- function(par, x, y, newx) {
#   
#   # the output: se.fit
#   # should be the standard error of the mean of the predicted value
#   
#   # par is a vector of parameters
#   # x and y were used to thit the parameters
#   # newx is where we want to predict new values
#   
#   r <- residual?
#   # r <- residuals(mprobit(par, x, y))
#   rss <- r^2
#   df <- length(x) - length(par)
#   res.var <- r / df
#   
# }


# we could use instead, something like this:
# - gls {nlme}
# 



























# test other fitting procedures ----
# MLE would match the default procedure used in glm()
# right now we use least squares minimization


tests <- function() {
  
  allData <- processDistanceData()
  
  df <- allData[['ipsi_bsa']]
  
  # nls fits work:
  
  # mod <- nls(Targ_chosen ~ mprobit(p, Difference),
  #            data = df,
  #            start = list(p = c(-0.5, 1, 0)),
  #            # lower = c(-3, 0.2, 0, 0),
  #            # upper = c(3, 3, 0.3, 0.3)
  #            )
  
  # nlrob also works:
   
  modr <- robustbase::nlrob(Targ_chosen ~ mprobit(p, Difference),
                            data = df,
                            start = list(p = c(-0.5, 1, 0)),
                            # lower = list('p' = c(-3, 0.2, 0  )),
                            # upper = list('p' = c( 3, 3,   0.3)),
                            # method="mtl",
                            doCov=TRUE
                            )
  
  fit <- fit.mprobit(x = df$Difference,
              y = df$Targ_chosen,
              start = c(-0.5, 1, 0),
              lower = c(-3, 0.2, 0),
              upper = c(3, 3, 0.3))
  
  r <- df$Targ_chosen - mprobit(fit$par, df$Difference)
  rss <- sum(r^2)
  df <- length(df$Difference) - length(par)
  res.var <- rss / df
  
  rank <- 2 # 1 + n variables??
  p <- rank
  p1 <- seq_len(p)
  
  # I can not do the rest:
  
  # # qrX <- qr.lm(object))$pivot[p1]
  # X <- model.matrix(~ Difference, data = df) # ????? 
  # # X[, piv] %*% qr.solve(qr.R(qrX)[p1, p1])
  # # ip <- drop(XRinv^2 %*% rep(res.var, p))
  # 
  # newX <- seq(-3.5,3.5,0.1)
  # C <- matrix(data = c(rep(1,length(newX)), newX), nrow = length(newX), ncol = 2)
  # 
  # # C <- c(1, 1.5)
  # std.er <- sqrt(t(C) %*% vcov(fit$par) %*% C)
  
}


testMLE <- function() {
  
  allData <- processDistanceData()
  
  df <- allData[['ipsi_bsa']]
  
  x = df$Difference
  y = df$Targ_chosen
  mlemod <- stats4::mle(minuslogl=mprobit.nll,
                        start = list('p' = c(-0.5, 1, 0)),
                        # fixed = list(x = x, y = y),
                        method = "L-BFGS-B",
                        lower = c(-3, 0.01, 0),
                        upper = c(3, Inf, 0.3),
                        x=x,
                        y=y)
  
  
} 
