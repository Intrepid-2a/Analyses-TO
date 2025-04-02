

# first example from: https://link.springer.com/content/pdf/10.3758/BF03192747.pdf

# logistic function:
# https://en.wikipedia.org/wiki/Logistic_function
# this can be changed to have max/min parameters in there (could be a single symmetric cutoff or margin even)

# we should then use the gnlm::gnlr function to fit our modified logistic function to the data


Contrast = c(0.010, 0.016, 0.025, 0.040, 0.063, 0.100)

df <- data.frame(Contrast = c(0.010, 0.016, 0.025, 0.040, 0.063, 0.100),
                 NumYes = c(40, 47, 65, 107, 155, 149),
                 NumNo = c(120, 113, 95, 53, 5, 11))

resp.mat <- matrix(c(df$NumYes, df$NumNo), ncol = 2, byrow = FALSE)

df <- data.frame(NumYes = c(40, 47, 65, 107, 155, 149),
                 NumNo = c(120, 113, 95, 53, 5, 11))


wb <- function(p) {
  p[3] - (1 - p[3] - p[4]) * (1 - exp(-((Contrast/p[1])^exp(p[2]))))
}

sim.fit <- gnlm::gnlr(y = resp.mat, 
                      distribution = "binomial",
                      mu = wb,
                      pmu = c(0.04, log(3.4), 0.25, 0.017))

sim.fit <- gnlm::gnlr(y = resp.mat, 
                      distribution = "binomial",
                      mu = wb,
                      pmu = c(.01, 0.7, .01, .01))

