# fitFunctions.R

# For fitting noise around epidemic curve

nll.nb <- function(log.c = 0, logodds = 0, data = cases$count){
  # Transform values
  c <- exp(log.c)
  prob <- 1 / (1 + exp(logodds))
  -sum(dnbinom(data,c,prob,log=T))
}

nll.pois <- function(log.l = 0, data = cases$count){
  # Transform value
  ll <- exp(log.l)
  -sum(dpois(data,ll,log=T))
}

# For fitting incubation period distribution

nllGammaIcens <- function(logmean = NULL, lograte = NULL){
  pp <- exp(c(logmean,lograte))
  alpha <- pp[1]*pp[2]
  lambda <- pp[2]
  # Calculate probabilities of being in intervals
  intervalProb <- (
    pgamma(perInc_max
           , shape = alpha, rate = lambda
           , log = F) -
      pgamma(perInc_min
             , shape = alpha, rate = lambda
             , log = F)
  )
  # Adjust for data with two exposures
  firstExp <- (
    pgamma(perInc_min[ipOr] + 0.5
           , shape = alpha, rate = lambda
           , log = F) -
      pgamma(perInc_min[ipOr] - 0.5
             , shape = alpha, rate = lambda
             , log = F)
  )
  secondExp <- (
    pgamma(perInc_max[ipOr] + 0.5
           , shape = alpha, rate = lambda
           , log = F) -
      pgamma(perInc_max[ipOr] - 0.5
             , shape = alpha, rate = lambda
             , log = F)
  )
  intervalProb[ipOr] <- firstExp+secondExp
  nll <- -sum(log(intervalProb))
  return(nll)
}