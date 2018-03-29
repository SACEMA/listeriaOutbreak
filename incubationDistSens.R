# incubationDistSens.R
require(tidyverse)
require(bbmle)
require(interval)

fname <- '~/Dropbox\ (Personal)/SACEMA/NICD/Listeria/Goulet2013.xlsx'
sname <- 'incDistGouletSens.Rdata'

if(sname %in% dir()){
  load(sname)
}else{
  
  incDat <- (
    readxl::read_xlsx(fname)
  )
  
  incDat <- (
    incDat
    %>% filter(.,ref != 25) # remove data from outbreak where source is questionable
    %>% filter(.,ref != 27) # remove data from outbreak without individual-level information
    %>% select(.,-c(id,country,year,ref,food))
  )
  
  # Duration of the incubation period
  perInc_min <- rep(NA,nrow(incDat))
  perInc_max <- rep(NA,nrow(incDat))
  # Add uncertainty for 'known' incubation periods
  ipKnown <- which(incDat$treatment == 'known')
  perInc_min[ipKnown] <- incDat$incPer[ipKnown] - .5
  perInc_max[ipKnown] <- incDat$incPer[ipKnown] + .5
  # Add uncertainty for incubation periods based on sampling times
  ipSens <- which(incDat$treatment == 'sens')
  perInc_min[ipSens] <- incDat$incPerMax[ipSens] - sampleDelay
  perInc_min[perInc_min<0] <- 0
  perInc_max[ipSens] <- incDat$incPerMax[ipSens]
  # Add uncertainty for left censored data
  ipLeft <- which(incDat$treatment == 'censored')
  perInc_min[ipLeft] <- 0
  perInc_max[ipLeft] <- incDat$incPerMax[ipLeft]
  # Add uncertainty for interval censored data
  ipOr <- which(incDat$treatment == 'or')
  perInc_min[ipOr] <- incDat$incPerMin[ipOr]
  perInc_max[ipOr] <- incDat$incPerMax[ipOr]
  
  npmle_perInc <- icfit(perInc_min,perInc_max,conf.int = T,control=icfitControl(B=bsIterations))
  
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
  
  initParams <- c(logmean = log(20), lograte = log(1))
  estIncGamma <- mle(nllGammaIcens,start = as.list(initParams),method = )
  ciIncGamma <- confint(estIncGamma)
  
  ciIncGamma_Rate <- round(unname(exp(ciIncGamma[2,])),2)
  
  meanIncGamma <- round(unname(exp(coef(estIncGamma)[1])),2)
  rateIncGamma <- round(unname(exp(coef(estIncGamma)[2])),2)
  ciIncGamma <- round(unname(exp(ciIncGamma[1,])),2)
  
  save(npmle_perInc,meanIncGamma,rateIncGamma,ciIncGamma,file = sname)
}
