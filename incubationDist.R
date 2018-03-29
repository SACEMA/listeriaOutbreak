# incubationDist.R
require(tidyverse)
require(bbmle)
require(interval)

fname <- '~/Dropbox\ (Personal)/SACEMA/NICD/Listeria/Goulet2013.xlsx'
sname <- 'incDistGoulet.Rdata'

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
  # Add uncertainty for left censored data
  ipLeft <- which(incDat$treatment %in% c('sens','censored'))
  perInc_min[ipLeft] <- 0
  perInc_max[ipLeft] <- incDat$incPerMax[ipLeft]
  # Add uncertainty for interval censored data
  ipOr <- which(incDat$treatment == 'or')
  perInc_min[ipOr] <- incDat$incPerMin[ipOr]
  perInc_max[ipOr] <- incDat$incPerMax[ipOr]
  
  # Nonparametric fit
  npmle_perInc <- icfit(perInc_min,perInc_max,conf.int = T,control=icfitControl(B=bsIterations))

  # Parametric fit
  initParams <- c(logmean = log(1), lograte = log(1))
  estIncGamma <- mle(nllGammaIcens,start = as.list(initParams))
  ciIncGamma <- confint(estIncGamma)
  
  ciIncGamma_Rate <- round(unname(exp(ciIncGamma[2,])),2)
  
  meanIncGamma <- round(unname(exp(coef(estIncGamma)[1])),2)
  rateIncGamma <- round(unname(exp(coef(estIncGamma)[2])),2)
  ciIncGamma <- round(unname(exp(ciIncGamma[1,])),2)
  
  save(npmle_perInc,meanIncGamma,rateIncGamma,ciIncGamma,file = sname)
}
