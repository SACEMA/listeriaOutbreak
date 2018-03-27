# epiCurve.R
require(tidyverse)
require(EpiWeek)
require(bbmle)

fname <- '~/Documents/ListeriaData/Data for sitrep 210321.xlsx'

epiCurve <- (
  readxl::read_xlsx(fname)
  %>% select(.,c(year,cases,epiweeks,province))
  %>% group_by(.,year,epiweeks)
  %>% summarise(.,count=n())
)

cases <- (
  epiCurve
  %>% filter(., (year==2018 & epiweeks < 9) | (year==2017 & epiweeks >=40))
)

cases$count
mean(cases$count)
var(cases$count)
sd(cases$count)
plot(cases$count,type = 's',ylim=c(0,45))
abline(h=mean(cases$count),col='red',lty=3)

hist(cases$count,seq(0,45,3))

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

nbfit <- mle2(nll.nb
              , start = c(log.c = 0, logodds = 0)
)

pfit <- mle2(nll.pois
              , start = c(log.l = 0)
)
AIC(nbfit)
AIC(pfit)

exp(coef(pfit))

mean(cases$count)
