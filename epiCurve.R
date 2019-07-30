# epiCurve.R
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

hist(cases$count,seq(0,45,3))

nbfit <- mle2(nll.nb
              , start = c(log.c = 0, logodds = 0)
)

pfit <- mle2(nll.pois
              , start = c(log.l = 0)
)
AIC(nbfit)
AIC(pfit)

exp(coef(pfit))