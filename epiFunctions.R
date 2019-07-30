# epiFunctions.R

# For half-life of 2 weeks, prob = 1-sqrt(2)/2
# For mean of 2 weeks, prob = 1/3

calcProb <- function(meanTime){
  return(1/(meanTime+1))
}

exposureProfile <- function(baseline,effectiveness,prop = 1-calcProb(expectedShelfLife),recallWeek=10,maxWeek=18){
  
  re <- c(rep(baseline,recallWeek-1),baseline*(1-effectiveness))
  while(length(re)<maxWeek){
    re <- c(re,prop*re[length(re)])
  }
  
  exp <- tibble(week = 1:maxWeek, riskExp = re)
  
}

projCurve <- function(casesGen # future cases generated through risky exposures
                      , gamMean = meanIncGamma
                      , gamRate = rateIncGamma
                      , maxWeek = length(casesGen)
                      , breaks=seq(3.5,maxWeek*7,7)
                      , week=1:maxWeek
                      , browse = F
                      ){
  if(browse) browser()
  prop <- diff(pgamma(c(0,breaks),gamMean*gamRate,gamRate))
  expected <- rep(0,maxWeek)
  for(ww in week){
    spread <- casesGen[ww]*prop
    expected[ww:maxWeek] <- expected[ww:maxWeek] + spread[1:(maxWeek-ww+1)]
  }
  return(expected)
}
