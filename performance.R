# First run the chunks in listeriaSummary

final <- readxl::read_xlsx('~/Documents/ListeriaData/FinalReformat.xlsx')
actual <- (final
  %>% filter(year==2018&epiweek>=8)
  %>% select(epiweek,totalCases)
)
points(actual$epiweek,actual$totalCases,pch=21,lwd=3,col='#F7941E')
