# Checks historic data was processed correctly
checkSums <- function( data ) {
  sum(apply(data[,3:32], 1, function(r) {
    sum(r[1:22],na.rm = T) - sum(r[23:30],na.rm = T)
  }))
}

# Checks forecast data were processed correctly
checkForecastSums <- function( data ) { 
  sum(apply(data[,3:31], 1, function(r) {
    sum(r[1:21],na.rm = T) - sum(r[22:29],na.rm = T)
  }))
}

missingYearsForCountry <- function( data, country, years = 1971:2014 ) {
  dataForCountry <- data[data$Country == country, ]
  missingYears <- years[is.na(match(years,dataForCountry$Year))]
  if( length(missingYears) > 0 ) {
    print( paste(country,": ",paste( missingYears, collapse=","), sep="") )  
  }
} 

.defaultCountries = c("Austria","Belgium","Canada","Denmark","Finland","France","Greece","Iceland","Ireland","Japan","Netherlands","New Zealand","Norway","Portugal","Spain","Sweden","Switzerland","United Kingdom","United States")

missingYears <- function( data = allData, countries = .defaultCountries ) {
  complete <- data[complete.cases( data ),]
  for( cntry in countries) missingYearsForCountry( complete, cntry)
}

# missingYears(demoData[,c("Country","Year",demoVars)])
# missingYears(cb_rates)
# missingYears(econData)
# missingYears(inflationData)
# missingYears(capitalData[,c("Country", "Year", "RGDPL")])
# missingYears(patappresid)
# missingYears(labourData[,c("Country", "Year", "Hours")])
# missingYears(completeIndex)
# 
allvars <- Reduce(function(x, y) union(x,all.vars(y)), equations, c())
missingYears(allData[,c(idx,allvars)])

# for( country in unmeanique(allData$Country)) {
#   
#   plot(econForecasts1[forecastData1$Country==country & forecastData1$Year<2050,7]~forecastData1$Year[forecastData1$Country==country & forecastData1$Year<2050],type='l',xlab='Year',ylab=paste('D Impact: ',country,' Infl',sep=""))
#   dev.copy2pdf(file=paste('../tex/figures/PATAPP_rgdpl_1lag_pcpat/forecastInfTrends_',country,'.pdf',sep=''))
# }