processCBRates<-function( cbFile = "../data_17_07/imf_ifs_rates.csv")
{
	# Read the CB rates from the IMF IFS data
	cb_rates<-read.table(cbFile,header=T,sep=",",quote="\"",na.strings=c("","...","-"))

	cb_rates$CB.Rate <- apply(cb_rates, 1, function(x) if(!is.na(x[4])) x[4] else if(!is.na(x[3])) x[3] else x[5])
	
	euroCountries <- c("France","Austria","Belgium","Finland","Germany","Ireland","Italy","Netherlands","Portugal","Spain")
	
	euroAreaData <- cb_rates[cb_rates$Country == "Euro Area" & cb_rates$Year > 1998, ]
	
	for(country in euroCountries) {
	  cb_rates <- cb_rates[!(cb_rates$Country == country & cb_rates$Year > 1998),]
	  euroAreaData$Country <- country
	  cb_rates <- rbind(cb_rates,euroAreaData)
	}
	
	# Greece joined later...
	euroAreaData$Country <- "Greece"
	cb_rates <- rbind(cb_rates,euroAreaData[euroAreaData$Year>2000,])

	# Remove the Euro Area
	cb_rates<-cb_rates[cb_rates$Country != "Euro Area",c("Country","Year","CB.Rate")]
  cb_rates$CB.Rate <- as.numeric(cb_rates$CB.Rate)
	cb_rates
}

processNewCBRates<-function()
{
	# Read the CB rates from the IMF IFS data
	cb_rates<-read.table("../data_02_12_11/annual_CB_disc_rate_02122011.csv",header=T,sep=",",quote="\"",na.strings=c("...","-"))

	# Read the French Call Money Rates, also from the IMF
	fr_rate <-read.csv("../Individual/france_call_money_rate.csv",header=T,sep=",",quote="\"",na.strings="...")
	fr_rate$Country <- "France"
	names(fr_rate)[2]<-"CB.Rate"
	cb_rates <-rbind(cb_rates,fr_rate) 
	
	# Push the data for Euro rates to the countries as relevant
	cb_rates[cb_rates$Country %in% c("France","Austria","Belgium","Finland","Germany","Ireland","Italy","Netherlands","Portugal","Spain") & cb_rates$Year>1998,]$CB.Rate<-cb_rates[cb_rates$Country == "Euro Area"&cb_rates$Year>1998,]$CB.Rate
	cb_rates[cb_rates$Country %in% c("Greece") & cb_rates$Year>2000,]$CB.Rate<-cb_rates[cb_rates$Country == "Euro Area"&cb_rates$Year>2000,]$CB.Rate

	# Add the dutch rates from the Dutch CB (from 94 onwards discount rate discontinues; the data comes from fixed advance rate at DNB https://www.dnb.nl/en/statistics/statistics-dnb/financial-markets/interest-rates/index.jsp)
	dutchRates<-c(4.5,2.75,2.0,2.5,2.75)
	for(index in 0:(length(dutchRates)-1))
	{
		cb_rates$CB.Rate[cb_rates$Country=="Netherlands" & cb_rates$Year == (1994+index)] <- dutchRates[index+1]
	}

	# Add the UK rates from the BoE
	#uk_rates<-read.csv("../Individual/uk_base_rate.csv",header=T,sep=",",quote="\"")
	#cb_rates[810:836,]$CB.Rate <- uk_rates[7:33,]$CB.Rate
	
	# Add the Oz Rates from the BoA
	#oz_rates<-read.csv("../Individual/oz_base_rate.csv",header=T,sep=",",quote="\"")
	#cb_rates[1:38,]$CB.Rate <- oz_rates[1:38,]$Cash.Rate
	debugOut("Here")
	# Remove the Euro Area
	cb_rates<-cb_rates[cb_rates$Country != "Euro Area",]

	# Extrapolate the remaining missing values
	#cb_rates$CB.Rate<-na.approx(cb_rates$CB.Rate)

	cb_rates
}
