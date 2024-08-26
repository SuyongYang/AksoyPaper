processForecasts<-function()
{
	demoForecasts<-read.csv("../data_17_07/WPP2017_Forecasts.csv",sep=",",quote='"',header=T,strip.white = T)
	demoForecasts$Country<-as.character(demoForecasts$Country)
	demoForecasts$Country[demoForecasts$Country=="United States of America"] <- "United States"
	
	allCols<-ncol(demoForecasts)
	
	for(group in 1:7)
	{
		demoForecasts[,paste("X",group,sep="")]<-demoForecasts[,2*group +1]+demoForecasts[,2*group +2]
	}

	demoForecasts$X8<-rowSums(demoForecasts[,(8*2+1):allCols])

	demoForecasts$Total<-rowSums(demoForecasts[,(allCols+1):(allCols+8)])

	maxDiff<-max(abs(demoForecasts$Total-rowSums(demoForecasts[,3:allCols])))
	if(maxDiff>1)
	{
		print("Incorrect sums!")
	}

	for(group in 1:8)
	{
		demoForecasts[,paste("p",group,sep="")]<-demoForecasts[,paste("X",group,sep="")]/demoForecasts$Total
	}

	maxDiff<-max(abs(1-rowSums(demoForecasts[,(ncol(demoForecasts)-7):ncol(demoForecasts)])))
	if(maxDiff>1)
	{
		print("Incorrect proportions!")
	}

	for(group in 1:7)
	{
		demoForecasts[,paste("alpha_",group,sep="")]<-demoForecasts[,paste("p",group,sep="")]-demoForecasts$p8
	}
	demoForecasts
}

processHistoric<-function()
{
	demoForecasts<-read.csv("../data_17_07/WPP2017_POP_F15_1_ANNUAL_POPULATION_BY_AGE_BOTH_SEXES_modified.csv",na.strings=c(".."),sep=",",quote='"',header=T)
	demoForecasts$Country<-as.character(demoForecasts$Country)
	demoForecasts$Country[demoForecasts$Country=="United States of America"] <- "United States"
#	demoForecasts<-demoForecasts[demoForecasts$Country %in% c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Greece","Iceland","Ireland","Italy","Japan","Luxembourg","Netherlands","New Zealand","Norway","Portugal","Sweden","Switzerland","United Kingdom","United States"),]	
	allCols<-ncol(demoForecasts)
	
	for(group in 1:7)
	{
		demoForecasts[,paste("X",group,sep="")]<-demoForecasts[,2*group +1]+demoForecasts[,2*group +2]
	}

	# Because the data is inconsistent - sometimes there is only 80+, sometimes there is 80+ and the older age groups,
	# and sometimes there is only the older age groups, need to adjust the 80+ to only be there if the older age groups
	# are not
	demoForecasts$X80.[!is.na(demoForecasts$X80.84)] <- 0
	demoForecasts$X8<-rowSums(demoForecasts[,(8*2+1):24],na.rm=T)

	demoForecasts$Total<-rowSums(demoForecasts[,(allCols+1):(allCols+8)])
	demoPData<-pdata.frame(demoForecasts,c("Country", "Year"))
#	is.pconsecutive(demoPData, index=c("Country", "Year"))
#	demoForecasts<-pdata.frame(demoForecasts,c("Country", "Year"))
	demoPData$popGrowth<-diff(log(demoPData$Total))
	demoForecasts <- merge(demoForecasts, demoPData[,c("Country","Year", "popGrowth")]) 
	

	maxDiff<-max(abs(demoForecasts$Total-rowSums(demoForecasts[,3:allCols],na.rm = T)),na.rm=T)
	if(maxDiff>1)
	{
		print("Incorrect sums!")
	}

	for(group in 1:8)
	{
		demoForecasts[,paste("p",group,sep="")]<-demoForecasts[,paste("X",group,sep="")]/demoForecasts$Total
	}

	maxDiff<-max(abs(1-rowSums(demoForecasts[,(ncol(demoForecasts)-7):ncol(demoForecasts)])))
	if(maxDiff>1)
	{
		print("Incorrect proportions!")
	}

	for(group in 1:7)
	{
		demoForecasts[,paste("alpha_",group,sep="")]<-demoForecasts[,paste("p",group,sep="")]-demoForecasts$p8
	}
	demoForecasts
}
