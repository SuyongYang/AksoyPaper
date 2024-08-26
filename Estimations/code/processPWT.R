processPWTNew<-function()
{
	read.table("../pwt/pwt_new.csv",header=T,sep=",",quote='\"',na.strings="na")
}

processInv <- function(pwtfile = "../data_17_07/pwt_9_RGDP.csv", ...)
{
	# Read the data on GDP and the investment ratio from 1950 onward
	pwt_inv50<-read.table(pwtfile,header=T,sep=",",quote="\"",na.strings="na")

	#pwt_inv50 <- calculateCapital(pwt_inv50,...)
	return(pwt_inv50[,c("Country","Year","RGDPL")])
}

calculateCapital <- function(data, rateName="ki", popName="POP", depreciation_rate = 0.1, inv.impact.immediate=F)
{
	# Calculate real investment from real GDP and the investment ratio
	data$I <- data$RGDPL*data[,rateName]*data[,popName]/1000
	data$K <- 0
	
	investAdjust = 1
	if(inv.impact.immediate)
	{
		investAdjust = 0
	}
	
	# Calculate the cumulative investment one country at a time
	for(country in levels(data$Country))
	{
		initial_year  <- min(data$Year[complete.cases(data[data$Country == country,c("RGDPL",rateName,popName)])])

		if(initial_year > 1950)
		{
			cat(country,": ",initial_year,"\n")
		}

		if(initial_year > 2010)
		{
			next;
		}

		# Calculate the assumed steady-state growth rate of capital from the initial growth rate of GDP, implicitly
		# assuming that K and Y grow at the same rate in steady state, and that the economy was close to steady state
		initial_growth <- ((data[data$Country==country & data$Year==(initial_year+5),"RGDPL"]*data[data$Country==country & data$Year==(initial_year+5),popName])/(data[data$Country==country & data$Year==initial_year,"RGDPL"]*data[data$Country==country & data$Year==initial_year,popName]))^(1/5)-1
	
		# Calculate the initial estimated capital base
		data$K[data$Country==country & data$Year==initial_year]  <- (1+initial_growth)/(initial_growth+depreciation_rate)*data$I[data$Country==country & data$Year==initial_year]
	
		# Now for each year calculate the capital base from the depreciated previous year's capital base plus investment	
		for(year in (initial_year+1):max(data$Year))
		{
			data$K[data$Country==country & data$Year==year] <- data$K[data$Country==country & data$Year==(year-1)]*(1-depreciation_rate)+data$I[data$Country==country & data$Year==(year-investAdjust)]
		}
	}

	return(data)
}

loadConversionRates <- function()
{
	pwt<-read.table("../pwt/pwt.csv",header=T,sep=",",quote="\"",na.strings="na")
	return(pwt[,c("Country","Year","XRAT","PPP")])
}
