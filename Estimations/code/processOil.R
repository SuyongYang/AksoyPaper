processOil<-function( oilfile="../data_17_07/oilprices.csv")
{
	oilPrices<-read.csv(oilfile,header=T,na.strings=c("#N/A"))

	for(year in unique(inflationData$Year))
	{
		oilPrices$Oil.Price[oilPrices$Year == year]<-oilPrices$Oil.Price[oilPrices$Year == year]/inflationData$CPI[inflationData$Year==year & inflationData$Country == "United States"]
	}

	return(oilPrices)
}
