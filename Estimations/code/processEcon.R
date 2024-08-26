processEcon<-function(capFormFile="../data_17_07/oecd_GDP_Capital.csv",savingsFile="../data_17_07/oecd_savings.csv")

#version with core eu (germany, france, spain, italy) in the sample
#processEcon<-function(capFormFile="../OECD/annual_GDP_CapForm_eu.csv",savingsFile="../OECD/annual_GDP_Savings.csv")

{
	# Read data from the two tables
	econData<-read.table(capFormFile,header=T,sep=",",quote="\"",na.strings="..")
	saveData<-read.table(savingsFile,header=T,sep=",",quote="\"",na.strings="..")
	
	# Calculate rates from absolute amounts
	econData$Investment.Rate <- econData$NGFCF/econData$NGDP	
	saveData$Savings.Rate<-saveData$NSavings/saveData$NGDP

	
	# Delete columns that are duplicates
	saveData$NGDP<-NULL

	# Merge all the data into a single frame
	#fullData<-merge(merge(merge(econData,saveData),lending),bop)
	fullData<-merge(econData,saveData)
	fullData$Year <- as.numeric(fullData$Year)
	fullData
}

processInflation <- function( file = "../data_17_07/imf_ifs_cpi.csv", ukFile = "../data_17_07/oecd_uk_cpi.csv" )
{
	#inflationData<-read.table("../IMF_IFS/annual_cpi_28_10_11.csv",header=T,sep=",",quote="\"",na.strings=c("...","-"))
	inflationData<-read.table(file,header=T,sep=",",quote="\"",na.strings=c("...","-"))
	inflationData$infl<-c(inflationData$CPI[-1]/inflationData$CPI[-length(inflationData$CPI)]-1,0)
	inflationData$alt.infl<-c(0,inflationData$CPI[-length(inflationData$CPI)]/inflationData$CPI[-1]-1)
	inflationData$Year <- as.numeric(inflationData$Year)
	
	ukInfl<-read.table(ukFile,header=T,sep=",",quote="\"",na.strings=c(".."))
	ukInfl$infl<-c(ukInfl$CPI[-1]/ukInfl$CPI[-length(ukInfl$CPI)]-1,0)
	ukInfl$alt.infl<-c(0,ukInfl$CPI[-length(ukInfl$CPI)]/ukInfl$CPI[-1]-1)
	ukInfl$Year <- as.numeric(ukInfl$Year)
	
	rbind(inflationData,ukInfl)
}

processNewInflation <- function()
{
  inflationData<-read.table("../IMF_IFS/annual_cpi_28_10_11.csv",header=T,sep=",",quote="\"",na.strings=c("...","-"))
	#inflationData<-read.table("../IMF_IFS/annual_cpi.csv",header=T,sep=",",quote="\"",na.strings=c("...","-"))
	inflationData$infl<-c(inflationData$CPI[-1]/inflationData$CPI[-length(inflationData$CPI)]-1,0)
	inflationData$alt.infl<-c(0,inflationData$CPI[-length(inflationData$CPI)]/inflationData$CPI[-1]-1)
	inflationData$Year <- as.numeric(inflationData$Year)
	inflationData
}
