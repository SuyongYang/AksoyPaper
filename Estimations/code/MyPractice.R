capFormFile="../data_17_07/oecd_GDP_Capital.csv"
savingsFile="../data_17_07/oecd_savings.csv"
  
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

############################
  
file = "../data_17_07/imf_ifs_cpi.csv"
ukFile = "../data_17_07/oecd_uk_cpi.csv" 

  #inflationData<-read.table("../IMF_IFS/annual_cpi_28_10_11.csv",header=T,sep=",",quote="\"",na.strings=c("...","-"))
  inflationData<-read.table(file,header=T,sep=",",quote="\"",na.strings=c("...","-"))
  #inflationData<-read.csv(file,header=T,sep=",",quote="\"",na.strings=c("...","-"))
  inflationData$infl<-c(inflationData$CPI[-1]/inflationData$CPI[-length(inflationData$CPI)]-1,0)
  inflationData$alt.infl<-c(0,inflationData$CPI[-length(inflationData$CPI)]/inflationData$CPI[-1]-1)
  inflationData$Year <- as.numeric(inflationData$Year)
  
  ukInfl<-read.table(ukFile,header=T,sep=",",quote="\"",na.strings=c(".."))
  ukInfl$infl<-c(ukInfl$CPI[-1]/ukInfl$CPI[-length(ukInfl$CPI)]-1,0)
  ukInfl$alt.infl<-c(0,ukInfl$CPI[-length(ukInfl$CPI)]/ukInfl$CPI[-1]-1)
  ukInfl$Year <- as.numeric(ukInfl$Year)
  
  rbind(inflationData,ukInfl)

############################################
  
Pat_panel <- pdata.frame(PATAPPRESID, index = c("Country", "Year"))

ggplot(inflationData, aes(x = Year, y = InflationRate, color = Country, group = Country)) +
    geom_line() +
    geom_point() +
    labs(title = "Inflation Rate Over Time by Country",
         x = "Year",
         y = "Inflation Rate (%)") +
    theme_minimal()
  