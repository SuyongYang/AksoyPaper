library('plm')
library('xtable')
library('lmtest')
library('dyn')
library('dynlm')
library('mFilter')
library('astsa')
library('sandwich')
library('demography')
library('Matrix')
library("pwt9")



rm(list=ls())

source('./processEcon.R')
#source('./cb_rates.R')
source('./processLabour.R')
source('./functions.R')
source('./processOil.R')
source('./processForecasts.R') # Provides demographic data
#source('./junkForecasts.R')




PATAPPRESID<-read.csv("../data_17_07/wb_wdi_patents.csv",header=T,sep=",",quote="\"",na.strings=c("..","n.a.","-"))



CB.Rate<-read.csv("../data_17_07/annual_CB_rates.csv",header=T,sep=",",quote="\"",na.strings=c("..","n.a.","-"))

LR.Rate<-read.csv("../data_17_07/annual_longrates.csv",header=T,sep=",",quote="\"",na.strings=c("..","n.a.","-"))


#when open economy estimation are done open the following two lines that collects data from Lane (1970-2011) 
#nfa_gdp<-read.csv("../data_17_07/NetFA/nfagdponly.csv",header=T,sep=",",quote="\"",na.strings=c("..","n.a.","-"))
#nfa<-read.csv("../data_17_07/NetFA/nfaonly.csv",header=T,sep=",",quote="\"",na.strings=c("..","n.a.","-"))


# Age-specific fertility from Human Fertility Database

#asfr <- read.csv("../data_17_07/HFD/asfrRR.txt",

#with extended fertility dataset use asfrRR_mod file              
asfr <- read.csv("../data_17_07/HFD/asfrRR_mod.txt",
                 header=T,
                 sep="",
                 quote="\"",
                 na.strings=c(".","n.a.","-"))
codes <- read.csv("../data_17_07/countryCodes2.csv",
                                    header=T,
                                    sep=",")

asfr <-merge(asfr,codes)
asfr <- asfr[order(asfr$Country, asfr$Year, asfr$Age),]
levels(asfr$Age) <- 12:55

# fertility <- read.csv("../data_17_07/Fertility/HFC_ASFRstand_TOT.txt",
#                       header=T,
#                       sep=",",
#                       quote="\"",
#                       na.strings=c(".","n.a.","-"),
#                       strip.white = T)
# names(fertility)[1] <- "Code"
# codes <- read.csv("../data_17_07/countryCodes.csv",
#                   header=T,
#                   sep=",")
# 
# fertility<-merge(fertility,codes)
# fertility<-fertility[fertility$AgeDef=="ACY",c("Country","Year1","Age","ASFR")]
# names(fertility)[2] <- "Year"

deathRatesByAge<-read.csv("../data_17_07/deathRatesByAge.csv",
                          header=T,
                          sep=",",
                          quote="\"",
                          colClasses = c("factor","integer","integer","numeric"),
                          na.strings=c(".","n.a.","-"))

popByAge<-read.csv("../data_17_07/popByAge.csv",
                    header=T,
                    sep=",",
                    quote="\"",
                    colClasses = c("factor","integer","integer","numeric"),
                    na.strings=c(".","n.a.","-"))

# years <- sort(unique(asfr$Year))
# ages <-  sort(unique(asfr$Age))
# asfr$Country <- NULL
# countryData <- reshape(asfr,timevar="Year", idvar="Age",direction="wide")

matrixForCountry <- function( country, data, series="Deaths" ) {
  countryData <- data[data$Country == country,]
  countryData$Country <- NULL
  years <- sort(unique(countryData$Year))
  ages <-  sort(unique(countryData$Age))
  countryData <- reshape(countryData,timevar="Year", idvar="Age",direction="wide")
  row.names(countryData) <- countryData$Age
  countryData$Age <- NULL
  colnames(countryData) <- years
  countryData
}

leeCarterForCountry <- function( country, data=deathRatesByAge, pop=popByAge, series = "Deaths", type="mortality", max.age = 100 ) {
  # Need to use reshape to create a matrix
  rates <- matrixForCountry( country, data, series)
  population <- matrixForCountry( country, pop, "Population")
  
  cols <- intersect(colnames(rates), colnames(population))
  rows <- intersect(row.names(rates), row.names(population))
  rates <- rates[rows,cols]
  population <- population[rows,cols]
  
  dd <- demogdata(rates,population,as.numeric(rows),as.numeric(cols),type,country,"Total")
  lca(dd, adjust = "none",interpolate=T, max.age = max.age)
} 

countries<-c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Greece","Germany", "Iceland","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal","Spain","Sweden","Switzerland","United Kingdom","United States")

leeCarter <- function(  data=deathRatesByAge, pop=popByAge, ... ) {
  lcCountries <- intersect(countries,unique( data$Country ))
  
  res <- data.frame(Country=character(0), Year=integer(0), k=numeric(0))
  
  for( country in lcCountries ) {
    print(country)
    lcCountry <- data.frame(Country=character(0), Year=integer(0), k=numeric(0))
    tryCatch(lcCountry <- leeCarterForCountry(country, data, pop,... ),
             error = function(e) {
               print(paste(country, " FAILED!!!", sep="")) 
            })
    res <- rbind(res, 
                 data.frame(Country=rep(country,length(lcCountry$year)),Year=lcCountry$year,k=as.vector(lcCountry$kt)))
  }
  
  res
}

print("Calculating Mortality Index:")
lcData <- leeCarter()
names(lcData)[3] <- "k.Mortality"

print("Calculating Fertility Index:")
lcFert <- leeCarter(data=asfr[,c("Country", "Year", "Age", "ASFR")],type="fertility",series="ASFR",max.age = 50)
names(lcFert)[3] <- "k.Fertility"
ltRates<-read.csv("../data_17_07/oecd_lt_yields.csv",header=T,sep=",",quote="\"",na.strings=c("..","n.a.","-"))


#countries<-c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Greece","Germany", "Iceland","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal","Spain","Sweden","Switzerland","United Kingdom","United States","Germany", "Turkey")



# Historical Demographic Data
demoData<-processHistoric()

econData<-processEcon()
inflationData<-processInflation()
#cb_rates<-processCBRates()
labourData <- processLabour()
demoForecasts <- processForecasts()
oilPrices <- processOil()




data("pwt9.0")
pwt <- pwt9.0
pwt <- pwt[,c(1,3,5,7)]
names(pwt)[1]<-"Country"
names(pwt)[2]<-"Year"
levels(pwt$Country)[173] <- "United States"
pwt$RGDPL<-pwt$rgdpe/pwt$pop

pwt$RGDPlevel<-pwt$rgdpe

econData$g<-growthRate(econData, "RGDP")

#allData2<-merge(demoData,econData)
#allData2<-merge(allData2,labourData)
#allData2<-merge(allData2,pwt[,c("Country", "Year", "RGDPL", "RGDPlevel")])
#write.table(allData2, "../data_17_07/allDataH2.csv", sep="\t") 

## Construct three groups
demoData$youth<-demoData$p1 + demoData$p2
demoData$working<-demoData$p3 + demoData$p4+demoData$p5+demoData$p6
demoData$oap<-1-demoData$youth-demoData$working
demoData$beta_1<-demoData$youth-demoData$oap
demoData$beta_2<-demoData$working-demoData$oap

#contsruct alternative age groups young (0-20) young workers (20-34) experienced workers (35-54) and older workers (55-64) old age (65+) 

##construct the  five age groups
demoData$altyouth0019<-(demoData$X0.4+demoData$X5.9 +demoData$X10.14+demoData$X15.19)/demoData$Total
demoData$altworking2034<-(demoData$X20.24+demoData$X25.29+demoData$X30.34)/demoData$Total
demoData$altworking3554<-(demoData$X35.39+demoData$X40.44+demoData$X45.49+demoData$X50.54)/demoData$Total
demoData$altworking5564<-(demoData$X55.59+demoData$X60.64)/demoData$Total
demoData$altoap65<-1-demoData$altyouth0019-demoData$altworking2034-demoData$altworking3554-demoData$altworking5564


demoData$gamma_1<-demoData$altyouth0019-demoData$altoap65
demoData$gamma_2<-demoData$altworking2034-demoData$altoap65
demoData$gamma_3<-demoData$altworking3554-demoData$altoap65
demoData$gamma_4<-demoData$altworking5564-demoData$altoap65


#contsruct four age groups: alternative age groups young (0-20) young workers (20-35) older workers (35-65)and old age (65+) 

#demoData$X80<-demoData$X80.+demoData$X80.84+demoData$X85.89+demoData$X90.94+demoData$X95.99+demoData$X100.

#demoData$altyouth0019<-(demoData$X0.4+demoData$X5.9 +demoData$X10.14+demoData$X15.19)/demoData$Total
#demoData$altworking2034<-(demoData$X20.24+demoData$X25.29+demoData$X30.34)/demoData$Total
#demoData$altworking3564<-(demoData$X35.39+demoData$X40.44+demoData$X45.49+demoData$X50.54+demoData$X55.59+demoData$X60.64)/demoData$Total
#demoData$altoap65<-1-demoData$altyouth0019-demoData$altworking2034-demoData$altworking3564


#demoData$delta_1<-demoData$altyouth0019-demoData$altoap65
#demoData$delta_2<-demoData$altworking2034-demoData$altoap65
#demoData$delta_3<-demoData$altworking3564-demoData$altoap65

#contsruct four age groups VERSIOn 2: alternative age groups young (0-20) young workers (20-40) older worker 40-60 and 60+ 



#demoData$altyouth0019<-(demoData$X0.4+demoData$X5.9 +demoData$X10.14+demoData$X15.19)/demoData$Total
#demoData$altworking2039<-(demoData$X20.24+demoData$X25.29+demoData$X30.34+demoData$X35.39)/demoData$Total
#demoData$altworking4059<-(demoData$X40.44+demoData$X45.49+demoData$X50.54+demoData$X55.59)/demoData$Total
#demoData$altoap60<-1-demoData$altyouth0019-demoData$altworking2039-demoData$altworking4059


#demoData$delta2_1<-demoData$altyouth0019-demoData$altoap60
#demoData$delta2_2<-demoData$altworking2039-demoData$altoap60
#demoData$delta2_3<-demoData$altworking4059-demoData$altoap60

#contsruct four age groups VERSIOn 3: alternative age groups young (0-20) young workers (20-45) older worker 45-65 and 65+ 
#labelled as alt_2



#demoData$altyouth0019<-(demoData$X0.4+demoData$X5.9 +demoData$X10.14+demoData$X15.19)/demoData$Total
#demoData$altworking2044<-(demoData$X20.24+demoData$X25.29+demoData$X30.34+demoData$X35.39+demoData$X40.44)/demoData$Total
#demoData$altworking4564<-(demoData$X45.49+demoData$X50.54+demoData$X55.59++demoData$X60.64)/demoData$Total
#demoData$altoap65<-1-demoData$altyouth0019-demoData$altworking2044-demoData$altworking4564


#demoData$delta3_1<-demoData$altyouth0019-demoData$altoap65
#demoData$delta3_2<-demoData$altworking2044-demoData$altoap65
#demoData$delta3_3<-demoData$altworking4564-demoData$altoap65




##construct the  five age groups

demoData$altyouth0019<-(demoData$X0.4+demoData$X5.9 +demoData$X10.14+demoData$X15.19)/demoData$Total
demoData$altworking2034<-(demoData$X20.24+demoData$X25.29+demoData$X30.34)/demoData$Total
demoData$altworking3554<-(demoData$X35.39+demoData$X40.44+demoData$X45.49+demoData$X50.54)/demoData$Total
demoData$altworking5564<-(demoData$X55.59+demoData$X60.64)/demoData$Total
demoData$altoap65<-1-demoData$altyouth0019-demoData$altworking2034-demoData$altworking3554-demoData$altworking5564


demoData$gamma_1<-demoData$altyouth0019-demoData$altoap65
demoData$gamma_2<-demoData$altworking2034-demoData$altoap65
demoData$gamma_3<-demoData$altworking3554-demoData$altoap65
demoData$gamma_4<-demoData$altworking5564-demoData$altoap65


##construct the  five age groups (Version 2): 019, 20-34, 35-49, 50-59 and 60+

demoData$altyouth0019<-(demoData$X0.4+demoData$X5.9 +demoData$X10.14+demoData$X15.19)/demoData$Total
demoData$altworking2034<-(demoData$X20.24+demoData$X25.29+demoData$X30.34)/demoData$Total
demoData$altworking3549<-(demoData$X35.39+demoData$X40.44+demoData$X45.49)/demoData$Total
demoData$altworking5059<-(demoData$X50.54+demoData$X55.59)/demoData$Total
demoData$altoap60<-1-demoData$altyouth0019-demoData$altworking2034-demoData$altworking3549-demoData$altworking5059


demoData$gamma2_1<-demoData$altyouth0019-demoData$altoap60
demoData$gamma2_2<-demoData$altworking2034-demoData$altoap60
demoData$gamma2_3<-demoData$altworking3549-demoData$altoap60
demoData$gamma2_4<-demoData$altworking5059-demoData$altoap60


##construct the six age groups 019 20-29 30-39 40-49 50-59 60+ 

demoData$altyouth0019<-(demoData$X0.4+demoData$X5.9 +demoData$X10.14+demoData$X15.19)/demoData$Total
demoData$altworking2029<-(demoData$X20.24+demoData$X25.29)/demoData$Total
demoData$altworking3039<-(demoData$X30.34+demoData$X35.39)/demoData$Total
demoData$altworking4049<-(demoData$X40.44+demoData$X45.49)/demoData$Total
demoData$altworking5059<-(demoData$X50.54+demoData$X55.59)/demoData$Total
demoData$altoap60<-1-demoData$altyouth0019-demoData$altworking2029-demoData$altworking3039-demoData$altworking4049-demoData$altworking5059


demoData$iota_1<-demoData$altyouth0019-demoData$altoap60
demoData$iota_2<-demoData$altworking2029-demoData$altoap60
demoData$iota_3<-demoData$altworking3039-demoData$altoap60
demoData$iota_4<-demoData$altworking4049-demoData$altoap60
demoData$iota_5<-demoData$altworking5059-demoData$altoap60


##construct the  six age groups (Version 2): 014, 15-29, 30-44, 45-59 60-74 75+

demoData$altyouth0014<-(demoData$X0.4+demoData$X5.9 +demoData$X10.14)/demoData$Total
demoData$altworking1529<-(demoData$X15.19+demoData$X20.24+demoData$X25.29)/demoData$Total
demoData$altworking3044<-(demoData$X30.34+demoData$X35.39+demoData$X40.44)/demoData$Total
demoData$altworking4559<-(demoData$X45.49+demoData$X50.54+demoData$X55.59)/demoData$Total
demoData$altworking6074<-(demoData$X60.64+demoData$X65.69++demoData$X70.74)/demoData$Total
demoData$altoap75<-1-demoData$altyouth0014-demoData$altworking1529-demoData$altworking3044-demoData$altworking4559-demoData$altworking6074


demoData$iota2_1<-demoData$altyouth0014-demoData$altoap75
demoData$iota2_2<-demoData$altworking1529-demoData$altoap75
demoData$iota2_3<-demoData$altworking3044-demoData$altoap75
demoData$iota2_4<-demoData$altworking4559-demoData$altoap75
demoData$iota2_5<-demoData$altworking6074-demoData$altoap75






# FORECASTS
## Construct three groups
#demoForecasts$youth<-demoForecasts$p1 + demoForecasts$p2
#demoForecasts$working<-demoForecasts$p3 + demoForecasts$p4+demoForecasts$p5+demoForecasts$p6
#demoForecasts$oap<-1-demoForecasts$youth-demoForecasts$working
#demoForecasts$beta_1<-demoForecasts$youth-demoForecasts$oap
#demoForecasts$beta_2<-demoForecasts$working-demoForecasts$oap

#construct four age groups

#demoForecasts$altyouth0019<-(demoForecasts$X0.4+demoForecasts$X5.9 +demoForecasts$X10.14+demoForecasts$X15.19)/demoForecasts$Total
#demoForecasts$altworking2034<-(demoForecasts$X20.24+demoForecasts$X25.29+demoForecasts$X30.34)/demoForecasts$Total
#demoForecasts$altworking3564<-(demoForecasts$X35.39+demoForecasts$X40.44+demoForecasts$X45.49+demoForecasts$X50.54+demoForecasts$X55.59+demoForecasts$X60.64)/demoForecasts$Total
#demoForecasts$altoap65<-1-demoForecasts$altyouth0019-demoForecasts$altworking2034-demoForecasts$altworking3564


#demoForecasts$delta_1<-demoForecasts$altyouth0019-demoForecasts$altoap65
#demoForecasts$delta_2<-demoForecasts$altworking2034-demoForecasts$altoap65
#demoForecasts$delta_3<-demoForecasts$altworking3564-demoForecasts$altoap65


#contsruct four age groups VERSIOn 2: alternative age groups young (0-20) young workers (20-40) older worker 40-60 and 60+ 
#labelled as alt_1



#demoForecasts$altyouth0019<-(demoForecasts$X0.4+demoForecasts$X5.9 +demoForecasts$X10.14+demoForecasts$X15.19)/demoForecasts$Total
#demoForecasts$altworking2039<-(demoForecasts$X20.24+demoForecasts$X25.29+demoForecasts$X30.34+demoForecasts$X35.39)/demoForecasts$Total
#demoForecasts$altworking4059<-(demoForecasts$X40.44+demoForecasts$X45.49+demoForecasts$X50.54+demoForecasts$X55.59)/demoForecasts$Total
#demoForecasts$altoap60<-1-demoForecasts$altyouth0019-demoForecasts$altworking2039-demoForecasts$altworking4059


#demoForecasts$delta2_1<-demoForecasts$altyouth0019-demoForecasts$altoap60
#demoForecasts$delta2_2<-demoForecasts$altworking2039-demoForecasts$altoap60
#demoForecasts$delta2_3<-demoForecasts$altworking4059-demoForecasts$altoap60


#contsruct four age groups VERSIOn 3: alternative age groups young (0-20) young workers (20-45) older worker 45-65 and 65+ 
#labelled as alt_2



#demoForecasts$altyouth0019<-(demoForecasts$X0.4+demoForecasts$X5.9 +demoForecasts$X10.14+demoForecasts$X15.19)/demoForecasts$Total
#demoForecasts$altworking2044<-(demoForecasts$X20.24+demoForecasts$X25.29+demoForecasts$X30.34+demoForecasts$X35.39+demoForecasts$X40.44)/demoForecasts$Total
#demoForecasts$altworking4564<-(demoForecasts$X45.49+demoForecasts$X50.54+demoForecasts$X55.59++demoForecasts$X60.64)/demoForecasts$Total
#demoForecasts$altoap65<-1-demoForecasts$altyouth0019-demoForecasts$altworking2044-demoForecasts$altworking4564


#demoForecasts$delta3_1<-demoForecasts$altyouth0019-demoForecasts$altoap65
#demoForecasts$delta3_2<-demoForecasts$altworking2044-demoForecasts$altoap65
#demoForecasts$delta3_3<-demoForecasts$altworking4564-demoForecasts$altoap65





##construct the  five age groups
#demoForecasts$altyouth0019<-(demoForecasts$X0.4+demoForecasts$X5.9 +demoForecasts$X10.14+demoForecasts$X15.19)/demoForecasts$Total
#demoForecasts$altworking2034<-(demoForecasts$X20.24+demoForecasts$X25.29+demoForecasts$X30.34)/demoForecasts$Total
#demoForecasts$altworking3554<-(demoForecasts$X35.39+demoForecasts$X40.44+demoForecasts$X45.49+demoForecasts$X50.54)/demoForecasts$Total
#demoForecasts$altworking5564<-(demoForecasts$X55.59+demoForecasts$X60.64)/demoForecasts$Total
#demoForecasts$altoap65<-1-demoForecasts$altyouth0019-demoForecasts$altworking2034-demoForecasts$altworking3554-demoForecasts$altworking5564


#demoForecasts$gamma_1<-demoForecasts$altyouth0019-demoForecasts$altoap65
#demoForecasts$gamma_2<-demoForecasts$altworking2034-demoForecasts$altoap65
#demoForecasts$gamma_3<-demoForecasts$altworking3554-demoForecasts$altoap65
#demoForecasts$gamma_4<-demoForecasts$altworking5564-demoForecasts$altoap65


##construct the  five age groups (Version 2): 019, 20-34, 35-49, 50-59 and 60+
#labelled as alt_1


#demoForecasts$altyouth0019<-(demoForecasts$X0.4+demoForecasts$X5.9 +demoForecasts$X10.14+demoForecasts$X15.19)/demoForecasts$Total
#demoForecasts$altworking2034<-(demoForecasts$X20.24+demoForecasts$X25.29+demoForecasts$X30.34)/demoForecasts$Total
#demoForecasts$altworking3549<-(demoForecasts$X35.39+demoForecasts$X40.44+demoForecasts$X45.49)/demoForecasts$Total
#demoForecasts$altworking5059<-(demoForecasts$X50.54+demoForecasts$X55.59)/demoForecasts$Total
#demoForecasts$altoap60<-1-demoForecasts$altyouth0019-demoForecasts$altworking2034-demoForecasts$altworking3549-demoForecasts$altworking5059


#demoForecasts$gamma2_1<-demoForecasts$altyouth0019-demoForecasts$altoap60
#demoForecasts$gamma2_2<-demoForecasts$altworking2034-demoForecasts$altoap60
#demoForecasts$gamma2_3<-demoForecasts$altworking3549-demoForecasts$altoap60
#demoForecasts$gamma2_4<-demoForecasts$altworking5059-demoForecasts$altoap60


allData<-merge(demoData,econData)

allData<-merge(allData,CB.Rate)
allData<-merge(allData,LR.Rate)

allData<-merge(allData,inflationData)
#allData<-merge(allData,nfa)
#allData<-merge(allData,nfa_gdp)


allData<-merge(allData,oilPrices,all.x=T)

allData<-merge(allData,PATAPPRESID)


allData<-merge(allData,ltRates,all.x = T)
allData<-merge(allData,lcData,all.x = T)
allData<-merge(allData,lcFert,all.x = T)
allData<-merge(allData,pwt[,c("Country", "Year", "RGDPL", "RGDPlevel")])



allData$gRGDPL<-growthRate(allData,"RGDPL")

# Don't merge in RGDP from the labour data because it causes merge problems
# and we have it already anyway
allData<-merge(allData,labourData[,c("Country", "Year", "Hours")])
allData$h<-allData$Hours*1e6/allData$Total

# Calculate the real rate 
allData$realInterest<-allData$CB.Rate/100-allData$infl
allData$CB.Rate<-allData$CB.Rate/100

#Calculate the long real rate
#allData$longrealInterest<-allData$LR.Rate/100-allData$infl

#allData$LR.Rate<-allData$LR.Rate/100


countries<-c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Greece","Germany", "Iceland","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal","Spain","Sweden","Switzerland","United Kingdom","United States","Germany", "Turkey")

allData <- allData[allData$Country %in% countries,]

write.table(allData, "../data_17_07/allDataH.txt", sep="\t") 

write.table(demoForecasts, "../data_17_07/DemoForecast.txt", sep="\t") 



