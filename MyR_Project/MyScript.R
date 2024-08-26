x=c()
y=c()
z=c()
for (i in 1:1000000){
  x=c(x,(log(100,10*i)))
  y=c(y,prod(x))
  z=c(z,sum(x))
}
plot(x,type = 'l',col='red')
lines(y,type = 'l',col='blue')
plot(y,type = 'l')
plot(z,type = 'l')

choose(10,4)

curve(dnorm(x), -10,10)
x=-10:10
plot(x,x^2,pch=2,cex=3,col='red',main='ref',sub='dfs',xlab='dfsds',ylab='qwesd',las=3)
points(x,x^2, pch=11,cex=1,col='papayawhip')

png('test.png',480,480)
par(mar=c(2,2,0,0))
curve(x^2,type="l")
dev.off()

curve(x^2,type='b',pch=1,cex=1,col='blue')
arrows(0,0,0.8,0.8,length=.1)
abline(0.6,1)


abline(h=0.6)
abline(v=0.6)
text(.2,.4,"kmd",pos=4)
curve(100*dnorm(x,0,1),pch = 7,add=TRUE, lwd=2,lty=2)

library(ggplot2)

head(mpg)
ggplot()+geom_point(data=mpg, mapping=aes(x=displ,y=hwy,color=class,shape=class)) +
  geom_smooth(data=mpg, mapping=aes(x=displ,y=hwy,color=class),se=F) +
  scale_shape_manual(values=1:7)
  scale_color_grey() 
  #scale_shape_manual(values=1:7)


library(WDI)
WDIsearch("life exp")

wdi_raw <- WDI(indicator=c("SP.DYN.LE00.FE.IN"),start = 1960,end=2020)
ggplot(wdi_raw)+geom_point(aes(x=country,y=SP.DYN.LE00.FE.IN ))

library(dplyr)
ourdata<- filter(wdi_raw, iso2c=="US")
ourdata<-rename(ourdata, LE_fem=SP.DYN.LE00.FE.IN)
head(ourdata)
ourdata<-select(ourdata, year,LE_fem)
plot(ourdata$year,ourdata$LE_fem)

ourdata<-arrange(ourdata,year)
ourdata<-subset(ourdata,!is.na(LE_fem))
ggplot(data=ourdata,mapping=aes(x=year,y=LE_fem)) + geom_smooth() +
  geom_point()
ggsave("my_testt.png",width = 7,height = 5)
summary(ourdata)
dev.off()

data("iris")
iris$Sepal.Length[5:10] <- NA # create some NAs for this example

ggplot(data=subset(iris, !is.na(Sepal.Length)), aes(x=Sepal.Length)) + 
  geom_bar()

le_data<- WDI(indicator = c("SP.DYN.LE00.FE.IN"),start = 1960,end=2019) %>% rename(LE = SP.DYN.LE00.FE.IN)
ctryinfo<-as.data.frame(WDI_data$country,stringsAsFactors = FALSE) %>% select(country,income)
alldata<- left_join(le_data, ctryinfo)



data(affairs, package='wooldridge')
haskids<-factor(affairs$kids, labels=c('no','yes'))
table(haskids)
prop.table(table(haskids))
table(haskids,affairs$ratemarr)
marriage=factor(affairs$ratemarr,labels=c('very unhappy', 'unhappy','average','happy','very happy'))

dpie(table(haskids))
par(mar=c(5,6,4,2))
barplot(prop.table(table(haskids,marriage)),horiz=T,las=1,main='Distribution of Happiness',sub = "this is my work")

data(ceosal1,package='wooldridge')
attach(ceosal1)
ROE<-ceosal1$roe
hist(ROE)
hist(ROE,breaks=c(0,5,10,20,30,60))
plot(density(ROE))
hist(ROE,freq=F,ylim=c(0,0.07))
lines(density(ROE),lwd=3,col='red')
plot(ecdf(ROE),col='blue')
quantile(ROE,.95)
cor(ceosal1$salary,ROE)
plot(ceosal1$salary,ROE)
boxplot(ROE~ceosal1$consprod)
dbinom(0,10,.2)
pnorm(10,10,3)

curve(dnorm(x,10,3),0,30)

choose(10,2)

qnorm(pnorm(12,10,2),10,2)

plot(density(rnorm(100)),ylim=c(0,0.5))
lines(density(rnorm(1000)))
lines(density(rnorm(10000)),col='red')
lines(density(rnorm(10000000)),col='blue',ylim=c(0,0.5),lwd=2)

set.seed(2343)
rnorm(10)
set.seed(2343)
rnorm(10)

ybar<-mean(ROE)
n<-length(ROE)
s<-sd(ROE)
se<-s/sqrt(n)
plot(density(rt(10000,209)))
c<-qt(.025,n-1)
CI<-c(ybar - c*se,ybar+c*se)

t.test(ROE, mu=15, alternative="greater", conf.level=0.95)
t.test(ROE, mu=17)

data(ceosal1,package='wooldridge')
attach(ceosal1)

cov(roe,salary)
detach(ceosal1)
lm(salary~roe, ceosal1)
summary(lm(salary~roe, ceosal1))
with(ceosal1, plot(roe, salary, ylim=c(0,4000)))
abline(lm(salary~roe, ceosal1),col='red',type='l',lwd=3)
mean(salary)
lm(salary~1)
abline(lm(salary~roe),col='blue')
abline(lm(salary~0+roe))
abline(lm(salary~1),col='brown')
summary(lm(salary~0+roe))
abline(lm(salary~1),col='brown')
mean(resid(lm(salary~0+roe)))
mean(resid(lm(salary~roe)))
confint(lm(salary~roe),level=.99)

data(mlb1, package='wooldridge')
res<-lm(log(salary)~ years + gamesyr+bavg+hrunsyr+rbisyr,data=mlb1)
summary(res)
library(car)
linearHypothesis(res,c("bavg=0","hrunsyr=2*rbisyr"))
matchCoefs(res,"yr")
library(effects)

data(hprice2,package='wooldridge')
res<-lm( log(price) ~ log(nox)+log(dist)+rooms+I(rooms^2)+stratio, data = hprice2)
summary(res)

plot(effect("rooms",res))
library(lmtest)
data(gpa3, package='wooldridge')
reg<-lm(cumgpa~sat+hsperc+tothrs+female+black+white, data=gpa3, subset=(spring==1))
coeftest(reg)
coeftest(reg,vcov=hccm)
myH0<-c("black","white")
linearHypothesis(reg,myH0)
linearHypothesis(reg,myH0,vcov=hccm)

install.packages(c('AER','car','censReg','dplyr','dummies','dynlm',
                   'effects','ggplot2','lmtest','maps','mfx',
                   'orcutt','plm','quantmod','sandwich','quantreg','rio','rmarkdown',
                   'sampleSelection','stargazer','survival','systemfit',
                   'truncreg','tseries','urca','xtable','vars','WDI',
                   'wooldridge','xts','zoo'))

x<-(c('AER','car','censReg','dplyr','dummies','dynlm',
                   'effects','ggplot2','lmtest','maps','mfx',
                   'orcutt','plm','quantmod','sandwich','quantreg','rio','rmarkdown',
                   'sampleSelection','stargazer','survival','systemfit',
                   'truncreg','tseries','urca','xtable','vars','WDI',
                   'wooldridge','xts','zoo'))

lapply(x, require, character.only = TRUE)


compl<-complete.cases(hprice2)
table(compl)

library(quantreg)
data(rdchem, package = 'wooldridge')
ols<-lm(rdintens~ I(sales/1000)+profmarg, data = rdchem)
lad<-rq(rdintens~I(sales/1000)+profmarg,data=rdchem)
summary(lad)
library(stargazer)

stargazer(ols,lad,type="text")

library(dynlm)
library(quantmod,zoo,xts)
getSymbols("BA",auto.assign = T)
getSymbols("005930.KS",auto.assign = T)
Samsung<- `005930.KS`
head(Samsung)
plot(Samsung$`005930.KS.Adjusted`, las=2)
head(BA)
plot(BA$BA.Volume,las=2)

library(plm)
data(crime2, package='wooldridge')
crime2.p<-pdata.frame(crime2,index=46)
pdim(crime2.p)


#################################
library(readxl)
USMacroSWQ <- read_xlsx("./us_macro_quarterly.xlsx",
                        sheet = 1,
                        col_types = c("text", rep("numeric", 9)))

# set the column names
colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI", "GS10", 
                          "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")

# format the date column
USMacroSWQ$Date <- as.yearqtr(USMacroSWQ$Date, format = "%Y :0 %q")

# define GDP as ts object
GDP <- ts(USMacroSWQ$GDPC96,
          start = c(1957, 1), 
          end = c(2013, 4), 
          frequency = 4)

# define GDP growth as a ts object
GDPGrowth <- ts(400*log(GDP[-1]/GDP[-length(GDP)]),
                start = c(1957, 2), 
                end = c(2013, 4), 
                frequency = 4)

# 3-months Treasury bill interest rate as a 'ts' object
TB3MS <- ts(USMacroSWQ$TB3MS,
            start = c(1957, 1), 
            end = c(2013, 4), 
            frequency = 4)

# 10-years Treasury bonds interest rate as a 'ts' object
TB10YS <- ts(USMacroSWQ$GS10, 
             start = c(1957, 1), 
             end = c(2013, 4), 
             frequency = 4)

# generate the term spread series
TSpread <- TB10YS - TB3MS

#We estimate both equations separately by OLS and use coeftest() 
#to obtain robust standard errors.

# Estimate both equations using 'dynlm()'
VAR_EQ1 <- dynlm(GDPGrowth ~ L(GDPGrowth, 1:2) + L(TSpread, 1:2), 
                 start = c(1981, 1), 
                 end = c(2012, 4))

VAR_EQ2 <- dynlm(TSpread ~ L(GDPGrowth, 1:2) + L(TSpread, 1:2),
                 start = c(1981, 1),
                 end = c(2012, 4))

# rename regressors for better readability
names(VAR_EQ1$coefficients) <- c("Intercept","Growth_t-1", 
                                 "Growth_t-2", "TSpread_t-1", "TSpread_t-2")
names(VAR_EQ2$coefficients) <- names(VAR_EQ1$coefficients)

library(sandwich)
# robust coefficient summaries
coeftest(VAR_EQ1, vcov. = sandwich)
coeftest(VAR_EQ2, vcov. = sandwich)

# set up data for estimation using `VAR()`
VAR_data <- window(ts.union(GDPGrowth, TSpread), start = c(1980, 3), end = c(2012, 4))

library(vars)
# estimate model coefficients using `VAR()`
VAR_est <- VAR(y = VAR_data, p = 2)
VAR_est

summary(VAR_est)

# obtain the adj. R^2 from the output of 'VAR()'
summary(VAR_est$varresult$GDPGrowth)$adj.r.squared
#> [1] 0.2887223
summary(VAR_est$varresult$TSpread)$adj.r.squared
#> [1] 0.8254311

linearHypothesis(VAR_EQ1, 
                 hypothesis.matrix = c("TSpread_t-1", "TSpread_t-2"),
                 vcov. = sandwich)

linearHypothesis(VAR_EQ2, 
                 hypothesis.matrix = c("Growth_t-1", "Growth_t-2"),
                 vcov. = sandwich)
forecasts <- predict(VAR_est)
forecasts

plot(VAR_est)
plot(forecasts)

# define ts object of the U.S. PCE Price Index
PCECTPI <- ts(log(USMacroSWQ$PCECTPI), 
              start = c(1957, 1), 
              end = c(2012, 4), 
              freq = 4)

# plot logarithm of the PCE Price Index
plot(log(PCECTPI),
     main = "Log of United States PCE Price Index",
     ylab = "Logarithm",
     col = "steelblue", 
     lwd = 2)

#par(mar=c(1,1,1,1))

plot(400 * Delt(PCECTPI),
     main = "United States PCE Price Index",
     ylab = "Percent per annum",
     col = "steelblue", 
     lwd = 2)

# add a dashed line at y =  0 
abline(0, 0, lty = 2)

summary(ur.ers(log(window(GDP, start = c(1962, 1), end = c(2012, 4))),
               model = "trend", 
               lag.max = 2))
summary(ur.ers(Samsung$`005930.KS.Adjusted`, model="trend",lag.max=10))

plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)), 
     plot.type =   "single", 
     lty = c(2, 1),
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     ylim = c(-5, 17),
     main = "Interest Rates")

# add the term spread series
lines(as.zoo(TSpread), 
      col = "steelblue",
      lwd = 2,
      xlab = "Date",
      ylab = "Percent per annum",
      main = "Term Spread")

# shade the term spread
library(plotfunctions)  # for alpha...
polygon(c(time(TB3MS), rev(time(TB3MS))), 
        c(TB10YS, rev(TB3MS)),
        col = alpha("steelblue", f = 0.2),
        border = NA)

# add horizontal line add 0
abline(0, 0)

# add a legend
legend("topright", 
       legend = c("TB3MS", "TB10YS", "Term Spread"),
       col = c("black", "black", "steelblue"),
       lwd = c(2, 2, 2),
       lty = c(2, 1, 1))

# test for nonstationarity of 3-month treasury bills using ADF test
ur.df(window(TB3MS, c(1962, 1), c(2012, 4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")

# test for nonstationarity of 10-years treasury bonds using ADF test
ur.df(window(TB10YS, c(1962, 1), c(2012, 4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")

# test for nonstationarity of 3-month treasury bills using DF-GLS test
ur.ers(window(TB3MS, c(1962, 1), c(2012, 4)),
       model = "constant", 
       lag.max = 6)

# test for nonstationarity of 10-years treasury bonds using DF-GLS test
ur.ers(window(TB10YS, c(1962, 1), c(2012, 4)),
       model = "constant", 
       lag.max = 6)

# import data on the Wilshire 5000 index
W5000 <- read.csv2("./WILL5000INDFC_Max.csv", 
                   stringsAsFactors = F, 
                   header = T, 
                   sep = ",", 
                   na.strings = ".")

class(W5000)

# transform the columns
W5000$DATE <- as.Date(W5000$DATE) #as.Date in zoo package
class(W5000$DATE)
# subset : 1989-29-12 to 2013-12-31
library(tidyverse)
str(W5000$DATE)
DATE_Sub<- window(W5000$DATE, start=c(13348), end=c(13350))
tail(W5000)
# not working
summary(W5000$DATE)
max(W5000$DATE)
W5000$WILL5000INDFC <- as.numeric(W5000$WILL5000INDFC)
summary(W5000)

# remove NAs
W5000 <- na.omit(W5000)
hist(W5000$WILL5000INDFC)
plot(W5000$WILL5000INDFC, type='l',lty=1, lwd=1, col='steelblue')
lines(W5000$WILL5000INDFC, type='l',lty=1, lwd=1, col='red')

# compute daily percentage changes
W5000_PC <- data.frame("Date" = W5000$DATE, 
                       "Value" = as.numeric(Delt(W5000$WILL5000INDFC) * 100))
W5000_PC <- na.omit(W5000_PC)

# plot percentage changes
plot(W5000_PC, 
     ylab = "Percent", 
     main = "Daily Percentage Changes",
     type="l", 
     col = "steelblue", 
     lwd = 0.5)

# add horizontal line at y = 0
abline(0, 0)

# plot sample autocorrelation of daily percentage changes
acf(W5000_PC$Value, main = "Wilshire 5000 Series")

# estimate GARCH(1,1) model of daily percentage changes
library(fGarch)
GARCH_Wilshire <- garchFit(data = W5000_PC$Value, trace = F)
summary(W5000_PC$Value)
summary(GARCH_Wilshire)
plot(GARCH_Wilshire@sigma.t,type='l')

dev_mean_W5000_PC <- W5000_PC$Value - GARCH_Wilshire@fit$coef[1]

# plot deviation of percentage changes from mean
plot(W5000_PC$Date, dev_mean_W5000_PC, 
     type = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "Estimated Bands of +- One Conditional Standard Deviation",
     lwd = 0.2)

abline(0, 0)

# add GARCH(1,1) confidence bands (one standard deviation) to the plot
lines(W5000_PC$Date, 
      GARCH_Wilshire@fit$coef[1] + GARCH_Wilshire@sigma.t, 
      col = "darkred", 
      lwd = 0.5)

lines(W5000_PC$Date, 
      GARCH_Wilshire@fit$coef[1] - GARCH_Wilshire@sigma.t, 
      col = "darkred", 
      lwd = 0.5)

#garch exer
fit = garchFit(~garch(1, 1), data = garchSim())
## formula -
formula(fit)

length(garchSim())
head(garchSim())
head(fGarch::dem2gbp,trace=T)
plot(fGarch::dem2gbp$DEM2GBP,type='l')

WDIsearch("real interest")
ListOfWDI<-WDI_data
head(ListOfWDI$series)
#wdi_raw <- WDI(indicator=c("FR.INR.RINR"),start = 1960,end=2020)
RealInt <- WDI(indicator=c("FR.INR.RINR"))
Rgdpcap<-WDI(indicator = c("GDP per capita, PPP (constant 1987 international $)"))
#RealInt.P<- pdata.frame(RealInt,index=c("country","year"), stringsAsFactors = F)
rm(wdi_raw,ListOfWDI,RealInt.P,Realr)

names(RealInt.P)
RealInt <- rename(RealInt, RealInt = FR.INR.RINR)
#Realr<-RealInt.P %>% filter(country=="Korea, Rep.") %>%  dplyr::select(RealInt)
#plot(as.numeric(Realr$RealInt,is.na),type="l")

List_GDP<-WDIsearch("GDP")
# 1960~2020 current account / GDP
CA_Rate <- WDI(indicator = c("BN.CAB.XOKA.GD.ZS"))
CA_Rate <- rename(CA_Rate, CA_Rate = BN.CAB.XOKA.GD.ZS )
names(CA_Rate)

# 1960~2020 saving / GDP
Sav_Rate <- WDI(indicator = c("NY.GNS.ICTR.ZS"))
Sav_Rate <- rename(Sav_Rate, Sav_Rate = NY.GNS.ICTR.ZS )
names(Sav_Rate)
# 1960~2020 RGDP (current US dollar)
RGDP <- WDI(indicator = c("NY.GDP.MKTP.CD"))
RGDP <- rename(RGDP, RGDP = NY.GDP.MKTP.CD)
names(RGDP)

VAR_data <- merge(RGDP,Sav_Rate)
VAR_data01 <- merge(VAR_data,CA_Rate)
VAR_data02<- merge(VAR_data01,RealInt)

library(countrycode)
tem<-codelist
class(tem)
names(tem)
#library(tidyverse)
tem01<- dplyr::select(tem,iso3n,iso2c,p4.name)
#tem01<- dplyr::select(tem,iso3n)

tem02<- rename(tem01, 'Country code'=iso3n)

VAR_data03<- left_join(VAR_data02,tem02, by='iso2c')
VAR_data03<-VAR_data03[,-9]
library(readxl)
library(xlsx)
World_Pop_2019 <- read_xlsx("World2019_POP_history_BY_AGE.xlsx",skip = 16)
World_Pop_2019_01<- rename(World_Pop_2019,"year"= "Reference date (as of 1 July)")
World_Pop_2019_01<- rename(World_Pop_2019_01,"country"= "Region, subregion, country or area *")

World_Pop_2019_02<-World_Pop_2019_01[,-c(1,2,3,4,6,7)]
VAR_data04<-right_join(VAR_data03,World_Pop_2019_02,by=c("Country code","year"))

#rm(VAR_data06)
temVAR<-VAR_data04[,10:30]
class(temVAR)
temVAR02<-as.double(temVAR[,1])
for (i in 1:21){
temVAR01<-as.double(temVAR[,i])
temVAR02<-cbind(temVAR02,temVAR01)
}

temVAR03<-temVAR02[,-1]
dim(temVAR)
summary(temVAR03)
names(temVAR03)<-names(temVAR)
class(names(temVAR))

for (i in 1:21){
  rename(temVAR03,names(temVAR)[i]=names(temVAR03)[i])  
}
rename(temVAR03)[1]="dd"
pop<- sum(VAR_data04[,]
write.csv(VAR_data04,'VAR_data04.csv')
tem03<-numeric(length(VAR_data04[,1]))
 for (i in 1:length(VAR_data04[,1])){
   tem03[i]<- VAR_data04[i,1]==VAR_data04[i,9]
 }
 class(tem03)
 table(tem03)
VAR_data05<-cbind(VAR_data04,tem03)
tem04<-VAR_data05 %>% filter(tem03==0)

library('tidyverse')
library('readxl')
PanelData = read_xls('Data_DtaToXlsx.xls')
summary(PanelData)
Panelg7 = PanelData %>% filter(g7==1)
PanelCountryNames = Panelg7 %>% select(country,countrycode)

###########################
library(haven)
Final05<-read_dta('DATA_FINAL05.dta')
CaDEPY<-Final05 %>% filter(countrycode==124) %>% select(DEPY)
FrDEPY<-Final05 %>% filter(countrycode==250) %>% select(DEPY)
GerDEPY<-Final05 %>% filter(countrycode==276) %>% select(DEPY)
ItDEPY<-Final05 %>% filter(countrycode==380) %>% select(DEPY)
JpDEPY<-Final05 %>% filter(countrycode==392) %>% select(DEPY)
UkDEPY<-Final05 %>% filter(countrycode==826) %>% select(DEPY)
UsDEPY<-Final05 %>% filter(countrycode==840) %>% select(DEPY)

CaDEPO<-Final05 %>% filter(countrycode==124) %>% select(DEPO)
FrDEPO<-Final05 %>% filter(countrycode==250) %>% select(DEPO)
GerDEPO<-Final05 %>% filter(countrycode==276) %>% select(DEPO)
ItDEPO<-Final05 %>% filter(countrycode==380) %>% select(DEPO)
JpDEPO<-Final05 %>% filter(countrycode==392) %>% select(DEPO)
UkDEPO<-Final05 %>% filter(countrycode==826) %>% select(DEPO)
UsDEPO<-Final05 %>% filter(countrycode==840) %>% select(DEPO)

T=c(1947:2020)
plot(T,CaDEPY$DEPY,type='l',col=1, ylim=c(0,1))
lines(T,CaDEPO$DEPO,type='l',col=1)
lines(T,FrDEPY$DEPY,type='l',col=2)
lines(T,FrDEPO$DEPO,type='l',col=2)
cor(CaDEPY$DEPY,CaDEPO$DEPO, use = "complete.obs")
cor(FrDEPY$DEPY,FrDEPO$DEPO, use = "complete.obs")
cor(GerDEPY$DEPY,GerDEPO$DEPO, use = "complete.obs")
cor(ItDEPY$DEPY,ItDEPO$DEPO, use = "complete.obs")
cor(JpDEPY$DEPY,JpDEPO$DEPO, use = "complete.obs")
cor(UkDEPY$DEPY,UkDEPO$DEPO, use = "complete.obs")
cor(UsDEPY$DEPY,UsDEPO$DEPO, use = "complete.obs")

table=c()
for (i in 1:22){
  range_s=1+(i-1)*45
  range_e= i*45
  temp = c(range_s,range_e)
  table= rbind(table,temp)
}
obs = c(1:22)
table = cbind(obs,table)
country=c('Australia',
           'Austria',	
           'Belgium',	
           'Canada',	
           'Denmark',	
           'Finland',	
           'France'	,
           'Germany',	
           'Greece'	,
            'Iceland'	,
            'Ireland',
            'Italy'	,
            'Japan'	,
            'Netherlands',	
            'New Zealand',	
            'Norway',
            'Portugal',
            'Spain'	,
            'Sweden',
            'Switzerland',
            'United Kingdom',
            'United States of America')
table=cbind(country,table)
countrycode=c(36,40,56,124,208,246,250,276,300,352,372,
              380,392,528,554,578,620,724,752,756,826,840)
table=cbind(countrycode,table)
EconDat<-read_csv("EconData.csv",col_names=F)
code=c()
for (i in 1:22){
  temp = rep(countrycode[i],45)
  code = c(code,temp)
}
EconDat<-cbind(code,EconDat)
year<-rep(c(1971:2015),22)
EconDat<-cbind(year,EconDat)
names(EconDat)<-c('year','countrycode','x1','x2','x3','AksoyRIR')


NewDat<-left_join(Final05,EconDat)
write.csv(NewDat,'NewDat.csv')

Final06<-read_dta('DATA_FINAL06.dta')

tem<-Final06 %>% filter(countrycode==840)%>%select(RIR)
tem1<-Final06 %>% filter(countrycode==840)%>%select(AksoyRIR)
t<-c(1947:2020)
plot(t,tem$RIR,type='l')
lines(t,tem1$AksoyRIR,type='l',col=2)

#################################################

temp<-read_dta('DATA_FINAL06.dta')
names(temp)
library(WDI)
WDIsearch('interest rate')
WDIsearch("Deposit interest rate (%)")
NomInt <- WDI(indicator=c("FR.INR.TDPT"),start = 1960,end=2020)
#######################

ex_interest <- imf_data(database_id = 'IFS',
                        indicator = c('FILR_PA', 'EREER_IX'),
                        freq = 'A')
ex_interest01 <- imf_data(database_id = 'IFS',
                        indicator = c('FIGB_PA'),
                        freq = 'A')

###################3

m <- seq(-1,2,length=40)
p <- seq(0.3,3,length=40)
post<-matrix(0,40,40)
n<-10
y<-rnorm(n)
for (i in 1:40){
  for (j in 1:40){
    post[i,j]= p[j]^(n/2-1)*exp(-(p[j]/2)*sum((y-m[i])^2))
  }
}
contour(m,p,post)


post<-outer(m,p,function(m,p)
  {p^(n/2-1)*exp(-(p/2)*sum((y-m)^2))})

contour(m,p,post)
persp(m,p,post)

####################
library(R2WinBUGS)
data("schools")
head(schools)

#################

library(readxl)
library(tidyverse)
library(haven)
Fin06<- read_dta('DATA_FINAL06.dta')
Fin07<- subset(Fin06, select=-c(iso2c))
Fin<-read_dta('DATA_FINAL.dta')
Fin_mod<- subset(Fin, select = c(countrycode,iso2c))
tem1<-c()
for (i in 1:165){
  tem<-Fin_mod[1+165*(i-1),]
  tem1<-rbind(tem1,tem)
}
Fin08<-left_join(Fin07,tem1)
library(imfr)
all_iso2c
imf_codelist(database_id = 'ifs')
imf_code<-imf_codes('CL_INDICATOR_IFS')

imf_id<-imf_ids()

year<-as.numeric(testt$year)
testt01<-subset(testt,select=-c(year))
testt02<-cbind(testt01,year)
Fin09<-left_join(Fin08,testt02,by=c('iso2c','year'))

lendingRate<-imf_data('ifs','FILR_PA',start=1947)
depositRate<- imf_data('ifs','FIDR_PA',start=1947)
GovRate<- imf_data('ifs','FIGB_PA',start=1947)
#==> right here!
CPI<-imf_data('ifs','PCPI_IX',start=1947)
year<-as.numeric(CPI$year)
testt01<-subset(CPI,select=-c(year))
testt02<-cbind(testt01,year)
Fin09<-left_join(Fin08,testt02,by=c('iso2c','year'))
year<-as.numeric(GovRate$year)
testt01<-subset(GovRate,select=-c(year))
testt02<-cbind(testt01,year)
Fin10<-left_join(Fin09,testt02,by=c('iso2c','year'))
year<-as.numeric(lendingRate$year)
testt01<-subset(lendingRate,select=-c(year))
testt02<-cbind(testt01,year)
Fin11<-left_join(Fin10,testt02,by=c('iso2c','year'))
year<-as.numeric(depositRate$year)
testt01<-subset(depositRate,select=-c(year))
testt02<-cbind(testt01,year)
Fin12<-left_join(Fin11,testt02,by=c('iso2c','year'))


write_xlsx(Fin12,'DATA_FINAL12.xlsx')

#########################

# % This code estimates a reduced-form VAR by Bayesian methods and computes
# % impulse responses using recursive ordering
# 
# % Woong Yong Park
# % April 9, 2016
# 
# %% housekeeping
# 
# clear;
# clc;
# 
# % time stamp
# timeStart = datestr(now);
# tic;
# 
# % initialize random number generator
# rng shuffle;
# 
# %% setup
# 
# % dates of the sample
# y_all   = 150;                 % number of all observations
# y_vars  = 5;                   % number of variables
# y_start = 1;                   % start date (obs number) (after y_start, first 'pen' obs are used as initial obs)
# y_end   = 150;                 % end date (obs number)
# 
# pen     = 4;                  % lags for endogenous variables
# 
# dates   = (1974:1:2020)'
# % dates (1974 - 2020)
# dates2  = dates((1+pen):end);    % dates excluding the training sample and the initial conditions
# 
# % data file
# datanms = 'DATA_FINAL07 for PBVAR_g7_L4_original_RIR.xlsx';
# 
# % folder and file names
# folder  = 'm_01';
# filenms = 'DATA_FINAL07 for PBVAR_g7_L4_original_RIR_results';
# 
# % variables in the data files
# %1; Growth rate of world oil production (will be accumulated to the level)
# %2; Global real activity (index based on dry cargo shipping rates)
# %3; Real price of oil 
# varnms = {  'DEP'; 'lnRgdp';'SAV';'CUR';'RIR'};   
# shocknms = {'DEP'; 'lnRgdp';'SAV';'CUR';'RIR'};
#         
# % impulse responses
# nstep    = 20;          % IRFs horizon
# 
# % simulations
# nsimul   = 5000;
# 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %% estimation
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# % check the directory
# dir_chk = exist(folder,'dir');
# if (dir_chk==0)
#     disp('The designated folder does not exists... Create it.')
#     mkdir(folder);
# end
# 
# %***** load data
# % First pen rows will be used as initial conditions.

library(readxl)
datt = read_xlsx('DATA_FINAL07 for PBVAR_g7_L4_original_RIR.xlsx');
dat = datt(:,2:104);
%dat = table2array(dat);
if (y_all~=size(dat,1))
    error('Check the nubmer of observations!')
end

if (y_vars~=size(dat(:,1:5),2))
    error('Check the number of variables')
end

%***** numbers

% number of endogenous variables
my   = y_vars;

% number of right-hand-side variables in each equation
% endogenous variables + constant
mk   = my*pen+78;

% number of observations (# of obs - # of lags)
%T    = (y_end - y_start + 1) - pen;
T=150;

%***** data matrices

% y(t)
%Y  = dat((y_start+pen):y_end,:);
Y = dat(:,1:5);

% lags of y(t)
%xdat = [];
%for jdx=1:pen
%    xdat = [xdat dat((y_start+pen-jdx):(y_end-jdx),:)];
%end
% 1's
%X  = [xdat ones(T,1)];
X = dat(:,6:end);

%clear xdat;

%***** OLS(MLE) estimates and residuals

XX     = X'*X;
iXX    = inv(XX);
BOLS   = XX\(X'*Y); % inv(X'*X)*(X'*Y)
UOLS   = Y - X*BOLS;
S      = UOLS'*UOLS;
OMGOLS = S/T;
##########################
library(plm)
library(haven)
library(readxl)
library(xlsx)
data("Produc", package = "plm")
zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
          data = Produc, index = c("state","year"))
summary(zz)
pdim(Produc)
##################
G7_L2_DEP <-read_dta('G7_L2_bal_dep.dta')
names(G7_L2_DEP)
G7_L2_DEP_plreg <-plm(DEP~L1_DEP + L1_lnrgdpCap + L1_SAV
                      + L1_CUR + L1_RIR + L2_DEP
                      + L2_lnrgdpCap + L2_SAV
                      + L2_CUR + L2_RIR , 
                      data=G7_L2_DEP, index = c("countrycode","year"), 
                      effect = "twoways", model="within")

Infix<-fixef(G7_L2_DEP_plreg, effect = "individual")
Tmfix<-fixef(G7_L2_DEP_plreg, effect = "time")
Coef<-c(G7_L2_DEP_plreg$coefficients,Infix,Tmfix)
write

library(WDI)
RnD<-WDI(country="all", indicator=c("GB.XPD.RSDV.GD.ZS"),
    start=1960, end=2022)

ggplot(RnD)+geom_point(aes(x=country,y=GB.XPD.RSDV.GD.ZS ))

library(dplyr)
Kor_RnD<- filter(RnD, iso2c=="KR")
Kor_RnD01<-rename(Kor_RnD, RnD_ratio=GB.XPD.RSDV.GD.ZS)
Kor_RnD02<-select(Kor_RnD01, year,RnD_ratio)
plot(Kor_RnD02$year,Kor_RnD02$RnD_ratio,type='l',col='red')

library(countrycode)
dfd<-countrycode(RnD$iso2c, origin = 'iso2c', destination = 'un')
RnD01<-cbind(RnD,dfd)
write.csv(RnD01,"RnD_world.csv")