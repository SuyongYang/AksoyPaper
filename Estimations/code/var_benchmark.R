# Relevant matrices are defined in the following source file

source('varFunctions_withpi6_rgdp1l_threegroups1l.R')

#Benchmark model with 6 endogenous variables. 3 age groups (1-19, 20-59 and 60+), oil prices and population growth rates are exogenous to the system


# The endogenous variables in the VAR
#endo<-list(y="diff(log(RGDPL),1)",i="Investment.Rate",s="Savings.Rate",h="diff(log(Hours/Total),1)",rr="realInterest",pi="infl")
endo<-list(y="diff(log(RGDPlevel),1)",i="Investment.Rate",s="Savings.Rate",h="log(Hours/Total)",rr="realInterest",pi="infl")




# Exogenous, non-demographic variables
exo<-c("lag(log(Oil.Price))","lag(log(Oil.Price),2)","popGrowth", "lag(popGrowth,1)")


#Robustness test with Lee-Carter measures of age specific fertility and mortality rates: replace above expression with
#exo<-c("lag(log(Oil.Price))","lag(log(Oil.Price),2)", "k.Mortality", "k.Fertility")
#alter the source file varFunctions_withpi6_rgdp1l_threegroups1l.R file accordingly


# Demographic variables
demoVars<-paste("beta",1:2,sep="_")

# Define the long names for formulae so that we can use it for captions etc
longNames<-list(y = "Growth per Capita",i = "Invest", s = "Saving", h = "Hours",rr = "RealInterest", pi="Infl")

lags<-1

###this is the correct estimation with demo variables 
equations<-setUpEquations()




###this is the estiamtion without demo variables as a control
#equations<-setUpNonDemoEquations()


# Restricting the data to not contain Germany (reunification) and  Turkey (unreliable demo data) no complete r&d data for Australia, Italy, 
#allData<-allData[!(allData$Country %in% c("Germany","Turkey")),]

#allData<-allData[!(allData$Country %in% c("Germany","Turkey","Australia","Italy")),]
allData<-allData[!(allData$Country %in% c("Germany","Turkey")),]


#allData<-allData[!(allData$Country %in% c("Turkey"))]

# The index to be used 
idx = c("Country","Year")

allPData <- pdata.frame(allData, index=idx)
# 
extractIndex <- function( x ) {
  fr <- model.frame(pFormula(x), allPData)
  index(fr)
}

completeIndex <- Reduce(function(a,b) merge(a,b),lapply(equations,extractIndex))
completeIndex$Year <- as.numeric(as.character(completeIndex$Year))

# because of lags, the prior data needs to be there too
for(i in 1:nrow(completeIndex)) {
  if(length(completeIndex$Year[completeIndex$Country == completeIndex$Country[i] & completeIndex$Year == (completeIndex$Year[i] - 1)]) == 0) {
    completeIndex <- rbind(completeIndex, list(Country = completeIndex$Country[i], Year = completeIndex$Year[i] - 1 ))
  }
  if(length(completeIndex$Year[completeIndex$Country == completeIndex$Country[i] & completeIndex$Year == (completeIndex$Year[i] - 2)]) == 0) {
    completeIndex <- rbind(completeIndex, list(Country = completeIndex$Country[i], Year = completeIndex$Year[i] - 2 ))
  }
}

allData <- merge(completeIndex, allData)
# The effect to be used
effect="individual"

varModel<-estimateVAR(equations,allData[allData$Year >= 1970 & allData$Year <= 2014,],effect)

pvals<-sapply(varModel,function(x) coeftest(x,vcov.=function(y) vcovHC(y,method="white2",type="HC3"))[,4])  

dir.create("../tex/benchmark_3G_nopat/benchmark_nopat_level",recursive = T, showWarnings = F)

dir.create("../Waldtests/benchmark_3G_nopat/3G_nopat_level",recursive = T, showWarnings = F)


# Construct the correlation matrix
corMatrix<-varCorrelationMatrix(varModel)
varMatrices <- extractVARMatrices(varModel)
covarMatrices<-varCOVARMatrix(varModel)

varRange <- c((1:length(endo)),(length(endo) + length(exo) + 1):ncol(varModel$y$vcov))
exoVarVcovs <- lapply(varModel, function(mod) vcovHC(mod,method="white2", type="HC3")[varRange,varRange])
diagMat <- bdiag(as.list(exoVarVcovs))
write.table(as.matrix(diagMat),file="../tex/benchmark_3G_nopat/benchmark_nopat_level/waldprep.csv",row.names = F,col.names = F,sep = ',')
write.table(as.matrix(diagMat),file="../Waldtests/benchmark_3G_nopat/3G_nopat_level/waldprep.csv",row.names = F,col.names = F,sep = ',')
#printTable(waldtestresults[[name]] ,"../tex/benchmark_3G_nopat/benchmark_nopat_level/waldtestresults.tex")





# Calculate the long-term matrices
longTerm<-longTermMatrices(varMatrices)

# Calculate the yearly impacts of demographics in long and short term
demoImpacts<-t(longTerm$D %*% t(as.matrix(demoData[,colnames(longTerm$D)])))
stDemoImpacts<-t(varMatrices$D %*% t(as.matrix(demoData[,colnames(varMatrices$D)])))

#summary(lm(formula(paste("a_1~",paste("a_",2:ncol(test),sep="",collapse="+"),sep="")),data=test))


  eqns<-names(equations)
eqns[7]<-"pi"
eqns[1:6]<-toupper(eqns[1:6])

# rownames(varMatrices$A1)<-eqns
# colnames(varMatrices$A1)<-paste("$",eqns,"_{t-1}$",sep="")
# print(xtable(varMatrices$A1),sanitize.text.function=function(name){name})
# 
#  endoSums<-varMatrices$A1 + varMatrices$A2
#  rownames(endoSums)<-toupper(names(equations))
#  colnames(endoSums)<-toupper(names(equations))
#  endoSums<-fixRowNames(fixColNames(endoSums))
#  print(xtable(endoSums),sanitize.text.function=function(name){name})
# 
# rownames(varMatrices$A2)<-eqns
# colnames(varMatrices$A2)<-paste("$",eqns,"_{t-2}$",sep="")
# print(xtable(varMatrices$A2),sanitize.text.function=function(name){name})


printTable(varMatrices$A1,"Sum of VAR coefficients $A_1","../tex/benchmark_3G_nopat/benchmark_nopat_level/varCoefs.tex")

#printTable(varMatrices$A1 ,"../tex/benchmark_3G_nopat/benchmark_nopat_level/A1matrix.tex")
#printTable(varMatrices$A2 ,"../tex/benchmark_3G_nopat/benchmark_nopat_level/A2matrix.tex")

write.table(varMatrices$A1, "../tex/benchmark_3G_nopat/benchmark_nopat_level/A1.txt", sep="\t") 
write.table(varMatrices$A1, "../Waldtests/benchmark_3G_nopat/3G_nopat_level/A1.txt", sep="\t")

#write.table(varMatrices$A2, "../tex/benchmark_3G_nopat/benchmark_nopat_level/A2.txt", sep="\t") 



# Prepare Correlation Matrix
printTable(corMatrix,digits=3,"Residual Correlation Matrix","../tex/benchmark_3G_nopat/benchmark_nopat_level/correlationMatrix.tex")

# Prepare Covariance Matrix
printTable(covarMatrices,digits=5,"Covariance Matrix","../tex/benchmark_3G_nopat/benchmark_nopat_level/CovarMatrix.tex")

# Prepare Wald Matrix
#printTable(pvcovHCMatrix,digits=5,"Parameter Covariance Matrix","../tex/benchmark_3G_nopat/benchmark_nopat_level/pvcovHC.tex")


# Prepare s.t. Exogenous Impacts
# print(xtable(varMatrices$B),sanitize.text.function=function(name){name})

# Prepare s.t. Demographic Impacts
varMatrices$D<-cbind(varMatrices$D,-t(t(rowSums(varMatrices$D))))
colnames(varMatrices$D)<-paste("$\\beta_",1:3,"$",sep="")
printTable(varMatrices$D,"Short-Run Demographic Impact","../tex/benchmark_3G_nopat/benchmark_nopat_level/shortRunImpacts.tex")

write.table(varMatrices$D, "../tex/benchmark_3G_nopat/benchmark_nopat_level/shortD.txt", sep="\t") 
write.table(varMatrices$D, "../Waldtests/benchmark_3G_nopat/3G_nopat_level/shortD.txt", sep="\t")


# Prepare l.t. Exogenous Impacts
rownames(longTerm$B)<-eqns
colnames(longTerm$B)<-c("$POIL_{t-1}$","$POIL_{t-2}$","$popGrowth_{t-1}$","$popGrowth_{t-2}$")
# print(xtable(longTerm$B),sanitize.text.function=function(name){name})

write.table(longTerm$B, "../tex/benchmark_3G_nopat/benchmark_nopat_level/B.txt", sep="\t") 



# Prepare l.t. Demographic Impacts
ltD<-longTerm$D
# rownames(ltD)<-toupper(names(equations))
ltD<-cbind(ltD,-t(t(rowSums(ltD))))
colnames(ltD)<-paste("$\\beta_",1:3,"$",sep="")
#ltD <- fixRowNames(fixColNames(ltD))
printTable(ltD,"Long-Run Demographic Impact",file="../tex/benchmark_3G_nopat/benchmark_nopat_level/longRunImpacts.tex")

write.table(ltD, "../tex/benchmark_3G_nopat/benchmark_nopat_level/ltD.txt", sep="\t") 



  


varModel2Ways<-estimateVAR(equations,data=allData,effect="twoways")
varMatrices2Ways<-extractVARMatrices(model=varModel2Ways)

write.table(varMatrices2Ways$A1, "../tex/benchmark_3G_nopat/benchmark_nopat_level/A12ways.txt", sep="\t") 
write.table(varMatrices2Ways$A1, "../Waldtests/benchmark_3G_nopat/3G_nopat_level/A12ways.txt", sep="\t")

varMatrices2Ways$D<-cbind(varMatrices2Ways$D,-t(t(rowSums(varMatrices2Ways$D))))
colnames(varMatrices2Ways$D)<-paste("$\\beta_",1:3,"$",sep="")
printTable(varMatrices2Ways$D,"Short-Run Demographic Impact","../tex/benchmark_3G_nopat/benchmark_nopat_level/shortRunImpacts2ways.tex")

write.table(varMatrices2Ways$D, "../tex/benchmark_3G_nopat/benchmark_nopat_level/shortD2ways.txt", sep="\t") 
write.table(varMatrices2Ways$D, "../Waldtests/benchmark_3G_nopat/3G_nopat_level/shortD2ways.txt", sep="\t")

lt2Ways<-longTermMatrices(varMatrices2Ways)

ltD2<-lt2Ways$D
#ltD <- fixRowNames(fixColNames(ltD))
printTable(ltD2,"Long-Run Demographic Impact (2-way effects)",file="../tex/benchmark_3G_nopat/benchmark_nopat_level/longRun2ways.tex")

varRange <- c((1:length(endo)),(length(endo) + length(exo) + 1):ncol(varModel2Ways$y$vcov))
exoVarVcovs <- lapply(varModel2Ways, function(mod) vcovHC(mod,method="white2", type="HC3")[varRange,varRange])
diagMat <- bdiag(as.list(exoVarVcovs))
write.table(as.matrix(diagMat),file="../tex/benchmark_3G_nopat/benchmark_nopat_level/waldprep2ways.csv",row.names = F,col.names = F,sep = ',')
write.table(as.matrix(diagMat),file="../Waldtests/benchmark_3G_nopat/3G_nopat_level/waldprep2ways.csv",row.names = F,col.names = F,sep = ',')

# pallData<-pdata.frame(allData,index=idx)

#  oilPriceSingle<-with(allData[allData$Country=="Austria",],data.frame(POIL=Oil.Price[c(-1,-2)],LPOIL=Oil.Price[c(-1,-length(Oil.Price))]))
#  oilImpacts<-t(longTerm$B %*% t(oilPriceSingle))*100

forecastData<-rbind(demoData[,c("Country","Year",paste("beta_",1:2,sep=""))],demoForecasts[,c("Country","Year",paste("beta_",1:2,sep=""))])
forecastData<-forecastData[forecastData$Year>=1970,]
forecastData1<-forecastData[forecastData$Year>=2000,]

econForecasts<-t(longTerm$D[,1:2] %*% t(forecastData[,3:4]))

econForecasts1<-t(longTerm$D[,1:2] %*% t(forecastData1[,3:4]))

colnames(econForecasts)<-toupper(names(equations))

printTable(averagePredictions(),"Average Predicted Impact on GDP Growth by Country",
      file="../tex/benchmark_3G_nopat/benchmark_nopat_level/averagePredictions.tex", label="tab:avgpredict")

printTable(demoImpactsByCountry(c(unique(allData$Country),"Germany"),1970),"Diﬀerence in Predicted Impact of Demographic Factors between 1970 and 2007",file="../tex/benchmark_3G_nopat/benchmark_nopat_level/byCountryImpacts.tex")
printTable(demoImpactsByCountry(c(unique(allData$Country),"Germany"),2000),"Diﬀerence in Predicted Impact of Demographic Factors between 2000 and 2007",file="../tex/benchmark_3G_nopat/benchmark_nopat_level/byCountryImpacts2.tex")

print(longTerm$D,digits=3, sanitize.text.function=function(text){text}, file="../tex/benchmark_3G_nopat/benchmark_nopat_level/Dmatrix.txt", sep="\t")


model. ="within"
data. = allData

resultTables()

coefTable<-sapply(varModel,function(x){coef(summary(x))[,1]})
print(fixRowNames(xtable(coefTable,digits=3,align=rep("r", 1+ncol(coefTable)))),sanitize.text.function=function(text){text}, file="../tex/benchmark_3G_nopat/benchmark_nopat_level/pcoef.tex")

stdErrTable<-sapply(varModel,function(x){coef(summary(x))[,2]})
print(fixRowNames(xtable(stdErrTable,digits=3,align=rep("r", 1+ncol(stdErrTable)))),sanitize.text.function=function(text){text}, file="../tex/benchmark_3G_nopat/benchmark_nopat_level/parstdErrs.tex")


coeftestsTable<-lapply(varModel,function(x) coeftest(x,vcov.=function(y) vcovHC(y,"white2", type="HC3")))
print(fixRowNames(xtable(coeftestsTable,digits=3,align=rep("r", 1+ncol(coeftestsTable)))),sanitize.text.function=function(text){text}, file="../tex/benchmark_3G_nopat/benchmark_nopat_level/coeftest.tex")


#outputs all columns for one of the equations.

coeftest(varModel$y,vcov.=function(y) vcovHC(y,"white2", type="HC3"))
coeftest(varModel$i,vcov.=function(y) vcovHC(y,"white2", type="HC3"))
coeftest(varModel$s,vcov.=function(y) vcovHC(y,"white2", type="HC3"))
coeftest(varModel$h,vcov.=function(y) vcovHC(y,"white2", type="HC3"))
coeftest(varModel$rr,vcov.=function(y) vcovHC(y,"white2", type="HC3"))
coeftest(varModel$pi,vcov.=function(y) vcovHC(y,"white2", type="HC3"))
 

#vcov for parameter estimates
vcovHC_y<-vcovHC(varModel$y,method="white2", type="HC3")
write.table(vcovHC_y, "../tex/benchmark_3G_nopat/benchmark_nopat_level/bvcov_y.txt", sep="\t") 

vcovHC_i<-vcovHC(varModel$i,method="white2", type="HC3")
write.table(vcovHC_i, "../tex/benchmark_3G_nopat/benchmark_nopat_level/bvcov_i.txt", sep="\t") 

vcovHC_s<-vcovHC(varModel$s,method="white2", type="HC3")
write.table(vcovHC_s, "../tex/benchmark_3G_nopat/benchmark_nopat_level/bvcov_s.txt", sep="\t") 

vcovHC_h<-vcovHC(varModel$h,method="white2", type="HC3")
write.table(vcovHC_h, "../tex/benchmark_3G_nopat/benchmark_nopat_level/bvcov_h.txt", sep="\t") 

vcovHC_rr<-vcovHC(varModel$rr,method="white2", type="HC3")
write.table(vcovHC_rr, "../tex/benchmark_3G_nopat/benchmark_nopat_level/bvcov_rr.txt", sep="\t") 


vcovHC_pi<-vcovHC(varModel$pi,method="white2", type="HC3")
write.table(vcovHC_pi, "../tex/benchmark_3G_nopat/benchmark_nopat_level/bvcov_pi.txt", sep="\t") 

lapply(names(varModel),function(x) write.csv (vcovHC(varModel[[x]],method="white2", type="HC3"),file=paste(x,"_vcov.csv",sep="")))



