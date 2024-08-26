createLaggedVars<-function(vars,lags)
{
	# Creates a string that looks like:
	#
	# "lag(var[1],lag[1]) + lag(var[2],lag[1]) + ... + lag(var[n],lag[1]) + lag(var[1],lag[2]) + ... + lag(var[n],lag[m])"
	#
	# where n is the number of elements of vector var, and m the number of elements of vector lags
	result<-""
	for(individualLag in lags)
	{
		if(nchar(result)>0)
		{
			result<-paste(result," + ",sep="")
		}

		result<- paste(result,paste("lag(",vars,",",individualLag,")",sep="",collapse="+"),sep="")
	}
	return(result)
}

# Creates the equations that make up the VAR and put them in a list
setUpEquations <- function(endo.=endo,exo.=exo,lags.=lags,demoVars.=demoVars)
{
	equations<-list()
	
	rhsString<-paste(createLaggedVars(endo.,1:lags),paste(exo.,collapse=" + "),paste(demoVars.,collapse=" + "),sep=" + ")

	for(name in names(endo.))
	{
		equations[[name]]<-formula(paste(endo.[[name]],rhsString,sep=" ~ "))
	}

	return(equations)
}

# Removes demographic variables from the equations
setUpNonDemoEquations<-function(equations)
{
	return(sapply(equations, FUN=function(eqn){update(eqn,as.formula(paste(".~.-",paste(demoVars,collapse="-"),sep="")))}))
}

# The function to construct a HAC-consistent covariance matrix from the model
myvcov<-function(aModel)
{
  pvcovHC(aModel,method="white2", type="HC3")
}





# Find the index of the first alpha
findFirstBeta <- function(equation)
{
	match(demoVars[1],attr(terms(equation),"term.labels"))
}

# Gets the list of demographic variables - better than 'paste' because it could deal with differently named vars
# (e.g. p1,p2...)
getBetas <- function(equations)
{
	firstBetaIndex <- findFirstBeta(equations[[1]])
	betas<-attr(terms(equations[[1]]),"term.labels")[firstBetaIndex:(firstBetaIndex+1)]
}

# Run all equations in the system with one- and two-way effects and also test demo significance
runAllBothEffects<-function(equations)
{
#	removeOilPrice<-.~.-lag(log(Oil.Price))-lag(log(Oil.Price),2)-diff(log(Total),1)-lag(diff(log(Total),1))
	removeOilPrice<-.~.-lag(log(Oil.Price))-lag(log(Oil.Price),2)-popGrowth-lag(popGrowth,1)

	for(name in names(equations))
	{
		runBothEffects(longNames[[name]],equations[[name]],getBetas(equations),adjustTwoWays=removeOilPrice)
	}
}

# Estimates all equations in the list provided and on the data passed, using the effect indicated
estimateVAR <- function(equations, data., effect=effect., model.="within")
{
	model<-list()
	#waldtestresults<-list()

	for(name in names(equations))
	{
		model[[name]]<-plm(equations[[name]],data=data.,index=idx,effect=effect,model=model.)
		#waldtestresults[[name]]<-coeftest(model[[name]],vcovHC(model[[name]],method="white2",type="HC3"))
	}
	#waldtestresults<-coeftest(model[[name]],vcovHC(model[[name]],method="white2",type="HC3"))

	return(model)
	#return(waldtestresults)


	#write.table(waldtestresults, file = "../tex/tables/waldtest.txt", append = FALSE, quote = TRUE, sep = " ",
       #     eol = "\n", na = "NA", dec = ".", row.names = TRUE,
       #     col.names = TRUE, qmethod = c("escape", "double"),
       #     fileEncoding = "")
	#write.csv("../tex/tables/waldtest.txt")





}

# Determine the HC var-covar matrix from the estimated VAR
#pvcovHC<-function(model)
#{
#	pvcovHCMatrix<-data.frame()
#
#	
#	for(name in names(model))
#	{
#		pvcovRow<-list()
#		
#		
#		{
#			pvcovRow[[name]]<-coeftest(model[[name]],vcovHC(model[[name]],method="white2",type="HC3"))
#
#		}
#
#		pvcovHCMatrix<-rbind(pvcovHCMatrix,pvcovRow)
#		rownames(pvcovHCMatrix)[nrow(pvcovHCMatrix)]<-name
#	}
#
#	return(pvcovHCMatrix)
#}


# Determine the VAR correlation matrix from the provided estimated VAR
varCorrelationMatrix<-function(model)
{
	corMatrix<-data.frame()

	
	for(name in names(model))
	{
		corRow<-list()
		
		for(other in names(model))
		{
			corRow[[other]]<-cor(model[[name]]$residuals,model[[other]]$residuals)
		}

		corMatrix<-rbind(corMatrix,corRow)
		rownames(corMatrix)[nrow(corMatrix)]<-name
	}

	return(corMatrix)
}

# Determine the VAR residual variance covariance matrix 
varCOVARMatrix<-function(model)
{
	covMatrix<-data.frame()

	
	for(name in names(model))
	{
		covRow<-list()
		
		for(other in names(model))
		{
			covRow[[other]]<-cov(model[[name]]$residuals,model[[other]]$residuals)
		}

		covMatrix<-rbind(covMatrix,covRow)
		rownames(covMatrix)[nrow(covMatrix)]<-name
	}

	return(covMatrix)
}


# The function to construct a HAC-consistent covariance matrix from the model


# Extract the matrices of the VAR from the estimated model
extractVARMatrices <- function(model)
{
	matrices<-list()

	offSet<-as.numeric(attr(model[[1]]$coefficients[1],'name')=="(Intercept)")
	firstBetaIndex<-findFirstBeta(model[[1]]$formula)

	matrices$A1 <- t(as.matrix(sapply(model,function(x){x$coefficients[1:6+offSet]})))
#	matrices$A2 <- t(as.matrix(sapply(model,function(x){x$coefficients[8:14+offSet]})))
	matrices$B  <- t(as.matrix(sapply(model,function(x){x$coefficients[7:(firstBetaIndex-1)+offSet]})))
	matrices$D  <- t(as.matrix(sapply(model,function(x){x$coefficients[firstBetaIndex:(firstBetaIndex+length(demoVars)-1)+offSet]})))

	return(matrices)
}


############################################################################


#dfData <- as.data.frame(
#  read.csv("http://www.stat.tamu.edu/~sheather/book/docs/datasets/MichelinNY.csv",
#                   header=T))

# using direct calculations
#vY <- as.matrix(dfData[, -2])[, 5]                        # dependent variable
#mX <- cbind(constant = 1, as.matrix(dfData[, -2])[, -5])  # design matrix

#vBeta <- solve(t(mX)%*%mX, t(mX)%*%vY)                    # coefficient estimates
#dSigmaSq <- sum((vY - mX%*%vBeta)^2)/(nrow(mX)-ncol(mX))  # estimate of sigma-squared
#mVarCovar <- dSigmaSq*chol2inv(chol(t(mX)%*%mX))          # variance covariance matrix
#vStdErr <- sqrt(diag(mVarCovar))                          # coeff. est. standard errors
#print(cbind(vBeta, vStdErr))         
############################################################################



longTermMatrices<-function(varMatrices)
{
	longTerm<-list()
	longTerm$B<-solve(diag(nrow(varMatrices$A1))-varMatrices$A1) %*% varMatrices$B
	longTerm$D<-solve(diag(nrow(varMatrices$A1))-varMatrices$A1) %*% varMatrices$D

	longTerm
}

################################################################################



#coeftest1<-coeftest(model,vcovHC(model,method="white2",type="HC3"))
#waldtestestimateVAR<-waldtest(estimateVAR,vcov=function(x) vcovHC(x,method="white2",type="HC3"))
################################################################################




# Uses the estimated VAR in model to predict the outcome on data; 'rolling' indicates whether to perform a rolling
# forecast, using the predicted values from the previous period as inputs to the next one (T), or whether to produce
# a series of one-period ahead forecasts (F, the default), which requires that the dataset contain the endogenous variables.
varPredict <- function(model, data, rolling=F)
{
	if(!('pdata.frame' %in% class(data)))
	{
		data<-pdata.frame(data,index=idx)
	}

	# Extract the VAR matrix from the model
	fullMatrix <- t(as.matrix(sapply(model,function(x){x$coefficients})))

	# Extract the relevant columns from the dataset
	forecastData<-model.frame(model[[1]]$formula,data=data)

	if(!rolling)
	{
		# If the forecast is not rolling it's one-period ahead, so easy...
		forecast<-t(fullMatrix %*% t(as.matrix(as.data.frame(forecastData[,2:ncol(forecastData)]))))
		forecast<-data.frame(attr(forecastData,"index"),forecast)

		# Fix the names - only needed in the single-equation case but hey
		names(forecast)[3:ncol(forecast)]<-names(model)	
	}
	else
	{
		dataIndex <- attr(forecastData,"index")
		dataIndex$Year<-as.numeric(as.character(dataIndex$Year))
		forecast<-NULL

		maxYearByCountry <- aggregate(dataIndex$Year,list(Country=dataIndex$Country),FUN=max)
		debugOut(maxYearByCountry)
		for(year in unique(dataIndex$Year))
		{
			forecastForYear<-t(fullMatrix %*% t(as.matrix(forecastData[dataIndex$Year == year,2:ncol(forecastData)])))
			
			# Add the fixed effect for each variable
			for(variable in names(model))
			{
				forecastForYear[,variable] <- forecastForYear[,variable] + fixef(varModel[[variable]],"individual")[dataIndex$Country[dataIndex$Year == year]]
				for(country in dataIndex$Country[dataIndex$Year == year])
				{
					if(year < maxYearByCountry[maxYearByCountry$Country == country,2])
					{
						forecastData[forecastData$Year == (year+1) & forecastData$Country == country,2:(length(model)+1)] <-forecastForYear[dataIndex$Country[dataIndex$Year == year]]
					}
					
					if(lags > 1 & year < maxYearByCountry[maxYearByCountry$Country == country,2]-1)
					{
						forecastData[forecastData$Year == (year+2) & forecastData$Country == country,(length(model)+2):(2*length(model)+1)] <-forecastForYear[dataIndex$Country[dataIndex$Year == year]]
					}
				}
			}
			forecast<-rbind(forecast,forecastForYear)	
		}
		forecast<-data.frame(attr(forecastData,"index"),forecast)
	}
	for(variable in names(model))
	{
		if(!rolling)
		{
			forecast[,variable] <- forecast[,variable] + fixef(model[[variable]],"individual")[forecast$Country]
		}
		
		forecastData<-model.frame(model[[variable]]$formula,data=data)
		forecast[,paste(variable,"actual",sep=".")]<-forecastData[,1]
	}
	
	return(forecast)
}

createForecastResults<-function(model, forecastData, rolling=F)
{
	forecast<-varPredict(model,forecastData, rolling)
	
	results<-NULL
	for(name in names(model))
	{
		predicted<-forecast[,name]
		actual<-forecast[,paste(name,"actual",sep=".")]
		
		cumPredicted<-aggregate(predicted,by=list(Country= forecast$Country),FUN=mean)
		cumActual<-aggregate(actual,by=list(Country= forecast$Country),FUN=mean)
		
		if(!rolling)
		{
			results<-rbind(results,data.frame(RMSE=sqrt(mean((predicted-actual)^2,na.rm=T)),bias=mean(predicted-actual,na.rm=T),cor=cor(predicted,actual,use='c'),cum.cor=cor(cumPredicted[,2],cumActual[,2],use='c')))
		}
		else
		{
			results<-rbind(results,data.frame(RMSE=sqrt(mean((predicted-actual)^2,na.rm=T)),cum.RMSE=sqrt(mean((cumPredicted-cumActual)^2,na.rm=T)),bias=mean(predicted-actual,na.rm=T),cor=cor(predicted,actual,use='c'),cum.cor=cor(cumPredicted[,2],cumActual[,2],use='c')))
		}
		row.names(results)[nrow(results)]<-name
	}
	
	return(results)
}

testForecasting<-function(equations,data, year, rolling=F,forecastData=data)
{
	estimatingData<-data[data$Year<=year,]
	
	# We need three years worth of data to start forecasting due to lags
	forecastData<-forecastData[forecastData$Year>=year-lags,]
	
	model<-estimateVAR(equations, estimatingData, effect)
	
	# Create a version of the equations without demo vars
	nonDemoEqns<-setUpNonDemoEquations(equations)
	
	nonDemoModel<-estimateVAR(nonDemoEqns, estimatingData, effect)
	
	ndForecastResults<-createForecastResults(nonDemoModel,forecastData,rolling)
	forecastResults<-createForecastResults(model,forecastData,rolling)	
	
	# Do a random-walk model forecast also
	varNames<-lapply(equations,function(x){row.names(attr(terms(x),'factors'))[1]})
	rwResults<-as.data.frame(t(as.data.frame(sapply(varNames,function(x){rmseRW(x,allData,year,rolling)}))))
	row.names(rwResults)<-names(equations)
	names(rwResults)[1]<-"RMSE"
	
	# First merge the complex model forecasts
	results<-merge(ndForecastResults,forecastResults,by="row.names", suffixes=c("",".Demo"))
	
	# Fix the row names
	# Why do we have to do this?
	row.names(results)<-results$Row.names
	results$Row.names<-NULL
	
	results<-merge(rwResults,results,,by="row.names",suffixes=c(".RW",""))
	
	# Why do we have to do this?
	row.names(results)<-results$Row.names
	results$Row.names<-NULL
	return(results)
}

createForecastTables <- function()
{
	origData1YForecast<-testForecasting(equations,allData,1997,rolling=F)
  printTable(origData1YForecast[names(equations),],"1-Year-Ahead Forecast, 1997-2007","../tex/tables/fore1Y.tex",
	           label="tab:fore1Y",digits=3)

	origDataRollingForecast<-testForecasting(equations,allData,1997,rolling=T)
	origDataRollingForecast<-origDataRollingForecast[,c("RMSE.RW","RMSE.cum","RMSE","cum.RMSE","RMSE.Demo","cum.RMSE.Demo")]
	
	printTable(origDataRollingForecast[names(equations),],"Rolling Forecast, 1997-2007","../tex/tables/foreRolling.tex",
             label="tab:foreRolling",digits=3)
}

# Calculates forecast metrics for a random walk model
rmseRW <- function(variable, data, year,rolling=F)
{
	# Estimate a model on a constant only using between, so that the constant will capture the per-country drift
	est.model<-plm(formula(paste(variable,"~Country-1",sep="")),data=data[data$Year <= year,],index=idx,model='pooling')
	
	# This is a bogus estimation but it seems to be the only way to get the data we need
	fore.model<-plm(formula(paste(variable,"~Country-1",sep="")),data=data[data$Year >= year,],index=idx,model='pooling')
	
	# Extract the data that we can then use for forecasting
	fore.data<-as.data.frame(model.frame(fore.model))
  est.data<-as.data.frame(model.frame(est.model))
  
	# The intercept is the first coefficient, which is also 	
	currentCountry<-"Dummy"
	
	cumulativeForcast<-0
	cummulativeActual<-0
	
	# EFFING SHITE APPLY, ADPLY, WHATEVER-PLY ALL HAVE EFFING TIME-WASTING PROBLEMS SO WE HAVE TO DO THIS BY HAND
	forecastData<-data.frame(act=numeric(nrow(fore.data)),fore=numeric(nrow(fore.data)),act.cum=numeric(nrow(fore.data)),fore.cum=numeric(nrow(fore.data)))
	
	for(index in 1:nrow(fore.data))
	{
		thisCountry<-fore.data[index,2]
		
		# The first column of the data frame used in the estimation of the *future* model is the actual (i.e. to be estimated) value
		actual<-fore.data[index,1]
		
		# The estimate is the intercept…plus the country adjustment for countries other than the first
    if(thisCountry != currentCountry)
    {
      currentCountry<-thisCountry
      
      forecast<-lastNonNA(est.data[est.data[,2] == thisCountry,1])
      
      if(length(forecast) == 0)
      {
        debugOut(thisCountry)
        debugOut(priorYearForCountry)
      }
        #est.model$coefficients[[paste("Country", thisCountry,sep='')]]
    }
    else
    {
      if(rolling)
      {
        forecast<-forecastData$fore[index-1]
      }
      else
      {
        forecast<-forecastData$act[index-1]
      }
    }
    
		# Return the forecast 
		forecastData$act[index]<-actual
		forecastData$fore[index]<-forecast
		
		# if(rolling)
		# {
			# # If this is the first year of a new country…
			# if(thisCountry != currentCountry)
			# {
				# # reset cumulative values	
				# cumulativeForecast<-0
				# cumulativeActual<-0
				
				# # remember the new country
				# currentCountry<-thisCountry
			# }
			
			# # update cumulative values, and make actual and forecast cumulative
			# cumulativeForecast<-cumulativeForecast+forecast
			# #forecast<-cumulativeForecast
			
			# cumulativeActual<-cumulativeActual+actual
			# #actual<-cumulativeActual
			
			# forecastData$act.cum[index]<-cumulativeActual
			# forecastData$fore.cum[index]<-cumulativeForecast
		
		# }
	}
	if(rolling)
	{
		aggByCountry<-aggregate(forecastData[,c('act','fore')],list(by=fore.data[,2]),mean)
		
		return(list(RMSE=sqrt(mean((forecastData$act-forecastData$fore)^2)),RMSE.cum=sqrt(mean((aggByCountry$act-aggByCountry$fore)^2))))
	}
	else
	{
		return(list(RMSE=sqrt(mean((forecastData$act-forecastData$fore)^2))))		
	}
}

# Output result tables for our VAR
resultTables<-function(path="../tex/tables")
{
  resTab1<-multiResultTable(varModel$y,varModel$i,varModel$s)
  caption(resTab1)<-"Results for Growth, Investment and Savings"
  label(resTab1)<-"tab:resGIS"
  print(fixRowNames(resTab1),sanitize.text.function=function(text){text}, file=paste(path,"/resultsGIS.tex",sep=""))
  
  resTab2<-multiResultTable(varModel$h,varModel$rr,varModel$pi)
  caption(resTab2)<-"Results for Hours, Interest Rate, and Inflation"
  label(resTab2)<-"tab:resHRPi"
  print(fixRowNames(resTab2),sanitize.text.function=function(text){text}, file=paste(path,"/resultsHRRDPi.tex",sep=""))
  print("Done")
}

# Calculate the difference between the demographic impact in year (or the first year in the data)
# and the last year in the data for each country
demoImpactsByCountry<-function(countries=NA,year=0)
{
  result<-aggregate(demoImpacts[demoData$Year>=year,],list(demoData$Country[demoData$Year>=year]),FUN=function(data){100*(data[length(data)]-data[1])})
  rownames(result)<-result[,1]
  result[,1]<-NULL
  
  if(!is.na(countries))
  {
    result<-result[rownames(result) %in% countries,]
  }
  
  result<-result[order(row.names(result)),]
  result
}

demoRangesByCountry<-function()
{
  result<-aggregate(demoImpacts,list(demoData$Country),FUN=function(data){100*(diff(range(data)))})
  rownames(result)<-result[,1]
  result[,1]<-NULL
  colnames(result)<-c("y","I","S","H","rr","$\\pi$")
  result$H<-exp(result$H/100)
  result
}

averagePredictions<-function()
{
  results<-data.frame()
  
  for(country in c(unique(allData$Country),"Germany"))
  {
    resultForCountry<-data.frame(country = country, 
                                 noughties=100*mean(econForecasts[forecastData$Country == country & forecastData$Year %in% 2000:2009,"Y"]),
                                 tens=100*mean(econForecasts[forecastData$Country == country & forecastData$Year %in% 2010:2019,"Y"]))
    
    results<-rbind(results,resultForCountry)
  }
  rownames(results)<-results$country
  results$country<-NULL
  colnames(results)<-c("2000-2009","2010-2019")
  results$Change<-results[,2]-results[,1]
  results[order(row.names(results)),]
}

predictedActualOrder<-function(yearRange)
{
  results<-data.frame()
  for(country in c(unique(allData$Country)))
  {
    resultForCountry<-data.frame(Country = country, noughties=100*mean(econForecasts[forecastData$Country == country & forecastData$Year %in% yearRange,"G"]))
    
    results<-rbind(results,resultForCountry)
  }
  
  actualAvg<-with(pwtNew[pwtNew$Year %in% c(yearRange,min(yearRange)-1),],aggregate(rgdpch,list(Country),FUN=function(values){(log(values[length(values)])-log(values[1]))/length(values)-1}))
  names(actualAvg)<-c("Country","Growth")
  actualAvg$actualOrder<-nrow(actualAvg)-rank(actualAvg$Growth)
  results$predictedOrder<-nrow(results)-rank(results$noughties)
  results<-merge(results,actualAvg,by="Country")
  results
}

predictivePlot<-function(yearRange=2000:2009, showCorr=F)
{
  results<-predictedActualOrder(yearRange)
  plot(results$predictedOrder,results$actualOrder,xlab="Predicted Rank", ylab="Actual Rank",pty='+')
  if(showCorr)
  {
    text(1,1,paste("Correlation: ",cor(results$predictedOrder,results$actualOrder),sep=""),pos=4)
  }
  print(paste("Correlation: ",cor(results$predictedOrder,results$actualOrder),sep=""))
  print(paste("Variable Correlation: ",cor(results$Growth,results$noughties),sep=""))
}

predictivePlotAllDecades<-function(periodLength=10)
{
  results<-data.frame()
  periodStart<-1970
  
  for(periodEnd in seq(1970+periodLength-1,2009,by=periodLength))
  {
    nextResults<-predictedActualOrder((periodEnd-periodLength+1):periodEnd)
    nextResults$Decade<-periodEnd
    results<-rbind(results,nextResults)
  }
  plot(results$predictedOrder,results$actualOrder)
  text(1,1,paste("Correlation: ",cor(results$predictedOrder,results$actualOrder),sep=""),pos=4)
  print(paste("Variable Correlation: ",cor(results$Growth,results$noughties),sep=""))
  
  for(country in unique(results$Country))
  {
    results$predictedOrder[results$Country==country]<-40/periodLength-rank(results$noughties[results$Country==country])
    results$actualOrder[results$Country==country]<-40/periodLength-rank(results$Growth[results$Country==country])
  }
  X11()
  plot(results$predictedOrder,results$actualOrder)
  text(1,1,paste("Correlation: ",cor(results$predictedOrder,results$actualOrder),sep=""),pos=4)
}

plotForecast<-function(country,maxYear=2030,meanLength=5,minYear=1970)
{
  impactForCountry<-econForecasts[forecastData$Country==country & forecastData$Year<maxYear,1]
  xlim<-c(min(forecastData$Year[forecastData$Country==country]),maxYear)
  trendForCountry<-with(pwtNew[pwtNew$Country==country,],diff(log(rgdpch),meanLength)/meanLength)
  trendForCountry<-trendForCountry[!is.na(trendForCountry)]
  #trendForCountry<-rollmean(trendForCountry,5,na.rm=T)	
  firstYear<-min(forecastData[forecastData$Country==country,"Year"],minYear) 
  firstRow<-match(paste(country,"-",firstYear+(meanLength-1)/2,sep=""),names(trendForCountry)) 
  debugOut(firstRow)	
  if(is.na(firstRow))
  {
    firstRow<-1
    trendFirstYear<-as.numeric(substr(names(trendForCountry)[1],nchar(country)+2,nchar(country)+6))-(meanLength-1)/2
  }
  else
  {
    trendFirstYear<-firstYear
  }
  
  lastYear<-trendFirstYear+length(trendForCountry)-firstRow
  trendForCountry<-trendForCountry[firstRow:length(trendForCountry)]
  debugOut(trendForCountry)
  debugOut(impactForCountry)
  ylim<-range(impactForCountry,trendForCountry)
  plot(impactForCountry~forecastData$Year[forecastData$Country==country & forecastData$Year<maxYear],type='l',xlab='Year',ylab='Impact of Age Structure on GDP Growth',xlim=xlim,ylim=ylim)
  par(new=T)
  plot(trendFirstYear:lastYear,trendForCountry,type='l',lty='dashed',xlab='',ylab='',xlim=xlim,ylim=ylim)
}

testInteractiveEffects<-function(bModel)
{
  updModel<-update(bModel,.~.+I(alpha_1^2)+I(alpha_2^2)+I(alpha_3^2)+I(alpha_4^2)+I(alpha_5^2)+I(alpha_6^2)+I(alpha_7^2))
  updModel2<-update(bModel,.~.+(alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6)*(alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7))
  # debugOut(coeftest(updModel,vcov.=vcov))
  print(paste("SQUARE: SBC plain: ", criterion(bModel)," SBC squared: ",criterion(updModel)))
  print(paste("Wald :",waldtest(bModel,updModel,vcov=myvcov)[2,4]))
  print(paste("INTERACT: SBC plain: ", criterion(bModel)," SBC squared: ",criterion(updModel2)))
  print(paste("Wald :",waldtest(bModel,updModel2,vcov=myvcov)[2,4]))
}

plotMultiTrends<-function(plotFun=plotTrendVDemoAlt,name="")
{
  mar<-par("mar")
  oma<-par('oma')	
  par(mfrow=c(2,2),mar=c(4,4,0,0)+0.1, oma=c(1,1,1,1)+0.1)
  index<-0
  for(country in unique(allData$Country))
  {
    index<-index+1
    plotFun(country,setXLab=F,xlab=country,setYLab=((index%%2)!=0),maxYear=2030)
    
    if((index %% 4) == 0)
    {
      dev.copy2pdf(file=paste('../tex/figures/forecastTrends',name,index/4,'.pdf',sep=''))
    }
  }
  if((index %% 4) != 0)
  {
    dev.copy2pdf(file=paste('../tex/figures/forecastTrends',floor(index/4)+1,'.pdf',sep=''))
  }
  
  par(mfrow=c(1,1),mar=mar,oma=oma)
}

plotDemoMeans<-function()
{
  demoProps<-rbind(demoData[demoData$Country %in% unique(allData$Country),c("Country","Year",paste("p",1:8,sep=""))],demoForecasts[demoForecasts$Country %in% unique(allData$Country),c("Country","Year",paste("p",1:8,sep=""))])	
  demoProps<-demoProps[demoProps$Year %in% 1970:2030,]
  demoByYear<-aggregate(demoProps[,paste("p",1:8,sep="")],list(demoProps$Year),mean)
  rownames(demoByYear)<-demoByYear$Group.1
  barplot(t(as.matrix(demoByYear[,2:9])))
  mtext("0-9",side=4,line=-1,at=0.05,las=1,adj=-0.1)
  mtext("10-19",side=4,line=-1,at=0.15,las=1,adj=-0.1)
  mtext("20-29",side=4,line=-1,at=0.28,las=1,adj=-0.1)
  mtext("30-39",side=4,line=-1,at=0.40,las=1,adj=-0.1)
  mtext("40-49",side=4,line=-1,at=0.51,las=1,adj=-0.1)
  mtext("50-59",side=4,line=-1,at=0.65,las=1,adj=-0.1)
  mtext("60-69",side=4,line=-1,at=0.78,las=1,adj=-0.1)
  mtext("70+",side=4,line=-1,at=0.90,las=1,adj=-0.1)
}

plotDemoMeansLines<-function()
{
  demoProps<-rbind(demoData[demoData$Country %in% unique(allData$Country),c("Country","Year",paste("p",1:8,sep=""))],demoForecasts[demoForecasts$Country %in% unique(allData$Country),c("Country","Year",paste("p",1:8,sep=""))])	
  demoProps<-demoProps[demoProps$Year %in% 1970:2030,]
  demoByYear<-aggregate(demoProps[,paste("p",1:8,sep="")],list(demoProps$Year),mean)
  
  rownames(demoByYear)<-demoByYear$Group.1
  
  ylim<-c(0.05,0.20)#range(demoByYear[,2:9])
  cols<-gray(seq(0,0.6,0.2))
  
  lty = 1:8
  lwd = floor(lty/7)+1
  
  for(index in 2:9)
  {
    plot(1970:2030,demoByYear[,index],ylim=ylim,type='l',lty=lty[index-1],lwd=lwd[index-1],yaxt='n',xaxt='n',xlab='',ylab='')
    par(new=T)
  }
  
  axis(1)
  axis(2)
  abline(v=2011,lty='dashed',col=cols[4])	
  mtext("Year",1,3,cex=1.5)
  mtext("Proportion of Population in Age Group",2,2.5,cex=1.5)
  legend(locator(1),legend=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70+"),lty=lty,lwd=lwd,ncol=2)
  par(new=F)
}

plotDemoSample<-function(demoVar="p1")
{
  sampleCountries<-c("United Kingdom", "United States", "Japan", "Italy")
  sampleData<-allData[allData$Country %in% sampleCountries,]
  ylim<-range(sampleData[,demoVar])
  for(i in 1:length(sampleCountries))
  {
    with(sampleData[sampleData$Country == sampleCountries[i],],plot(Year,eval(parse(text=demoVar)),type='l',lty=i,ylim=ylim,xaxt='n',yaxt='n',xlab='',ylab=''))
    par(new=T)
  }
  
  axis(1)
  axis(2)
  mtext("Year",1,3,cex=1.5)
  mtext("Proportion of Population Aged 20-29",2,2.5,cex=1.5)
  legend(locator(1),legend=sampleCountries,lty=1:length(sampleCountries))
  par(new=F)
}

plotAvgAge<-function(demoVar="p1")
{
  avgAge<-allData$p1*5+allData$p2*15+allData$p3*25+allData$p4*35+allData$p5*45+allData$p6*55+allData$p7*65+allData$p8*75
  sampleCountries<-c("United Kingdom", "United States", "Japan", "Italy")
  ylim<-range(avgAge[allData$Country %in% sampleCountries])
  for(i in 1:length(sampleCountries))
  {
    plot(1970:2007,avgAge[allData$Country == sampleCountries[i]],type='l',lty=i,ylim=ylim,xaxt='n',yaxt='n',xlab='',ylab='')
    par(new=T)
  }
  
  axis(1)
  axis(2)
  mtext("Year",1,3,cex=1.5)
  mtext("Average Age of Population",2,2.5,cex=1.5)
  legend(locator(1),legend=sampleCountries,lty=1:length(sampleCountries))
  par(new=F)
}

plotOPvInfl<-function()
{
  ylim=c(-3,3)
  xlim=c(1970,2006)
  plot(1969+1:37,scale(diff(pallData$Oil.Price[pallData$Country=="Austria"])),type='l',ylim=ylim,xlim=xlim,ylab="Oil Price Change v. Mean Inflation(dashed)",xlab="Year")
  par(new=T)
  plot(1969+1:37,scale(aggregate(pallData$infl,list(pallData$Year),mean)$x[c(-38)]),type='l',lty='dashed',ylim=ylim,xlim=xlim,xlab="",ylab="")
  par(new=F)
}