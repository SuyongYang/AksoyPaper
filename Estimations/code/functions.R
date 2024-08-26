source('./utilityFunctions.R')

# WTF?
prepareNewData<-function()
{
  econData<-processEcon('../data_02_12_11/sna_table1-annual_GDP_02122011.csv','../data_02_12_11/sna_table2-annual_DispIncome_02122011.csv') 
  demoData<-processHistoric()
  labourData<-processLabour()
  inflationData<-processNewInflation()
  cb_rates<-processNewCBRates()
  pwtNew<-processPWTNew()
  
  newData<-merge(econData,demoData,by=idx)
  newData<-merge(newData,labourData,by=idx)
  newData<-merge(newData,inflationData,by=idx)
  newData<-merge(newData,cb_rates,by=idx)
  newData<-merge(newData,pwtNew,by=idx)
  newData<-merge(newData,oilPrices)
  
  newData
}

# Returns a column containing the trend component of rgdpch
createFilteredGDP<-function(data)
{
	# Create a results column
	resultColumn<-numeric(nrow(data))
	
	for(country in unique(data$Country))
	{
		filter<-with(data[data$Country==country,],hpfilter(log(rgdpch*Total),freq=6.25))
		resultColumn[data$Country==country]<-with(filter,trend)
	}
	return(resultColumn)
}

# From a model extracts each column of data in turn and summarises it, returning
# a frame with all the results
collectModelRanges<-function(model)
{
	result<-NULL
	for(col in 1:ncol(model$model))
	{
		colSummary<-c(name=names(model$model)[col],as.list(summary.default(model$model[,col])),var=var(model$model[,col]))
		debugOut("Here")
		if(is.null(result))
		{
			debugOut("Creating")
			debugOut(names(colSummary))
			result<-data.frame(colSummary)
			debugOut("Created")
		}
		else
		{
	 		debugOut("Adding")
			debugOut(names(colSummary))
			result<-rbind(result,data.frame(colSummary))
	 		debugOut("Added")
		}
	}
	result
}

# Performs rolling break-poin tests onb the specified formula and data
rollingBreakpointTest<-function(formula, data,maxLags=1)
{
	fullModel<-plm(formula,data=data,index=idx,effect="individual")
	fullCriterion<-criterion(fullModel)
	debugOut(attr(fullCriterion,"K"))
	
	optValue<- fullCriterion
	optPos <- 0

	print(paste("Full Criterion: ",fullCriterion,sep=""))
	try(
	for(year in (min(data$Year)+maxLags+2):(max(data$Year)-1-maxLags))
	{
		models<-modelsWithBreak(formula,data,year,maxLags)

		breakCriterion<-criterion(models$early,models$late)

		# Need the 
		if(attr(fullCriterion,"N") != attr(breakCriterion,"N"))
		{
			stop("The number of observations in the full sample does not match those in the sub-samples")
		}

		if(breakCriterion > optValue)
		{
			optValue<-breakCriterion
			optPos<-year
		}	
		
		print(paste("Year: ",year,", Criterion: ",breakCriterion,", Accept?: ",breakCriterion>fullCriterion,sep=""))
		debugOut(attr(breakCriterion,"K"))
	})

	print(paste("Optimal break at year ",optPos,", SBC is ",optValue," (Full set: ",fullCriterion,")",sep=""))
}

modelsWithBreak<-function(formula,data,year,maxLags=1)
{
	early<-plm(formula,data=data[data$Year <= year,],index=idx,effect="individual")

	# We need the greater than sign because it needs to include the 'prior' year to deal with first lags
	late<-plm(formula,data=data[data$Year > (year-maxLags),],index=idx,effect="individual")

	list(early=early, late=late)
}

# Performs model selection tests to find the model to use
modelSelection<-function()
{
  alphas<-all.vars(gdpFormula)[6:12]
  gdpMod<-runBothEffects("GDP",gdpFormula,alphas,adjustTwoWays=.~.-diff(log(Oil.Price))-lag(log(Oil.Price)))  
  savingsWealth<-runBothEffects("Savings with Wealth",savingsWealthFormula,alphas)
  savingsRes<-runBothEffects("Savings (Restricted to Wealth Dataset",savingsFormula,alphas, rowSelectionFn=getRowsWithWealth)
  savings<-runBothEffects("Savings",savingsFormula,alphas)
  investment<-runBothEffects("Investment", invFormula,alphas)
  investmentWSavings<-runBothEffects("Investment (with Savings)", invFormulaWithSavings,alphas)
  
  updateForm<-.~.-alpha_1-alpha_2-alpha_3-alpha_4-alpha_5-alpha_6-alpha_7+p1+p2+p3+p5+p6+p7+p8
  alphas<-all.vars(update(gdpFormula,updateForm))[6:12]
  
  print("With Direct Proportions")
  print("")
  gdpMod<-runBothEffects("GDP",update(gdpFormula,updateForm),alphas,adjustTwoWays=.~.-diff(log(Oil.Price))-lag(log(Oil.Price)))  
  savingsWealth<-runBothEffects("Savings with Wealth",update(savingsWealthFormula,updateForm),alphas)
  savingsRes<-runBothEffects("Savings (Restricted to Wealth Dataset",update(savingsFormula,updateForm),alphas, rowSelectionFn=getRowsWithWealth)
  savings<-runBothEffects("Savings",update(savingsFormula,updateForm),alphas)
  investment<-runBothEffects("Investment", update(invFormula,updateForm),alphas)
  investmentWSavings<-runBothEffects("Investment (with Savings)", update(invFormulaWithSavings,updateForm),alphas)
  
  
}

runBothEffects<-function(name, form, alphas, adjustTwoWays=.~.,rowSelectionFn=function(data){T})
{
	theData<-allData[rowSelectionFn(allData),]	
	individualModel<-plm(form,data=theData,index=idx,effect="individual", model="within")

	form2ways<-update(form,adjustTwoWays)
  debugOut(form2ways)
	twoways<-plm(form2ways,data=theData,index=idx,effect="twoways")
	
	print(paste(name,":",sep=""))
	print(paste(" individual: ",criterion(individualModel)[1],sep=""))
	print(paste(" twoways: ",criterion(twoways)[1],sep=""))
	sig<-waldtest(individualModel,update(individualModel,as.formula(paste(".~.-",paste(alphas,collapse="-"),sep=""))),vcov = myvcov)
	print(paste("alpha sig: ",sig$'Pr(>Chisq)'[2],sep=""))
	print("")
  
	list(individual=individualModel,twoways=twoways)
}

SBCTable<-function(formula,data,...)
{
  partStrings<-as.character(formula)
  depVar<-partStrings[2]
  expVars<-partStrings[3]
  
  results<-data.frame(criterion=numeric(2^7))
  
  for(index in 0:(2^7-1))
  {
    if(index %% 10 == 0 && index > 0)
    {
      print(index)
    }
      currentExp<-expVars
      
      for(varIndex in 0:6)
      {
        if(bitAnd(index,2^varIndex) > 0)
        {
          currentExp<-paste(currentExp,"+alpha_",as.character(varIndex+1),sep="")
        }
      }
      
      currentFormula<-as.formula(paste(depVar,"~",currentExp,sep=""))
      currentMod<-plm(currentFormula,data,index=c("Country","Year"),...)
      results$criterion[index+1]<-criterion(currentMod)
  }
   
  results
}

plotByCountryIndividual<-function(data, countries, directory)
{
	index<-0
	
	countryNames<-unique(countries)
	
	for(country in countryNames)
	{
		for(colname in names(data))
		{
			dir.create(paste(directory,"/",colname,sep=""),recursive=T)
			index<-index+1
			plot(data[countries==country,colname], type="l")
			dev.copy2pdf(file=paste(directory,"/",colname,"/",country,".pdf",sep=""))
		}
	}	
}

plotByCountry<-function(data, countries,groups=1,ncol=1,...)
{
	ylim<-c(min(data,na.rm=T),max(data,na.rm=T))
	
	xlim<-c(1,max(aggregate(data,list(countries),FUN=length)$x))
	index<-0
	
	countryNames<-unique(countries)
	
	splitCountries<-split(countryNames,index(countryNames)%%groups+1)
	
	for(groupIndex in 1:groups)
	{
		X11()
		cols<-character(length(splitCountries[[groupIndex]]))
    cols[]<-rainbow(7)
		styles<-character(length(cols))
    styles[]<-c("solid","dashed","dotted","dotdash","longdash")
		index<-0
		for(country in splitCountries[[groupIndex]])
		{
			index<-index+1
			plot(data[countries==country], type="l",xlim=xlim,ylim=ylim,xlab="",ylab="",col=cols[index],lty=styles[index],...)
			par(new=T)
		}
		#legend((xlim[2]+xlim[1])/2, (ylim[2]-ylim[1])/3+ylim[1],splitCountries[[groupIndex]],col=cols,lty="solid")
		legend(locator(1),legend=splitCountries[[groupIndex]],col=cols,lty=styles,ncol=ncol)
	}	
}


createRollingPlots<-function()
{
	# Creates all types of rolling regression plots for all formulae
	rollingAll(allData,gdpFormula,"GDP")
	rollingAll(allData,savingsFormula,"Savings")
	rollingAll(allData,invFormula,"Investment")
}

plotDemoByCountry<-function(dataSet)
{
	dir.create('../tex/figures/demoByCountry/')
	par(las=2)
	for(country in unique(dataSet$Country))
	{
		barplot(t(as.matrix(dataSet[dataSet$Country==country,paste("p",1:8,sep="")])),names.arg=1970:2007,col=rainbow(8),main=country)
		dev.copy2pdf(file=paste('../tex/figures/demoByCountry/',country,'.pdf',sep=""))
	}
}

plotDemoByCountryLines<-function(dataSet)
{
	dir.create('../tex/figures/demoByCountryLines/')
	col<-rainbow(8)
	par(las=2)
	for(country in unique(dataSet$Country))
	{
		multiplot(allData[allData$Country==country,c("Year",paste("p",1:8,sep=""))],c(paste("Age ",seq(0,60,10),"-",seq(9,69,10),sep=""),"Age 70+"),T,"Year",country,c(0,0.25))
		dev.copy2pdf(file=paste('../tex/figures/demoByCountryLines/',country,'.pdf',sep=""))
	}
}

##
# Calculates the growth rate of the named series, assuming there are Year and Country columns that
# contain the obvious data. First year rates are set to NA
##
growthRate<-function(dataFrame,series)
{
	growthRate<-c(NA,dataFrame[-1,c(series)]/dataFrame[-nrow(dataFrame),c(series)]-1)
	
	for(country in unique(dataFrame$Country))
	{
		minYear<-min(dataFrame$Year[dataFrame$Country==country])
		growthRate[dataFrame$Year == minYear & dataFrame$Country == country]<-NA
	}
	return(growthRate)
}

plotDepVars<-function(dataSet)
{
	# Get yearly mean values (UNWEIGTHED at present) for the series we want to plot
	meanValues1<-aggregate(dataSet[,c("Savings.Rate","Investment.Rate","g","infl","realInterest",paste("p",(0:7)*10,sep=""))],list(Year=dataSet$Year),mean,na.rm=T)

	# Standardise the values so that they can be plotted in the same range
	meanValues[,-1]<-scale(meanValues1[,-1])
	meanValues$realInterest<--meanValues$realInterest

	# Calculate the common max and min
	ylim<-c(min(meanValues[,-1],na.rm=T),max(meanValues[,-1],na.rm=T))
	
	defaults <- par(no.readonly = TRUE)
	
	par(xaxs="i",yaxs="i")
	with(meanValues,plot(Savings.Rate~Year,ylim=ylim,xaxs="i",yaxs="i",xlab="Year",ylab="Value",type="l"))
	par(new=T,xaxt='n',yaxt='n')
	with(meanValues,plot(Investment.Rate~Year,ylim=ylim,xlab="",ylab="",type="l",lty="dashed",col="blue"))
	par(new=T,xaxt='n',yaxt='n')
	with(meanValues,plot(g~Year,ylim=ylim,xlab="",ylab="",type="l",lty="longdash",col="red"))
	par(new=T,xaxt='n',yaxt='n')
	with(meanValues,plot(infl~Year,ylim=ylim,xlab="",ylab="",type="l",lty="dotdash",col="green"))
	par(new=T,xaxt='n',yaxt='n')
	with(meanValues,plot(realInterest~Year,ylim=ylim,xlab="",ylab="",type="l",lty="solid",col="purple"))
	par(new=T,xaxt='n',yaxt='n')
	with(meanValues,plot(p40~Year,ylim=ylim,xlab="",ylab="",type="l",lty="solid",col="blue"))
	
	legend(2000, 2.8,c("Savings Rate","Inv. Rate","GDP Growth","Inflation", "-Real Interest","Ages 40-50"),col=c("black","blue","red", "green","purple","blue"), lty=c("solid","dashed","longdash","dotdash","solid","solid"))
	dev.copy2pdf(file="../tex/figures/mainVars.pdf")
	par(defaults)

	ylim<-c(0.05,0.2)
	styles<-c("solid","dashed","longdash","dotdash","solid","dashed","longdash","dotdash")	
	colours<-c("black","blue","red","black","blue","red","black","blue")
	for(index in 0:7)
	{
		if(index == 0)
		{
			par(xaxs="i",yaxs="i")
			xlab<-"Year"
			ylab<-"Proportion of Population in Age Group"
		}
		else
		{
			par(new=T,xaxt='n',yaxt='n')
			xlab<-""
			ylab<-""
		}
			
		with(meanValues1,plot(meanValues1[,paste("p",index*10,sep="")]~Year,ylim=ylim,xlab=xlab,ylab=ylab,type="l",lty=styles[index+1],col=colours[index+1]))
	}
	legend(2003, 0.199,c("0-9","10-19","20-29","30-39", "40-49","50-59","60-69","70+"),col=colours, lty=styles)
	dev.copy2pdf(file="../tex/figures/demoVars.pdf")

	par(defaults)
}

regressions<-function(dataSet)
{
	idx <- c("Country","Year")

	#savingsFormula<-Savings.Rate~alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7+realInterest+rgdpch
	savingsFormula<-Savings.Rate~alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7+realInterest
	#savingsFormula<-Savings.Rate~alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7+realInterest+lag(Savings.Rate)
	#savingsFormula<-Savings.Rate~alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7+lag(alpha_1)+lag(alpha_2)+lag(alpha_3)+lag(alpha_4)+lag(alpha_5)+lag(alpha_6)+lag(alpha_7)+realInterest+lag(Savings.Rate)+rgdpch

	savingsModel<-plm(savingsFormula,data=dataSet,index=idx,effect="twoways")
	#savingsModel<-plm(savingsFormula,data=dataSet,index=idx,model="pooling")
	modelOutput(savingsModel,"Panel Regression results for Savings","Savings")
	
	savingsLagFormula<-Savings.Rate~p8+lag(Savings.Rate)
	#savingsLagFormula<-Savings.Rate~alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7+realInterest++lag(alpha_1)+lag(alpha_2)+lag(alpha_3)+lag(alpha_4)+lag(alpha_5)+lag(alpha_6)+lag(alpha_7)+lag(realInterest)+ lag(Savings.Rate)
	savingsLagModel<-plm(savingsLagFormula,data=dataSet,index=idx,effect="twoways")
	#savingsModel<-plm(savingsFormula,data=dataSet,index=idx,model="pooling")
	modelOutput(savingsLagModel,"Panel Regression results for Savings with Lag","SavingsLag")
	
	#savingsWealthFormula<-Savings.Rate~alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7+realInterest+Financial.Wealth+Housing.Wealth+lag(Financial.Wealth)+lag(Housing.Wealth)
	#savingsWealthFormula<-Savings.Rate~alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7+realInterest+Financial.Wealth+Housing.Wealth
	savingsWealthFormula<-Savings.Rate~alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7+lag(alpha_1)+lag(alpha_2)+lag(alpha_3)+lag(alpha_4)+lag(alpha_5)+lag(alpha_6)+lag(alpha_7)+lag(realInterest)+lag(Savings.Rate)+Financial.Wealth+Housing.Wealth+lag(Financial.Wealth)+lag(Housing.Wealth)
	savingsWealthModel<-plm(savingsWealthFormula,data=dataSet,index=idx, effect="twoways")
	modelOutput(savingsWealthModel,"Panel Regression results for Savings with Wealth","SavingsWealth")
	
	investmentFormula<-Investment.Rate~alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7+Savings.Rate
	investmentModel<-plm(investmentFormula,data=dataSet,index=idx,effect="twoways")
	modelOutput(investmentModel,"Panel Regression results for Investment","Investment")

	growthFormula<-I(log(rgdpch)-(log(lag(rgdpch))))~alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7+lag(Investment.Rate)+log(lag(rgdpch))+I(Total/lag(Total))
	growthModel<-plm(growthFormula,data=dataSet,index=idx,effect="twoways")
	modelOutput(growthModel,"Panel Regression results for Growth","Growth")

	levelFormula<-log(rgdpch)~alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7+log(h)+log(K)
	levelModel<-plm(levelFormula,data=dataSet,index=idx,effect="twoways")
	modelOutput(levelModel,"Panel Regression results for Level","Level")

	ecmFormula<-I(log(rgdpch)-(log(lag(rgdpch))))~I(alpha_1 - lag(alpha_1))+I(alpha_2 - lag(alpha_2))+I(alpha_3 - lag(alpha_3))+I(alpha_4 - lag(alpha_4))+I(alpha_5 - lag(alpha_5))+I(alpha_6 - lag(alpha_6))+I(alpha_7 - lag(alpha_7))+I(log(h)-lag(log(h)))+I(log(K)-lag(log(K)))+log(lag(rgdpch))+lag(alpha_1)+lag(alpha_2)+lag(alpha_3)+lag(alpha_4)+lag(alpha_5)+lag(alpha_6)+lag(alpha_7)+lag(log(h))+lag(log(K))
	ecmModel<-plm(ecmFormula,data=dataSet,index=idx)
	modelOutput(ecmModel,"Panel Regression results for Level(ECM)","Level(ECM)")

	hoursFormula<-h~alpha_1+alpha_2+alpha_3+alpha_4+alpha_5+alpha_6+alpha_7
	hoursModel<-plm(hoursFormula,data=dataSet,index=idx,effect="twoways")
	modelOutput(hoursModel,"Panel Regression results for Hours Worked","Hours")
}

modelOutput<-function(model,caption,name)
{
  # Saves the coefficients of the given model in a table with the given caption and name
  outTab<-fixRowNames(xtable(coeftest(model,vcov.=vcovHC(model, method="white2", type="HC3")), caption=caption, label=paste("tab:",name,sep="")))
  print(outTab, file=paste("../tex/tables/",fixFileName(name),".tex",sep=""), size="scriptsize",sanitize.rownames.function=function(names){names})
	
  modelAlphas<-getAlphas(model)
  plot(modelAlphas,ylab=paste("Impact of Proportional Cohort size on",name),type='l', xlab="Cohort")
  dev.copy2pdf(file=paste("../tex/figures/",fixFileName(name),".pdf",sep=""))
}

###
# Creates a data frame containing one row per country and one column for each data column which specifies
# for which range of years the data is available for that country
###
summariseByCountry<-function(dataFrame,showAll=F)
{	
	startYear<-min(dataFrame$Year)
	endYear <- max(dataFrame$Year)
	
	# Prepare a results frame with one row for each country
	results <- data.frame(Country=as.factor(unique(dataFrame$Country))) 
	#names(results) <- "Country"
	
	for(colIndex in 3:length(dataFrame))
	{
		# Get the data for the current column
		colSet <- dataFrame[,c(1,2,colIndex)]
		
		# Check whether there are any missing rows
#		missingForCol <- colSet[!complete.cases(colSet),]
#	
#		# if not, go to the next column - all data present here	
#		if(nrow(missingForCol)==0)
#		{
#			next
#		}
		
		aggregated <- aggregate(dataFrame[colIndex], list(dataFrame$Country),function(dataCol){
					result <- ""
					min <- 0
					max <- 0
					missingYears <- vector()
					for(index in 1:length(dataCol))
					{
						currentYear <- startYear+index-1
						if(!is.na(dataCol[index]))
						{
							if(min == 0)
							{
								min <- currentYear 
							}
							
							max <- currentYear					
						}
						else
						{
							if(min > 0)
							{
								missingYears <- c(missingYears,currentYear)
							}
						}
					}
					missingYears<-missingYears[missingYears < max]
					
					if(showAll | (min>startYear|max<endYear))
					{
						result <- paste(as.character(min),"-",as.character(max))
					}
					
					if(length(missingYears)>0)
					{
						missingStr<-paste(missingYears,collapse=",")
						print(missingStr)
						result <- paste(result," [",missingStr,"]",sep="")
					}
					
					result
				});
		names(aggregated) <- c("Country",names(colSet)[3])
#		aggregated[,2]  <- aggregated[,2]+1
		results  <- merge(results,aggregated, all.x = T)
#		results[is.na(results[,names(colSet)[3]]),names(colSet)[3]] <- 1970
	}
	
#	results$Overall<-apply(results,1,function(x){max(x[2:length(results)])})
	results
}

createSummaryStatsTable<-function(data,caption,name)
{
	outTab<-xtable(data, display=c('d','fg','fg'),caption=caption, label=paste("tab:",name,sep=""))
	print(outTab, file=paste("../tex/",name,".tex",sep=""), size="scriptsize")
}

createTable<-function(data,caption,name)
{
	outTab<-xtable(data, caption=caption, label=paste("tab:",name,sep=""))
	#outTab<-xtable(data, display=rep('d',length(data)+1),caption=caption, label=paste("tab:",name,sep=""))
	print(outTab, file=paste("../tex/tables/",name,".tex",sep=""), size="scriptsize")
}

summarise<-function(dataFrame, names=character(0))
{
	if(length(names)==0)
	{
		names<-names(dataFrame)
	}
	
	resultsFrame<-data.frame(Mean=numeric(0), Std=numeric(0))
	
	for(name in names)
	{
		dataForName<-dataFrame[,c(name)]
		resultsFrame<-rbind(resultsFrame,data.frame(list(mean=mean(dataForName,na.rm=T),std=sd(dataForName,na.rm=T))))
	}
	
	row.names(resultsFrame)<-names
	
	return(resultsFrame)
}

appendListToFrame<-function(dataFrame,listForRow, depVar)
{
	listForRow <- as.list(listForRow)
	
	# First deal with the lagged dependent variable
	LDPname<-paste("lag(",depVar,")",sep="")
	for(index in 1:(length(listForRow)))
	{
		if(names(listForRow)[index] == LDPname)
		{
			names(listForRow)[index]<-"LDV"
		}
	}
	# Add any missing entries to the list
	for(name in names(dataFrame))
	{
		if(!(name %in% names(listForRow)))
		{
			listForRow[name] <- NA
		}
	}
	coefFrame<-as.data.frame(listForRow,optional=T)

	# Bind the two frames
	dataFrame<-rbind(dataFrame,coefFrame)
	
	while(depVar %in% row.names(dataFrame))
	{
		depVar<-paste(depVar,".",sep="")
	}
	# Assign the dependent Variable as the row Name
	row.names(dataFrame)[nrow(dataFrame)]<-depVar

		
	# Return the data Frame
	return(dataFrame)
}

multiplot<-function(data, titles, firstColumnAreNames=F,xlab="",ylab="", ylim=NA)
{
	styles<-c("solid","dashed","dotted")	
	colours<-c("black","blue","red","green")
	
	if(is.na(ylim))
	{
		ylim<-c(min(data[,(1+firstColumnAreNames):ncol(data)]),max(data[,(1+firstColumnAreNames):ncol(data)]))
	}
	
	if(firstColumnAreNames)
	{
		xlim<-c(data[1,1],data[nrow(data),1])
	}
	else
	{
		xlim<-c(1,nrow(data))	
	}
	
	legendStyles<-character(length(titles))
	legendCols<-character(length(titles))

	legendPosition<-"bottomright"
	
	for(index in (1+firstColumnAreNames):ncol(data))
	{
		legendStyles[index-firstColumnAreNames]<-styles[(index-1)%%length(styles)+1]
		legendCols[index-firstColumnAreNames]<-colours[(index-1)%%length(colours)+1]
		if(firstColumnAreNames)
		{
			x<-data[,1]
		}
		else
		{
			x<-1:nrow(data)
		}
		if((data[nrow(data),index]-ylim[1]) < 0.3*(ylim[2]-ylim[1]))
		{
			legendPosition<-"topright"
		}
		plot(x<-x,y<-data[,index],col=colours[(index-1)%%length(colours)+1],lty=styles[(index-1)%%length(styles)+1],type="l",xlab = xlab, ylab=ylab, xlim=xlim,ylim=ylim);
		par(new=T)
		par(xaxt='n')
		par(yaxt='n')
		xlab=""
		ylab=""
	}
	
	legend(legendPosition, titles,col=legendCols,lty=legendStyles)
	
	par(new=F)
	par(xaxt='s')
	par(yaxt='s')
}

plotSignificances<-function(data, significance, firstColumnAreNames=F)
{
	styles<-c("solid","dashed","dotted")	
	colours<-c("black","blue","red","green")
	
	ylim<-c(min(data[,(1+firstColumnAreNames):ncol(data)]),max(data[,(1+firstColumnAreNames):ncol(data)]))
	
	if(firstColumnAreNames)
	{
		xlim<-c(data[1,1],data[nrow(data),1])
	}
	else
	{
		xlim<-c(1,nrow(data))	
	}
	
	legendStyles<-character(ncol(data)-firstColumnAreNames)
	legendCols<-character(ncol(data)-firstColumnAreNames)
	
	for(index in (1+firstColumnAreNames):ncol(data))
	{
		par(new=T)
		par(xaxt='n')
		par(yaxt='n')
		
		legendStyles[index-firstColumnAreNames]<-styles[(index-1)%%length(styles)+1]
		legendCols[index-firstColumnAreNames]<-colours[(index-1)%%length(colours)+1]
		
		if(firstColumnAreNames)
		{
			x<-data[,1]
		}
		else
		{
			x<-1:nrow(data)
		}
		# If the significance only has one column it is not considered a frame, but a vector
		if(ncol(data)==2)
		{
			sigColumn<-significance
		}
		else
		{
			sigColumn<-significance[,index-firstColumnAreNames]
		}
		
		plot(x[sigColumn<.05],data[sigColumn<.05,index],col=colours[(index-1)%%length(colours)+1],type="p",xlab = "", ylab="", xlim=xlim,ylim=ylim);
	}
	
	par(new=F)
	par(xaxt='s')
	par(yaxt='s')
}

highlightSignificant<-function(outTab)
{
	for(r in 1:nrow(outTab))
	{
		for(c in 2:ncol(outTab))
		{
			if(((r %% 2) == 1) || (as.numeric(outTab[r,c]) > 0.05))
			{
				#cat(paste(r,",",c,"\n",sep=""))
				#if(class(outTab[r,c]) != "numeric")
				#{
				#	cat(paste("Not Numeric:",class(outTab[r,c]),"\n"))
				#}
				outTab[r,c]<-formatC(as.numeric(outTab[r,c]),format="f",digits=2)
			}
			else
			{
				outTab[r,c]<-as.character(paste("\\begin{em}\\textbf{",formatC(as.numeric(outTab[r,c]),format="f",digits=2),"}\\end{em}",sep=""))
			}
		}
	}
	return(outTab)
}


getRowsWithWealth<-function(allData)
{
	rows<-row.names(allData)[!(is.na(allData$Housing.Wealth)|is.na(allData$Financial.Wealth))]
	rows<-as.numeric(rows)
	extraRows<-numeric(0)
	for(index in rows){if(index > 1 && (allData$Country[index-1] == allData$Country[index])){extraRows<-c(extraRows,index-1)}}
	actualRows<-union(extraRows,rows)
	return(actualRows)
}

byCountryExcludeRegression<-function(formula, data, effect="twoways")
{
  # Performs a plm regression of the given formula on the given data, once for each country in the dataset,
  # with that country excluded. returns a dataframe with one row for each result
  results<-NA
  pVals<-NA
  failedCountries<-character(0)
  countries<-character(0)
  alphaSigs<-rep(0,2*length(c("None",unique(as.character(data$Country)))))
  index<-0
  for(country in c("None",unique(as.character(data$Country))))
	{
		index<-index+1
		singleMod<-NA
    singleData<-data[data$Country!=country,]
		singleMod<-plm(formula,data=singleData,index=c("Country","Year"),effect=effect)
		#print(paste(country,": ",singleMod$df.residual,sep=""))
		debugOut(country)
		coefs<-coeftest(singleMod,vcov.=myvcov)
		alphaSigs[2*index]<-waldtest(singleMod,update(singleMod,.~.-alpha_1-alpha_2-alpha_3-alpha_4-alpha_5-alpha_6-alpha_7),vcov=myvcov)[2,4]
		debugOut("Done!")
		if(!is.data.frame(results))
		{
			results<-data.frame(t(coefs[,1]))
			pVals<-data.frame(t(coefs[,4]))
			countries<-c(countries,country,"")
		}
		else
		{
			results<-rbind(results,data.frame(t(coefs[,1])))
			pVals<-rbind(pVals,data.frame(t(coefs[,4])))
			countries<-c(countries,country,"")
		}
	}
	
	fullResults<-data.frame(numeric(0))
	for(index in seq(nrow(results),1,-1))
	{
		fullResults<-rbind(pVals[index,],fullResults)
		fullResults<-rbind(results[index,],fullResults)
	}
	fullResults<-data.frame(Country=countries,fullResults)
	fullResults$AlphaSig<-alphaSigs
	return(fullResults)
}

byExcludedCountryTable<-function(formula, data, columnNames, caption, path="../tex/tables/", effect="twoways")
{
  # Performs the regression of formula on data with each country excluded in turn and outputs
  # the data to a tex file at the named path, called 'byExclucedCountry<caption>.tex'
  
  # Run the regressions
  res<-byCountryExcludeRegression(formula,data,effect)
  res<-res[,c(1:6,13:ncol(res))]
  # Create a table with the results
  outTab<-xtable(res,digits=2,caption=paste(caption," by EXCLUDED country",sep=""))
  
  # Highlight statistically significant items in the table
  outTab<-highlightSignificant(outTab)
  
  # Update the names of the table columns
  columnNames<-columnNames[c(1:7,14:length(columnNames))]
  names(outTab)[1:length(columnNames)]<- columnNames
  outTab<-fixColNames(outTab)
  # Write the table to the relevant file
  print(outTab, file=paste(path,"byExcludedCountry",gsub(" |\\(|\\)","",caption),".tex", sep = ""), size="scriptsize",include.rownames=F,floating.environment="sidewaystable",sanitize.text.function=function(x){x},hline.after=seq(0,nrow(res),2))
}

byCountryTable<-function(formula, data, caption, altFormula=NA)
{
  res<-byCountryRegression(formula, data, altFormula)
  outTab<-xtable(res,digits=2,caption=paste("Individual Country Results for ",caption,sep=""), label=paste("tab:byCountry",caption,sep=""))
  outTab<-highlightSignificant(outTab)
  outTab<-fixColNames(outTab)
  print(outTab, file=paste("../tex/tables/byCountry",caption,".tex",sep=""), size="scriptsize",include.rownames=F,floating.environment="sidewaystable",sanitize.text.function=function(x){x},hline.after=seq(0,nrow(res),2))
}

byCountryRegression<-function(formula, data, altFormula=NA)
{
  results<-NA
	pVals<-NA
	failedCountries<-character(0)
	countries<-character(0)
  
	for(country in unique(data$Country))
	{
    
    if(isDebug())
      print(country)
 
		singleMod<-NA
		tryCatch(singleMod<-dyn$lm(formula,ts(allData[allData$Country==country,])))
		if(any(is.na(singleMod$coefficients)))
		{
			print(paste("Failed for",country))
			failedCountries<-c(failedCountries,country)
			next
		}
   
    
		coefs<-tryCatch(coeftest(singleMod,vcov.=vcovHAC(singleMod)),error=function(e){
      print(paste("Failed for",country))
      NA
		})
        
    
    if(is.na(coefs))
  	{
			print(paste("Failed for",country))
			failedCountries<-c(failedCountries,country)
			next
		}
    
    if(!is.data.frame(results))
		{
			results<-data.frame(t(coefs[,1]))
			pVals<-data.frame(t(coefs[,4]))
			countries<-c(countries,country,"")
		}
		else
		{
			results<-rbind(results,data.frame(t(coefs[,1])))
			pVals<-rbind(pVals,data.frame(t(coefs[,4])))
			countries<-c(countries,country,"")
		}
	}
	if(!is.na(altFormula))
	{
		altResults<-pvcm(altFormula,data[!(data$Country %in% failedCountries),],model="within")
		if(max(abs(results[,2:ncol(results)]-altResults))>1e-10)
		{
			print("Results differ!")
			error("Results differ!")
		}

	}
	fullResults<-data.frame(numeric(0))
	for(index in seq(nrow(results),1,-1))
	{
		fullResults<-rbind(pVals[index,],fullResults)
		fullResults<-rbind(results[index,],fullResults)
	}
	fullResults<-data.frame(Country=countries,fullResults)
			
	return(fullResults)
}
