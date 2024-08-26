fixFileName<-function(name)
{
  name<-gsub(" ","_",name)
  name
}

isDebug<-function()
{
  return(exists(x="DEBUG") && !is.null(DEBUG) && DEBUG)
}
debugOut<-function(string)
{
	if(isDebug())
		print(string)
}

fixRowNames<-function(table)
{
  for(rowInd in 1:nrow(table))
  {
    row.names(table)[rowInd]<-fixName(row.names(table)[rowInd])
  }
  
  table
}
  
fixColNames<-function(table)
{
  for(colInd in 1:ncol(table))
  {
    colnames(table)[colInd]<-fixName(colnames(table)[colInd])
  }
  
  table
}

fixName<-function(nameIn)
{
  name<-nameIn
  name<-gsub("[.]"," ",name)
  #name<-gsub("[(),]"," ",name)
  #name<-gsub("[^_]1"," ",name)
  name<-gsub("log[(]([^)]*)[)]","\\1",name)
  
  # Get rid of diff first
  name<-sub("diff.*RGDPL[^,]*, 1[)]","y",name)
  name<-sub("diff.*Hours.Total[^,]*, 1[)]","H",name)
  
  # If this one is still here after the above we are running a levels model
  name<-sub("Hours.Total","H",name)
  name<-gsub("Oil.Price","POIL",name)
  name<-gsub("CB.Rate","R",name)
  name<-gsub("infl","\\\\pi",name)
  name<-gsub("Savings.Rate","S",name)
  name<-gsub("Investment.Rate","I",name)
  name<-gsub("pi","\\\\pi",name)
  # Single lags need not specify the length of lag
  name<-gsub("lag[(]([^),]*),[ ]*([1-9])[)]","$\\1_{t-\\2}$",name)
  name<-gsub("lag[(](.*)[)]","$\\1_{t-1}$",name)
  
  name<-gsub("diff ","$\\\\Delta$",name)
  name<-gsub("K.Total","k",name)
  
  name<-gsub("X Intercept","const.",name)
  name<-gsub("trend","t",name)
  name<-gsub("alpha_([1-9])","$\\\\delta_\\1$",name)
  
  name<-sub("^y$","$y$",name)
  name<-sub("^i$","$I$",name)
  name<-sub("^s$","$S$",name)
  name<-sub("^h$","$H$",name)
  name<-sub("^r$","$R$",name)
  name<-sub("^r$","$R$",name)
  name<-sub("^pi$","$\\\\pi$",name)  
  name
}

logLik.plm<-function(model)
{
	# Returns the maximised log likelihood of a model
	N  <- nrow(model$model)
	-N/2*(log(2*pi)+1+log(sum(model$residuals^2)/N))
}

criterion<-function(...)
{
	# Calculates the Schwartz-Bayesian Information criterion for one or more models (under the assumption that multiple models are part of the same estimation, e.g. breakpoint)
	# Args:
	#	...: List of models
	models<-as.vector(list(...))
  
  	# Determine the total number of observations
	N<-sum(sapply(models,function(x){nrow(x$model)}))
  
  	# The number of observations minus the number of residual degrees of freedom is the number of estimated parameters  
	K<-(N-sum(sapply(models,function(x){x$df.residual})))
  
  	# The SBC
	structure(.Data=sum(sapply(models,logLik))-K/2*log(N),N=N,K=K)
}

# Thanks to Achim Zeileis (online)...
# And the author's of xtable
xtable.coeftest <- function(x, ...) {
   xtable:::xtable.summary.lm(list(coef = unclass(x)), ...)
}

multiResultTable<-function(... )
{
  Rs<-c()
  alphaSigs<-c()
  nobs<-c()

  input.list<-list(...)
  combined<-data.frame()
  for(currentModel in input.list)
  {
    debugOut("Here mRT")
  	  # Create the same table as LM summary one from coeftest results
      x <- data.frame(unclass(coeftest(currentModel,vcov.=vcovHC)), check.names = FALSE)
    
      x[,2]<-formatC(x[,2],format="f",digits=2)
      x[,1]<-formatC(x[,1],format="f",digits=2)
   
      for(rowInd in 1:nrow(x))
      {
          x[rowInd,2]<-paste(x[rowInd,2],switch(1+(x[rowInd,4]>.05)," *","$\\;\\;\\;$"),sep="")
      }
      debugOut("Almost There!")
      if(nrow(combined)==0)
      {
        combined<-x[,c(1,2)]
      }
      else
      {
        combined<-cbind(combined,x[,c(1,2)])
      }
      Rs<-c(Rs,formatC(summary(currentModel)$r.squared[1],format="f",digits=2),"")
    debugOut("There")
      .env = environment()
    model = currentModel
    alphaSigs<-c(alphaSigs,formatC(waldtest(currentModel,as.formula(paste(c(".~.",demoVars),sep="-",collapse="-")),vcov=myvcov)[2,4],format="f",2),"")
      nobs<-c(nobs,as.character(nrow(currentModel$model)),"")
  }
  debugOut(Rs)
  combined<-rbind(combined,Rs)
  rownames(combined)[nrow(combined)]<-"$R^2$"
  combined<-rbind(combined,alphaSigs)
  rownames(combined)[nrow(combined)]<-"$\\Pr(\\delta_j=0)$"
  combined<-rbind(combined,nobs)
  rownames(combined)[nrow(combined)]<-"obs."
  debugOut(combined)
  xtable(combined, digits=2,align=rep("r", 1+ncol(combined)))
}

forCountry<-function(dataSet,...)
{
	dataSet[dataSet$Country %in% ...,]
}

testElipsis<-function(...)
{
  switch(list(...),print(T),print(F))
  for(param in names(list(...)))
  {
    print(param)
  }
}

printTable<-function(table, caption, file,...)
{
  print(xtable(fixRowNames(fixColNames(table)),caption=caption,...),
        sanitize.text.function=function(name){name},
        file=file)
}