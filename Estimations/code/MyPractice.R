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
library(tidyverse)
last_obs <- Pat_panel %>%
    group_by(Country) %>%
    filter(!is.na(PATAPPRESID)) %>%  # Remove rows where Year is NA
    filter(as.numeric(Year) == max(as.numeric(Year)) )
str(Pat_panel)
library(scales)
log20_trans <- function() {
  trans_new("log20", 
            transform = function(x) log(x, base = 20), 
            inverse = function(x) 20^x, 
            breaks = log_breaks(base = 20)
            )
}

ggplot(Pat_panel, aes(x = Year, y = PATAPPRESID, color = Country, group = Country)) +
    geom_line() +
    geom_point() +
    geom_text(data = last_obs, aes(label = Country), 
            hjust = -0.1, 
            vjust = -0.5,
            nudge_x = .2,
            nudge_y = .1,
            check_overlap = T,
            size = 2,
            show.legend = FALSE) +
    #scale_y_continuous(trans = log20_trans()) +
    scale_y_log10() +  # Apply logarithmic scale to y-axis
    labs(title = "Patents series by Country",
         x = "Year",
         y = "Patents") +
    theme_minimal()
##############################################

# Install the sympy package if you haven't already
library(caracas)
library(reticulate)
library(Ryacas)

#use_python("C:/Users/USER/AppData/Local/Programs/Python/Python312/python.exe", required = TRUE)
#py_config()
#sympy <- import("sympy")

library(reticulate)
library(magrittr)  # For the `%>%` pipe operator

# Not working below....
runDLRsym <- function(nv, ng) {
  
  # Define symbolic matrices
  nv=6;ng=3
  A <- matrix(NA,nv,nv)
  for(row in 1:nv){
    for(col in 1:nv){
      tem = sprintf("a%d%d",row,col)
      A[row,col] = tem
    }
  }
  #A = as_y(A)
  
  
  D <- matrix(NA,nv,(ng-1))
  for(row in 1:nv){
    for(col in 1:(ng-1)){
      tem = sprintf("d%d%d",row,col)
      D[row,col] = tem
    }
  }
  
  identity_matrix = diag(nv)
  Inner = matrix(NA,nv,nv)
  for(i in 1:nv){
    for(j in 1:nv){
      Inner[i,j] = sprintf("%d-%s",identity_matrix[i,j],A[i,j])
    }
  }
  
  # outer(0:3, 1:4, "-") + diag(2:5)
  
  # Compute the inverse of C
  inverseC <- sympy$Inverse(inner)

  # Calculate DLRsym
  DLRsym <- inverseC %*% D
  DLRsym <- sympy$Matrix(DLRsym)
  # Save each element of DLRsym to a separate file
  for (row in 1:nv) {
    for (col in 1:(ng-1)) {
      name <- sprintf("DLRsym%d%d.txt", row , col)
      varr <- DLRsym[row , col]
      writeLines(as.character(varr), con = name)
    }
  }

  # Print completion message
  print("Execution completed.")
}

A <- diag(4)
Ayac <- as_y(A)
y_print(Ayac)
B <- A
B[2, 2] <- "-t"
Byac <- as_y(B)
Byac
y_print(Byac)

sum(Ayac, Byac)

runDLRsym(6,3)

library(Ryacas)

?sum()

ySym("f'a{1}{1}'")
?ysym
