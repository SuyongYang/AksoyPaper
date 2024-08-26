install.packages('pder')
data("DemocracyIncome", package = "pder")
data("DemocracyIncome25", package = "pder")
library("plm")
pdim(DemocracyIncome)
head(DemocracyIncome, 4)

library(panelvar)
library(tidyverse)
library(haven)
Dat<-read_dta("../Data_FINAL05.dta")
Dat01<-Dat %>% filter(g7==1)
Dat02<-Dat01 %>% select(countrycode,year,gRGDP,DEPY, DEPO,SAV,CUR,RIR,gW)
Dat03<-as.matrix(Dat02)
Dat04<-as.data.frame(Dat03)

PVAR<-pvargmm(
  dependent_vars=c("gRGDP","DEPY","DEPO","SAV","CUR","RIR"),
  lags=1,
  predet_vars=c("gW"),
  #exog_vars=c("gW"),
  transformation = "fod",
  data=Dat04,
  panel_identifier = c("countrycode", "year"),
  steps=c("twostep"),
  system_instruments = T,
  system_constant = TRUE,
  #pca_instruments = FALSE,
  #pca_eigenvalue = 1,
  max_instr_dependent_vars=7,
  #max_instr_predet_vars,
  min_instr_dependent_vars = 2L,
  #min_instr_predet_vars = 1L,
  collapse = T,
  tol = 1e-09,
  #progressbar = TRUE
)
IR <- oirf(PVAR, n.ahead = 8)
GIR <- girf(PVAR, n.ahead = 8, ma_approx_steps= 8)
BS <- bootstrap_irf(PVAR, typeof_irf = c("OIRF"),
                                      n.ahead = 8,
                                      nof_Nstar_draws = 500,
                                      confidence.band = 0.95)
plot(GIR, BS)




PVAR_NoColapse<-pvargmm(
  dependent_vars=c("gRGDP","DEPY","DEPO","SAV","CUR","RIR"),
  lags=1,
  predet_vars=c("gW"),
  #exog_vars=c("gW"),
  transformation = "fod",
  data=Dat04,
  panel_identifier = c("countrycode", "year"),
  steps=c("twostep"),
  system_instruments = T,
  system_constant = TRUE,
  #pca_instruments = FALSE,
  #pca_eigenvalue = 1,
  max_instr_dependent_vars=7,
  #max_instr_predet_vars,
  min_instr_dependent_vars = 2L,
  #min_instr_predet_vars = 1L,
  collapse = F,
  tol = 1e-09,
  #progressbar = TRUE
)
# not working

data("abdata")
Arellano_Bond_1991_table4b <-pvargmm(
  dependent_vars = c("n"),
  lags = 2,
  exog_vars = c("w", "wL1", "k", "ys", "ysL1",
                "yr1979", "yr1980", "yr1981", "yr1982",
                "yr1983", "yr1984"),
  transformation = "fod",
  data = abdata,
  panel_identifier = c("id", "year"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = T)

score<- read.table('https://home.ewha.ac.kr/~msoh/Bayesianbook/score.txt',head=T)
tsco<-as.data.frame(cbind(score$ID,score$mid))
names(tsco)<-c("ID","mid")
score[score$Total>70,]
library(tidyverse)
score%>%filter(Total>70)
write.csv(score,file="score.csv",row.names=F,quote=F)
t.test(score$mid,score$final)
hist(score$final)

theta<-seq(0,1,length=1000)
ftheta<-dbeta(theta,3,12)
plot(theta,ftheta,type='l',xlab='theta',ylab='f(theta)')
abline(v=qbeta(c(0.025,0.975),3,12))
ftheta2<-dbeta(theta,3,9)
lines(theta,ftheta2,lty=2)
Sigma<-matrix(c(1,0.7,0.7,1),nrow=2,ncol=2,byrow=T)
R=chol(Sigma)
t(R)%*%R

plot(dgamma(theta,1,2),type='b',col='steelblue')
lines(dexp(theta,2),type='h',col='red')
