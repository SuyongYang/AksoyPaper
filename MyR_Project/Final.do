use "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\DATA_FINAL01.dta"
help pvar
help var
help svar
import excel "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\DATA_FINAL02.xls", sheet("Sheet1") clear
import excel "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\DATA_FINAL02.xls", sheet("Sheet1") firstrow clear
save "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\DATA_FINAL02.dta"
tsset countrycode year
gen lnlabor = ln(labor)
gen glabor = d.lnlabor
gen lnrgdp = ln(rgdp)
gen grgdp = d.lnrgdp
pvar glabor grgdp depr depy sav_rate ca_rate realint
pvargranger
pvar glabor grgdp depy depr sav_rate ca_rate realint
pvarirf,oirf mc(200) byoption(yrescale)
help dummy
pvarsoc
pvarsoc glabor grgdp depy depr sav_rate ca_rate realint
help pvar
pvar glabor grgdp depy depr sav_rate ca_rate realint,lag(1/4)
pvar glabor grgdp depy depr sav_rate ca_rate realint,lags(1/4)
pvar glabor grgdp depy depr sav_rate ca_rate realint,lag(1/4)
pvar glabor grgdp depy depr sav_rate ca_rate realint,lags(4)
pvar glabor grgdp depy depr sav_rate ca_rate realint,lags(3)
pvar glabor grgdp depy depr sav_rate ca_rate realint,lags(2)
pvar glabor grgdp depy depr sav_rate ca_rate realint,lags(1)
gen laborPopRatio=labor /pop
pvar laborPopRatio grgdp depy depr sav_rate ca_rate realint,lags(1)
pvarirf,oirf mc(200) byoption(yrescale)
pvar glabor grgdp depy depr sav_rate ca_rate realint,lags(1)
pvarirf,oirf mc(200) byoption(yrescale)
pvarirf,oirf mc(200) byoption(yrescale)
pvar grgdp depy depr sav_rate ca_rate realint,lags(1)
pvarirf,oirf mc(200) byoption(yrescale)
pvar  depy depr grgdp sav_rate ca_rate realint,lags(1)
pvarirf,oirf mc(200) byoption(yrescale)
import delimited "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\VAR_data04.csv", numericcols(5 6 7 8 9 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31) clear 
import excel "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\VAR_data05.xlsx", sheet("VAR_data04") firstrow clear
destring RGDP Sav_Rate CA_Rate RealInt Countrycode, replace force
destring K L M N O P Q R S T U V W X Y Z AA AB AC AD AE, replace force
save "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\DATA_FINAL03.dta"
gen lnlabor = ln(labor )
tsset Countrycode year
import delimited "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\VAR_data04.csv", numericcols(5 6 7 8) clear 
tsset Countrycode year
tsset countrycode year
sum(v10:v11)
gen depy = (v10+v11+v12+v13)/(v14+v15+v16+v17+v18+v19+v20+v21)
import delimited "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\VAR_data04.csv", numericcols(5 6 7 8) clear 
destring v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30, replace force
gen depy = (V10+V11+V12+V13)/(V14+V15+V16+V17+V18+V19+V20+V21)
gen depy = (v10+v11+v12+v13)/(v14+v15+v16+v17+v18+v19+v20+v21)
gen depr = (v22+v23+v24+v25+v26+v27+v28+v29+v30)/(v14+v15+v16+v17+v18+v19+v20+v21)
gen pop = (v10+v11+v12+v13)+(v14+v15+v16+v17+v18+v19+v20+v21)+(v22+v23+v24+v25+v26+v27+v28+v29+v30)
gen labor = (v14+v15+v16+v17+v18+v19+v20+v21)
gen lnlabor = ln(labor)
tsset countrycode year
gen glabor = d.lnlabor
gen g7=0
gen g7=1 if (countrycode ==840 |countrycode ==392 |countrycode ==276 |countrycode ==826 |countrycode ==250 |countrycode ==124 |countrycode == 380 )
replace g7=1 if (countrycode ==840 |countrycode ==392 |countrycode ==276 |countrycode ==826 |countrycode ==250 |countrycode ==124 |countrycode == 380 )
pvargranger
gen lnrgdp = ln(rgdp)
gen grgdp = d.lnrgdp
pvar glabor grgdp depy depr sav_rate ca_rate realint
pvarirf,oirf mc(200) byoption(yrescale)
pvar glabor grgdp depy depr sav_rate ca_rate realint if g7==1
pvarirf,oirf mc(200) byoption(yrescale)
save "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\DATA_FINAL04.dta"
