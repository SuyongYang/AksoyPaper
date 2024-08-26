use "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\DATA_FINAL06.dta"
bvar DEP lnrgdpCap SAV CUR RIR if g7==1
bayes:var DEP lnrgdpCap SAV CUR RIR if g7==1
use greensolow
describe
tsset v1
bayes:var DEP lnrgdpCap SAV CUR RIR if g7==1
bayesvarstable
bayes, mcmcsize(1000) rseed(17): var Dgdp Dco2
bayes, mcmcsize(1000) rseed(17): var DEP lnrgdpCap SAV CUR RIR if g7==1
bayesvarstable
bayesirf
 bayesirf create birf1, set(birf)
 bayes, saving(bsim, replace)
 bayesirf create birf1, set(birf)
bayesirf graph irf
bayesirf graph irf, byoption(yrescale)
bayesirf graph oirf
bayesirf table oirf, impulse(Dep)
bayesirf table oirf, impulse(DEP)
gen y1947=0
replace y1947 = 1 if year==1947
do "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\Final.do"
do "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\Final.do"
save "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\DATA_FINAL06.dta", replace
do "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\Final.do"
tab year, gen(y_)
use "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\DATA_FINAL06.dta", clear
drop y1947
save "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\DATA_FINAL06.dta", replace
tab year
tab year, gen(year)
gen US=0
replace US = 1 if countrycode ==840
gen UK=0
replace UK = 1 if countrycode ==826
do "C:\Users\user\AppData\Local\Temp\STDf34_000000.tmp"
save "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\DATA_FINAL06.dta", replace
tsset v1
bayes: var DEP lnrgdpCap SAV CUR RIR if g7==1, exog(year1 year2 US UK FR)
bayes, mcmcsize(1000) rseed(17): var DEP lnrgdpCap SAV CUR RIR if g7==1, exog(year1 year2 US UK FR)
 bayes, saving(bsim, replace)
 bayesirf create birf1, set(birf)
 bayesirf create birf2, set(birf)
bayesirf graph oirf, impulse(DEP)
bayes: var DEP lnrgdpCap SAV CUR RIR if g7==1, exog(year1 year2 year3 year4 year5 year6 year7 year8 year9 year10 year11 year12 year13 year14 year15 year16 year17 year18 year19 year20 year21 year22 year23 year24 year25 year26 year27 year28 year29 year30 year31 year32 year33 year34 year35 year36 year37 year38 year39 year40 year41 year42 year43 year44 year45 year46 year47 year48 year49 year50 year51 year52 year53 year54 year55 year56 year57 year58 year59 year60 year61 year62 year63 year64 year65 year66 year67 year68 year69 year70 year71 year72 year73 year74 US UK FR JP IT GER CA)
bayes, mcmcsize(1000) rseed(17): var DEP lnrgdpCap SAV CUR RIR if g7==1, exog(year1 year2 year3 year4 year5 year6 year7 year8 year9 year10 year11 year12 year13 year14 year15 year16 year17 year18 year19 year20 year21 year22 year23 year24 year25 year26 year27 year28 year29 year30 year31 year32 year33 year34 year35 year36 year37 year38 year39 year40 year41 year42 year43 year44 year45 year46 year47 year48 year49 year50 year51 year52 year53 year54 year55 year56 year57 year58 year59 year60 year61 year62 year63 year64 year65 year66 year67 year68 year69 year70 year71 year72 year73 year74 US UK FR JP IT GER CA)
 bayes, saving(bsim, replace)
 bayesirf create birf3, set(birf)
bayesirf graph oirf, impulse(DEP)
bayesirf graph oirf, impulse(DEP)
bayesirf table oirf, impulse(DEP)
save "C:\Users\user\Documents\PhD thesis\Aksoy_ Replication practice\data\ReplicationFiles_Empirical\BenchmarkModel\MyR_Project\DATA_FINAL06.dta", replace
