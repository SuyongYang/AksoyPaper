DevelopingAged_L1_mod_RIR_some_balanced_dep
%AIC
AIC = [log(det((y_all)^-1*S1))+2/y_all*y_vars*mk];
%BIC
BIC = [log(det((y_all)^-1*S1))+log(y_all)/y_all*y_vars*mk];
%HQ
HQ = [log(det((y_all)^-1*S1))+2*log(log(y_all))/y_all*y_vars*mk];
save tot_developingAged_dep_Bal.mat AIC BIC HQ;


DevelopingAged_L2_mod_RIR_some_balanced_dep
load tot_developingAged_dep_Bal.mat
AIC=AIC;
BIC=BIC;
HQ=HQ;
%AIC
AIC = [AIC;log(det((y_all)^-1*S1))+2/y_all*y_vars*mk];
%BIC
BIC = [BIC;log(det((y_all)^-1*S1))+log(y_all)/y_all*y_vars*mk];
%HQ
HQ = [HQ;log(det((y_all)^-1*S1))+2*log(log(y_all))/y_all*y_vars*mk];
save tot_developingAged_dep_Bal.mat AIC BIC HQ ;


DevelopingAged_L3_mod_RIR_some_balanced_dep
load tot_developingAged_dep_Bal.mat
AIC=AIC;
BIC=BIC;
HQ=HQ;

%AIC
AIC = [AIC;log(det((y_all)^-1*S1))+2/y_all*y_vars*mk];
%BIC
BIC = [BIC;log(det((y_all)^-1*S1))+log(y_all)/y_all*y_vars*mk];
%HQ
HQ = [HQ;log(det((y_all)^-1*S1))+2*log(log(y_all))/y_all*y_vars*mk];
save tot_developingAged_dep_Bal.mat AIC BIC HQ ;

DevelopingAged_L4_mod_RIR_some_balanced_dep
load tot_developingAged_dep_Bal.mat
AIC=AIC;
BIC=BIC;
HQ=HQ;

%AIC
AIC = [AIC;log(det((y_all)^-1*S1))+2/y_all*y_vars*mk];
%BIC
BIC = [BIC;log(det((y_all)^-1*S1))+log(y_all)/y_all*y_vars*mk];
%HQ
HQ = [HQ;log(det((y_all)^-1*S1))+2*log(log(y_all))/y_all*y_vars*mk];
save tot_developingAged_dep_Bal.mat AIC BIC HQ ;