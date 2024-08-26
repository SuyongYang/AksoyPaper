DevelopingNotAged_L1_mod_RIR_unbalanced_depodepy
%AIC
AIC = [log(det((y_all)^-1*S1))+2/y_all*y_vars*mk];
%BIC
BIC = [log(det((y_all)^-1*S1))+log(y_all)/y_all*y_vars*mk];
%HQ
HQ = [log(det((y_all)^-1*S1))+2*log(log(y_all))/y_all*y_vars*mk];
save tot_developingNotAged_depodepy_UBal.mat AIC BIC HQ;


DevelopingNotAged_L2_mod_RIR_unbalanced_depodepy
load tot_developingNotAged_depodepy_UBal.mat AIC BIC HQ
AIC=AIC;
BIC=BIC;
HQ=HQ;
%AIC
AIC = [AIC;log(det((y_all)^-1*S1))+2/y_all*y_vars*mk];
%BIC
BIC = [BIC;log(det((y_all)^-1*S1))+log(y_all)/y_all*y_vars*mk];
%HQ
HQ = [HQ;log(det((y_all)^-1*S1))+2*log(log(y_all))/y_all*y_vars*mk];
save tot_developingNotAged_depodepy_UBal.mat AIC BIC HQ;


DevelopingNotAged_L3_mod_RIR_unbalanced_depodepy
load tot_developingNotAged_depodepy_UBal.mat AIC BIC HQ
AIC=AIC;
BIC=BIC;
HQ=HQ;

%AIC
AIC = [AIC;log(det((y_all)^-1*S1))+2/y_all*y_vars*mk];
%BIC
BIC = [BIC;log(det((y_all)^-1*S1))+log(y_all)/y_all*y_vars*mk];
%HQ
HQ = [HQ;log(det((y_all)^-1*S1))+2*log(log(y_all))/y_all*y_vars*mk];
save tot_developingNotAged_depodepy_UBal.mat AIC BIC HQ;

DevelopingNotAged_L4_mod_RIR_unbalanced_depodepy
load tot_developingNotAged_depodepy_UBal.mat AIC BIC HQ
AIC=AIC;
BIC=BIC;
HQ=HQ;

%AIC
AIC = [AIC;log(det((y_all)^-1*S1))+2/y_all*y_vars*mk];
%BIC
BIC = [BIC;log(det((y_all)^-1*S1))+log(y_all)/y_all*y_vars*mk];
%HQ
HQ = [HQ;log(det((y_all)^-1*S1))+2*log(log(y_all))/y_all*y_vars*mk];
save tot_developingNotAged_depodepy_UBal.mat AIC BIC HQ;
