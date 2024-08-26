% Demographic Structure and Macroeconomic Trends - Aksoy, Basso, Smith, Grasl 
% This code does the Waldtest for the benchmark specification


clear
nv = 6;
ng = 3;
mm = nv*nv + nv*(ng-1);
load A1.txt;
A1=A1;
load shortD.txt;
D = shortD(:,1:end-1);
load waldprep.csv;
vcovm3g = waldprep;

load gradnum
DLR_est = inv(eye(nv)-A1)*D;
DLR3g = inv(eye(nv)-A1)*shortD;

%Tests beta1 beta2
dvcov3g = zeros(nv,ng-1);
for row = 1:nv
    for col = 1:ng-1
dvcovP1=transpose(gradnum(:,row,col))*vcovm3g;
dvcov3g(row,col)=dvcovP1*gradnum(:,row,col); %good
    end
end
Waldstat = DLR_est.*(dvcov3g.^-1).*DLR_est %ok...
Waldstattest = 1-chi2cdf(Waldstat,1)
%Tests beta3
for row = 1:1:nv;
    for i = 1:1:mm
        gradnum_sum(i,row) = - sum(gradnum(i,row,:));      
    end
end
for row = 1:nv
dvcovP1=transpose(gradnum_sum(:,row))*vcovm3g;
dvcovsum(row)=dvcovP1*gradnum_sum(:,row);
end
DLR_estd8 = -sum(DLR_est,2);
Waldstatd8 = DLR_estd8.*(dvcovsum'.^-1).*DLR_estd8
Waldstatd8test = 1-chi2cdf(Waldstatd8,1)
%Tests beta1, beta2, beta3
clear dvcov dvcovP1 tempgradnum
for row = 1:nv
tempgradnum(:,:) = gradnum(:,row,:);
dvcovP1=transpose(tempgradnum)*vcovm3g;
dvcov=dvcovP1*tempgradnum;
WaldJoin3g(row,:) = DLR_est(row,:)*(dvcov^-1)*DLR_est(row,:)';
end
WaldJoin3g
Waldjoin3gtest = 1-chi2cdf(WaldJoin3g,2)
clear dvcov dvcovP1 tempgradnum
for row = 1:nv
tempgradnum(:,:) = [gradnum(:,row,2) gradnum_sum(:,row)] ;
dvcovP1=transpose(tempgradnum)*vcovm3g;
dvcov=dvcovP1*tempgradnum;
WaldJoin3g(row,:) = DLR_est(row,:)*(dvcov^-1)*DLR_est(row,:)';
end
WaldJoin3g
Waldjoin3gtest2 = 1-chi2cdf(WaldJoin3g,3)


clear dvcovP1
gradnum3g_dif(:,:) = gradnum(:,:,2)- gradnum_sum(:,:);% + gradnum(i,5,7)  
for row = 1:nv
dvcovP1=transpose(gradnum3g_dif(:,row))*vcovm3g;
dvcov_dif(row)=dvcovP1*gradnum3g_dif(:,row);
end
DLR_est_dif = DLR_est(:,2)-DLR_estd8(:);
Waldstat3g_dif = DLR_est_dif.*(dvcov_dif'.^-1).*DLR_est_dif
Waldstat3g_diftest = 1-chi2cdf(Waldstat3g_dif,1)

gradnum3g_difall(:,:) = - gradnum(:,:,1) + gradnum(:,:,2)- gradnum_sum(:,:);
clear dvcovP1
for row = 1:nv
dvcovP1=transpose(gradnum3g_difall(:,row))*vcovm3g;
dvcov_difall(row)=dvcovP1*gradnum3g_difall(:,row);
end
DLR_est_difall = - DLR_est(:,1) + DLR_est(:,2)-DLR_estd8(:);
Waldstat3g_difall = DLR_est_difall.*(dvcov_difall'.^-1).*DLR_est_difall
Waldstat3g_difalltest = 1-chi2cdf(Waldstat3g_difall,1)

Results = [DLR3g Waldstattest Waldstatd8test Waldjoin3gtest Waldstat3g_diftest Waldstat3g_difalltest ]
save('Results.txt','Results','-ascii')
