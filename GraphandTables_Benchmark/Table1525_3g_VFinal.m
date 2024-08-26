% Demographic Structure and Macroeconomic Trends - Aksoy, Basso, Smith, Grasl 
% This code produces Table 2 in the paper. Output Projections

clear
startyear = 2010;
nv = 6;
ng = 3;
load A1.txt;
A1=A1;
load shortD.txt;
D = shortD(:,1:end-1);
load waldprep.csv;
vcovm3g = waldprep;

load gradnum
DLR3g = inv(eye(nv)-A1)*D;

dvcov3g = zeros(nv,ng-1);
for row = 1:nv
    for col = 1:ng-1
dvcovP1=transpose(gradnum(:,row,col))*vcovm3g;
dvcov3g(row,col)=dvcovP1*gradnum(:,row,col);
    end
end

load ProjecPopData.csv;
load HistPopData.txt;
%get weights
mh = size(HistPopData,1);
mp = size(ProjecPopData,1);
Popweights = zeros(mh+mp,ng);
for i = 1:mh
    sumpop = sum(HistPopData(i,1:end));
    Popweights(i,1) = sum(HistPopData(i,1:4))/sumpop;
    Popweights(i,2) = sum(HistPopData(i,5:12))/sumpop;
    Popweights(i,3) = sum(HistPopData(i,13:end))/sumpop;
end
for i = 1:mp
    sumpop = sum(ProjecPopData(i,1:end));
    Popweights(mh+i,1) = sum(ProjecPopData(i,1:4))/sumpop;
    Popweights(mh+i,2) = sum(ProjecPopData(i,5:12))/sumpop;
    Popweights(mh+i,3) = sum(ProjecPopData(i,13:end))/sumpop;
end

load EconData.csv;

sy = startyear - 2000+1;
for i= 1:1:22;

DW(:,:) = [Popweights(sy+16*(i-1):16+16*(i-1),:); Popweights(mh+1+85*(i-1):mh+85+85*(i-1),:)]; 

DW_3g(:,1) = DW(:,1) - DW(:,3);
DW_3g(:,2) = DW(:,2) - DW(:,3);
dDW_3g = diff(DW_3g);

s1 = 5;
s2 = 15;
dDW_3g_per = sum(dDW_3g(1:s1,1:2))';
temp = DLR3g*dDW_3g_per;
dY09(i) = temp(1);
dDW_3g_per = sum(dDW_3g(1:s2,1:2))';
temp = DLR3g*dDW_3g_per;
dY19(i) = temp(1);
dDW_3g_per = sum(dDW_3g(s1+1:s2,1:2))';
temp= DLR3g*dDW_3g_per;
dY(i) = temp(1);
std_dY = (dvcov3g*(dDW_3g_per.^2)).^0.5;
std_dY_v2 = (dvcov3g*(sum(dDW_3g(s1:s2,1:2).^2))').^0.5;

zlim  = -dY(i)/std_dY(1);
zlim_v2  = -dY(i)/std_dY_v2(1);
test(i) = normcdf(-zlim,0,1);
test_v2(i) = normcdf(-zlim_v2,0,1);
if test(i)<0.05 
    sigg(i) = 5;
elseif test(i)<0.1
    sigg(i) = 10;
else
    sigg(i) = 50;
end

if test_v2(i)<0.01 
    sigg_v2(i,1) = 1;
elseif test<0.05
    sigg_v2(i,1) = 5;
elseif test<0.1
    sigg_v2(i,1) = 10;
else
    sigg_v2(i,1) = 50;
end

DataCountry = EconData(1+45*(i-1):45+45*(i-1),:);
logOutput = log(DataCountry(1:40,3));
Outputg = diff(logOutput);
y(i) = nanmean(Outputg); 
logOutput = log(DataCountry(1:40,3));
Outputg = diff(logOutput);
y2(i) = nanmean(Outputg);%Growth
if i ==8
    y(i) = 0.0201;
    y2(i) = 0.0201; %put manually number for Germany. 0.01434 for mean (1990-2015) and 0.0201 for mean (1970-2015)
end
Y09(i) = y(i)+dY09(i);
Y19(i) = Y09(i)+dY(i);
end

FinalTable = [ y' Y09' Y19' dY' test'];
save('Pred1525.txt','FinalTable','-ascii')









