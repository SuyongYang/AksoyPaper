% Demographic Structure and Macroeconomic Trends - Aksoy, Basso, Smith, Grasl 
% This code produces Figure 2 in the paper (also A.1 in the online Appendix). Projections for output,
% investment, savings and interest rates


clear
nv = 6;
ng = 3;
% load A12ways.txt;
% A1=A12ways;
% load shortD2ways.txt;
% D = shortD2ways(:,1:end-1);
% load waldprep2ways.csv;
% vcovm3g = waldprep2ways;
% load gradnum_2ways

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

Countries = fileread('listcountries.txt');

% Australia	1
% Austria	2
% Belgium	3
% Canada	4
% Denmark	5
% Finland	6
% France	7
% Germany	8
% Greece	9
% Iceland	10
% Ireland	11
% Italy	12
% Japan	13
% Netherlands	14
% New Zealand	15
% Norway	16
% Portugal	17
% Spain	18
% Sweden	19
% Switzerland	20
% United Kingdom	21
% United States of America	22


%load DataGRR.csv;
%for i = 1:1:23;
%Countries(1+15*(i-1):15+15*(i-1))
%end
TT = 20; %years of projection
vecpos = [22 13 12 7]; %US Japan Italy France
vecpos2 = [4 1 9 18]; % Canada Australia Greece Spain
%1 gdp, 2 inv, 3 sav, 4 hours, 5 rr, 6 inf
var1 = 1; %put 1 for outut or 2 for investment 
var2 = 5; %put 5 for real rates or 3 for savings
var3 = 2; %put 5 for real rates or 3 for savings
var4 = 3; %put 5 for real rates or 3 for savings
varname = ['Output Growth'; 'Investment   '; 'Savings      '; 'Hours        '; 'Interest Rate'];
grop = 1;
flag = 1; %put one to include line with intial value zero otherwise
figure
for j = 1:1:4;
if grop == 1;
i = vecpos(j);
else
i = vecpos2(j);
end

DW(:,:) = [Popweights(1+16*(i-1):16+16*(i-1),:); Popweights(mh+1+85*(i-1):mh+85+85*(i-1),:)]; 

DW_3g(:,1) = DW(:,1) - DW(:,3);
DW_3g(:,2) = DW(:,2) - DW(:,3);


dDW_3g = diff(DW_3g);

dY = zeros(nv,TT+1);
std_dY = zeros(nv,TT+1);
dYCI1 = zeros(nv,TT+1);
dYCI2 = zeros(nv,TT+1);
dYCIin1 = zeros(nv,TT+1);
dYCIin2 = zeros(nv,TT+1);
std_dY_v2 = zeros(nv,TT+1);
dYCI1_v2 = zeros(nv,TT+1);
dYCI2_v2 = zeros(nv,TT+1);

z  = 1.28;
z2 = 0.85;
s = 2;
dY(:,s) = DLR3g*dDW_3g(s+10,1:2)';
std_dY(:,s) = (dvcov3g*(dDW_3g(s+10,1:2).^2)').^0.5;
dYCI1(:,s) = dY(:,s) + z*std_dY(:,s);
dYCI2(:,s) = dY(:,s) - z*std_dY(:,s);
dYCIin1(:,s) = dY(:,s) + z2*std_dY(:,s);
dYCIin2(:,s) = dY(:,s) - z2*std_dY(:,s);
for t = s+1:1:TT+1;
    dY(:,t) = DLR3g*sum(dDW_3g(s+10:t+10,1:2))';
    std_dY(:,t) = (dvcov3g*(sum(dDW_3g(s+10:t+10,1:2)).^2)').^0.5;
    dYCI1(:,t) = dY(:,t) + z*std_dY(:,t);
    dYCI2(:,t) = dY(:,t) - z*std_dY(:,t);
    dYCIin1(:,t) = dY(:,t) + z2*std_dY(:,t);
    dYCIin2(:,t) = dY(:,t) - z2*std_dY(:,t);
end


DataCountry = EconData(1+45*(i-1):45+45*(i-1),:);
if var1 == 1
logOutput = log(DataCountry(1:40,3));
Outputg = diff(logOutput);
y1 = nanmean(Outputg);%Growth
if i ==8 %Germany
    y1 =  0.021;
end
elseif var1 == 2
y1 = nanmean(DataCountry(1:40,1)); %Investment
end
if var2 == 5
y2 = nanmean(DataCountry(1:40,4));%Real rate
if i ==8 %Germany
    y2 =  0.012;
end
elseif var2 == 3
y2 = nanmean(DataCountry(1:40,2)); %Saving Rate
end
y3 = nanmean(DataCountry(1:40,1)); %Investment
if i ==8 %Germany
    y3 =  0.229;
end
y4 = nanmean(DataCountry(1:40,2)); %Savings
if i ==8 %Germany
    y4 =  0.084;
end
s = 1;
TT = TT+1;
tau = 2010+TT;
t = 2010:1:2010+TT-1;
shadedarea11(j,1:TT-s+1) = dYCI1(var1,s:TT)+y1;
shadedarea12(j,1:TT-s+1) = dYCI2(var1,s:TT)+y1;
shadedareain11(j,1:TT-s+1) = dYCIin1(var1,s:TT)+y1;
shadedareain12(j,1:TT-s+1) = dYCIin2(var1,s:TT)+y1;
EstPath1(j,1:TT-s+1) = dY(var1,s:TT)+y1;
subplot(4,4,j,'Fontsize', 10);
shadedplot(t,dYCI1(var1,s:TT)+y1,dYCI2(var1,s:TT)+y1,[200/255 200/255 200/255],'k')
hold on
shadedplot(t,dYCIin1(var1,s:TT)+y1,dYCIin2(var1,s:TT)+y1,[160/255 160/255 160/255],'k')
hold on
plot(t,dY(var1,s:TT)+y1,'-.k','LineWidth', 2)
if flag ==1
plot(t,y1,'.','LineWidth', 2)
set(gca,'fontsize',8)
end
if j ==1
ylabel(varname(var1,1:end))
end
title(Countries(1+15*(i-1):15+15*(i-1)))
if var1 ==1
ymax = max(dYCI1(var1,s:TT)+y1)*(1 + sign( max(dYCI1(var1,s:TT)+y1))*0.25);
ymin = min(dYCI2(var1,s:TT)+y1)*(1 - sign(min(dYCI2(var1,s:TT)+y1))*0.25);
else
ymax = max(dYCI1(var1,s:TT)+y1)*(1 + sign( max(dYCI1(var1,s:TT)+y1))*0.1);
ymin = min(dYCI2(var1,s:TT)+y1)*(1 - sign(min(dYCI2(var1,s:TT)+y1))*0.1);
end
axis([2010 tau ymin ymax])
hold off
shadedarea21(j,1:TT-s+1) = dYCI1(var2,s:TT)+y2;
shadedarea22(j,1:TT-s+1) = dYCI2(var2,s:TT)+y2;
shadedareain21(j,1:TT-s+1) = dYCI1(var2,s:TT)+y2;
shadedareain22(j,1:TT-s+1) = dYCI2(var2,s:TT)+y2;
EstPath2(j,1:TT-s+1) = dY(var2,s:TT)+y2;
subplot(4,4,j+4,'Fontsize', 10);
shadedplot(t,dYCI1(var2,s:TT)+y2,dYCI2(var2,s:TT)+y2,[200/255 200/255 200/255],'k')
hold on
shadedplot(t,dYCIin1(var2,s:TT)+y2,dYCIin2(var2,s:TT)+y2,[160/255 160/255 160/255],'k')
hold on 
plot(t,dY(var2,s:TT)+y2,'-.k','LineWidth', 2)
set(gca,'fontsize',8)
if flag ==1
plot(t,y2,'.','LineWidth', 2)
end
if var2 ==5
ymax = 0.05;%max(dYCI1(var2,s:TT)+y2)*(1 + sign( max(dYCI1(var2,s:TT)+y2))*0.5);
ymin = -0.14;%min(dYCI2(var2,s:TT)+y2)*(1 - sign(min(dYCI2(var2,s:TT)+y2))*0.5);
else
ymax = max(dYCI1(var2,s:TT)+y2)*(1 + sign( max(dYCI1(var2,s:TT)+y2))*0.1);
ymin = min(dYCI2(var2,s:TT)+y2)*(1 - sign(min(dYCI2(var2,s:TT)+y2))*0.1);
end
axis([2010 tau ymin ymax])
if j ==1
ylabel(varname(var2,1:end))
end
shadedarea31(j,1:TT-s+1) = dYCI1(var3,s:TT)+y3;
shadedarea32(j,1:TT-s+1) = dYCI2(var3,s:TT)+y3;
EstPath3(j,1:TT-s+1) = dY(var3,s:TT)+y3;
subplot(4,4,j+8,'Fontsize', 10);
shadedplot(t,dYCI1(var3,s:TT)+y3,dYCI2(var3,s:TT)+y3,[200/255 200/255 200/255],'k')
hold on
shadedplot(t,dYCIin1(var3,s:TT)+y3,dYCIin2(var3,s:TT)+y3,[160/255 160/255 160/255],'k')
hold on 
plot(t,dY(var3,s:TT)+y3,'-.k','LineWidth', 2)
set(gca,'fontsize',8)
if flag ==1
plot(t,y3,'.','LineWidth', 2)
end
ymax = max(dYCI1(var3,s:TT)+y3)*(1 + sign( max(dYCI1(var3,s:TT)+y3))*0.1);
ymin = min(dYCI2(var3,s:TT)+y3)*(1 - sign(min(dYCI2(var3,s:TT)+y3))*0.1);
axis([2010 tau ymin ymax])
if j ==1
ylabel(varname(var3,1:end))
end
shadedarea41(j,1:TT-s+1) = dYCI1(var4,s:TT)+y4;
shadedarea42(j,1:TT-s+1) = dYCI2(var4,s:TT)+y4;
EstPath4(j,1:TT-s+1) = dY(var4,s:TT)+y4;
subplot(4,4,j+12,'Fontsize', 10);
shadedplot(t,dYCI1(var4,s:TT)+y4,dYCI2(var4,s:TT)+y4,[200/255 200/255 200/255],'k')
hold on
shadedplot(t,dYCIin1(var4,s:TT)+y4,dYCIin2(var4,s:TT)+y4,[160/255 160/255 160/255],'k')
hold on 
plot(t,dY(var4,s:TT)+y4,'-.k','LineWidth', 2)
set(gca,'fontsize',8)
if flag ==1
plot(t,y4,'.','LineWidth', 2)
end
ymax = max(dYCI1(var4,s:TT)+y4)*(1 + sign( max(dYCI1(var4,s:TT)+y4))*0.1);
ymin = min(dYCI2(var4,s:TT)+y4)*(1 - sign(min(dYCI2(var4,s:TT)+y4))*0.1);
axis([2010 tau ymin ymax])
if j ==1
ylabel(varname(var4,1:end))
end
xlabel('Years')
hold off
TT=TT-1;
end
if grop == 1;
save shadedarea1_g1 shadedarea11 shadedarea12 
save shadedareain1_g1 shadedareain11 shadedareain12 
save shadedarea2_g1 shadedarea21 shadedarea22
save shadedareain2_g1 shadedarea21 shadedarea22
save shadedarea3_g1 shadedarea31 shadedarea32
save shadedarea4_g1 shadedarea41 shadedarea42
save EstPath_g1 EstPath1 EstPath2 EstPath3 EstPath4
else
save shadedarea1_g2 shadedarea11 shadedarea12
save shadedareain1_g2 shadedareain11 shadedareain12 
save shadedarea2_g2 shadedarea21 shadedarea22
save shadedareain2_g2 shadedarea21 shadedarea22
save shadedarea3_g2 shadedarea31 shadedarea32
save shadedarea4_g2 shadedarea41 shadedarea42
save EstPath_g2 EstPath1 EstPath2 EstPath3 EstPath4
end
