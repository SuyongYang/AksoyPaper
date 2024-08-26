% Demographic Structure and Macroeconomic Trends - Aksoy, Basso, Smith, Grasl 
% This code produces Figure A.2 in the online Appendix. In sample projections for output,
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


load HistData70.csv;
%get weights
mh = size(HistData70,1);
Popweights = zeros(mh,ng);
for i = 1:mh
    sumpop = sum(HistData70(i,1:end));
    Popweights(i,1) = sum(HistData70(i,1:4))/sumpop;
    Popweights(i,2) = sum(HistData70(i,5:12))/sumpop;
    Popweights(i,3) = sum(HistData70(i,13:end))/sumpop;
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
vecpos = [22 13 12 7]; %US Japan Italy France
%vecpos = [7 13 12 22]; %US Japan Italy France
vecpos2 = [4 1 14 18]; % Canada Australia Netherlands Spain
%1 gdp, 2 inv, 3 sav, 4 hours, 5 rr, 6 inf
var1 = 1; %put 1 for outut or 2 for investment 
var2 = 5; %put 5 for real rates or 3 for savings
var3 = 2; %put 5 for real rates or 3 for savings
var4 = 3; %put 5 for real rates or 3 for savings
varname = ['Output Growth'; 'Investment   '; 'Savings      '; 'Hours        '; 'Interest Rate'];
grop = 2;
flag = 0; %put one to include line with intial value zero otherwise
flagfilter = 0; %put one for HP and 0 for Band pass
factorHP = 1200;%1600; %Which parameter should one use.
factorCF = [1 35];

TT = 44; %years of back fitting 1970 - 2010
figure

start = 10;
TT = TT-start;
for j = 1:1:4;
if grop == 1;
i = vecpos(j);
else
i = vecpos2(j);
end

DW(:,:) = Popweights(1+46*(i-1):46+46*(i-1),:); 

DW_3g(:,1) = DW(:,1) - DW(:,3);
DW_3g(:,2) = DW(:,2) - DW(:,3);


dDW_3g = diff(DW_3g);
dDW_3g = dDW_3g(start:end,:);

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
s = 1;
dY(:,s) = DLR3g*dDW_3g(s,1:2)';
std_dY(:,s) = (dvcov3g*(dDW_3g(s,1:2).^2)').^0.5;
dYCI1(:,s) = dY(:,s) + z*std_dY(:,s);
dYCI2(:,s) = dY(:,s) - z*std_dY(:,s);
dYCIin1(:,s) = dY(:,s) + z2*std_dY(:,s);
dYCIin2(:,s) = dY(:,s) - z2*std_dY(:,s);
for t = s+1:1:TT+1;
    dY(:,t) = DLR3g*sum(dDW_3g(s:t,1:2))';
    std_dY(:,t) = (dvcov3g*(sum(dDW_3g(s:t,1:2)).^2)').^0.5;
    dYCI1(:,t) = dY(:,t) + z*std_dY(:,t);
    dYCI2(:,t) = dY(:,t) - z*std_dY(:,t);
    dYCIin1(:,t) = dY(:,t) + z2*std_dY(:,t);
    dYCIin2(:,t) = dY(:,t) - z2*std_dY(:,t);
end


DataCountry = EconData(1+45*(i-1):45+45*(i-1),:);
logOutput = log(DataCountry(1:45,3));
Outputg = diff(logOutput);
%Outputg2  = Outputg(2:end-1) - BK(Outputg,1,100,1);
Outputg2  = Outputg - CF(Outputg,factorCF(1),factorCF(2));
[z Outputg] = Hpf(Outputg,factorHP);
if flagfilter ==1
    Outputg = Outputg(start:TT+start);
else
    Outputg = Outputg2(start:TT+start);
end
%
y1 = Outputg(1);%Growth
if i ==8 %Germany
    y1 =  0.021;
end

RR = DataCountry(1:45,4);
RR2  = RR - CF(RR,factorCF(1),factorCF(2));
[z RR] = Hpf(RR,factorHP);
if flagfilter ==1
    RR = RR(start:TT+start);
else
    RR = RR2(start:TT+start);
end
y2 = RR(1);%Real rate
if i ==8 %Germany
    y2 =  0.012;
end
Invv = DataCountry(1:45,1);
Invv2  = Invv - CF(Invv,factorCF(1),factorCF(2));
[z Invv] = Hpf(Invv,factorHP);
if flagfilter ==1
    Invv = Invv(start:TT+start);
else
    Invv = Invv2(start:TT+start);
end


y3 = Invv(1); %Investment
if i ==8 %Germany
    y3 =  0.229;
end
Sav = DataCountry(1:45,2);
if i ==7
    Sav(1:8) = Sav(9);
end
Sav2  = Sav - CF(Sav,factorCF(1),factorCF(2));
[z Sav] = Hpf(Sav,factorHP);
if flagfilter ==1
    Sav = Sav(start:TT+start);
else
    Sav = Sav2(start:TT+start);
end
y4 = Sav(1); %Savings
if i ==8 %Germany
    y4 =  0.084;
 end
s = 1;
TT = TT+1;
tau = 1970+start+TT;
t = 1970+start:1:1970+start+TT-1;
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
plot(t,Outputg','-b','LineWidth', 2)
if flag ==1
plot(t,y1,'.','LineWidth', 2)
end
set(gca,'fontsize',8)
if j ==1
ylabel(varname(var1,1:end))
end
title(Countries(1+15*(i-1):15+15*(i-1)))
ymax = max(dYCI1(var1,s:TT)+y1)*(1 + sign( max(dYCI1(var1,s:TT)+y1))*0.7);
ymin = min(dYCI2(var1,s:TT)+y1)*(1 - sign(min(dYCI2(var1,s:TT)+y1))*0.7);
axis([1970+start-1 tau ymin ymax])
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
plot(t,RR','-b','LineWidth', 2)
set(gca,'fontsize',8)
if flag ==1
plot(t,y2,'.','LineWidth', 2)
end
if i ==22
    ymax = y2+0.05;
    ymin = y2-0.05;
elseif i ==7
   ymax = y2+0.05;
    ymin = y2-0.08;
else
    if grop ==2
      ymax = y2+0.075; %max(dYCI1(var2,s:TT)+y2)*(1 + sign( max(dYCI1(var2,s:TT)+y2))*0.5);
      ymin = y2-0.075;%min(dYCI2(var2,s:TT)+y2)*(1 - sign(min(dYCI2(var2,s:TT)+y2))*0.5); 
    else
      ymax = y2+0.1; %max(dYCI1(var2,s:TT)+y2)*(1 + sign( max(dYCI1(var2,s:TT)+y2))*0.5);
      ymin = y2-0.1;%min(dYCI2(var2,s:TT)+y2)*(1 - sign(min(dYCI2(var2,s:TT)+y2))*0.5);
    end
    ymax = max(dYCI1(var2,s:TT)+y2)*(1 + sign( max(dYCI1(var2,s:TT)+y2))*1.2);
    ymin = min(dYCI2(var2,s:TT)+y2)*(1 - sign(min(dYCI2(var2,s:TT)+y2))*0.35);
end
axis([1970+start-1 tau ymin ymax])
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
plot(t,Invv','-b','LineWidth', 2)
set(gca,'fontsize',8)
if flag ==1
plot(t,y3,'.','LineWidth', 2)
end
ymax = max(dYCI1(var3,s:TT)+y3)*(1 + sign( max(dYCI1(var3,s:TT)+y3))*0.15);
ymin = min(dYCI2(var3,s:TT)+y3)*(1 - sign(min(dYCI2(var3,s:TT)+y3))*0.15);
axis([1970+start-1 tau ymin ymax])
if j ==1
ylabel(varname(var3,1:end))
end
shadedarea41(j,1:TT-s+1) = dYCI1(var4,s:TT)+y4;
shadedarea42(j,1:TT-s+1) = dYCI2(var4,s:TT)+y4;
EstPath4(j,1:TT-s+1) = dY(var4,s:TT)+y4;
subplot(4,4,j+12,'Fontsize', 10);
[ha, hb, hc] = shadedplot(t,dYCI1(var4,s:TT)+y4,dYCI2(var4,s:TT)+y4,[200/255 200/255 200/255],'k')
hold on
shadedplot(t,dYCIin1(var4,s:TT)+y4,dYCIin2(var4,s:TT)+y4,[160/255 160/255 160/255],'k')
hold on 
h1 = plot(t,dY(var4,s:TT)+y4,'-.k','LineWidth', 2)
h2 = plot(t,Sav','-b','LineWidth', 2)
set(gca,'fontsize',8)
if flag ==1
plot(t,y4,'.','LineWidth', 2)
end
ymax = max(dYCI1(var4,s:TT)+y4)*(1 + sign( max(dYCI1(var4,s:TT)+y4))*0.2);
ymin = min(dYCI2(var4,s:TT)+y4)*(1 - sign(min(dYCI2(var4,s:TT)+y4))*0.7);
axis([1970+start-1 tau ymin ymax])
if j ==1
ylabel(varname(var4,1:end))
end
xlabel('Years')
hold off
TT = TT-1;
end
if j == 4
    if flagfilter == 1
        h = legend([h1 h2 ha(2)], texlabel('Estimated Projection'), texlabel('HP filtered data'),texlabel('One-sided 90% Interval') );
    else
        h = legend([h1 h2 ha(2)], texlabel('Estimated Projection'), texlabel('Band Pass filtered data'),texlabel('One-sided 90% Interval') );
    end
    set(h,'Position', [0.5 0.01 0.05 0.05], 'Orientation', 'horizontal','Box', 'off');
end
hold off
