% This code estimates a reduced-form VAR by Bayesian methods and computes
% impulse responses using recursive ordering

% Woong Yong Park
% April 9, 2016

%% housekeeping

clear;
clc;

% time stamp
timeStart = datestr(now);
tic;

% initialize random number generator
rng shuffle;

%% setup

% dates of the sample
y_all   = 419;                 % number of all observations
y_vars  = 3;                   % number of variables
y_start = 1;                   % start date (obs number) (after y_start, first 'pen' obs are used as initial obs)
y_end   = 419;                 % end date (obs number)

pen     = 24;                  % lags for endogenous variables

dates   = (1973+1/12:1/12:2007+11/12)'; % dates (Feb 1973 - Nov 2007)
dates2  = dates((1+pen):end);    % dates excluding the training sample and the initial conditions

% data file
datanms = 'data.txt';

% folder and file names
folder  = 'm_01';
filenms = 'rfvar_results';

% variables in the data files
%1; Growth rate of world oil production (will be accumulated to the level)
%2; Global real activity (index based on dry cargo shipping rates)
%3; Real price of oil 
varnms = {  'Oil production';
            'Real activity';        
            'Real price of oil'};   
shocknms = {'Oil supply shock';
            'Aggregate demand shock';
            'Oil-specific demand shock'};
        
% impulse responses
nstep    = 20;          % IRFs horizon

% simulations
nsimul   = 5000;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% estimation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% check the directory
dir_chk = exist(folder,'dir');
if (dir_chk==0)
    disp('The designated folder does not exists... Create it.')
    mkdir(folder);
end

%***** load data
% First pen rows will be used as initial conditions.

dat = load(datanms);

if (y_all~=size(dat,1))
    error('Check the nubmer of observations!')
end

if (y_vars~=size(dat,2))
    error('Check the number of variables')
end

%***** numbers

% number of endogenous variables
my   = y_vars;

% number of right-hand-side variables in each equation
% endogenous variables + constant
mk   = my*pen+1;

% number of observations (# of obs - # of lags)
T    = (y_end - y_start + 1) - pen;

%***** data matrices

% y(t)
Y  = dat((y_start+pen):y_end,:);

% lags of y(t)
xdat = [];
for jdx=1:pen
    xdat = [xdat dat((y_start+pen-jdx):(y_end-jdx),:)];
end
% 1's
X  = [xdat ones(T,1)];

clear xdat;

%***** OLS(MLE) estimates and residuals

XX     = X'*X;
iXX    = inv(XX);
BOLS   = XX\(X'*Y); % inv(X'*X)*(X'*Y)
UOLS   = Y - X*BOLS;
S      = UOLS'*UOLS;
OMGOLS = S/T;

%***** prior distribution

nu0    = mk + my + 1;
S0     = 10^3*eye(my);
B0     = zeros(size(BOLS));
XX0    = 10^3*eye(mk);

%***** posterior distribution

XX1    = XX+XX0;
iXX1   = inv(XX1);
B1     = XX1\(XX*BOLS+XX0*B0);
b1     = reshape(B1,mk*my,1);
U1     = Y - X*B1;
S1     = U1'*U1;
nu1    = T+nu0;

%***** simulation
% we do not save B's and OMG's

irfsl = zeros(my,my,nstep,nsimul);
vdcmp = zeros(my,my,nstep,nsimul);

for ids=1:nsimul

    OMGtmp = iwishrnd(S1,nu1);
    Btmp   = reshape(mvnrnd(b1,kron(OMGtmp,iXX1)),mk,my);
    
    %***** impulse responses
    irfs = call_irfs(Btmp,OMGtmp,my,pen,nstep);

    % compute the response of the level of oil production
    irfsl(:,:,:,ids) = irfs;
    for idh=2:nstep
        irfsl(1,:,idh,ids) = irfsl(1,:,idh-1,ids) + irfs(1,:,idh);
    end

    %***** variance decomposition
    vdcmp(:,:,:,ids) = call_vdcmp(irfs,my,nstep);

end
    
%***** summary statistics

irfslsumm = struct;
irfslsumm.mean   = mean(irfsl,4);
irfslsumm.median = median(irfsl,4);
irfslsumm.p5     = quantile(irfsl,0.05,4);
irfslsumm.p95    = quantile(irfsl,0.95,4);

vdcmpsumm = struct;
vdcmpsumm.mean   = mean(vdcmp,4);
vdcmpsumm.median = median(vdcmp,4);
vdcmpsumm.p5     = quantile(vdcmp,0.05,4);
vdcmpsumm.p95    = quantile(vdcmp,0.95,4);

clear irfsl vdcmp;

%% report

save(fullfile(folder,filenms),'irfslsumm','vdcmpsumm');

% generate plots

% factors to be multiplied to IRFs (signs and units)
fc = [-1;1;1];
% yaxis
yx = [-25 15; -5 10; -5 15];

pdx = 1;
for idx=1:my
    for idy=1:my
        subplot(3,3,pdx)
        plot(0:(nstep-1),fc(idy)*squeeze(irfslsumm.median(idx,idy,:)),'b-')
        hold on;
        plot(0:(nstep-1),fc(idy)*squeeze(irfslsumm.p5(idx,idy,:)),'b--')
        plot(0:(nstep-1),fc(idy)*squeeze(irfslsumm.p95(idx,idy,:)),'b--')
        plot(0:(nstep-1),zeros(nstep,1),'k')
        hold off;
        title([varnms{idx} ' to ' shocknms{idy}])
        xlim([0 (nstep-1)])
        ylim(yx(idx,:))
        pdx = pdx+1;
    end
end

print('-dpdf',fullfile(folder,sprintf('%s_irfs.pdf',filenms)));

% print variance decompositions

disp('==================================================================')
disp('variance decompositions')
disp('------------------------------------------------------------------')
disp('== median')
disp(' ')
disp('h=0')
disp(vdcmpsumm.median(:,:,1))
disp('h=6')
disp(vdcmpsumm.median(:,:,7))
disp('h=12')
disp(vdcmpsumm.median(:,:,13))
disp('== 5%')
disp(' ')
disp('h=0')
disp(vdcmpsumm.p5(:,:,1))
disp('h=6')
disp(vdcmpsumm.p5(:,:,7))
disp('h=12')
disp(vdcmpsumm.p5(:,:,13))
disp('== 95%')
disp(' ')
disp('h=0')
disp(vdcmpsumm.p95(:,:,1))
disp('h=6')
disp(vdcmpsumm.p95(:,:,7))
disp('h=12')
disp(vdcmpsumm.p95(:,:,13))

%% Elapsed time

disp('--------------------------------------------------------')
fprintf('Started at     : %s\n',timeStart)
fprintf('Terminated at  : %s\n',datestr(clock))
timeTerminate = toc;
fprintf('Elapsed time   : %12.2f seconds\n',timeTerminate)
fprintf('               : %12.2f hours\n',timeTerminate/3600)