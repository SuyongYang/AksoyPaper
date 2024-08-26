addpath(genpath('/User/Documents/VAR-Toolbox/v3dot0/'))

% Set the deterministic variable in the VAR (1=constant, 2=trend)
det = 1;
% Set number of nlags
nlags = 1;
% Estimate VAR by OLS
[VAR, VARopt] = VARmodel(ENDO,nlags,det);



rmpath(genpath('/User/DocumentsVAR-Toolbox/v3dot0'))
