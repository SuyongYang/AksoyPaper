function [c, tre]=hpf(y,phi)

% HPF - HP Filter.
%
% Ellen R. McGrattan,  4-23-87
% Revised, 3-22-92, ERM
% Adapted to MATLAB 5, 24-6-97, Marco A. Maffezzoli

[t,k]=size(y);
i=[1,1,2,2,1:t-2,2:t-2,3:t-2,3:t-1,3:t,t-1,t-1,t,t]';
j=[1,2,1,2,3:t,3:t-1,3:t-2,2:t-2,1:t-2,t-1,t,t-1,t]';
s=[1+phi,-2*phi,-2*phi,1+5*phi,phi*ones(1,t-2),...
   -4*phi*ones(1,t-3),(1+6*phi)*ones(1,t-4),...
   -4*phi*ones(1,t-3),phi*ones(1,t-2),...
   1+5*phi,-2*phi,-2*phi,1+phi]';
m=sparse(i,j,s);
c=y-m\y;
tre = m\y;