
tic
%parpool(4)
nv= 6;
ng = 3;
runDLRsym(nv, ng);
toc
getjacobDLR(nv, ng);
%4000 seconds to run--> do not use parfor...
toc