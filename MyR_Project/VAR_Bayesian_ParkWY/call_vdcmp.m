function vdcmp=call_vdcmp(irfs,my,nstep)
% This function computes the variance decomposition using recursive ordering


vdcmp = zeros(my,my,nstep); % # of equations x # of shocks x horizons
tmp   = zeros(my,my,nstep);

% h=0
for idy=1:my
    tmp(:,idy,1) = diag(irfs(:,idy,1)*irfs(:,idy,1)');
end
for idy=1:my
    vdcmp(idy,:,1) = tmp(idy,:,1)./sum(tmp(idy,:,1));
end

for idh=2:nstep
    for idy=1:my
        tmp(:,idy,idh) = tmp(:,idy,idh-1) + diag(irfs(:,idy,idh)*irfs(:,idy,idh)');
    end
    for idy=1:my
        vdcmp(idy,:,idh) = tmp(idy,:,idh)./sum(tmp(idy,:,idh));
    end
end
