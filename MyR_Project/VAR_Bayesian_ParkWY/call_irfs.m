function irfs=call_irfs(B,OMG,my,pen,nstep)
% This function computes the impulse responses given B and Omega using
% recursive ordering. It assumes that the intercept is always included

% Cholesky decomposition (lower triangular)
A     = chol(OMG)';

%B1 = reshape(B(1:(end-1),:)',my,my,pen);
tem=my*pen;
B1 = reshape(B(1:tem,:)',my,my,pen);

irfs = zeros(my,my,nstep); % # of equations x # of shocks x horizons
irfs(:,:,1) = A;           % on impact (h=0)
for idh=2:nstep            % h = 1,2,...,(nstep-1)
   for idy=1:min(pen,idh-1)
      irfs(:,:,idh) = irfs(:,:,idh)+B1(:,:,idy)*irfs(:,:,idh-idy);
   end
end
