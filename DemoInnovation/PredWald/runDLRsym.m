function runDLRsym(nv, ng)
%this function generates the DLRsym to be used in the wald test
tic
    A = sym(zeros(nv));
    D = sym(zeros(nv,ng-1));

    for row = 1:nv
        for col = 1:nv
            A(row,col) = sym(sprintf('a%d%d',row,col));
        end
    end

    for row = 1:nv
        for col = 1:ng-1
            D(row,col) = sym(sprintf('d%d%d',row,col));
        end
    end
    
    
    inner = diag(ones(nv,1)) - A;
    C = sym(zeros(nv));

    for row = 1:nv
        for col = 1:nv
            C(row,col) = sym(sprintf('c%d%d',row,col));
        end
    end

    inverseC = inv(C);

    for row = 1:nv
        for col = 1:nv
            inverseC = subs(inverseC,C(row,col), inner(row,col));
        end
    end
    inverseA = inverseC;
    DLRsym = inverseA*D;
    clear A C D inner inverseA inverseC
     for row = 1:nv
        parfor col = 1:ng-1
            name = sprintf('DLRsym%d%d',row,col);
            varr = DLRsym(row,col);
            savetofile(varr,name);                     
        end
        toc
     end
    toc
    
    
    
    
    
    
    