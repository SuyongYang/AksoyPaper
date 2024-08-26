function getjacobDLR(nv, ng)

%this function generates the jacobian of the DLRsym to be used in the wald test
tic
for row = 1:nv
    for col = 1:nv
        if row == 1 && col ==1
        vec = sym(sprintf('a%d%d',row,col));
        else
        vec = [vec; sym(sprintf('a%d%d',row,col))];
        end
    end
    for col = 1:ng-1
        vec = [vec; sym(sprintf('d%d%d',row,col))];
    end
end
mm = size(vec,1);   
     for row = 1:nv
        for col = 1:ng-1
            name = sprintf('DLRsym%d%d',row,col);
            eval(['load ' name])    % load name ....? 
            corevar = varr;
            for iii = 1:mm
                dd = jacobian(corevar,vec(iii));  %symbolic function jacobian(...)
                name = sprintf('dDLRsym%d%d%d',row,col,iii);
                save(name,'dd');
                %savetofile(dd,name);
            end
            clear varr corevar
        end
     end
     toc