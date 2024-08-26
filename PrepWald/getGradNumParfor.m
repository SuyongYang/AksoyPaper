function gradnum = getGradNumParfor(nv, ng)
tic
    m = nv*nv + nv*(ng-1);  %--> 48
    for row = 1:nv
        for col  = 1:ng-1
            for ii = 1:m              
                name = sprintf('dDLRsym%d%d%d',row,col,ii);               
                dd = load(name);
                gradnum(ii,row,col) = RunGradEval(dd, nv, ng); %!!
            end
        end
    end
    save gradnum gradnum
    clear gradnum
 toc
% tic
%     m = nv*nv + nv*(ng-1);
%     for row = 1:nv
%         for col  = 1:ng-1
%             parfor ii = 1:m              
%                 name = sprintf('dDLRsym%d%d%d',row,col,ii);               
%                 dd = load(name);
%                 gradnum(ii,row,col) = RunGradEval_2ways(dd, nv, ng);
%             end
%         end
%     end
%     save gradnum_2ways gradnum
%  toc