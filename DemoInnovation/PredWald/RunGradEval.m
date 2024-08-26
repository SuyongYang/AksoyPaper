function [tempcalc] = RunGradEval(gradvec_temp, nv, ng)
    
    load A1.txt;
    A1=A1;
    load shortD.txt;
    D = shortD(:,1:end-1);

    for row = 1:nv
        for col = 1:nv
        eval(['a' num2str(row) num2str(col) '= A1(row,col);'])
        end
        for col = 1:ng-1
        eval(['d' num2str(row) num2str(col) '= D(row,col);']) %inverted col and row since saved matrix was inverted
        end
    end

tempcalc = eval(gradvec_temp.varr);
end

