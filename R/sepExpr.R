sepExpr <-
function(allDataPre){
    if (class(allDataPre)[1] =='data.frame'){
        for (i in 1:ncol(allDataPre)){
            if ('double'==typeof(allDataPre[,i])){
                expBound = i
                break
            }
        }
        geneData = allDataPre[,1:(expBound-1)]
        exprData = allDataPre[,expBound:ncol(allDataPre)]
        return(list(geneData,exprData))
    } else if (class(allDataPre)[1] =='data.table'){
        for (i in 1:ncol(allDataPre)){
            if ('double'==typeof(allDataPre[[i]])){
                expBound = i
                break
            }
        }
        geneData = allDataPre[,1:(expBound-1),with=F]
        exprData = allDataPre[,expBound:ncol(allDataPre), with = F]
        return(list(geneData,exprData))
    }
}
