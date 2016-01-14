
# to separate microarray expression data into two data frames, one containing
# gene information other containing expression values. Most people seem to use
# this kind of output. use as list[gene,expression] = sepExpr(data) to get to
# dfs in one go
#' @export
sepExpr = function(allDataPre){
    if (class(allDataPre)[1] %in% c('data.frame','tbl_df')){
        # unlist is there to enable tbl_df to work
        for (i in 1:ncol(allDataPre) %>% unlist){
            if ('double'==typeof(allDataPre[,i])){
                expBound = i
                break
            }
        }
        geneData = allDataPre[,1:(expBound-1),drop=F]
        exprData = allDataPre[,expBound:ncol(allDataPre),drop=F]
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


# common read-write functions for data analysis -----
#' @export
write.design = function(x, file){
    write.table(x,file= file, sep = '\t', quote=F, row.names = F)
}

#' @export
read.design  = function(x){
    read.table(x,header=T,sep='\t',stringsAsFactors=F,quote="")
}

#' @export
read.exp = function(x){
    read.csv(x,header = T,stringsAsFactors=F)
}
