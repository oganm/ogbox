
# to separate microarray expression data into two data frames, one containing
# gene information other containing expression values. Most people seem to use
# this kind of output. use as list[gene,expression] = sepExpr(data) to get to
# dfs in one go
#' @export
sepExpr = function(allDataPre){
    if (class(allDataPre)[1] %in% c('data.frame','tbl_df')){
        # unlist is there to enable tbl_df to work
        for (i in 1:ncol(allDataPre)){
            if ('double'==typeof(allDataPre[,i] %>% unlist)){
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
read.design  = function(x,...){
    read.table(x,header=T,sep='\t',stringsAsFactors=F,quote="",...)
}

#' @export
read.exp = function(x,...){
    data.table::fread(x,stringsAsFactors=F,data.table=F,...)
}

#' List cel files
#' @description list.celfiles from oligo package. it's better than affy since it 
#' has listGzipped option.
#' @param ... arguments to pass to \code{list.files}
#' @param listGzipped logical. If true adds gzipped files to the results
#' @return A string vector, listing files
#' @export
celFiles = function (..., listGzipped = FALSE) 
{
    files <- list.files(...)
    if (listGzipped) {
        return(files[grep("\\.[cC][eE][lL]\\.[gG][zZ]$|\\.[cC][eE][lL]$", 
                          files)])
    }
    else {
        return(files[grep("\\.[cC][eE][lL]$", files)])
    }
}


#' Detects gender of microarray samples
#' @description Detects gender of microarray samples based on expression of sex specific genes as described by Toker et al. 2016 (PMC5034794).
#' @param x Expression data frame with gene symbol and probename information in the first columns
#' @param geneColName column name for gene symbols
#' @param probeColName column name for probeset names
#' @param maleGenes names of male genes
#' @param femaleGenes names of female genes
#' @export
bioGender <- function(x,geneColName = 'Gene.Symbol', probeColName = 'Probe', maleGenes = c('RPS4Y1','KDM5D') , femaleGenes = 'XIST'){
    aned = x
    probeM <- aned[[probeColName]][grep(ogbox::regexMerge(maleGenes), aned[[geneColName]])] %>% as.character
    probeF <- aned[[probeColName]][grep(ogbox::regexMerge(femaleGenes), aned[[geneColName]])] %>% as.character
    print(paste('male probes found:', paste(probeM,collapse =', ')))
    print(paste('female probes found:', paste(probeF,collapse =', ')))
    
    
    if(length(probeM) > 0 | length(probeF) > 0){
        dataSex <- t(aned[aned[[probeColName]] %in% c(probeM, probeF),] %>%
                         (ogbox::sepExpr) %>% {.[[2]]})
        
        colnames(dataSex) <- aned[[probeColName]][aned[[probeColName]] %in%  c(probeM, probeF)]
        Clusters <- kmeans(dataSex, centers=2)
        
        Centers <- Clusters$center
        if(length(probeM)>0 & length(probeF)>0){
            if(mean(Centers[1,probeM]) > mean(Centers[1,probeF]) & mean(Centers[2,probeM]) < mean(Centers[2,probeF])){
                Clusters$cluster[Clusters$cluster==1] <- "M"
                Clusters$cluster[Clusters$cluster==2] <- "F"
            } else if(mean(Centers[1,probeM]) < mean(Centers[1,probeF]) & mean(Centers[2,probeM]) > mean(Centers[2,probeF])){
                Clusters$cluster[Clusters$cluster==1] <- "F"
                Clusters$cluster[Clusters$cluster==2] <- "M"
            } else {
                stop("Gender genes disagree, cannot decide about biological gender")
            }
        } else if(length(probeF) == 0) {
            if (mean(Centers[1,]) > mean(Centers[2,])) {
                Clusters$cluster[Clusters$cluster==1] <- "M"
                Clusters$cluster[Clusters$cluster==2] <- "F"
            } else {
                Clusters$cluster[Clusters$cluster==1] <- "F"
                Clusters$cluster[Clusters$cluster==2] <- "M"
            }  
        } else if (length(probeM) == 0) {
            if (mean(Centers[1,]) > mean(Centers[2,])){
                Clusters$cluster[Clusters$cluster==1] <- "F"
                Clusters$cluster[Clusters$cluster==2] <- "M"
            } else {
                Clusters$cluster[Clusters$cluster==1] <- "M"
                Clusters$cluster[Clusters$cluster==2] <- "F"
            }
        } 
        
        BioGender <- Clusters$cluster
        return(BioGender)
    } else {
        print("No sex specific genes on the platform")
    }
}



#' @export
pickRandom = function(labels,
                      allValues,
                      tolerance,
                      allowSelf = TRUE,
                      invalids = c(),
                      n = 500){
    
    distribution = ecdf(allValues)
    percentiles = distribution(allValues[labels])
    range = quantile(allValues, c((percentiles -tolerance/2) %>% sapply(max,0),
                                  (percentiles + tolerance/2) %>% sapply(min,.99))) %>% 
        matrix(ncol = 2)
    colnames(range) = c('min','max')
    allValues = allValues[!names(allValues) %in% invalids]
    eligibles = range %>% apply(1,function(x){
        which( allValues >= x['min'] & allValues <= x['max']) %>% names
    })
    names(eligibles) = labels
    
    random = eligibles %>% sapply(function(x){
        if(length(x)==0){
            return(rep(NA,length(n)) %>% as.character)
        }
        sample(x,n,replace= TRUE)
    }) %>% as.data.frame(stringsAsFactors = FALSE)
}