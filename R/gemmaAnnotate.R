# uses probeset-gene reallignments provided in Gemma by Zoubarev, A., et al.,2012
# download the annotation file using getGemmaAnot and use it on the ExpressionSet
# object that is outputted by affy package pre-processing functions (eg. rma)
require(RCurl)
library(stringr)

# download's gemma's annotation files for your pleasure. annotType is for go terms
# attached to the genes. bioProcess is the least detailed version.
# uses gunzip unix utility.
#' @export
getGemmaAnnotGoogle = function(chipName, chipFile, annotType=c('bioProcess','noParents','allParents')){
    # gemma's own search system is crappy... so let me google that for you
    searchResult=getURL(paste0('http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=chibi+Gemma+showarraydesign+',
                  chipName))
    id = str_extract(strsplit(searchResult,':')[[1]][6], perl('(?<=id=)[0-9]*'))
    getGemmaAnnot(id, chipFile, annotType)
}

#' @export
getGemmaAnnot = function(id,chipFile,annotType){
    download.file(paste0('http://www.chibi.ubc.ca/Gemma/arrays/downloadAnnotationFile.html?id=',id,'&fileType=',annotType[1]),paste0(chipFile,'.gz'))
    system(paste0('gunzip ', chipFile,'.gz'))
}

# for affy package
#' @export
gemmaAnnot = function(normalized, chipFile, outFile=NA){
    expression = exprs(normalized)
    annotations = read.table(chipFile, header=T,sep='\t', quote="")
    # gene symbols are turned into Gene.Symbols because I was using it as Gene.Symbols
    # all this time. I ain't gonna change my scripts...
    names(annotations)[2] = 'Gene.Symbol'
    # Still not changing my code...
    names(annotations)[1] = 'Probe'
    
    annotations = annotations[match(rownames(expression), annotations$Probe),]

    annotatedExpr = cbind(annotations, expression)
    if (is.na(outFile)){
        return(annotatedExpr)
    } else {
        write.csv(annotatedExpr,  file = outFile, row.names = F)
        return(invisible(annotatedExpr))
    }
}

#' @export
gemmaAnnotOligo = function(normalized, chipFile, outFile = NA){
    featureData(normalized) <- getNetAffx(normalized, "transcript")
    expression <- get("exprs", pos=assayData(normalized))
    annotations = read.table(chipFile, header=T,sep='\t', quote="")
    # gene symbols are turned into Gene.Symbols because I was using it as Gene.Symbols
    # all this time. I ain't gonna change my scripts...
    names(annotations)[2] = 'Gene.Symbol'
    # Still not changing my code...
    names(annotations)[1] = 'Probe'
    
    annotations = annotations[match(rownames(expression), annotations$Probe),]

    annotatedExpr = cbind(annotations, expression)
    if (is.na(outFile)){
        return(annotatedExpr)
    } else {
        write.csv(annotatedExpr,  file = outFile, row.names = F)
        return(invisible(annotatedExpr))
    }
}

#' @export
gemmaGeneMatch = function(probesets, chipFile){
    annotations = read.table(chipFile, header=T,sep='\t', quote="")
    names(annotations)[2] = 'Gene.Symbol'
    names(annotations)[1] = 'Probe'
    annotations[match(probesets,annotations$Probe),'Gene.Symbol']
}
