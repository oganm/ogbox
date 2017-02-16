# uses probeset-gene reallignments provided in Gemma by Zoubarev, A., et al.,2012
# download the annotation file using getGemmaAnot and use it on the ExpressionSet
# object that is outputted by affy package pre-processing functions (eg. rma)


#' @export
getGemmaAnnot = function(chipName,chipFile,annotType = c('bioProcess','noParents','allParents'), 
                         overwrite=FALSE){
    annotType = match.arg(annotType)
    if (annotType == 'allParents'){
        annotType = ''
    } else{
        annotType = paste0('_',annotType)
    }
    if(file.exists(chipFile) & !overwrite){
        print('the file already exists. not overwriting')
        return(FALSE)
    }
    download.file(paste0('http://chibi.ubc.ca/microannots/',chipName,annotType,'.an.txt.gz'),
                  paste0(chipFile,'.gz'))
    
    out = system(paste0('gunzip -f ', chipFile,'.gz'))
    if(out==0){
        return(TRUE)
    } else{
        return(FALSE)
    }
}

# for affy package
#' @export
gemmaAnnot = function(normalized, chipFile, outFile=NA){
    expression = exprs(normalized)
    annotations = read.table(chipFile, header=T,sep='\t', quote="", stringsAsFactors = F)
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
    annotations = read.table(chipFile, header=T,sep='\t', quote="", stringsAsFactors = F)
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
    annotations = read.table(chipFile, header=T,sep='\t', quote="",stringsAsFactors = F)
    names(annotations)[2] = 'Gene.Symbol'
    names(annotations)[1] = 'Probe'
    annotations[match(probesets,annotations$Probe),'Gene.Symbol']
}
