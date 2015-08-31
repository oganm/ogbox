# given two directories, matches the files that are the same based on their
# md5 sums
require(tools)

#' @export
matchFiles = function(dir1, dir2, fileOut=NA, basename=T, justMatches=F, removeSameName=F){
    files1 = list.files(dir1, full.names=T)
    files2 = list.files(dir2, full.names=T)
    sums1 = md5sum(files1)
    sums2 = md5sum(files2)
    
    sums2[match(sums1,sums2)]
    
    matching1 = names(sums1[match(sums2,sums1)])
    matching1 = cbind(matching1, names(sums2))
    
    matching2 =  names(sums2[match(sums1,sums2)])
    matching2 = cbind(names(sums1), matching2)
    
    out = unique(rbind(matching1,matching2))
    out = as.data.frame(out)
    colnames(out) = c(dir1,dir2)
    
    if (basename){
        out = apply(out,2,basename)
    }
    
    if (justMatches){
        out=out[apply(out,1,function(x){!any(is.na(x))}),]
    }
    
    if (removeSameName){
        out = out[out[,1]!=out[,2],]
    }
    
    if (!is.na(fileOut)){
        dir.create(dirname(fileOut), showWarnings=F, recursive=T)
        write.table(out,file=fileOut,quote=F,sep='\t',col.names=T,row.names=F)
    }
    
    return(out)
}