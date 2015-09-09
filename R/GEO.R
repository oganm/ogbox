# replacemt to gsmDown. Didn't remove it in case I'm using it somewhere. This
# will include all GEO related functions
library(RCurl)
library(stringr)
library(parallel)

#' @export
gsmFind = function(GSE, regex=NULL, cores = 1){
    # finds GSMs that match to a regular expression from a GSE (description not GSM ID)
    page = getURL(paste0('www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=', GSE))
    sampleSize = as.numeric(str_extract(page, '(?<=Samples\\ \\().*?(?=\\))'))
    # due to the structure of the page if there are more than 500 samples,
    # download the list of gsms. in the list the title isnt present hence the
    # need to look at the pages individually, this can make the function go slow
    if (sampleSize>500){
        page =  getURL(paste0('http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',GSE,'&targ=self&view=brief&form=text'))
        page = strsplit(page,split = '\n')[[1]]
        gsms = trimNAs(str_extract(page,"GSM.*?(?=\r)"))
        # only try doing this if regex is provided
        if (!is.null(regex)){
            if (cores==1){
                gsms = gsms[
                    sapply(1:len(gsms),function(i){
                        page = getURL(paste0('http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',gsms[i]))
                        grepl(regex,str_extract(str_extract(page,'Title.*?\\n.*?\n'),'(?<=\\>).*?(?=\\<)'))
                    })]
            } else {
                gsms = gsms[unlist(mclapply(1:len(gsms),function(i){
                    page = getURL(paste0('http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',gsms[i]))
                    grepl(regex,str_extract(str_extract(page,'Title.*?\\n.*?\n'),'(?<=\\>).*?(?=\\<)'))
                }, mc.cores = cores))]
            }
        }
    } else {
        page = getURL(paste0('www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=', GSE))
        if (is.null(regex))
            regex = ''
        gsms = regmatches(page,gregexpr(paste0('GSM[0-9]*?(?=<.*\n.*?',regex,'.*?</td)'),page,perl=T))[[1]]
    }
    return(gsms)
    
}

#' @export
gsmSize = function(gsm, warnings = T){
    page = getURL(paste0('http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',gsm))
    fileURL = fileURL = URLdecode(str_extract(page,'ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM.*?(c|C)(e|E)(l|L)%2Egz'))
    
    if (len(fileURL) == 0){
        if (warnings){
            warning(paste(gsm,"doesn't have a file attached"))
        }
        return(invisible(F))
    }
    sizeString = getURL(fileURL,nobody=1L, header=1L)
    size = as.numeric(
        str_extract(sizeString, perl('(?<=(Content-Length: )).*?(?=\r)')))
    return(size)
}

#' @export
gsmDown = function(gsm,outfile, overwrite = F, warnings = T, unzip = T){
    # downloads a given GSM
    dir.create(dirname(outfile), showWarnings=F,recursive=T)
    if (file.exists(outfile) & !overwrite){
        print('you already have it bro. i aint gonna get it again')
        print(basename(outfile))
        return(invisible(F))
    }
    page = getURL(paste0('http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=',gsm))
    
    fileURL =fileURL = URLdecode(str_extract(page,'ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM.*?(c|C)(e|E)(l|L)%2Egz'))
    
    if (len(fileURL) == 0){
        if (warnings){
            warning(paste(gsm,"doesn't have a file attached"))
        }
        return(invisible(F))
    }
    download.file(fileURL,paste0(outfile,'.cel.gz'))
    if (unzip){
        system(paste0('gunzip -f "',outfile,'.cel.gz"'))
    }
    invisible(T)
}

#' @export
gseDown = function(GSE,regex =NULL,outDir, extension = '.cel',overwrite=F, unzip = T){
    # downloads GSMs matching a regular expression from a GSE (description not GSM ID)
    gsms = gsmFind(GSE, regex)
    for (i in 1:length(gsms)){
        gsmDown(gsms[i],paste0(outDir,'/', gsms[i],extension),overwrite, unzip)
    }
}
