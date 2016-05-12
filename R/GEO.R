library(RCurl)
library(stringr)
library(parallel)
# returns a vector
#' @export
whichGSE = function(GSM){
    page = getURL(paste0('www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=', GSM))
    str_extract_all(page, 'GSE[0-9]*?(?=<)')[[1]]
}

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
    urls = unlist(str_extract_all(page,'ftp://ftp.ncbi.nlm.nih.gov/geo/samples/GSM.*?(c|C)(e|E)(l|L)%2Egz'))
    if (len(urls)>1){
        if (warnings){
            warning(paste(gsm,"has mutliple files"))
        }
        return(invisible(F))
    }
    
    if (len(urls)==0){
        if (warnings){
            warning(paste(gsm,"doesn't have a file attached"))
        }
        return(invisible(F))
    }
    
    fileURL  = URLdecode(urls)
    
    download.file(fileURL,paste0(outfile,'.gz'))
    if (unzip){
        system(paste0('gunzip -f "',outfile,'.gz"'))
    }
    invisible(T)
}

#' Download multiple cel files from GSE
#' @description Downloads all CEL files from GSMs related to a GSE. Optionally, a regular expression can be specified to download a subset of cel files
#' @param GSE GSE identifier of the dataset
#' @param regex regular expression to limit files to be downloaded. Searches the regex inside GSM titles
#' @param outDir directory to save the cel files
#' @param extension which extension to use when saving the file
#' @param overwrite should it overwrite the files if they exist
#' @param unzip should it decompress downloaded files.
#' @export
gseDown = function(GSE,regex =NULL,outDir, extension = '.cel',overwrite=F, unzip = T){
    # downloads GSMs matching a regular expression from a GSE (description not GSM ID)
    gsms = gsmFind(GSE, regex)
    for (i in 1:length(gsms)){
        gsmDown(gsms[i],paste0(outDir,'/', gsms[i],extension),overwrite, unzip)
    }
}

#' Downloads soft file of a GSE
#' @description Downloads a soft file.
#' @param GSE GSE identifier of the dataset
#' @param file destination file
#' @export
softDown = function(GSE,file){
    download.file(paste0("ftp://ftp.ncbi.nlm.nih.gov/geo/series/",
                  gsub('(((?<=GSE)([0-9]|[0-9][0-9]|[0-9][0-9][0-9]))|((?<=GSE.)[0-9][0-9][0-9])|((?<=GSE..)[0-9][0-9][0-9]))$','nnn',GSE,perl = T),'/',
                  GSE,'/soft/',GSE,'_family.soft.gz'),destfile = file)
}



#' A parser for soft files
#' @description Parses soft files in a flat file. Expression data inside the soft file can also be returned.
#' Returns a table of metadata if expression data is ommited. If not, returns a list. First element of the 
#' list is the metadata, the second element is a list of expression data that needs to be flattened independently
#'  since the expression tables can be different depending on the data
#' @param softFile soft file to be parsed
#' @param mergeFrame if there are different data fields, what to do with them. not yet implemented
#' @param n how many samples are there in the GSE, if not provided, it will be acquired from GEO
#' @param expression should the expression data be returned
#' @return A data.frame or a list depending on \code{expression}
#' @export
softParser = function (softFile, mergeFrame = c("intersect", "union"), n = NULL, 
                       expression = F) 
{
    con = file(softFile, open = "r")
    oneLine = readLines(con, n = 1, warn = FALSE)
    if (is.null(n)) {
        while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 
               0) {
            if (grepl("\\^SERIES", oneLine)) {
                GSE = strsplit(oneLine, " = ")[[1]][2]
                n = len(gsmFind(GSE))
                break
            }
        }
    }
    i = 0
    sampleData = vector(mode = "list", length = n)
    if (expression) {
        expressionData = vector(mode = "list", length = n)
    }
    while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
        if (grepl("\\^SAMPLE", oneLine)) {
            sampLines = vector(mode = "character", length = 0)
            while (oneLine != "!sample_table_begin") {
                sampLines = c(sampLines, oneLine)
                oneLine = readLines(con, n = 1, warn = FALSE)
            }
            if (expression) {
                expressionLines = vector(mode = "character", 
                                         length = 0)
                while (oneLine != "!sample_table_end") {
                    expressionLines = c(expressionLines, oneLine)
                    oneLine = readLines(con, n = 1, warn = FALSE)
                }
            }
            i = i + 1
            sampleData[[i]] = sampLines
            if (expression) {
                expressionData[[i]] = expressionLines
            }
            print(i)
        }
    }
    close(con)
    names(sampleData) = sapply(sampleData, function(x) {
        strsplit(x[1], " = ")[[1]][2]
    })
    sampleData = lapply(sampleData, function(x) {
        x[grepl("^\\!", x)]
    })
    samples = lapply(sampleData, function(x) {
        singleSample = sapply(x, function(y) {
            out = strsplit(y, "( = (?!.*?: ))|(: )", perl = T)[[1]]
            if (len(out) == 1) {
                out[2] = "NULL"
            }
            return(out[2])
        })
        names(singleSample) = sapply(x, function(y) {
            out = strsplit(y, "( = (?!.*?: ))|(: )", perl = T)[[1]][1]
            return(out)
        })
        dups = unique(names(singleSample)[duplicated(names(singleSample))])
        for (i in 1:len(dups)) {
            temp = paste0(singleSample[names(singleSample) %in% 
                                           dups[i]], collapse = " ")
            singleSample = singleSample[!names(singleSample) %in% 
                                            dups[i]]
            singleSample[dups[i]] = temp
        }
        return(singleSample)
    })
    fields = table(unlist(lapply(samples, names)))
    if (mergeFrame[1] == "intersect") {
        fields = names(fields[fields == max(fields)])
        samples = lapply(samples, function(x) {
            x[fields]
        })
    }
    samples = as.data.frame(t(as.data.frame(samples)))
    if (expression) {
        expressionData = lapply(expressionData, function(x) {
            con = textConnection(x)
            dat = read.table(con, comment.char = "!", sep = "\t", 
                             header = T)
            close(con)
            return(dat)
        })
        names(expressionData) = rownames(samples)
        return(list(samples, expressionData))
    }
    return(samples)
}
