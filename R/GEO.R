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


#' @export
softDown = function(GSE,file){
    download.file(paste0("ftp://ftp.ncbi.nlm.nih.gov/geo/series/",
                  gsub('((?<=E)|((?<=E)|[0-9]))[0-9]$','nnn',GSE,perl = T),'/',
                  GSE,'/soft/',GSE,'_family.soft.gz'),destfile = file)
}


#' @export
softParser = function(softFile, # file to read
                      mergeFrame = c('intersect', 'union'), # union not implemented
                      n=NULL # number of samples in the file
){
    con  = file(softFile, open = "r")
    oneLine = readLines(con, n = 1, warn = FALSE)
    
    # if n isn't provided grab it online
    if (is.null(n)){
        while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0){
            if (grepl('\\^SERIES', oneLine)){
                GSE = strsplit(oneLine,' = ')[[1]][2]
                n = len(gsmFind(GSE))
                break
            }
        }
    }
    
    i=0
    sampleData = vector(mode ='list', length = n)
    # get relevant information
    while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
        if (grepl('\\^SAMPLE', oneLine)){
            sampLines = vector(mode = 'character', length=0)
            while (oneLine != '!sample_table_begin'){
                sampLines = c(sampLines, oneLine)
                oneLine = readLines(con, n = 1, warn = FALSE)
            }
            i = i+1
            sampleData[[i]] = sampLines
            print(i)
        }
    }
    
    close(con)
    
    names(sampleData) = sapply(sampleData,function(x){
        strsplit(x[1],' = ')[[1]][2]
    })
    
    sampleData = lapply(sampleData,function(x){
        x[grepl('^\\!',x)]
    })
    
    samples = lapply(sampleData,function(x){
        singleSample = sapply(x, function(y){
            out = strsplit(y, '(\ =\ (?!.*?:\ ))|(:\ )', perl=T)[[1]]
            if (len(out)==1){
                out[2] = "NULL"
            }
            return(out[2])
        })
        
        names(singleSample) = sapply(x,function(y){
            out = strsplit(y, '(\ =\ (?!.*?:\ ))|(:\ )', perl=T)[[1]][1]
            return(out)
        })
        
        # some fields occur more than once. merge'em
        dups = unique(names(singleSample)[duplicated(names(singleSample))])       
        
        for (i in 1:len(dups)){
            temp  = paste0(singleSample[names(singleSample) %in% dups[i]], collapse = ' ')
            singleSample = singleSample[!names(singleSample) %in% dups[i]]
        }
        
        
        # print(names(singleSample))
        return(singleSample)
    })
    
    
    
    fields = table(unlist(lapply(samples,names)))
    
    if (mergeFrame[1] =='intersect'){
        fields = names(fields[fields==max(fields)])
        samples = lapply(samples, function(x){
            x[fields]
        })
    }
    samples = as.data.frame(t(as.data.frame(samples)))
    return(samples)
}
