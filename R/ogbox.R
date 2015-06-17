
#' Trim first line comments
#' @description Deletes the comments added to the first lines. Useful when comment char is 
#' only a comment char in the beginning of the file
#' @param fileName name of the file
#' @param commentChar comment indicator
#' @param outFile File to save
#' @return a textConnection object
#' @export
trimHeadComment = function(fileName, commentChar = '#',outFile = NULL){
    lines = readLines(fileName)
    i=1
    while (T){
        if (substr(lines[i],1,1)!=commentChar){
            break}
        i = i+1
    }
    if (is.null(outFile)){
        return(textConnection(paste(lines[i:length(lines)],collapse='\n')))
    } else {
        fileConn = file(outFile)
        writeLines(text = lines[i:length(lines)] , con = fileConn)
        close(fileConn)
        invisible(textConnection(paste(lines[i:length(lines)],collapse='\n')))
    }
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


#' Clear display
#' @description clear display, taken from http://stackoverflow.com/questions/14260340/function-to-clear-the-console-in-r
#' @export
clc = function(){
    cat("\014") 
}


# allows directly assigning list outputs to variables
# http://stackoverflow.com/questions/1826519/function-returning-more-than-one-value
#' @export
list <- structure(NA,class="result")

#' @export
"[<-.result" <- function(x,...,value) {
    args <- as.list(match.call())
    args <- args[-c(1:2,length(args))]
    length(value) <- length(args)
    for(i in seq(along=args)) {
        a <- args[[i]]
        if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
    }
    x
}



#' Delete everything
#' @description Clear global environment
#' @export
purge =   function() {
    rm(list = ls(.GlobalEnv, all.names = T), envir = .GlobalEnv)
}


#' Read specific lines from a file
#' @description Creates a text connection for specific lines of a file and
#' returns them as an output of \code{fun}. Modified from  http://stackoverflow.com/questions/18235269/efficiently-reading-specific-lines-from-large-files-into-r
#' @param file the name of the file which lines will be read from
#' @param fun function that will process each line
#' @return A list containing the outputs per line
#' @export
checkLines = function(file,lines,fun = readLines, ...){
    outAll = vector(mode= 'list',length = length(lines))
    for (i in 1:length(lines)){
        con = pipe(paste0("sed -n -e'",lines[i],"p' ",file))
        out = fun(con, ...)
        outAll[[i]] = out
    }
    return(outAll)
}

#' Source R files from github
#' @description sources R files from github
#' @param user username
#' @param repo repository name
#' @param script name of the script
#' @details Does not require \code{user}, \code{repo} or \code{script} to be 
#' strings. Defaults to capitalized .R if extension not provided
#' @export
sourceGithub = function(user, repo, script){
    user = substitute(user)
    user = as.character(user)
    repo = substitute(repo)
    repo = as.character(repo)
    script = substitute(script)
    script = as.character(script)
    
    require(RCurl)
    if (!grepl('[.](r|R)',script)){
        script = paste0(script,'.R')
    }
    text = getURL(paste0(
        "https://raw.githubusercontent.com/",user,'/',repo,'/master/',script),
        ssl.verifypeer=FALSE) 
    source(textConnection(text))
}

# multiple iterations of gsub in a single line. it replaces every element in patterns
# with matching elements at replacements. Starts from the first
#' @export
gsubMult = function(patterns, replacements, x,
                    ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    for (i in 1:length(patterns)){
        x = gsub(patterns[i],replacements[i],x,
                 ignore.case, perl, fixed, useBytes)
    }
    return(x)
}

# returns parent directory.
#' @export
getParent = function(step = 1){
    wd = getwd()
    for (i in 1:step){
        wd = dirname(wd)
    }
    return(wd)
}

#merges lists by their common names. adds non common ones.
#' @export
mergeList = function(aList,bList,forceUnique=T){
    allNames = unique(c(names(aList),names(bList)))
    outList = vector(mode= "list",length = length(allNames))
    names(outList) = allNames
    outList = sapply(allNames,function(x){
        out=(c(aList[[x]],bList[[x]]))
        if (forceUnique){
            out = unique(out)
        }
        return(out)
    })
    return(outList)
}

# seeks for a given object in a single layered list
#' @export
findInList = function(object, aList){
    indices = vector()
    for (i in 1:length(aList)){
        if (object %in% aList[[i]]){
            indices = c(indices, i)
        }
    }
    return(indices)
}

# counts total no of elements in a single layered list
#' @export
listCount = function(aList){
    length(unlist(aList))
}

# removes NAs in a vector by shortening it
#' @export
trimNAs = function(aVector) {
    return(aVector[!is.na(aVector)])
}

# removes given elements in a vector by shortening it
#' @export
trimElement = function (aVector,e){
    return(aVector[!(aVector %in% e)])
}

# finds total depth of a list which is assumed to be symmetrical
#' @export
listDepth = function(deList){
    step = 1
    while (T){
        if (typeof(eval( parse(text = paste(c("deList",rep('[[1]]',step)),sep='',collapse = '')))) != "list"){
            return(step)
        }
        step = step +1
    }
}

#source
#http://www.r-bloggers.com/a-quick-way-to-do-row-repeat-and-col-repeat-rep-row-rep-col/
#' @export
repRow<-function(x,n){
    matrix(rep(x,each=n),nrow=n)
}
repCol<-function(x,n){
    matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

# repeats every element in the vector n times
#' @export
repIndiv = function (aVector, n){
    output = vector(length = length(aVector) * n)
    step = 1
    for (i in aVector){
        output[(step * n - n + 1):(n * step)] = rep(i, n)
        step = step + 1
    }
    return(output)
}

# http://stackoverflow.com/questions/6513378/create-a-variable-capturing-the-most-frequent-occurence-by-group
#' @export
mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# load that bloody function no matter what
# doesn't seems to work all that well...
#' @export
insist = function(name){
    name = substitute(name)
    name = as.character(name)
    if (!require(name, character.only = T)) {
        install.packages(name)
        Sys.sleep(5)
        library(name, character.only = T, logical.return = F)
    }
}



#direct text eval
#' @export
teval = function(daString){
    eval(parse(text=daString))
}


# for navigating through list of lists with teval
#' @export
listParse = function (daList,daArray){
    out = ''
    for (i in daArray){
        out = paste0(out , '[[' ,  daArray[i] , ']]')
    }
    eval(parse(text=paste0('daList' , out)))
}

#returns the final step as a list
#' @export
listParseW = function (daList,daArray){
    out = ''
    if (length(daArray) > 1){
        for (i in daArray[1 : (length(daArray) - 1)]){
            out = paste0(out , '[[' ,  i , ']]')
        }
    }
    out = paste0(out ,'[', daArray[length(daArray)], ']')
    eval(parse(text=paste0('daList' , out)))
}

# sets the list element
#' @export
listSet = function(daList,daArray ,something){
    name = substitute(daList)
    name = as.character(name)
    out = ''
    for (i in daArray){
        out = paste0(out , '[[' ,  i , ']]')
    }
    
    eval(parse(text = paste0(name, out, '<<-something')))
}

# > listStr(c(1,2,3))
# [1] "[[1]][[2]][[3]]"
#' @export
listStr = function(daArray){
    out = ''
    for (i in daArray[1 : length(daArray)]){
        
        out = paste0(out, '[[',  i, ']]')
    }
    return(out)
}

# the last element is returned with a singe "["
# > listStrW(c(1,2,3))
# [1] "[[1]][[2]][3]"
#' @export
listStrW = function(daArray){
    out = ''
    if (length(daArray) > 1){
        for (i in daArray[1 : (length(daArray) - 1)]){
            out = paste0(out,'[[',i, ']]')
        }
    }
    out = paste0(out,'[', daArray[length(daArray)],']')
    return(out)
}



#concatanate to preallocated. initiate vectors with NAs to make it work
#' @export
"%c%" = function (x, y){
    start = which(is.na(x))[1]
    x[start:(start+length(y) - 1)]= y
    return(x)
}



# turn every member of daList to a color from the palette
#' @export
toColor = function(daList, palette = NULL){
    daList = as.factor(daList)
    uniq = unique(daList)
    if (is.null(palette[1])){
        palette = rainbow(length(uniq))
    }
    if (is.null(names(palette))){
        names(palette) = uniq
    }
    cols = vector (length = length(daList))
    #to match palette names to uniq names so that custom naming is possible
    if (!is.null(names(palette))){
        palette = trimNAs(palette[match(uniq,names(palette))])
        names(palette) = uniq
    }
    
    for (i in 1:length(uniq)){
        cols[daList == uniq[i]]= palette[i]
    }
    out = list()
    out$cols = cols
    out$palette = palette
    
    return(out)
}

# creates a color gradient from a continuous variable. returns assigned color values and the legend
#' @export
toColorGrad = function(daList, startCol = 'white', endCol = 'red', fine = 0.01){
    colorGrad =  colorRampPalette(c(startCol, endCol))
    colorLegend = data.frame(value=seq(min(daList),max(daList),fine),
                             color=colorGrad(len(seq(min(daList),max(daList),fine))))
    
    colors = colorLegend$color[
        sapply(daList,function(x){
            which(abs(colorLegend$value-x) == min(abs(colorLegend$value-x)))
        })]
    
    out = list(cols = colors, palette = colorLegend)
    return(out)
}

#to use with ggplot violins. adapted from http://stackoverflow.com/questions/17319487/median-and-quartile-on-violin-plots-in-ggplot2
#' @export
median.quartile <- function(x){
    out <- quantile(x, probs = c(0.25,0.5,0.75))
    ICR = out[3] - out[1]
    out = c(out[1] - 1.5 * ICR ,out, out[3] + 1.5 * ICR)
    if (out[1] < min(x)){
        out[1] = min(x)
    }
    if (out[5] > max(x)){
        out[5] = max(x)
    }
    names(out) <- c("whisDown","ymin","y","ymax","whisUp")
    return(out)
}
#' @export
threeQuartile <- function(x){
    out <- quantile(x, probs = c(0.25,0.5,0.75))
    names(out) <- c("ymin","y","ymax")
    return(out)
}

# for intersecting more than one sets
#' @export
intersectMult = function (...){
    targets = list(...)
    
    out = intersect(targets[[1]],targets[[2]])
    if (length(targets)>=3){
        for (i in 3:(length(targets))){
            out = intersect(out, targets[[i]])
        }
    }
    return(out)
}
# does 0-1 scaling
#' @export
scale01 = function(x){
    x = (x - min(x))/(max(x)-min(x))
    return(x)
}

# to add a color gradient legend to plots
# https://aurelienmadouasse.wordpress.com/2012/01/13/legend-for-a-continuous-color-scale-in-r/
#' @export
legendGrad <- function(col, lev){
    
    opar <- par
    
    n <- length(col)
    
    bx <- par("usr")
    
    box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
                bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
    box.cy <- c(bx[3], bx[3])
    box.sy <- (bx[4] - bx[3]) / n
    
    xx <- rep(box.cx, each = 2)
    
    par(xpd = TRUE)
    for(i in 1:n){
        
        yy <- c(box.cy[1] + (box.sy * (i - 1)),
                box.cy[1] + (box.sy * (i)),
                box.cy[1] + (box.sy * (i)),
                box.cy[1] + (box.sy * (i - 1)))
        polygon(xx, yy, col = col[i], border = col[i])
        
    }
    par(new = TRUE)
    plot(0, 0, type = "n",
         ylim = c(min(lev), max(lev)),
         yaxt = "n", ylab = "",
         xaxt = "n", xlab = "",
         frame.plot = FALSE)
    axis(side = 4, las = 2, tick = FALSE, line = .25)
    par <- opar
}

# to use instead of "head" with bidimentially huge matrices
#' @export
corner = function(x){
    row = 10
    col = 10
    if (ncol(x)<10){
        col = ncol(x)
    }
    if (nrow(x)<10){
        row = nrow(x)
    }
    x[1:row,1:col]
}

# http://codegolf.stackexchange.com/questions/49671/do-you-want-to-code-a-snowman/49780#49780
#' @export
snowman = function(x){
    W =c("_===_"," ___\n .....","  _\n  /_\\"," ___\n (_*_)",",",".","_"," ",".","o","O","-"," ","\\"," "," ","<"," ","/"," "," ","/"," ","",">"," ","\\",""," : ","] [","> <","   "," : ","\" \"","___","   ")
    i=as.integer(strsplit(x,"")[[1]]);cat(" ",W[i[1]],"\n",W[i[5]+12],"(",W[i[3]+8],W[i[2]+4],W[i[4]+8],")",W[i[6]+20],"\n",W[i[5]+16],"(",W[i[7]+28],")",W[i[6]+24],"\n"," (",W[i[8]+32], ")",sep="")
}


# merges regex's with an or clause. search for multiple regexes
#' @export
regexMerge = function(regexList){
    paste0('(',paste0(regexList,collapse=')|('),')')
}

# http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
#' @export
trimWS <- function (x) gsub("^\\s+|\\s+$", "", x)

#' @export
coVar = function(x) ( 100*sd(x)/mean(x) )