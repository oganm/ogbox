#' @export
remove_table_numbers = function(table){
    old_attributes = attributes(table)
    table %<>% as.character() %>% gsub("\\(\\\\#tab:.*?\\)","",.)
    attributes(table) = old_attributes
    return(table)
}

#' Trim first line comments
#' @description Deletes the comments added to the first lines. Useful when comment char is 
#' only a comment char in the beginning of the file
#' @param fileName name of the file or a connection
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



#' Clear parent environment
#' @export
purge =   function() {
    rm(list = ls(parent.frame(), all.names = T), envir = parent.frame())
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
# if n is a vector repeat every element as specified in the corresponding element of n
#' @export
repIndiv = function (aVector, n){
    if (length(n) ==1){
        n = rep(n,length(aVector))
    }
    output = vector(length = sum(n))
    for (i in 1:length(aVector)){
        output[(sum(n[(i-1):0])+1):(sum(n[i:0]))] = rep(aVector[i], n[i])
    }
    
    return(output)
}

# http://stackoverflow.com/questions/6513378/create-a-variable-capturing-the-most-frequent-occurence-by-group
#' @export
modStat <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# load that bloody function no matter what
# doesn't seems to work all that well...
#' @export
insist = function(name,...){
    name = substitute(name)
    name = as.character(name)
    if (!require(name, character.only = T)) {
        install.packages(name,...)
        Sys.sleep(5)
        library(name, character.only = T, logical.return = F)
    }
}



#direct text eval
#' @export
teval = function(x,envir = parent.frame(),
                 enclos = if(is.list(envir) || is.pairlist(envir))
                     parent.frame() else baseenv()){
    eval(parse(text=x),envir,enclos)
}



#concatanate to preallocated. initiate vectors with NAs to make it work
#' @export
"%con%" = function (x, y){
    start = which(is.na(x))[1]
    x[start:(start+length(y) - 1)]= y
    return(x)
}



# turn every member of daList to a color from the palette
#' @export
toColor = function(vector, palette = NULL,NAcolor = 'white'){
    if(class(vector) == "factor"){
        vector = as.character(vector)
    }
    if(!is.null(palette) & !is.null(names(palette))){
        assertthat::assert_that(all(vector %in%names(palette)))
    }
    if(is.null(names(palette)) & !is.null(palette)){
        names(palette) = unique(vector)
    }
    out = replaceElement(vector,dictionary = palette,NAreplace = NAcolor)
    names(out) = c('cols','palette')
    return(out)
}

#' Replaces elements of a vector based on the dictonary provided
#' @param vector a vector
#' @param dictionary a named vector or just a vector if labels are provided
#' @param labels a character vector
#' @param NAreplace that to put instead of NAs
#' @return A list with $newVector and $dictionary.
#' @export
replaceElement = function(vector, dictionary = NULL,labels = NULL, NAreplace = NA){
    if(class(vector) == "factor"){
        vector = as.character(vector)
    }
    #vector = as.factor(vector)
    uniq = unique(vector) %>% trimNAs
    if (is.null(dictionary[1])){
        dictionary = rainbow(length(uniq))
    }
    
    if(is.null(labels) & !is.null(names(dictionary))){
        labels = names(dictionary)
    } else if(is.null(labels) & is.null(names(dictionary))){
        labels = uniq
    }
    
    cols = vector(length = length(vector))
    #to match palette names to uniq names so that custom naming is possible
    # dictionary = dictionary[trimNAs(match(uniq,labels))]
    #names(dictionary) = uniq
    
    
    for (i in 1:length(dictionary)){
        vector[vector == labels[i]]= dictionary[i]
    }
    
    vector[is.na(vector)] = NAreplace
    
    if(length(dictionary)>0){
        names(dictionary) = labels
    }
    out = list(newVector = vector , dictionary = dictionary)
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
medianQuartile <- function(x){
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
intersectMult = function (..., list = NULL){
    if(is.null(list)){
        targets = list(...)
    } else{
        assertthat::assert_that(class(list) == 'list')
        targets = list
    }
    out = intersect(targets[[1]],targets[[2]])
    if (length(targets)>=3){
        for (i in 3:(length(targets))){
            out = intersect(out, targets[[i]])
        }
    }
    return(out)
}

#' @export
intersectList = function(list){
    intersectMult(list = list)
}

#' @export
cbindMult = function(..., list){
    if(is.null(list)){
        targets = list(...)
    } else{
        assertthat::assert_that(class(list) == 'list')
        targets = list
    }
    out = cbind(targets[[1]],targets[[2]])
    if (length(targets)>=3){
        for (i in 3:(length(targets))){
            out = cbind(out, targets[[i]])
        }
    }
    return(out)
}


#' @export
rbindMult = function(..., list){
    if(is.null(list)){
        targets = list(...)
    } else{
        assertthat::assert_that(class(list) == 'list')
        targets = list
    }
    out = rbind(targets[[1]],targets[[2]])
    if (length(targets)>=3){
        for (i in 3:(length(targets))){
            out = rbind(out, targets[[i]])
        }
    }
    return(out)
}



# does 0-1 scaling
#' @export
scale01 = function(x){
    scaleToInt(x,1,0)
}

# scale to intervals
#' @export
scaleToInt = function(x, max,min){
    scaleFun = scaleIntervals(max(x,na.rm = TRUE),min(x, na.rm=TRUE),max,min)
    scaleFun(x)
}

#' @export
scaleIntervals = function(max,min,maxOut,minOut){
    a = (max - min)/(maxOut - minOut)
    b = max - maxOut*a
    if(a != 0){
        return(teval(paste0("function(x){(x - ",b,")/",a,'}')))
    }else{
        mean = (maxOut - minOut)/2
        return(teval(paste0("function(x){x[] = ",mean,";return(x)}")))
    }
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
corner = function(x,row = 10,col = row){
    if (ncol(x)<col){
        col = ncol(x)
    }
    if (nrow(x)<row){
        row = nrow(x)
    }
    x[seq_len(row),seq_len(col)]
}

# http://codegolf.stackexchange.com/questions/49671/do-you-want-to-code-a-snowman/49780#49780
#' @export
snowman = function(x){
    W =c("_===_"," ___\n .....","  _\n  /_\\"," ___\n (_*_)",",",".","_"," ",".","o","O","-"," ","\\"," "," ","<"," ","/"," "," ","/"," ","",">"," ","\\",""," : ","] [","> <","   "," : ","\" \"","___","   ")
    i=as.integer(strsplit(x,"")[[1]]);cat(" ",W[i[1]],"\n",W[i[5]+12],"(",W[i[3]+8],W[i[2]+4],W[i[4]+8],")",W[i[6]+20],"\n",W[i[5]+16],"(",W[i[7]+28],")",W[i[6]+24],"\n"," (",W[i[8]+32], ")",sep="")
}


# merges regex's with an or clause. search for multiple regexes
#' @export
regexMerge = function(regexList, exact = FALSE){
    assertthat::assert_that(is.logical(exact))
    exact = as.character(exact)
    out = switch (exact,
        'FALSE' = {paste0('(',paste0(regexList,collapse=')|('),')')},
        'TRUE' =  {paste0('(\\Q',paste0(regexList,collapse='\\E)|(\\Q'),'\\E)')}
    )
    out = paste0('(',out,')')
}

# http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
#' @export
trimWS <- function (x) gsub("^\\s+|\\s+$", "", x)

#' @export
coVar = function(x) ( 100*sd(x)/mean(x) )

# bordered circles for igraph 
# http://lists.gnu.org/archive/html/igraph-help/2013-03/msg00030.html
#' @export
mycircle <- function(coords, v=NULL, params) {
    vertex.color <- params("vertex", "color")
    if (length(vertex.color) != 1 && !is.null(v)) {
        vertex.color <- vertex.color[v]
    }
    vertex.size  <- 1/200 * params("vertex", "size")
    if (length(vertex.size) != 1 && !is.null(v)) {
        vertex.size <- vertex.size[v]
    }
    vertex.frame.color <- params("vertex", "frame.color")
    if (length(vertex.frame.color) != 1 && !is.null(v)) {
        vertex.frame.color <- vertex.frame.color[v]
    }
    vertex.frame.width <- params("vertex", "frame.width")
    if (length(vertex.frame.width) != 1 && !is.null(v)) {
        vertex.frame.width <- vertex.frame.width[v]
    }
    
    mapply(coords[,1], coords[,2], vertex.color, vertex.frame.color,
           vertex.size, vertex.frame.width,
           FUN=function(x, y, bg, fg, size, lwd) {
               symbols(x=x, y=y, bg=bg, fg=fg, lwd=lwd,
                       circles=size, add=TRUE, inches=FALSE)
           })
}

#' A violin plot overlayed by a boxplot
#' 
#' This function returns a list of ggplot elements that overlays a boxplot over
#' a violin plot.
#' 
#' @param data Default dataset to use for plot. If not already a data.frame, 
#' will be converted to one by \link[ggplot2]{fortify}. If not specified, must
#' be suppled in each layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot. If not 
#' specified, must be suppled in each layer added to the plot.
#'  
#' 
#' @export
geom_ogboxvio = function(data=NULL, mapping = NULL){
   list(ggplot2::geom_violin(color="#C4C4C4", fill="#C4C4C4",data=data, mapping = mapping),
        ggplot2::geom_boxplot(width=0.1,fill = 'lightblue',data=data, mapping = mapping), 
        cowplot::theme_cowplot(),
        ggplot2::theme(axis.text.x  = ggplot2::element_text(size=25),
              axis.title.y = ggplot2::element_text(vjust=0.5, size=25),
              axis.title.x = ggplot2::element_text(vjust=0.5, size=0) ,
              title = ggplot2::element_text(vjust=0.5, size=25),
              axis.text.y = ggplot2::element_text(size = 13)))
}

#' @export
geom_signif = function(pValues,maxY, size = 7){
    if (length(maxY)==1){
        maxY=rep(maxY,length(pValues))
    }
    markers = signifMarker(pValues)
    return(
        lapply(1:length(pValues), function(i){
            ggplot2::annotate('text',x=i,y=maxY[i]+max(maxY)/10,label = markers[i],size =size )
            })
    )
}


#' @export
signifMarker = function(pValues){
    markers = rep('',length(pValues))
    markers[pValues>=0.01 & pValues<0.05] = '*'
    markers[pValues>=0.001 & pValues<0.01] = '**'
    markers[pValues<0.001 ] = '***'
    return(markers)
}

# https://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
#' @author Achim Zeileis
#' @author Kurt Hornik
#' @author Paul Murrell
#' @export
pal <- function(col, border = "transparent", ...)
{
    n <- length(col)
    plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
         axes = FALSE, xlab = "", ylab = "", ...)
    rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

# sets rownames to first collumn
#' @export
rn2col = function(frame){
    frame = cbind(rownames(frame),frame)
    frame[,1] = as.character(frame[,1])
    names(frame)[1] = 'rownames'
    return(frame)
}

# sets first collumn to row names, drops the collumn
#' @export
col2rn = function(frame){
    rownames(frame) = frame[,1]
    frame = frame[,-1,drop=F]
    return(frame)
}


# #' @export
# mergeMutliFrame = function(list, ...){
#     out = merge(list[[1]],list[[2]],...)
#     for (i in 3:len(list)){
#         out = merge(out,list[[i]],...)
#     }
#     return(out)
# }



# #' @export
# "+" = function(x,y) {
#     if(is.character(x) || is.character(y)) {
#         return(paste(x , y, sep=""))
#     } else {
#         .Primitive("+")(x,y)
#     }
# }


#' Non standardly evaluated \%in\%
#' @description First value provided is non standardly evaluated and searched within the second value
#' @export
`%tin%` = function(x,y){
    x = substitute(x)
    x = as.character(x)
    x %in% y
}

#' @export
qNormToValues = function(x,values,uniquelyOrdered = FALSE ,...){
    values %<>% sort
    if (uniquelyOrdered){
        x_unique <- unique(x)
        if(length(values) != length(x_unique)){
            warning('number of values and input is not equal. which is fine...')
            values = values[seq(1,length(values),length.out = length(x_unique)) %>% round]
        }
        x_ranks <- rank(x_unique,...)
        
        x_ranks[match(x,x_unique)] %>% {values[.]}
    } else{
        
        if(length(values) != length(x)){
            warning('number of values and input is not equal. which is fine...')
            values = values[seq(1,length(values),length.out = length(x)) %>% round]
        }
        x %>% rank(...) %>% {values[.]}
    }
}


# adapted from
# https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html
#' Roxygen table maker
#' 
#' @param df data.frame
#' @param col.names logica. If colnames should be included
#' @param ... variables for format function
#'
#' @export
roxygenTabular <- function(df,col.names= TRUE,  ...) {
    stopifnot(is.data.frame(df))
    
    align <- function(x) if (is.numeric(x)) "r" else "l"
    col_align <- vapply(df, align, character(1))
    
    if(col.names){
        df = rbind(paste0('\\strong{',colnames(df),'}'),df)
    }
    
    cols <- lapply(df, format, ...)
    contents <- do.call("paste",
                        c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))
    
    paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
          contents, "\n}\n", sep = "")
}

#' @export
catFile = function(file){
    readLines(file) %>%
        cat(sep = '\n')
}

#' @export
set_file_wd = function(){
    command = commandArgs(trailingOnly = FALSE)
    
    file = gsub('--file=','',command[grepl('--file',command)])
    if(length(file) == 1){
        setwd(dirname(file))
    }
}