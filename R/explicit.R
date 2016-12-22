#' Make function calls explicit
#' 
#' This function turns function calls into explicit function calls (packageName::functionName) in a file
#' The packages must be currently loaded
#' @param fileIn input file
#' @param fileOut output file. default is to overwrite fileIn
#' @param ignore which packages to ignore when translating
#' @export
explicit = function(fileIn,fileOut=fileIn,
                    ignore = 'base'){

    readLines(fileIn) %>% paste(collapse='\n')
    file = readLines(fileIn)
    
    parseData = file %>% paste(collapse='\n') %>% parse(text = .) %>% getParseData %>% 
        filter(token %in% c('SYMBOL_PACKAGE','NS_GET','SYMBOL_FUNCTION_CALL'))
    
    # filter the functions that are already explicit
    alreadyExplicitPackage = parseData$token %in% 'SYMBOL_PACKAGE' %>% which 
    alreadyExplicitNS = parseData$token %in% 'NS_GET' %>% which
    assertthat::assert_that(all(alreadyExplicitPackage+1 == alreadyExplicitNS))
    alreadExplicitFunction = alreadyExplicitNS + 1
    alreadyExplicit = c(alreadyExplicitPackage,alreadyExplicitNS,alreadExplicitFunction)
    
    parseData = parseData[!1:nrow(parseData) %in% alreadyExplicit,]
    
    parseData$package = parseData$text %>%
        lapply(function(x){
            if(exists(x)){
                gsub("^.+namespace.(.+)>$", "\\1", capture.output(environment(get(x))))
            } else {
                # functions that are taken as arguments are parsed as SYMBOL_FUNCTION_CALLs but they don not exits
                # in the current environment
                'nonExist'
            }
        }) %>% unlist
    parseData %<>%
        filter((!package %in% c(ignore, 'nonExist', 'NULL'))&(!grepl('environment:',package)))
    
    out = file
    for (i in 1:nrow(parseData)){
        line = file[parseData[i,]$line1]
        out[parseData[i,]$line1] = paste0(substr(line,1,parseData[i,]$col1-1),
                        parseData[i,]$package,'::',
                        substr(line,parseData[i,]$col1,parseData[i,]$col2),
                        substr(line,parseData[i,]$col2+1,nchar(line)))
    }
    
    fileOut = file(fileOut,open = 'w')
    writeLines(out, fileOut)
    close(fileOut)
}