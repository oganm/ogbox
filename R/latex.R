#' Prettify a latex table from knitr
#' @description Makes latex tables coming out of knitr prettier. Requires booktabs latex package on the file
#' @export
prettifyLatexKable = function(table,columnLines = FALSE,headerLine = TRUE){
    tableLines = table  %>% 
        stringr::str_replace_all(pattern = '\n\\\\hline','') %>%
        stringr::str_split('\n') %>% {.[[1]]} 
    
    tableTop = tableLines %>% 
    {grep(pattern = '\\\\begin\\{tabular\\}',x=.)}
    
    if(!columnLines){
        tableLines[tableTop] %<>% stringr::str_replace_all(pattern='\\|','')
    }
    
    tableBottom = tableLines %>% 
    {grep(pattern = '\\\\end\\{tabular\\}',x=.)}
    
    newTableLines = c(tableLines[1:tableTop],
                      '\\toprule',
                      tableLines[tableTop+1],
                      '\\midrule',
                      tableLines[(tableTop+2):(tableBottom-1)],
                      '\\bottomrule',
                      tableLines[tableBottom:length(tableLines)])
    
    outTable = newTableLines %>% paste(collapse ='\n')
    attributes(outTable) = attributes(table)
    return(outTable)
}


#' Creates a markdown link to the rendered form of a latex formula
#' @export
latexImg = function(latex, markdown=T){
    
    link = paste0('http://latex.codecogs.com/gif.latex?',
                  gsub('\\=','%3D',URLencode(latex)))
    
    # they need to be upper case
    link = gsub("(%..)","\\U\\1",link,perl=TRUE)
    if (!markdown){
        return(link)
    }
    return(paste0('![](',link,')'))
}


