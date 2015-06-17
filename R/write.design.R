write.design <-
function(x, file){
    write.table(x,file= file, sep = '\t', quote=F, row.names = F)
}
