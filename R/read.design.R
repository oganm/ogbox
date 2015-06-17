read.design <-
function(x){
    read.table(x,header=T,sep='\t',stringsAsFactors=F,quote="")
}
