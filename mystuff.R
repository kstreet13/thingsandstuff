##############################################
### Packages
require(RColorBrewer)
require(scales)
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){
    biocLite(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}
##############################################

##############################################
### Numeric
scaleAB <- function(x,a=0,b=1){
    ((x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE)))*(b-a)+a
}
matrixJitter <- function(mat, times = 1){
    for(i in seq_len(times)){
        mat <- apply(mat,2,jitter,amount=0)
    }
    return(mat)
}
d <- function(sides, n=1, sum=TRUE){
    roll <- floor(runif(n,max=sides)+1)
    if(sum){
        return(sum(roll))
    }
    return(roll)
}
lenu <- function(x){
    length(unique(x))
}
sortu <- function(x){
    sort(unique(x))
}
corner <- function(x, n = 6){
    x[1:min(c(n,nrow(x))),1:min(c(n,ncol(x)))]
}
##############################################

##############################################
### Plotting
RStudioGD <- function(){
  .Call("rs_createGD")
  graphics:::par(
       pch = 16, las = 1
  )
}
colorby <- function(x, alpha = 1, colors = NULL){
    if(class(x) %in% c('character','logical','factor')){
        x <- as.factor(x)
        if(is.null(colors) && lenu(x) <= 29){
            colors <- c(brewer.pal(9,'Set1'),brewer.pal(8,'Set2'),brewer.pal(12,'Set3'))[1:lenu(x)]
        }
    }
    if(class(x) %in% c('integer','numeric')){
        x <- cut(x, breaks = 100)
    }
    
    if(is.null(colors)){
        #colors <- brewer.pal(11,'Spectral')[-6]
        #colors <- c(brewer.pal(10,'Paired')[10],'grey85',brewer.pal(10,'Paired')[4])
        colors <- c(colorRampPalette(c(brewer.pal(9,'Set1')[4],rgb(0,0,0)))(100)[65], brewer.pal(9,'Set1')[2], colorRampPalette(c(brewer.pal(9,'Set1')[3],rgb(1,1,1)))(100)[65])
    }
    mypal <- colorRampPalette(colors)
    if(class(x)=='factor'){
        return(alpha(mypal(length(levels(x)))[x],alpha=alpha))
    }
}
##############################################
