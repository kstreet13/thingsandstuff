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
colorby <- function(x, alpha = 1, colors = NULL, diverging = FALSE, symmDiv = TRUE){
  if(diverging){
    if(! class(x) %in% c('integer','numeric')) stop("'diverging' can only be used with numeric variables")
    if(is.null(colors)){
      colors <- c(brewer.pal(5,'Set1')[4], 'white', brewer.pal(5,'Set1')[3])
    }else{
      colors <- colors[1:3]
    }
    if(length(alpha) == 1){
      alpha <- rep(alpha, length(x))
    }
    cc <- rep('', length(x))
    posind <- x >= 0
    if(symmDiv){
      m <- max(abs(x))
      cc[which(!posind)] <- colorby(c(-m,0,x[which(!posind)]),
                                    colors = colors[1:2],
                                    diverging = FALSE)[-c(1:2)]
      cc[which(posind)] <- colorby(c(0,m,x[which(posind)]),
                                   colors = colors[2:3],
                                   diverging = FALSE)[-c(1:2)]
    }else{
      cc[which(!posind)] <- colorby(x[which(!posind)],
                                    colors = colors[1:2],
                                    diverging = FALSE)
      cc[which(posind)] <- colorby(x[which(posind)],
                                   colors = colors[2:3],
                                   diverging = FALSE)
    }
    return(alpha(cc, alpha=alpha))
  }
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
lgd <- function(x, alpha = 1, colors = NULL){
  if(class(x) %in% c('character','logical','factor')){
    cc <- colorby(x, alpha = alpha, colors = colors)
    lgd <- unique(cbind(x, cc))
  }
  if(class(x) %in% c('integer','numeric')){
    summaryx <- c(min(x),
                  .75*min(x)+.25*max(x),
                  .5*min(x)+.5*max(x),
                  .25*min(x)+.75*max(x),
                  max(x))
    cc <- colorby(c(summaryx, x), alpha = alpha, colors = colors)
    lgd <- cbind(round(summaryx, digits = 2), cc[1:5])
  }
  return(lgd)
}
legendby <- function(x, alpha = 1, colors = NULL, 
                     bty = 'n', pch = 16, pos = 'topleft', ...){
  cc <- colorby(x, alpha = alpha, colors = colors)
  lgd <- lgd(x, alpha = alpha, colors = NULL)
  legend(x = pos, legend = lgd[,1], col = lgd[,2], bty = bty, pch = pch, ...)
}
##############################################
