violinplot <- function (x, ...) {
  UseMethod("violinplot", x)
}


violinplot.matrix <- function(x, col='grey50', names = 1:ncol(x), show.median = TRUE, 
                       show.box = FALSE, box.big = 5, box.small = 2, bars = FALSE, 
                       add = FALSE, xlab='', ylab='', width = 1, max.width = 1,
                       col.pt = 'black', ...){
  col <- rep(col, length.out = ncol(x))
  col.pt <- rep(col.pt, length.out = ncol(x))
  width <- rep(width, length.out = ncol(x))
  width <- max.width * width / mean(width)
  
  xat <- sapply(1:ncol(x),function(i){
    sum(rep(width,each=2)[1:(2*i-1)])/2
  })
  
  dens <- apply(x,2,density, na.rm=TRUE)
  xs <- sapply(dens,function(d){d$x})
  ys <- sapply(dens,function(d){d$y})
  
  ys <- ys * .5 * max.width/max(ys,na.rm=TRUE)
  
  if(!add){
    plot(c(xat[1]-.4*width[1],xat[ncol(x)]+.4*width[ncol(x)]), range(xs,na.rm = TRUE), col='white', xaxt = 'n', xlab=xlab, ylab=ylab, ...)
    axis(1, at=xat, labels = names, ...)
  }
  if(bars){
    abline(v=xat)
  }
  for(i in 1:ncol(x)){
    xx <- c(xat[i] - ys[,i] * width[i], xat[i] + rev(ys[,i]) * width[i])
    yy <- c(xs[,i], rev(xs[,i]))
    polygon(xx,yy, col = col[i])
  }
  if(show.box){
    stats <- boxplot(x, plot=FALSE)$stats
    segments(x0=xat,y0=stats[1,],y1=stats[5,], lwd=box.small, lend=2)
    segments(x0=xat,y0=stats[2,],y1=stats[4,], lwd=box.big, lend=2)
  }
  if(show.median){
    points(xat,apply(x,2,median,na.rm=TRUE), col=col.pt, pch=16)
    points(xat,apply(x,2,median,na.rm=TRUE), col='white', pch=1)
  }
  return(invisible(NULL))
}


violinplot.list <- function(x, col='grey50', names = 1:length(x), show.median = TRUE, 
                              show.box = FALSE, box.big = 5, box.small = 2, bars = FALSE, 
                              add = FALSE, xlab='', ylab='', width = 1, max.width = 1,
                            col.pt = 'black', ...){
  
  col <- rep(col, length.out = length(x))
  col.pt <- rep(col.pt, length.out = length(x))
  width <- rep(width, length.out = length(x))
  width <- width / mean(width)
  
  xat <- sapply(1:length(x),function(i){
    sum(rep(width,each=2)[1:(2*i-1)])/2
  })
  
  dens <- lapply(x, density, na.rm=TRUE)
  xs <- sapply(dens,function(d){d$x})
  ys <- sapply(dens,function(d){d$y})
  
  ys <- ys * .5 * max.width/max(ys,na.rm=TRUE)
  
  if(!add){
    plot(c(xat[1]-.4*width[1],xat[length(x)]+.4*width[length(x)]), range(xs,na.rm = TRUE), col='white', xaxt = 'n', xlab=xlab, ylab=ylab, ...)
    axis(1, at=xat, labels = names, ...)
  }
  if(bars){
    abline(v=xat)
  }
  for(i in 1:length(x)){
    xx <- c(xat[i] - ys[,i] * width[i], xat[i] + rev(ys[,i]) * width[i])
    yy <- c(xs[,i], rev(xs[,i]))
    polygon(xx,yy, col = col[i])
  }
  if(show.box){
    stats <- boxplot(x, plot=FALSE)$stats
    segments(x0=xat,y0=stats[1,],y1=stats[5,], lwd=box.small, lend=2)
    segments(x0=xat,y0=stats[2,],y1=stats[4,], lwd=box.big, lend=2)
  }
  if(show.median){
    points(xat,sapply(x,median,na.rm=TRUE), col=col.pt, pch=16)
    points(xat,sapply(x,median,na.rm=TRUE), col='white', pch=1)
  }
  return(invisible(NULL))
}


