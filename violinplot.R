violinplot <- function (x, ...) {
    UseMethod("violinplot", x)
}

violinplot.list <- function(x, col = 'grey50', names = NULL, show.median = TRUE, 
                            show.box = FALSE, box.big = 5, box.small = 2, bars = FALSE, 
                            add = FALSE, xlab='', ylab='', same.width = TRUE,
                            from = -Inf, to = Inf, col.pt = 'black', ...){
    
    col <- rep(col, length.out = length(x))
    col.pt <- rep(col.pt, length.out = length(x))
    xat <- 1:length(x)
    if(is.null(names)){
        if(is.null(names(x))){
            names <- 1:length(x)
        }else{
            names <- names(x)
        }
    }
    
    if(all(diff(unlist(x))==0)){ # degenerative case
        bw <- .05
    }else{
        bw <- 2 * density(unlist(x), na.rm=TRUE)$bw
    }
    
    if(all(is.finite(c(from,to)))){
        dens <- lapply(x, density, na.rm=TRUE, from = from, to = to, bw = bw)
    }
    if(all(is.infinite(c(from,to)))){
        dens <- lapply(x, density, na.rm=TRUE, bw = bw)
    }
    if(is.finite(from) & is.infinite(to)){
        dens <- lapply(x, density, na.rm=TRUE, from = from, bw = bw)
    }
    if(is.infinite(from) & is.finite(to)){
        dens <- lapply(x, density, na.rm=TRUE, to = to, bw = bw)
    }
    xs <- sapply(dens,function(d){d$x})
    ys <- sapply(dens,function(d){d$y})
    
    if(same.width){
        ys <- .5 * apply(ys, 2, function(x){ x/max(x) })
    }else{
        ys <- ys * .5 / max(ys,na.rm=TRUE)
    }
    
    if(!add){
        plot(c(xat[1]-.4,xat[length(x)]+.4), range(c(0,1,xs),na.rm = TRUE), col='white', xaxt = 'n', xlab=xlab, ylab=ylab, ...)
        axis(1, at=xat, labels = names, ...)
    }
    if(bars){
        abline(v=xat)
    }
    for(i in 1:length(x)){
        xx <- c(xat[i] - ys[,i], xat[i] + rev(ys[,i]))
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

violinplot.by <- function(x, ...){
    violinplot.list(x, ...)
}

violinplot.matrix <- function(x, ...){
    violinplot.list(as.list(data.frame(x)), ...)
}