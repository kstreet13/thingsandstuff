
set.seed(1)
x <- rbind(
	matrix(rnorm(20*2, mean = c(2,2)), ncol = 2, byrow = TRUE),
	matrix(rnorm(20*2, mean = c(2,-2)), ncol = 2, byrow = TRUE),
	matrix(rnorm(20*2, mean = c(-2,2)), ncol = 2, byrow = TRUE),
	matrix(rnorm(20*2, mean = c(-2,-2)), ncol = 2, byrow = TRUE),
	matrix(runif(20*2, min = -5, max = 5), ncol = 2, byrow = TRUE)
)
cc <- rep(brewer.pal(5,'Set1'), each=20)

plot(x, asp=1, col=cc)

km5 <- factor(kmeans(x, centers = 5)$cluster)
km6 <- factor(kmeans(x, centers = 6)$cluster)
km7 <- factor(kmeans(x, centers = 7)$cluster)

plot(x, asp=1, col = colorby(km5))
plot(x, asp=1, col = colorby(km6))
plot(x, asp=1, col = colorby(km7))




cluslist <- list(km5, km6, km7)

calcARIbar <- function(cluslist){
	aris <- unlist(sapply(1:(length(cluslist)-1), function(i){
		sapply((i+1):length(cluslist), function(j){
			mclust::adjustedRandIndex(cluslist[[i]], cluslist[[j]])
		})
	}))
	return(mean(aris))
}

merges <- lapply(1:length(cluslist), function(cli){
	cl <- cluslist[[cli]]
	clusnames <- sort(unique(cl))
	mergemat <- matrix(0, nrow = length(clusnames), ncol = length(clusnames))
	rownames(mergemat) <- clusnames
	colnames(mergemat) <- clusnames
	ari.current <- calcARIbar(cluslist)
	
	for(i in 1:(length(clusnames)-1)){
		for(j in (i+1):length(clusnames)){
			cl.i <- cl
			cl.i[which(cl==clusnames[j])] <- clusnames[i]
			cluslist.i <- cluslist
			cluslist.i[[cli]] <- cl.i
			mergemat[i,j] <- calcARIbar(cluslist.i) - ari.current
		}
	}
	mergemat <- mergemat + t(mergemat)
	return(mergemat)
})





triangleplot <- function(x, clus=NULL){
	if(!isSymmetric(x)){
		stop('x is not symmetric!')
	}
	diag(x) <- mean(x)
	
	cc <- matrix(colorby(as.numeric(x), diverging = TRUE), ncol = ncol(x))
	diag(cc) <- 'grey95'
	
	sizes <- table(factor(clus)) / length(clus)
	at <- c(0, cumsum(sizes))
	
	plot(0:1, c(0,-.5), asp=1, col='transparent')
	for(i in 1:length(sizes)){
		for(j in i:length(sizes)){
			if(i==j){
				polygon(x=c(at[i], at[i+1], mean(at[c(i,i+1)])),
						y=c(0,0, -(sizes[i])/2),
						col=cc[i,j])
			}else{
				polygon(x=c(mean(at[c(i,j)]),
							mean(at[c(i+1,j)]),
							mean(at[c(i+1,j+1)]),
							mean(at[c(i,j+1)])),
						y=c(-(at[j]-at[i])/2,
							-(at[j]-at[i+1])/2,
							-(at[j+1]-at[i+1])/2,
							-(at[j+1]-at[i])/2 ),
						col=cc[i,j])
			}
		}
	}
	text(x = at[-length(at)] + .5*sizes, y = rep(.05, length(sizes)),
		 labels = sort(unique(clus)))
}

triangleplot(merges[[3]], clus=cluslist[[3]])
triangleplot(merges[[3]], clus=1:7)
