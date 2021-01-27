
library(splines)
library(MASS)
PPM_NOISE_AREA <- c(10, 10.5)

procParams$WINDOWSIZE <- 64
procParams$SMOOTHSIZE <- 32

zone <- as.numeric(simplify2array(strsplit(procParams$PPMBCRANGE, " ")))
if (is.na(zone)) {
   i1 <- 1
   i2 <- specMat$size - 1
} else {
   i1<-length(which(specMat$ppm>max(zone)))
   i2<-which(specMat$ppm<=min(zone))[1]
}

   # Ajust the window size parameter
if (procParams$BCMETH=='bclocmin') {
   n <- i2-i1+1
   ws <- procParams$WINDOWSIZE
   dws <- 0
   signdws <- ifelse ( n/ws>round(n/ws), 1, -1 )
   dmin <- 1
   repeat {
       d <- abs(n/(ws+signdws*dws)-round(n/(ws+signdws*dws)))
       if (d>dmin) { dws <- dws - signdws; break }
       dmin <- d; dws <- dws + signdws;
   }
   procParams$WINDOWSIZE <- ws+signdws*dws
   n2 <- round(n/(ws+signdws*dws))*(ws+signdws*dws)
   if (n2<n) i2 <- i2 - (n-n2)
}

i <- 1

specSig <- fitdistr(specMat$int[i,length(which(specMat$ppm>PPM_NOISE_AREA[2])):(which(specMat$ppm<=PPM_NOISE_AREA[1])[1])], "normal")$estimate[2]

BL <- rep(0,length(specMat$int[i,]))

if (procParams$BCMETH=='bclocmin') {
   x <- specMat$int[i,c(i1:i2)]
   xmat <- matrix(x, nrow=procParams$WINDOWSIZE)
   ymin <- apply(xmat, 2, min) + 1.5*specSig
   y1 <- rep(ymin, each = procParams$WINDOWSIZE)
   if (procParams$SMOOTHSIZE>0) {
       y2 <- Smooth(y1,procParams$SMOOTHSIZE)
       BL[i1:i2] <- simplify2array(lapply(c(1:length(x)), function(k) { min(y1[k], y2[k]); }))
   } else {
       BL[i1:i2] <- y1[1:length(x)]
   }


   alpha <- 0.2
   y1 <- BL[i1:i2]
   y2 <- y1
   for (i in 2:length(y1)) y2[i] <- y2[i-1] + alpha * (y1[i] - y2[i-1])

   plot(x, type="l", col="magenta", ylim=c(min(ymin), max(ymin)) )
   lines(BL[i1:i2], col="grey")
   lines(y2, col="orange")
   lines( x-y2, col="blue" )
   abline(h=0, col="red")
}
