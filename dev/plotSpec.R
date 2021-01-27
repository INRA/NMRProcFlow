plotSpectrum <- function(spec, ppm = c(spec$ppm[1], spec$ppm[spec$spec$SI]), ratio=1, title="", 
                        reverse=TRUE, active=FALSE, col="blue", overlay=FALSE )
{
   spec.int <- spec$int

   # Graphic
   i1<-max(length(which(spec$ppm<=ppm[1])),1)
   i2<-length(which(spec$ppm<=ppm[2]))
   ymax <- max( spec.int[i1:i2]/ratio )
   ymin <- min( spec.int[spec.int<0], 0.0 )
   ymin <- ifelse(ymin>0.0, 0.0, max(1.1*ymin,-0.5*ymax))
   ppmlim <- ppm
   if(reverse==TRUE) ppmlim <- rev(ppm)
   if (overlay) {
      lines( spec$ppm[i1:i2], spec.int[i1:i2], col=col )
   } else {
      if (active==FALSE) x11()
      plot( cbind( spec$ppm[i1:i2], spec.int[i1:i2]),type="l", xlim=ppmlim, ylim=c(ymin,ymax),col=col, xlab="ppm", ylab="intensities", main=title)
      abline(h=0,v=0, col="red")
   }
}

plotSpecList <- function(specList, specMat, PPM=c(-0.5,11), RATIO=1, active=FALSE)
{
   col = sample(colours(), length(specList))
   for (i in 1:length(specList)) {
       spec <- specList[[i]]
       spec$int <- rev(specMat$int[i, ])
       spec$ppm <- rev(spec$ppm)
       if (i==1) {
          plotSpectrum(spec, ppm=PPM, ratio=RATIO, col=col[i], active=active, overlay=FALSE);
       } else {
          plotSpectrum(spec, ppm=PPM, ratio=RATIO, col=col[i], active=TRUE, overlay=TRUE);
       }
   }
}

library(MASS)
PPM_NOISE_AREA <- c(10, 10.5)

if (dim(specParamsDF)[1] == specMat$nspec) {
   specList <- lapply( c(1:specMat$nspec), function (x) {
       spec <- list()
       spec$pmin <- specMat$ppm_min
       spec$pmax <- specMat$ppm_max
       spec$dppm <- specMat$dppm
       spec$ppm <- specMat$ppm
       spec$param <- procParams
       spec$proc <- as.list(specParamsDF[x,])
       spec$proc$TD <- spec$proc$SI
       spec$proc$Spectrum <- as.character(spec$proc$Spectrum)
       spec$sig <- fitdistr(specMat$int[x,length(which(specMat$ppm>PPM_NOISE_AREA[2])):(which(specMat$ppm<=PPM_NOISE_AREA[1])[1])], "normal")$estimate[2]
       spec
   })
}
