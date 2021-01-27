#------------------------------
# Alignment of the selected PPM ranges
#------------------------------
RAlign1D <- function(specMat, zone, RELDECAL=0.35, idxSref=0, ProgressFile=NULL)
{
   # Alignment of each PPM range
   NBPASS <- 10
   if( !is.null(ProgressFile) ) fh<-file(ProgressFile,"wt"); writeLines(paste0("0;",NBPASS), fh); close(fh)
   i1 <- ifelse( max(zone)>=specMat$ppm_max, 1, length(which(specMat$ppm>max(zone))) )
   i2 <- ifelse( min(zone)<=specMat$ppm_min, specMat$size - 1, which(specMat$ppm<=min(zone))[1] )

   logf <- file.path(dirname(ProgressFile),'RAlign1D.log');
   Write.LOG(logf,paste("RAlign1D: i1=",i1,", i2=",i2,", nrow=",specMat$nspec,", ncol=",specMat$size, "\n"), mode="wt")

   decal <- round((i2-i1)*RELDECAL)
   for( n in 1:NBPASS) {

sink(logf, append=TRUE)
       V <- C_segment_shifts( specMat$int, idxSref, decal, i1-1, i2-1)
sink()
       #ret <- C_align_segment(specMx$int, idxSref, round((i2-i1)*RELDECAL), i1-1, i2-1)
       #C_scale_segment(specMx$int, 1.2, i1-1, i2-1)

Write.LOG(logf,paste("\nRAlign1D: NBPASS=",n,", MoyShift=",round(mean(V),2),"DECALMAX=",decal, "IDXSREF=",idxSref, " SUM=", sum(specMat$int[,c(i1:i2)]), "\n" ))

       if( !is.null(ProgressFile) ) fh<-file(ProgressFile,"wt"); writeLines(paste0(n,";",NBPASS), fh); close(fh)
   }

   return(specMat)
}
