
#PPM_to_ALIGN <- c( 8.22 , 8.24  ); RATIO <- 1
#PPM_to_ALIGN <- c( 7.759 , 7.797 ); RATIO <- 1
PPM_to_ALIGN <- c( 8.25 , 8.28 ); RATIO <- 0.4


source("C:/Workdir/Metabolomic/NMRProcFlow/dockerApps/nmrproc/dev/plotSpec.R")
plotSpecList(specList,specMat, PPM=PPM_to_ALIGN, RATIO=RATIO)

zone <- PPM_to_ALIGN
i1<-length(which(specMat$ppm>max(zone)))
i2<-which(specMat$ppm<=min(zone))[1]

NBPASS <- 2
for( i in 1:NBPASS) {
    V <- C_segment_shifts (specMat$int, i1-1, i2-1)
    ret <- C_align_segment (specMat$int, V, i1-1, i2-1)
    cat("PASS ",i,": ", paste(V,collapse=" "), "\n")
}

source("C:/Workdir/Metabolomic/NMRProcFlow/dockerApps/nmrproc/dev/plotSpec.R")
plotSpecList(specList,specMat, PPM=PPM_to_ALIGN, RATIO=RATIO)

