# source("C:/Workdir/Metabolomic/NMRProcFlow/dockerApps/nmrproc/dev/ReadWriteParams.R")

source("C:/Workdir/Metabolomic/NMRProcFlow/dockerApps/nmrproc/nmrspec/exec/libspec.R")

### Parse the section 'section' within the 'INI.file' file
#   Get the INI.list as an initial list to add or replace the couple of values (key=value)
Parse.INI <- function(INI.file, INI.list=list(), section="PROCPARAMS")
{
  connection <- file(INI.file)
  Lines  <- readLines(connection)
  close(connection)

  Lines <- chartr("[]", "==", Lines)  # change section headers

  connection <- textConnection(Lines)
  d <- read.table(connection, as.is = TRUE, sep = "=", fill = TRUE)
  close(connection)

  L <- d$V1 == ""                    # location of section breaks
  d <- subset(transform(d, V3 = V2[which(L)[cumsum(L)]])[1:3], V1 != "")
  d <- d[d$V3 == section,]

  #INI.list <- list()
  for( i in 1:dim(d)[1] ) {
       if (! is.na(suppressWarnings(as.numeric(d$V2[i])))) {
           eval(parse(text=paste0('INI.list$',d$V1[i], '<-', as.numeric(d$V2[i]))))
           next
       }
       if (! is.na(suppressWarnings(as.logical(d$V2[i])))) {
           eval(parse(text=paste0('INI.list$',d$V1[i], '<-', as.logical(d$V2[i]))))
           next
       }
       eval(parse(text=paste0('INI.list$',d$V1[i], '<-"', d$V2[i],'"')))
  }
  return(INI.list)
}


# processing steps done : BC + Zeroing + Alignment 
#datadir <- "C:/Workdir/Share/tmp/8e21bc463ae709284211a1012444ca"
datadir <- "C:/Workdir/Share/tmp/_379e57d0b7617bed9da7e2d27e00ae"

#--------------------------
# READ 
#--------------------------

# procParams
INI.file <- file.path(datadir,"SpecProcpar.ini")
procParams <- Parse.INI(INI.file, section="PROCPARAMS")
procParams$LOGFILE <- file.path(datadir,'info.log')

# spec.packc
specPackFile <- file.path(datadir,"specs.pack")
specMat <- readSpecMatrix(specPackFile)
specMat$dppm <- (specMat$ppm_max - specMat$ppm_min)/(specMat$size - 1)
specMat$ppm <- rev(seq(from=specMat$ppm_min, to=specMat$ppm_max, by=specMat$dppm))

# specParams
specParamFile <- file.path(datadir,"list_pars.csv")
specParamsDF <- read.table(specParamFile, header=T, sep=";", stringsAsFactors=FALSE)

# Get the list of all Raw directories
LIST <- read.table(file.path(datadir,'rawids.csv'), sep=';', stringsAsFactors=F)

# samples
samplesFile <- file.path(datadir,"samples.csv")
samples <- read.table(samplesFile, header=F, sep=";", stringsAsFactors=FALSE)

# factors
factorsFile <- file.path(datadir,"factors")
factors <- read.table(factorsFile, header=F, sep=";", stringsAsFactors=FALSE)

#source("C:/Workdir/Metabolomic/NMRProcFlow/dockerApps/nmrproc/dev/blcorrSpec.R")
#source("C:/Workdir/Metabolomic/NMRProcFlow/dockerApps/nmrproc/dev/alignSpec.R")

DATADIR <- datadir
bucketfile <- file.path(datadir,'bucket_list.in')

# Limit size of buckets
MAXBUCKETS<-700
DECAL <- 1
NOISE_FAC <- 3

   # exit if not PPM ranges file
   zonesfile <- file.path(DATADIR,'zones5_list.in')
   file.exists(zonesfile)

   # get the PPM ranges list
   zones <- read.table(zonesfile, header=T, sep="\t",stringsAsFactors=FALSE)
   zones$min <- zones[,1]-0.5*zones[,2]
   zones$max <- zones[,1]+0.5*zones[,2]

   # Noise estimation
   zone <- as.numeric(simplify2array(strsplit(procParams$PPMNOISERANGE, " ")))
   if (is.na(zone)) {
       PPM_NOISE_AREA <- c(10, 10.5)
   } else {
       PPM_NOISE_AREA <- c(min(zone), max(zone))
   }
   idx_Noise <- c( length(which(specMat$ppm>PPM_NOISE_AREA[2])),(which(specMat$ppm<=PPM_NOISE_AREA[1])[1]) )

   Vref <- C_spec_ref(specMat$int)
   ynoise <- C_noise_estimation(Vref,idx_Noise[1],idx_Noise[2])

   sum_y2 <- Vref[idx_Noise[1]:idx_Noise[2]]%*%Vref[idx_Noise[1]:idx_Noise[2]]
   sum2_vref <- Vref%*%Vref
   SNRDB <- 20*log10(sqrt(sum2_vref/length(Vref))/sqrt(sum_y2/(idx_Noise[2]-idx_Noise[1]+1)))

   bdata <- list()
   bdata$ynoise <- ynoise
   bdata$vnoise <- NULL
   bdata$inoise_start <- idx_Noise[1]
   bdata$inoise_end <- idx_Noise[2]
   bdata$R <- procParams$AIBIN_R
   bdata$dppm <- specMat$dppm
   bdata$noise_fac <- NOISE_FAC
   bdata$bin_fac <- 0.5
   bdata$peaknoise_rate <- 15
   bdata$BUCMIN <- 0.003
   bdata$VREF <- 1


   buckets_zones <- NULL
   buckets_IntVal <- NULL
   buckets_indx <- NULL
   N <- dim(zones)[1]
   for ( i in 1:N ) {
       i2<-which(specMat$ppm<=zones$min[i])[1]
       i1<-length(which(specMat$ppm>zones$max[i]))
       Mbuc <- matrix(, nrow = MAXBUCKETS, ncol = 2)
       Mbuc[] <- 0
       buckets_m <- C_aibin_buckets(specMat$int, Mbuc, Vref, bdata, i1, i2)
       buckets_int <- C_all_buckets_integrate (specMat$int, buckets_m, 0)

       buckets_zones  <- rbind( buckets_zones,  cbind( specMat$ppm[buckets_m[,1]+DECAL], specMat$ppm[buckets_m[,2]-DECAL] ) )
       buckets_IntVal <- cbind( buckets_IntVal, buckets_int )
       buckets_indx <- rbind( buckets_indx, buckets_m )
   }

   buckets_IntVal_CSN <- C_buckets_CSN_normalize( buckets_IntVal )
   VrefMed <- C_MedianSpec(specMat$int)
   bucVref_int <- C_buckets_integrate (VrefMed, buckets_indx, 0)
   bucVref_int_CSN <- 100*bucVref_int/sum(bucVref_int)
   bucRatio <- buckets_IntVal_CSN / bucVref_int_CSN
   Coeff <- apply(bucRatio,1,median)

   buckets_IntVal_PQN <- buckets_IntVal_CSN / Coeff


   M <- cbind( (buckets_zones[,1] + buckets_zones[,2])/2, abs(buckets_zones[,1] - buckets_zones[,2]))
   colnames(M) <- c('V1','V2')

   MatInt <- C_buckets_CSN_normalize (buckets_IntVal)

# source("C:/Workdir/Metabolomic/NMRProcFlow/dockerApps/nmrproc/nmrspec/exec/libspec.R")
# source("C:/Workdir/Metabolomic/NMRProcFlow/dockerApps/nmrproc/dev/ReadWriteParams.R")

