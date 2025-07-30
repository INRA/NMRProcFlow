#------------------------------------------------
# ID Rnmr.R
# Rnmr1D package: Build 1r spectrum from FID file (Bruker/RS2D/Varian/Jeol/nmrML)
# Project: NMRProcFlow
# (C) 2015-2025 - D. JACOB - IMRAE
#------------------------------------------------

#' Spec1rDoProc
#'
#' \code{Spec1rDoProc} belongs to the low-level functions group - it processes only one raw spectrum at time.
#' @param Input Full directory path of the raw spectrum
#' @param param a Spec1rProcpar list
#' @return spec object
Spec1rDoProc <- function(Input, param=Spec1rProcpar)
{
   .CALL(Input, param)
}


#' Spec1rProcpar
#'
#' Initialize Parameter Lists by the default ones
#' @return
#' \itemize{
#'   \item \code{DEBUG} : Debug - defaut value = TRUE
#'   \item \code{LOGFILE} : Messages output file - default value = ""
#'   \item \code{VENDOR} : Instrumental origin of the raw data (bruker, varian, jeol, rs2d) - default value = 'bruker'
#'   \item \code{READ_RAW_ONLY} : Read raw file only; do not carry out processing; raw file is depending on INPUT_SIGNAL - default value = FALSE
#'   \item \code{INPUT_SIGNAL} : What type of input signal: 'fid' or '1r' - default value = 'fid'
#'   \item \code{PDATA_DIR} : subdirectory containing the 1r file (bruker's format only) - default value = 'pdata/1'
#'   \item \code{LB} : Exponantial Line Broadening parameter - default value = 0.3
#'   \item \code{GB} : Gaussian Line Broadening parameter - default value = 0
#'   \item \code{REMLFREQ} : Remove low frequencies by applying a polynomial subtraction method. - default order of the model = 0
#'   \item \code{REVPPM} : Reverse ppm scale - default value = FALSE
#'   \item \code{BLPHC} : Number of points for baseline smoothing during phasing - default value = 50
#'   \item \code{KSIG} : Number of times the noise signal to be considered during phasing - default value = 6
#'   \item \code{CPMG} : Indicate if CPMG sequence  - default value = FALSE
#'   \item \code{ZEROFILLING} : Zero Filling - - default value = FALSE
#'   \item \code{ZFFAC} : Max factor for Zero Filling - default value = 4
#'   \item \code{LINEBROADENING} : Line Broading - default value = TRUE
#'   \item \code{TSP} : PPM referencing - default value = FALSE
#'   \item \code{RABOT} : Zeroing of Negative Values - default value = FALSE
#'   \item \code{OPTPHC0} : Zero order phase optimization - default value = TRUE
#'   \item \code{OPTPHC1} : First order phase optimization - default value = FALSE
#'   \item \code{OPTCRIT1} : Criterium for phasing optimization (1 for SSpos, 2 for SSneg, 3 for Entropy - default value = 2
#'   \item \code{JGD_INNER} : JEOL : internal (or external) estimation for Group Delay - default value = TRUE
#' }
Spec1rProcpar <- list (

### General Parameters
    DEBUG=TRUE,                # Debug 
    LOGFILE="",                # Messages output file
    VENDOR="bruker",           # Instrumental origin of the raw data, (bruker, varian)
    READ_RAW_ONLY=FALSE,       # Read Raw file only; do not carry out processing; if raw file is depending on INPUT_SIGNAL
    INPUT_SIGNAL="fid",        # What type of input signal: 'fid' or '1r'
    PDATA_DIR='pdata/1',       # subdirectory containing the 1r file (bruker's format only)
    CLEANUP_OUTPUT=TRUE,       # Clean up the final output objet
    CPREGEX='(^CP\\.|^CP$|\\.CP$)',  # Regex for CP pulses

### PRE-PROCESSING - 1r
    ZF1R=FALSE,                # "ZeroFilling" on the 1r spectrum

### PRE-PROCESSING - FID
    LINEBROADENING=TRUE,       # Line Broading
    LB= 0.3,                   # Exponantial Line Broadening parameter
    GB= 0,                     # Gaussian Line Broadening parameter
    REVPPM=FALSE,              # Reverse ppm scale
    ZEROFILLING=FALSE,         # Zero Filling
    ZFFAC=4,                   # Max factor for Zero Filling
    TSP=FALSE,                 # PPM referencing
    O1RATIO=1,                 # Fractionnal value of the Sweep Width for PPM calibration
                               # if not based on the parameter of the spectral region center (O1)
    RABOT=FALSE,               # Zeroing of Negative Values
    AJUSTBL=FALSE,             # Ajust Baseline at the final step
    REMLFREQ=0,                # Remove low frequencies by applying a polynomial subtraction method.
    BLPHC=50,                  # Number of points for baseline smoothing during phasing
    KSIG=2,                    # Number of times the noise signal to be considered
    GAMMA=0.005,               # Penalty factor for the calculation of the entropy
    CPMG=FALSE,                # Indicate if CPMG sequence
    KZERO=0.3,                 # PPM range around the center to be masked during phase correction
    KZEROTHRES=250,            # Number of times the noise level to consider for removing negative areas in the phasing calculation
    OPTSTEP=TRUE,              # applied the lifting minimum to zero
    OPTCRIT0=0,                # Criterium for zero order phasing optimization (0 for SSneg, 1 for SSpos)
    CRITSTEP1=0,               # Criterium for first step of the first order phasing optimization
    CRITSTEP2=2,               # Criterium for second step of the first order phasing optimization
    RATIOPOSNEGMIN=0.4,        # Ratio Positive/Negative Minimum
    JGD_INNER=TRUE,            # JEOL : internal (or external) estimation for Group Delay
    RMS=0,

### Phase Correction
    OPTPHC0=TRUE,              # Zero order phase optimization
    OPTPHC1=TRUE,              # Zero order and first order phases optimization
    OPTCRIT1=2,                # Global criterium for first order phasing optimization (1 for SSpos, 2 for SSneg, 3 for Entropy)
    ADJPZTSP=FALSE,            # Ajust phc0 based on TSP peak : active / desactivate
    DHZPZTSP=7,                # Ajust phc0 based on TSP peak : HZ range around TSP peak
    DPHCPZTSP = 0.8,           # Ajust phc0 based on TSP peak : phase range around previous estimated value
    MVPZTSP=FALSE,             # Ajust phc0 based on TSP peak : adjustment of the mean value close to the TSP
    MVPZFAC=1.25,              # Acceptance of entropy multiplied by this factor
    DHZPZRANGE=250,            # Ajust phc0 based on mean spectrum on the ppm range closed to the TSP peak
    KSTART=NULL,               # Start of interval in proportion to the ppm scale
    KSTOP=NULL                 # End of interval in proportion to the ppm scale
)

#--------------------------------
# verbose function
#--------------------------------
.v <- function(..., logfile=Spec1rProcpar$LOGFILE) cat(sprintf(...), sep='', file=logfile, append=TRUE)



#--------------------------------
# READ FID && Acquisition Parameters
#--------------------------------

### Estime Group Delay
#-- fid : free induction decay - Complex data type

.estime_grpdelay <- function(fid)
{
   P <- sqrt(  Re(fid)^2 + Im(fid)^2 )
   V <- cbind(Re(fid), Im(fid))
   G <- 0
   nd0 <- which(P>max(P)/2)[1];
   if (nd0>1) {
      for( i in 1:(dim(V)[2]) ) {
         nd0 <- which(P>max(P)/2)[1];
         nd0p <- which.max(abs(V[1:(nd0-1),i]))
         if ((abs(V[nd0p,i])/abs(V[nd0,i]))>0.5) nd0 <- nd0p;
         while(nd0>1 && sign(V[nd0-1,i])==sign(V[nd0,i])) nd0 <- nd0-1;
         G <- G + 0.9999*(nd0-1 + V[nd0-1,i]/(V[nd0-1,i]-V[nd0,i]))
      }
      G <- G/dim(V)[2]
   }
   G
}


#--------------------------------
# BRUKER : FID & 1r
#--------------------------------

#### Retrieve a parameter value from Bruker acquisition parameters
#--  internal routine
#--  ACQ: list of acquisition parameters of the acqus file
#--  paramStr: name of the parameter
.bruker.get_param <- function (ACQ,paramStr,type="numeric", arrayType=FALSE)
{
   regexpStr <- paste("^...",paramStr,"=",sep="")
   if (!arrayType) {
       acqval <- gsub("^[^=]+= ?","" ,ACQ[which(simplify2array(regexpr(regexpStr,ACQ))>0)])
   } else {
       acqval <-simplify2array(strsplit(ACQ[which(simplify2array(regexpr(regexpStr,ACQ))>0)+1]," "))
   }
   if (type=="numeric")
       acqval <-as.numeric(acqval)
   if (type=="string")
       acqval <-gsub("<","",gsub(">","",acqval))
   acqval
}

#### Read FID data and the main parameters needed to generate the real spectrum
#--  internal routine
#-- DIR: bruker directory containing the FID
.read.FID.bruker <- function(DIR)
{
   cur_dir <- getwd()
   setwd(DIR)

   # FID filename
   FIDFILE <- "fid"
   if (!file.exists(FIDFILE)) 
       stop("FID File ", FIDFILE, " does not exist\n")
   # Acquisition parameters filename
   ACQFILE <- "acqus"
   if (!file.exists(ACQFILE)) 
       stop("Acquisition parameter File (", ACQFILE, ") does not exist\n")

   ACQ     <- readLines(ACQFILE)
   PROBE   <- .bruker.get_param(ACQ,"PROBHD",type="string")
   SOLVENT <- .bruker.get_param(ACQ,"SOLVENT",type="string")
   PULSE   <- .bruker.get_param(ACQ,"PULPROG",type="string")
   TEMP    <- .bruker.get_param(ACQ,"TE",type="numeric")
   RELAXDELAY <- .bruker.get_param(ACQ,"D",type="numeric",  arrayType=TRUE)[2]
   PULSEWIDTH <- .bruker.get_param(ACQ,"P",type="numeric",  arrayType=TRUE)[1]
   P15     <- .bruker.get_param(ACQ,"P",type="numeric",  arrayType=TRUE)[16]
   SPINNINGRATE  <- .bruker.get_param(ACQ,"MASR",type="numeric")
   NUMBEROFSCANS <- .bruker.get_param(ACQ,"NS")
   DUMMYSCANS <- .bruker.get_param(ACQ,"DS")
   NUC     <- .bruker.get_param(ACQ,"NUC1",type="string")
   TD      <- .bruker.get_param(ACQ,"TD")
   SW      <- .bruker.get_param(ACQ,"SW")
   SWH     <- .bruker.get_param(ACQ,"SW_h")
   O1      <- .bruker.get_param(ACQ,"O1")
   SFO1    <- .bruker.get_param(ACQ,"SFO1")
   GRPDLY  <- .bruker.get_param(ACQ,"GRPDLY")
   DECIM   <- .bruker.get_param(ACQ,"DECIM")
   DSPFVS  <- .bruker.get_param(ACQ,"DSPFVS")
   DTYPA   <- .bruker.get_param(ACQ,"DTYPA")
   BYTORDA <- .bruker.get_param(ACQ,"BYTORDA")
   ENDIAN = ifelse( BYTORDA==0, "little", "big")
   INSTRUMENT <- "Bruker"
   SOFTWARE <- gsub("..TITLE= ?Parameter ...., ","", gsub("Version ", "", gsub("\t\t", " ", ACQ[1])))
   ORIGIN   <- gsub("^[^=]+= ?","", ACQ[which(simplify2array(regexpr("^..ORIGIN=",ACQ))>0)])
   ORIGPATH <- gsub("^.. ", "", ACQ[which(simplify2array(regexpr("acqus$",ACQ))>0)])

   SIZE <- ifelse( DTYPA==0, 4L, 8L)
   DTYPE <- ifelse( DTYPA==0, "int", "double" )
   
   to.read <- file(FIDFILE,"rb")
   signal <- readBin(to.read, what=DTYPE, n=TD, size=SIZE, endian = ENDIAN)
   close(to.read)

   setwd(cur_dir)

   TDsignal<-length(signal)
   if (TDsignal < TD) {
     fidGoodSize = sapply(vector("list", length = TD), function (x) 0)
     fidGoodSize[1:TDsignal] = signal
   } else if (TDsignal > TD) {
     fidGoodSize = signal(1:TD)
   } else {
     fidGoodSize = signal
   }
   rawR <- fidGoodSize[seq(from = 1, to = TD, by = 2)]
   rawI <- fidGoodSize[seq(from = 2, to = TD, by = 2)]

   SI <- 2^round(log2(TD)+0.499)/2
   if (TD>2*SI) {
      rawR <- rawR[1:SI]
      rawI <- rawI[1:SI]
      TD <- 2*SI
   }
   fid <- complex(real=rawR, imaginary=rawI)
   TD <- length(fid)

   if (is.null(GRPDLY) || is.na(GRPDLY) ||  length(GRPDLY)==0 || GRPDLY<0) {

      # See http://sbtools.uchc.edu/help/nmr/nmr_toolkit/bruker_dsp_table.asp
      group_delay_matrix <- matrix(c(44.7500,46.0000,46.311,2.750,33.5000,36.5000,36.530,2.833,66.6250,48.0000,47.870,2.875,
                   59.0833,50.1667,50.229,2.917,68.5625,53.2500,53.289,2.938,60.3750,69.5000,69.551,2.958,69.5313,72.2500,71.600,2.969,
                   61.0208,70.1667,70.184,2.979,70.0156,72.7500,72.138,2.984,61.3438,70.5000,70.528,2.989,70.2578,73.0000,72.348,2.992,
                   61.5052,70.6667,70.700,2.995,70.3789,72.5000,72.524, NA,61.5859,71.3333,71.3333, NA,70.4395,72.2500,72.2500, NA,
                   61.6263,71.6667,71.6667, NA,70.4697,72.1250,72.1250, NA,61.6465,71.8333,71.8333, NA,70.4849,72.0625,72.0625, NA,
                   61.6566,71.9167,71.9167, NA,70.4924,72.0313,72.0313, NA), nrow = 21, ncol = 4, byrow = TRUE, 
                                     dimnames = list(c(2, 3, 4, 6, 8, 12, 16, 24, 32, 48, 64, 96, 128, 192, 
                                                       256, 384, 512, 768, 1024, 1536, 2048), c(10, 11, 12,13)))

      GRPDLY <- tryCatch({
                   group_delay_matrix[toString(DECIM), toString(DSPFVS)]
                },  error = function(e) { 
                  .estime_grpdelay(fid)
                })
   }
   if (is.null(GRPDLY) || is.na(GRPDLY) ||  length(GRPDLY)==0 || GRPDLY<0)  GRPDLY <- 0

   setwd(cur_dir)

   acq <- list( INSTRUMENT=INSTRUMENT, SOFTWARE=SOFTWARE, ORIGIN=ORIGIN, ORIGPATH=ORIGPATH, 
                PROBE=PROBE, PULSE=PULSE, SOLVENT=SOLVENT, TEMP=TEMP, NUC=NUC, 
                NUMBEROFSCANS=NUMBEROFSCANS, DUMMYSCANS=DUMMYSCANS, OFFSET=O1/SFO1,
                RELAXDELAY=RELAXDELAY, SPINNINGRATE=SPINNINGRATE, PULSEWIDTH=PULSEWIDTH,
                TD=TD, SW=SW, SWH=SWH, SFO1=SFO1, O1=O1, GRPDLY=GRPDLY, P15=P15 )
   spec <- list( path=DIR, acq=acq, fid=fid )

   spec
}

#### Read 1r data and the main parameters needed to generate the real spectrum
#--  internal routine
#-- DIR: bruker directory containing the 1r
.read.1r.bruker <- function(DIR, param=Spec1rProcpar)
{
   cur_dir <- getwd()
   setwd(DIR)
   
   # Acquisition parameters filename
   ACQFILE <- "acqus"
   if (!file.exists(ACQFILE)) 
       stop("Acquisition parameter File (", ACQFILE, ") does not exist\n")

   # Processing parameters filename
   PROCFILE <- paste(param$PDATA_DIR,"/procs",sep="")
   if (!file.exists(PROCFILE)) 
       stop("Processing parameter File (", PROCFILE, ") does not exist\n")

   # Read Acquisition parameters
   ACQ     <- readLines(ACQFILE)
   PROBE   <- .bruker.get_param(ACQ,"PROBHD",type="string")
   SOLVENT <- .bruker.get_param(ACQ,"SOLVENT",type="string")
   NUC     <- .bruker.get_param(ACQ,"NUC1",type="string")
   PULSE   <- .bruker.get_param(ACQ,"PULPROG",type="string")
   TEMP    <- .bruker.get_param(ACQ,"TE",type="numeric")
   RELAXDELAY <- .bruker.get_param(ACQ,"D",type="numeric",  arrayType=TRUE)[2]
   PULSEWIDTH <- .bruker.get_param(ACQ,"P",type="numeric",  arrayType=TRUE)[1]
   P15     <- .bruker.get_param(ACQ,"P",type="numeric",  arrayType=TRUE)[16]
   SPINNINGRATE  <- .bruker.get_param(ACQ,"MASR",type="numeric")
   NUMBEROFSCANS <- .bruker.get_param(ACQ,"NS")
   TD      <- .bruker.get_param(ACQ,"TD")
   SW      <- .bruker.get_param(ACQ,"SW")
   SWH     <- .bruker.get_param(ACQ,"SW_h")
   SFO1    <- .bruker.get_param(ACQ,"SFO1")
   O1      <- .bruker.get_param(ACQ,"O1")
   GRPDLY  <- .bruker.get_param(ACQ,"GRPDLY")
   INSTRUMENT <- "Bruker"
   SOFTWARE <- gsub("..TITLE= ?Parameter ...., ","", gsub("Version ", "", gsub("\t\t", " ", ACQ[1])))
   ORIGIN   <- gsub("^[^=]+= ","", ACQ[which(simplify2array(regexpr("^..ORIGIN=",ACQ))>0)])
   ORIGPATH <- gsub("^.. ", "", ACQ[which(simplify2array(regexpr("acqus$",ACQ))>0)])

   GRPDLY <- tryCatch ( { if (!is.na(as.numeric(GRPDLY)) && as.numeric(GRPDLY)>0) round(GRPDLY); }, warning = function(w){}, error=function(e){} )
   if (is.null(GRPDLY)) {
        GRPDLY <- 0
   }

   # Read Processing parameters
   PROC <- readLines(PROCFILE)
   BYTORDP <- .bruker.get_param(PROC,"BYTORDP")
   DTYPP <- .bruker.get_param(PROC,"DTYPP")
   NC_proc <-  .bruker.get_param(PROC,"NC_proc")
   OFFSET <- .bruker.get_param(PROC,"OFFSET")
   SI <- .bruker.get_param(PROC,"SI")
   PHC0 <-  .bruker.get_param(PROC,"PHC0")
   PHC1 <-  .bruker.get_param(PROC,"PHC1")

   # Read the 1r/1i spectra
   ENDIAN <- ifelse( BYTORDP==0, "little", "big")
   SIZE <- ifelse( DTYPP==0, 4L, 8L)
   DTYPE <- ifelse( DTYPP==0, "int", "double" )

   # 1r filename
   SPECFILE <- paste(param$PDATA_DIR,"/1r",sep="")
   if (!file.exists(SPECFILE))
       stop("1r File (", SPECFILE, ") does not exist\n")   
   to.read <- file(SPECFILE,"rb")
   signalR <- rev(readBin(to.read, what=DTYPE, n=SI, size=SIZE, endian = ENDIAN))
   close(to.read)
   signalR <- (2^NC_proc)*signalR
   TD <- SI <- length(signalR)

   # 1i filename
   SPECFILE <- paste(param$PDATA_DIR,"/1i",sep="")
   if (!file.exists(SPECFILE))
       stop("1i File (", SPECFILE, ") does not exist\n")   
   to.read = file(SPECFILE,"rb")
   signalI<-rev(readBin(to.read, what=DTYPE, n=SI, size=SIZE, endian = ENDIAN))
   close(to.read)
   signalI <- (2^NC_proc)*signalI

   pmax <- OFFSET
   pmin <- OFFSET - SW

   # If "ZeroFilling" on the 1r spectrum
   if (param$ZF1R) {
      # Compute the fid
      spec <- complex(real=signalR, imaginary=signalI)
      fid <- stats::fft(rev(spec), inverse=TRUE)
      # Insert zeros just before the tail
      k=0.99
      tailFID <- fid[round(k*length(fid)):length(fid)]
      fid <- c( fid[1:(round(k*length(fid))-1)], rep(complex(real=0, imaginary=0), TD), tailFID )
      # Compute the real spectrum
      spec <- rev(stats::fft(fid))
      signalR <- (max(signalR)/max(Re(spec)))*Re(spec)
      #signalI <- (max(signalI)/max(Im(spec)))*Im(spec)
      # Change point sizes
      TD <- SI <- length(signalR)
   }

   setwd(cur_dir)

   dppm <- SW/(TD-1)
   ppm <- seq(from=pmin, to=pmax, by=dppm)

   acq <- list( INSTRUMENT=INSTRUMENT, SOFTWARE=SOFTWARE, ORIGIN=ORIGIN, ORIGPATH=ORIGPATH, 
                PROBE=PROBE, PULSE=PULSE, NUC=NUC, NUMBEROFSCANS=NUMBEROFSCANS, SOLVENT=SOLVENT, TEMP=TEMP, 
                RELAXDELAY=RELAXDELAY, SPINNINGRATE=SPINNINGRATE, PULSEWIDTH=PULSEWIDTH,
                TD=TD, SW=SW, SWH=SWH, SFO1=SFO1, O1=O1, GRPDLY=GRPDLY, P15=P15 )

   proc <- list( phc0=PHC0*pi/180, phc1=PHC1*pi/180, SI=SI )

   param$ZEROFILLING <- FALSE
   param$LINEBROADENING <- FALSE

   spec <- list( path=DIR, param=param, acq=acq, proc=proc, fid=NULL, int=signalR, dppm=dppm, pmin=pmin, pmax=pmax, ppm=ppm )
   #spec <- list( path=DIR, param=param, acq=acq, proc=proc, fid=NULL, int=signalR, img=signalI, dppm=dppm, pmin=pmin, pmax=pmax, ppm=ppm )

   spec
}



#--------------------------------
# RS2D : FID & 1r
#--------------------------------

#### Retrieve a parameter value from RS2D acquisition parameters
#--  internal routine
#--  ACQ: list of acquisition parameters of the acqus file
#--  paramStr: name of the parameter
.rs2d.get_param <- function (ACQ,paramStr,type="numeric")
{
   regexpStr <- paste("^",paramStr,"=",sep="")
   acqval <- gsub("^[^=]+= ?","" ,ACQ[which(simplify2array(regexpr(regexpStr,ACQ))>0)])
   if (type=="numeric")
       acqval <-as.numeric(acqval)
   acqval
}

#### Read FID data and the main parameters needed to generate the real spectrum
#--  internal routine
#-- DIR: RS2D directory containing the FID
.read.FID.rs2d <- function(DIR)
{
   cur_dir <- getwd()
   setwd(DIR)

   # FID filename
   FIDFILE <- "data.dat"
   if (!file.exists(FIDFILE)) 
       stop("DATA File ", FIDFILE, " does not exist\n")
   # Acquisition parameters filename
   ACQFILE <- "header.xml"
   if (!file.exists(ACQFILE)) 
       stop("Acquisition parameter File (", ACQFILE, ") does not exist\n")
   ACQ <- NULL
   tree <- xmlTreeParse(ACQFILE)
   root <- xmlRoot(tree)
   xmlParams <- xmlElementsByTagName(root, "entry", recursive = TRUE)
   for(i in 1:length(xmlParams)) {
      L <- xmlToList(xmlElementsByTagName(xmlParams[[i]], "value", recursive = FALSE)[[1]])
      ACQ <- c(ACQ,paste0(L$name,"=",L$value))
   }

   PROBE   <- .rs2d.get_param(ACQ,"PROBES",type="string")
   SOLVENT <- .rs2d.get_param(ACQ,"SOLVENT",type="string")
   PULSE   <- .rs2d.get_param(ACQ,"SEQUENCE_NAME",type="string")
   TEMP    <- .rs2d.get_param(ACQ,"SAMPLE_TEMPERATURE")
   RELAXDELAY  <- .rs2d.get_param(ACQ,"Last delay")
   PULSEWIDTH <- .rs2d.get_param(ACQ,"P1.width")*1e6
   SPINNINGRATE <- .rs2d.get_param(ACQ,"SPIN_RATE")
   NUMBEROFSCANS <- .rs2d.get_param(ACQ,"NUMBER_OF_AVERAGES")
   DUMMYSCANS <- .rs2d.get_param(ACQ,"DUMMY_SCAN")
   NUC     <- .rs2d.get_param(ACQ,"OBSERVED_NUCLEUS",type="string")
   TD      <- .rs2d.get_param(ACQ,"Nb Point")
   SFO1    <- .rs2d.get_param(ACQ,"OBSERVED_FREQUENCY")/1e6
   SWH     <- .rs2d.get_param(ACQ,"SPECTRAL_WIDTH")
   SW      <- SWH/SFO1
   O1      <- .rs2d.get_param(ACQ,"OFFSET_FREQ_1")
   OFFSET  <- .rs2d.get_param(ACQ,"SR")/SFO1
   LCKSTATE <- ifelse( .rs2d.get_param(ACQ,"LOCK",type="string")=='true', TRUE, FALSE )
   ENDIAN  <-  "big"
   INSTRUMENT <- paste("RS2D",.rs2d.get_param(ACQ,"MODEL_NAME",type="string"))
   SOFTWARE <-  gsub(" \\[.+\\]","", .rs2d.get_param(ACQ,"SOFTWARE_VERSION",type="string"))

   NP <- 2*TD
   to.read = file(FIDFILE,"rb")
   signal<-readBin(to.read, what="double", n=NP, size=4L, endian = ENDIAN)
   close(to.read)
   setwd(cur_dir)
   
   rawR <- signal[seq(from = 1, to = NP, by = 2)]
   rawI <- signal[seq(from = 2, to = NP, by = 2)]
   fid <- complex(real=rawR, imaginary=rawI)
   TD <- length(fid)

   acq <- list( INSTRUMENT=INSTRUMENT, SOFTWARE=SOFTWARE, ORIGIN="RS2D", ORIGPATH=DIR, 
                PROBE=PROBE, PULSE=PULSE, SOLVENT=SOLVENT, TEMP=TEMP, NUC=NUC, OFFSET=OFFSET,
                NUMBEROFSCANS=NUMBEROFSCANS, DUMMYSCANS=DUMMYSCANS, LCKSTATE=LCKSTATE,
                RELAXDELAY=RELAXDELAY, SPINNINGRATE=SPINNINGRATE, PULSEWIDTH=PULSEWIDTH,
                TD=TD, SW=SW, SWH=SWH, SFO1=SFO1, O1=O1, GRPDLY=0 )
   spec <- list( path=DIR, acq=acq, fid=fid )

   spec
}

#### Read 1r data and the main parameters
#--  internal routine
#-- DIR: rs2d directory containing the experience
.read.1r.rs2d <- function(DIR, param=Spec1rProcpar)
{
   cur_dir <- getwd()
   RAWDIR <- file.path(DIR,param$PDATA_DIR)
   if (!file.exists(RAWDIR))
       stop("Invalide folder : ", RAWDIR, " does not exist\n")

   setwd(RAWDIR)
   
   # FID filename
   FIDFILE <- "data.dat"
   if (!file.exists(FIDFILE)) 
       stop("DATA File ", FIDFILE, " does not exist\n")
   # Acquisition parameters filename
   ACQFILE <- "header.xml"
   if (!file.exists(ACQFILE)) 
       stop("Acquisition parameter File (", ACQFILE, ") does not exist\n")
   ACQ <- NULL
   tree <- xmlTreeParse(ACQFILE)
   root <- xmlRoot(tree)
   xmlParams <- xmlElementsByTagName(root, "entry", recursive = TRUE)
   for(i in 1:length(xmlParams)) {
      L <- xmlToList(xmlElementsByTagName(xmlParams[[i]], "value", recursive = FALSE)[[1]])
      ACQ <- c(ACQ,paste0(L$name,"=",L$value))
   }

   PROBE   <- .rs2d.get_param(ACQ,"PROBES",type="string")
   SOLVENT <- .rs2d.get_param(ACQ,"SOLVENT",type="string")
   PULSE   <- .rs2d.get_param(ACQ,"SEQUENCE_NAME",type="string")
   TEMP    <- .rs2d.get_param(ACQ,"SAMPLE_TEMPERATURE")
   RELAXDELAY  <- .rs2d.get_param(ACQ,"Last delay")
   PULSEWIDTH <- .rs2d.get_param(ACQ,"P1.width")*1e6
   SPINNINGRATE <- .rs2d.get_param(ACQ,"SPIN_RATE")
   NUMBEROFSCANS <- .rs2d.get_param(ACQ,"NUMBER_OF_AVERAGES")
   DUMMYSCANS <- .rs2d.get_param(ACQ,"DUMMY_SCAN")
   NUC     <- .rs2d.get_param(ACQ,"OBSERVED_NUCLEUS",type="string")
   TD      <- .rs2d.get_param(ACQ,"Nb Point")
   SFO1    <- .rs2d.get_param(ACQ,"OBSERVED_FREQUENCY")/1e6
   SWH     <- .rs2d.get_param(ACQ,"SPECTRAL_WIDTH")
   SW      <- SWH/SFO1
   O1      <- .rs2d.get_param(ACQ,"OFFSET_FREQ_1")
   OFFSET  <- .rs2d.get_param(ACQ,"SR")/SFO1
   LOCKPPM <- 0
   LCKSTATE <- ifelse( .rs2d.get_param(ACQ,"LOCK",type="string")=='true', TRUE, FALSE )
   ENDIAN  <-  "big"
   INSTRUMENT <- paste("RS2D",.rs2d.get_param(ACQ,"MODEL_NAME",type="string"))
   SOFTWARE <-  gsub(" \\[.+\\]","", .rs2d.get_param(ACQ,"SOFTWARE_VERSION",type="string"))
   GRPDLY <- 0

   # Read Processing parameters
   SI <- TD
   PHC0 <-  .rs2d.get_param(ACQ,"PHASE_0")
   PHC1 <-  .rs2d.get_param(ACQ,"PHASE_1")

   # Read the 1r spectrum
   NP <- 2*TD
   to.read = file(FIDFILE,"rb")
   signal<-readBin(to.read, what="double", n=NP, size=4L, endian = ENDIAN)
   close(to.read)
   setwd(cur_dir)

   rawR <- signal[seq(from = 1, to = NP, by = 2)]
   rawI <- signal[seq(from = 2, to = NP, by = 2)]

   dppm <- SW/(SI-1)
   pmin <- OFFSET - SW/2
   pmax <- OFFSET + SW/2
   ppm <- seq(from=pmin, to=pmax, by=dppm)

   acq <- list( INSTRUMENT=INSTRUMENT, SOFTWARE=SOFTWARE, ORIGIN="RS2D", ORIGPATH=RAWDIR, 
                PROBE=PROBE, PULSE=PULSE, NUC=NUC, SOLVENT=SOLVENT, TEMP=TEMP, 
                RELAXDELAY=RELAXDELAY, SPINNINGRATE=SPINNINGRATE, PULSEWIDTH=PULSEWIDTH,
                TD=TD, SW=SW, SWH=SWH, SFO1=SFO1, O1=O1, GRPDLY=GRPDLY )

   proc <- list( phc0=PHC0*pi/180, phc1=PHC1*pi/180, SI=SI )

   param$ZEROFILLING <- FALSE
   param$LINEBROADENING <- FALSE

   spec <- list( path=DIR, param=param, acq=acq, proc=proc, fid=NULL, int=rev(rawR), dppm=dppm, pmin=pmin, pmax=pmax, ppm=ppm )

   spec
}



#--------------------------------
# VARIAN : FID only
#--------------------------------

#### Retrieve a parameter value from Varian acquisition parameters
#--  internal routine
#--  ACQ: list of acquisition parameters of the procpar file
#--  paramStr: name of the parameter
.varian.get_param <- function (ACQ,paramStr,type="numeric")
{
   regexpStr <- paste("^",paramStr," ",sep="")
   acqval <- gsub("1 ","",ACQ[which(simplify2array(regexpr(regexpStr,ACQ))>0)+1])
   if (type=="numeric")
       acqval <-as.numeric(acqval)
   if (type=="string")
       acqval <-gsub("\"","",acqval)
   acqval
}

#### Read FID data and the main parameters needed to generate the real spectrum
#--  internal routine
#-- DIR: Varian directory containing the FID
.read.FID.varian <- function(DIR)
{
   cur_dir <- getwd()
   setwd(DIR)

   # FID filename
   FIDFILE <- "fid"
   if (!file.exists(FIDFILE)) 
       stop("FID File ", FIDFILE, " does not exist\n")
   # Acquisition parameters filename
   ACQFILE <- "procpar"
   if (!file.exists(ACQFILE)) 
       stop("Acquisition parameter File (", ACQFILE, ") does not exist\n")

   ACQ  <- readLines(ACQFILE)
   SWH  <-  .varian.get_param(ACQ,"sw")
   SFO1 <- .varian.get_param(ACQ,"sfrq")
   REFFRQ <- .varian.get_param(ACQ,"reffrq")
   O1   <- .varian.get_param(ACQ,"tof")
   OFFSET <- 1e6*(1- REFFRQ/SFO1)
   SW   <- SWH/SFO1
   ENDIAN = "big"

   to.read = file(FIDFILE,"rb")
   # See https://github.com/OpenVnmrJ/OpenVnmrJ/blob/master/src/bin/read_raw_data.c
   #/*****************/
   #struct datafilehead
   #/*****************/
   #/* Used at the beginning of each data file (fid's, spectra, 2D) */
   #{
   #   int32    nblocks;       /* number of blocks in file          */
   #   int32    ntraces;       /* number of traces per block        */
   #   int32    np;            /* number of elements per trace      */
   #   int32    ebytes;        /* number of bytes per element       */
   #   int32    tbytes;        /* number of bytes per trace         */
   #   int32    bbytes;        /* number of bytes per block         */
   #   int16   vers_id;       /* software version and file_id status bits   */
   #   int16   status;        /* status of whole file               */
   #   int32       nbheaders;     /* number of block headers        */
   #};
   header1<-readBin(to.read, what="integer",size=4, n=6, endian = ENDIAN)
   header2<-readBin(to.read, what="integer",size=2, n=2, endian = ENDIAN)
   header3<-readBin(to.read, what="integer",size=4, n=1, endian = ENDIAN)
   header <- c(header1, header2, header3 )

   #/*******************/
   #struct datablockhead
   #/*******************/
   #/* Each file block contains the following header            */
   #{
   #   int16      scale;    /* scaling factor                   */
   #   int16      status;   /* status of data in block          */
   #   int16      index;    /* block index                      */
   #   int16      mode;     /* mode of data in block            */
   #   int32          ctcount;  /* ct value for FID             */
   #   float      lpval;    /* F2 left phase in phasefile       */
   #   float      rpval;    /* F2 right phase in phasefile      */
   #   float      lvl;      /* F2 level drift correction        */
   #   float      tlt;      /* F2 tilt drift correction         */
   #};
   block1<-readBin(to.read, what="integer",size=2, n=4, endian = ENDIAN)
   block2<-readBin(to.read, what="integer",size=4, n=1, endian = ENDIAN)
   block3<-readBin(to.read, what="double",size=4, n=4, endian = ENDIAN)
   block <- list( c(block1, block2), block3 )

   ## Read FID
   dtype <- "double"
   if ( as.logical(intToBits(header[8]))[4] == FALSE ) dtype  <- "integer"
   signal<-readBin(to.read, what=dtype,size=header[4], n=header[3], endian = ENDIAN)
   close(to.read)
   setwd(cur_dir)

   # Get the raw Real and Imaginary spectra 
   td <- length(signal)
   rawR <- signal[seq(from = 1, to = td, by = 2)]
   rawI <- signal[seq(from = 2, to = td, by = 2)]
   fid <- complex(real=rawR, imaginary=rawI)
   TD <- length(fid)

   GRPDLY <- 0
   PROBE <- .varian.get_param(ACQ,"probe_",type="string")
   SOLVENT <-  .varian.get_param(ACQ,"solvent",type="string")
   PULSE <- .varian.get_param(ACQ,"pslabel",type="string")
   ORIGPATH <- .varian.get_param(ACQ,"exppath",type="string")
   RELAXDELAY  <- .varian.get_param(ACQ,"d1")
   PULSEWIDTH <- .varian.get_param(ACQ,"pw90")
   SPINNINGRATE <- .varian.get_param(ACQ,"spin")
   NUMBEROFSCANS <- .varian.get_param(ACQ,"nt")
   DUMMYSCANS <- .varian.get_param(ACQ,"ss")
   TEMP <- .varian.get_param(ACQ,"temp") + 274.15
   NUC <- .varian.get_param(ACQ,"tn",type="string")

   acq <- list( INSTRUMENT="VARIAN", SOFTWARE="VnmrJ", ORIGIN="VARIAN", ORIGPATH=ORIGPATH, PROBE=PROBE, PULSE=PULSE, SOLVENT=SOLVENT, 
                RELAXDELAY=RELAXDELAY, SPINNINGRATE=SPINNINGRATE, PULSEWIDTH=PULSEWIDTH, TEMP=TEMP, NUC=NUC,
                NUMBEROFSCANS=NUMBEROFSCANS, DUMMYSCANS=DUMMYSCANS, OFFSET=OFFSET,
                TD=TD, SW=SW, SWH=SWH, SFO1=SFO1, O1=O1, GRPDLY=GRPDLY )
   spec <- list( path=DIR, acq=acq, fid=fid )

   spec

}



#--------------------------------
# JEOL : FID only
#--------------------------------

#### Read FID data and the main parameters needed to generate the real spectrum
#--  internal routine
#-- FILE: JEOL JDF file containing the FID
.read.FID.jeol <- function(FILE)
{
   if (!file.exists(FILE))
       stop("File ", FILE, " does not exist\n")

   #--------------
   # Dictionnaries
   #--------------
   
   Data_value_type <- c( "string", "integer", "float", "complex", "infinity" )
    
   Data_axis_type <- c( "None","REAL","TPPI","COMPLEX","REAL COMPLEX","ENVELOPE" )
   
   Data_format <- c(
       "Void","One_D","Two_D","Three_D","Four_D","Five_D","Six_D","Seven_D","Eight_D",
        "Void","Void","Void","Small_Two_D","Small_Three_D","Small_Four_D"
   )
   
   Unit_labels <- c(
       "","Abundance","Ampere","Candela","dC","Coulomb","deg","Electronvolt","Farad","Sievert","Gram","Gray ","Henry","Hz","Kelvin",
       "Joule","Liter","Lumen","Lux","Meter","Mole","Newton","Ohm","Pascal","Percent","Point","ppm","Radian","s","Siemens","Steradian",
       "T","Volt","Watt","Weber","dB","Dalton","Thompson","Ugeneric","LPercent","PPT","PPB","Index"
   )
   
   Unit_prefix <- c( "Yotta","Zetta","Exa","Pecta","Tera","G","M","k","","m","u","n","p","Femto","Atto","Zepto" )
   
   Instruments <- c(
      "NONE","GSX","ALPHA","ECLIPSE","MASS_SPEC","COMPILER","OTHER_NMR","UNKNOWN","GEMINI","UNITY","ASPECT","UX","FELIX","LAMBDA",
      "GE_1280","GE_OMEGA","CHEMAGNETICS","CDFF","GALACTIC","TRIAD","GENERIC_NMR","GAMMA","JCAMP_DX","AMX","DMX","ECA","ALICE",
      "NMR_PIPE","SIMPSON"
   )

   trim <- function (x) gsub("^\\s+|\\s+$", "", x)

   #--------------
   # Read Header
   #--------------
   
   to.read = file(FILE,"rb")
   Header <- list(
     File_Identifier=readChar(to.read, 8),                                                        # JEOL.NMR otherwise incorrect data
     Endian=readBin(to.read, what="int", n=1, size=1L, signed=F, endian = "big"),                 # 0 for Big an 1 for Little
     Major_Version=readBin(to.read, what="int", n=1, size=1L, signed=F, endian = "big"),          # must be 1
     Minor_Version=readBin(to.read, what="int", n=2, size=1L, signed=F, endian = "big"),          # must be 1
     Data_Dimension_Number=readBin(to.read, what="int", n=1, size=1L, signed=F, endian = "big"),  # 1-8 (8 is max dimensionality)
     Data_Dimension_Exist=readBin(to.read, what="int", n=1, size=1L, signed=F, endian = "big"),   # see manual
     Data_Type=readBin(to.read, what="int", n=1, size=1L, signed=F, endian = "big"),              # 
     Instrument=readBin(to.read, what="int", n=1, size=1L, signed=F, endian = "big"),             # 
     Translate=readBin(to.read, what="int", n=8, size=1L, signed=F, endian = "big"),              # should always be 1,2,3,4,5,6,7,8 otherwise indicated processed data 
     Data_Axis_Type=readBin(to.read, what="int", n=8, size=1L, signed=F, endian = "big"),         # (1= real, 3=complex) for more see manual
     Data_Units=readBin(to.read, what="int", n=16, size=1L, signed=F, endian = "big"),            # see manual
     Title=readChar(to.read, 124),                                                                #
     Data_Axis_Ranged=readBin(to.read, what="int", n=4,  size=1L, signed=F, endian = "big"),      #
     Data_Points=readBin(to.read, what="int", n=8,  size=4L, endian = "big"),                     #
     Data_Offset_Start=readBin(to.read, what="int", n=8,  size=4L, endian = "big"),               #
     Data_Offset_Stop=readBin(to.read, what="int", n=8,  size=4L, endian = "big"),                #
     Data_Axis_Start=readBin(to.read, what="numeric", n=8,  endian = "big"),                      #
     Data_Axis_Stop=readBin(to.read, what="numeric", n=8,  endian = "big"),                       #
     Creation_Time=readBin(to.read, what="int", n=4,  size=1L, signed=F, endian = "big"),         #
     Revision_Time=readBin(to.read, what="int", n=4,  size=1L, signed=F, endian = "big"),         #
     Node_Name=readChar(to.read, 16),                                                             #
     Site=readChar(to.read, 128),                                                                 #
     Author=readChar(to.read, 128),                                                               #
     Comment=readChar(to.read, 128),                                                              #
     Data_Axis_Titles=readChar(to.read, 256),                                                     #
     Base_Freq=readBin(to.read, what="numeric", n=8,  endian = "big"),                            #
     Zero_Point=readBin(to.read, what="numeric", n=8, size=8L, endian = "big"),                   #
     Reversed=readBin(to.read, what="int", n=1, size=1L, signed=F, endian = "big"),               #
     Reserved=readBin(to.read, what="int", n=11, size=1L, signed=F, endian = "big"),              #
     History_Used=readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = "big"),          #
     History_Length=readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = "big"),        #
     Param_Start=readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = "big"),           #
     Param_Length=readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = "big"),          #
     List_Start=readBin(to.read, what="int", n=8,  size=4L, signed=T, endian = "big"),            #
     List_Length=readBin(to.read, what="int", n=8,  size=4L, signed=T, endian = "big"),           #
     Data_Start=readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = "big"),            #
     Data_Length=readBin(to.read, what="int", n=1,  size=8L, signed=T, endian = "big"),           #
     Context_Start=readBin(to.read, what="int", n=1,  size=8L, signed=T, endian = "big"),         #
     Context_Length=readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = "big"),        #
     Annote_Start=readBin(to.read, what="int", n=1,  size=8L, signed=T, endian = "big"),          #
     Annote_Length=readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = "big"),         #
     Total_Size=readBin(to.read, what="int", n=1,  size=8L, signed=T, endian = "big"),            #
     Unit_Location=readBin(to.read, what="int", n=8,  size=1L, signed=T, endian = "big"),         #
     Compound_Units=readBin(to.read, what="int", n=3,  size=1L, signed=T, endian = "big")         #
   )
   close(to.read)

   # Spectrum type must be FID
   Spectrum_type <- ifelse( Unit_labels[ Header$Data_Units[2]+1 ]=="s", 'fid', 'spectrum')
   if ( Spectrum_type != "fid" )
       stop("File ", FILE, " seems not contain an FID spectrum\n")

   #--------------
   # Read Parameter Header
   #--------------
   
   # read in Parameters
   
   endian <- ifelse(Header$Endian==0, 'big', 'little')
   to.read = file(FILE,"rb")
   seek(to.read,where=Header$Param_Start, origin="start")
   Params <- list (
      Parameter_Size=readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = endian),
      Low_Index=readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = endian),
      High_Index=readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = endian),
      Total_Size=readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = endian)
   )
   
   #--------------
   # Read Parameters
   #--------------
   
   nparams <- Params$High_Index+1;
   procpar <- list()
   for(i in 1:nparams) {
   
       pad <- readBin(to.read, what="int", n=4, size=1L, signed=T, endian = endian)
       Unit_Scaler <- readBin(to.read, what="int", n=1, size=2L, signed=T, endian = endian)
       Units <- readBin(to.read, what="int", n=10, size=1L, signed=F, endian = endian)
       pad <- readBin(to.read, what="int", n=16, size=1L, signed=T, endian = endian)
       Value_Type <- readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = endian)
       seek(to.read,where=-20, origin="current")
   
       Value <- NULL
       if (Value_Type==0) {
           Value <- readChar(to.read, 16)
           Value <- gsub("\\\\" , "/", Value)
           Value <- gsub("<<" , "", Value)
           pad <- readBin(to.read, what="int", n=4, size=1L, signed=T, endian = endian)
       }
       if (Value_Type==1) {
           Value <- readBin(to.read, what="int", n=1,  size=4L, signed=T, endian = endian)
           pad <- readBin(to.read, what="int", n=16, size=1L, signed=T, endian = endian)
       }
       if (Value_Type==2) {
           Value <- readBin(to.read, what="numeric", n=1,  size=8L, signed=T, endian = endian)
           pad <- readBin(to.read, what="int", n=12, size=1L, signed=T, endian = endian)
       }
       if (Value_Type==3) {
           v_rl <- readBin(to.read, what="numeric", n=1,  size=8L, signed=T, endian = endian)
           v_im <- readBin(to.read, what="numeric", n=1,  size=8L, signed=T, endian = endian)
           Value <- complex(v_rl,v_im);
           pad <- readBin(to.read, what="int", n=4, size=1L, signed=T, endian = endian)
       }
       if (Value_Type==4) {
           Value <- readBin(to.read, what="int", n=1,  size=8L, signed=T, endian = endian)
           pad <- readBin(to.read, what="int", n=16, size=1L, signed=T, endian = endian)
       }
       if (is.null(Value)) {        
           pad <- readBin(to.read, what="int", n=20, size=1L, signed=T, endian = endian)
       }
       Name <- tolower(readChar(to.read, 28))
       Name <- gsub("\\.", "_", gsub(" ", "_", trim(Name)))

       if (! is.na(suppressWarnings(as.numeric(Value)))) {
           Value <- as.numeric(Value)
       } else if (! is.na(suppressWarnings(as.logical(Value)))) {
           Value <- as.logical(Value)
       } else
           Value <- trim(Value)

       prefix <- ''
       if (Units[1]>0) {
            v1 <- trunc(Units[1] / 16)
            v2 <- (Units[1] / 16 - v1)*16
            prefix <- ifelse(v1<8 , Unit_prefix[v1 + 9], Unit_prefix[v1 - 7])
       }
       v_unit <- paste0(prefix,Unit_labels[Units[2]+1])

       expr <- paste0( 'procpar$',tolower(trim(Name)), '<- list( "value_type"="', Data_value_type[Value_Type+1],'", ')
       if (Value_Type==0)
          expr <- paste0(expr, '"value"="',Value,'"')
       else
          expr <- paste0(expr, '"value"=',Value)
       expr <- paste0(expr, ' ,"Unit"="',v_unit, '", "Unit_Scaler"=',Unit_Scaler,')')
       eval(parse(text=expr))
   }   
   seek(to.read,0)
   close(to.read)

   #--------------
   # Read Fid Data
   #--------------
   fid <- NULL
   to.read = file(FILE,"rb")
   seek(to.read,where=Header$Data_Start, origin="start")
   if (Header$Data_Type==1) { # One_D - 64 bits
       seek(to.read,where=Header$Data_Offset_Start[1], origin="current")
       readpoints <-  Header$Data_Offset_Stop[1]-Header$Data_Offset_Start[1]+1;
       rawR <- readBin(to.read, what="numeric", n=readpoints, size=8L, endian = endian)
       seek(to.read,where=Header$Data_Offset_Start[1], origin="current")
       rawI <- readBin(to.read, what="numeric", n=readpoints, size=8L, endian = endian)
       fid  <- complex(real=rawR, imaginary=rawI)
   }
   close(to.read)

   #--------------
   # Acq parameters + Real Spectrum
   #--------------
   gp <- function(varlabel, defvalue='') {
        expr<-paste0('procpar$',varlabel,'$value')
        value <- eval(parse(text=expr))
        ifelse( is.null(value), defvalue, value)
   }

   NUC <- "1H"
   if( tolower(procpar$x_domain$value=="carbon") ) NUC <- "13C"

   acq <- list( INSTRUMENT="JEOL", SOFTWARE=gp('version','undef'),
                ORIGIN=Header$File_Identifier, ORIGPATH=Header$Title, SOLVENT=procpar$solvent$value, TEMP=procpar$temp_set$value, 
                PROBE=procpar$x_probe_map$value, PULSE=procpar$experiment$value, NUC=NUC,
                NUMBEROFSCANS=procpar$total_scans$value, DUMMYSCANS=procpar$x_prescans$value, PULSEWIDTH=procpar$x_pulse$value,
                RELAXDELAY=procpar$relaxation_delay$value, SPINNINGRATE=procpar$spin_set$value, TD=procpar$x_points$value, 
                SW=procpar$x_sweep$value/Header$Base_Freq[1], SWH=procpar$x_sweep$value, OFFSET=procpar$x_offset$value,
                SFO1=procpar$irr_freq$value, O1=procpar$x_offset$value*Header$Base_Freq[1], GRPDLY=0 )
   acq$TD <- length(fid)

   # Unit issues
   if (procpar$temp_set$Unit=="dC") acq$TEMP <- acq$TEMP + 273.15
   if (procpar$irr_freq$value>1000000)  acq$SFO1 <- acq$SFO1/1000000

   # Group Delay : Internal or External
   if (Spec1rProcpar$JGD_INNER) {
      GRPDLY <- 0
      factors <- eval(parse(text=paste0("c(",gsub(" +",",",procpar$factors$value),")")))
      orders <- eval(parse(text=paste0("c(",gsub(" +",",",procpar$orders$value),")")))
      for (k in 1:orders[1]) {
            GRPDLY <- GRPDLY+ 0.5*(( orders[k+1] - 1)/prod(factors[k:orders[1]]));
      }
      acq$GRPDLY <- GRPDLY
   } else {
      acq$GRPDLY <- .estime_grpdelay(fid)
   }

   spec <- list( path=FILE, acq=acq, fid=fid )

   spec
}



#--------------------------------
# nmrML : FID only
#--------------------------------

#### Read FID data from the nmrML file 'filename'
#--  internal routine
.read.FID.nmrML <- function(filename)
{
   if (!file.exists(filename))
       stop("File ", filename, " does not exist\n")

   what <- "double"
   endian <- "little"
   sizeof <- 8
   compression <- "gzip"
   
   tree <- xmlTreeParse(filename)
   root <- xmlRoot(tree)

   fidData <- xmlElementsByTagName(root, "fidData", recursive = TRUE)[["acquisition.acquisition1D.fidData"]]
   b64string <- gsub("\n", "", xmlValue(fidData))
   byteFormat <- xmlAttrs(fidData)["byteFormat"]
   raws <- memDecompress(base64decode(b64string), type=compression)
   signal <- readBin(raws, n=length(raws), what=what, size=sizeof, endian = endian)
   TD <- length(signal)

   SFO1 <- as.double(xmlAttrs(xmlElementsByTagName(root, "irradiationFrequency", recursive = TRUE)[[1]])["value"])
   O1 <- as.double(xmlAttrs(xmlElementsByTagName(root, "irradiationFrequencyOffset", recursive = TRUE)[[1]])["value"])
   SWH <-  as.double(xmlAttrs(xmlElementsByTagName(root, "sweepWidth", recursive = TRUE)[[1]])["value"])
   SW <- SWH/SFO1
   TD  <-  as.integer(xmlAttrs(xmlElementsByTagName(root, "DirectDimensionParameterSet", recursive = TRUE)[[1]])["numberOfDataPoints"])

   TEMP <- as.double(xmlAttrs(xmlElementsByTagName(root, "sampleAcquisitionTemperature", recursive = TRUE)[[1]])["value"])
   RELAXDELAY <- as.double(xmlAttrs(xmlElementsByTagName(root, "relaxationDelay", recursive = TRUE)[[1]])["value"])
   SPINNINGRATE <- as.double(xmlAttrs(xmlElementsByTagName(root, "spinningRate", recursive = TRUE)[[1]])["value"])
   PULSEWIDTH <- as.double(xmlAttrs(xmlElementsByTagName(root, "pulseWidth", recursive = TRUE)[[1]])["value"])
   GRPDLY  <- 0
   if ( length(xmlElementsByTagName(root, "groupDelay", recursive = TRUE)) ) {
       GRPDLY <- as.double(xmlAttrs(xmlElementsByTagName(root, "groupDelay", recursive = TRUE)[[1]])["value"])
   }

   instrument <- xmlElementsByTagName(root, "instrumentConfiguration", recursive = TRUE)[[1]]
   INSTRUMENT <- xmlAttrs(xmlElementsByTagName(instrument,"cvParam")[[1]])["name"]
   PROBE <- xmlAttrs(xmlElementsByTagName(instrument,"userParam")[[1]])["value"]
   NUC_LABEL <- xmlAttrs(xmlElementsByTagName(root, "acquisitionNucleus", recursive = TRUE)[[1]])["name"]
   if (length(grep("hydrogen",NUC_LABEL))>0) NUC <- '1H'
   if (length(grep("carbon",NUC_LABEL))>0)   NUC <- '13C'

   SOFTWARE <- ''
   PULSE   <- ''

   ORIGIN   <- '-'
   repeat {
       if ( length(grep("BRUKER",toupper(INSTRUMENT)))>0  ) { ORIGIN <- 'BRUKER'; break; }
       if ( length(grep("VARIAN",toupper(INSTRUMENT)))>0  ) { ORIGIN <- 'VARIAN'; break; }
       if ( length(grep("JEOL",  toupper(INSTRUMENT)))>0  ) { ORIGIN <- 'JEOL';   break; }
       break
   }
   ORIGPATH <- '-'
   if ( length(xmlAttrs(xmlElementsByTagName(root, "acquisition1D", recursive = TRUE)[[1]])["id"]) ) {
      ORIGPATH <- xmlAttrs(xmlElementsByTagName(root, "acquisition1D", recursive = TRUE)[[1]])["id"]
   } else {
      ORIGPATH <- gsub(".nmrML", "", basename(filename))
   }
   SOLVENT <- '-'

   td <- length(signal)
   rawR <- signal[seq(from = 1, to = td, by = 2)]
   rawI <- signal[seq(from = 2, to = td, by = 2)]
   fid <- complex(real=rawR, imaginary=rawI)
   TD <- length(fid)

   # Bruker && Jeol : Estimation of the Group Delay if zero
   if (GRPDLY==0 && (length(grep("Bruker",INSTRUMENT))>0 || length(grep("JEOL",INSTRUMENT))>0)) {
       GRPDLY <- .estime_grpdelay(fid)
   }

   acq <- list( INSTRUMENT=INSTRUMENT, SOFTWARE=SOFTWARE, ORIGIN=ORIGIN, ORIGPATH=ORIGPATH, PROBE=PROBE, PULSE=PULSE, SOLVENT=SOLVENT,
                RELAXDELAY=RELAXDELAY, SPINNINGRATE=SPINNINGRATE, PULSEWIDTH=PULSEWIDTH, TEMP=TEMP, NUC=NUC,
                NUMBEROFSCANS='-', DUMMYSCANS='-', TD=TD, SW=SW, SWH=SWH, SFO1=SFO1, O1=O1, OFFSET=O1/SFO1, GRPDLY=GRPDLY )
   spec <- list( path=filename, acq=acq, fid=fid )

   spec
}



#--------------------------------
# Magritek : 1R only
#--------------------------------

#### Retrieve a parameter value from Magritek acquisition parameters
#--  internal routine
#--  ACQ: list of acquisition parameters of the acqu.par file
#--  paramStr: name of the parameter
.magritek.get_param <- function (ACQ,paramStr,type="numeric")
{
   regexpStr <- paste("^",paramStr," +=",sep="")
   acqval <-  gsub("^[^=]+= ?","",ACQ[which(simplify2array(regexpr(regexpStr,ACQ))>0)])
   if (type=="numeric") {
     acqval <- as.numeric(acqval)
   } else {
     acqval <- gsub("\\\"","", acqval)
   }
   acqval
}

# Read Header
.magritek.read_header <- function(con)
{
   revstr <- function(s) paste(rev(strsplit(s,'')[[1]]), collapse='')
   Header <- list(
      owner=revstr(readChar(con, 4)),
      format=revstr(readChar(con, 4)),
      version=revstr(readChar(con, 4)),
      dataType=readBin(con, what="int", n=1, size=4, signed=TRUE),
      xDim=readBin(con, what="int", n=1, size=4, signed=TRUE),
      yDim=readBin(con, what="int", n=1, size=4, signed=TRUE),
      zDim=readBin(con, what="int", n=1, size=4, signed=TRUE),
      qDim=readBin(con, what="int", n=1, size=4, signed=TRUE)
   )
   Header
}

# Read Data
.magritek.read_data <- function(con, Header) {
   readpoints <- Header$xDim*12
   rawR <- readBin(con, what="numeric", n=readpoints, size=4, signed=TRUE, endian = "little")
   xDim <- Header$xDim
   X <- rawR[1:xDim]
   Y <- rawR[ c(rep(FALSE,xDim),((xDim+1):(3*xDim) %% 2)==1) ]
   Z <- rawR[ c(rep(FALSE,xDim),((xDim+1):(3*xDim) %% 2)==0) ]
   list( X=X, Y=Y, Z=Z )
}

#### Read 1r data and the main parameters
#--  internal routine
#-- DIR: rs2d directory containing the experience
.read.1r.magritek <- function(DIR)
{
   cur_dir <- getwd()
   setwd(DIR)
   
   # Spectrum filename
   DATAFILE <- "spectrum.1d"
   if (!file.exists(DATAFILE)) 
       stop("DATA File ", DATAFILE, " does not exist\n")
   # Acquisition parameters filename
   ACQFILE <- "acqu.par"
   if (!file.exists(ACQFILE)) 
       stop("Acquisition parameter File (", ACQFILE, ") does not exist\n")

   # Read Spectrum
   to.read = file(DATAFILE,"rb")
   Header <- .magritek.read_header(to.read)
   rawR <- .magritek.read_data(to.read, Header)
   close(to.read)

   # ACQ parameters
   ACQ  <- readLines(ACQFILE)
   setwd(cur_dir)

   INSTRUMENT <- paste(.magritek.get_param(ACQ,"Spectrometer",type="string"),.magritek.get_param(ACQ,"InstrumentType",type="string"))
   SFO1 <- .magritek.get_param(ACQ,"b1Freq")
   PULSEWIDTH <- .magritek.get_param(ACQ,"pulseAmplitude90")
   TEMP <- .magritek.get_param(ACQ,"CurrentTemperatureMagnet") + 273.15
   SOLVENT <- .magritek.get_param(ACQ,"Solvent",type="string")
   NUC <- .magritek.get_param(ACQ,"rxChannel",type="string")
   NUMBEROFSCANS = .magritek.get_param(ACQ,"nrScans")
   SOFTWARE <- .magritek.get_param(ACQ,"Software",type="string")

   SW = max(rawR$X) - min(rawR$X)
   SWH <- SW*SFO1
   SI <- TD <- length(rawR$X)
   PULSE <- "zg30"
   PHC0 <- PHC1 <- 0
   AQ <- RELAXDELAY <- SPINNINGRATE <- O1 <- GRPDLY <- 0
   ORIGIN <- PROBE <- ""
   ORIGPATH <- DIR

   acq <- list( INSTRUMENT=INSTRUMENT, SOFTWARE=SOFTWARE, ORIGIN=ORIGIN, ORIGPATH=ORIGPATH, 
                   PROBE=PROBE, PULSE=PULSE, NUC=NUC, SOLVENT=SOLVENT, TEMP=TEMP, 
                   RELAXDELAY=RELAXDELAY, SPINNINGRATE=SPINNINGRATE, PULSEWIDTH=PULSEWIDTH,
                   TD=TD, SW=SW, SWH=SWH, SFO1=SFO1, O1=O1, GRPDLY=GRPDLY )
   
   proc <- list( phc0=PHC0*pi/180, phc1=PHC1*pi/180, SI=SI )
   
   dppm <- SW/(SI-1)
   pmin <- min(rawR$X)
   pmax <- max(rawR$X)
   ppm <- rawR$X
   
   spec <- list( path=getwd(), param=NULL, acq=acq, proc=proc, fid=NULL, int=rawR$Y, dppm=dppm, pmin=pmin, pmax=pmax, ppm=ppm )
   spec
}



#--------------------------------
# Pre-Processing
#--------------------------------

### Group Delay correction
.groupDelay_correction <- function(spec, param=Spec1rProcpar)
{
    fid <- spec$fid
    if (! is.null(param$GRDFLG) && param$GRDFLG) spec$acq$GRPDLY <- .estime_grpdelay(fid)
    if (spec$acq$GRPDLY>0) {
       m <- length(fid)
       if (is.null(param$OC)) {
          P <- sqrt(  Re(fid)^2 + Im(fid)^2 )
          nd0 <- which(P>max(P)/2)[1];
          param$OC <- (sign(Re(fid)[nd0])==sign(Im(fid)[nd0]))
       }
       # Omega Centred ?
       if (param$OC) {
          Omega <- ((-m/2):(m/2-1))/m
       } else {
          Omega <- (0:(m-1))/m
       }
       i <- complex(real=0,imaginary=1)
       p <- ceiling(m/2); seq1 <- ( (p+1):m ); seq2 <- ( 1:p )
       Spectrum <- stats::fft(fid)
       Spectrum <- c( Spectrum[seq1], Spectrum[seq2] )
       Spectrum <- Spectrum * exp(i*spec$acq$GRPDLY*2*pi*Omega)
       p <- length(seq1); seq1 <- ( (p+1):m ); seq2 <- ( 1:p )
       Spectrum <- c( Spectrum[seq1], Spectrum[seq2] )
       fid <- stats::fft(Spectrum, inverse = TRUE)
    }
    fid
}

### removeLowFreq : remove low frequencies 
#       by applying a polynomial subtraction method.
#  np : polynomial order
# See https://www.rezolytics.com/articles/4/
#     https://www.r-bloggers.com/fitting-polynomial-regression-in-r/
.removeLowFreq <- function(fid, np=5)
{
   rawR <- Re(fid)
   rawI <- Im(fid)
   
   model <- stats::lm(rawR ~ poly(1:length(rawR), np))
   rM <- stats::fitted(model)
   rawR <- rawR - rM
   
   model <- stats::lm(rawI ~ poly(1:length(rawR), np))
   iM <- stats::fitted(model)
   rawI <- rawI - iM

   complex(real=rawR, imaginary=rawI)
}

#### Apply some preprocessing: zero_filling, line broading
#--  Generate real and imaginary parts
.preprocess <- function(spec, param=Spec1rProcpar)
{
    logfile <- param$LOGFILE

    td <- length(spec$fid)

    ### Line Broadening
    if (param$LINEBROADENING && param$LB!=0) {
       if(param$DEBUG) .v("\tExp. Line Broadening (LB=%f)\n", param$LB, logfile=logfile)
       t <- seq(0,td-1)/(2*spec$acq$SWH)
       if (param$GB==0) {
           vlb <- exp( -t*param$LB*pi )
       } else {
           if(param$DEBUG) .v("\tGauss. Line Broadening (GB=%f)\n", param$GB, logfile=logfile)
           AQ <- td/(2*spec$acq$SWH)
           vlb <- exp(  t*param$LB*pi - ( t^2 )*param$LB*pi/(2*param$GB*AQ) )
           # Test : sin bell (LB>0) & sin bell squared (LB<0) : param$GB in [ pi/6, pi/4 ]
           # vlb <- sin( (pi-param$GB)*t/AQ+param$GB)
           # if (param$LB<0) vlb <- vlb^2
           Tmax <- max(vlb); vlb <- vlb/Tmax
       }
       spec$fid <- vlb*spec$fid
    }

    if (param$DEBUG) .v("\tTD = %d\n", td, logfile=logfile)

    ### TD needs to be power of 2; if not, apply a padding of zeros
    tdp2 <- 2^round(log2(td)+0.4999)
    if (td < tdp2 ) {
       if(param$DEBUG) .v("\tZero Padding = %d\n", tdp2 - td, logfile=logfile)
       spec$fid <- c( spec$fid, rep(complex(real=0, imaginary=0), (tdp2-td)) )
       td <- length(spec$fid)
    }

    transform2spec <- function(fid) {
       ### FFT of FID
       if(param$DEBUG) .v("\tFFT ...", logfile=logfile)
       Spectrum <- stats::fft(fid)
       if(param$DEBUG) .v("OK\n", logfile=logfile)
       ### Rotation
       td <- length(Spectrum); p <- td/2
       spec.p <- c( Spectrum[(p+1):td], Spectrum[1:p] )
       rawspec <- spec.p
       rawspec
    }

    # Compute the spectrum in freq. domain before zero filling
    spec$fid0 <- .groupDelay_correction(spec, param)
    rawspec <- transform2spec(spec$fid0)
    spec$data0 <- rawspec
   
    ### Zero filling
    if (param$ZEROFILLING) {
       TDMAX <- min(param$ZFFAC*td,131072)
       if(param$DEBUG) .v("\tZero Filling (x%d)\n", round(TDMAX/td), logfile=logfile)
       while ((td/TDMAX) < 1 ) {
          td <- length(spec$fid)
          spec$fid <- c( spec$fid, rep(complex(real=0, imaginary=0), td) )
          td <- length(spec$fid)
       }
       td <- length(spec$fid)

       ### Group Delay correction if needed
       if (spec$acq$GRPDLY>0) {
          if(param$DEBUG) .v("\tApplied GRPDLY ...", logfile=logfile)
          spec$fid <- .groupDelay_correction(spec, param)
          if(param$DEBUG) .v("OK\n", logfile=logfile)
       }
       # Compute the spectrum in freq. domain after zero filling
       rawspec <- transform2spec(spec$fid)

    } else {
       if(param$DEBUG) .v("\tApplied GRPDLY ...OK\n", logfile=logfile)
       spec$fid <- spec$fid0
    }
    if (param$DEBUG) .v("\tSI = %d\n", td, logfile=logfile)

    ## Remove low frequencies 
    if(param$REMLFREQ>0) {
       if(param$DEBUG) .v("Remove low frequencies  ...\n",logfile=logfile)
       spec$fid <- .removeLowFreq(spec$fid, np=param$REMLFREQ)
       if(param$DEBUG) .v("OK\n",logfile=logfile)
    }

    param$SI <- length(rawspec)
    proc <- list( phc0=0, phc1=0, crit=NULL, RMS=0, SI=length(rawspec))
    if (! "phc0" %in% names(param) || is.null(param$phc0)) param$phc0 <- param$phc1 <- 0

    # PPM Calibration
    m <- proc$SI
    SW <- spec$acq$SW
    SWH <- spec$acq$SWH
    if (param$O1RATIO==1) {
        offset <- spec$acq$OFFSET
    } else {
        offset <- SW*param$O1RATIO
    }
    spec$dppm <- SW/(m-1)
    spec$pmin <- offset - SW/2
    spec$pmax <- SW + spec$pmin
    spec$ppm <- seq(from=spec$pmin, to=spec$pmax, by=spec$dppm)
    spec$B <- C_estime_sd(Re(spec$data0),128)

    ### Save into the spec object instance
    spec$data <- rawspec
    spec$param <- param
    spec$proc <- proc

    ### return the spec object instance
    spec
}



#--------------------------------
# Phase correction
#--------------------------------

.zeroNegPeaks <- function(spec,Yre)
{
   Ythres <- -spec$param$KZEROTHRES*spec$B; n1 <- 0
   for (k in 1:length(Yre)) {
       if (Yre[k]<Ythres && n1==0) { n1 <- k; next }
       if (Yre[k]>Ythres && n1>0) {  Yre[n1:(k-1)] <- 0; n1 <- 0 }
   }
   Yre
}

.computeCrit <- function(spec, phc) {
   V <- spec$data0;
   if (spec$param$REVPPM) V <- rev(V)
   spec1r <- C_corr_spec_re(list(re=Re(V),im=Im(V), phc0=phc[1], phc1=phc[2]))
   NPBLSM <- 100
   Yre <- spec1r$re - C_Estime_LB2 (spec1r$re, 1, length(spec1r$re)-1, NPBLSM, NPBLSM, 6*spec$B)
   n <- round(length(Yre)/24)
   Yre <- Yre[n:(23*n)]
   if (spec$param$BLPHC>0) Yre <- Yre + rep(spec$B/4, length(Yre))
   #x0 <- 0.5*(spec$pmax-spec$pmin) + spec$param$KZERO*c(-1,1)
   #Yre[ round(length(Yre)*x0[1]/spec$acq$SW):round(length(Yre)*x0[2]/spec$acq$SW) ] <- 0
   Yre <- .zeroNegPeaks(spec,Yre)
   entropy <-  tryCatch({
         Fentropy(phc, Re(V),Im(V), spec$param$BLPHC, spec$param$BLPHC, spec$param$KSIG*spec$B, spec$param$GAMMA)
   }, error = function(e) {
      return(0)
   })
   crit <- c( sum(Yre[Yre>0]^2), sum(abs(Yre[Yre<0]^2)), entropy, sum(abs(Yre)) )
   crit
}

.checkPhc <- function(spec, phc, lopt, count=0) {
   phc0 <- phc[1]*180/pi; 
   phc[1] <- (floor(phc0) %% 360 + phc0 %% 1)*pi/180
   if (phc[1]<0) phc[1] <- phc[1] + 360
   crit <- .computeCrit(spec, phc)
   RatioPosNegMin <- spec$param$RATIOPOSNEGMIN
   if ( crit[1] < RatioPosNegMin*crit[2] ) {
       if (spec$param$DEBUG).v("\n\t%d: Spos= %2.4e, Sneg= %2.4e, Rotation of phc0: %3.6f => ", 
                                lopt, crit[1], crit[2], phc[1]*180/pi, logfile=spec$param$LOGFILE)
       if (phc[1]>pi) phc[1] <- phc[1] - pi
       else           phc[1] <- phc[1] + pi
       crit <- .computeCrit(spec, phc)
       if (spec$param$DEBUG) .v("%3.6f", phc[1]*180/pi, logfile=spec$param$LOGFILE)
   }
   if (spec$param$DEBUG).v("\n\t%d: [%d] Spos= %2.4e, Sneg= %2.4e, phc=(%3.6f, %3.6f)   ", 
                 lopt, count, crit[1], crit[2], phc[1]*180/pi, phc[2]*180/pi, logfile=spec$param$LOGFILE)
   list(crit=crit, phc=phc)
}

.capSolvent <- function(spec,lopt)
{
   V <- spec$data0;
   if (spec$param$REVPPM) V <- rev(V)
   SI <- length(V)
   x0 <- 0.5*(spec$pmax-spec$pmin) + spec$param$KZERO*c(-1,1)
   if (spec$param$DEBUG) .v("\n\t%d: -- masking the ppm range = (%3.6f, %3.6f)   ", lopt, x0[1]+spec$pmin, x0[2]+spec$pmin, logfile=spec$param$LOGFILE)
   V[ round(SI*x0[1]/spec$acq$SW):round(SI*x0[2]/spec$acq$SW) ] <- 0+0i
   V
}

.maskNegPeaks <- function(spec,Y,lopt)
{
   V <- spec$data0;
   if (spec$param$REVPPM) V <- rev(V)
   SI <- length(V)
   Ythres <- -spec$param$KZEROTHRES*spec$B
   n1 <- 0
   for (k in 1:SI) {
       if (Y$re[k]<Ythres && n1==0) { n1 <- k; next }
       if (Y$re[k]>Ythres && n1>0) {
           x0 <- spec$acq$SW*c(n1,k-1)/SI + spec$pmin
           #if (spec$param$DEBUG) .v("\n\t%d: -- masking the ppm range = (%3.6f, %3.6f)   ", lopt, x0[1], x0[2], logfile=spec$param$LOGFILE)
           V[n1:(k-1)] <- 0+0i
           n1 <- 0
       }
   }
   V
}

.optimphase0 <- function(spec)
{
   # functions to be optimised
   rms0 <- function(ang, y, B, type=0) {
       Yrot <- C_corr_spec_re(list(re=Re(y),im=Im(y), phc0=ang, phc1=0))
       Yre <- Yrot$re
       NPBLSM <- 100
       Yre <- Yre - C_Estime_LB2 (Yre, 1, length(Yre)-1, NPBLSM, NPBLSM, 6*B)
       n <- round(length(Yre)/24)
       Yre <- Yre[n:(23*n)]
       if (type==0) ret <- sum((Yre[Yre<0])^2);
       if (type==1) ret <- sum((Yre[Yre>0])^2);
       return(ret)
   }

   sumneg0 <- function(ang, y, n1, n2) {
       Yrot <- C_corr_spec_re(list(re=Re(y),im=Im(y), phc0=ang, phc1=0))
       Yre <- Yrot$re
       a <- (Yre[n2]-Yre[n1])/(n2-n1);  b <- (Yre[n1]*n2-Yre[n2]*n1)/(n2-n1)
       #10000000*sum(sapply(n1:n2, function(k){ Y<-a*k + b; ifelse(Yre[k]<Y, abs(Y - Yre[k]), 0) }))
       10000000*sum(sapply(n1:n2, function(k){ Y<-a*k + b; ifelse(Yre[k]<Y, (Y - Yre[k])*(Y - Yre[k]), 0) }))
   }

   mean0 <- function(ang, y, n1, n2) {
       Yrot <- C_corr_spec_re(list(re=Re(y),im=Im(y), phc0=ang, phc1=0))
       Yre <- Yrot$re
       Ym <- mean(Yre[n1:n2])
       10000000*sum((Yre[n1:n2]-Ym)*(Yre[n1:n2]-Ym))
   }

   sumneg <- function(ang, y, n1, n2) {
       Yrot <- C_corr_spec_re(list(re=Re(y),im=Im(y), phc0=ang, phc1=0))
       Yre <- Yrot$re
       10000000*sum(sapply(n1:n2, function(k){ ifelse(Yre[k]<0, Yre[k]*Yre[k],0) }))
   }

   V <- spec$data0
   if (spec$param$REVPPM) V <- rev(V)
   crittype <- spec$param$OPTCRIT0
   best <- stats::optimize(rms0, interval = c(-2*pi, 2*pi), maximum = FALSE, y = V, B=spec$B, type=crittype)
   L <- .checkPhc(spec, c(best[["minimum"]],0), 0)
   crit0 <- L$crit
   CritID <- spec$param$OPTCRIT1
   if (spec$acq$NUC %in% c('1H','H1','31P')) {
       #V <- .capSolvent(spec,0);
       V <- .maskNegPeaks(spec,C_corr_spec_re(list(re=Re(spec$data0),im=Im(spec$data0), phc0=best[["minimum"]], phc1=0)),0);
       best2 <- stats::optimize(rms0, interval = c(-2*pi, 2*pi), maximum = FALSE, y = V, B=spec$B, type=crittype)
       L2 <- .checkPhc(spec, c(best2[["minimum"]],0), 0)
       if (L2$crit[CritID] < crit0[CritID]) { L <- L2; crit0 <- L2$crit }
   }

   # Adjust phc0 if PULSE is of type CP
   pulse1 <- length(grep(spec$param$CPREGEX, toupper(spec$acq$PULSE)))>0
   pulse2 <- length(grep("CPMG", toupper(spec$acq$PULSE)))>0
   pulse3 <- length(grep("NOESY", toupper(spec$acq$PULSE)))>0
   if (! spec$param$TSP && (pulse1 || pulse2 || pulse3)) {
       if (pulse1) {
           if (is.null(spec$param$KSTART)) spec$param$KSTART <- 0.15
           if (is.null(spec$param$KSTOP))  spec$param$KSTOP  <- 0.85
       }
       if (pulse2 || pulse3) {
           if (is.null(spec$param$KSTART)) spec$param$KSTART <- 0.18
           if (is.null(spec$param$KSTOP))  spec$param$KSTOP  <- 0.47
       }
       fspec <- stats::fft(spec$fid)
       m <- length(fspec); p <- ceiling(m/2)
       fspec <- c( fspec[(p+1):m], fspec[1:p] )
       if ( spec$param$REVPPM ) fspec <- fspec[rev(1:m)]
       n1 <- round(spec$param$KSTART*length(fspec))
       n2 <- round(spec$param$KSTOP*length(fspec))
       phc0 <- L$phc[1]
       dPHC <- 0.7854
       best <- stats::optimize(sumneg, interval = c(phc0-dPHC, phc0+dPHC), maximum = FALSE, y = fspec, n1=n1, n2=n2)
       nloop <- 1
       repeat {
          if (nloop==20 || abs(best[["minimum"]])<6.28) break
          best <- stats::optimize(sumneg, interval = c(-3.14, 3.14), maximum = FALSE, y = fspec, n1=n1, n2=n2)
          nloop <- nloop + 1
       }
       if (spec$param$DEBUG) .v("\n\t%d: KSTART = %1.2f , KSTOP = %1.2f, nloop = %d", 
                                0, spec$param$KSTART, spec$param$KSTOP, nloop, logfile=spec$param$LOGFILE)
       phc0 <- best[["minimum"]]
       L <- .checkPhc(spec, c(phc0,0), 0)
   }

   # Ajust phc0 based on TSP peak if present
   if (spec$param$TSP && spec$param$ADJPZTSP) {
       if (spec$param$DEBUG) .v("\n\t%d: ADJPZTSP: 0.00 +/- %2.1f Hz, MVPZTSP: %d, DPHC=%2.3f , DPHC2=%d", 
                                0, spec$param$DHZPZTSP, spec$param$MVPZTSP, spec$param$DPHCPZTSP, spec$param$DHZPZRANGE, 
                                logfile=spec$param$LOGFILE)
       m <- spec$proc$SI
       SW <- spec$acq$SW
       x0 <- abs(spec$pmin)/SW
       n1 <- round(m*(x0-0.4/SW))
       n2 <- round(m*(x0+0.2/SW))
       fspec <- stats::fft(spec$fid)
       m <- length(fspec); p <- ceiling(m/2)
       fspec <- c( fspec[(p+1):m], fspec[1:p] )
       if ( spec$param$REVPPM ) fspec <- fspec[rev(1:m)]
       phc0 <- L$phc[1]
       new_spec1r <- C_corr_spec_re(list(re=Re(fspec),im=Im(fspec), phc0=phc0, phc1=0))
       Yre <- new_spec1r$re
       if (max(Yre[n1:n2])>10*C_estime_sd(Yre,128)) {
           n0 <- which(Yre[n1:n2] == max(Yre[n1:n2])) + n1 - 1
           dPHC <- spec$param$DPHCPZTSP
           dN <- round(spec$param$DHZPZTSP/(spec$acq$SFO1*spec$dppm))
           best2 <- stats::optimize(sumneg0, interval = c(phc0-dPHC, phc0+dPHC), maximum = FALSE, y = fspec, n1=n0-dN, n2=n0+dN)
           phc0 <- best2[["minimum"]]
           L2 <- .checkPhc(spec, c(phc0,0), 0)
           cond <- (! L2$crit[CritID] > 1.25* crit0[CritID]) || (! L2$crit[3] > 1.25* crit0[3]) 
           if (cond) { L <- L2; crit0 <- L2$crit }
           # adjustment of the mean value close to the TSP
           if (spec$param$MVPZTSP) {
               n1 <- n0 + dN
               n2 <- n1 + round(spec$param$DHZPZRANGE/(spec$acq$SFO1*spec$dppm))
               best2 <- stats::optimize(mean0, interval = c(phc0-dPHC/10, phc0+dPHC/10), maximum = FALSE, y = fspec, n1=n1, n2=n2)
               L2 <- .checkPhc(spec, c(best2[["minimum"]],0), 0)
               FAC <- spec$param$MVPZFAC
               cond <- (! L2$crit[CritID] > FAC* crit0[CritID]) || (! L2$crit[3] > FAC* crit0[3])
               if (cond) { L <- L2; crit0 <- L2$crit }
           }
       }
   }

   spec$proc$crit <- L$crit
   spec$proc$phc0 <- L$phc[1]
   spec$proc$RMS <- L$crit[CritID]
   if (spec$param$DEBUG) .v("\n\tBest solution: phc0 = %3.6f, Sneg= %2.4e   ", L$phc[1]*180/pi, L$crit[2], logfile=spec$param$LOGFILE)
   spec
}

.optimRun <- function(spec, V, phc, flg, lopt)
{
   ret <- FALSE
   CritID <- spec$param$OPTCRIT1
   crit_prev <- spec$proc$crit
   N <- 2; C <- 0
   while( C==0 && N>0 ) {
      opt <- tryCatch({
                 stats::optim(par=phc, fn=Fmin, method="Nelder-Mead", re=Re(V), im=Im(V), 
                                blphc=spec$param$BLPHC, B=spec$param$KSIG*spec$B, flg=flg, control=list(maxit=200))
             },  error = function(e) {  list(convergence=1) })
      if (opt$convergence==0) {
         C <- 1
         L <- .checkPhc(spec, opt$par, lopt, opt$counts[1]); crit <- L$crit; opt$par <- L$phc
         if ( ! is.vector(crit_prev) || (crit[CritID]<crit_prev[CritID]) ) {
            spec$proc$phc0 <- opt$par[1]
            spec$proc$phc1 <- opt$par[2]
            spec$proc$RMS <- crit[CritID]
            spec$proc$crit <- crit
            ret <- TRUE
         } else {
            spec$proc$crit <- crit_prev
         }
      }
      else {
         if (spec$param$DEBUG).v("\n\t%d: No convergence   ",  lopt, logfile=spec$param$LOGFILE)
         N <- N - 1
      }
      phc <- c( stats::runif(1, -pi, pi), stats::runif(1, -pi/10, pi/10) )
   }
   lopt <- lopt + 1
   return( list(spec=spec, lopt=lopt, ret=ret) )
}

.optimExec <- function(spec, V, phc, flg, lopt)
{
   L <- .optimRun(spec, V, phc, flg, lopt); spec <- L$spec; lopt <- L$lopt
   if ( spec$param$BLPHC>0 && spec$acq$NUC %in% c('1H','H1','31P') ) { # spec$param$BLPHC>0 && 
        #V <- .capSolvent(spec,lopt);
        V <- .maskNegPeaks(spec,C_corr_spec_re(list(re=Re(spec$data0),im=Im(spec$data0), phc0=spec$proc$phc0, phc1=spec$proc$phc1)),0);
        L <- .optimRun(spec, V, phc, flg, lopt); spec <- L$spec; lopt <- L$lopt
   }
   return( list(spec=spec, lopt=lopt) )
}

.optimphase1 <- function(spec)
{
   BLPHC <- spec$param$BLPHC
   CRITSTEP1 <- spec$param$CRITSTEP1
   CRITSTEP2 <- spec$param$CRITSTEP2
   V <- spec$data0;
   if (spec$param$REVPPM) V <- rev(V)

   lopt <- 1

   phc_init <- c(spec$proc$phc0,spec$proc$phc1)

   # BLPHC==0
   spec$param$BLPHC <-  0
   L <- .optimExec(spec, V, phc_init, CRITSTEP1, lopt); spec <- L$spec; lopt <- L$lopt
   if (spec$param$OPTSTEP && CRITSTEP1>0) {
      phc <- c(spec$proc$phc0,spec$proc$phc1)
      L <- .optimExec(spec, V, phc, CRITSTEP2, lopt); spec <- L$spec; lopt <- L$lopt
   }

   # BLPHC>0
   spec$param$BLPHC <- BLPHC
   L <- .optimExec(spec, V, phc_init, CRITSTEP1, lopt); spec <- L$spec; lopt <- L$lopt
   if (spec$param$OPTSTEP && CRITSTEP1>0) {
      phc <- c(spec$proc$phc0,spec$proc$phc1)
      L <- .optimExec(spec, V, phc, CRITSTEP2, lopt); spec <- L$spec; lopt <- L$lopt
   }

   sumneg <- function(par, y, n1, n2) {
      Yrot <- C_corr_spec_re(list(re=Re(y),im=Im(y), phc0=par[1], phc1=par[2]))
      Yre <- Yrot$re
      10000000*sum(sapply(n1:n2, function(k){ ifelse(Yre[k]<0, Yre[k]*Yre[k],0) }))
   }

   pulse1 <- length(grep(spec$param$CPREGEX, toupper(spec$acq$PULSE)))>0
   pulse2 <- length(grep("CPMG", toupper(spec$acq$PULSE)))>0
   pulse3 <- length(grep("NOESY", toupper(spec$acq$PULSE)))>0
   if (pulse1 || pulse2 || pulse3) {
      if (pulse1) {
           if (is.null(spec$param$KSTART)) spec$param$KSTART <- 0.15
           if (is.null(spec$param$KSTOP))  spec$param$KSTOP  <- 0.85
      }
      if (pulse2 || pulse3) {
           if (is.null(spec$param$KSTART)) spec$param$KSTART <- 0.18
           if (is.null(spec$param$KSTOP))  spec$param$KSTOP  <- 0.47
      }
      fspec <- stats::fft(spec$fid)
      m <- length(fspec); p <- ceiling(m/2)
      fspec <- c( fspec[(p+1):m], fspec[1:p] )
      if ( spec$param$REVPPM ) fspec <- fspec[rev(1:m)]
      n1 <- round(spec$param$KSTART*length(fspec))
      n2 <- round(spec$param$KSTOP*length(fspec))
      phc <- c(spec$proc$phc0, spec$proc$phc1)
      nloop <- 0
      while(nloop<20) {
          best <- stats::optim(par=phc, fn=sumneg, method="Nelder-Mead", y = fspec,  n1=n1, n2=n2, control=list(maxit=200))
          nloop <- nloop + 1
          if (abs(best$par[1])<9.425 && abs(best$par[2])<1.571) break
          phc <- c( runif(1,0,3.14), runif(1,-0.7854,0.7854) )
      }
      if (spec$param$DEBUG) .v("\n\t%d: KSTART = %1.2f , KSTOP = %1.2f, nloop = %d", 
                                0, spec$param$KSTART, spec$param$KSTOP, nloop, logfile=spec$param$LOGFILE)
      spec$proc$phc0 <- best$par[1]
      spec$proc$phc1 <- best$par[2]
      L <- .checkPhc(spec, c(spec$proc$phc0,spec$proc$phc1), 0)
   }

   if (spec$param$DEBUG) .v("\nBest solution: phc = (%3.6f, %3.6f)   ", spec$proc$phc0*180/pi, spec$proc$phc1*180/pi, logfile=spec$param$LOGFILE)
   spec
}

# Compute the final spectra based on the best optimisation
.computeSpec <- function(spec)
{
   if (!spec$param$OPTPHC0 && !spec$param$OPTPHC1) {
      spec$proc$phc0 <- spec$param$phc0
      spec$proc$phc1 <- spec$param$phc1
   }
   if (spec$param$DEBUG) .v("\nPhasing: phc = (%3.6f, %3.6f)\n", spec$proc$phc0*180/pi, spec$proc$phc1*180/pi,
                                                                  logfile=spec$param$LOGFILE)

   fspec <- stats::fft(spec$fid)
   m <- length(fspec); p <- ceiling(m/2)
   fspec <- c( fspec[(p+1):m], fspec[1:p] )
   if ( spec$param$REVPPM ) fspec <- fspec[rev(1:m)]
   new_spec1r <- C_corr_spec_re(list(re=Re(fspec),im=Im(fspec), phc0=spec$proc$phc0, phc1=spec$proc$phc1))
   spec$data <- complex(real=new_spec1r$re,imaginary=new_spec1r$im)
   spec
}



#--------------------------------
# PPM calibration
#--------------------------------
.ppm_calibration <- function(spec)
{
   logfile <- spec$param$LOGFILE
   
   # PPM Calibration
   m <- spec$proc$SI
   SW <- spec$acq$SW

   # Calibration based on TSP
   if (spec$param$TSP) {
      x0 <- abs(spec$pmin)/SW
      n1 <- round(m*(x0-0.4/SW)); n2 <- round(m*(x0+0.2/SW))
      range <- c(n1:n2)
      if (max(spec$int[ range ])>10*C_estime_sd(spec$int,128)) {
          n0 <- which(spec$int[ range ] == max(spec$int[ range ])) + n1 - 2
          spec$pmin <- -SW*(n0/m)
          spec$pmax <- SW + spec$pmin
          spec$ppm <- seq(from=spec$pmin, to=spec$pmax, by=spec$dppm)
          if (spec$param$DEBUG) .v("PPM min =%f\n", spec$pmin ,logfile=logfile)
      }
   }

   spec
}



#--------------------------------
# Main routines
#--------------------------------


### FID Processing - Main routine
#--    DIR        : absolute path of the Bruker/Varian directory
#--    procparams : list of processing parameters, see the  default parameter list 'Spec1rFromFID.params'
###
.CALL <- function ( Input, param=Spec1rProcpar )
{

   spec <- NULL
   logfile <- param$LOGFILE

   fok <- TRUE
   if (!dir.exists(Input)) Input <- dirname(Input)
   if (!dir.exists(Input)) fok <- FALSE

   if(param$DEBUG) {
      if (!dir.exists(Input)) {
         .v("Path %s does not exist! \n",Input,logfile=logfile)
      } else {
         if(param$INPUT_SIGNAL == "fid") {
            .v("Path of the FID : %s\n",Input,logfile=logfile)
            .v("Read the FID ...",logfile=logfile)
         } else {
            .v("Path of the 1R : %s\n",Input,logfile=logfile)
            .v("Read the 1R ...",logfile=logfile)
         }
      }
   }

   ## Read FID or 1r and parameters
   repeat {
      if (!fok) break

      if (param$VENDOR == "nmrml") {
         spec <- .read.FID.nmrML(Input)
         break
      }
      if (param$VENDOR == "bruker" && param$INPUT_SIGNAL == "fid") {
         spec <- .read.FID.bruker(Input)
         break
      }
      if (param$VENDOR == "bruker" && param$INPUT_SIGNAL == "1r") {
         spec <- .read.1r.bruker(Input,param)
         break
      }
      if (param$VENDOR == "rs2d" && param$INPUT_SIGNAL == "fid") {
         spec <- .read.FID.rs2d(Input)
         break
      }
      if (param$VENDOR == "rs2d" && param$INPUT_SIGNAL == "1r") {
         spec <- .read.1r.rs2d(Input,param)
         break
      }
      if (param$VENDOR == "varian"){
         param$INPUT_SIGNAL <- "fid"
         spec <- .read.FID.varian(Input)
         break
      }
      if (param$VENDOR == "jeol"){
         param$INPUT_SIGNAL <- "fid"
         spec <- .read.FID.jeol(Input)
         break
      }
      if (param$VENDOR == "magritek" && param$INPUT_SIGNAL == "1r") {
         spec <- .read.1r.magritek(Input)
         break
      }
      break
   }

   repeat {
      if (!fok || param$READ_RAW_ONLY) break

      if(param$DEBUG) .v("OK\n",logfile=logfile)

      if ( param$INPUT_SIGNAL == "fid") {

          ## Pre-processing: group delay, zero filling, line broadening
          if(param$DEBUG) .v("Preprocessing ...\n",logfile=logfile)
          spec <- .preprocess(spec,param)
          if(param$DEBUG) .v("OK\n",logfile=logfile)

          ## Phasing
          if(param$OPTPHC0) {
               if(param$DEBUG) .v("Optimizing the zero order phase ...",logfile=logfile)
               spec <- .optimphase0(spec)
               if(param$DEBUG) .v("OK\n",logfile=logfile)
          }
          if(param$OPTPHC1) {
              if(param$DEBUG) .v("Optimizing both zero order and first order phases ...",logfile=logfile)
              spec <- .optimphase1(spec)
              if(param$DEBUG) .v("OK\n",logfile=logfile)
          }

          # Get new spectrum
          spec <- .computeSpec(spec)

          # Get real spectrum
          spec$int <- Re(spec$data)

          # Ajust Baseline if required
          if(param$AJUSTBL) {
              if (param$DEBUG) .v("Ajust Baseline ... ", logfile=logfile)
              spec$int <- ajustBL(spec$int,0)
              if(param$DEBUG) .v("OK\n",logfile=logfile)
          }

          # PPM calibration based on TSP
          if (param$TSP) {
              if (param$DEBUG) .v("PPM calibration based on TSP  ... ", logfile=logfile)
              spec <- .ppm_calibration(spec)
              if(param$DEBUG) .v("OK\n",logfile=logfile)
          }

          # Zeroing of Negative Values
          if (param$RABOT) {
              V <- stats::quantile( spec$int[ spec$int < 0 ], 0.25 )
              spec$int[ spec$int < V ] <- V
          }

          #cleanup the final object
          if (param$CLEANUP_OUTPUT) {
              spec$crit <- NULL
              spec$fid0 <- NULL
              spec$data0 <- NULL
              spec$data <- NULL
              spec$B <- NULL
          }
      }

      # Define spec list as a Spectrum object
      class(spec) = "Spectrum"
      # Ouput spec object

      break
   }

   utils::flush.console()
   spec

}

## .Finalize
# Get a list as output with the finalized spectra data ready to be plotted
# Inputs:
#   spec   : object obtained from the .CALL() function  (i.e. Spect1r.doProc() as external call)
#   ratio  : ratio between the max intensity of the plot, and the max intensity of the spectrum; default value = 1
#   ppm    : ppm window of the plot; default = all the spectrum
# Outputs:
#   x,y : spectra data (ppm, intensity)
#   xlim, ylim : limits (ranges) of ppm(xlim) and intensity (ylim)
#   tille : origin path of the spectra
Finalize <- function(spec, ppm = c(spec$ppm[1], spec$ppm[spec$proc$SI]), ratio=1, reverse=TRUE)
{
   if (spec$param$TSP) spec <- .ppm_calibration(spec)

   # Get real spectrum
   spec.int <- spec$int
   if (spec$param$RABOT) {
       V <- stats::quantile( spec.int[ spec.int < 0 ], 0.25 )
       spec.int[ spec.int < V ] <- V
       spec.int <- ajustBL(spec.int,0)
   }

   # Graphic
   i1<-which(spec$ppm>=ppm[1])[1]
   i2<-length(which(spec$ppm<=ppm[2]))
   ymax <- max( spec.int[i1:i2]/ratio )
   ymin <- min( spec.int[spec.int<0], 0.0 )
   #ymin <- ifelse(ymin>0.0, 0.0, max(1.1*ymin,-0.5*ymax))
   title <- paste( basename(dirname(spec$path)),basename(spec$path),sep="/")
   ppmlim <- ppm
   if(reverse==TRUE) ppmlim <- rev(ppm)
   list( x=spec$ppm[i1:i2], y=spec.int[i1:i2], xlim=ppmlim, ylim=c(ymin,ymax), title=title )
}

### Default print for the 'Spectrum' object
printSpectrum = function(obj)
{
   cat(" Dir. Path = ", obj$path, "\n")
   if (obj$acq$INSTRUMENT == 'bruker') {
      cat(" Spectrometer = ", obj$acq$ORIGIN, "\n")
      cat(" SOFTWARE     = ", obj$acq$SOFTWARE, "\n")
      cat(" ORIGPATH     = ", obj$acq$ORIGPATH, "\n")
   } else {
      cat(" Spectrometer = ", obj$acq$INSTRUMENT, "\n")
   }
   cat(" PROBE     = ", obj$acq$PROBE, "\n",
        "PULSE     = ", obj$acq$PULSE, "\n",
        "SOLVENT   = ", obj$acq$SOLVENT, "\n",
        "GRPDLY    = ", obj$acq$GRPDLY, "\n",
        "TD        = ", obj$acq$TD, "\n",
        "SI        = ", obj$proc$SI, "\n",
        "SW        = ", obj$acq$SW, "\n",
        "SWH       = ", obj$acq$SWH, "\n",
        "SFO1      = ", obj$acq$SFO1, "\n",
        "O1        = ", obj$acq$O1, "\n",
        "-- phc0   = ", obj$phc0, "\n",
        "-- phc1   = ", obj$phc1, "\n",
        "ppm_min   = ", obj$pmin, "\n",
        "ppm_max   = ", obj$pmax, "\n",
        "delta_ppm = ", obj$dppm, "\n")
   cat("\n")
}

### Plot the 'Spectrum' object
#   ratio  : ratio between the max intensity of the plot, and the max intensity of the spectrum; default value = 1
#   ppm    : ppm window of the plot; default = all the spectrum
#   active : 0 => Open a new graphic window; 1 => put in the active graphic window (replace the content)
plotSpectrum = function(obj, ppm = c(obj$ppm[1], obj$ppm[obj$proc$SI]), ratio=1, title="", 
                         reverse=TRUE, active=FALSE, col="blue", overlay=FALSE, ovlCol="green" )
{
   g <- Finalize(obj, ppm, ratio=ratio, reverse=reverse)
   if (nchar(title)==0) title <- g$title
   if (overlay) {
      graphics::lines( g$x, g$y, col=ovlCol )
   } else {
      if (active==FALSE) grDevices::dev.new()
      graphics::plot( cbind( g$x, g$y), type="l", xlim=g$xlim, ylim=g$ylim, col=col, xlab="ppm", ylab="intensities", main=title)
      graphics::abline(h=0,v=0, col="red")
   }

}

writeSpec = function(spec, outdir, mode="bin", name="1r")
{
   ENDIAN <- "little"
   SIZE <- 4
   if (nchar(outdir)==0) stop("Error: outdir missing. You need to specify the output directory\n")
   binfile <- paste(outdir,name,sep='/')

   if (spec$param$TSP) spec <- .ppm_calibration(spec)

   # Get real spectrum
   spec.int <- spec$int
   if (spec$param$RABOT) {
       V <- stats::quantile( spec.int[ spec.int < 0 ], 0.25 )
       spec.int[ spec.int < V ] <- V
       spec.int <- ajustBL(spec.int,0)
   }
   zz <- file(binfile, "wb")
   writeBin(as.integer(rev(spec.int)), zz, size=SIZE, endian=ENDIAN)
   close(zz)

   procfile <- paste(outdir,'procs',sep='/')
   proclist <- c(
       '##TITLE= Processing parameters, POSTPROC		Rnmr1D v1.3',
       '##ORIGIN= Bruker BioSpin GmbH',
       '@@ *** Processing parameters in the same format as generated by Bruker TopSpin software (procs) ***',
       '##@BYTORDP= 0',
       '##@DTYPP= 0',
       paste('##@LB= ', spec$proc$LB, sep=''),
       paste('##@GB= ', spec$proc$GB, sep=''),
       '##@MC2= 0',
       paste('##@OFFSET= ', spec$pmax, sep=''),
       paste('##@PHC0= ', spec$phc0, sep=''),
       paste('##@PHC1= ', spec$phc1*2*pi, sep=''),
       '##@PH_mod= 1',
       paste('##@SF= ', spec$acq$SFO1, sep=''),
       paste('##@SI= ', spec$proc$SI, sep=''),
       '##@SSB= 0',
       paste('##@SW_p= ', spec$acq$SWH, sep=''),
       paste('##@TDeff= ', spec$proc$SI, sep=''),
       '##@WDW= 1',
       paste('##@YMAX_p= ', max(spec$int), sep=''),
       paste('##@YMIN_p= ', min(spec$int), sep=''),
       '@@ *** Additionnal parameters ***',
       '##@USER= djacob',
       '##@EMAIL= djacob65@gmail.com',
       '##@BLC_mod= Automatic baseline recognition',
       paste('##@SOLVENT_mod= ', spec$proc$ZERO_SOLVENT_METH, sep=''),
       paste('##@GRPDELAY= ', spec$acq$GRPDLY, sep='')
   )
   utils::write.table(proclist, file=procfile, sep='', row.names=F, col.names=F, quote=F)
   system(paste("sed -i -e 's/^##@/##\\$/g' ", procfile, sep=''))
}

### Write a Matrix of Spectrum in a binary mode (PACK format)
#   specMat : the Matrix of Spectrum : 1 row <=> 1 spectrum, 1 column <=> a same value of ppm
#   ppm_min, ppm_max :  the ppm range of the spectra
#   filepack : the full path of binary file
#' @export writeSpecMatrix
writeSpecMatrix = function(specMat, ppm_min, ppm_max, filepack)
{
   C_write_pack(specMat, ppm_min, ppm_max, filepack)
}

### Read a Matrix of Spectrum in a binary mode (PACK format)
#   Input: filepack : the full path of binary file
#   Output: a list with :
#         int: the Matrix of Spectrum : 1 row <=> 1 spectrum, 1 column <=> a same value of ppm
#         nspec & size : respectively the number of spectra and their size points
#         ppm_min & ppm_max : respectively the minimum and the maximum of the PPM range
#' @export readSpecMatrix
readSpecMatrix = function(filepack)
{
   C_read_pack(filepack)
}


#' @export spec_ref
spec_ref= function (specMat, selected=NULL)
{
   if (is.null(selected)) {
       C_spec_ref(specMat, numeric(0))
   } else {
       C_spec_ref(specMat, selected)
   }
}

#' @export spec_ref_interval
spec_ref_interval= function (specMat, istart, iend, selected=NULL)
{
   if (is.null(selected)) {
       C_spec_ref_interval(specMat, istart, iend, numeric(0))
   } else {
       C_spec_ref_interval(specMat, istart, iend, selected)
   }
}

#' @export segment_shifts
segment_shifts = function (specMat, idx_vref, decal_max, istart, iend, selected=NULL)
{
   if (is.null(selected)) {
       C_segment_shifts (specMat, idx_vref, decal_max, istart, iend, numeric(0))
   } else {
       C_segment_shifts (specMat, idx_vref, decal_max, istart, iend, selected)
   }
}

#' @export align_segment
align_segment = function (specMat, shifts, istart, iend, apodize=0, selected=NULL)
{
   if (is.null(selected)) {
       C_align_segment (specMat, shifts, istart, iend, apodize, numeric(0))
   } else {
       C_align_segment (specMat, shifts, istart, iend, apodize, selected)
   }
}
