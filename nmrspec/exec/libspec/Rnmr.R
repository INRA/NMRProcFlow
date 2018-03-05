#------------------------------------------------
# Rnmr1D package: Build 1r spectrum from FID file (Bruker/Varian/nmrML)
# Project: NMRProcFlow - MetaboHUB
# (C) 2015 - D. JACOB - IMRA UMR1332 BAP
#------------------------------------------------

require(XML)
require(base64enc)

Spec1r.doProc <- function(Input, ...) UseMethod("Spec1r.doProc")

Spec1r.doProc.default <- function(Input, ...)
{
   .CALL(Input,...)
}

### Processing parameters
Spec1r.Procpar.default <- list (

### General Parameters
    DEBUG=TRUE,                # Debug 
    LOGFILE=stderr(),          # Messages output file
    VENDOR="bruker",           # Instrumental origin of the raw data, (bruker, varian)
    READ_RAW_ONLY=FALSE,       # Read Raw file only; do not carry out processing; if raw file is depending on INPUT_SIGNAL
    INPUT_SIGNAL="fid",        # What type of input signal: 'fid' or '1r'
    PDATA_DIR='pdata/1',       # subdirectory containing the 1r file (bruker's format only)

### PRE-PROCESSING
    LB= 0.3,                   # Exponantial Line Broadening parameter
    GB= 0,                     # Gaussian Line Broadening parameter
    REVTIME=FALSE,             # Reverse time points
    BLPHC=FALSE,               # Intensity offset correction
    ZEROFILLING=FALSE,         # Zero Filling
    ZFFAC=4,                   # Max factor for Zero Filling 
    LINEBROADENING=TRUE,       # Line Broading
    TSP=FALSE,                 # PPM referencing
    TSPSNR=100,                # Minimal S/N Ratio for TSP Peak
    RABOT=FALSE,               # Zeroing of Negative Values 
    OPTPHC0=TRUE,              # Zero order phase optimization
    OPTPHC1=FALSE,             # First order phase optimization
    ALGO2=FALSE,               # alternative algorithm for phase optimization
    FRACPPM= 0,                # Origin point for adjustment of the first order phase (defined as a fraction of the ppm scale)
    CFRACPPM= 0.25,            # Origin point for adjustment of the first order phase (defined as a fraction of the ppm scale)
    INCFRACPPM1=0.125,         # Incrementation value for search of optimal point for adjustment of the first order phase : step 1
    INCFRACPPM2=0.0125,        # Incrementation value for search of optimal point for adjustment of the first order phase : step 2
    JGD_INNER=TRUE,            # JEOL : internal or external estimation for Group Delay
    RMS=0
)

#--------------------------------
# Initialize Parameter Lists by the default ones
#--------------------------------
Spec1r.Procpar    <- Spec1r.Procpar.default

#--------------------------------
# verbose function
#--------------------------------
.v <- function(..., logfile=Spec1r.Procpar$LOGFILE) cat(sprintf(...), sep='', file=logfile, append=TRUE)

#--------------------------------
# READ FID && Acquisition Parameters
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
       acqval <-simplify2array(strsplit(ACQ[which(simplify2array(regexpr(regexpStr,ACQ))>0)+1]," "))[2]
   }
   if (type=="numeric")
       acqval <-as.numeric(acqval)
   if (type=="string")
       acqval <-gsub("<","",gsub(">","",acqval))
   acqval
}

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

### Estime Group Delay
#-- fid : free induction decay - Complex data type
.estime_grpdelay <- function( fid )
{
   P <- sqrt(  Re(fid)^2 + Im(fid)^2 )
   V <- cbind(Re(fid), Im(fid))
   G <- 0
   for( i in 1:(dim(V)[2]) ) {
      nd0 <- which(P>max(P)/2)[1];
      nd0p <- which.max(abs(V[1:(nd0-1),i]))
      if ((abs(V[nd0p,i])/abs(V[nd0,i]))>0.5) nd0 <- nd0p;
      while(sign(V[nd0-1,i])==sign(V[nd0,i])) nd0 <- nd0-1;
      G <- G + 0.9999*(nd0-1 - V[nd0-1,i]/(V[nd0,i]-V[nd0-1,i]))
   }
   G <- G/dim(V)[2]
   G
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
       stop("Invalide data type : File ", FIDFILE, " does not exist\n")
   # Acquisition parameters filename
   ACQFILE <- "acqus"
   if (!file.exists(ACQFILE)) 
       stop("Invalide data type : Acquisition parameter File (", ACQFILE, ") does not exist\n")

   ACQ     <- readLines(ACQFILE)
   PROBE   <- .bruker.get_param(ACQ,"PROBHD",type="string")
   SOLVENT <- .bruker.get_param(ACQ,"SOLVENT",type="string")
   PULSE   <- .bruker.get_param(ACQ,"PULPROG",type="string")
   TEMP    <- .bruker.get_param(ACQ,"TE",type="string")
   RELAXDELAY  <- .bruker.get_param(ACQ,"D",type="string",  arrayType=TRUE)
   PULSEWIDTH <- .bruker.get_param(ACQ,"P",type="string",  arrayType=TRUE)
   SPINNINGRATE <- .bruker.get_param(ACQ,"MASR",type="string")
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

   SIZE = ifelse( DTYPA==0, 4, 8)

   to.read = file(FIDFILE,"rb")
   signal<-readBin(to.read, what="int", n=TD, size=4L, endian = ENDIAN)

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

   acq <- list( INSTRUMENT=INSTRUMENT, SOFTWARE=SOFTWARE, ORIGIN=ORIGIN, ORIGPATH=ORIGPATH, 
                PROBE=PROBE, PULSE=PULSE, SOLVENT=SOLVENT, TEMP=TEMP, NUC=NUC, 
                NUMBEROFSCANS=NUMBEROFSCANS, DUMMYSCANS=DUMMYSCANS,
                RELAXDELAY=RELAXDELAY, SPINNINGRATE=SPINNINGRATE, PULSEWIDTH=PULSEWIDTH,
                TD=TD, SW=SW, SWH=SWH, SFO1=SFO1, O1=O1, GRPDLY=GRPDLY )
   spec <- list( path=DIR, acq=acq, fid=fid )

   spec
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
       stop("Invalide data type : File ", FIDFILE, " does not exist\n")
   # Acquisition parameters filename
   ACQFILE <- "procpar"
   if (!file.exists(ACQFILE)) 
       stop("Invalide data type : Acquisition parameter File (", ACQFILE, ") does not exist\n")

   ACQ  <- readLines(ACQFILE)
   SWH  <-  .varian.get_param(ACQ,"sw")
   SFO1 <- .varian.get_param(ACQ,"sfrq")
   O1   <- .varian.get_param(ACQ,"tof")
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
                NUMBEROFSCANS=NUMBEROFSCANS, DUMMYSCANS=DUMMYSCANS,
                TD=TD, SW=SW, SWH=SWH, SFO1=SFO1, O1=O1, GRPDLY=GRPDLY )
   spec <- list( path=DIR, acq=acq, fid=fid )

   spec

}

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
           #cat("Unknkown Value_Type\n")
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
   
       #cat(Name,"=",trim(Value),v_unit,"\n")
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

   if( procpar$temp_set$Unit=="dC") acq$TEMP <- acq$TEMP + 273.15

   acq$TD <- length(fid)

   # Group Delay : Internal or External
   if (Spec1r.Procpar$JGD_INNER) {
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
                NUMBEROFSCANS='-', DUMMYSCANS='-', TD=TD, SW=SW, SWH=SWH, SFO1=SFO1, O1=0, GRPDLY=GRPDLY )
   spec <- list( path=filename, acq=acq, fid=fid )

   spec
}

#### Read 1r data and the main parameters needed to generate the real spectrum
#--  internal routine
#-- DIR: bruker directory containing the 1r
.read.1r.bruker <- function(DIR, param=Spec1r.Procpar) {

   cur_dir <- getwd()
   setwd(DIR)
   
   # Acquisition parameters filename
   ACQFILE <- "acqus"
   if (!file.exists(ACQFILE)) 
       stop("Invalide data type : Acquisition parameter File (", ACQFILE, ") does not exist\n")

   # 1r filename
   SPECFILE <- paste(param$PDATA_DIR,"/1r",sep="")
   if (!file.exists(SPECFILE))
       stop("Invalide data type : File (", SPECFILE, ") does not exist\n")

   # Processing parameters filename
   PROCFILE <- paste(param$PDATA_DIR,"/procs",sep="")
   if (!file.exists(PROCFILE)) 
       stop("Invalide data type : Acquisition parameter File (", PROCFILE, ") does not exist\n")

   # Read Acquisition parameters
   ACQ     <- readLines(ACQFILE)
   PROBE   <- .bruker.get_param(ACQ,"PROBHD",type="string")
   SOLVENT <- .bruker.get_param(ACQ,"SOLVENT",type="string")
   NUC     <- .bruker.get_param(ACQ,"NUC1",type="string")
   PULSE   <- .bruker.get_param(ACQ,"PULPROG",type="string")
   TEMP    <- .bruker.get_param(ACQ,"TE",type="string")
   RELAXDELAY  <- .bruker.get_param(ACQ,"D",type="string",  arrayType=TRUE)
   PULSEWIDTH <- .bruker.get_param(ACQ,"P",type="string",  arrayType=TRUE)
   SPINNINGRATE <- .bruker.get_param(ACQ,"MASR",type="string")
   TD      <- .bruker.get_param(ACQ,"TD")
   SW      <- .bruker.get_param(ACQ,"SW")
   SWH     <- .bruker.get_param(ACQ,"SW_h")
   SFO1    <- .bruker.get_param(ACQ,"SFO1")
   O1      <- .bruker.get_param(ACQ,"O1")
   GRPDLY  <- .bruker.get_param(ACQ,"GRPDLY")
   DTYPA   <- .bruker.get_param(ACQ,"DTYPA")
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
   NC_proc <-  .bruker.get_param(PROC,"NC_proc")
   OFFSET <- .bruker.get_param(PROC,"OFFSET")
   SI <- .bruker.get_param(PROC,"SI")
   PHC0 <-  .bruker.get_param(PROC,"PHC0")
   PHC1 <-  .bruker.get_param(PROC,"PHC1")

   # Read the 1r spectrum
   ENDIAN = ifelse( BYTORDP==0, "little", "big")
   SIZE = ifelse( DTYPA==0, 4, 8)
   to.read = file(SPECFILE,"rb")
   signal<-rev(readBin(to.read, what="int",size=SIZE, n=SI, signed = TRUE, endian = ENDIAN))
   signal <- (2^NC_proc)*signal
   close(to.read)
   TD <- length(signal)

   setwd(cur_dir)

   dppm <- SW/(TD-1)
   pmax <- OFFSET
   pmin <- OFFSET - SW
   ppm <- seq(from=pmin, to=pmax, by=dppm)

   acq <- list( INSTRUMENT=INSTRUMENT, SOFTWARE=SOFTWARE, ORIGIN=ORIGIN, ORIGPATH=ORIGPATH, 
                PROBE=PROBE, PULSE=PULSE, NUC=NUC, SOLVENT=SOLVENT, TEMP=TEMP, 
                RELAXDELAY=RELAXDELAY, SPINNINGRATE=SPINNINGRATE, PULSEWIDTH=PULSEWIDTH,
                TD=TD, SW=SW, SWH=SWH, SFO1=SFO1, O1=O1, GRPDLY=GRPDLY )

   proc <- list( phc0=PHC0, phc1=PHC1, SI=SI )

   param$ZEROFILLING <- FALSE
   param$LINEBROADENING <- FALSE

   spec <- list( path=DIR, param=param, acq=acq, proc=proc, fid=NULL, int=signal, dppm=dppm, pmin=pmin, pmax=pmax, ppm=ppm )

   spec
}

### Group Delay correction
.groupDelay_correction <- function(spec, param=Spec1r.Procpar)
{
    fid <- spec$fid
    if (spec$acq$GRPDLY>0) {
       if (! is.null(param$GRDFLG) && param$GRDFLG) spec$acq$GRPDLY <- .estime_grpdelay(fid)
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
       Spectrum <- fft(fid)
       Spectrum <- c( Spectrum[seq1], Spectrum[seq2] )
       Spectrum <- Spectrum * exp(i*spec$acq$GRPDLY*2*pi*Omega)
       p <- length(seq1); seq1 <- ( (p+1):m ); seq2 <- ( 1:p )
       Spectrum <- c( Spectrum[seq1], Spectrum[seq2] )
       fid <- fft(Spectrum, inverse = TRUE)
    }
    fid
}

#--------------------------------
# Pre-Processing
#--------------------------------

#### Apply some preprocessing: zero_filling, line broading
#--  Generate real and imaginary parts
.preprocess <- function(spec, param=Spec1r.Procpar)
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
    spec$fid0 <- .groupDelay_correction(spec, param)

    ### Zero filling
    if (param$ZEROFILLING) {
       TDMAX <- min(param$ZFFAC*td,131072)
       if(param$DEBUG) .v("\tZero Filling (x%d)\n", round(TDMAX/td), logfile=logfile)
       while ((td/TDMAX) < 1 ) {
          td=length(spec$fid)
          spec$fid <- c( spec$fid, rep(complex(real=0, imaginary=0), td) )
          td=length(spec$fid)
       }
       td=length(spec$fid)
    }
    if (param$DEBUG) .v("\tSI = %d\n", td, logfile=logfile)

    ### Group Delay correction if needed
    if (spec$acq$GRPDLY>0) {
       if(param$DEBUG) .v("\tApplied GRPDLY ...", logfile=logfile)
       spec$fid <- .groupDelay_correction(spec, param)
       if(param$DEBUG) .v("OK\n", logfile=logfile)
    }

    ### FFT of FID
    if(param$DEBUG) .v("\tFFT ...", logfile=logfile)
    Spectrum <- fft(spec$fid)
    if(param$DEBUG) .v("OK\n", logfile=logfile)

    ### Rotation
    p <- td/2
    spec.p <- c( Spectrum[(p+1):td], Spectrum[1:p] )
    param$REVTIME <- param$REVTIME || ( length(grep("VARIAN",toupper(spec$acq$INSTRUMENT)))>0 || length(grep("JEOL",toupper(spec$acq$INSTRUMENT)))>0)
    if ( param$REVTIME ) {
       rawspec <- spec.p[rev(1:td)]
    } else {
       rawspec <- spec.p
    }
    param$SI <- length(rawspec)

    proc <- list( phc0=0, phc1=0, RMS=0, SI=length(rawspec) )

    ### Save into the spec object instance
    spec$acq$TD <- length(rawspec)
    spec$data <- rawspec
    spec$param <- param
    spec$proc <- proc

    ### return the spec object instance
    spec
}

.computeSSneg <- function(Yre) {
#  Ajust baseline
   bl <- C_Estime_LB2 (Yre, 1, length(Yre)-1, 100, 300, 3*C_estime_sd(Yre,128))
   Yre <- Yre - bl
   n <- round(length(Yre)/24)
   Yre <- Yre[n:(23*n)]
   ret <- sum((Yre[Yre<0])^2)
   return(ret)
}

.optimphase0 <- function(spec)
{
   # rms function to be optimised
   rms0 <- function(ang, y)  {
      Yrot <- C_corr_spec_re(list(re=Re(y),im=Im(y), phc0=ang, phc1=0, alpha=0))
      ret <- .computeSSneg(Yrot$re)
      return(ret)
   }

   if (spec$param$ALGO2==FALSE && spec$acq$NUC %in% c('1H','H1')) {
      V <- spec$data
      V[ round(spec$proc$SI*(0.5-0.3/spec$acq$SW)):round(spec$proc$SI*(0.5+0.3/spec$acq$SW)) ] <- 0+0i
      lparams <- list(re=Re(V),im=Im(V), phc0=0, phc1=0, alpha=0, p=0.95, blphc=spec$param$BLPHC)
      best1 <- C_optim_phc(-pi, pi, lparams, 0, 1e-7)
      best2 <- C_optim_phc(0, 2*pi, lparams, 0, 1e-7)
      if ( best1[["objective"]] < best2[["objective"]] ) {
         best <- best1
      } else { 
         best <- best2
      }
      lparams <- list(re=Re(V),im=Im(V), phc0=0, phc1=0, alpha=0, p=0.95, blphc=1)
      best1 <- C_optim_phc(best[["minimum"]]-0.5, best[["minimum"]]+0.5, lparams, 0, 1e-7)
      if ( best1[["objective"]] < best[["objective"]] ) {
         best <- best1
      }
   } else {
      interval <- c(-3*pi, 3*pi)
      best <- stats::optimize(rms0, interval = interval, maximum = FALSE, y = spec$data)
   }
   spec$proc$phc0 <- best[["minimum"]]
   spec$proc$RMS <- best[["objective"]]
   spec$data <- spec$data * exp(complex(real = 0, imaginary = spec$proc$phc0))

   spec
}

.optimphase1 <- function(spec)
{
   spec1r_cal <- function(flg=0,midzero=0) {
      if (flg==0) { 
          fid <- spec$fid0
      } else {
          fid <- spec$fid
      }
      m <- length(fid)
      Spectrum <- fft(fid)
      p <- ceiling(m/2); seq1 <- ( (p+1):m ); seq2 <- ( 1:p )
      V <- c( Spectrum[seq1], Spectrum[seq2] )
      if (midzero==1) {
          N <- length(fid); SW <- spec$acq$SW
          V[ round(N*(0.5-0.3/SW)):round(N*(0.5+0.3/SW)) ] <- 0+0i
      }
      return(V)
   }

   # rms function to be optimised
   rms1 <- function(ang, spec1r, fracppm)  {
      Yrot <- C_corr_spec_re(list(re=Re(spec1r),im=Im(spec1r), phc0=spec$proc$phc0, phc1=ang, alpha=fracppm))
      ret <- .computeSSneg(Yrot$re)
      return(ret)
   }

   OptimAlpha <- function(amin, amax, ainc) {
      opt <- NULL
      blphc <-  ifelse( spec$param$BLPHC, 1, 0)
      for(fp in seq(from=amin, to=amax, by=ainc)) {
         if (spec$param$ALGO2==FALSE && spec$acq$NUC %in% c('1H','H1')) {
             lparam <- list(re=Re(spec1r),im=Im(spec1r), phc0=spec$proc$phc0, phc1=0, alpha=fp, p=0.95, blphc=blphc)
             best <- C_optim_phc(-0.6*pi, 0.6*pi, lparam, 1, 0.0001)
             opt_tmp <- list(phc1=best[["minimum"]], rms=best[["objective"]], fracppm=fp)
             if (is.null(opt) || opt_tmp$rms<opt$rms) opt <- opt_tmp
         } else {
             best <- stats::optimize(rms1, interval = c(-2*pi, 2*pi), maximum = FALSE, spec1r = spec1r, fracppm=fp)
             opt_tmp <- list(phc1=best[["minimum"]], rms=best[["objective"]], fracppm=fp)
         }
         if (is.null(opt) || opt_tmp$rms<opt$rms) opt <- opt_tmp
      }
      opt
   }

   # Compute criterim for phc1=0
   spec1r <- spec1r_cal(flg=1,midzero=1)
   blphc <-  ifelse( spec$param$BLPHC, 1, 0)
   best0 <- C_optim_phc(0, 0, list(re=Re(spec1r),im=Im(spec1r), phc0=spec$proc$phc0, phc1=0, alpha=0, p=0.95, blphc=blphc), -1, 0)

   # Optimisation of the first order phase
   if (spec$param$FRACPPM>0) {
      spec1r <- spec1r_cal(flg=1,midzero=1)
      opt <- OptimAlpha(spec$param$FRACPPM, spec$param$FRACPPM, 0)
   } else {
      # Step 1: Fast exploration of full space for alpha
      spec1r <- spec1r_cal(flg=0,midzero=1)
      opt <- OptimAlpha(0.125, 0.875, spec$param$INCFRACPPM1)
      # Step 2: Exploration of the optimial local space for alpha
      opt <- OptimAlpha(opt$fracppm-0.0625, opt$fracppm+0.0625, spec$param$INCFRACPPM1)
   }
   # if criterim is better for phc1=0
   if (opt$rms>best0[["objective"]]) {
      opt$rms <- best0[["objective"]]
      opt$phc1 <- 0
   }

   spec$proc$phc1 <- opt$phc1
   spec$proc$RMS <- opt$rms
   spec$proc$CFRACPPM <- opt$fracppm
   spec1r <- spec1r_cal(flg=1,midzero=0)
   new_spec1r <- C_corr_spec_re(list(re=Re(spec1r),im=Im(spec1r), phc0=spec$proc$phc0, phc1=spec$proc$phc1, alpha=opt$fracppm))
   spec$data <- complex(real=new_spec1r$re,imaginary=new_spec1r$im)
   if (spec$param$REVTIME) spec$data <- spec$data[rev(1:length(spec$data))]

   spec
}

.ppm_calibration <- function(spec)
{
   logfile <- spec$param$LOGFILE
   
   # Get a default calibration
   m <- spec$proc$SI
   SW <- spec$acq$SW
   SWH <- spec$acq$SWH
   O1 <- spec$acq$O1
   spec$dppm <- SW/(m-1)
   spec$pmin <- 0

   repeat {
      if (spec$acq$INSTRUMENT=="JEOL") {
         spec$pmin <- spec$acq$OFFSET-SW/2
         break
      }
      if (spec$acq$NUC %in% c('1H','H1') ) {
         if (spec$acq$INSTRUMENT=="VARIAN") {
            ARRAY_SWREF   <- c(  10,   11, 12, 14,   15, 20)
            ARRAY_PMINREF <- c(-0.4, -0.5, -1, -2, -2.6, -4)
            n <- trunc(SW+0.1)
            if (sum(ARRAY_SWREF==trunc(SW))==0) n <- n + 1
            if (sum(ARRAY_SWREF==trunc(SW))==0) n <- n -2
            if (sum(ARRAY_SWREF==trunc(SW))==0) {
                spec$pmin <- 1 - round(SW/5) # 5 - 0.5*SW
            } else {
                spec$pmin <- ARRAY_PMINREF[which(ARRAY_SWREF==n)]
            }
            break
      
         }
         spec$pmin <- (O1/SWH-0.5)*SW
         break
      }
      if (spec$acq$NUC == '13C') {
         spec$pmin <- (O1/SWH-0.5)*SW
         #spec$pmin <- -20
         break
      }
      break
   }

   # Calibration based on TSP
   if (spec$param$TSP) {
      vmin <- spec$param$TSPSNR*C_estime_sd(spec$int,128)
      n0 <- findTSPPeak(spec$int, vmin, round(0.3/spec$dppm,0))
      if (n0>0) {
         spec$pmin <- -SW*(n0/m)
         if (spec$param$DEBUG) .v("PPM min =%f\n", spec$pmin ,logfile=logfile)
      } else {
         if (spec$param$DEBUG) .v("Internal Ref. not found\n" ,logfile=logfile)
      }
   }

   spec$pmax <- SW + spec$pmin
   spec$ppm <- seq(from=spec$pmin, to=spec$pmax, by=spec$dppm)

   spec
}

### FID Processing - Main routine
#--    DIR        : absolute path of the Bruker/Varian directory
#--    procparams : list of processing parameters, see the  default parameter list 'Spec1rFromFID.params'
###
.CALL <- function ( Input, param=Spec1r.Procpar )
{

   logfile <- param$LOGFILE
   if(param$DEBUG)
       if(param$INPUT_SIGNAL == "fid") {
         .v("Read the FID ...",logfile=logfile)
       } else {
          .v("Read the 1R ...",logfile=logfile)
       }

   ## Read FID or 1r and parameters
   repeat {
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
      break
   }

   repeat {
      if (param$READ_RAW_ONLY) break

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
              if(param$DEBUG) .v("Optimizing the first order phase ...",logfile=logfile)
              spec <- .optimphase1(spec)
              if(param$DEBUG) .v("OK\n",logfile=logfile)
          }

          # Get real spectrum
          spec$int <- ajustBL(Re(spec$data),0)

          # ppm calibration
          if (param$DEBUG) .v("Initialization of the PPM scale ... ", logfile=logfile)
          spec <- .ppm_calibration(spec)
          if(param$DEBUG) .v("OK\n",logfile=logfile)

          # Zeroing of Negative Values
          if (param$RABOT) {
              V <- quantile( spec$int[ spec$int < 0 ], 0.25 )
              spec$int[ spec$int < V ] <- V
          }
      }

      # Define spec list as a Spectrum object
      class(spec) = "Spectrum"
      # Ouput spec object

      break
   }

   flush.console()
   spec

}


### Write a Matrix of Spectrum in a binary mode (PACK format)
#   specMat : the Matrix of Spectrum : 1 row <=> 1 spectrum, 1 column <=> a same value of ppm
#   ppm_min, ppm_max :  the ppm range of the spectra
#   filepack : the full path of binary file
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
readSpecMatrix = function(filepack)
{
   C_read_pack(filepack)
}


spec_ref= function (specMat, selected=NULL)
{
   if (is.null(selected)) {
       C_spec_ref(specMat, numeric(0))
   } else {
       C_spec_ref(specMat, selected)
   }
}

spec_ref_interval= function (specMat, istart, iend, selected=NULL)
{
   if (is.null(selected)) {
       C_spec_ref_interval(specMat, istart, iend, numeric(0))
   } else {
       C_spec_ref_interval(specMat, istart, iend, selected)
   }
}

segment_shifts = function (specMat, idx_vref, decal_max, istart, iend, selected=NULL)
{
   if (is.null(selected)) {
       C_segment_shifts (specMat, idx_vref, decal_max, istart, iend, numeric(0))
   } else {
       C_segment_shifts (specMat, idx_vref, decal_max, istart, iend, selected)
   }
}

align_segment = function (specMat, shifts, istart, iend, selected=NULL)
{
   if (is.null(selected)) {
       C_align_segment (specMat, shifts, istart, iend, numeric(0))
   } else {
       C_align_segment (specMat, shifts, istart, iend, selected)
   }
}
