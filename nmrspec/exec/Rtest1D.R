library(Rnmr1D)
library(doParallel)

# get all the function names of the given package "Rnmr1D"
r <- unclass(lsf.str(envir = asNamespace("Rnmr1D"), all = T))

# filter weird names
r <-  gsub("<-", "", gsub("\\[", "", r))

# create functions in trhe Global Env. with the same name
for(name in r) eval(parse(text=paste0(name, '<-Rnmr1D:::', name)))


# source("/srv/shiny-server/exec/libspec.R")

source("exec/RnmrTools.R")

conffile <- "conf/global.ini"
conf <- Parse.INI(conffile, section="GLOBAL")
conf <- Parse.INI(conffile, INI.list=conf, section="NMRSPEC")

# the output data directory
DATADIR <- "/opt/data/_cfa5e969dd0d2df7f79579815d3864"

outDataViewer <- DATADIR

#  argument: the INI parameter file
INI.filename <- paste0(DATADIR,'/',conf$Rnmr1D_INI)

procParams <- Parse.INI(INI.filename, INI.list=list(), section="PROCPARAMS")
env.list <- Parse.INI(INI.filename, INI.list=list(), section="ENV")

#obj <- readRDS(file.path(DATADIR,'speclist.rds'))

# specParams
specParamFile <- file.path(DATADIR,"list_pars.csv")
specParamsDF <- read.table(specParamFile, header=T, sep=";", stringsAsFactors=FALSE)
SI <- (as.list(specParamsDF[1,]))$SI

# Read the specs.pack
specPackFile <- file.path(DATADIR,conf$SPEC_PACKED)
specMat <-  Rnmr1D:::readSpecMatrix(specPackFile)
specMat$dppm <- (specMat$ppm_max - specMat$ppm_min)/(specMat$size - 1)
specMat$ppm <- rev(seq(from=specMat$ppm_min, to=specMat$ppm_max, by=specMat$dppm))

# Read the samples List file
samples <- read.table( file.path(DATADIR,'/samples.csv'), header=F, sep=";", stringsAsFactors=FALSE)
factors <- read.table( file.path(DATADIR,'/factors'), header=F, sep=";", stringsAsFactors=FALSE)

# Rnmr1D macrocommand file
CMD.filename <- file.path(DATADIR,conf$Rnmr1D_PCMD)



#--- Here starst the specific test ---
source("R/utils.R")

zoneref <- NA
zonenoise <- c(10.2, 10.5)
normmeth <- 'PQN'
snrlevel <- 3

input <- list( normmeth=normmeth, snrlevel=snrlevel)

#library(openxlsx)
#OUTFILE <- file.path(DATADIR,"wb.xlsx")
#wb <- createWorkbook()
#write_qhnmr_wb(wb, outDataViewer, zoneref, zonenoise)
#saveWorkbook(wb, OUTFILE, overwrite = TRUE)

bucketfile <- file.path(outDataViewer,conf$BUCKET_LIST)
outDataMat <- NULL

file.exists(bucketfile)

        outsnr <-  get_SNR_dataset(specMat, bucketfile, c(min(zonenoise), max(zonenoise)), ratio=TRUE)

        buckets <- read.table(bucketfile, header=F, sep="\t",stringsAsFactors=FALSE)
        BUCsel <- get_Buckets_upperSNR(buckets, outsnr, input$snrlevel)

        outdata <- get_Buckets_dataset(specMat, bucketfile, input$normmeth, zoneref)

        nbfc <- length(colnames(outdata)) - length(buckets[,1])
        if (specMat$nspec>1 && BUCsel>1) {
           outDataMat <- cbind( outdata[, 1:nbfc ],  outdata[, BUCsel[ grep("\\.2$", BUCsel, invert=TRUE) ] ] )
        } else {
           outDataMat <- cbind( t(outdata[, 1:nbfc ]),  t(outdata[ , colnames(outdata) %in% BUCsel])  )
        }




