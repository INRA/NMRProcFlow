#------------------------------------------------
# Build 1r spectrum from FID file (Bruker)
# R script for testing only
# (C) 2014 - D. JACOB - IMRA UMR1332 BAP
#------------------------------------------------

#suppressMessages(library(MassSpecWavelet))
suppressMessages(library(Rcpp))

# Comment these lines below before compiling 
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])
sourceCpp(file.path(PATH, 'libCspec.cpp'))
source(file.path(PATH, 'Rnmr.R'))
