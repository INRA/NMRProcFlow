suppressMessages(library(Matrix))
suppressMessages(library(MASS))
suppressMessages(library(signal))
suppressMessages(library(ptw))
suppressMessages(library(speaq))
suppressMessages(library(rjson))

options(show.error.locations = TRUE)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
.N <- function(x) { as.numeric(as.vector(x)) }
.C <- function(x) { as.vector(x) }

# Init counter
init_counter <- function(ProgressFile, n) {
    fh<-file(ProgressFile,"wt"); writeLines(as.character(n), fh); close(fh)
    dirout <- file.path(dirname(ProgressFile),".out")
    if( file.exists(dirout) )  unlink(dirout, recursive=TRUE)
    dir.create(dirout)
}

# Put i to 1
inc_counter <- function(ProgressFile, i) {
   fileout <- file.path(dirname(ProgressFile),".out",paste0(i,".out"))
   if (! file.exists(fileout)) {
       fl<-file(fileout,"wt"); writeLines("1", fl); close(fl)
   }
}

# Read the counter
get_counter <- function(ProgressFile) {
   n <- as.numeric(readLines(ProgressFile))
   c <- length(list.files(path=file.path(dirname(ProgressFile),".out"), pattern="*.out", all.files=FALSE, full.names=FALSE))
   return( list(value=c, size=n))
}

### Write within the 'INI.file' file  with the INI format, the 'metalist' list, a list  of lists 
#   EXCLU being a list of keys to exclude to the writting
Write.INI <- function(INI.file, metalist, EXCLU=c())
{
  INI.list <- c()
  for (i in 1:length(metalist)) {
      section <- names(metalist)[i]
      INI.list <- c( INI.list, paste0('[',section,']') )
      M<-unlist(eval(parse(text=paste0('metalist$',section))))
      for ( k in 1:length(M) ) {
          if ( names(M[k]) %in% EXCLU ) next
          INI.list <- c( INI.list, paste0(names(M[k]),'=',M[k],sep="") )
      }
      INI.list <- c( INI.list, '' )
   }
   write.table(INI.list, file=INI.file, sep='', row.names=F, col.names=F, quote=F)
}

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

### Write within the 'LOG.file' file, the 'textline' text
#   mode : can be either 'at' for 'appending' mode or 'wt' for 'writing' mode
Write.LOG <- function(LOG.file, textline="", mode="at")
{
  fileLog<-file(LOG.file,mode)
  writeLines(textline, fileLog)
  close(fileLog)
}

### Management of the historic of processings, (i.e. the sucessive versions of the matrix of the binary spectra, i.e. the specs.pack file)
#   push : Save the input (add) on the top of the stack (i.e. the current matrix of the binary spectra, the logfile)
#   pop  : Remove the last input from the stack (i.e. the last saved matrix of the binary spectra on the top of the list of files)  and replace by the previous one

get_maxSTACKID <- function(DATADIR, FILENAME)
{
  lstFiles <- list.files(path=DATADIR, full.names=FALSE, recursive = FALSE)
  if (length( grep(paste0(FILENAME,'.[0-9]'), lstFiles) )==0) return(0)
  return(max(as.numeric(gsub(paste0(FILENAME,'.'), '', lstFiles[grep(paste0(FILENAME,'.[0-9]'), lstFiles)]))))
}

push_STACK <- function (DATADIR, listfiles, STACKID)
{
   for (f in 1:length(listfiles)) {
      F0 <- file.path(DATADIR,listfiles[f])
      if (! file.exists(F0)) next
      FN <- file.path(DATADIR,paste0(listfiles[f],'.',sprintf("%03d",STACKID+1)))
      file.copy(F0, FN, overwrite = TRUE)
   }
}

pop_STACK <- function (DATADIR, listfiles, STACKID)
{
   for (f in 1:length(listfiles)) {
      FN <- file.path(DATADIR,paste0(listfiles[f],'.',sprintf("%03d",STACKID)))
      F0 <- file.path(DATADIR,listfiles[f])
      if (! file.exists(FN)) next
      file.copy(FN, F0, overwrite = TRUE)
      unlink(FN)
   }
}

# -----
# Generate the 'samples.csv' & 'factors' files from the list of raw spectra
# -----

# RAWDIR : the directory containing the uploaded files (unzipped ZIP and samples files)
#   samples.txt : the uploaded Sample file
# DATADIR : where data will be stored and used by NMRViewer
#   samples.csv : the Sample file used by NMRViewer
#   factors : file given the list of the severals factors used by NMRViewer
generate_Metadata_File <- function(RawZip, DATADIR, procParams)
{
   # Unzip the archive
   RAWDIR <- dirname(RawZip)
   ext <- tolower(gsub("^.*\\.", "", RawZip))
   if (ext=='7z') {
       system(paste0("cd ",RAWDIR,"; 7zr x -y ",RawZip))
   } else {
       unzip(RawZip, files = NULL, list = FALSE, overwrite = TRUE,  junkpaths = FALSE, exdir = RAWDIR, unzip = "internal",   setTimes = FALSE)
   }

   if (procParams$VENDOR=="varian") return( set_Metadata_File_Varian(RAWDIR, DATADIR, procParams) )
   if (procParams$VENDOR=="nmrml")  return( set_Metadata_File_nmrML(RAWDIR, DATADIR, procParams) )
   if (procParams$VENDOR=="jeol")   return( set_Metadata_File_jeol(RAWDIR, DATADIR, procParams) )

   # if a file  of samples was uploaded along with the ZIP file
   SampleFile <- file.path(RAWDIR,'samples.txt')
   if (file.exists(SampleFile)) {
        samples <- read.table(SampleFile, sep="\t", header=T,stringsAsFactors=FALSE)
        if (dim(samples)[2]>=4) {
             return(set_Metadata_File_Bruker(RAWDIR, DATADIR, procParams))
        }
        return(0)
   }
   if (procParams$INPUT_SIGNAL=='fid') return( generate_Metadata_File_fid(RAWDIR, DATADIR, procParams) )
   if (procParams$INPUT_SIGNAL=='1r')  return( generate_Metadata_File_1r(RAWDIR, DATADIR, procParams) )
   return(0)
}

generate_Metadata_File_fid <- function(RAWDIR, DATADIR, procParams)
{
   ERRORLIST <- c()
   OKRAW <- 1
   lstfac <- c("1;Samplecode")
   RAWPATH <- gsub("//", "/", RAWDIR)
   LIST <- gsub("//", "/", list.files(path = RAWPATH, pattern = "fid$", all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE))
   if ( class(LIST)=="character" && length(LIST)==0 ) return(0)
   L <- simplify2array(strsplit(LIST,'/'))
   if (class(L) != "matrix") {
      L <- simplify2array(lapply(L, length))
      LM <- round(mean(L),0)
      RMLIST <- c()
      if (sum(L<LM)>0) {
          ERRORLIST <- c( ERRORLIST, LIST[ which(L < LM) ] )
          RMLIST <- c( RMLIST, which(L < LM) )
      }
      if (sum(L>LM)>0) {
          ERRORLIST <- c( ERRORLIST, LIST[ which(L > LM) ] )
          RMLIST <- c( RMLIST, which(L > LM) )
      }
      if (length(RMLIST)>0) LIST <- LIST[ -RMLIST ]
   }
   LIST <- as.data.frame(t(simplify2array(strsplit(LIST,'/'))))

   nDir <- dim(simplify2array(strsplit(RAWPATH,'/')))[1]
   LIST <- LIST[, c(-1:-nDir)]
   nc <- dim(LIST)[2]
   nr <- dim(LIST)[1]
   if (nc<3) {
      if (!is.null(procParams)) return(0)
      LIST <- cbind(LIST[,1],LIST)
   }

   SL <- NULL
   if (nc>3) { SL <- LIST[, c(1:(nc-3)) ]; LIST <- LIST[ , c((nc-2):nc) ]; }
   nc=3

   if (length(levels(LIST[,1]))<nr && length(levels(LIST[,1]))>1 &&
       length(levels(LIST[,2]))<nr && length(levels(LIST[,2]))>1) {
      L <- levels(LIST[,1])
      LIST2 <- NULL
      for (i in 1:length(L)) {
         L2 <- LIST[ LIST[,1]==L[i], ]
         LIST2 <- rbind( LIST2, L2[ L2[,2]==levels(L2[,2])[1], ] )
      }
      LIST <- LIST2
   }
   nr <- dim(LIST)[1]
   MS <- as.matrix(LIST)

   if( !is.null(SL)) { LIST2 <- cbind(SL[c(1: dim(LIST)[1])], LIST); } else { LIST2 <- LIST; }
   nc <- dim(LIST2)[2]
   rawdir <- cbind( sapply(1:nr, function(x){ do.call( paste, c( RAWPATH, as.list(LIST2[x,c(1:(nc-1))]), sep="/")) }), MS[, 2], rep(0,nr) )

   if (length(levels(LIST[,1]))==nr) {
      M <-  MS[, c(1,1) ]
   } else {
      M <-  MS[, c(1,2) ]
   }
   if (nr==1 && class(M)=="character") M <- as.matrix(t(M))

   if (length(ERRORLIST)>0) {
      write.table(ERRORLIST, file=file.path(DATADIR,'errorlist.csv'), sep=';', row.names=F, col.names=F, quote=F)
   }
   if (OKRAW==1) {
      write.table(M, file=file.path(DATADIR,'samples.csv'), sep=';', row.names=F, col.names=F, quote=F)
      write.table(gsub("//", "/", rawdir), file=file.path(DATADIR,'rawids.csv'), sep=';', row.names=F, col.names=F, quote=F)
      fileConn<-file(file.path(DATADIR,'factors'))
      writeLines(lstfac, fileConn)
      close(fileConn)
   }
   return(OKRAW)
}

generate_Metadata_File_1r <- function(RAWDIR, DATADIR, procParams)
{
   ERRORLIST <- c()
   OKRAW <- 1
   lstfac <- c("1;Samplecode")
   RAWPATH <- gsub("//", "/", RAWDIR)

   LIST <- gsub("//", "/", list.files(path = RAWPATH, pattern = "1r$", all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE))
   if ( class(LIST)=="character" && length(LIST)==0 ) return(0)
   L <- simplify2array(strsplit(LIST,'/'))
   if (class(L) != "matrix") {
      L <- simplify2array(lapply(L, length))
      LM <- round(mean(L),0)
      RMLIST <- c()
      if (sum(L<LM)>0) {
          ERRORLIST <- c( ERRORLIST, LIST[ which(L < LM) ] )
          RMLIST <- c( RMLIST, which(L < LM) )
      }
      if (sum(L>LM)>0) {
          ERRORLIST <- c( ERRORLIST, LIST[ which(L > LM) ] )
          RMLIST <- c( RMLIST, which(L > LM) )
      }
      if (length(RMLIST)>0) LIST <- LIST[ -RMLIST ]
   }
   LIST <- as.data.frame(t(simplify2array(strsplit(LIST,'/'))))

   # Check if we have a Bruker directory structure
   nDir <- dim(simplify2array(strsplit(RAWPATH,'/')))[1]
   LIST <- LIST[, c(-1:-nDir)]
   nc <- dim(LIST)[2]
   nr <- dim(LIST)[1]
   if (nc<5) return(0)

   SL <- NULL
   if (nc>5) { SL <- LIST[, c(1:(nc-5)) ]; LIST <- LIST[ , c((nc-4):nc) ]; }
   nc=5

   L <- levels(LIST[,4])
   if (length(L)>1) {
      LIST <- LIST[ LIST[,4]==L[1], ]
   }
   nr <- dim(LIST)[1]

   if (length(levels(LIST[,1]))<nr && length(levels(LIST[,1]))>1 &&
       length(levels(LIST[,2]))<nr && length(levels(LIST[,2]))>1) {
      L <- levels(LIST[,1])
      LIST2 <- NULL
      for (i in 1:length(L)) {
         L2 <- LIST[ LIST[,1]==L[i], ]
         LIST2 <- rbind( LIST2, L2[ L2[,2]==levels(L2[,2])[1], ] )
      }
      LIST <- LIST2
   }
   nr <- dim(LIST)[1]
   MS <- as.matrix(LIST)

   if( !is.null(SL)) { LIST2 <- cbind(SL[c(1: dim(LIST)[1])], LIST); } else { LIST2 <- LIST; }
   nc <- dim(LIST2)[2]
   rawdir <- cbind( sapply(1:nr, function(x){ do.call( paste, c( RAWPATH, as.list(LIST2[x,c(1:(nc-3))]), sep="/")) }), MS[, 2], MS[, 4] )

   if (length(levels(LIST[,1]))==nr) {
      M <-  MS[, c(1,1) ]
   } else {
      M <-  MS[, c(1,2) ]
   }
   if (nr==1 && class(M)=="character") M <- as.matrix(t(M))

   if (length(ERRORLIST)>0) {
      write.table(ERRORLIST, file=file.path(DATADIR,'errorlist.csv'), sep=';', row.names=F, col.names=F, quote=F)
   }
   if (OKRAW==1) {
      write.table(M, file=file.path(DATADIR,'samples.csv'), sep=';', row.names=F, col.names=F, quote=F)
      write.table(gsub("//", "/", rawdir), file=file.path(DATADIR,'rawids.csv'), sep=';', row.names=F, col.names=F, quote=F)
      fileConn<-file(file.path(DATADIR,'factors'))
      writeLines(lstfac, fileConn)
      close(fileConn)
   }

   return(OKRAW)
}

set_Metadata_File <- function(RAWDIR, DATADIR, procParams)
{
   if (procParams$VENDOR == "bruker") return (set_Metadata_File_Bruker(RAWDIR, DATADIR, procParams))
   if (procParams$VENDOR == "varian") return (set_Metadata_File_Varian(RAWDIR, DATADIR, procParams))
   if (procParams$VENDOR == "nmrml")  return (set_Metadata_File_nmrML(RAWDIR, DATADIR, procParams))
   if (procParams$VENDOR == "jeol")   return (set_Metadata_File_jeol(RAWDIR, DATADIR, procParams))
   return(0)
}

set_Metadata_File_Bruker <- function(RAWDIR, DATADIR, procParams)
{
   SampleFile <- file.path(RAWDIR,'samples.txt')
   samples <- read.table(SampleFile, sep="\t", header=T,stringsAsFactors=FALSE)
   samplesize <- dim(samples)
   nraw <- samplesize[1]
   nbcol <- samplesize[2]

   lstfac <- c("1;Samplecode")
   rawdir <- NULL
   ERRORLIST <- c()
   OKRAW <- 1

   if (procParams$INPUT_SIGNAL == "1r") {
      LIST <- gsub('//', '/', list.files(path = RAWDIR, pattern = "1r$", all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE))
      if ( class(LIST)=="character" && length(LIST)==0 ) return(0)
      LIST <- as.data.frame(t(simplify2array(strsplit(LIST,'/'))))
      
      # Check if we have a Bruker directory structure
      nDir <- dim(simplify2array(strsplit(RAWDIR,'/')))[1]
      nc <- dim(LIST)[2]
      if ((nc-nDir)>5) {
          RAWDIR <- do.call( paste, c( RAWDIR, as.list(LIST[1,c((nDir+1):(nc-5))]), sep="/"))
      }
      for (i in 1:nraw) {
          if ( file.exists( paste(RAWDIR, samples[i,1],samples[i,3],"pdata",samples[i,4], "1r", sep="/")) ) {
              rawdir <- rbind( rawdir, c( paste(RAWDIR, samples[i,1], samples[i,3], sep="/"), samples[i,3], samples[1,4] ) )
          } else {
              ERRORLIST <- c( ERRORLIST, paste(samples[1,1],samples[i,3],"pdata",samples[i,4], "1r", sep="/") )
              OKRAW <- 0
          }
      }
   } else {
      LIST <- gsub("//", "/", list.files(path = RAWDIR, pattern = "fid$", all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE))
      LIST <- as.data.frame(t(simplify2array(strsplit(LIST,'/'))))
      
      # Check if we have a Bruker directory structure
      nDir <- dim(simplify2array(strsplit(RAWDIR,'/')))[1]
      nc <- dim(LIST)[2]
      if ((nc-nDir)>3) {
          RAWDIR <- do.call( paste, c( RAWDIR, as.list(LIST[1,c((nDir+1):(nc-3))]), sep="/"))
      }
      for (i in 1:nraw) {
          if ( file.exists( paste(RAWDIR, samples[i,1],samples[i,3],"fid", sep="/")) ) {
              rawdir <- rbind( rawdir, c( paste(RAWDIR, samples[i,1], samples[i,3], sep="/"), samples[i,3], 0 ) )
          } else {
              ERRORLIST <- c( ERRORLIST, paste(samples[i,1],samples[i,3],"fid", sep="/") )
              OKRAW <- 0
          }
      }
   }

   if (nbcol==4) {
       M <- cbind(samples[,1], samples[,2])
   }
   if (nbcol>4) {
       M <- samples[,c(-3:-4)]
       lstfac <- c( lstfac, paste(c(2:(nbcol-3)), colnames(samples)[c(-1:-4)], sep=";") )
   }

   if (length(ERRORLIST)>0) {
      write.table(ERRORLIST, file=file.path(DATADIR,'errorlist.csv'), sep=';', row.names=F, col.names=F, quote=F)
   }
   if (OKRAW==1) {
      write.table(M, file=file.path(DATADIR,'samples.csv'), sep=';', row.names=F, col.names=F, quote=F)
      write.table(gsub("//", "/", rawdir), file=file.path(DATADIR,'rawids.csv'), sep=';', row.names=F, col.names=F, quote=F)
      fileConn<-file(file.path(DATADIR,'factors'))
      writeLines(lstfac, fileConn)
      close(fileConn)
   }

   return(OKRAW)
}

set_Metadata_File_Varian <- function(RAWDIR, DATADIR, procParams)
{
   lstfac <- c("1;Samplecode")
   rawdir <- NULL
   ERRORLIST <- c()
   OKRAW <- 1

   LIST <- gsub('//', '/', list.files(path = RAWDIR, pattern = "fid$", all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE))
   if ( class(LIST)=="character" && length(LIST)==0 ) return(0)

   SampleFile <- file.path(RAWDIR,'samples.txt')
   if (file.exists(SampleFile)) {
       samples <- read.table(SampleFile, sep="\t", header=T,stringsAsFactors=FALSE)
       samplesize <- dim(samples)
       nraw <- samplesize[1]
       nbcol <- samplesize[2]

       LIST <- as.data.frame(t(simplify2array(strsplit(LIST,'/'))))
      
       # Check directory structure
       nDir <- dim(simplify2array(strsplit(RAWDIR,'/')))[1]
       nc <- dim(LIST)[2]
       if ((nc-nDir)>2) {
           RAWDIR <- do.call( paste, c( RAWDIR, as.list(LIST[1,c((nDir+1):(nc-2))]), sep="/"))
       }
      for (i in 1:nraw) {
          if ( file.exists( paste(RAWDIR, samples[i,1],"fid", sep="/")) ) {
              rawdir <- rbind( rawdir, c( paste(RAWDIR, samples[i,1], sep="/"), 0, 0 ) )
          } else {
              ERRORLIST <- c( ERRORLIST, paste(samples[i,1],"fid", sep="/") )
              OKRAW <- 0
          }
      }
      if (nbcol==2) {
          M <- cbind(samples[,1], samples[,2])
      }
      if (nbcol>2) {
         M <- samples
         lstfac <- c( lstfac, paste(c(2:(nbcol-1)), colnames(samples)[c(-1:-2)], sep=";") )
      }
   } else {
      rawdir <- cbind( dirname(LIST), rep(0, length(LIST)), rep(0, length(LIST)) )
      M <- cbind( basename(dirname(LIST)), basename(dirname(LIST)) )
   }
   if (length(ERRORLIST)>0) {
      write.table(ERRORLIST, file=file.path(DATADIR,'errorlist.csv'), sep=';', row.names=F, col.names=F, quote=F)
   }
   if (OKRAW==1) {
      write.table(M, file=file.path(DATADIR,'samples.csv'), sep=';', row.names=F, col.names=F, quote=F)
      write.table(gsub("//", "/", rawdir), file=file.path(DATADIR,'rawids.csv'), sep=';', row.names=F, col.names=F, quote=F)
      fileConn<-file(file.path(DATADIR,'factors'))
      writeLines(lstfac, fileConn)
      close(fileConn)
   }
   return(OKRAW)
}

set_Metadata_File_nmrML <- function(RAWDIR, DATADIR, procParams)
{
      set_Metadata_File_ext(RAWDIR, DATADIR, procParams, "nmrML")
}
set_Metadata_File_jeol <- function(RAWDIR, DATADIR, procParams)
{
      set_Metadata_File_ext(RAWDIR, DATADIR, procParams, "jdf")
}

set_Metadata_File_ext <- function(RAWDIR, DATADIR, procParams, ext)
{
   lstfac <- c("1;Samplecode")
   rawdir <- NULL
   ERRORLIST <- c()
   OKRAW <- 1
   pattern <- paste0('.',ext,'$')
   LIST <- gsub('//', '/', list.files(path = RAWDIR, pattern = pattern, all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE))
   if ( class(LIST)=="character" && length(LIST)==0 ) return(0)

   SampleFile <- file.path(RAWDIR,'samples.txt')
   if (file.exists(SampleFile)) {
      samples <- read.table(SampleFile, sep="\t", header=T,stringsAsFactors=FALSE)
      samplesize <- dim(samples)
      nraw <- samplesize[1]
      nbcol <- samplesize[2]

      LIST2 <- as.data.frame(t(simplify2array(strsplit(LIST,'/'))))
      nc <- dim(LIST2)[2]
      for (i in 1:nraw) {
          if (sum( samples[i,1] == LIST2[,nc]) == 1) {
              rawdir <- rbind( rawdir,  c( LIST[ which(samples[i,1] == LIST2[,nc]) ], 0, 0)  )
          } else {
              ERRORLIST <- c( ERRORLIST, samples[i,1] )
              OKRAW <- 0
          }
      }
      if (nbcol==2) {
          M <- cbind(samples[,1], samples[,2])
      }
      if (nbcol>2) {
         M <- samples
         lstfac <- c( lstfac, paste(c(2:(nbcol-1)), colnames(samples)[c(-1:-2)], sep=";") )
      }
   } else {
      rawdir <- cbind( LIST, rep(0, length(LIST)), rep(0, length(LIST)) )
      M <- cbind( basename(LIST), gsub(pattern, "", basename(LIST)) )
   }
   if (length(ERRORLIST)>0) {
      write.table(ERRORLIST, file=file.path(DATADIR,'errorlist.csv'), sep=';', row.names=F, col.names=F, quote=F)
   }
   if (OKRAW==1) {
      write.table(M, file=file.path(DATADIR,'samples.csv'), sep=';', row.names=F, col.names=F, quote=F)
      write.table(gsub("//", "/", rawdir), file=file.path(DATADIR,'rawids.csv'), sep=';', row.names=F, col.names=F, quote=F)
      fileConn<-file(file.path(DATADIR,'factors'))
      writeLines(lstfac, fileConn)
      close(fileConn)
   }
   return(OKRAW)
}

#------------------------------
# airPLS
#------------------------------
WhittakerSmooth <- function(x,w,lambda,differences=1)
{
  x=matrix(x,nrow = 1, ncol=length(x))
  L=length(x)
  E=spMatrix(L,L,i=seq(1,L),j=seq(1,L),rep(1,L))
  D=methods::as(diff(E,1,differences),"dgCMatrix")
  W=methods::as(spMatrix(L,L,i=seq(1,L),j=seq(1,L),w),"dgCMatrix")
  background=solve((W+lambda*t(D)%*%D),t((w*x)));
  return(as.vector(background))
}
 
airPLS <- function(x,lambda=100,porder=1, itermax=8)
{
  x = as.vector(x)
  m = length(x)
  w = rep(1,m)
  i = 1
  repeat {
     z = WhittakerSmooth(x,w,lambda,porder)
     d = x-z
     sum_smaller = abs(sum(d[d<0])) 
     if(sum_smaller<0.001*sum(abs(x))||i==itermax) break
     w[d>=0] = 0
     w[d<0] = exp(i*abs(d[d<0])/sum_smaller)
     w[1] = exp(i*max(d[d<0])/sum_smaller)
     w[m] = exp(i*max(d[d<0])/sum_smaller)
     i=i+1
  }
  return(z) 
}


#------------------------------
# Peak detection for spectra
#------------------------------
# Input parameters
#   - X: spectral dataset in matrix format in which each row contains a single sample
#   - nDivRange: size of a single small segment after division of spectra, Default value: 64
#   - baselineThresh: removal of all the peaks with intensity lower than this threshold, Default value: 50000
# Output parameters
#   - peak lists of the spectra
detectSpecPeaks <- function (X, nDivRange, scales=seq(1, 16, 2), baselineThresh, SNR.Th=-1, ProgressFile=NULL) 
{
  LOGFILE <- file.path(dirname(ProgressFile),"clupa.out")
  if( !is.null(ProgressFile) ) Write.LOG(LOGFILE,"Ralign1D:  BEGIN detectSpecPeaks");
  nFea <- ncol(X)
  nSamp <- nrow(X)
  noiseEsp <- 0.005
  if (SNR.Th < 0) SNR.Th <- max(scales) * 0.05
  if( !is.null(ProgressFile) ) init_counter(ProgressFile, nSamp)
  pList <- foreach(i=1:nSamp) %dopar% {
     myPeakRes <- NULL
     mySpec <- X[i, ]
     for (k in 1:length(nDivRange)) {
        divR <- nDivRange[k]
        for (j in 1:(trunc(nFea/divR) - 3)) {
           startR <- (j - 1) * divR + 1
           if (startR >= nFea)  startR <- nFea
           endR <- (j + 3) * divR
           if (endR > nFea) endR <- nFea
           xRange <- mySpec[startR:endR]
           xMean <- mean(xRange)
           xMedian <- median(xRange)
           if ((xMean == xMedian) || abs(xMean - xMedian)/((xMean + xMedian) * 2) < noiseEsp) next
           peakInfo <- MassSpecWavelet::peakDetectionCWT(mySpec[startR:endR], scales = scales, SNR.Th = SNR.Th)
           majorPeakInfo <- peakInfo$majorPeakInfo
           if (length(majorPeakInfo$peakIndex) > 0) myPeakRes <- c(myPeakRes, majorPeakInfo$peakIndex + startR - 1)
        }
     }
     plst <- list(myPeakRes)
     plst[[1]] <- sort(unique(plst[[1]]))
     plst[[1]] <- plst[[1]][which(mySpec[ plst[[1]] ] > baselineThresh)]
     plst[[1]] <- sort(plst[[1]])
     if( !is.null(ProgressFile) ) inc_counter(ProgressFile, i)
     if( !is.null(ProgressFile) ) Write.LOG(LOGFILE,paste("Ralign1D:  Spectrum",i," Nb peaks =",length(plst[[1]])));
     plst
  }
  if( !is.null(ProgressFile) ) Write.LOG(LOGFILE,"Ralign1D:  END detectSpecPeaks");
  pList <-  simplify2array(pList)
  return(pList)
}


#------------------------------
# CluPA alignment for multiple spectra
#------------------------------
# Input parameters
#   - X: spectral dataset in the matrix format in which each row contains a single sample
#   - peakList: peak lists of the spectra
#   - refInd: index of the reference spectrum
#   - maxShift:  maximum number of the points for a shift step
# Output parameters
#   - aligned spectra: same format as input
dohCluster <- function (X, peakList, refInd = 1, maxShift = 50, acceptLostPeak = TRUE, ProgressFile=NULL) 
{
  refSpec = X[refInd, ]
  if( !is.null(ProgressFile) ) init_counter(ProgressFile, nrow(X))
  Y <- foreach(tarInd=1:nrow(X), .combine=rbind) %dopar% {
     if (tarInd == refInd) { refSpec; } else {
     tarSpec <- X[tarInd, ]
     myPeakList <- c(peakList[[refInd]], peakList[[tarInd]])
     myPeakLabel <- double(length(myPeakList))
     myPeakLabel[1:length(peakList[[refInd]])] <- 1
     res <- hClustAlign(refSpec, tarSpec, myPeakList, myPeakLabel, 1, length(tarSpec), maxShift = maxShift, acceptLostPeak = TRUE)
     if( !is.null(ProgressFile) ) inc_counter(ProgressFile, tarInd)
     res$tarSpec
  }}
  return(Y)
}

#------------------------------
# Reference spectrum determination
#------------------------------
FindRef <- function (peakList)
{
    opts <- list(chunkSize=2)
    disS <- foreach(refInd = 1:length(peakList), .combine = 'rbind', .options.nws=opts) %dopar% {
        V <- rep(NA, length(peakList))
        for (tarInd in 1:length(peakList)) if (refInd != tarInd) {
            V[tarInd] = 0
            for (i in 1:length(peakList[[tarInd]])) V[tarInd] = V[tarInd] + min(abs(peakList[[tarInd]][i] - peakList[[refInd]]))
        }
        V
    }
    sumDis = double(length(peakList))
    for (refInd in 1:length(peakList)) {
        disS[refInd, refInd] = 0
        sumDis[refInd] = sum(disS[refInd, ])
    }
    orderSumdis = order(sumDis)
    return(list(refInd = orderSumdis[1], orderSpec = orderSumdis))
}

#------------------------------
# Spectra alignment - see https://cran.r-project.org/web/packages/speaq/vignettes/speaq.pdf
#------------------------------
# Input parameters
#   - data: n x p datamatrix
#   - nDivRange: size of a single small segment after division of the whole spectrum, Default value: 64
#   - reference: number of the spectrum reference; if NULL, automatic detection, Default value: NULL
#   - baselineThresh: removal of all the peaks with intensity lower than this threshold, Default value: 50000
# Output parameters
#   - Y: n x p datamatrix
CluPA <- function(data, reference=reference, nDivRange, scales = seq(1, 16, 2), baselineThresh,  SNR.Th = -1, maxShift=50, ProgressFile=NULL)
{
  LOGFILE <- NULL
  if( !is.null(ProgressFile) ) LOGFILE <- file.path(dirname(ProgressFile),conf$LOGFILE)
  if( !is.null(ProgressFile) ) Write.LOG(LOGFILE,"Ralign1D:  BEGIN CluPA", mode="at");

  ## Peak picking
  if( !is.null(ProgressFile) ) Write.LOG(LOGFILE,paste("Ralign1D:  --- Peak detection : nDivRange =",nDivRange));
  startTime <- proc.time()
  peakList <- detectSpecPeaks(X=data, nDivRange=nDivRange, scales=scales, baselineThresh=baselineThresh, SNR.Th = SNR.Th, ProgressFile=ProgressFile)
  endTime <- proc.time()
  if( !is.null(ProgressFile) ) Write.LOG(LOGFILE,paste("Ralign1D:  --- Peak detection time: ",(endTime[3]-startTime[3])," sec"));
  if( !is.null(ProgressFile) ) unlink(ProgressFile)

  ## Reference spectrum determination
  if (reference == 0) {
     if( !is.null(ProgressFile) ) Write.LOG(LOGFILE,"Ralign1D:  --- Find the spectrum reference...");
     startTime <- proc.time()
     resFindRef<- FindRef(peakList)
     refInd <- resFindRef$refInd
     endTime <- proc.time()
     if( !is.null(ProgressFile) ) Write.LOG(LOGFILE,paste("Ralign1D:  --- The reference is: ",refInd));
     if( !is.null(ProgressFile) ) Write.LOG(LOGFILE,paste("Ralign1D:  --- Finding time: ",(endTime[3]-startTime[3])," sec"));
  } else  {
     refInd=reference
  }
  ## Spectra alignment to the reference
  if( !is.null(ProgressFile) ) Write.LOG(LOGFILE,paste("Ralign1D:  --- Spectra alignment to the reference: maxShift =",maxShift));
  startTime <- proc.time()
  Y <- dohCluster(data, peakList=peakList, refInd=refInd, maxShift=maxShift, acceptLostPeak, ProgressFile=ProgressFile)
  endTime <- proc.time()
  if( !is.null(ProgressFile) ) Write.LOG(LOGFILE,paste("Ralign1D:  --- Spectra alignment time: ",(endTime[3]-startTime[3])," sec"));
  if( !is.null(ProgressFile) ) Write.LOG(LOGFILE,"Ralign1D:  END CluPA");

  ## Output  
  return(Y)
}

#------------------------------
# Calibration ot the PPM Scale
#------------------------------
RCalib1D <- function(specMat, PPM_NOISE_AREA, zoneref, ppmref)
{
   i1<-length(which(specMat$ppm>max(zoneref)))
   i2<-which(specMat$ppm<=min(zoneref))[1]

   # PPM calibration of each spectrum
   for ( i in 1:specMat$nspec ) {
       i0 <- i1 + which(specMat$int[i, i1:i2]==max(specMat$int[i, i1:i2])) - 1
       ppm0 <- specMat$ppm_max - (i0-1)*specMat$dppm
       dppmref <- ppm0 - ppmref
       decal <- 0
       # sig <- fitdistr(specMat$int[i, length(which(specMat$ppm>PPM_NOISE_AREA[2])):(which(specMat$ppm<=PPM_NOISE_AREA[1])[1])], "normal")$estimate[2]
       sig <- C_estime_sd(specMat$int[i, 1:specMat$size],128)
       if (abs(dppmref) > specMat$dppm) {
           decal <- trunc(dppmref/specMat$dppm)
           dppmref <- dppmref - decal*specMat$dppm
       }
       if (abs(dppmref) > 0.5*specMat$dppm) {
           decal <- decal + trunc(2*dppmref/specMat$dppm)
           dppmref <- dppmref - trunc(2*dppmref/specMat$dppm)*specMat$dppm
       }
       if (decal==0) next

       if (decal<0) {
          specMat$int[i, 1:(specMat$size-abs(decal))] <- specMat$int[i,(1+abs(decal)):specMat$size]
          specMat$int[i, (specMat$size-abs(decal)+1):specMat$size] <- rnorm(length((specMat$size-abs(decal)+1):specMat$size), mean=specMat$int[i,specMat$size-abs(decal)-1], sd=sig)
       }
       if (decal>0) {
          specMat$int[i,(1+abs(decal)):specMat$size] <- specMat$int[i, 1:(specMat$size-abs(decal))]
          specMat$int[i, 1:abs(decal)] <- rnorm(length(1:abs(decal)), mean=specMat$int[i,abs(decal)+1], sd=sig)
       }
   }
   return(specMat)
}

#------------------------------
# Normalisation of the Intensities
#------------------------------
RNorm1D <- function(specMat, normmeth, zones)
{
   N <- dim(zones)[1]
   if (N>1) registerDoParallel(cores=2)

   if (normmeth=='CSN') {
      # 1/ Integration of each zone ...
      SUM <- foreach(i=1:N, .combine='+') %dopar% {
          i1<-length(which(specMat$ppm>max(zones[i,])))
          i2<-which(specMat$ppm<=min(zones[i,]))[1]
          simplify2array(lapply( 1:specMat$nspec, function(x) { 
                0.5*(specMat$int[x, i1] + specMat$int[x, i2]) + sum(specMat$int[x,(i1+1):(i2-1)])
          }))
      }
      COEFF <- SUM/mean(SUM)
   }
   if (normmeth=='PQN') {
      # 1/ Get spectra values for each zone ...
      SUBMAT <- foreach(i=1:N, .combine=cbind) %dopar% {
          i1<-length(which(specMat$ppm>max(zones[i,])))
          i2<-which(specMat$ppm<=min(zones[i,]))[1]
          # .. for each spectrum
          t(simplify2array(lapply( 1:specMat$nspec, function(x) { specMat$int[x,i1:i2] })))
      }
      # Calculate the most probable quotient
      V <- apply(SUBMAT, 2, median)
      MQ <- t(t(SUBMAT)/V)
      COEFF <- apply(MQ,1,median)
   }

   # 2/ Apply to each spectrum, its corresponding coefficient
   V <- lapply( 1:specMat$nspec, function(x) { specMat$int[x,] <<- specMat$int[x,]/COEFF[x] } )
   return(specMat)
}

#------------------------------
# Global Baseline Correction
#------------------------------
RGbaseline1D <- function(specMat,PPM_NOISE_AREA, zone, WS, NEIGH, ProgressFile=NULL)
{

   NFAC <- 1.5
   NEGFAC <- 10
   NBPASS <- 2

   i1 <- ifelse( max(zone)>=specMat$ppm_max, 1, length(which(specMat$ppm>max(zone))) )
   i2 <- ifelse( min(zone)<=specMat$ppm_min, specMat$size - 1, which(specMat$ppm<=min(zone))[1] )
   TD <- specMat$size

   # Baseline Estimation for each spectrum
   if( !is.null(ProgressFile) ) init_counter(ProgressFile, specMat$nspec)
   BLList <- foreach(i=1:specMat$nspec, .combine=cbind) %dopar% {
       sig <- fitdistr(specMat$int[i, length(which(specMat$ppm>PPM_NOISE_AREA[2])):(which(specMat$ppm<=PPM_NOISE_AREA[1])[1])], "normal")$estimate[2]
       mv <- simplify2array(lapply( c(3:61), function(x) { mean(abs(specMat$int[i, ((x-1)*TD/64):(x*TD/64)])); }))
       mmoy<-min(mv)
       if (min(specMat$int[i, ])< -NEGFAC*mmoy) {
           Vthreshold <- max(specMat$int[ i, specMat$int[i, ]/mmoy < -NEGFAC ])
           specMat$int[ i, specMat$int[i, ]/mmoy < -NEGFAC ] <- Vthreshold
       }
       V <- specMat$int[i, ]
       if (NBPASS==1) {
           BL <- C_Estime_LB (V, i1, i2, WS, NEIGH, NFAC*sig)
       } else {
           BL <- 0*rep(1:length(V))
           for( n in 1:NBPASS) {
              # Estimation of Baseline
              BLn <- C_Estime_LB (V, i1, i2, WS, NEIGH, NFAC*sig)
              V <- V - BLn
              BL <- BL + BLn
           }
       }
       if( !is.null(ProgressFile) ) inc_counter(ProgressFile, i)
       BL
   }

   # Baseline Correction for each spectrum
   for ( i in 1:specMat$nspec ) {
       mv <- simplify2array(lapply( c(3:61), function(x) { mean(abs(specMat$int[i, ((x-1)*TD/64):(x*TD/64)])); }))
       mmoy<-min(mv)
       if (min(specMat$int[i, ])< -NEGFAC*mmoy) {
           Vthreshold <- max(specMat$int[ i, specMat$int[i, ]/mmoy < -NEGFAC ])
           specMat$int[ i, specMat$int[i, ]/mmoy < -NEGFAC ] <- Vthreshold
       }
       if( is.null(dim(BLList)) ) { BL <- BLList; } else { BL <- BLList[,i]; }
       specMat$int[i,c(i1:i2)] <- specMat$int[i,c(i1:i2)] - BL[c(i1:i2)]
   }

   return(specMat)
}

#------------------------------
# Local Baseline Correction (deprecated)
#------------------------------
RBaseline1D <- function(specMat,PPM_NOISE_AREA, zone, WINDOWSIZE, ProgressFile=NULL)
{
   i1 <- ifelse( max(zone)>=specMat$ppm_max, 1, length(which(specMat$ppm>max(zone))) )
   i2 <- ifelse( min(zone)<=specMat$ppm_min, specMat$size - 1, which(specMat$ppm<=min(zone))[1] )

   SMOOTHSIZE <- round(WINDOWSIZE/2)
   ALPHA <- 0.2

   # Ajust the window size parameter
   n <- i2-i1+1
   ws <- WINDOWSIZE
   dws <- 0
   signdws <- ifelse ( n/ws>round(n/ws), 1, -1 )
   dmin <- 1
   if (n>WINDOWSIZE) {
      repeat {
          d <- abs(n/(ws+signdws*dws)-round(n/(ws+signdws*dws)))
          if ( d>dmin ) { dws <- dws - signdws; break }
          dmin <- d; dws <- dws + signdws;
      }
      WINDOWSIZE <- ws+signdws*dws
      n2 <- round(n/(ws+signdws*dws))*(ws+signdws*dws)
      if (n2<n) i2 <- i2 - (n-n2)
   } else {
      WINDOWSIZE <- n
   }

   # Baseline Estimation for each spectrum
   if( !is.null(ProgressFile) ) init_counter(ProgressFile, specMat$nspec)
   BLList <- foreach(i=1:specMat$nspec, .combine=cbind) %dopar% {
       specSig <- fitdistr(specMat$int[i,length(which(specMat$ppm>PPM_NOISE_AREA[2])):(which(specMat$ppm<=PPM_NOISE_AREA[1])[1])], "normal")$estimate[2]
       x <- specMat$int[i,c(i1:i2)]
       xmat <- matrix(x, nrow=WINDOWSIZE)
       ymin <- apply(xmat, 2, min) + 1.2*specSig
       y1 <- rep(ymin, each = WINDOWSIZE)
       y2 <- Smooth(y1,SMOOTHSIZE)
       BL <- simplify2array(lapply(c(1:length(x)), function(k) { min(y1[k], y2[k]); }))
       BL <- lowpass1(BL, ALPHA)
       if( !is.null(ProgressFile) ) inc_counter(ProgressFile, i)
       BL
   }

   # Baseline Correction for each spectrum
   for ( i in 1:specMat$nspec ) {
       if( is.null(dim(BLList)) ) { BL <- BLList; } else { BL <- BLList[,i]; }
       specMat$int[i,c(i1:i2)] <- specMat$int[i,c(i1:i2)] - BL
   }

   return(specMat)
}

#------------------------------
# q-NMR Baseline Correction
#------------------------------
Rqnmrbc1D <- function(specMat, PPM_NOISE_AREA, zone, ProgressFile=NULL)
{
   i1 <- ifelse( max(zone)>=specMat$ppm_max, 1, length(which(specMat$ppm>max(zone))) )
   i2 <- ifelse( min(zone)<=specMat$ppm_min, specMat$size - 1, which(specMat$ppm<=min(zone))[1] )
   n <- i2-i1+1
   NLOOP <- 5
   dN <- round(0.00075/specMat$dppm)
   CSIG <- 5

   # Baseline Estimation for each spectrum
   if( !is.null(ProgressFile) ) init_counter(ProgressFile, specMat$nspec)
   BLList <- foreach(i=1:specMat$nspec, .combine=cbind) %dopar% {
       specSig <- fitdistr(specMat$int[i,length(which(specMat$ppm>PPM_NOISE_AREA[2])):(which(specMat$ppm<=PPM_NOISE_AREA[1])[1])], "normal")$estimate[2]
       x <- specMat$int[i,c(i1:i2)]
       bc <- 0*rep(1:n)
       for (l in 1:NLOOP) {
           bci <- C_GlobSeg(x, dN, CSIG*specSig)
           x <- x - bci
           bc <- bc + bci
       }
       if( !is.null(ProgressFile) ) inc_counter(ProgressFile, i)
       bc
  }

   # Baseline Correction for each spectrum
   for ( i in 1:specMat$nspec ) {
       if( is.null(dim(BLList)) ) { BL <- BLList; } else { BL <- BLList[,i]; }
       V <- specMat$int[i,c(i1:i2)] - BL
       specMat$int[i,c(i1:i2)] <- V
   }

   return(specMat)
}

#------------------------------
# airPLS : Local Baseline Correction
#------------------------------
RairPLSbc1D <- function(specMat, zone, clambda, ProgressFile=NULL)
{
   i1 <- ifelse( max(zone)>=specMat$ppm_max, 1, length(which(specMat$ppm>max(zone))) )
   i2 <- ifelse( min(zone)<=specMat$ppm_min, specMat$size - 1, which(specMat$ppm<=min(zone))[1] )
   n <- i2-i1+1
   cmax <- 6

   lambda <- ifelse (clambda==cmax, 5, 10^(cmax-clambda) )
   # Baseline Estimation for each spectrum
   if( !is.null(ProgressFile) ) init_counter(ProgressFile, specMat$nspec)
   BLList <- foreach(i=1:specMat$nspec, .combine=cbind) %dopar% {
       x <- specMat$int[i,c(i1:i2)]
       bc <- airPLS(x, lambda)
       if( !is.null(ProgressFile) ) inc_counter(ProgressFile, i)
       bc
   }

   # Baseline Correction for each spectrum
   for ( i in 1:specMat$nspec ) {
       if( is.null(dim(BLList)) ) { BL <- BLList; } else { BL <- BLList[,i]; }
       V <- specMat$int[i,c(i1:i2)] - BL
       specMat$int[i,c(i1:i2)] <- V
   }

   return(specMat)
}

#------------------------------
# Denoising the selected PPM ranges
#------------------------------
RFilter1D <- function(specMat,zone, FILTORD, FILTLEN, ProgressFile=NULL)
{
   i1 <- ifelse( max(zone)>=specMat$ppm_max, 1, length(which(specMat$ppm>max(zone))) )
   i2 <- ifelse( min(zone)<=specMat$ppm_min, specMat$size - 1, which(specMat$ppm<=min(zone))[1] )

   sgfilt <- sgolay(p=FILTORD, n=FILTLEN)

   # Denoising each spectrum
   if( !is.null(ProgressFile) ) init_counter(ProgressFile, specMat$nspec)
   for ( i in 1:specMat$nspec ) {
        x <- specMat$int[i,c(i1:i2)]
        SpecMat_sg <- filter(sgfilt,x)
        specMat$int[i,c(i1:i2)] <- SpecMat_sg
        if( !is.null(ProgressFile) ) inc_counter(ProgressFile, i)
   }

   return(specMat)

}

#------------------------------
# Zeroing the selected PPM ranges
#------------------------------
RZero1D <- function(specMat, zones, LOGFILE=NULL, ProgressFile=NULL)
{
   # Zeroing each PPM range
   N <- dim(zones)[1]
   if( !is.null(ProgressFile) ) init_counter(ProgressFile, N)

   for ( i in 1:N ) {
       i1<-length(which(specMat$ppm>max(zones[i,])))
       i2<-which(specMat$ppm<=min(zones[i,]))[1]
       specMat$int[,c(i1:i2)] <- matrix(0,specMat$nspec,(i2-i1+1))
       if( !is.null(LOGFILE) ) Write.LOG(LOGFILE,paste("Rnmr1D:     Zone",i,"= (",min(zones[i,]),",",max(zones[i,]),")"))
       if( !is.null(ProgressFile) ) inc_counter(ProgressFile, i)
   }

   return(specMat)
}

#------------------------------
# LS : Alignment of the selected PPM ranges
#------------------------------
RAlign1D <- function(specMat, zone, RELDECAL=0.35, idxSref=0, Selected=NULL, ProgressFile=NULL)
{
   # Alignment of each PPM range
   NBPASS <- 3
   if( !is.null(ProgressFile) ) init_counter(ProgressFile, NBPASS)
   i1 <- ifelse( max(zone)>=specMat$ppm_max, 1, length(which(specMat$ppm>max(zone))) )
   i2 <- ifelse( min(zone)<=specMat$ppm_min, specMat$size - 1, which(specMat$ppm<=min(zone))[1] )
   
   decal <- round((i2-i1)*RELDECAL)
   for( n in 1:NBPASS) {
       ret <- align_segment(specMat$int, segment_shifts( specMat$int, idxSref, decal, i1-1, i2-1, Selected-1), i1-1, i2-1, Selected-1)
       if( !is.null(ProgressFile) ) inc_counter(ProgressFile, n)
   }

   return(specMat)
}

#------------------------------
# CluPA : Alignment of the selected PPM ranges
#------------------------------
RCluPA1D <- function(specMat, zonenoise, zone, resolution=0.02, SNR=3, idxSref=0, Selected=NULL, ProgressFile=NULL)
{
   i1 <- ifelse( max(zone)>=specMat$ppm_max, 1, length(which(specMat$ppm>max(zone))) )
   i2 <- ifelse( min(zone)<=specMat$ppm_min, specMat$size - 1, which(specMat$ppm<=min(zone))[1] )
   
   # Noise estimation
   if (is.na(zonenoise)) {
       PPM_NOISE_AREA <- c(10.2, 10.5)
   } else {
      PPM_NOISE_AREA <- c(min(zonenoise), max(zonenoise))
   }
   idx_Noise <- c( length(which(specMat$ppm>PPM_NOISE_AREA[2])),(which(specMat$ppm<=PPM_NOISE_AREA[1])[1]) )
   Vref <- spec_ref(specMat$int)
   ynoise <- C_noise_estimation(Vref,idx_Noise[1],idx_Noise[2])
   
   # Parameters
   baselineThresh <- SNR*mean( C_noise_estimate(specMat$int, idx_Noise[1],idx_Noise[2], 1) )
   nDivRange <- max( round(resolution/specMat$dppm,0), 64 )
   maxshift <- min( round(0.01/specMat$dppm), round(nDivRange/4) )

   # Subpart of spectra
   if( is.null(Selected)) M<-specMat$int[, c(i1:i2) ] else  M<-specMat$int[Selected, c(i1:i2) ];

    M.aligned <- CluPA(M, reference=idxSref, nDivRange, scales = seq(1, 8, 2), 
                          baselineThresh,  SNR.Th = 0.1, maxShift=maxshift, ProgressFile=ProgressFile)

   if( is.null(Selected)) specMat$int[ ,c(i1:i2)] <- M.aligned else specMat$int[ Selected,c(i1:i2)] <- M.aligned

   return(specMat)
}

#------------------------------
# PTW : Alignment of the selected PPM ranges
#------------------------------
RWarp1D <- function(specMat, zone, idxSref=0, warpcrit=c("WCC","RMS"), Selected=NULL)
{
   i1 <- ifelse( max(zone)>=specMat$ppm_max, 1, length(which(specMat$ppm>max(zone))) )
   i2 <- ifelse( min(zone)<=specMat$ppm_min, specMat$size - 1, which(specMat$ppm<=min(zone))[1] )

   if( is.null(Selected)) M<-specMat$int[, c(i1:i2) ] else  M<-specMat$int[Selected, c(i1:i2) ];
   if (is.null(Selected)) nspec <- specMat$nspec else nspec <- length(Selected);

   if (idxSref==0 || ( !is.null(Selected) && !(idxSref %in% Selected) )) {
      V      <- bestref(M, optim.crit=warpcrit)
      refid  <- V$best.ref
   } else {
     refid <- idxSref
   }

   ref    <- M[refid, ]
   ssampl <- M[ c(1:nspec)[-refid], ]
   #out    <- ptw(ref, ssampl, warp.type = "individual", mode = "forward", optim.crit=warpcrit)
   out    <- ptw(ref, ssampl, warp.type = "global", mode = "forward", init.coef = c(0, 1, 0), optim.crit=warpcrit)
   M      <- out$warped.sample
   M[is.na(M)] <- 0
   if( is.null(Selected)) specMat$int[ c(1:nspec)[-refid],c(i1:i2)] <- M else specMat$int[ Selected[-refid],c(i1:i2)] <- M

   return(specMat)
}

#------------------------------
# Shift of the selected PPM ranges
#------------------------------
RShift1D <- function(specMat, zone, RELDECAL=0, Selected=NULL)
{
   i1 <- ifelse( max(zone)>=specMat$ppm_max, 1, length(which(specMat$ppm>max(zone))) )
   i2 <- ifelse( min(zone)<=specMat$ppm_min, specMat$size - 1, which(specMat$ppm<=min(zone))[1] )
   di <- round(RELDECAL / specMat$dppm,0);
   j1 <- i1 - di
   j2 <- i2 - di

   if( is.null(Selected) ) {
        M <- specMat$int[, c(i1:i2) ]
        specMat$int[, c(i1:i2) ] <- 0
        specMat$int[, c(j1:j2) ] <- M
   } else  {
        M <- specMat$int[Selected, c(i1:i2) ]
        specMat$int[Selected, c(i1:i2) ] <- 0
        specMat$int[Selected, c(j1:j2) ] <- M
   }

   return(specMat)
}

#------------------------------
# Bucket : Apply the bucketing based on the 'Algo' algorithm with the resolution 'resol'.
# Then elinate buckets with a SNR under the threshold given by 'snr'
#------------------------------
RBucket1D <- function(specMat, Algo, resol, snr, zones, zonenoise, LOGFILE=NULL, ProgressFile=NULL)
{
   BUCKET_LIST  <- 'bucket_list.in'
   BUC.filename <- 'SpecBuckets.txt'
   BUC.cmd <- 'SpecBucCmd.lst'

   # Limit size of buckets
   MAXBUCKETS<-2000
   NOISE_FAC <- 3

   if (Algo %in% c('aibin','unif')) {
      # Noise estimation
      if (is.na(zonenoise)) {
          PPM_NOISE_AREA <- c(10.2, 10.5)
      } else {
         PPM_NOISE_AREA <- c(min(zonenoise), max(zonenoise))
      }
      idx_Noise <- c( length(which(specMat$ppm>PPM_NOISE_AREA[2])),(which(specMat$ppm<=PPM_NOISE_AREA[1])[1]) )
      Vref <- spec_ref(specMat$int)
      ynoise <- C_noise_estimation(Vref,idx_Noise[1],idx_Noise[2])
      Vnoise <- abs( C_noise_estimate(specMat$int, idx_Noise[1],idx_Noise[2], 1) )
      
      bdata <- list()
      bdata$ynoise <- ynoise
      bdata$vnoise <- NULL
      bdata$inoise_start <- idx_Noise[1]
      bdata$inoise_end <- idx_Noise[2]
      bdata$R <- resol
      bdata$dppm <- specMat$dppm
      bdata$noise_fac <- NOISE_FAC
      bdata$bin_fac <- 0.5
      bdata$peaknoise_rate <- 15
      bdata$BUCMIN <- 0.003
      bdata$VREF <- 1

      Write.LOG(BUC.cmd,sprintf("bucket %s %f %f %f %f",Algo,PPM_NOISE_AREA[1],PPM_NOISE_AREA[2], resol, snr), mode="at")
   }

   if (Algo=='vsb') {
      Write.LOG(BUC.cmd,sprintf("bucket %s",Algo), mode="at")
   }

   # For each PPM range
   buckets_zones <- NULL
   N <- dim(zones)[1]
   if( !is.null(ProgressFile) ) init_counter(ProgressFile, N)
   buckets_zones <- foreach(i=1:N, .combine=rbind) %dopar% {
       Write.LOG(BUC.cmd,paste(min(zones[i,]), max(zones[i,])), mode="at")
       i2<-which(specMat$ppm<=min(zones[i,]))[1]
       i1<-length(which(specMat$ppm>max(zones[i,])))
       if (Algo=='aibin') {
          Mbuc <- matrix(, nrow = MAXBUCKETS, ncol = 2)
          Mbuc[] <- 0
          buckets_m <- C_aibin_buckets(specMat$int, Mbuc, Vref, bdata, i1, i2)
       }
       if (Algo=='unif') {
          seq_buc <- seq(i1, i2, round(resol/specMat$dppm))
          n_bucs <- length(seq_buc) - 1
          buckets_m <- cbind ( seq_buc[1:n_bucs], seq_buc[2:(n_bucs+1)])
       }
       if (Algo=='vsb') {
          buckets_m <- matrix( c( i1, i2 ), nrow=1, ncol=2, byrow=T )
       }

       if( !is.null(LOGFILE) ) Write.LOG(LOGFILE,paste("Rnmr1D:     Zone",i,"= (",min(zones[i,]),",",max(zones[i,]),"), Nb Buckets =",dim(buckets_m)[1]))
       if( !is.null(ProgressFile) ) inc_counter(ProgressFile, i)
       # Keep only the buckets for which the SNR average is greater than 'snr'
       if (dim(buckets_m)[1]>1) {
          MaxVals <- C_maxval_buckets (specMat$int, buckets_m)
          buckets_m <- buckets_m[ which( apply(t(MaxVals/(2*Vnoise)),1,quantile)[4,]>snr), ]
       }
       cbind( specMat$ppm[buckets_m[,1]], specMat$ppm[buckets_m[,2]] )
   }
   Write.LOG(BUC.cmd,"EOL\n", mode="at")

   if( !is.null(LOGFILE) ) Write.LOG(LOGFILE,paste("Rnmr1D:     Total Buckets =",dim(buckets_zones)[1]))

   if( buckets_zones[1,1]>buckets_zones[1,2] )  {  colnames(buckets_zones) <- c('max','min') }
                                          else  {  colnames(buckets_zones) <- c('min','max') }
   f_append <- ifelse (file.exists(BUC.filename), TRUE, FALSE )

   # The bucket zones files
   write.table(buckets_zones, file=BUC.filename, append=f_append, sep="\t", row.names=F, col.names=!f_append, quote=F)
   buclist <- cbind( 0.5*(buckets_zones[,1]+buckets_zones[,2]), abs(buckets_zones[,2]-buckets_zones[,1]) )
   write.table(buclist, file=BUCKET_LIST, append=f_append, sep="\t", row.names=F, col.names=F, quote=F)
   gc()
}

#------------------------------
# Check if the macro-command file (CMD.filename) is compliant with the allowed commands
#------------------------------
check_MacroCmdFile <- function(CMD.filename) {
   ret <- 1
   allowKW <- c( 'align', 'warp', 'clupa', 'shift', 'gbaseline', 'baseline', 'qnmrbline', 'airpls', 'binning', 'calibration', 'normalisation', 'denoising', 'bucket', 'zero', 'EOL' )

   tryCatch({
      # Read the macrocommand file
      CMDTEXT <- gsub("\t", "", readLines(CMD.filename))
      CMDTEXT <- CMDTEXT[ grep( "^[^ ]", CMDTEXT ) ]
      CMD <- CMDTEXT[ grep( "^[^#]", CMDTEXT ) ]
      CMD <- gsub("^ ", "", gsub(" $", "", gsub(" +", ";", CMD)))
      L <- unique(sort(gsub(";.*$","", CMD)))
      L <- L[ grep( "^[^0-9]", L)]
      ret <- ifelse( sum(L %in% allowKW)==length(L), 1, 0 )
   }, error=function(e) {
       ret <- 0
   })
   return(ret)
}

# ------------------------------------
# Process the Macro-commands file
# ------------------------------------
RProcCMD1D <- function(specMat, specParamsDF, CMDTEXT, NCPU=1, LOGFILE=NULL, ProgressFile=NULL)
{
   lbALIGN <- 'align'
   lbGBASELINE <- 'gbaseline'
   lbBASELINE <- 'baseline'
   lbQNMRBL <- 'qnmrbline'
   lbAIRPLS <- 'airpls'
   lbBIN <- 'binning'
   lbCALIB <- 'calibration'
   lbNORM <- 'normalisation'
   lbFILTER <- 'denoising'
   lbWARP <- 'warp'
   lbCLUPA <- 'clupa'
   lbSHIFT <- 'shift'
   lbBUCKET <- 'bucket'
   lbZERO <- 'zero'
   EOL <- 'EOL'

   SI <- (as.list(specParamsDF[1,]))$SI

   CMDTEXT <- CMDTEXT[ grep( "^[^ ]", CMDTEXT ) ]
   CMD <- CMDTEXT[ grep( "^[^#]", CMDTEXT ) ]
   CMD <- gsub("^ ", "", gsub(" $", "", gsub(" +", ";", CMD)))

   samples <- read.table( 'samples.csv', header=F, sep=";", stringsAsFactors=FALSE)

   specMat$fWriteSpec <- FALSE
   
   while ( length(CMD)>0 && CMD[1] != EOL ) {
   
      cmdLine <- CMD[1]
      cmdPars <- unlist(strsplit(cmdLine[1],";"))
      cmdName <- cmdPars[1]

      repeat {
          if (cmdName == lbCALIB) {
              params <- as.numeric(cmdPars[-1])
              if (length(params)>=3) {
                 PPMRANGE <- c( min(params[1:2]), max(params[1:2]) )
                 PPMREF <- params[3]
                 PPM_NOISE <- ifelse( length(params)==5, c( min(params[4:5]), max(params[4:5]) ), c( 10.2, 10.5 ) )
                 Write.LOG(LOGFILE, paste0("Rnmr1D:  Calibration: PPM REF =",PPMREF,", Zone Ref = (",PPMRANGE[1],",",PPMRANGE[2],")"));
                 specMat <- RCalib1D(specMat, PPM_NOISE, PPMRANGE, PPMREF)
                 specMat$fWriteSpec <- TRUE
                 CMD <- CMD[-1]
              }
              break
          }
          if (cmdName == lbNORM) {
              params <- cmdPars[-1]
              if (length(params)==2) {
                 params <- as.numeric(params)
                 PPMRANGE <- c( min(params[1:2]), max(params[1:2]) )
                 Write.LOG(LOGFILE,paste0("Rnmr1D:  Normalisation: Zone Ref = (",PPMRANGE[1],",",PPMRANGE[2],")"));
                 specMat <- RNorm1D(specMat, normmeth='CSN', zones=matrix(PPMRANGE,nrow=1, ncol=2))
                 specMat$fWriteSpec <- TRUE
                 CMD <- CMD[-1]
              }
              if (length(params)==1) {
                 NORM_METH <- params[1]
                 CMD <- CMD[-1]
                 zones <- NULL
                 while(CMD[1] != EOL) {
                    zones <- rbind(zones, as.numeric(unlist(strsplit(CMD[1],";"))))
                    CMD <- CMD[-1]
                 }
                 Write.LOG(LOGFILE,"Rnmr1D:  Normalisation of the Intensities based on the selected PPM ranges...")
                 Write.LOG(LOGFILE,paste0("Rnmr1D:     Method =",NORM_METH))
                 specMat <- RNorm1D(specMat, normmeth=NORM_METH, zones=zones)
                 specMat$fWriteSpec <- TRUE
                 CMD <- CMD[-1]
              }
              break
          }
          if (cmdName == lbGBASELINE || cmdName == lbBASELINE) {
              params <- as.numeric(cmdPars[-1])
              if (length(params)==6) {
                 PPM_NOISE <- c( min(params[1:2]), max(params[1:2]) )
                 PPMRANGE <- c( min(params[3:4]), max(params[3:4]) )
                 Write.LOG(LOGFILE,paste0("Rnmr1D:  Baseline Correction: PPM Range = ( ",min(PPMRANGE)," , ",max(PPMRANGE)," )"));
                 registerDoParallel(cores=NCPU)
                 if (cmdName == lbBASELINE) {
                     if (params[5]>0) { # to be compliant with version<1.1.4
                        BCMETH <- params[5]
                        WSFAC <- (7-params[6])/4
                        WINDOWSIZE <- round(WSFAC*SI/(BCMETH*64))
                     } else {
                        WINDOWSIZE <- round(( 1/2^(params[6]-2) )*(SI/64))
                     }
                     Write.LOG(LOGFILE,paste0("Rnmr1D:     Type=Local - Window Size = ",WINDOWSIZE));
                     specMat <- RBaseline1D(specMat,PPM_NOISE, PPMRANGE, WINDOWSIZE, ProgressFile=ProgressFile)
                 } else {
                     WS <- params[5]
                     NEIGH <- params[6]
                     Write.LOG(LOGFILE,paste0("Rnmr1D:     Type=Global - Smoothing Parameter=",WS," - Window Size=",NEIGH));
                     specMat <- RGbaseline1D(specMat,PPM_NOISE, PPMRANGE, WS, NEIGH, ProgressFile=ProgressFile)
                 }
                 specMat$fWriteSpec <- TRUE
                 CMD <- CMD[-1]
              }
              break
          }
          if (cmdName == lbQNMRBL) {
              params <- as.numeric(cmdPars[-1])
              if (length(params)==4) {
                 PPM_NOISE <- c( min(params[1:2]), max(params[1:2]) )
                 PPMRANGE <- c( min(params[3:4]), max(params[3:4]) )
                 Write.LOG(LOGFILE,paste0("Rnmr1D:  Baseline Correction: PPM Range = ( ",min(PPMRANGE)," , ",max(PPMRANGE)," )"))
                 Write.LOG(LOGFILE,paste0("Rnmr1D:     Type=q-NMR"))
                 registerDoParallel(cores=NCPU)
                 specMat <- Rqnmrbc1D(specMat,PPM_NOISE, PPMRANGE, ProgressFile=ProgressFile)
                 specMat$fWriteSpec <- TRUE
                 CMD <- CMD[-1]
              }
              break
          }
          if (cmdName == lbAIRPLS) {
              params <- as.numeric(cmdPars[-1])
              if (length(params)==3) {
                 PPMRANGE <- c( min(params[1:2]), max(params[1:2]) )
                 LAMBDA <- params[3]
                 Write.LOG(LOGFILE,paste0("Rnmr1D:  Baseline Correction: PPM Range = ( ",min(PPMRANGE)," , ",max(PPMRANGE)," )"))
                 Write.LOG(LOGFILE,paste("Rnmr1D:     Type=airPLS, lambda=",LAMBDA))
                 registerDoParallel(cores=NCPU)
                 specMat <- RairPLSbc1D(specMat, PPMRANGE, LAMBDA, ProgressFile=ProgressFile)
                 specMat$fWriteSpec <- TRUE
                 CMD <- CMD[-1]
              }
              break
          }

          if (cmdName == lbFILTER) {
              params <- as.numeric(cmdPars[-1])
              if (length(params)==4) {
                 PPMRANGE <- c( min(params[1:2]), max(params[1:2]) )
                 FORDER <- params[3]
                 FLENGTH <- params[4]
                 Write.LOG(LOGFILE,paste0("Rnmr1D:  Denoising: PPM Range = ( ",min(PPMRANGE)," , ",max(PPMRANGE)," )"));
                 Write.LOG(LOGFILE,paste0("Rnmr1D:     Filter Order=",FORDER," - Filter Length=",FLENGTH));
                 registerDoParallel(cores=NCPU)
                 specMat <- RFilter1D(specMat,PPMRANGE, FORDER, FLENGTH, ProgressFile=ProgressFile)
                 specMat$fWriteSpec <- TRUE
                 CMD <- CMD[-1]
              }
              break
          }
          if (cmdName == lbALIGN) {
              params <- as.numeric(cmdPars[-1])
              Selected <- NULL
              if (length(params)==6 && (params[5]<2 || params[6])) {
                 level <- unique(samples[ order(as.character(samples[, params[5]+1])), params[5]+1 ])[params[6]]
                 Selected <- .N(rownames(samples[ samples[, params[5]+1]==level, ]))
              }
              if (length(params)>=4) {
                 PPMRANGE <- c( min(params[1:2]), max(params[1:2]) )
                 RELDECAL= params[3]
                 idxSref=params[4]
                 Write.LOG(LOGFILE,paste0("Rnmr1D:  Alignment: PPM Range = ( ",min(PPMRANGE)," , ",max(PPMRANGE)," )"))
                 Write.LOG(LOGFILE,paste0("Rnmr1D:     Rel. Shift Max.=",RELDECAL," - Reference=",idxSref))
                 specMat <- RAlign1D(specMat, PPMRANGE, RELDECAL, idxSref, Selected=Selected, ProgressFile=ProgressFile)
                 specMat$fWriteSpec <- TRUE
                 CMD <- CMD[-1]
              }
              break
          }
          if (cmdName == lbWARP) {
              params <- cmdPars[-1]
              Selected <- NULL
              if (length(params)==6 && (.N(params[5])<2 || .N(params[6]))) {
                 level <- unique(samples[ order(as.character(samples[, .N(params)[5]+1])), .N(params)[5]+1 ])[.N(params[6])]
                 Selected <- .N(rownames(samples[ samples[, .N(params[5])+1]==level, ]))
              }
              if (length(params)>=4) {
                 PPMRANGE <- c( min(.N(params[1:2])), max(.N(params[1:2])) )
                 idxSref=.N(params[3])
                 warpcrit=.C(params[4])
                 Write.LOG(LOGFILE,paste0("Rnmr1D:  Alignment: PPM Range = ( ",min(PPMRANGE)," , ",max(PPMRANGE)," )"))
                 Write.LOG(LOGFILE,paste0("Rnmr1D:  Parametric Time Warping Method - Reference=",idxSref," - Optim. Crit=",warpcrit))
                 specMat <- RWarp1D(specMat, PPMRANGE, idxSref, warpcrit, Selected=Selected)
                 specMat$fWriteSpec <- TRUE
                 CMD <- CMD[-1]
              }
              break
          }
          if (cmdName == lbCLUPA) {
              params <- as.numeric(cmdPars[-1])
              Selected <- NULL
              if (length(params)==9 && (params[8]<2 || params[9])) {
                 level <- unique(samples[ order(as.character(samples[, params[8]+1])), params[8]+1 ])[params[9]]
                 Selected <- .N(rownames(samples[ samples[, params[8]+1]==level, ]))
              }
              if (length(params)>=7) {
                 PPM_NOISE <- c( min(params[1:2]), max(params[1:2]) )
                 PPMRANGE <- c( min(params[3:4]), max(params[3:4]) )
                 RESOL= params[5]
                 SNR= params[6]
                 idxSref=params[7]
                 Write.LOG(LOGFILE,paste0("Rnmr1D:  Alignment: PPM Range = ( ",min(PPMRANGE)," , ",max(PPMRANGE)," )"))
                 Write.LOG(LOGFILE,paste0("Rnmr1D:     CluPA - Resolution =",RESOL," - SNR threshold=",SNR, " - Reference=",idxSref))
                 specMat <- RCluPA1D(specMat, PPM_NOISE, PPMRANGE, RESOL, SNR, idxSref, Selected=Selected, ProgressFile=ProgressFile)
                 specMat$fWriteSpec <- TRUE
                 CMD <- CMD[-1]
              }
              break
          }
          if (cmdName == lbSHIFT) {
              params <- as.numeric(cmdPars[-1])
              Selected <- NULL
              if (length(params)==5 && (params[4]<2 || params[5])) {
                 level <- unique(samples[ order(as.character(samples[, params[4]+1])), params[4]+1 ])[params[5]]
                 Selected <- .N(rownames(samples[ samples[, params[4]+1]==level, ]))
              }
              if (length(params)>=3) {
                 PPMRANGE <- c( min(params[1:2]), max(params[1:2]) )
                 RELDECAL= params[3]
                 Write.LOG(LOGFILE,paste0("Rnmr1D:  Shift: PPM Range = ( ",min(PPMRANGE)," , ",max(PPMRANGE)," )"))
                 Write.LOG(LOGFILE,paste0("Rnmr1D:     Shift value =",RELDECAL))
                 specMat <- RShift1D(specMat, PPMRANGE, RELDECAL, Selected=Selected)
                 specMat$fWriteSpec <- TRUE
                 CMD <- CMD[-1]
              }
              break
          }
          if (cmdName == lbZERO) {
              CMD <- CMD[-1]
              zones2 <- NULL
              while(CMD[1] != EOL) {
                  zones2 <- rbind(zones2, as.numeric(unlist(strsplit(CMD[1],";"))))
                  CMD <- CMD[-1]
              }
              Write.LOG(LOGFILE,"Rnmr1D:  Zeroing the selected PPM ranges ...")
              specMat <- RZero1D(specMat, zones2, LOGFILE=LOGFILE, ProgressFile=ProgressFile)
              specMat$fWriteSpec <- TRUE
              CMD <- CMD[-1]
              break
          }
          if (cmdName == lbBUCKET) {
              if ( !( length(cmdPars) >= 6 && cmdPars[2] %in% c('aibin','unif') ) &&
                   !( length(cmdPars) == 2 && cmdPars[2] %in% c('vsb') ) ) {
                 CMD <- CMD[-1]
                 break;
              }
              Write.LOG(LOGFILE,"Rnmr1D: \nRnmr1D:  Bucketing the selected PPM ranges ...")
              CMD <- CMD[-1]
              zones <- NULL
              while(CMD[1] != EOL) {
                  zones <- rbind(zones, as.numeric(unlist(strsplit(CMD[1],";"))))
                  CMD <- CMD[-1]
              }
              if ( cmdPars[2] %in% c('aibin','unif') ) {
                  params <- as.numeric(cmdPars[-c(1:2)])
                  PPM_NOISE <- c( min(params[1:2]), max(params[1:2]) )
                  resol <- params[3]; snr <- params[4];
                  Write.LOG(LOGFILE,paste0("Rnmr1D:     ",toupper(cmdPars[2])," - Resolution =",resol," - SNR threshold=",snr))
              } else {
                  PPM_NOISE <- NULL
                  resol <- 0; snr <- 0;
                  Write.LOG(LOGFILE,paste0("Rnmr1D:     ",toupper(cmdPars[2])))
              }
              registerDoParallel(cores=NCPU)
              RBucket1D(specMat, cmdPars[2], resol, snr, zones, PPM_NOISE, LOGFILE=LOGFILE, ProgressFile=ProgressFile)
              CMD <- CMD[-1]
              break
          }
          CMD <- CMD[-1]
          break
      }
      gc()
   }
   return(specMat)
}

#----
# Generates the buckets table
#----
get_Buckets_table <- function(bucketfile)
{
   outtable <- NULL
   if ( file.exists(bucketfile) ) {
      # Read the buckets
      buckets <- read.table(bucketfile, header=F, sep="\t",stringsAsFactors=FALSE)
      colnames(buckets) <- c("center", "width")
      buckets$name <- gsub("^(\\d+)","B\\1", gsub("\\.", "_", gsub(" ", "", sprintf("%7.4f",buckets[,1]))) )
      buckets$min <- buckets[,1]-0.5*buckets[,2]
      buckets$max <- buckets[,1]+0.5*buckets[,2]
     
      outtable <- buckets[, c("name", "center", "min", "max", "width") ]
   }
   return(outtable)
}

#----
# Generates the buckets data set
#----
get_Buckets_dataset <- function(specMat, bucketfile, norm_meth='CSN', zoneref=NA)
{
   outdata <- NULL
   if ( file.exists(bucketfile) ) {
      # Read the buckets
      buckets <- read.table(bucketfile, header=F, sep="\t",stringsAsFactors=FALSE)
      buckets$min <- buckets[,1]-0.5*abs(buckets[,2])
      buckets$max <- buckets[,1]+0.5*abs(buckets[,2])
      # get index of buckets' ranges
      buckets_m <- t(simplify2array(lapply( c( 1:(dim(buckets)[1]) ), 
                     function(x) { c( length(which(specMat$ppm>buckets[x,]$max)), length(which(specMat$ppm>buckets[x,]$min)) ) }
                    )))
      # Integration
      buckets_IntVal <- C_all_buckets_integrate (specMat$int, buckets_m, 0)
      buckets_IntVal_CSN <- C_buckets_CSN_normalize( buckets_IntVal )
      if (norm_meth == 'CSN') {
          buckets_IntVal <- buckets_IntVal_CSN
      }
      if (norm_meth == 'PQN') {
          bucVref_IntVal <- C_MedianSpec(buckets_IntVal_CSN)
          bucRatio <- buckets_IntVal_CSN / bucVref_IntVal
          Coeff <- apply(bucRatio,1,median)
          buckets_IntVal <- buckets_IntVal_CSN / Coeff
      }
      # if supplied, integrate of all spectra within the PPM range of the reference signal
      Vref <- 0*c(1:specMat$nspec)
      if (! is.na(zoneref)) {
          istart <- length(which(specMat$ppm>max(zoneref)))
          iend <- length(which(specMat$ppm>min(zoneref)))
          Vref <- C_spectra_integrate (specMat$int, istart, iend)
          buckets_IntVal <- buckets_IntVal/Vref
      }
      # read samples
      samplesFile <- file.path(dirname(bucketfile),'samples.csv')
      samples <- read.table(samplesFile, header=F, sep=";", stringsAsFactors=FALSE)
      # read factors
      factorsFile <- file.path(dirname(bucketfile),"factors")
      factors <- read.table(factorsFile, header=F, sep=";", stringsAsFactors=FALSE)
      # write the data table
      bucnames <- gsub("^(\\d+)","B\\1", gsub("\\.", "_", gsub(" ", "", sprintf("%7.4f",buckets[,1]))) )
      outdata <- cbind( samples[, -1], buckets_IntVal )
      colnames(outdata) <- c( factors[,2], bucnames )
   }
   return(outdata)
}

#----
# Generates the SNR dataset
#----
get_SNR_dataset <- function(specMat, bucketfile, zone_noise, ratio=TRUE)
{
   outdata <- NULL
   if ( file.exists(bucketfile) ) {
      # read samples
      samplesFile <- file.path(dirname(bucketfile),'samples.csv')
      samples <- read.table(samplesFile, header=F, sep=";", stringsAsFactors=FALSE)
      # read factors
      factorsFile <- file.path(dirname(bucketfile),"factors")
      factors <- read.table(factorsFile, header=F, sep=";", stringsAsFactors=FALSE)
      # Read the buckets
      buckets <- read.table(bucketfile, header=F, sep="\t",stringsAsFactors=FALSE)
      buckets$min <- buckets[,1]-0.5*buckets[,2]
      buckets$max <- buckets[,1]+0.5*buckets[,2]
      # get index of buckets' ranges
      buckets_m <- t(simplify2array(lapply( c( 1:(dim(buckets)[1]) ), 
                     function(x) { c( length(which(specMat$ppm>buckets[x,]$max)), length(which(specMat$ppm>buckets[x,]$min)) ) }
                    )))
      # Compute Vnoise vector & Maxvals maxtrix
      i1 <- ifelse( max(zone_noise)>=specMat$ppm_max, 1, length(which(specMat$ppm>max(zone_noise))) )
      i2 <- ifelse( min(zone_noise)<=specMat$ppm_min, specMat$size - 1, which(specMat$ppm<=min(zone_noise))[1] )
      flg <- 1
      Vnoise <- abs( C_noise_estimate(specMat$int, i1, i2, flg) )
      MaxVals <- C_maxval_buckets (specMat$int, buckets_m)
      # write the data table
      bucnames <- gsub("^(\\d+)","B\\1", gsub("\\.", "_", gsub(" ", "", sprintf("%7.4f",buckets[,1]))) )
      if (ratio) {
         outdata <- cbind( samples[, -1], MaxVals/(2*Vnoise))
         colnames(outdata) <- c( factors[,2], bucnames )
      } else {
         outdata <- cbind( samples[, -1], Vnoise, MaxVals )
         colnames(outdata) <- c( factors[,2], 'Noise', bucnames )
      }
      outdata <- data.frame(outdata, stringsAsFactors=FALSE)
   }
   return(outdata)
}


#----
# Generates the Spectra Data
#----
get_Spectra_Data <- function(specMat,samplesFile)
{
   # read samples
   samples <- read.table(samplesFile, header=F, sep=";", stringsAsFactors=FALSE)
   outdata <- cbind( specMat$ppm, t(specMat$int) )
   colnames(outdata) <- c( "ppm", samples[,1] )
   return(outdata)
}
