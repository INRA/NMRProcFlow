outDataViewer <- "C:/Workdir/Share/tmp/081aaeae80db3a978c62dfa1"
outDir <- "C:/Workdir/Share/tmp/a74a35ddafb4ef308cd6effa"
SampleFile <<- file.path(outDir,'samples.txt')

write_textlines <- function (filetext, msg) {
   fh<-file(filetext,"at")
   writeLines(msg, fh)
   close(fh)
}

generate_metadata_file <- function() {

   #write_textlines(file.path(outDataViewer,conf$LOGFILE),"Generate the 'samples.csv' & 'factors' files from the list of raw spectra\n")
   write_textlines(file.path(outDataViewer,'info.log'),"Generate the 'samples.csv' & 'factors' files from the list of raw spectra\n")
   lstfac <- c("1;Samplecode")
   LIST <- list.files(path = outDir, pattern = "fid$", all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE)
   RAW <- unique(gsub( "/[0-9]+/fid" , "", LIST))
   V <- gsub( ".*/.+/", "", RAW )
   rawdir <- dirname(RAW[1])
   nbcol <- 1
   m_samples <- NULL
   NbFactors <<- 0

   OKRAW <- 1
   ERRORLIST <- c()

   repeat {

      # if a file  of samples was uploaded along with the ZIP file
      if (! is.null(SampleFile)) {
         samples <- read.table(file.path(outDir,'samples.txt'), sep="\t", header=T)
         m_samples <- as.matrix(samples)
         samplesize <- dim(m_samples)
         RAW <- sapply( c(1:(dim(m_samples)[1])), function(x){ paste(rawdir, m_samples[x,1], sep='/'); })
         V <- m_samples[, 1]
         listfid <- sapply( c(1:(dim(m_samples)[1])), function(x){ paste(rawdir, m_samples[x,1], m_samples[x,3], 'fid', sep='/'); })
         ERRORLIST <- gsub(rawdir, "", listfid[ ! sapply( listfid, file.exists ) ])
         OKRAW <- ifelse (length(ERRORLIST)>0, 0, 1 )
         if (OKRAW==0) break
         nbcol <- samplesize[2]
      }

      # if there is no information regarding both 'expno' and 'procno',
      # take the smallest expno and procno identifiers
      if (is.null(SampleFile) || nbcol<3) {
         expno <- sapply( c(1:length(RAW)), function(x) {
                L <- list.files(path = RAW[x], pattern = "fid$", all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE) 
                min( as.numeric(gsub( "/", "", dirname(gsub( RAW[x], "", L))))) 
         })
         RAW2 <- paste(RAW,expno,sep='/')
         procno <- sapply( c(1:length(RAW2)), function(x) {
                L <- list.files(path = RAW2[x], pattern = "1r$", all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE)
                min(as.numeric(dirname(gsub ( ".*/pdata/", "" ,L))))
         })
         M <- NULL
         if (nbcol==1) { M <- cbind(V,V); } else { M <- cbind(V, m_samples[,2]); }
         IDS <- cbind(RAW, expno, procno)
         break
      }
      
      # if there is some information regarding 'expno' and 'procno',
      # check if it is consistent with the raw data within the provided ZIP file
      if (! is.null(SampleFile) && nbcol>2 ) {
          RAW <- sapply( c(1:(dim(m_samples)[1])), function(x){ paste(rawdir, m_samples[x,1], sep='/'); })
          list1r <- sapply( c(1:(dim(m_samples)[1])), function(x){ paste(rawdir, m_samples[x,1], m_samples[x,3], 'pdata', m_samples[x,4], '1r', sep='/'); })
          ERRORLIST <- gsub(rawdir, "", list1r[ ! sapply( list1r, file.exists ) ])
          OKRAW <- ifelse (length(ERRORLIST)>0, 0, 1 )
          if (OKRAW==0) break
          if (nbcol==4) {
             M <- cbind(m_samples[,1], m_samples[,2]);
             IDS <- cbind(RAW, m_samples[,3], m_samples[,4])
             break
          }
          if (nbcol>4) {
             M <- m_samples[,c(-3:-4)]
             IDS <- cbind(RAW, m_samples[,3], m_samples[,4])
             lstfac <- c( lstfac, paste(c(2:(nbcol-3)), colnames(m_samples)[c(-1:-4)], sep=";") )
             NbFactors <<- nbcol - 4
             break
          }
      }

      break
   }
   if (OKRAW==1) {
      samplefile <- file.path(outDataViewer,'samples.csv')
      write.table(M, file=samplefile, sep=';', row.names=F, col.names=F, quote=F)
      
      rawidsfile <- file.path(outDataViewer,'rawids.csv')
      write.table(IDS, file=rawidsfile, sep=';', row.names=F, col.names=F, quote=F)
      
      factorsfile <- file.path(outDataViewer,'factors')
      fileConn<-file(factorsfile)
      writeLines(lstfac, fileConn)
      close(fileConn)
   } else {
      write_textlines(file.path(outDataViewer,'info.log'),"ERROR: Some information regarding raw data are not consistent; Here is the list below:\n")
      write_textlines(file.path(outDataViewer,'info.log'),paste0(paste(ERRORLIST, collapse="\n"),"\n"))
   }
   return(OKRAW)

}
