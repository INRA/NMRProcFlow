# source("C:/Workdir/Metabolomic/NMRProcFlow/dockerApps/nmrproc/dev/parseCMD.R")

lbALIGN <- 'align'
lbBASELINE <- 'baseline'
lbBIN <- 'binning'
lbCALIB <- 'calibration'
lbZERO <- 'zero'
EOL <- 'EOL'

procNames <- c(lbALIGN, lbBASELINE, lbBIN, lbCALIB, lbZERO )

#datadir <- "C:/Workdir/Share/tmp/8e21bc463ae709284211a1012444ca"
#datadir <- "C:/Workdir/Share/tmp/92da814bf8c03b35c24732c10e231c"
datadir <- "C:/Workdir/Metabolomic/NMRProcFlow/DATA/Tlse_BrainMice"

cmdfile <- file.path(datadir, "SpecProcCmd.txt")

CMDTEXT <- readLines(cmdfile)
CMDTEXT <- CMDTEXT[ grep( "^[^ ]", CMDTEXT ) ]
CMD <- CMDTEXT[ grep( "^[^#]", CMDTEXT ) ]
CMD <- gsub("^ ", "", gsub(" $", "", gsub(" +", ";", CMD)))

while ( length(CMD)>0 && CMD[1] != EOL ) {

   cmdLine <- CMD[1]
   cmdPars <- unlist(strsplit(cmdLine[1],";"))
   cmdName <- cmdPars[1]

   repeat {
       if (cmdName == lbCALIB) {
           params <- as.numeric(cmdPars[-1])
           if (length(params)==3) {
              PPMRANGE <- c( min(params[1:2]), max(params[1:2]) )
              PPMREF <- params[3]
              cat("Ok Calibration: ",paste(PPMRANGE,collapse=" "),PPMREF,"\n")
              CMD <- CMD[-1]
           }
           break
       }
       if (cmdName == lbBASELINE) {
           params <- as.numeric(cmdPars[-1])
           if (length(params)==6) {
              PPM_NOISE <- c( min(params[1:2]), max(params[1:2]) )
              PPMRANGE <- c( min(params[3:4]), max(params[3:4]) )
              BCTYPE <- params[5]
              WSFAC <- params[6]
              cat("Ok Baseline: ",paste(PPM_NOISE,collapse=" "),paste(PPMRANGE,collapse=" "),BCTYPE,WSFAC,"\n")
              CMD <- CMD[-1]
           }
           break
       }
       if (cmdName == lbALIGN) {
           CMD <- CMD[-1]
           zones1 <- NULL
           while(CMD[1] != EOL) {
               zones1 <- rbind(zones1, as.numeric(unlist(strsplit(CMD[1],";"))))
               CMD <- CMD[-1]
           }
           CMD <- CMD[-1]
           cat("Ok Alignment: ",dim(zones1)[1]," zones\n")
           break
       }
       if (cmdName == lbZERO) {
           CMD <- CMD[-1]
           zones2 <- NULL
           while(CMD[1] != EOL) {
               zones2 <- rbind(zones2, as.numeric(unlist(strsplit(CMD[1],";"))))
               CMD <- CMD[-1]
           }
           CMD <- CMD[-1]
           cat("Ok Zeroing: ",dim(zones2)[1]," zones\n")
           break
       }
       break
   }

}

