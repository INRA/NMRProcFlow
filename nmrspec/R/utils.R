
zonelist1 <- c('zones2_list.in')
zonelist2 <- c('zones3_list.in', 'zones4_list.in', 'zones5_list.in', 'vsb_list.in')

#----
# Write Text lines 'msg' into file 'filetext'
#----
write_textlines <- function (filetext, msg, mode="at")
{
   fh<-file(filetext,mode)
   writeLines(msg, fh)
   close(fh)
}

#----
# Delete the file 'thefile'
#----
delete_file <- function(thefile)
{
   fullpathfile <- file.path(outDataViewer,thefile)
   if (file.exists(fullpathfile) ) unlink (fullpathfile)
}

delete_list_file <- function(thelistfile)
{
   ret <- sapply(thelistfile, delete_file)
}

#----
# Generate a INI file from the list of processing parameters
# procParams : the list of processing parameters
# INI.file : the INI output file
#----
generate_INI_file <- function(procParams, cores=conf$CORES )
{
   INI.file <- file.path(outDataViewer,conf$Rnmr1D_INI)
   metalist <- list( 'ENV'=list('PPM_MIN'=conf$PPM_MIN, 'PPM_MAX'=conf$PPM_MAX, 
                                'PPM_MIN_13C'=conf$PPM_MIN_13C, 'PPM_MAX_13C'=conf$PPM_MAX_13C, 'CORES'=cores), 
                     'PROCPARAMS'=procParams )
   Write.INI(INI.file, metalist, EXCLU=c( 'LOGFILE' ))
}

#----
# Get the list of processing parameters from the INI file
# procParams : the list of processing parameters
# INI.file : the INI output file
#----
get_INI_params <- function()
{
   INI.file <- file.path(outDataViewer,conf$Rnmr1D_INI)
   procParams <- Parse.INI(INI.filename, INI.list=list(), section="PROCPARAMS")
   procParams
}

#----
# Write the selected Zones List (ppmlist) to a file (zonesfile)
#----
write_selectZonesToFile <- function(ppmlist,zonesfile)
{
   selectZonesFile <- file.path(outDataViewer,zonesfile)
   if (file.exists(selectZonesFile)) unlink(selectZonesFile)
   repeat{
       if ( nchar(ppmlist)<1 ) break
       if ( ! nchar(ppmlist) %in% simplify2array(gregexpr(pattern ="\n",ppmlist)) ) break
       M1 <- t(simplify2array(strsplit(strsplit(ppmlist, "\n")[[1]], " ")))
       if (sum(is.na(as.numeric(M1)))>0) break
       if (dim(M1)[2]!=2) break
       M1 <- as.data.frame(M1)
       M <- cbind( (as.numeric(as.vector(M1$V1)) + as.numeric(as.vector(M1$V2)))/2, abs(as.numeric(as.vector(M1$V1)) - as.numeric(as.vector(M1$V2))))
       write.table(M,  selectZonesFile, sep="\t", row.names=FALSE, col.names=FALSE)
       break
   }
}

#----
# Get the jQuery script line in order to fill the textarea (id) with the values within the file (zonesfile)
#----
getUpdateTextAreaScript <- function( id, outDataViewer, zonesfile)
{
   fileName <- file.path(outDataViewer,zonesfile)
   jscript <- ''
   if ( file.exists(fileName) ) {
       zones <- read.table(fileName, header=F, sep="\t",stringsAsFactors=FALSE)
       str_list_vals <- paste0(sapply( 1:dim(zones)[1], 
                       function(x){ paste0(zones[x,1]+0.5*zones[x,2], " ",zones[x,1]-0.5*zones[x,2],';') }), sep="", collapse="")
       jscript <- paste0( "$('#",id,"').val('",str_list_vals,"'); replace_by_NL('",id,"'); ")
   } else {
       jscript <- paste0( "$('#",id,"').val(''); ")
   }
   return(jscript)
}

#----
# Generates the shell script that :
#  i/ init the semaphores
#  ii/ will launch the R cmdR script, 
# then launches the shell script in background mode (nohup)
#----
submit_Rscript <- function(outDir, outDataViewer, cmdR)
{
   cmdfile <- file.path(outDir,'jobScript.sh')
   pidfile <- file.path(outDataViewer,conf$JobPIDFile)
   LOGFILE <- file.path(outDataViewer,conf$ERRORFILE)

   fh<-file(cmdfile,"wt")
   writeLines("#!/bin/bash\n\n", fh)
   writeLines( paste0('echo "1" > ',file.path(outDataViewer,conf$semapFileIn),"\n"), fh)
   writeLines( paste0(cmdR,"\n"), fh)
   writeLines( paste0('RET=$(echo $?)' ,"\n"), fh)
   writeLines( paste0('echo "1" > ',file.path(outDataViewer,conf$semapFileOut),"\n"), fh)
   #writeLines( paste0('[ -f "',pidfile,'" ] && rm -f ',pidfile,"\n"), fh)
   #writeLines( paste0('[ $RET -ne 0 ] && echo "ERROR" > ', file.path(outDataViewer,conf$semapFileErr),"\n"), fh)
   writeLines("exit $RET\n", fh)
   close(fh)

   delete_file(conf$semapFileIn)
   delete_file(conf$semapFileOut)
   delete_file(conf$semapFileErr)
   delete_file(conf$ProgressFile)

   system( paste("nohup /bin/sh ",cmdfile, " 2>>",LOGFILE," 1>>",LOGFILE,"& echo $! > ",pidfile, sep=""))
}

#----
# Read the specs.pack
#----
get_specMat <- function()
{
   specPackFile <- file.path(outDataViewer,conf$SPEC_PACKED)
   specMat <- readSpecMatrix(specPackFile)
   specMat$dppm <- (specMat$ppm_max - specMat$ppm_min)/(specMat$size - 1)
   specMat$ppm <- rev(seq(from=specMat$ppm_min, to=specMat$ppm_max, by=specMat$dppm))
   return(specMat)
}


#----
# Get the bucket list where SNR < snrlevel
#----
get_Buckets_upperSNR <- function(buckets, outsnr, snrlevel)
{
     bucnames <- gsub("^(\\d+)","B\\1", gsub("\\.", "_", gsub(" ", "", sprintf("%7.4f",buckets[,1]))) )
     if (dim(outsnr)[1]>3) {
        # SNR threshold is appled on the 3rd quartile (1=>0%, 2=>25%, 3=>50%, 4=>75%, 5=>100%)
        SNRavg <- simplify2array(lapply( as.list(outsnr[, bucnames]), function(v) { quantile(as.numeric(v)) } ))[4, ]
        BUCsel <- names(SNRavg[ SNRavg>snrlevel ])
     } else {
        BUCsel <- bucnames
     }
     BUCsel <- BUCsel[grep("\\.1", BUCsel, invert=TRUE)]
     BUCsel
}

#----
# Get the data matrix
#----
get_Data_matrix <- function(outDataViewer, zoneref, zonenoise)
{
     specMat <- get_specMat()
     bucketfile <- file.path(outDataViewer,conf$BUCKET_LIST)
     outDataMat <- NULL
     if ( file.exists(bucketfile) ) {
        outsnr <- get_SNR_dataset(specMat, bucketfile, c(min(zonenoise), max(zonenoise)), ratio=TRUE)
        buckets <- read.table(bucketfile, header=F, sep="\t",stringsAsFactors=FALSE)
        BUCsel <- get_Buckets_upperSNR(buckets, outsnr, input$snrlevel)
        outdata <- get_Buckets_dataset(specMat, bucketfile, input$normmeth, zoneref)
        nbfc <- length(colnames(outdata)) - length(buckets[,1])
        if (specMat$nspec>1) {
           outDataMat <- cbind( outdata[, 1:nbfc ],  outdata[, BUCsel[ grep("\\.2$", BUCsel, invert=TRUE) ] ] )
        } else {
           outDataMat <- cbind( t(outdata[, 1:nbfc ]),  t(outdata[ , colnames(outdata) %in% BUCsel])  )
        }
     }
     outDataMat
}

#----
# Fill in a Workbook based on the 'simple' template (See https://rdrr.io/cran/openxlsx/)
#----
write_simple_wb <- function(wb, outDataViewer, zoneref, zonenoise)
{
     bucketfile <- file.path(outDataViewer,conf$BUCKET_LIST)
     # Buckets table
     addWorksheet(wb = wb, sheetName = "Buckets", gridLines = TRUE)
     # Data matrix
     addWorksheet(wb = wb, sheetName = "Data", gridLines = TRUE)
     # SNR matrix
     addWorksheet(wb = wb, sheetName = "SNR", gridLines = TRUE)
     # Macro_Cmd
     addWorksheet(wb = wb, sheetName = "Macro-CMD", gridLines = TRUE)
     # Styles
     styBH <- createStyle(fgFill = "#0070C0", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")
     styREM <- createStyle(fontColour = "green", borderStyle="none", fgFill="white")
     styBOLD <- createStyle(textDecoration = "Bold", borderStyle="none", fgFill="white")
     if ( file.exists(bucketfile) ) {
         specMat <- get_specMat()
         outsnr  <- get_SNR_dataset(specMat, bucketfile, c(min(zonenoise), max(zonenoise)), ratio=TRUE)
         buckets <- get_Buckets_table(bucketfile)
         datamat <- get_Data_matrix(outDataViewer, zoneref, zonenoise)
         shid <- 1
            writeData(wb, shid, x = buckets, colNames=TRUE, rowNames=FALSE, withFilter = FALSE)
            addStyle(wb, shid, style = styBH, rows = 1, cols = c(1:dim(buckets)[2]), gridExpand = TRUE)
         shid <- shid + 1
            writeData(wb, shid, x = datamat, colNames=TRUE, rowNames=FALSE, withFilter = FALSE)
            addStyle(wb, shid, style = styBH, rows = 1, cols = c(1:dim(datamat)[2]), gridExpand = TRUE)
         shid <- shid + 1
            writeData(wb, shid, x = outsnr,  colNames=TRUE, rowNames=FALSE, withFilter = FALSE)
            addStyle(wb, shid, style = styBH, rows = 1, cols = c(1:dim(outsnr)[2]), gridExpand = TRUE)
         shid <- shid + 1
         if (file.exists(file.path(outDataViewer,conf$Rnmr1D_PCMD))) {
            cmdlines <- readLines(file.path(outDataViewer,conf$Rnmr1D_PCMD))
            writeData(wb, shid, x = cmdlines,  colNames=TRUE, rowNames=FALSE, withFilter = FALSE)
            setColWidths(wb, shid, cols=1, widths=100, ignoreMergedCells = FALSE)
            L <- grep("^#", cmdlines)
            for (i in 1:length(L)) addStyle(wb, shid, style = styREM, rows = L[i], cols = 1, gridExpand = TRUE)
            L <- grep("^[^#]", cmdlines)
            for (i in 1:length(L)) addStyle(wb, shid, style = styBOLD, rows = L[i], cols = 1, gridExpand = TRUE)
         }
     }
}

#----
# Fill in a Workbook based on the 'qhnmr' template (See https://rdrr.io/cran/openxlsx/)
#----
write_qhnmr_wb <- function(wb, outDataViewer, zoneref, zonenoise)
{
     # Styles
     styBH   <- createStyle(fgFill = "#0070C0", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")
     styO    <- createStyle(fgFill = "#ED7D31", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")
     styB    <- createStyle(fgFill = "#8DB4E2")
     styVS   <- createStyle(fgFill = "#A9D08E")
     styVF   <- createStyle(fgFill = "#EBF1DE", halign = "CENTER")
     styG    <- createStyle(fgFill = "#F2F2F2")
     styP    <- createStyle(fgFill = "#E4DFEC")
     styDP   <- createStyle(fgFill = "#60497A", halign = "RIGHT", textDecoration = "Bold", fontColour = "white")
     styNeg  <- createStyle(fontColour = "#9C0006", fgFill = "#FFC7CE")
     styPos  <- createStyle(fontColour = "#006100", fgFill = "#C6EFCE")
     styREM  <- createStyle(fontColour = "green", borderStyle="none", fgFill="white")
     styBOLD <- createStyle(textDecoration = "Bold", borderStyle="none", fgFill="white")

     negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
     posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")

     # Create tabs
     tabs <- c( "Samples", "Buckets", "Data", "SNR", "Quantification", "Macro-CMD" )
     for (i in 1:length(tabs))  addWorksheet(wb = wb, sheetName = tabs[i], gridLines = TRUE)

     bucketfile <- file.path(outDataViewer,conf$BUCKET_LIST)
     if ( file.exists(bucketfile) ) {

     # Get the list of all Raw directories
         rawids <- read.table(file.path(outDataViewer,'rawids.csv'), sep=';', stringsAsFactors=F)[, c(2,3) ]
         
     # Get the list of samples
         samples <- read.table(file.path(outDataViewer,"samples.csv"), header=F, sep=";", stringsAsFactors=FALSE)
         # SS: Size of Samples
         SS <- dim(samples)[1]
         # SC: Size of Compounds
         SC <- dim(samples)[2]
     # read factors labels
         labels <- c( 'Rawnames', read.table(file.path(outDataViewer,"factors"), header=F, sep=";", stringsAsFactors=FALSE)[,2])

         specMat <- get_specMat()
         outsnr <- get_SNR_dataset(specMat, bucketfile, c(min(zonenoise), max(zonenoise)), ratio=TRUE)
         outdata <- get_Buckets_dataset(specMat, bucketfile, input$normmeth, zoneref)
         buckets <- read.table(bucketfile, header=F, sep="\t",stringsAsFactors=FALSE)
         BUCsel <- get_Buckets_upperSNR(buckets, outsnr, input$snrlevel)
         BUCsel <- BUCsel[ grep("\\.2$", BUCsel, invert=TRUE) ]
         buckets <- get_Buckets_table(bucketfile)
         buckets <- buckets[ buckets[,1] %in% BUCsel, ]

     # Sample tab
         shid <- 1
         if (SC>2) {
             T1 <- cbind( samples[,c(1,2)], rawids, samples[,c(3:SC)], rep(500, SS), rep(20, SS), rep(1, SS) )
             colnames(T1) <- c(labels[c(1,2)], 'expno', 'procno', labels[c(3:length(labels))], 'Volume (10^-6.L)', 'mgDW', 'Dilution')
         } else {
             T1 <- cbind( samples[,c(1,2)], rawids, rep(500, SS), rep(20, SS), rep(1, SS) )
             colnames(T1) <- c(labels[c(1,2)], 'expno', 'procno', 'Volume (10^-6.L)', 'mgDW', 'Dilution')
         }
         writeData(wb, shid, T1, colNames=TRUE, rowNames=FALSE, withFilter = FALSE)
         addStyle(wb, shid, style = styBH, rows = 1, cols = c(1:dim(T1)[2]), gridExpand = TRUE)
         addStyle(wb, shid, style = styVS, rows = c(2:(SS+1)), cols = 2, gridExpand = TRUE)
         if (SC>2) addStyle(wb, shid, style = styVF, rows = c(2:(SS+1)), cols = c(5:(SC+2)), gridExpand = TRUE)
         addStyle(wb, shid, style = styP,  rows = c(2:(SS+1)), cols = c((SC+3):(SC+5)), gridExpand = TRUE)
         setColWidths(wb, shid, cols = c(1:dim(T1)[2]), widths = 'auto')

     # Buckets tab
         shid <- shid + 1
         VS <- dim(buckets)[1]
         df <- as.data.frame(buckets)
         df$Compound <- paste(rep('C',VS), c(1:VS), sep='')
         TV <- cbind( rep(1, VS), rep(1, VS), rep(100, VS), rep(1, VS), rep('mg/gDW', VS ) )
         colnames(TV) <- c( 'Number of H', 'Signal Proportion', 'Molar Mass', 'Calibration Factor','Quantification Unit')
         T2 <- cbind( df, as.data.frame(TV) )
         writeData(wb, shid, T2, colNames=TRUE, rowNames=FALSE, withFilter = FALSE)
         addStyle(wb, shid, style = styBH, rows = 1, cols = c(1:dim(T2)[2]), gridExpand = TRUE)
         addStyle(wb, shid, style = styB,  rows = c(2:(VS+1)), cols = 1, gridExpand = TRUE)
         addStyle(wb, shid, style = styO,  rows = c(2:(VS+1)), cols = (dim(buckets)[2]+1), gridExpand = TRUE)
         addStyle(wb, shid, style = styP,  rows = c(2:(VS+1)), cols = c( (dim(df)[2]+1):dim(T2)[2] ), gridExpand = TRUE)
         setColWidths(wb, shid, cols = c(1:dim(T2)[2]), widths = 'auto')

     # Data matrix tab
         shid <- shid + 1
         if (SC>2) {
            T3 <- cbind( outdata[, c(1:(SC-1))], outdata[ , colnames(outdata) %in% BUCsel] )
         } else {
            T3 <- cbind( as.matrix(outdata[,1]), outdata[ , colnames(outdata) %in% BUCsel] )
         }
         for (i in 1:length(df$Compound))
             writeFormula(wb, shid, paste0(tabs[2],"!",int2col(dim(df)[2]),i+1), startRow=1, startCol=(SC+i-1));
         writeData(wb, shid, T3, startRow = 2, startCol = 1, colNames=TRUE, rowNames=FALSE, withFilter = FALSE)
         addStyle(wb, shid, style = styO,  rows = 1, cols = c(SC:(SC+VS-1)), gridExpand = TRUE)
         addStyle(wb, shid, style = styBH, rows = 2, cols = c(1:(SC+VS-1)), gridExpand = TRUE)
         addStyle(wb, shid, style = styVS, rows = c(3:(SS+2)), cols = 1, gridExpand = TRUE)
         addStyle(wb, shid, style = styVF, rows = c(3:(SS+2)), cols = c(2:(SC-1)), gridExpand = TRUE)
         setColWidths(wb, shid, cols = c(1:dim(T3)[2]), widths = 'auto')

     # SNR matrix tab
         shid <- shid + 1
         rule1 <- paste0("<=",input$snrlevel)
         rule2 <- paste0(">",input$snrlevel)
         if (SC>2) {
            T4 <- cbind( outsnr[, c(1:(SC-1))], outsnr[ , colnames(outsnr) %in% BUCsel] )
         } else {
            T4 <- cbind( as.matrix(outsnr[,1]),  outsnr[ , colnames(outsnr) %in% BUCsel] )
         }
         for (i in 1:length(df$Compound))
             writeFormula(wb, shid, paste0(tabs[2],"!",int2col(dim(df)[2]),i+1), startRow=1, startCol=(SC+i-1));
         writeData(wb, shid, T4, startRow = 2, startCol = 1, colNames=TRUE, rowNames=FALSE, withFilter = FALSE)
         addStyle(wb, shid, style = styO,  rows = 1, cols = c(SC:(SC+VS-1)), gridExpand = TRUE)
         addStyle(wb, shid, style = styBH, rows = 2, cols = c(1:dim(T4)[2]), gridExpand = TRUE)
         addStyle(wb, shid, style = styVS, rows = c(3:(SS+2)), cols = 1, gridExpand = TRUE)
         addStyle(wb, shid, style = styVF, rows = c(3:(SS+2)), cols = c(2:(SC-1)), gridExpand = TRUE)
         setColWidths(wb, shid, cols = c(1:dim(T4)[2]), widths = 'auto')
         conditionalFormatting(wb, shid, rows = c(3:(SS+2)), cols = c(SC:(SC+length(df$Compound)-1)), rule=rule1, style = negStyle)
         conditionalFormatting(wb, shid, rows = c(3:(SS+2)), cols = c(SC:(SC+length(df$Compound)-1)), rule=rule2, style = posStyle)
         writeData(wb, shid, t(t(c(paste0('SNR',rule1),paste0('SNR',rule2)))), startRow = SS+4, startCol = SC, 
                       colNames=FALSE, rowNames=FALSE, withFilter = FALSE)
         addStyle(wb, shid, style = styNeg,  rows = SS+4, cols = SC, gridExpand = FALSE)
         addStyle(wb, shid, style = styPos,  rows = SS+5, cols = SC, gridExpand = FALSE)

     # Quantification tab
         shid <- shid + 1
         for (i in 1:length(df$Compound))
             writeFormula(wb, shid, paste0(tabs[2],"!",int2col(dim(df)[2]),i+1), startRow=1, startCol=(SC+i-1));
         addStyle(wb, shid, style = styO,  rows = 1, cols = c(SC:(SC+VS-1)), gridExpand = TRUE)

         writeData(wb, shid, colnames(TV), startRow = 2, startCol = SC-1, colNames=FALSE, rowNames=FALSE, withFilter = FALSE)
         for (j in 1:dim(TV)[2] )
             for (i in 1:length(df$Compound))
                 writeFormula(wb, shid, paste0(tabs[2],"!",int2col(dim(df)[2]+j),i+1), startRow=1+j, startCol=(SC+i-1));
         addStyle(wb, shid, style = styDP, rows = c(2:(dim(TV)[2]+1)), cols = SC-1, gridExpand = TRUE)
         addStyle(wb, shid, style = styG,  rows = c(2:(dim(TV)[2]+1)), cols = c(SC:(SC+length(df$Compound)-1)), gridExpand = TRUE)

         startRow <- dim(TV)[2]+2
         writeData(wb, shid, T3[, c(1:(SC-1))], startRow = startRow, startCol = 1, colNames=TRUE, rowNames=FALSE, withFilter = FALSE)
         writeData(wb, shid, t(buckets[,1]), startRow = startRow, startCol = SC, colNames=FALSE, rowNames=FALSE, withFilter = FALSE)
         addStyle(wb, shid, style = styBH, rows = startRow, cols = c(1:dim(T3)[2]), gridExpand = TRUE)
         addStyle(wb, shid, style = styVS, rows = c((startRow+1):(SS+startRow)), cols = 1, gridExpand = TRUE)
         addStyle(wb, shid, style = styVF, rows = c((startRow+1):(SS+startRow)), cols = c(2:(SC-1)), gridExpand = TRUE)

         # Example Formula = Data!D3/D$5/D$2/D$3*(Samples!$G2*D$4)/Samples!$H2/1000*Samples!$I2
         #    Data!Dx     => paste0(tabs[3],"!",int2col(SC+i-1), 2:(SS+1) + 1L )  -  x <=> 3:(SS+2)
         #    'X'$2       => paste0(int2col(SC+i-1), '$', 2)                      -  X <=> i
         #    Samples!$Gx => paste0(tabs[1],"!", int2col(SC+3), 2:(SS+1) + 0L)    -  x <=> 2:(SS+1)
         #    Samples!$Hx => paste0(tabs[1],"!", int2col(SC+4), 2:(SS+1) + 0L)    -  x <=> 2:(SS+1)
         #    Samples!$Ix => paste0(tabs[1],"!", int2col(SC+5), 2:(SS+1) + 0L)    -  x <=> 2:(SS+1)

         for (i in 1:length(df$Compound)) {
               FQ <- data.frame( Z=paste0(  paste0(tabs[3],"!",int2col(SC+i-1), 2:(SS+1) + 1L ), '/',
                       paste0(int2col(SC+i-1), '$', 5),'/',paste0(int2col(SC+i-1), '$', 2),'/',paste0(int2col(SC+i-1), '$', 3), '*(',
                       paste0(tabs[1],"!", int2col(SC+3), 2:(SS+1) + 0L),'*',paste0(int2col(SC+i-1), '$', 4), ')/',
                       paste0(tabs[1],"!", int2col(SC+4), 2:(SS+1) + 0L),'/1000*',paste0(tabs[1],"!", int2col(SC+5), 2:(SS+1) + 0L)  ) )
               class(FQ$Z) <- c(class(FQ$Z), "formula")
               writeData(wb, shid, x=FQ, startRow = startRow+1, startCol = SC+i-1, colNames=FALSE, rowNames=FALSE, withFilter = FALSE)
         }

         setColWidths(wb, shid, cols = c(1:dim(T3)[2]), widths = 'auto')

     # Macro-CMD tab
         shid <- shid + 1
         if (file.exists(file.path(outDataViewer,conf$Rnmr1D_PCMD))) {
             cmdlines <- readLines(file.path(outDataViewer,conf$Rnmr1D_PCMD))
             writeData(wb, shid, x = cmdlines,  colNames=TRUE, rowNames=FALSE, withFilter = FALSE)
             setColWidths(wb, shid, cols=1, widths=100, ignoreMergedCells = FALSE)
             L <- grep("^#", cmdlines)
             for (i in 1:length(L)) addStyle(wb, shid, style = styREM, rows = L[i], cols = 1, gridExpand = TRUE)
             L <- grep("^[^#]", cmdlines)
             for (i in 1:length(L)) addStyle(wb, shid, style = styBOLD, rows = L[i], cols = 1, gridExpand = TRUE)
         }
     }
}


##---------------
## Extension for Galaxy Interactive Environment
##---------------
# Generates the shell script that :
#  i/ init the semaphores
#  ii/ will launch the upload script, 
# then launches the shell script
#----
submit_UploadScript <- function(outDataViewer, macrofile)
{
   cmdfile <- file.path(outDataViewer,'jobScript.sh')
   LOGFILE <- file.path(outDataViewer,conf$ERRORFILE)
   LOGFILE3 <- file.path(outDataViewer,conf$LOGFILE3)

   delete_file(conf$ERRORFILE)
   delete_file(conf$LOGFILE3)

   UploadScript <- conf$UPLOADSCRIPT
   if (! file.exists(UploadScript) ) {
      msg <- paste('ERROR: ',UploadScript," not found\n")
      write_textlines(LOGFILE3, msg, mode="at")
      RET <- 1
   } else {
     cmd <- paste(UploadScript,macrofile)
     RET <- system( paste(cmd, " 2>>",LOGFILE3," 1>>",LOGFILE3,"; echo $?"), intern=TRUE )
   }
   RET
}
