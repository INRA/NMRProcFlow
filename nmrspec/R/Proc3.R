##---------------
## Processing 
##---------------
    get_CorrCmd <- function() {
         isolate({
             procParams$REXEC <<- 'Rcorr1D'
             procParams$PPM_CALIBRATION <<- ifelse(input$tpreproc=='calibration', TRUE, FALSE)
             procParams$PPM_NORMALISATION <<- ifelse(input$tpreproc=='normalisation', TRUE, FALSE)
             procParams$BASELINECOR <<- ifelse(input$tpreproc=='baseline', TRUE, FALSE)
             procParams$DENOISING <<- ifelse(input$tpreproc=='denoising', TRUE, FALSE)
             if (input$tpreproc=='calibration') {
                 procParams$PPM_RANGE <<- input$ppmrefrange
                 procParams$PPMNOISERANGE <<- input$ppmnoiserange4
                 procParams$PPM_REF <<- input$ppmref
             }
             if (input$tpreproc=='normalisation') {
                 procParams$NORM_METH <<- input$normeth
             }
             if (input$tpreproc=='baseline') {
                 procParams$BCTYPE <<- input$bctype
                 if (input$bctype==1) {
                     procParams$GBCLEV <<- input$gbclev
                 }
                 if (input$bctype==2) {
                     procParams$BCMETH <<- 0
                     procParams$WSFAC <<- input$wsfac
                 }
                 if (input$bctype==4) {
                     procParams$LAMBDA <<- input$lambda
                 }
                 procParams$PPMBCRANGE <<- input$ppmbcrange
                 procParams$PPMNOISERANGE <<- input$ppmnoiserange
             }
             if (input$tpreproc=='denoising') {
                 procParams$PPMFILT <<- ifelse(input$fppmfilt==1, input$ppmfilt, '')
                 procParams$FILTORD <<- input$filtord
                 procParams$FILTLEN <<- input$filtlen
             }
         })
         ArrayProc[CORR] <<- 1
         return( paste(conf$Rscript_exec,"Rcorr1D -p ", as.character(outDataViewer), sep="") )
    }
    get_AlignCmd <- function() {
         isolate({
             procParams$REXEC <<- 'Ralign1D'
             procParams$ALIGNMETH <<- input$alignmeth
             procParams$IDXSREF <<- input$RefSpecSelect
             procParams$RELDECAL <<- input$aligndecal
             procParams$PTWCRIT <<- input$warpcrit
             procParams$TPROC <<- input$tpreproc
             procParams$PPM_RANGE <<- input$ppmrefrange3
             procParams$PPMNOISERANGE <<- input$ppmnoiserange3
             procParams$SNRBUCLEVEL <<- input$snrpiclev
             procParams$RESPPM <<- input$resclupa
         })
         ArrayProc[ALIGN] <<- 1
         return( paste(conf$Rscript_exec,"Ralign1D -p ", as.character(outDataViewer), sep="") )
    }
    get_BucketCmd <- function() {
         isolate({
             procParams$REXEC <<- 'Rbuc1D'
             procParams$BUCMETH <<- input$bucmeth
             procParams$PPMNOISERANGE <<- input$ppmnoiserange2
             procParams$SNRBUCLEVEL <<- input$snrbuclev
             procParams$PPMREFINT <<- input$ppmnrefint
             procParams$BUCAPPEND <<- ifelse(input$fbucappend==1, TRUE, FALSE)
             procParams$UNIF_R <<- input$unif_size
             procParams$AIBIN_R <<- input$aibin_r
             procParams$ERVA_R <<- input$erva_r
             procParams$importBuc_sep <<- input$importFormat
             procParams$importBuc_head <<- ifelse(input$fbucheader==1, TRUE, FALSE)
             procParams$importBuc_min <<- input$minBucPPM
             procParams$importBuc_max <<- input$maxBucPPM
         })
         ArrayProc[BUCKET] <<- 1
         return( paste(conf$Rscript_exec,"Rbuc1D -p ", as.character(outDataViewer), sep="") )
    }

    ## Processing required
    output$ProcSelect <- reactive({
         input$condProcPanels
         isolate({
             PPCMD <- file.path(outDataViewer,conf$Rnmr1D_PPCMD)
             ret <- ifelse(input$condProcPanels == 'Data Export' && file.exists(PPCMD) && file.info(PPCMD)$size>0 , 1, 0)
             PCMD <- file.path(outDataViewer,conf$Rnmr1D_PCMD)
             ret <- ifelse(input$condProcPanels == 'Data Export' && file.exists(PCMD) && file.info(PCMD)$size>0 , 1, ret)
             return(ret)
         })
    })
    outputOptions(output, 'ProcSelect', suspendWhenHidden=FALSE)
    outputOptions(output, 'ProcSelect', priority=1)

    ## Bucketing required
    output$BucSelect <- reactive({
         input$condProcPanels
         isolate({
             ret <- ifelse(input$condProcPanels == 'Data Export' && file.exists(file.path(outDataViewer,conf$BUCKET_LIST)), 1, 0)
             return(ret)
         })
    })
    outputOptions(output, 'BucSelect', suspendWhenHidden=FALSE)
    outputOptions(output, 'BucSelect', priority=1)

    ## Processing request
    output$ProcRequest <- reactive({
         input$process
         isolate({
             if ( values$load==0 || values$proc==0 || values$jobrun==1 || input$process==0 || sum(ArrayProc)>0 ) return(0);
             repeat {
                if (input$condProcPanels == 'Processing') {
                    switch( input$tpreproc, 'calibration'= { cmdR <- get_CorrCmd()  }, 
                                          'normalisation'= { cmdR <- get_CorrCmd()  }, 
                                               'baseline'= { cmdR <- get_CorrCmd()  },
                                               'ppmalign'= { cmdR <- get_AlignCmd() },
                                                'ppmzero'= { cmdR <- get_AlignCmd() },
                                              'denoising'= { cmdR <- get_CorrCmd()  } )
                    break
                }
                if (input$condProcPanels == 'Bucketing') {
                    cmdR <- get_BucketCmd()
                    break
                }
                if (input$condProcPanels == 'Data Export') {
                    return(0)
                }
                break
             }

             # Generate a INI file from the list of processing parameters
             generate_INI_file(procParams)

             # Save the current files
             if (input$condProcPanels == 'Bucketing') {
                 if( !file.exists(file.path(outDataViewer,conf$Rnmr1D_BCMD)) ) file.create(file.path(outDataViewer,conf$Rnmr1D_BCMD))
                 nbStackedBuc <- get_maxSTACKID(outDataViewer,conf$Rnmr1D_BCMD)
                 push_STACK(outDataViewer, c(conf$BUCKET_LIST, conf$LOGFILE2, conf$Rnmr1D_BCMD, zonelist2), nbStackedBuc)
             } else {
                 if( !file.exists(file.path(outDataViewer,conf$Rnmr1D_PCMD)) ) file.create(file.path(outDataViewer,conf$Rnmr1D_PCMD))
                 nbStackedProc <- get_maxSTACKID(outDataViewer,conf$SPEC_PACKED)
                 push_STACK(outDataViewer, c(conf$SPEC_PACKED, conf$LOGFILE, conf$Rnmr1D_PCMD, zonelist1), nbStackedProc)
             }
             delete_file(conf$ERRORFILE)

             # Launch the process ...
             procJobName <<- 'process'
             ERROR$MsgErrProc <- ''
             closeAlert(session, "ErrAlertProcId")
             submit_Rscript(outDir, outDataViewer, cmdR)
             values$jobrun <- 1
             values$updatevent <- 0
             return(1)
         })
    })
    outputOptions(output, 'ProcRequest', suspendWhenHidden=FALSE)
    outputOptions(output, 'ProcRequest', priority=1)

    ## Cancel the last processing
    output$UndoRequest <- renderUI({
         input$yes_undo
         html <- ''
         if (input$yes_undo>0) {
             nbStackedProc <- get_maxSTACKID(outDataViewer,conf$SPEC_PACKED)
             delete_list_file(zonelist1)
             delete_list_file(zonelist2)
             pop_STACK(outDataViewer, c(conf$SPEC_PACKED, conf$LOGFILE, conf$Rnmr1D_PCMD, zonelist1), nbStackedProc)
             html <- paste0('<script>refresh_spectrum();</script>')
             toggleModal(session, "modalUndo", toggle = "close")
             isolate({ values$updatevent <- values$updatevent + 1; })
         }
         HTML(html)
    })
    outputOptions(output, 'UndoRequest', suspendWhenHidden=FALSE)
    outputOptions(output, 'UndoRequest', priority=1)

    ## Cancel the last bucketing
    output$UndoBucket <- renderUI({
         input$yes_unBuc
         html <- ''
         if (input$yes_unBuc>0) {
             nbStackedBuc <- get_maxSTACKID(outDataViewer,conf$Rnmr1D_BCMD)
             if (nbStackedBuc>0) {
                 delete_list_file(zonelist1)
                 delete_list_file(zonelist2)
                 pop_STACK(outDataViewer, c(conf$BUCKET_LIST, conf$LOGFILE2, conf$Rnmr1D_BCMD, zonelist2), nbStackedBuc)
                 nbStackedBuc <- nbStackedBuc - 1
                 isolate({ values$updatevent <- values$updatevent + 1; })
             }
             if (nbStackedBuc==0) {
                 delete_file(conf$BUCKET_LIST); delete_file(conf$LOGFILE2)
             }
             toggleModal(session, "modalUnBuc", toggle = "close")
             html <- paste0('<script>refresh_spectrum();</script>')
         }
         HTML(html)
    })
    outputOptions(output, 'UndoBucket', suspendWhenHidden=FALSE)
    outputOptions(output, 'UndoBucket', priority=1)

    ##---------------
    ## SessReload : Reload the session
    ##---------------
    output$SessReload <- reactive({
         values$reload
         isolate({
             if (values$reload==1) {
                 INI.filename <- paste0(outDataViewer,'/',conf$Rnmr1D_INI)
                 procParams <<- Parse.INI(INI.filename, INI.list=list(), section="PROCPARAMS")
                 if (! file.exists(file.path(outDataViewer,"userfiles"))) {
                    outDir <<- outDataViewer
                    NameZip <<- 'noname.zip'
                 }
                 else {
                    V <- read.table(file.path(outDataViewer,"userfiles"), header=F, stringsAsFactors=FALSE)[,1]
                    NameZip <<- V[1]
                    SampleFilename <<- V[2]
                    outDir <<- dirname(V[3])
                    if (! file.exists(outDir) ) outDir <<- outDataViewer
                 }
                 if (file.exists(file.path(outDataViewer,"pcmdfiles"))) {
                    V <- read.table(file.path(outDataViewer,"pcmdfiles"), header=F, stringsAsFactors=FALSE)[,1]
                    PCMDFilename <<- V[1]
                 }
                 procJobName <<- 'process'
                 nbStackedProc <- get_maxSTACKID(outDataViewer,conf$SPEC_PACKED)
                 bsUndoLabel <- ifelse(nbStackedProc>0, paste0(' (',as.character(nbStackedProc),')'), '')
                 updateButton(session, "undo", icon=icon("undo"), label = paste0('Undo',bsUndoLabel), style="primary")
                 nbStackedBuc <- get_maxSTACKID(outDataViewer,conf$BUCKET_LIST)
                 if (file.exists(file.path(outDataViewer,conf$BUCKET_LIST))) nbStackedBuc <- nbStackedBuc + 1
                 bsUndoLabel <- ifelse(nbStackedBuc>0, paste0(' (',as.character(nbStackedBuc),')'), '')
                 updateButton(session, "unBucket", icon=icon("undo"), label = paste0('Undo',bsUndoLabel), style="primary")
                 values$load <- 1
                 values$proc <- 1
             }
         })
         return(values$reload)
    })
    outputOptions(output, 'SessReload', suspendWhenHidden=FALSE)
    outputOptions(output, 'SessReload', priority=1)

    ##---------------
    ## Processing : Start - Launch the process
    ##---------------
    output$Processing <- reactive({
         values$jobrun
         if (values$jobrun==0) return(0)
         if (procJobName != "process") return (0)
         return(1)
    })
    outputOptions(output, 'Processing', suspendWhenHidden=FALSE)
    outputOptions(output, 'Processing', priority=1)

    ##---------------
    ## Processing : Ended
    ##---------------
    output$Processed <- reactive({
         input$jobstatus
         isolate({
             if (is.null(procJobName)) return (0)
             if (procJobName != "process") return (0)
             if (input$jobstatus != "Ended" && input$jobstatus != "Error") return (0)
             ArrayProc <<- rep(0, NBPROC)
             toggleModal(session, "modalWatcher2", toggle = "close")
             if (input$jobstatus == "Error") {
                 infoLines <- readLines(paste0(outDataViewer,'/',conf$semapFileErr))
                 ERROR$MsgErrProc <- paste0( paste(as.list(infoLines), collapse="\n"),"\n\n" )
             }
             values$jobrun <- 0
             return(1)
         })
    })
    outputOptions(output, 'Processed', suspendWhenHidden=FALSE)
    outputOptions(output, 'Processed', priority=1)


    ## Reactive value that indicates if the "Job Watcher" is required
    output$modaljoblog <- reactive({
         return(input$joblog);
    })
    outputOptions(output, 'modaljoblog', suspendWhenHidden=FALSE)
    outputOptions(output, 'modaljoblog', priority=1)


    ##---------------
    ## Output: Watch the logfile in realtime
    ##---------------
    ## Watcher 2: Processing
    output$watcher2 <- renderUI({
         values$jobrun
         isolate({
             if (values$jobrun==0 || input$joblog==0) return(NULL)
             if (is.null(procJobName)) return (NULL)
             if (procJobName != "process") return (NULL)
             logtype <- ifelse(input$condProcPanels == 'Bucketing', 'bucket', 'proc')
             renderWatcher(values$jobrun,logtype,600)
         })
    })
    outputOptions(output, 'watcher2', suspendWhenHidden=FALSE)
    outputOptions(output, 'watcher2', priority=5)

    ## Watcher 1b: Watch the logfile in shift time
    output$watcher2b <- renderUI({ 
         input$proclog
         isolate({
             if (is.null(procJobName)) return (NULL)
             html <- ''
             LOGFILE <- ifelse (input$condProcPanels == 'Bucketing', conf$LOGFILE2, conf$LOGFILE)
             fileLog <- file.path(outDataViewer,LOGFILE)
             if (file.exists(fileLog)) {
                t1 <- file.info(file.path(outDataViewer,conf$semapFileIn))$ctime
                t2 <- file.info(file.path(outDataViewer,conf$semapFileOut))$ctime
                elapsedtime <- round(as.numeric(difftime(t2,t1),units="secs"),3)
                html <- paste0("<h4>Total Job Duration: ",elapsedtime," secs</h4>" )
                html <- paste0(html,'<div style="height: 600px;"><pre style="height: 600px;">', paste(as.list(readLines(fileLog)), collapse="\n"),'</pre></div>' )
             }
             return(HTML(html))
         })
    })

    DS_ext <- reactive({
         switch(input$exportFormat,  "csv" = ".csv", "ssv" = ".csv",  "tsv" = ".txt", "json"=".json")
    })
    DS_sep <- reactive({
         switch(input$exportFormat,  "csv" = ",", "ssv" = ";",  "tsv" = "\t")
    })

    ##---------------
    ## Import Buckets
    ##---------------
    output$importBUC <- reactive({
         input$importBucfile
         isolate({
            importBucfile <- input$importBucfile
            if ( is.null(importBucfile) ) return (0)
            file.copy( importBucfile$datapath, file.path(outDataViewer,'importBuckets'),overwrite = TRUE )
            return(1)
         })
    })
    outputOptions(output, 'importBUC', suspendWhenHidden=FALSE)
    outputOptions(output, 'importBUC', priority=1)

   ##---------------
   ## Output: Update the 'Export Format' SelectBox 
   ##---------------
   updateExportFormatSelect <- function () {
         if (file.exists(file.path(outDataViewer,"samples.csv"))) {
             samples <- read.table(file.path(outDataViewer,"samples.csv"), header=F, sep=";", stringsAsFactors=FALSE)
             v_options <- c("tsv", "csv", "ssv")
             names(v_options) <- c("Tabular Separator Value (TXT)", "Comma Separator Value (CSV)", "Semicolon Separator Value (CSV)")
             op_selected <- "tsv"
             if (dim(samples)[1]==1 && input$eptype=='epspec') {
                 v_options$"JSON Format" <- "json"
                 op_selected <- v_options[4]
             }
             updateSelectInput(session, "exportFormat", choices = v_options, selected=op_selected)
         }
   }

   observe ({
         input$eptype
         isolate({ if (values$proc==1) updateExportFormatSelect() })
   })

    ##---------------
    ## Export Data matrix
    ##---------------
    output$exportBS <- downloadHandler(
         filename = function() { paste0('data_',gsub("\\..*$", "", NameZip), DS_ext() ) },
         content = function(file) {
              # get the PPM range of the reference
              if (trim(input$ppmnrefint)=="") {
                  zoneref <- NA
              } else {
                  zoneref <- as.numeric(simplify2array(strsplit(input$ppmnrefint, " ")))
              }
              # get the PPM range of the noise
              zonenoise <- as.numeric(simplify2array(strsplit(trim(input$ppmsnrnoise), " ")))
              write.table(get_Data_matrix(outDataViewer, zoneref, zonenoise), file, sep=DS_sep(), row.names=FALSE, col.names=TRUE)
         }
    )

    ##---------------
    ## Export Buckets table
    ##---------------
    output$exportBUC <- downloadHandler(
         filename = function() { paste0('buckets_',gsub("\\..*$", "", NameZip), DS_ext() ) },
         content = function(file) {
              bucketfile <- file.path(outDataViewer,conf$BUCKET_LIST)
              if ( file.exists(bucketfile) )
                  write.table(get_Buckets_table(bucketfile), file, sep=DS_sep(), row.names=FALSE, col.names=TRUE)
         }
    )

    ##---------------
    ## Export SNR matrix
    ##---------------
    output$exportSNR <- downloadHandler(
         filename = function() { paste0('snr_',gsub("\\..*$", "", NameZip), DS_ext() ) },
         content = function(file) {
              specMat <- get_specMat()
              bucketfile <- file.path(outDataViewer,conf$BUCKET_LIST)
              zone <- as.numeric(simplify2array(strsplit(trim(input$ppmsnrnoise), " ")))
              if ( file.exists(bucketfile) )
                  write.table(get_SNR_dataset(specMat, bucketfile, c(min(zone), max(zone)), ratio=TRUE),
                              file, sep=DS_sep(), row.names=FALSE, col.names=TRUE)
         }
    )

    ##---------------
    ## Export XLSX Workbook
    ##---------------
    output$exportXLSX <- downloadHandler(
         filename = function() { paste0('WB_',gsub("\\..*$", "", NameZip), '.xlsx' ) },
         content = function(file) {
             # get the PPM range of the reference
              if (trim(input$ppmnrefint)=="") {
                  zoneref <- NA
              } else {
                  zoneref <- as.numeric(simplify2array(strsplit(input$ppmnrefint, " ")))
              }
             # get the PPM range of the noise
             zonenoise <- as.numeric(simplify2array(strsplit(trim(input$ppmsnrnoise), " ")))
             # Create a new Workbook
             wb <- createWorkbook()
             # Fill in  the Workbook based on the 'simple' template
             if (input$exportTempl == 'simple') {
                 write_simple_wb(wb, outDataViewer, zoneref, zonenoise)
             }
             # Fill in  the Workbook based on the 'qhnmr' template
             if (input$exportTempl == 'qhnmr') {
                 write_qhnmr_wb(wb, outDataViewer, zoneref, zonenoise)
             }

             # Save the Workbook
             saveWorkbook(wb, file, overwrite = TRUE)
         }
    )

    ##---------------
    ## Export Spectra Data
    ##---------------
    output$exportSpec <- downloadHandler(
         filename = function() { paste0(gsub("\\..*$", "", NameZip), DS_ext() ) },
         content = function(file) {
              specMat <- get_specMat()
              if (input$exportFormat == "json") {
                  samples <- read.table(file.path(outDataViewer,"samples.csv"), header=F, sep=";", stringsAsFactors=FALSE)
                  id <- samples[1,2]
                  L <- list( type = "single",
                             id = id,
                             xLabel = "ppm",
                             yLabel = "Intensity",
                             xMax =  specMat$ppm_max,
                             xMin = specMat$ppm_min,
                             yMax =  max(specMat$int),
                             yMin =  min(specMat$int),
                             data = rev(round(specMat$int[1,] )))
                  json <- toJSON( L )
                  fh<-file(file,"at")
                  writeLines(json, fh)
                  close(fh)
              } else {
                 samplesFile <- file.path(outDataViewer,'samples.csv')
                 if ( file.exists(samplesFile) )
                    write.table(get_Spectra_Data(specMat, samplesFile), file, sep=DS_sep(), row.names=FALSE, col.names=TRUE)
              }
         }
    )

    ##---------------
    ## Output: View the Macro Command file
    ##---------------
    output$viewCmd <- renderUI({
         input$cmdlog
         if (isolate(values$proc==0)) return(NULL)
         html <- '<pre>No processing jobs</pre>'
         if (file.exists(file.path(outDataViewer,conf$Rnmr1D_PCMD))) {
             html <- paste('<!-- ', paste(sample(c(0:9, letters[1:6]),30, replace=TRUE),collapse=""), '--><div id="viewCmd">',
                           '<pre>', paste(readLines(file.path(outDataViewer,conf$Rnmr1D_PCMD)),collapse="\n"),'</pre></div>',
                           '<script>$( "#viewCmd" ).css( "height", ( 2*$( window ).height()/3)+"px" );</script>', sep="")
         }
         return(HTML(html))
    })
    outputOptions(output, 'viewCmd', priority=5)


    ##---------------
    ## Export Macro commands
    ##---------------
    get_CMD_filename <- function () { paste0('NP_macro_cmd_',gsub("\\..*$", "", NameZip), '.txt' ) }
    get_CMD_content <- function(file) {
         cmdfile1 <- file.path(outDataViewer,conf$Rnmr1D_PPCMD)
         if ( file.exists(cmdfile1) ) file.copy(cmdfile1, file, overwrite = TRUE)
         cmdfile2 <- file.path(outDataViewer,conf$Rnmr1D_PCMD)
         if ( file.exists(cmdfile2) ) {
             if ( ! file.exists(cmdfile1) ) file.copy(cmdfile2, file, overwrite = TRUE)
             if ( file.exists(cmdfile1) ) file.append(file,cmdfile2)
         }
    }
    output$exportCMD <- downloadHandler(
         filename = function() { get_CMD_filename() },
         content = function(file) { get_CMD_content(file) }
    )
    output$exportCMD2 <- downloadHandler(
         filename = function() { get_CMD_filename() },
         content = function(file) { get_CMD_content(file) }
    )

   observeEvent( input$exportCMD3, {
       macrofile <- file.path(outDataViewer, get_CMD_filename())
	   get_CMD_content(macrofile)
	   submit_PythonScript(outDataViewer, macrofile)
   })
