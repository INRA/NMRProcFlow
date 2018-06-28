##---------------
## UI initialization events
##---------------
   output$panelHeader <- reactive({
       values$header==1
   })
   outputOptions(output, "panelHeader", suspendWhenHidden = FALSE)

   output$patchO1param <- reactive({
       values$header
       conf$O1_PARAM_PATCH==1
   })
   outputOptions(output, "patchO1param", suspendWhenHidden = FALSE)

   observeEvent( values$psession, {
       if (values$psession==1) {
          showTab(inputId = "conditionedPanels", target = "Processing", select=TRUE)
       }
   })
   observeEvent( values$header, {
       if (values$header==0 && values$psession==1) {
          runjs( "document.getElementById('conditionedPanels').style.display = 'none';" )
          hideTab(inputId = "conditionedPanels", target = "Load")
       }
   })

   ## Extension for Galaxy Interactive Environment
   observeEvent( values$fgalaxy, {
       if (values$fgalaxy==1) {
          runjs( "document.getElementById('nogalaxy').style.display = 'none';" )
       }
       if (values$fgalaxy==2) {
          runjs( "document.getElementById('egalaxy').style.display = 'none';" )
       }
   })

   output$FormatSelected <- reactive({
         if (input$vendor=="sinput") return(0)
         return(1)
   })
   outputOptions(output, 'FormatSelected', suspendWhenHidden=FALSE)
   outputOptions(output, 'FormatSelected', priority=20)

   observeEvent ( input$vendor, {
       v_options <- c("fid"); names(v_options) <- c('FID'); v_select<-'fid'
       if ( input$vendor=="bruker" ) {
            v_options[length(v_options)+1] <- '1r'; names(v_options)[length(v_options)] <- '1r spectrum';
            v_select<-'fid'
       }
       updateSelectInput(session, "spectype", choices = v_options, selected=v_select)
   })

   observeEvent ( ERROR$MsgErrLoad, {
       if (nchar(ERROR$MsgErrLoad)>0) {
          createAlert(session, "ErrAlertLoad", "ErrAlertLoadId", title = "", content = ERROR$MsgErrLoad, append = FALSE, style='danger')
       }
   })

   ##---------------
   ## Output: Update the 'Reference spectrum' SelectBox
   ##---------------
   updateRefSpecSelect <- function () {
         samples <- read.table(file.path(outDataViewer,"samples.csv"), header=F, sep=";", stringsAsFactors=FALSE)
         v_options <- c(0, 1:dim(samples)[1] )
         names(v_options) <- c('Auto reference',.C(samples[,2]))
         updateSelectInput(session, "RefSpecSelect", choices = v_options, selected=0)
   }

   ##---------------
   ## started: launched when all is started (ui & server)
   ##---------------
   output$started <- reactive({
         values$started
         return(1)
   })

   ##---------------
   ## SessInit: Init the session
   ##---------------
   output$SessInit <- reactive({
         values$sessinit
         repeat {
             if ( is.null(sessionViewer) ) {
                values$reload <- 0
                break
             }
             if ( file.exists(file.path(conf$DATASETS,sessionViewer)) ) {
                outDataViewer <<- file.path(conf$DATASETS,sessionViewer)
                INI.filename <- paste0(outDataViewer,'/',conf$Rnmr1D_INI)
                if (file.exists(INI.filename)) {
                   procParams <<- Parse.INI(INI.filename, INI.list=Spec1r.Procpar, section="PROCPARAMS")
                } else {
                   procParams <<- Spec1r.Procpar
                   generate_INI_file(procParams)
                }
                values$reload <- 1
                updateRefSpecSelect()
                break
             }
             outData <- file.path(tempdir(),sessionViewer)
             if ( file.exists(file.path(outData,'userfiles')) ) {
                outDataViewer <<- file.path(conf$DATASETS,sessionViewer)
                if ( ! file.exists(outDataViewer) ) dir.create(outDataViewer)
                system( paste("chmod 777 ",outDataViewer) )
                file.copy(file.path(outData,"userfiles"), file.path(outDataViewer,"userfiles"))
                V <- read.table(file.path(outDataViewer,"userfiles"), header=F, stringsAsFactors=FALSE)[,1]
                NameZip <<- V[1]
                ext <- tolower(gsub("^.*\\.", "", NameZip))
                RawZip <<- file.path(outData,paste0('raw.',ext))
                outDir <<- outData
                values$ziploaded <- 1
                values$reload <- 0
                break
             }
             values$reload <- 0
             break
         }
         return(values$sessinit)
   })
   outputOptions(output, 'SessInit', suspendWhenHidden=FALSE)
   outputOptions(output, 'SessInit', priority=1)

    ##---------------
    ## SessReload : Reload the session
    ##---------------
    output$SessReload <- reactive({
         values$reload
         isolate({
             if (values$reload==1) {
                 #INI.filename <- paste0(outDataViewer,'/',conf$Rnmr1D_INI)
                 #procParams <<- Parse.INI(INI.filename, INI.list=list(), section="PROCPARAMS")
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

    output$ZipPreLoaded <- reactive({
          if (values$ziploaded>0) {
             if (file.exists(file.path(outDir,conf$Rnmr1D_PPCMD))) initPreprocessingParams(file.path(outDir,conf$Rnmr1D_PPCMD))
             updateTextInput(session, "namezip", value = NameZip)
             shinyjs::disable("vendor")
          }
          return( values$ziploaded )
    })
    outputOptions(output, 'ZipPreLoaded', suspendWhenHidden=FALSE)
    outputOptions(output, 'ZipPreLoaded', priority=20)

##---------------
## Upload & Preprocessing
##---------------

   ##---------------
   ## Preprocessing : init parameters depending on macrocommand file
   ##---------------
   initPreprocessingParams <- function(macropcmdfile) {
       CMDTEXT <- gsub("\t", "", readLines(macropcmdfile))
       if ( length(grep("#%%", CMDTEXT[1]))==1 ) {
            procpar <- unlist(strsplit(gsub("#%% ", "", CMDTEXT[1]), "; "))
            parnames <- NULL; parvals <- NULL
            for (param in procpar ) {
                 parnames <- c( parnames, unlist(strsplit(param,"="))[1] ); parvals <- c( parvals, unlist(strsplit(param,"="))[2] );
            }
            names(parvals) <- parnames;  procpar <- data.frame(t(parvals), stringsAsFactors=FALSE)
            if (! is.null(procpar$Vendor)) {
               v_options <- c('bruker', 'varian','jeol', 'nmrml')
               names(v_options) <- c('Bruker', 'Varian/Agilent', 'Jeol JDF format', 'nmrML v1.0.rc1')
               v_select<-tolower(trim(procpar$Vendor))
               updateSelectInput(session, "vendor", choices = v_options, selected=v_select)
            }
            if (! is.null(procpar$Type)) {
               v_options <- c("fid","1r"); names(v_options) <- c('FID','1r spectrum');
               updateSelectInput(session, "spectype", choices = v_options, selected=trim(procpar$Type))
            }
            if (! is.null(procpar$LB))      { updateNumericInput(session, "LB", value = as.numeric(procpar$LB)); }
            if (! is.null(procpar$GB))      { updateNumericInput(session, "GB", value = as.numeric(procpar$GB)); }
            if (! is.null(procpar$PHC1))    { updateCheckboxInput(session, "optimphc1", value = ifelse( procpar$PHC1=="TRUE", 1, 0)); }
            if (! is.null(procpar$ZNEG))    { updateCheckboxInput(session, "rabot", value = ifelse( procpar$ZNEG=="TRUE", 1, 0)); }
            if (! is.null(procpar$TSP))     { updateCheckboxInput(session, "zeroref", value = ifelse( procpar$TSP=="TRUE", 1, 0)); }
            if (! is.null(procpar$O1RATIO)) { updateCheckboxInput(session, "o1param", value = ifelse( as.numeric(procpar$O1RATIO)>0, 1, 0));
                if (as.numeric(procpar$O1RATIO)>0) updateNumericInput(session, "o1ratio", value = as.numeric(procpar$O1RATIO));
            }
            if (! is.null(procpar$ZF))      { updateCheckboxInput(session, "zerofilling", value = ifelse( as.numeric(procpar$ZF)>0, 1, 0));
                if (as.numeric(procpar$ZF)>0) {
                    v_options <- c('2','4'); names(v_options) <- c("x2","x4");
                    updateSelectInput(session, "zffac", choices = v_options, selected=as.numeric(procpar$ZF))
                }
            }
       }
   }

   observeEvent ( input$macropcmd, {
       macropcmd <- input$macropcmd
       if ( ! is.null(macropcmd) ) initPreprocessingParams(macropcmd$datapath)
   })

   ##---------------
   ## Preprocessing : Launch Rnmr1D package
   ##---------------
   submit_job_preProcess <- function(procParams) {
       ret <- 1
       ErrMsg <<- "ERROR: Some information regarding raw data are not consistent. <br> Please, click the on 'Reset' button to retry."
       tryCatch({ repeat {
             # Check if is an archive file
             ext <- tolower(gsub("^.*\\.", "", RawZip))
             if ( !(ext %in% zipext)) {
                toggleModal(session, "modalWatcher1", toggle = "close")
                ErrMsg <<- "ERROR: The ZIP file must a real zip file. <br> Please, click the on 'Reset' button to retry."
                write_textlines(file.path(outDataViewer,conf$semapFileOut),"1\n")
                ret <- 0
                break
             }

             # Generate the 'samples.csv' & 'factors' files from the list of raw spectra
             LOGFILE <- file.path(outDataViewer,conf$LOGFILE0)
             if (file.exists(file.path(outDataViewer,"errorlist.csv"))) unlink(file.path(outDataViewer,"errorlist.csv"))
             write_textlines(LOGFILE, "Generate the 'samples.csv' & 'factors' files from the list of raw spectra\n")
             RET <- generate_Metadata_File(RawZip, outDataViewer, procParams)
             if (file.exists(file.path(outDataViewer,"errorlist.csv"))) {
                 write_textlines(LOGFILE, "ERROR: Some information regarding raw data are not consistent; Here is the list below\n")
                 ERRORLIST <- read.table(file.path(outDataViewer,"errorlist.csv"), header=F, sep=";", stringsAsFactors=FALSE)
                 write_textlines(LOGFILE, paste0(paste(ERRORLIST[,1], collapse="\n"),"\n") )
             }
             if ( RET == 0 ) {
                 ErrMsg <<- "ERROR: Some errors occur while attempting to generate metadata files. <br> Please, click the on 'Reset' button to retry."
                 write_textlines(file.path(outDataViewer,conf$semapFileErr), ErrMsg)
                 write_textlines(file.path(outDataViewer,conf$semapFileOut),"1\n")
                 ret <- 0
                 break
             }

             # Check if the upload macro-file is compliant with the allowed commands
             if ( file.exists(file.path(outDataViewer,conf$Rnmr1D_PCMD)) && check_MacroCmdFile(file.path(outDataViewer,conf$Rnmr1D_PCMD)) == 0 ) {
                 ErrMsg <<- "ERROR: The upload macro-file is NOT compliant with the allowed commands. <br> Please, click the on 'Reset' button to retry."
                 write_textlines(file.path(outDataViewer,conf$semapFileErr), ErrMsg)
                 write_textlines(file.path(outDataViewer,conf$semapFileOut),"1\n")
                 ret <- 0
                 break
             }

             # Generate a INI file from the list of processing parameters
             write_textlines(file.path(outDataViewer,conf$LOGFILE0),"Generate a INI file from the list of processing parameters\n")
             generate_INI_file(procParams)

             # Launch Rnmr1D ...
             cmdR <- paste(conf$Rscript_exec,"Rnmr1D -d -i ", as.character(outDir), "  -o ", as.character(outDataViewer), sep="")
             submit_Rscript(outDir, outDataViewer, cmdR)
             break
       }}, error=function(e) {
            ret <- 0
            values$error <- 1
            toggleModal(session, "modalWatcher1", toggle = "close")
            write_textlines(file.path(outDataViewer,conf$semapFileOut),"1\n")
            write_textlines(file.path(outDataViewer,conf$semapFileErr), ErrMsg)
            ERROR$MsgErrLoad <- ErrMsg
       })
       return(ret)
   }

   ##---------------
   ## Uploading file : Preparation
   ##---------------
   output$ZipUploaded <- reactive({
         return(  ifelse( is.null(input$zipfile) , 0, 1 ) )
   })
   outputOptions(output, 'ZipUploaded', suspendWhenHidden=FALSE)
   outputOptions(output, 'ZipUploaded', priority=20)

   output$fileUploaded <- reactive({
         input$goButton
         if (input$goButton==0) return(0)

         isolate({
            # Get & Rename the ZIP file
            if ( isolate({ values$ziploaded==0 }) ) {
                zipfile <- input$zipfile
                if ( is.null(zipfile) ) return (0)
                NameZip <<- zipfile$name
                outDir <<- dirname(zipfile$datapath)
                ext <- tolower(gsub("^.*\\.", "", NameZip))
                RawZip <<- paste(outDir,paste0('raw.',ext),  sep='/' )
                file.rename( zipfile$datapath, RawZip )

                # Create if not exist the directory where the data for NMRViewer will be stored
                outDataViewer <<- file.path(conf$DATASETS,sessionViewer)
                if ( ! file.exists(outDataViewer) ) dir.create(outDataViewer)
                system( paste("chmod 777 ",outDataViewer) )
            }

            samplefile <- input$samplefile
            SampleFile <<- NULL
            if ( ! is.null(samplefile) ) {
                 SampleFilename <<- samplefile$name
                 SampleFile <<- file.path(outDir,'samples.txt')
                 file.rename( samplefile$datapath, SampleFile )
                 system(paste0("rm -rf ",dirname(samplefile$datapath)))
                 listfiles <- c( NameZip, SampleFilename, SampleFile )
            } else {
                 listfiles <- c( NameZip, SampleFilename, '/tmp/RtmpNone' )
            }
            # Save the uploaded file names
            write.table( gsub( " ", "_", listfiles), file=file.path(outDataViewer,"userfiles"), sep=';', row.names=F, col.names=F, quote=F)

            # Trace the loading ...
            ZipSize <- file.info(RawZip)$size
            thedate <- paste0( format(Sys.Date(), format="%m/%d/%Y"), ' ', format(Sys.time(), format="%H:%M:%S"))
            write_textlines(conf$USER_ACCESS_FILE, paste(USER$email, thedate, sessionViewer, 'INIT', NameZip, ZipSize, sep="\t"))

            # Save the user name if not already exists
            if (is.na(USER$email)) USER$email <- 'none'
            if (! file.exists(file.path(outDataViewer,"user")) ) Write.LOG(file.path(outDataViewer,"user"),USER$email, mode="wt")

            # Save the processing file if provided
            macropcmd <- input$macropcmd
            PCMDFilename <<- NULL
            if ( ! is.null(macropcmd) ) {
                 CMDTEXT <- gsub("\t", "", readLines(macropcmd$datapath))
                 if ( length(grep( "^#%%", CMDTEXT))>0 ) {
                     CMDTEXT <- CMDTEXT[grep( "^#%%", CMDTEXT, invert=TRUE )]
                 }
                 fileConn<-file(file.path(outDataViewer,conf$Rnmr1D_PCMD))
                 writeLines(CMDTEXT, fileConn)
                 close(fileConn)
                 PCMDFilename <<- gsub( " ", "_", macropcmd$name )
                 write.table(PCMDFilename, file=file.path(outDataViewer,"pcmdfile"), sep=';', row.names=F, col.names=F, quote=F)
            }

            procJobName <<- "preprocess"
         })
         values$load <- 1
         return(1)
   })
   outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
   outputOptions(output, 'fileUploaded', priority=1)

   ##---------------
   ## Preprocessing : Start - Launch Rnmr1D package
   ##---------------
   output$fileProcessing <- reactive({
         if (values$load==0) return (0)
         isolate({
            if (values$reload==1) return (0)
            if (is.null(RawZip) ) return (0)
            if (is.null(procJobName)) return (0)
            # Init the processing parameters
            procParams <<- Spec1r.Procpar
            procParams$VENDOR <<- input$vendor
            procParams$INPUT_SIGNAL <<- input$spectype
            procParams$READ_RAW_ONLY <<- TRUE
            CMD <- paste0("#%% Vendor=",input$vendor,"; Type=",input$spectype,"; ")
            if (input$spectype=="fid") {
               procParams$READ_RAW_ONLY <<- FALSE
               procParams$LB <<- as.numeric(input$LB)
               procParams$GB <<- as.numeric(input$GB)
               procParams$REVTIME <<- ifelse(input$vendor == 'varian' || input$vendor == 'jeol', TRUE, FALSE)
               procParams$ZEROFILLING <<- ifelse(input$zerofilling==1, TRUE, FALSE)
               procParams$ZFFAC <<- as.numeric(input$zffac)
               procParams$OPTPHC0 <<- TRUE
               procParams$OPTPHC1 <<- ifelse(input$optimphc1==1, TRUE, FALSE)
               procParams$BLPHC <<- ifelse(input$optimphc1==1, TRUE, FALSE)
               procParams$RABOT <<- ifelse(input$rabot==1, TRUE, FALSE)
               procParams$TSP <<- ifelse(input$zeroref==1, TRUE, FALSE)
               CMD <- paste0(CMD, "LB=",input$LB,"; GB=",input$GB, "; ")
               CMD <- paste0(CMD, "ZF=",ifelse(input$zerofilling==1, input$zffac, 0),"; ")
               CMD <- paste0(CMD, "BLPHC=",input$optimphc1,"; PHC1=",input$optimphc1, "; FP=0; ")
               CMD <- paste0(CMD, "TSP=",input$zeroref)
               if (conf$O1_PARAM_PATCH==1 && input$o1param==1) {
                   procParams$TSP <<- FALSE
                   procParams$O1RATIO <<- as.numeric(input$o1ratio)
                   CMD <- paste0(CMD, "; O1RATIO=",input$o1ratio)
               }
            }
            write_textlines(file.path(outDataViewer,conf$Rnmr1D_PPCMD), paste0(CMD,"\n"), "wt")
            values$error <- 0
            ERROR$MsgErrLoad <- ''
            closeAlert(session, "ErrAlertLoadId")
            # Launch Rnmr1D package
            if (submit_job_preProcess(procParams)==1) {
                procJobName <<- 'preprocess'
                values$jobrun <- 1
                ERROR$MsgErrLoad <- ''
            } else {
                procJobName <<- 'preprocess'
                values$error <- 1
            }
            return(1)
         })
   })
   outputOptions(output, 'fileProcessing', suspendWhenHidden=FALSE)
   outputOptions(output, 'fileProcessing', priority=1)

   ##---------------
   ## Preprocessing : Ended
   ##---------------
   output$fileProcessed <- reactive({
         input$jobstatus
         values$error
         isolate({
            if (values$load==0) return (0)
            if ( is.null(RawZip) ) return (0)
            if (is.null(procJobName)) return (0)
            if (procJobName != "preprocess") return (1)
            if (values$error==0 && input$jobstatus != "Ended" && input$jobstatus != "Error") return (0)
            if (values$error==0) {
                toggleModal(session, "modalWatcher1", toggle = "close")
                updateRefSpecSelect()
                updateExportFormatSelect()
            }
            if (values$error==1 || ( input$jobstatus == "Error" && file.exists(paste0(outDataViewer,'/',conf$semapFileErr))) ) {
                 infoLines <- readLines(paste0(outDataViewer,'/',conf$semapFileErr))
                 ERROR$MsgErrLoad <- paste0( ErrMsg, paste(as.list(infoLines), collapse="\n"),"\n\n" )
            }
            values$jobrun <- 0
            values$proc <- 1
            return(1)
         })
   })
   outputOptions(output, 'fileProcessed', suspendWhenHidden=FALSE)
   outputOptions(output, 'fileProcessed', priority=10)

   ##---------------
   ## Output: Provides some information about loaded spectra
   ##---------------
   output$zipLog <- renderPrint({
        values$proc
        if (values$proc==0) return(NULL)
        if (values$header==0 && values$psession==1) return(NULL)
        t1 <- file.info(file.path(outDataViewer,conf$semapFileIn))$ctime
        t2 <- file.info(file.path(outDataViewer,conf$semapFileOut))$ctime
        elapsedtime <- as.numeric(difftime(t2,t1),units="secs")
        sess_name <- gsub("\\..*$", "", NameZip)
        if (conf$USRCONMGR==1 && isolate(values$reload)==1 && file.exists(file.path(outDataViewer,"user"))) {
                USER$email <<- readLines(file.path(outDataViewer,"user"))
        }
        output$title <- renderUI({ tags$table(style="width: 100%;", tags$tr(
                                   tags$td(tags$h4(paste(conf$TITLE,'- ver.',conf$VERSION))),
                                   tags$td(style="float: right;", tags$h4(sess_name)),
                                   tags$td(style="width: 15px;"," ")
                        ))})
        runjs( paste0("window.history.replaceState(null,'NMRProcFlow', '?", sessionViewer, "');") )
        output$jreload <- renderUI({ tags$script(HTML(paste0("document.title ='",gsub("\\..*$", "", NameZip), "';"))) })
        samples <- read.table(file.path(outDataViewer,"samples.csv"), header=F, sep=";", stringsAsFactors=FALSE)
        factors <- read.table(file.path(outDataViewer,"factors"), header=F, sep=";", stringsAsFactors=FALSE)

        # Trace some information ...
        SpecSize <- file.info(file.path(outDataViewer,conf$SPEC_PACKED))$size
        lbLoad <- 'LOAD'
        if (isolate(values$reload)==1) lbLoad <- 'RELOAD'
        thedate <- paste0( format(Sys.Date(), format="%m/%d/%Y"), ' ', format(Sys.time(), format="%H:%M:%S"))
        write_textlines(conf$USER_ACCESS_FILE, paste(USER$email, thedate, sessionViewer, lbLoad, NameZip, SpecSize, dim(samples)[1], sep="\t"))
        cat("Shiny Server version ", serverInfo()$version, "\n",
            "----\n",
            "Session Identifier = ", sessionViewer, "\n",
            "Instrument/Vendor/Format = ", procParams$VENDOR, "\n",
            "Spectra type = ", procParams$INPUT_SIGNAL, "\n",
            "The original name of the Zip file = ", NameZip, "\n",
            "The original name of the Samples file = ", SampleFilename, "\n",
            "The macro-command file for processing = ", PCMDFilename, "\n",
            "The number of Spectra = ", dim(samples)[1], "\n",
            "The number of Factors = ", dim(factors)[1]-1, "\n",
            "----\n",
           sep="")
   })
   outputOptions(output, 'zipLog', priority=5)

   ##---------------
   ## Output: DataTable of the spectra processing parameters
   ##---------------
   output$dataProcTable <- renderDataTable({
        values$proc
        if (values$proc==0) return(NULL)
        dataProcFile <- paste0(outDataViewer,'/list_pars.csv')
        dataProc <- NULL
        if (file.exists(dataProcFile))  {
              dataProcAll <- as.data.frame(read.table(dataProcFile, header=TRUE, sep=';'))
              if (procParams$VENDOR=="bruker") {
                 dataProc <- dataProcAll[ , c("Spectrum", "PULSE", "NUC", "SOLVENT", "EXPNO", "PROCNO", "PHC0", "PHC1", "SW", "SF", "SI") ]
              } else {
                 dataProc <- dataProcAll[ , c("Spectrum", "PULSE", "NUC", "SOLVENT", "PHC0", "PHC1", "SW", "SF", "SI") ]
              }
        }
        return(dataProc)
   }, options = list(pageLength=10, autoWidth = T, columnDefs = list(list(width = '30px', targets = c(2,4,5))) ))

   ##---------------
   ## ResetButton / Reload
   ##---------------
   AppliReload <- function(flg) {
        sessid <- paste0('_',paste(sample(c(0:9, letters[1:6]),30, replace=TRUE),collapse=""))
        outData <- file.path(tempdir(),sessid)
        if ( ! file.exists(outData) ) dir.create(outData)
        if (flg>0 && file.exists(file.path(outDataViewer,"userfiles")) && file.exists(RawZip) ) {
           #system( paste("chmod 777 ",outData) )
           if (file.exists(file.path(outDataViewer,conf$Rnmr1D_PPCMD))) {
               file.copy(file.path(outDataViewer,conf$Rnmr1D_PPCMD), file.path(outData,conf$Rnmr1D_PPCMD))
           }
           file.copy(file.path(outDataViewer,"userfiles"), file.path(outData,"userfiles"))
           file.copy(RawZip, file.path(outData,basename(RawZip)))
           file.remove(RawZip)
        }
        return(sessid)
   }
   # Reset the session with keeping the ZIP
   observeEvent( input$resetButton, {
         if (input$resetButton>0) {
            runjs( paste0("window.location.replace('?", AppliReload(1), "');") )
         }
   })
   # Full Reset
   observeEvent( input$resetFullButton, {
         if (input$resetFullButton>0) {
            runjs( paste0("window.location.replace('?", AppliReload(0), "');") )
         }
   })

   ##---------------
   ## Export Parameters
   ##---------------
   output$exportPars <- downloadHandler(
        filename = function() { paste0(strsplit(NameZip,"\\.")[[1]][1], '.txt' ) },
        content = function(file) {
             parsfile <- paste0(outDataViewer,'/list_pars.csv')
             if ( file.exists(parsfile) ) {
                PARS <- read.table(parsfile,header=T, sep=";", stringsAsFactors=FALSE)
                write.table(PARS, file, sep="\t", row.names=F, col.names=T, quote=F)
             }
        }
   )
