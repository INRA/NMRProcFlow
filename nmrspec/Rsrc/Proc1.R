##---------------
## UI initialization events
##---------------
   output$panelHeader <- reactive({
       values$header==1
   })
   outputOptions(output, "panelHeader", suspendWhenHidden = FALSE)

   observeEvent( values$psession, {
       if (values$psession==1) {
          showTab(inputId = "conditionedPanels", target = "Processing", select=TRUE)
       }
   })
   observeEvent( values$header, {
       if (values$header==0 && values$psession==1) {
          shinyjs::runjs( "document.getElementById('conditionedPanels').style.display = 'none';" )
          hideTab(inputId = "conditionedPanels", target = "Load")
       }
   })

   ## Extension for Galaxy Interactive Environment
   observeEvent( values$fgalaxy, {
       if (values$fgalaxy==1) {
          shinyjs::runjs( "document.getElementById('nogalaxy').style.display = 'none';" )
       }
       if (values$fgalaxy==2) {
          shinyjs::runjs( "document.getElementById('egalaxy').style.display = 'none';" )
       }
   })

   output$FormatSelected <- reactive({
       if (input$vendor=="sinput") return(0)
       return(1)
   })
   outputOptions(output, 'FormatSelected', suspendWhenHidden=FALSE)
   outputOptions(output, 'FormatSelected', priority=20)

   # Spectrum type depending on the vendor
   observeEvent ( input$vendor, {
       v_options <- c("fid"); names(v_options) <- c('FID'); v_select<-'fid'
       if ( input$vendor %in% c("bruker", "rs2d") ) {
            v_options[length(v_options)+1] <- '1r'; names(v_options)[length(v_options)] <- '1r spectrum';
            v_select<-'fid'
       }
       if ( input$vendor %in% c("magritek") ) {
            v_options <- c("1r"); names(v_options) <- c('1r spectrum')
            v_select<-'1r'
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
                 procParams <<- Parse.INI(INI.filename, INI.list=Spec1rProcpar, section="PROCPARAMS")
              } else {
                 procParams <<- Spec1rProcpar
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
              file.copy(RawZip, file.path(outDataViewer,basename(RawZip)))
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
                if (! file.exists(file.path(outDataViewer,"userfiles"))) {
                   outDir <<- outDataViewer
                   NameZip <<- 'noname.zip'
                }
                else {
                   V <- read.table(file.path(outDataViewer,"userfiles"), header=F, stringsAsFactors=FALSE)[,1]
                   NameZip <<- V[1]
                   SampleFilename <<- V[2]
                   outDir <<- dirname(V[3])
                   unlink(file.path(outDataViewer," zones*"))
                   if (! file.exists(outDir) ) outDir <<- outDataViewer
                }
                do.call(file.remove, list(list.files(outDataViewer, pattern="zones[0-9]_list.in", full.names=T)))
                shinyjs::runjs( paste0("document.title ='",NameZip,"';") )
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
            procpar <- unlist(strsplit(gsub("#%% ", "", CMDTEXT[1]), "; *"))
            parnames <- NULL; parvals <- NULL
            for (param in procpar ) {
                 parnames <- c( parnames, unlist(strsplit(param,"="))[1] ); parvals <- c( parvals, unlist(strsplit(param,"="))[2] );
            }
            names(parvals) <- parnames;  procpar <- data.frame(t(parvals), stringsAsFactors=FALSE)

            # Vendor
            if (! is.null(procpar$Vendor)) {
               v_options <- optionVendor # optionVendor : see Global.R
               v_select<-tolower(trim(procpar$Vendor))
               updateSelectInput(session, "vendor", choices = v_options, selected=v_select)
            }

            # Type : fid or 1r
            if (! is.null(procpar$Type)) {
               v_options <- c("fid","1r"); names(v_options) <- c('FID','1r spectrum');
               updateSelectInput(session, "spectype", choices = v_options, selected=trim(procpar$Type))
            }
            if (! is.null(procpar$LB)) {
               updateNumericInput(session, "LB", value = as.numeric(procpar$LB));
            }
            if (! is.null(procpar$GB)) { 
               updateNumericInput(session, "GB", value = as.numeric(procpar$GB));
            }

            # PHC0 & PHC1 : optim, user, file
            if (is.null(procpar$USRPHC)) procpar$USRPHC <- "FALSE";
            if (is.null(procpar$FILEPHC)) procpar$FILEPHC <- "FALSE";
            if (procpar$FILEPHC=="TRUE") procpar$USRPHC <- "TRUE";
            updateCheckboxInput(session, "usrphc", value = ifelse( procpar$USRPHC=="TRUE", 1, 0));
            updateCheckboxInput(session, "fphcfile", value = ifelse( procpar$FILEPHC=="TRUE", 1, 0));
            if (procpar$USRPHC=="FALSE" && procpar$FILEPHC=="FALSE") {
               if (! is.null(procpar$PHC0)) {
                  updateCheckboxInput(session, "optimphc0", value = ifelse( procpar$PHC0=="TRUE", 1, 0));
               }
               if (! is.null(procpar$PHC1)) { 
                  updateCheckboxInput(session, "optimphc1", value = ifelse( procpar$PHC1=="TRUE", 1, 0));
                  if (! is.null(procpar$CRIT1) && as.numeric(procpar$CRIT1) %in% c(1,2,3)) {
                     updateCheckboxInput(session, "CRITSTEP1", value = as.numeric(procpar$CRIT1));
                     v_options <- c('0','1'); names(v_options) <- c("Negative values", "Absolute Positive");
                     updateSelectInput(session, "CRITSTEP1", choices = v_options, selected=as.numeric(procpar$CRIT1))
                 }
               }
            }
            if (procpar$USRPHC=="TRUE" && procpar$FILEPHC=="FALSE") {
               if (! is.null(procpar$PHC0)) {
                  updateCheckboxInput(session, "USRPHC0", value = as.numeric(procpar$PHC0));
               }
               if (! is.null(procpar$PHC1)) {
                  updateCheckboxInput(session, "USRPHC1", value = as.numeric(procpar$PHC1));
               }
            }

            # Zero neg
            if (! is.null(procpar$ZNEG)) { 
                updateCheckboxInput(session, "rabot", value = ifelse( procpar$ZNEG=="TRUE", 1, 0));
            }

            # TSP
            if (! is.null(procpar$TSP)) {
                updateCheckboxInput(session, "zeroref", value = ifelse( procpar$TSP=="TRUE", 1, 0));
                if (procpar$TSP=="TRUE" && is.null(procpar$ADJPZTSP))
                   updateCheckboxInput(session, "adjpztsp", value = 1);
                if (procpar$TSP=="TRUE" && ! is.null(procpar$ADJPZTSP))
                   updateCheckboxInput(session, "adjpztsp", value = ifelse( procpar$ADJPZTSP=="TRUE", 1, 0));
                if (procpar$TSP=="TRUE" && ! is.null(procpar$ADJPZTSP) && ! is.null(procpar$MVPZTSP)) {
                   updateCheckboxInput(session, "mvpztsp", value = ifelse( procpar$MVPZTSP=="TRUE", 1, 0));
                   if (!is.null(procpar$DHZPZRANGE))
                       updateCheckboxInput(session, "dhzpzrange", value = as.numeric(procpar$DHZPZRANGE));
                }
            }

            # O1RATIO
            if (! is.null(procpar$O1RATIO)) {
                updateCheckboxInput(session, "o1param", value = ifelse( as.numeric(procpar$O1RATIO)>0, 1, 0));
                if (as.numeric(procpar$O1RATIO)>0) {
                   updateNumericInput(session, "o1ratio", value = as.numeric(procpar$O1RATIO));
                }
            }

            # Zero Filling
            if (! is.null(procpar$ZF)) {
                updateCheckboxInput(session, "zerofilling", value = ifelse( as.numeric(procpar$ZF)>0, 1, 0));
                if (as.numeric(procpar$ZF)>0 && as.numeric(procpar$ZF) %in% c(2,4,8)) {
                    v_options <- c('2','4','8'); names(v_options) <- c("x2","x4","x8");
                    updateSelectInput(session, "zffac", choices = v_options, selected=as.numeric(procpar$ZF))
                }
            }

            # Advance parameters
            if (! is.null(procpar$ADV) && grepl(':',procpar$ADV)) {
                updateCheckboxInput(session, "advparam", value = 1);
                updateTextInput(session,"cmdparam", value=trim(gsub(':','=',procpar$ADV)))
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
   submit_job_preProcess <- function() {
       ret <- 1
       ErrMsg <<- "ERROR: "
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
                 ErrMsg <<- "ERROR: Some errors occur while attempting to generate metadata files. <br> Please, click the on 'Log' button to see.<br>"
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
             cmdR <- paste(conf$Rscript_exec,"Rnmr1D ",optDebug," -i ", as.character(outDir), "  -o ", as.character(outDataViewer), sep="")
             RET <- submit_Rscript(outDir, outDataViewer, cmdR)
             if (RET!=0) {
                 ErrMsg <<- "ERROR: error occured while launching"
                 write_textlines(file.path(outDataViewer,conf$semapFileErr), ErrMsg)
                 write_textlines(file.path(outDataViewer,conf$semapFileOut),"1\n")
                 ret <- 0
             }
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
         Sys.sleep(1)
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
                file.copy(RawZip, file.path(outDataViewer,basename(RawZip)))
            }

            samplefile <- input$samplefile
            SampleFile <<- NULL
            if ( ! is.null(samplefile) ) {
                 SampleFilename <<- samplefile$name
                 SampleFile <<- file.path(outDir,'samples.txt')
                 file.rename( samplefile$datapath, SampleFile )
                 system(paste0("rm -rf ",dirname(samplefile$datapath)))
                 listfiles <- c( NameZip, SampleFilename, SampleFile, 'NA', '/tmp/RtmpNone' )
            } else {
                 listfiles <- c( NameZip, SampleFilename, '/tmp/RtmpNone', 'NA', '/tmp/RtmpNone' )
            }
            # Save the uploaded file names
            write.table( gsub( " ", "_", listfiles), file=file.path(outDataViewer,"userfiles"), sep=';', row.names=F, col.names=F, quote=F)

            phcfile <- input$phcfile
            PHCfile <<- NULL
            if ( ! is.null(phcfile) ) {
                 PHCfilename <<- phcfile$name
                 PHCfile <<- file.path(outDataViewer,'phc.txt')
                 file.copy(phcfile$datapath, PHCfile)
                 system(paste0("rm -rf ",dirname(phcfile$datapath)))
                 listfiles <- c( NameZip, SampleFilename, SampleFile, PHCfilename, PHCfile )
            } else {
                 listfiles <- c( NameZip, SampleFilename, '/tmp/RtmpNone', PHCfilename, '/tmp/RtmpNone' )
            }

            # Trace the loading ...
            ZipSize <- file.info(RawZip)$size
            thedate <- paste0( format(Sys.Date(), format="%m/%d/%Y"), ' ', format(Sys.time(), format="%H:%M:%S"))
            write_textlines(conf$USER_ACCESS_FILE, paste(USER$email, thedate, sessionViewer, 'INIT', NameZip, ZipSize, sep="\t"))

            # Get the file containing the user email if not already exists
            tmpSession <- file.path(tempdir(),sessionViewer)
            if ( file.exists(file.path(tmpSession,"user")) && ! file.exists(file.path(outDataViewer,"user")) )
                file.copy(file.path(tmpSession,"user"), file.path(outDataViewer,"user"))

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
            procParams <<- Spec1rProcpar
            procParams$VENDOR <<- input$vendor
            procParams$INPUT_SIGNAL <<- input$spectype
            procParams$READ_RAW_ONLY <<- TRUE
            procParams$PHCFILE <<- FALSE
            procParams$CPREGEX <<- conf$CPREGEX
            CMD <- paste0("#%% Vendor=",input$vendor,"; Type=",input$spectype,"; ")
            if (input$spectype=="fid") {
                procParams$READ_RAW_ONLY <<- FALSE
                procParams$LB <<- as.numeric(input$LB)
                procParams$GB <<- as.numeric(input$GB)
                procParams$OC <<- FALSE
                procParams$JGD_INNER <<- TRUE
                procParams$PHCFILE <<- ifelse(input$fphcfile==1, TRUE, FALSE)
                USROK <- ifelse( input$usrphc==1 && input$fphcfile==1 && is.null(PHCfile), 0, 1 )
                if (USROK==1 && input$usrphc==1) {
                   procParams$OPTPHC0 <<- FALSE
                   procParams$OPTPHC1 <<- FALSE
                   if (input$fphcfile==0) {
                       procParams$phc0 <<- as.numeric(input$USRPHC0)*pi/180
                       procParams$phc1 <<- as.numeric(input$USRPHC1)*pi/180
                   } else {
                       procParams$phc0 <<- 0
                       procParams$phc1 <<- 0
                   }
                } else {
                   procParams$OPTPHC0 <<- ifelse(input$optimphc1==0, TRUE, FALSE) # TRUE
                   procParams$OPTPHC1 <<- ifelse(input$optimphc1==1, TRUE, FALSE)
                   procParams$CRITSTEP1 <<- 0 # as.numeric(input$CRITSTEP1)
                   procParams$OPTCRIT1 <<- 2
                   procParams$BLPHC <<- 50
                   procParams$KSIG <<- 2
                   procParams$GAMMA <<- 0.005
                }
                procParams$RATIOPOSNEGMIN <<- 0.45
                procParams$KZEROTHRES <<- 250
                procParams$REVPPM <<- ifelse(input$vendor == 'varian' || input$vendor == 'jeol', TRUE, FALSE)
                procParams$ZEROFILLING <<- ifelse(input$zerofilling==1, TRUE, FALSE)
                procParams$ZFFAC <<- as.numeric(input$zffac)
                procParams$RABOT <<- ifelse(input$rabot==1, TRUE, FALSE)
                procParams$TSP <<- ifelse(input$zeroref==1, TRUE, FALSE)
                procParams$ADJPZTSP <<- ifelse(input$adjpztsp==1, TRUE, FALSE)
                #if (input$adjpztsp==1)
                #   procParams$DHZPZTSP <<- as.numeric(input$dhzpztsp)
                procParams$MVPZTSP <<- ifelse(input$mvpztsp==1, TRUE, FALSE)
                if (input$mvpztsp==1)
                   procParams$DHZPZRANGE <<- as.numeric(input$dhzpzrange)

                # Init the processing parameters
                CMD <- paste0(CMD, "LB=",input$LB,"; GB=",input$GB, "; " )
                CMD <- paste0(CMD, "ZF=",ifelse(input$zerofilling==1, input$zffac, 0),"; ")
                if (USROK==1 && input$usrphc==1) {
                   CMD <- ifelse( input$fphcfile==0, paste0(CMD, "USRPHC=TRUE; "), paste0(CMD, "FILEPHC=TRUE; ") )
                   CMD <- paste0(CMD, "PHC0=",as.numeric(input$USRPHC0), "; ")
                   CMD <- paste0(CMD, "PHC1=",as.numeric(input$USRPHC1), "; ")
                } else {
                   CMD <- paste0(CMD, "PHC1=",input$optimphc1, "; ")
                   CMD <- paste0(CMD, "CRIT1=",input$CRITSTEP1, "; ")
                }
                CMD <- paste0(CMD, "TSP=",input$zeroref)
                if (input$zeroref==1) {
                    CMD <- paste0(CMD, "; ADJPZTSP=",procParams$ADJPZTSP)
                }
                #if (input$zeroref==1 && input$adjpztsp==1) {
                #    CMD <- paste0(CMD, "; DHZPZTSP=",procParams$DHZPZTSP)
                #}
                if (input$zeroref==1 && input$mvpztsp==1) {
                    CMD <- paste0(CMD, "; MVPZTSP=",procParams$MVPZTSP)
                    CMD <- paste0(CMD, "; DHZPZRANGE=",procParams$DHZPZRANGE)
                }
                if (input$o1param==1) {
                    procParams$TSP <<- FALSE
                    procParams$O1RATIO <<- as.numeric(input$o1ratio)
                    CMD <- paste0(CMD, "; O1RATIO=",procParams$O1RATIO)
                }
                # Avanced Parameters
                if (input$advparam==1) optDebug <<- ifelse(input$debugProc==1, '-d', '')
                if (input$advparam==1 && grepl('=',input$cmdparam)) {
                    V <- as.vector(simplify2array(strsplit(input$cmdparam,",")))
                    for (k in 1:length(V))
                         if (grepl('=',V[k]))
                             procParams[[ trim(strsplit(V[k],"=")[[1]][1]) ]] <<- trim(strsplit(V[k],"=")[[1]][2])
                    CMD <- paste0(CMD, "; ADV=",trim(gsub('=',':',input$cmdparam)))
                }
            }
            write_textlines(file.path(outDataViewer,conf$Rnmr1D_PPCMD), paste0(CMD,"\n"), "wt")
            values$error <- 0
            ERROR$MsgErrLoad <- ''
            closeAlert(session, "ErrAlertLoadId")
            # Launch Rnmr1D package
            if (submit_job_preProcess()==1) {
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
            f <- list.files(outDir, include.dirs = F, full.names = T, recursive = T)
            file.remove(f)
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
        shinyjs::runjs( paste0("window.history.replaceState(null,'NMRProcFlow', '?", sessionViewer, "'); document.title ='",NameZip,"';") )
        output$jreload <- renderUI({ tags$script(HTML(paste0("document.title ='",gsub("\\..*$", "", NameZip), "';"))) })
        samples <- read.table(file.path(outDataViewer,"samples.csv"), header=F, sep=";", stringsAsFactors=FALSE)
        factors <- read.table(file.path(outDataViewer,"factors"), header=F, sep=";", stringsAsFactors=FALSE)

        # Set a default ppm range for noise
        NOISERANGE <<- get_noiserange()
		updateTextAreaInput(session, "ppmnoiserange", value = NOISERANGE)
		updateTextAreaInput(session, "ppmnoiserange2", value = NOISERANGE)
		updateTextAreaInput(session, "ppmnoiserange3", value = NOISERANGE)
		updateTextAreaInput(session, "ppmnoiserange4", value = NOISERANGE)
		updateTextAreaInput(session, "ppmnoiserange4", value = NOISERANGE)
		updateTextAreaInput(session, "ppmsnrnoise", value = NOISERANGE)

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
            "The number of Spectra = ", nrow(samples), "\n",
            "The number of Factors = ", nrow(factors)-1, "\n",
			#"The defaut ppm noise range = [",paste0(NOISERANGE,collapse=","),"]\n",
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
              repeat {
                 if (procParams$VENDOR=="bruker") {
                    dataProc <- dataProcAll[ , c("Spectrum", "PULSE", "NUC", "SOLVENT", "EXPNO", "PROCNO", "PHC0", "PHC1", "SW", "SF", "SI") ]
                    break
                 }
                 if (procParams$VENDOR=="rs2d") {
                    dataProc <- dataProcAll[ , c("Spectrum", "PULSE", "NUC", "SOLVENT", "PROCNO", "PHC0", "PHC1", "SW", "SF", "SI") ]
                    break
                 }
                 dataProc <- dataProcAll[ , c("Spectrum", "PULSE", "NUC", "SOLVENT", "PHC0", "PHC1", "SW", "SF", "SI") ]
                 break
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
        if (flg>0 && file.exists(file.path(outDataViewer,"userfiles")) ) {
           file.copy(file.path(outDataViewer,"userfiles"), file.path(outData,"userfiles"))
           if (file.exists(file.path(outDataViewer,conf$Rnmr1D_PPCMD)))
               file.copy(file.path(outDataViewer,conf$Rnmr1D_PPCMD), file.path(outData,conf$Rnmr1D_PPCMD))
           V <- read.table(file.path(outDataViewer,"userfiles"), header=F, stringsAsFactors=FALSE)[,1]
           ext <- tolower(gsub("^.*\\.", "", V[1]))
           RawZip <- file.path(outDataViewer,paste0('raw.',ext))
           if (file.exists(RawZip))
               file.copy(RawZip, file.path(outData,basename(RawZip)))
        }
        return(sessid)
   }
   # Reset the session with keeping the ZIP
   observeEvent( input$resetButton, {
         if (input$resetButton>0) {
            shinyjs::runjs( paste0("window.location.replace('?", AppliReload(1), "');") )
         }
   })
   # Full Reset
   observeEvent( input$resetFullButton, {
         if (input$resetFullButton>0) {
            shinyjs::runjs( paste0("window.location.replace('?", AppliReload(0), "');") )
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
