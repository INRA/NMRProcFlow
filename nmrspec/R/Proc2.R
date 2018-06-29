##---------------
## Watcher
##---------------

   ##---------------
   ## Output: Watch the logfile in realtime
   ##---------------
   renderWatcher <- function (flg,type, height) {
         if (flg==0) return(NULL)
         urlwatcher <- paste0(conf$WatcherURL, sessionViewer,'/',type,'/',digest(runif(1, 1, 10)))
         if (nchar(conf$PROXY_URL_ROOT)>0) urlwatcher <- paste0(conf$PROXY_URL_ROOT,'/',urlwatcher)
         return(HTML(paste0('<iframe id="ifwatcher" name="ifwatcher" src=',urlwatcher,' frameborder="0" style="overflow: hidden; height: ',height,'px; width: 100%; border: 0px;" width="100%" onload="this.contentWindow.document.documentElement.scrollTop=1000"></iframe>')))
   }

   ##---------------
   ## Output: Watch the logfile in realtime
   ##---------------
   ## Watcher 1: Preprocessing
   output$watcher <- renderUI({
        values$jobrun
        values$error
        if (is.null(procJobName)) return (NULL)
        if (isolate(input$goButton)>1 && procJobName != "preprocess") return (NULL)
        renderWatcher(values$jobrun || values$error, 'init', 600)
   })
   outputOptions(output, 'watcher', suspendWhenHidden=FALSE)
   outputOptions(output, 'watcher', priority=5)

   ## Watcher 1b: Preprocessing
   output$watcher1b <- renderUI({
        renderWatcher(1,'view',600)
   })

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
             if (input$condProcPanels == 'Processing') logtype <- 'proc'
             if (input$condProcPanels == 'Bucketing') logtype <- 'bucket'
             if (input$condProcPanels == 'Data Export') logtype <- 'export'
             renderWatcher(values$jobrun,logtype,600)
         })
    })
    outputOptions(output, 'watcher2', suspendWhenHidden=FALSE)
    outputOptions(output, 'watcher2', priority=5)

    ## Watcher 2b: Watch the logfile in shift time
    output$watcher2b <- renderUI({ 
         input$proclog
         isolate({
             if (is.null(procJobName)) return (NULL)
             html <- ''
             if (input$condProcPanels == 'Processing') LOGFILE <- conf$LOGFILE
             if (input$condProcPanels == 'Bucketing') LOGFILE <- conf$LOGFILE2
             if (input$condProcPanels == 'Data Export') LOGFILE <- conf$LOGFILE3
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

##---------------
## NMRViewer & Capture
##---------------

   ##---------------
   ## Output: the NMR Spectra viewer within an iframe ("ifspecview")
   ##---------------
   renderViewer <- function (flg) {
         if (flg==0) return(NULL)
         urlviewer <- paste0(conf$ViewerURL, sessionViewer)
         if(nchar(conf$PROXY_URL_ROOT)>0) urlviewer <- paste0(conf$PROXY_URL_ROOT,'/',urlviewer)
         return(HTML(paste0('<iframe id="ifspecview" name="ifspecview" src=',urlviewer,' frameborder="0" style="overflow: hidden; min-height:',conf$Viewer_min_Height,'px; width: 100%; border: 0px;" width="100%"></iframe><script>setTimeout(function(){viewerLoaded=1; toggle_capturemode();}, 1000);</script>')))
   }

   ##---------------
   ## Alert Box
   ##---------------
   observe ({
       ERROR$MsgErrProc
       if (nchar(ERROR$MsgErrProc)>0) {
          createAlert(session, "ErrAlertProc", "ErrAlertProcId", title = "", content = ERROR$MsgErrProc, append = FALSE, style='danger')
       }
   })

   ##---------------
   ## Alert Box
   ##---------------
   observe ({
       ERROR$MsgUpload
       if (nchar(ERROR$MsgUpload)>0) {
          createAlert(session, "AlertUpLoad", "AlertUpLoadId", title = "", content = ERROR$MsgUpload, append = FALSE, style=MsgStyle)
       }
   })

   observe ({
         input$removeHeader
         if (input$removeHeader==TRUE && isolate({ values$load })==1 ) {
             updateButton(session, "removeHeader", icon = icon("angle-double-down"), value = TRUE)
             runjs( "document.getElementById('conditionedPanels').style.display = 'none'; document.getElementById('lHeader').style.display = 'none';" )
             removeTooltip(session, "removeHeader")
             #addTooltip(session, "removeHeader", "Enable / Disable the banner and the main tabs", options = list(container = "body") )
             hideTab(inputId = "conditionedPanels", target = "Load")
         }
         if (input$removeHeader==FALSE && isolate({ values$load })==1 ) {
             updateButton(session, "removeHeader", icon = icon("angle-double-up"), value = FALSE)
             runjs( "document.getElementById('conditionedPanels').style.display = 'block';  document.getElementById('lHeader').style.display = 'block';" )
             #removeTooltip(session, "removeHeader")
             addTooltip(session, "removeHeader", "Enable / Disable the banner and the main tabs", options = list(container = "body") )
             showTab(inputId = "conditionedPanels", target = "Load")
         }
   })

   ##---------------
   ## capMode - switch On/Off
   ##---------------
   observe ({
         input$capMode
         if (input$capMode==TRUE) {
             updateButton(session, "capMode", icon = icon("toggle-on"), value = TRUE)
             addTooltip(session, "capMode", title="Switch OFF the Capture Mode", placement="bottom", options = list(container = "body"))
         } else {
             updateButton(session, "capMode", icon = icon("toggle-off"), value = FALSE)
             addTooltip(session, "capMode", title="Switch ON the Capture Mode", placement="bottom", options = list(container = "body"))
         }
   })

   observe ({
         input$jobstatus
         input$yes_undo
         isolate({
              if (procJobName == "process" && input$jobstatus == "Ended") {
                 nbStackedProc <- get_maxSTACKID(outDataViewer,conf$SPEC_PACKED)
                 bsUndoLabel <- ifelse(nbStackedProc>0, paste0(' (',as.character(nbStackedProc),')'), '')
                 updateButton(session, "undo", icon=icon("undo"), label = paste0('Undo',bsUndoLabel), style="primary")
              }
         })
   })

   observe ({
         input$jobstatus
         input$yes_unBuc
         isolate({
              if (procJobName == "process" && input$jobstatus == "Ended") {
                 nbStackedBuc <- get_maxSTACKID(outDataViewer,conf$Rnmr1D_BCMD)
                 bsUndoLabel <- ifelse(nbStackedBuc>0, paste0(' (',as.character(nbStackedBuc),')'), '')
                 updateButton(session, "unBucket", icon=icon("undo"), label = paste0('Undo',bsUndoLabel), style="primary")
              }
         })

   })

   ##---------------
   ## Output: the NMR Spectra viewer within an iframe ("ifspecview")
   ##  - this render is loaded when the data loading and the data processing are done (i.e. values$proc==1)
   ##  - the NMR Spectra viewer is reloaded within the "ifspecview" iframe.
   ##---------------
   output$nmrviewer <- renderUI({
         values$proc
         isolate({ renderViewer(values$proc); })
   })

   ##---------------
   ## Output: Javascript capture events - switch On/Off
   ##---------------
   output$captoggle <- renderUI({
         input$capMode
         if (input$capMode) {
             HTML(paste0('<script>CaptureMode=1; toggle_capturemode();</script>'))
         } else {
             HTML(paste0('<script>CaptureMode=0; toggle_capturemode();</script>'))
         }
   })

   ##---------------
   ## Output: Refresh Spectra viewer
   ##    When the user has clicked on the "View Selected Zones" (input$viewZones==1)
   ##    The "bucket_list.in" file that gathering the whole selected zones is created
   ##    and put in the shared data repository (conf$DATASETS). This file is then used by
   ##    the NMRviewer application in order to draw the shaded boxes.
   ##---------------
   output$captoggle2 <- renderUI({
         input$ppmrange1
         input$ppmrange2
         input$ppmrange3
         input$ppmrange4
         input$ppmrange5
         input$vsb_range
         tryCatch({
             html <- ""
             write_selectZonesToFile(input$ppmrange1,'zones1_list.in');
             write_selectZonesToFile(input$ppmrange2,'zones2_list.in');
             write_selectZonesToFile(input$ppmrange3,'zones3_list.in');
             write_selectZonesToFile(input$ppmrange4,'zones4_list.in');
             write_selectZonesToFile(input$ppmrange5,'zones5_list.in');
             write_selectZonesToFile(input$vsb_range,'vsb_list.in');
             html <- paste0('<script>refresh_if_capturebox();</script>')
         }, error=function(e) {})
         HTML(html)
   })
   outputOptions(output, 'captoggle2', suspendWhenHidden=FALSE)
   outputOptions(output, 'captoggle2', priority=1)

   output$updateZonelist <- renderUI({
         values$updatevent
         if (values$updatevent==0) return(NULL)
         jscript <- paste0("// ",format(Sys.time(),"%H:%M:%S")," \n")
         jscript <- paste(jscript, getUpdateTextAreaScript('ppmrange1',outDataViewer, 'zones1_list.in'), sep="\n" )
         jscript <- paste(jscript, getUpdateTextAreaScript('ppmrange2',outDataViewer, 'zones2_list.in'), sep="\n" )
         jscript <- paste(jscript, getUpdateTextAreaScript('ppmrange3',outDataViewer, 'zones3_list.in'), sep="\n" )
         jscript <- paste(jscript, getUpdateTextAreaScript('ppmrange4',outDataViewer, 'zones4_list.in'), sep="\n" )
         jscript <- paste(jscript, getUpdateTextAreaScript('ppmrange5',outDataViewer, 'zones5_list.in'), sep="\n" )
         jscript <- paste(jscript, getUpdateTextAreaScript('vsb_range',outDataViewer, 'vsb_list.in'), sep="\n" )
         HTML(paste0('<script>',jscript,'</script>'))
   })
   outputOptions(output, 'updateZonelist', suspendWhenHidden=FALSE)
   outputOptions(output, 'updateZonelist', priority=1)

   output$resizetoggle <- renderUI({
         input$resizeView
         imgheight <- ifelse( input$resizeView==TRUE, conf$IMGHEIGHT_MAX, conf$IMGHEIGHT)
         html <- paste0('<script>resize_spectrumHeight(',imgheight,');</script>')
         HTML(html)
   })
   outputOptions(output, 'resizetoggle', suspendWhenHidden=FALSE)
   outputOptions(output, 'resizetoggle', priority=1)
