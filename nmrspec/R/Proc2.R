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
         return(HTML(paste0('<iframe id="ifspecview" name="ifspecview" src=',urlviewer,' frameborder="0" style="overflow: hidden; min-height:',conf$Viewer_min_Height,'px; width: 100%; border: 0px;" width="100%"></iframe><script>setTimeout(function(){toggle_capturemode();}, 1000);</script>')))
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
