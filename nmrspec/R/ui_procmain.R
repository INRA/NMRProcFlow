# Processing Frame

##---------------
## NMR Spectra Viewer
##---------------
ui_viewer <- column(12, htmlOutput("nmrviewer"), htmlOutput("captoggle"), htmlOutput("captoggle2"), 
                        htmlOutput("UndoRequest"), htmlOutput("UndoBucket"), htmlOutput("resizetoggle"))


##---------------
## Rnmr1D Watcher
##---------------
ui_proc_wait <- conditionalPanel(condition="output.Processing==1", column(12, tags$div( id="loadmessage") ))
ui_proc_watcher <- bsModal("modalWatcher2", "Job Watcher","process", size="large", wellPanel(htmlOutput("watcher2")) )


##---------------
## Buttons Panel
##---------------
ui_proc_buttons <- column(4, 
     bsButton("capMode", icon=icon("toggle-off"), label = "", style="primary", type="toggle", value=FALSE ),
     bsButton("resizeView", icon=icon("unsorted"), label = "", style="primary", type="toggle", value=FALSE ),
     bsTooltip("resizeView", "Enlarge or Shrink the image height", "bottom", options = list(container = "body")),
     tags$br(), tags$br(),
     tags$br(), tags$br(),
     conditionalPanel(condition="input.condProcPanels != 'Data Export'", 
          bsButton("process", icon=icon("spinner"), label = "Launch", style="primary" ),
          checkboxInput("joblog", "Job Watcher", TRUE),
          bsTooltip("process", "Launch the current selected process", "bottom", options = list(container = "body")),
          tags$br(),
          conditionalPanel(condition="input.condProcPanels == 'Bucketing'",
              bsButton("unBucket", icon=icon("undo"), label = "Undo", style="primary" ),
              bsTooltip("unBucket", "Undo the last bucketing", "bottom", options = list(container = "body")),
              bsModal("modalUnBuc", "Undo confirmation ?", "unBucket", size = "small", actionButton("yes_unBuc", "Yes") )
          ),
          conditionalPanel(condition="input.condProcPanels != 'Bucketing'",
              bsButton("undo", icon=icon("undo"), label = "Undo", style="primary" ),
              bsTooltip("undo", "Undo the last processing", "bottom", options = list(container = "body")),
              bsModal("modalUndo", "Undo confirmation ?", "undo", size = "small", actionButton("yes_undo", "Yes") )
          ),
          tags$br(),
          bsButton("proclog", icon=icon("file-text-o"), label = "Log", style="info" ),
          bsTooltip("proclog", "View the Log file", "bottom", options = list(container = "body")),
          bsModal("modalProclog", "Log Watcher","proclog", size="large", htmlOutput("watcher2b")),
          conditionalPanel(condition="input.condProcPanels == 'Processing'",
              tags$br(),
              bsButton("cmdlog", icon=icon("file-text-o"), label = "CMD", style="info" ),
              bsTooltip("cmdlog", "View the Macro Command file", "bottom", options = list(container = "body")),
              bsModal("modalCmdlog", "Macro-Commands","cmdlog", size="large", htmlOutput("viewCmd", style = "overflow:auto;"), tags$br(), downloadButton("exportCMD", "Export Macro commands" ) )
          ),
          tags$br(), tags$br()
     )
)


##---------------
## Tabs Panel
##---------------
ui_proc_tabs <- column(12, conditionalPanel(condition="output.Processing==0",
     splitLayout(cellWidths = c("5%","74%", "20%"),
         column(1, tags$img(src="images/img_00.gif", height = 600, width = 1)),
         column(12, bsAlert("ErrAlertProc"), ui_proctabPanel ),
         ui_proc_buttons
     )
))


##---------------
## Main - Conditional: uploaded Files have been processed
##---------------
ui_proc_main <- conditionalPanel(condition="output.fileProcessed==1 || output.SessReload==1",
     ui_viewer,
     ui_proc_wait,
     conditionalPanel(condition="output.modaljoblog==1" ,ui_proc_watcher ),
     ui_proc_tabs,
     htmlOutput("updateZonelist")
)


##---------------
## Conditional: Input Files are uploaded
##---------------
ui_proc <- conditionalPanel(condition="input.conditionedPanels == 'Processing'",
    conditionalPanel(condition="output.fileUploaded==0 && output.SessReload==0",
        column(12, tags$br(), tags$p("Please, upload a ZIP file of Raw Spectra in order to launch the (pre)processing steps.") )
     ),
     ui_proc_main
)

