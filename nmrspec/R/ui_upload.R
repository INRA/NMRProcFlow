# Load Frame

##---------------
## Description
##---------------
ui_desc <- column(12,
    tags$br(),
    tags$p("An easy graphical tool dedicated to 1D NMR spectra processing for metabolomics"),
    tags$hr()
)

##---------------
## Upload Formular
##---------------
ui_load_form <-  conditionalPanel(condition="output.fileUploaded==0 && output.SessReload==0",
    column(4,
        selectInput("vendor", "Instrument/Vendor/Format:",  c("Select the input format"="sinput", "nmrML v1.0.rc1"="nmrml", "Bruker (TopSpin/X-winnmr)" = "bruker", "Varian/Agilent (VNMRJ)" = "varian", "Jeol (JDF/DELTA)" = "jeol"), selected = "sinput"),
        conditionalPanel(condition="output.FormatSelected==1",
             selectInput("spectype", "Spectra type:", 
                          c("1r spectrum" = "1r", "FID" = "fid"), selected = "fid"),
             conditionalPanel(condition="input.spectype=='fid'",
                 bsButton("bsadvusers", icon=icon("gear"), label = " Parameters", style="primary" ),
                 tags$br(),tags$br()
             ),
             
             conditionalPanel(condition="output.ZipPreLoaded==0",
                 fileInput( 'zipfile', 'ZIP file', accept = c( 'application/zip', '.7z' ) )
             ),
             conditionalPanel(condition="output.ZipPreLoaded==1",
                 disabled( textInput( 'namezip', '') )
             ),
             
             fileInput( 'samplefile', 'Samples file (Tabular format)', accept = c( 'text/plain' ) ),
             
             conditionalPanel(condition="output.ZipUploaded==1 || output.ZipPreLoaded==1",
                 checkboxInput("advancedUsr", "Advanced User", FALSE),
                 conditionalPanel(condition="input.advancedUsr==1",
                     fileInput( 'macropcmd', 'Macro-commands for processing (Text file)', accept = c( 'text/plain' ) )
                 ),
                 column(2, bsButton("goButton", label = "Launch", style="primary" ) ),
                 column(2, conditionalPanel(condition="output.ZipPreLoaded==1",
                     bsButton("resetFullButton", label = "Reset", style="primary" )
                 ), tags$br(),tags$br() )
             )
        ),
        column(12, withTags(table(align="left", tr(td(a(img(src="images/help.png"), href="http://nmrprocflow.org/c1", target="_blank")),
                                        td(valign="center", tags$p(class="cprght", "Get more information on input data format "))))))
    ),
    conditionalPanel(condition="input.spectype=='fid'",
        bsModal("modalAdvusers", "Pre-processing Parameters", "bsadvusers", size="large",
            numericInput("LB", "Exp. Line Broadening:", 0.3, min = -1, max = 2, step=0.1),
            numericInput("GB", "Gauss. Line Broadening:", 0, min = 0, max = 1, step=0.1),
            checkboxInput("zerofilling", "Zero filling", TRUE),
            conditionalPanel(condition="input.zerofilling==1",
                  selectInput("zffac", "Max factor for Zero Filling:", c("x4"="4" , "x2"="2" ), selected = "2")
            ),
            checkboxInput("optimphc1", "first order phase setting", FALSE),
            checkboxInput("rabot", "Zeroing of Negative Values", FALSE),
            checkboxInput("zeroref", "TSP/TMS/DSS", FALSE)
        )
    ),
    column(8,
        img(src="images/NMRProcFlow_img-1.png", height = 430, width = 650)
    )
)


ui_ErrLoad <- conditionalPanel(condition="output.fileUploaded==1", column(6, bsAlert("ErrAlertLoad") ) )

##---------------
## Watcher
##---------------

ui_load_wait <- conditionalPanel(condition="output.fileUploaded==1 && output.fileProcessed==0", column(12, tags$div( id="loadmessage") ) )
ui_load_watcher <- bsModal("modalWatcher1", "Log Watcher","goButton", size="large", wellPanel(htmlOutput("watcher")) )


##---------------
## Upload Report
##---------------
ui_load_report <- conditionalPanel(condition="output.fileProcessed==1 || output.SessReload==1",
    column(6, 
        verbatimTextOutput("zipLog"), tags$br(), tags$br(),
        uiOutput("reload"), 
        bsButton("resetButton", label = "Reset", style="primary" ),
        bsButton("loadlog", label = "Log", type="action", style="primary" ),
        bsModal("modalLoadlog", "Job Watcher", "loadlog", htmlOutput("watcher1b"), size="large" ),
        downloadButton("exportPars", "Export Parameters" ),
        bsTooltip("exportPars", "Download the spectra parameter table", "bottom", options = list(container = "body"))
    ),
    column(6,
        img(src="images/NMRSpecViewer_2.png", height = 300, width = 500)
    )
)

ui_table_report <- conditionalPanel(condition="output.fileProcessed==1 || output.SessReload==1",
    column(12, dataTableOutput("dataProcTable") )
)

##---------------
## Main
##---------------
ui_upload <- conditionalPanel(condition="input.conditionedPanels == 'Load'",
    ui_desc,
    column(12,
        ui_load_form,
        ui_load_wait,
        ui_load_watcher,
        ui_load_report,
        ui_ErrLoad,
        ui_table_report
    )
)
