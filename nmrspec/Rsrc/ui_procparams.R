# Processing parameters 

##---------------
## samples
##---------------
ui_proc_samples <- column(12,
     tags$br(),
     column(3,
         # selection
          radioButtons("tsamples", "Sample annotation",
             c("Import Samples file" = "importSamples",
               "Add/Modify a Factor" = "addfactor",  #"Reduce Samplecode" = "modSamplecode",
               "Export Samples" = "exportSamples"
               ), selected = "importSamples")
     ),
     column(9,
# Import Samples file
        conditionalPanel(condition="input.tsamples=='importSamples'",
        column(4,
             tags$br(),
             checkboxInput("fsamplappend", "Append the new factors", FALSE),
             selectInput("importFormat2", "Import Format",
                    c("Comma Separator Value (CSV)" = "csv", "Semicolon Separator Value (CSV)" = "ssv", 
                      "Tabular Separator Value (TXT)" = "tsv" ), selected = "tsv"),
             fileInput( 'importSamplfile', 'Samples file', accept = c( 'text/plain' ) )
        ),
        column(4,
             tags$br(),
             checkboxInput("fsamplheader", "Header", TRUE),
             selectInput("FacCol", "First Factor",
                    c("None" = "0", "Column 3" = "3", "Column 4" = "4", "Column 5" = "5", 
                      "Column 6" = "6", "Column 7" = "7", "Column 8" = "8" ), selected = "5"),
             tags$br(),
             conditionalPanel(condition="input.FacCol!=0",
                checkboxInput("FacOnlyOne", "Only one factor", FALSE)
             ),
             conditionalPanel(condition="output.importSAMPL==1",
                bsButton("bImpSampl", label = "Import Samples", style="primary" )
             )
        )
        ),
# Add/Modify a Factor
        conditionalPanel(condition="input.tsamples=='addfactor'",
        column(3,
             tags$br(),
             #tags$strong("Factor name (optional)",  class="textlabs"), tags$br(), 
             #inputTextarea("Facname", supclass="textsimple", nrows=1, ncols=18, value=""), tags$br(), tags$br(),
             autocomplete_input(id="Facname", label="Factor name (optional)", options=NULL , value = "", width = NULL,
                                placeholder = NULL, max_options = 0, hide_values = FALSE, create = TRUE ),
             tags$strong("PPM range",  class="textlabs"), tags$br(),
             inputTextarea("ppm_facrange", supclass="single", nrows=1, ncols=18, value=""), tags$br()
        ),
        column(3,
             tags$br(),
             numericInput("facnblevel", "Nb Levels:", 2, min = 2, max = 10, step=1),
             checkboxInput("ffacappend", "Append the new factors:", TRUE), tags$br()
        ),
        column(3,
             tags$br(), tags$br(),
             bsButton("bAddFactor", label = "Apply", style="primary" )
        )
        ),
# Reduce Samplecode
        conditionalPanel(condition="input.tsamples=='modSamplecode'",
        column(4,
             tags$br(),
             selectInput("sepField", "field separating character",
                    c("Minus" = "1", "Underscore" = "2", "dot" = "3", "blank" = "4" ), selected = "tsv"),
             numericInput("numField", "Field to be kept", 1, min = 1, max = 10, step=1),
             bsButton("bModSampl", label = "Apply", style="primary" )
        )
        ),
# Export Samples
        conditionalPanel(condition="input.tsamples=='exportSamples'",
        column(4,
             tags$br(),
             selectInput("exportFormat2", "Export Format",
                    c("Comma Separator Value (CSV)" = "csv", "Semicolon Separator Value (CSV)" = "ssv", 
                      "Tabular Separator Value (TXT)" = "tsv" ), selected = "tsv"),
             downloadButton("bExportSampl", "Export Samples" )
        )
        )
     )
)

##---------------
## Processing
##---------------
ui_proc_process <- column(12,
     tags$br(),
     column(3,
         # Preprocessing type selection
          radioButtons("tpreproc", "Processing Type:",
             c("PPM calibration" = "calibration",
               "Normalisation" = "normalisation",
               "Baseline correction" = "baseline",
               "Alignment" = "ppmalign",
               "PPM shift" = "ppmshift",
               "Zeroing" = "ppmzero"), selected = "baseline")
#               "Denoising" = "denoising"), selected = "baseline")
     ),
     column(9,
         # PPM calibration
         conditionalPanel(condition="input.tpreproc=='calibration'",
              column(3,
                  tags$strong('Range of the PPM reference:', class="textlabs"),tags$br(), inputTextarea("ppmrefrange", supclass="single", nrows=1, ncols=18, value=""),
                  numericInput("ppmref", "PPM value of the center of resonance:", 0, min = 0, max = 12, step=0.1),
                  tags$strong('noisy PPM range:', class="textlabs"),tags$br(), inputTextarea("ppmnoiserange4", supclass="single noise", nrows=1, ncols=18, value="10.5 10.2"),
                  bsTooltip("ppmnoiserange4", "select a PPM range for estimate the standard deviation of the noise", "bottom", options = list(container = "body"))
              )
         ),
         # Normalisation
         conditionalPanel(condition="input.tpreproc=='normalisation'",
              column(4,
                  selectInput("normeth", "Normalization Method:",
                         c("Constant Sum Normalization" = "CSN", "Probabilistic Quotient Normalization" = "PQN"), selected = "CSN")
              ),
              column(4,
                  tags$strong('Reference PPM ranges:', class="textlabs"),tags$br(), inputTextarea("ppmrange1", supclass="capture treset", nrows=10, ncols=22, value="")
              )
         ),
         # baseline correction
         conditionalPanel(condition="input.tpreproc=='baseline'",
              # Select Correction type & Noise area
              column(4,
                  selectInput("bctype", "Type of Correction", c("Global Correction" = "1", "Local Correction" = "4", "q-NMR" = "3"), selected = "1"),
                  tags$strong('noisy PPM range:', class="textlabs"),tags$br(), inputTextarea("ppmnoiserange", supclass="single noise", nrows=1, ncols=18, value="10.5 10.2"),
                  bsTooltip("ppmnoiserange", "select a PPM range for estimate the standard deviation of the noise", "bottom", options = list(container = "body"))

              ),
              column(4,
              # Local & qNMR Correction
                  conditionalPanel(condition="input.bctype>1",
                     tags$strong("Restricted PPM range:",  class="textlabs"), tags$br(), 
                     inputTextarea("ppmbcrange", supclass="single", nrows=1, ncols=18, value=""),
                     bsTooltip("ppmbcrange", "Restricted PPM range for applying the baseline correction", "bottom", options = list(container = "body")),
                     tags$br(), tags$br()
                  ),
              # Global Correction
                  conditionalPanel(condition="input.bctype==1",
                    selectInput("gbclev", "Level of Correction",
                         c("Soft correction" = "1", "Intermediate correction" = "2", "Strong correction" = "3", "High correction" = "4"),
                         selected = "0")
                  ),
              # Local Correction
                  conditionalPanel(condition="input.bctype==2",
                     numericInput("wsfac", "Level of Correction:", 4, min = 1, max = 7, step=1)
                  ),
              # airPLS
                  conditionalPanel(condition="input.bctype==4",
                     numericInput("lambda", "Level of Correction:", 2, min = 1, max = 6, step=1),
                     numericInput("porder", "Order:", 1, min = 1, max = 3, step=1)
                  )
              ),
              column(1, tags$br() )
         ),
         # alignment of some PPM ranges
         conditionalPanel(condition="input.tpreproc=='ppmalign'",
              column(1, tags$img(src="images/img_00.gif", height = 400, width = 1)),
              column(3,
                     selectInput("alignmeth", "Align. Method", c("Least Square" = "1", "Time Warping" = "2", "CluPA" = "3"), selected = "1"),
                     conditionalPanel(condition="input.alignmeth==1",
                         numericInput("aligndecal", "Relative max. shift:", 0.05, min = 0.01, max = 1, step=0.05),
                         bsTooltip("aligndecal", "Relative maximum shift allowed between the reference spectrum and another spectrum in order to find the best alignment", "bottom", options = list(container = "body")),
                         checkboxInput("fapodize", "Marks a break at both ends ", FALSE),
                         bsTooltip("fapodize", "Marks a break at both ends to avoid having a constant non-zero value at the shift points of the spectra", "bottom", options = list(container = "body"))
                     ),
                     conditionalPanel(condition="input.alignmeth==2",
                         selectInput("warpcrit", "Optim. Criterium", c("WCC" = "WCC", "RMS" = "RMS"), selected = "WCC")
                     ),
                     conditionalPanel(condition="input.alignmeth==3",
                         numericInput("resclupa", "Resolution (ppm)", 0.03, min = 0.02, max = 1, step=0.01),
                         numericInput("snrpiclev", "SNR threshold:", 5, min = 0, max = 10, step=1),
                         tags$strong('noisy PPM range:', class="textlabs"),tags$br(), inputTextarea("ppmnoiserange3", supclass="single noise", nrows=1, ncols=18, value="10.5 10.2"),
                         bsTooltip("ppmnoiserange3", "select a PPM range for estimate the standard deviation of the noise", "bottom", options = list(container = "body"))
                     )
              ),
              column(1, tags$img(src="images/img_00.gif", height = 400, width = 1)),
              column(3, 
                     selectInput("RefSpecSelect", "Reference Spectrum", c() ),
                     tags$strong('PPM Ranges to align:', class="textlabs"),
                     tags$br(),
                     inputTextarea("ppmrefrange3", supclass="single", nrows=1, ncols=18, value=""),
                     tags$br()
              )
         ),
         # zeroing of some PPM ranges
         conditionalPanel(condition="input.tpreproc=='ppmzero'",
              column(1, tags$img(src="images/img_00.gif", height = 400, width = 1)),
              column(3, tags$strong('PPM Ranges to clean:', class="textlabs"),tags$br(), inputTextarea("ppmrange2", supclass="capture treset", nrows=10, ncols=22, value=""))
         ),
         # PPM shift
         conditionalPanel(condition="input.tpreproc=='ppmshift'",
              column(1, tags$img(src="images/img_00.gif", height = 400, width = 1)),
              column(3,
                    numericInput("ppmdecal", "PPM shift value:", 0, min = -0.1, max = 0.1, step=0.01),
                    tags$br(),
                    tags$strong('PPM Ranges to shift:', class="textlabs"),
                    tags$br(),
                    inputTextarea("ppmrefrange2", supclass="single", nrows=1, ncols=18, value=""),
                    tags$br()
              )
         ),
         # Denoising
         conditionalPanel(condition="input.tpreproc=='denoising'",
              column(4,
                  tags$br(),
                  numericInput("filtord", "Filter order:", 3, min = 3, max = 20, step=1),
                  tags$br(),
                  numericInput("filtlen", "Filter length:", 5, min = 5, max = 25, step=2)
              ),
              column(4,
                 checkboxInput("fppmfilt", "Restricted PPM range:", FALSE),
                 conditionalPanel(condition="input.fppmfilt==1", inputTextarea("ppmfilt", supclass="single", nrows=1, ncols=18, value="") ),
                 bsTooltip("ppmfilt", "Restricted PPM range for applying the denoising", "bottom", options = list(container = "body"))
              )
         ),
         column(1, tags$img(src="images/img_00.gif", height = 320, width = 1))
     )
)


##---------------
## Bucketing
##---------------
ui_proc_bucket <- column(12,
     tags$br(),
     column(3,
         # Bucketing Method selection
         radioButtons("bucmeth", "Bucketing Method:",
                     c("Uniforme" = "uniforme", "Intelligent Bucketing" = "AIBIN", "Variable Size Buckets" = "VSB", "ERVA" = "ERVA", 
                       "Import Buckets"="bucimport", "Merging / Resetting"="bucreset" ), selected = "AIBIN")
     ),
     column(4,
         conditionalPanel(condition="input.bucmeth=='uniforme'", numericInput("unif_size", "PPM Resolution:", 0.04, min = 0.01, max = 0.1, step=0.01)),
         conditionalPanel(condition="input.bucmeth=='AIBIN'", numericInput("aibin_r", "Resolution Factor:", 0.5, min = 0.1, max = 0.6, step=0.1)),
         conditionalPanel(condition="input.bucmeth=='ERVA'", numericInput("erva_r", "Resolution Factor:", 0.0005, min = 0.0001, max = 0.01, step=0.0005)),
         conditionalPanel(condition="input.bucmeth=='uniforme' || input.bucmeth=='ERVA' || input.bucmeth=='AIBIN'", 
             numericInput("snrbuclev", "SNR threshold:", 3, min = 0, max = 10, step=1),
             tags$strong('noisy PPM range:', class="textlabs"),tags$br(), inputTextarea("ppmnoiserange2", supclass="single noise", nrows=1, ncols=18, value="10.5 10.2"),
             bsTooltip("ppmnoiserange2", "select a PPM range for estimate the standard deviation of the noise", "bottom", options = list(container = "body"))
         ),
         conditionalPanel(condition="input.bucmeth!='bucreset'",
             tags$br(),
             checkboxInput("fbucappend", "Append the new buckets:", TRUE),
             bsTooltip("fbucappend", "Append the new buckets to existing ones", "bottom", options = list(container = "body"))
         ),
         conditionalPanel(condition="input.bucmeth=='bucreset'", 
             tags$strong('PPM Ranges to merge:', class="textlabs"),tags$br(),  inputTextarea("ppmrange5", supclass="capture treset", nrows=10, ncols=25, value="")
         ),
         conditionalPanel(condition="input.bucmeth=='bucimport'",
             selectInput("importFormat", "Import Format:",
                    c("Comma Separator Value (CSV)" = "csv", "Semicolon Separator Value (CSV)" = "ssv", "Tabular Separator Value (TXT)" = "tsv" ),
                    selected = "csv"),
             fileInput( 'importBucfile', 'Buckets file', accept = c( 'text/plain' ) )
         )
     ),
     column(4,
         conditionalPanel(condition="input.bucmeth!='VSB' && input.bucmeth!='bucreset' && input.bucmeth!='bucimport'", 
             tags$strong('PPM Ranges:', class="textlabs"),tags$br(),  inputTextarea("ppmrange4", supclass="capture treset", nrows=10, ncols=25, value="")
         ),
         conditionalPanel(condition="input.bucmeth=='bucreset'", 
             tags$strong('PPM Ranges to reset:', class="textlabs"),tags$br(),  inputTextarea("ppmrange3", supclass="capture treset", nrows=10, ncols=25, value="")
         ),
         conditionalPanel(condition="input.bucmeth=='VSB'", 
             tags$strong('VSB Ranges:', class="textlabs"),tags$br(), inputTextarea("vsb_range", supclass="capture treset", nrows=10, ncols=25, value="")
         ),
         conditionalPanel(condition="input.bucmeth=='bucimport'",
             tags$br(),
             checkboxInput("fbucheader", "Header", TRUE),
             selectInput("minBucPPM", "PPM min", c("Column 1" = "1", "Column 2" = "2", "Column 3" = "3", "Column 4" = "4" ), selected = "3"),
             selectInput("maxBucPPM", "PPM max", c("Column 1" = "1", "Column 2" = "2", "Column 3" = "3", "Column 4" = "4" ), selected = "4")
         )
     ),
     column(1, tags$img(src="images/img_00.gif", height = 320, width = 1))
)


##---------------
## Data Export
##---------------
ui_proc_export <- column(12,
     tags$br(),
     column(3,
      # Export type selection
       radioButtons("eptype", "Data Type to Export:",
          c("Data matrix" = "epdata", "Buckets table" = "epbuckets", "SNR matrix"="epsnr", "XLSX Workbook"="epxlsx", "Spectral data"="epspec", "Macro-Commands"="epcmd"), selected = "epdata")
     ),
     conditionalPanel(condition="output.BucSelect==1 || input.eptype == 'epspec' || input.eptype=='epcmd'",
     column(4,
         conditionalPanel(condition="output.ProcSelect==1 && input.eptype=='epcmd'",
             tags$br(),
                tags$div(id="nogalaxy",
                  downloadButton("exportCMD2", "Export Macro commands" ),
                  bsTooltip("exportCMD2", "Download the Macro commands file", "bottom", options = list(container = "body"))
                ),
               ## Extension for Galaxy Interactive Environment
                tags$div(id="egalaxy",
                  bsButton("exportCMD3", icon=icon("spinner"), label = "Upload Macro commands to Galaxy", style="primary" ),
                  bsTooltip("exportCMD3", "Upload the Macro commands file to Galaxy history", "bottom", options = list(container = "body")),
                  tags$br(), tags$div( id="uploadmsg", style="display: none;"),
                  bsAlert("AlertUpLoad"),
                  tags$br(),
                  bsButton("exportlog", icon=icon("file-text-o"), label = "Log", style="info" ),
                  bsModal("modalExportlog", "Log Watcher","exportlog", size="large", htmlOutput("watcher2c"))
                )
         ),
     # Export format selection
         conditionalPanel(condition="input.eptype=='epdata' || input.eptype=='epbuckets' || input.eptype=='epsnr' || input.eptype == 'epspec'",
            selectInput("exportFormat", "Export Format:",
                 c("Tabular Separator Value (TXT)" = "tsv", "Comma Separator Value (CSV)" = "csv", "Semicolon Separator Value (CSV)" = "ssv"),
                 selected = "tsv")
         ),
         conditionalPanel(condition="input.eptype=='epxlsx'",
            selectInput("exportTempl", "Template Format:",
                 c("Simple template" = "simple", "qHNMR template" = "qhnmr" ),
                 selected = "simple")
         ),
         conditionalPanel(condition="input.eptype=='epdata' || input.eptype=='epxlsx'",
            numericInput("snrlevel", "SNR threshold:", 3, min = 0, max = 10, step=1)
         ),
         conditionalPanel(condition="input.eptype=='epdata' || input.eptype=='epsnr' || input.eptype=='epxlsx'",
            tags$strong('noisy PPM range:', class="textlabs"),tags$br(), inputTextarea("ppmsnrnoise", supclass="single noise", nrows=1, ncols=18, value="10.5 10.2"),
            bsTooltip("ppmsnrnoise", "select a PPM range for estimate the standard deviation of the noise", "bottom", options = list(container = "body"))
         )
     ),
     conditionalPanel(condition="input.eptype=='epdata' || input.eptype=='epxlsx'",
         column(4,
            # Normalization Method selection
            selectInput("normmeth", "Normalization Method:",
                         c("None"= "NONE", "Constant Sum Normalization" = "CSN", "Probabilistic Quotient Normalization" = "PQN"), selected = "CSN"),
            tags$strong('PPM range of the Reference:', class="textlabs"),tags$br(), inputTextarea("ppmnrefint", supclass="single", nrows=1, ncols=18, value=""),
            bsTooltip("ppmnrefint", "select the PPM range of the reference signal to normalize the integration (if relevant)", "bottom", options = list(container = "body")),
            tags$br(), tags$br(),
            conditionalPanel(condition="input.eptype=='epdata'",
                   downloadButton("exportBS", "Export Data Matrix" ),
                   bsTooltip("exportBS", "Download the data matrix along with samplecodes and factors", "bottom", options = list(container = "body"))
            ),
            conditionalPanel(condition="input.eptype=='epxlsx'",
                   downloadButton("exportXLSX", "Export Workbook" ),
                   bsTooltip("exportXLSX", "Download the workbook file with the data matrix, the bucket table and the SNR matrix as separate sheets", "bottom", options = list(container = "body"))
            )
         )
     ),
     conditionalPanel(condition="input.eptype=='epbuckets'",
         column(4,
            tags$br(),
            downloadButton("exportBUC", "Export Bucket Table" ),
            bsTooltip("exportBUC", "Download the bucket table file", "bottom", options = list(container = "body"))
         )
     ),
     conditionalPanel(condition="input.eptype=='epspec'",
         column(4,
            tags$br(),
            downloadButton("exportSpec", "Export Spectra Data" ),
            bsTooltip("exportSpec", "Download the spectra data file", "bottom", options = list(container = "body"))
         )
     ),
     conditionalPanel(condition="input.eptype=='epsnr'",
         column(4,
            tags$br(),
            downloadButton("exportSNR", "Export SNR Matrix" ),
            bsTooltip("exportSNR", "Download the SNR matrix file", "bottom", options = list(container = "body"))
         )
     )
     ),
     conditionalPanel(condition="output.ProcSelect==0 && input.eptype=='epcmd'",
         column(8,
           tags$p(tags$b("Warning: You need to launch at least one processing command before exporting the macro-command file"))
         )
     ),
     conditionalPanel(condition="output.BucSelect==0 && input.eptype != 'epspec' && input.eptype != 'epcmd'",
         column(8,
           tags$p(tags$b("Warning: You need to generate a bucket list before exporting this data type."))
         )
     ),
     column(1, tags$img(src="images/img_00.gif", height = 320, width = 1))
)


##---------------
## TabSetPanel
##---------------
ui_proctabPanel <- tabsetPanel(
     tabPanel("Samples", ui_proc_samples ),
     tabPanel("Processing", ui_proc_process ),
     tabPanel("Bucketing", ui_proc_bucket),
     tabPanel("Data Export", ui_proc_export),
     id = "condProcPanels",
     selected="Processing"
)
