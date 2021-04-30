source("Rsrc/ui_frame.R")
source("Rsrc/ui_upload.R")
source("Rsrc/ui_procparams.R")
source("Rsrc/ui_procmain.R")

# Define UI for NMR processing application
fluidPage(

  # Header
  tags$head(
      tags$link(rel="icon", href="images/favicon.ico"),
      tags$link(rel="stylesheet", type="text/css",href="style.css"),
      tags$link(rel="stylesheet", type="text/css",href="autocomplete.css"),
      tags$script(type="text/javascript", src = "js/md5.js"),                 # MD5 encoding
      tags$script(type="text/javascript", src = "js/passwdInputBinding.js"),  # password Input widget
      tags$script(type="text/javascript", src = "js/spec_capture.js"),        # Capture Management of Spectral areas
      tags$script(type="text/javascript", src = "js/jobstatus.js"),           # Job Status - Event trigger
      tags$script(type="text/javascript", src = "js/autocomplete-binding.js"),# autocompletion
      tags$script(type="text/javascript", src = "js/google-analytics.js")
  ),

  shinyjs::useShinyjs(debug = TRUE, html = FALSE),
  ui_header,
  ui_padding,

  fluidRow (

     conditionalPanel(condition="output.started==0",
        ui_blank
     ),
     conditionalPanel(condition="output.SessInit==0",
      # Front Page
        ui_frontpage
     ),
     conditionalPanel(condition="output.SessInit==1",

      # Main Panel
        ui_mainpanel,

      # Job Status (within a hidden textarea)
        tags$textarea(id="jobstatus", class="jobstatus", "0"),

      # Viewer Status (within a hidden textarea)
        tags$textarea(id="jobname", "0"),

      # Upload Files
        ui_upload,

      # 1R Spectrum - Processing parameters
        ui_proc,

      # Footer
        ui_footer

     )
  )

)
