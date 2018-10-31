library(shiny)
library(shinyBS)
library(shinyjs)

source("R/ui_frame.R", local=TRUE)
source("R/ui_upload.R", local=TRUE)
source("R/ui_procparams.R", local=TRUE)
source("R/ui_procmain.R", local=TRUE)

# Define UI for NMR processing application
shinyUI(fluidPage(

  # Header
  tags$head(
      tags$link(rel="icon", href="images/favicon.ico"),
      tags$link(rel="stylesheet", type="text/css",href="style.css"),
      tags$script(type="text/javascript", src = "js/md5.js"),                # MD5 encoding 
      tags$script(type="text/javascript", src = "js/passwdInputBinding.js"), # password Input widget
      tags$script(type="text/javascript", src = "js/spec_capture.js"),       # Capture Management of Spectral areas
      tags$script(type="text/javascript", src = "js/jobstatus.js"),          # Job Status - Event trigger
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

   # Upload Files
     ui_upload,

   # 1R Spectrum - Processing parameters 
     ui_proc,

   # Footer
     ui_footer

  ))

))
