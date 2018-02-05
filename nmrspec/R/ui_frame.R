# Header 
ui_header <- conditionalPanel( condition = 'output.panelHeader==1',withTags(
  table(tr(td(style="width: 5px;",""),
           td(style="width: 30px;", img(src="images/nmrpf_logo.png", height = 32, width = 32)),
           td(style="width: 5px;",""), 
           td(htmlOutput("title"))), width="100%", style="background-color: #8cb1db; color: #fff;")
))

ui_padding <- column(12, style="font-size: 5px;", htmlOutput("jreload"), tags$img(src="images/img_00.gif", height = 3, width = 100))

## Login module;
ui_login_gui <- div(class = "login",
      uiOutput("uiLogin"),
      div(class="passerr", textOutput("pass"))
)

ui_blank <- column(12,
  tags$br(),
  tags$p( tags$h2("Loading ...")),
  img(src="images/img_00.gif", height = 800)
)

# Login page as Front page
ui_frontpage <- column(12,
  tags$br(),
  withTags(table(align="center",
    tr(td(colspan=4,br(), br(), hr(), br())),
    tr(td(style="width: 1px;",""),
       td(style="width: 600px;",
           a(img(src="images/nmrpf_logo_full.png"),href=conf$URL_WEBSITE, target="_blank"), br(),
           p( h2("An easy graphical tool dedicated", br(),"to 1D NMR spectra processing", br(),"for metabolomics"))
       ),
       td(style="width: 50px;",""),
       td(valign="top", style="width: 400px;", ui_login_gui )),
    tr(td( colspan=3, br() ), td( bsAlert("ErrorAlert") ) ),
    tr(td( colspan=4, br(), hr() )),
    tr(td(colspan=4, align="center", a(img(src="images/INRA_logo.png", height = 60), href="http://www.inra.fr/en/", target="_blank"),
                     img(src="images/img_00.gif", height = 5, width = 100),
                     a(img(src="images/metaboHUB_logo.png", height = 60), href="http://metabohub.fr/index.php?lang=en", target="_blank") ))
  ))
)

# Main Panel
ui_mainpanel  <- mainPanel(
  width=12,
  tabsetPanel(
      tabPanel("Load") ,
      tabPanel("Processing"),
      #tabPanel("data analysis"),
      #tabPanel("Identification"),
      id = "conditionedPanels", type="pills"
  )
)

# Footer
ui_footer <- column(12, tags$hr(),
  tags$p(class="cprght", "NMRProcFlow -(C) INRA UMR 1332 BFP, Metabolomics Facility - MetaboHUB - 2015-2018")
)

# Textarea UI widget
inputTextarea <- function(inputId, supclass="", value="", nrows, ncols) {
    myclass <- "inputtextarea"
    if (nchar(supclass)>0) myclass <- paste(myclass,supclass)
    tagList(
        singleton(tags$head(tags$script(src = "js/textarea.js"))),
        tags$textarea(id = inputId,
                    class = myclass,
                    rows = nrows,
                    cols = ncols,
                    as.character(value))
    )
}
