# Header 
ui_header <- conditionalPanel( condition = 'output.panelHeader==1',withTags(
  table(tr(td(style="width: 5px;",""),
           td(style="width: 30px;", img(src="images/nmrpf_logo.png", height = 32, width = 32)),
           td(style="width: 5px;",""), 
           td(htmlOutput("title"))), width="100%", style="background-color: #337ab7; color: #fff;", id="lHeader")
))

ui_padding <- column(12, style="font-size: 5px;", tags$img(src="images/img_00.gif", height = 3, width = 100))

passwdInput <- function(inputId, label) {
    tagList(
      tags$label(label),
      tags$input(id = inputId, type="password", value="", class="form-control")
    )
}

## Login module;
ui_login_gui <- div(class = "login",
#      uiOutput("uiLogin"),
      wellPanel(
        textInput("userName", "Email:"),
        passwdInput("passwd", "Password:"),
        br(),
        actionButton("Login", "Log in"),
        HTML("&nbsp;&nbsp;"),actionLink("reqAccess", "", icon = icon("envelope")),
        tags$i(HTML(paste0('<a href="',conf$URL_ACCOUNT_REQUEST,'" target="_blank">Request an Access</a>')))
      ),
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
    tr(td(colspan=4, align="center", a(img(src="images/INRAE_logo.png", height = 20), href="http://www.inrae.fr/en/", target="_blank") ))
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
  tags$p(class="cprght", "NMRProcFlow (C) INRAE 2016-2025")
)

# Textarea UI widget
inputTextarea <- function(inputId, supclass="", value="", nrows, ncols) {
    myclass <- "form-control inputtextarea"
    if (nchar(supclass)>0) myclass <- paste(myclass,supclass)
    tagList(
        singleton(tags$head(tags$script(src = "js/textarea.js"))),
        tags$textarea(id = inputId,
                    class = myclass,
                    rows = nrows,
                    cols = ncols,
                    as.character(value)), tags$br()
    )
}


logical_js <- function(b) { tolower(isTRUE(b)) }

# Autocomplete text input field
autocomplete_input <- function(
  id, label, options, value = "", width = NULL, placeholder = NULL,
  max_options = 0, hide_values = FALSE, create = FALSE, contains = FALSE
) {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("jsonlite is needed to convert list of options into json!")
      }
      value <- shiny::restoreInput(id = id, default = value)
      js_opts <- jsonlite::toJSON(as.list(options), auto_unbox = TRUE)
      width <- shiny::validateCssUnit(width)
      if (length(value) == 0L) value <- ""
      shiny::div(
         class = "form-group shiny-input-container autocomplete",
         style = if (!is.null(width)) paste0("width: ", width, ";"),
         if (!is.null(label)) shiny::tags$label(label, `for` = id),
         shiny::tags$input(
            id = id, type = "text", class = "form-control", result = value,
            value = value, placeholder = placeholder, "data-options" = js_opts,
            "data-max" = max_options, "data-contains" = logical_js(contains),
            "data-hide" = logical_js(hide_values), "data-create" = logical_js(create),
            autocomplete = "off"
         ),
         htmltools::htmlDependency(
            "autocomplete", "0.0.1", c(href = "dqshinyRes"),
            script = "js/autocomplete-binding.js", stylesheet = "css/autocomplete.css"
         )
      )
}
