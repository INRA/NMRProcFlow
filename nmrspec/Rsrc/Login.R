#### Log in module ###
CONNECT  <- reactiveValues(Logged = Logged)

#passwdInput <- function(inputId, label) {
#    tagList(
#      tags$label(label),
#      tags$input(id = inputId, type="password", value="", class="form-control")
#    )
#}
#
#output$uiLogin <- renderUI({
#    if (CONNECT$Logged == FALSE) {
#      wellPanel(
#        textInput("userName", "Email:"),
#        passwdInput("passwd", "Password:"),
#        br(),
#        actionButton("Login", "Log in"),
#        HTML("&nbsp;&nbsp;"),actionLink("reqAccess", "", icon = icon("envelope")),
#        tags$i(HTML(paste0('<a href="',conf$URL_ACCOUNT_REQUEST,'" target="_blank">Request an Access</a>')))
#      )
#    }
#})

observe ({
    input$login
    ERROR$MsgErrLog <- ''
    closeAlert(session, "ErrorAlertId")
    tryCatch({ repeat {
      if (CONNECT$Logged) break;
      if (is.null(input$Login)) break;
      if (input$Login == 0) break;
      Username <- isolate(input$userName)
      Password <- isolate(input$passwd)
      if (nchar(Username) == 0 || nchar(Password) == 0) {
            ERROR$MsgErrLog <- "Error: User name or password are empty!\n"
            break;
      }
      USERS <- read.table(conf$USER_ACCOUNT_FILE, sep=";", header=F, stringsAsFactors=FALSE)
      names(USERS) <- c('userid','firstname','lastname','country','institute','email', 'password')
      UserID<-1; while( USERS$userid[UserID] != Username ) { UserID<-UserID+1; if ( UserID>length(USERS$userid)) break; }
      if ( UserID>length(USERS$userid) ) {
            ERROR$MsgErrLog <- "Error: User name is unknown\n"
            break;
      }
      Password_Saved <- digest(USERS[UserID, ]$password, algo="md5", ascii=TRUE, serialize=FALSE)
      if (USERS$userid[UserID] != Username || Password_Saved != Password) {
          ERROR$MsgErrLog <- "Error: User name and/or Password wrong\n"
          break;
      }
      USER <<- USERS[UserID,]
      CONNECT$Logged <- TRUE
      break;
    }}, error=function(e) {})

})

observe ({
    ERROR$MsgErrLog
    if (nchar(ERROR$MsgErrLog)>0) {
       createAlert(session, "ErrorAlert", "ErrorAlertId", title = "", content = ERROR$MsgErrLog, append = FALSE, style='danger')
    }
})
