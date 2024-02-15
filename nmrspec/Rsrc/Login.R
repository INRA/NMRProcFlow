#### Log in module ###

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
