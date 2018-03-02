
# Define Server for NMR processing application
shinyServer(function(input, output, session) {

    #----------------------------------------------------
    # Init
    #----------------------------------------------------

    # --- Internal variables ---
    outDir <- NULL              # directory to output the resulting files
    RawZip <- NULL              # the full path name of the uploaded ZIP file (raw.zip)
    NameZip <- NULL             # the name of the uploaded ZIP file
    SampleFilename <- 'NA'      # the name of the uploaded Sample file
    SampleFile <- NULL          # the full path name of the uploaded Sample file
    PCMDFilename <- NULL        # the Macro-commands file for processing
    procParams <- NULL          # Processing Parameters
    outDataViewer <- NULL       # where data will be stored and used by NMRViewer
    sessionViewer <- NULL       # ID of the NMRViewer session
    procJobName <- NULL         # Name/ID of the current Job => serves to discriminate which reative/render to activate                        
    NBPROC <- 4                 # Total of process types
    CORR <- 1                   # Baseline Correction
    ALIGN <- 2                  # Alignment
    BUCKET <- 3                 # Bucketing
    UNDO <- 4                   # Undo
    ErrMsg <- ''                # Error Message
    MsgStyle <- 'danger'        # Style of Message Box
    ArrayProc <- rep(0, NBPROC) # Init the status of each process type

    ERROR <- reactiveValues(MsgErrLog = '', MsgErrLoad='', MsgErrProc='', MsgUpload='' )
    values <- reactiveValues()
    values$sessinit <- 0
    values$started <- 0
    values$load <- 0
    values$reload <- 0
    values$ziploaded <- 0
    values$proc <- 0
    values$jobrun <- 0
    values$error <- 0
    values$updatevent <- 0
    values$header <- 1
    values$psession <- 0
    values$fgalaxy <- 0
    values$uploadmsg <- 0


    source("R/utils.R", local=TRUE)     # general routines
    source("R/Login.R", local=TRUE)     # Log in module
    source("R/Proc1.R", local=TRUE)     # Upload & Preprocessing
    source("R/Proc2.R", local=TRUE)     # NMRViewer & Capture
    source("R/Proc3.R", local=TRUE)     # Processing

    #----------------------------------------------------
    # Main
    #----------------------------------------------------

    observe({

        # Gets Session Idenfier => SID
        sessid <- ''
        cdata <- session$clientData
        lparams <- unlist(strsplit(gsub("\\?", "", cdata[['url_search']]),  '&'))
        if (length(lparams)>0) {
            sessid <- lparams[1]
        }
        params <- parseQueryString(cdata$url_search)
        if (!is.null(params[['header']])) {
            values$header <- as.numeric(params[['header']])
        }
        if (as.numeric(conf$HEADER)==0) {
            values$header <- 0
        }
        if (as.numeric(conf$GALAXY)==1) {
            values$fgalaxy <- 1
            values$uploadmsg <- 3
        } else {
            values$fgalaxy <- 2
        }


        # Is a new Session ?
        newSession <- nchar(sessid)==0 || ! ( file.exists(file.path(tempdir(),sessid)) || file.exists(file.path(conf$DATASETS,sessid)) )
        # Is User logged ?
        condLogin <- CONNECT$Logged == TRUE

        # If session identifier is not valid or not provided, create a new one
        if ( newSession ) {
           sessid <- paste0('_',paste(sample(c(0:9, letters[1:6]),30, replace=TRUE),collapse=""))
           outData <- file.path(tempdir(),sessid)
           if ( ! file.exists(outData) ) dir.create(outData)
        }
        # With a valid session identifier, needless to login
        if (condLogin || ! newSession) {
           sessionViewer <<- sessid
           values$sessinit <- 1
           if ( newSession ) {
               output$jreload <- renderUI({ tags$script(HTML(paste0("window.history.replaceState('",sessid,"', '",sessid,"', '?", sessid, "');"))) })
           }
           values$psession <- ifelse( file.exists(file.path(conf$DATASETS,sessid,conf$SPEC_PACKED)), 1, 0 )
        }

        output$title <- renderUI({ tags$h4(paste(conf$TITLE,'- ver.',conf$VERSION))})
        values$started <- 1

    })

})

