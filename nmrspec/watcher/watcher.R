# ID watcher.R
# Copyright (C) 2015 INRA - MetaboHUB
# Authors: D. Jacob
#
#  This file is part of NMRProcFLow
#
#
# Root of the scritps
workdir <- "/srv/shiny-server/watcher/"
source(paste(workdir, "global.R", sep=''))

# Semaphores
semapIn <- FALSE
semapOut <- FALSE
semapErr <- FALSE

meta_refresh <- ''
html <- ''
progress <- ''
jscript <- ''

get_jscript <- function(jvardocParent, value) {
   return( paste0('<script>parent.document.getElementById("',jvardocParent,'").innerHTML="',value,'"; parent.jobStatusTrigger();</script>') )
}

repeat {
    jscript <- get_jscript(conf$jvardocParent, "Started")

    if (nchar(SERVER$path_info)==0) break

    # Gets Session Idenfier => SID
    opts <- strsplit(SERVER$path_info,'/')[[1]]
    opts <- opts[ opts != "" ]
    if (length(opts)<4) break
    PROG <- opts[1]
    SID  <- opts[2]
    TYPE <- opts[3]
    PROC <- opts[4]

    if (TYPE=='init')   LOGFILE <- conf$LOGFILE0
    if (TYPE=='view')   LOGFILE <- conf$LOGFILE0
    if (TYPE=='proc')   LOGFILE <- conf$LOGFILE
    if (TYPE=='bucket') LOGFILE <- conf$LOGFILE2
    if (TYPE=='export') LOGFILE <- conf$LOGFILE3

    # Gets file list in the session directory
    DataViewer <- file.path(conf$DATASETS,SID)
    lstFiles <- list()
    if (nchar(SID)>0 && ! file.exists(DataViewer)) break
    lstFiles <- list.files(path=DataViewer, full.names=FALSE, recursive = FALSE)
    if (length(lstFiles)==0) break

    # Positions the semaphores
    if (conf$semapFileIn    %in% lstFiles) semapIn    <- TRUE
    if (conf$semapFileOut   %in% lstFiles) semapOut   <- TRUE
    if (conf$semapFileErr   %in% lstFiles) semapErr   <- TRUE

    # Quit if no job has been launched
    if (! semapIn && ! semapOut ) break

    # if a job has been launched, but always running
    if (semapIn && ! semapOut ) {

        elapsedtime <- round(as.numeric(difftime(Sys.time(),file.info(file.path(DataViewer,conf$semapFileIn))$ctime,units="secs")),3)
        html <- paste0("<h3>Job running since ",elapsedtime," secs</h3>")

        ProgressFile <- file.path(DataViewer,conf$ProgressFile)
        if (file.exists(ProgressFile)) {
            P <- get_counter(ProgressFile)
            p1p <- round(100*(P$value/P$size)); p1px <- 4*p1p; p2px <- 400 - p1px;
            progress <- paste0('<table border="0" CELLPADDING="0" CELLSPACING="0"><tr>',
                   '<td style="font-size:8pt;text-decoration:none;font-weight:bold;">current process:&nbsp;</td>',
                   '<td style="background-color: #337AB7; width: ',p1px,'px; height: 10px;"></td>',
                   '<td style="background-color: #DEDEDE; width: ',p2px,'px; height: 10px;"></td>',
                   '<td style="font-size:8pt;text-decoration:none;font-weight:bold;">',p1p,'%</td>',
                   '</tr></table>')
        }

        fileLog <- file.path(DataViewer,LOGFILE)
        if (file.exists(fileLog)) {
           LOG <- readLines(fileLog)
           LOG <- LOG[ grep("Spos=", LOG, invert=TRUE) ]
           LOG <- LOG[ grep("No convergence", LOG, invert=TRUE) ]
           LOG <- LOG[ grep("-- masking", LOG, invert=TRUE) ]
           NL <- length(LOG)
           FL <- max( NL-conf$INFOLINES, 1)
           html <- paste0(html,"<pre>", paste(as.list(LOG[c((FL):(NL))]), collapse="\n"),"</pre>" )
        }

        meta_refresh <- paste0('<meta http-equiv="refresh" content="',conf$REFRESHDELAY,' "/>')
        jscript <- get_jscript(conf$jvardocParent, "Running")

        break
    }

    # if a job has been launched, but finished now
    if (semapOut ) {
        t1 <- file.info(file.path(DataViewer,conf$semapFileIn))$ctime
        t2 <- file.info(file.path(DataViewer,conf$semapFileOut))$ctime
        elapsedtime <- round(as.numeric(difftime(t2,t1),units="secs"),3)
        html <- paste0("<h3>Total Job Duration: ",elapsedtime," secs</h3>" )
        fileLog <- file.path(DataViewer,LOGFILE)
        if (file.exists(fileLog)) {
           html <- paste0(html,"<pre>", paste(as.list(readLines(fileLog)), collapse="\n"),"</pre>" )
        }
        if (semapErr) {
           jscript <- get_jscript(conf$jvardocParent, "Error")
        } else {
           jscript <- get_jscript(conf$jvardocParent, "Ended")
        }
        break
    }

    break
}

if (TYPE=='view') jscript <- ''

#---------------------------------------
# HTML CONTENT
#---------------------------------------
setContentType("text/html")

#---------------------------------------
# HTML FORM
#---------------------------------------

cat('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
  <meta charset="utf-8" />
',meta_refresh,'
  <TITLE>',conf$TITLE,'</TITLE>
</head>
<body>
',html,'
',progress,'
',jscript,'
</body></html>
', sep='')
