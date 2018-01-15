library(Rnmr1D)
library(shiny)
library(shinyBS)
library(digest)
library(rjson)
library(parallel)
library(openxlsx)

# R options: See https://rdrr.io/r/base/options.html

source("exec/RnmrTools.R")

conffile <- "conf/global.ini"
conf <- Parse.INI(conffile, section="GLOBAL")
conf <- Parse.INI(conffile, INI.list=conf, section="NMRSPEC")

zipext <- c('zip', '7z')

if (conf$CORES==0) {
   conf$CORES <- detectCores()
}

MAXZIPSIZE <- 200
if (is.numeric(conf$MAXZIPSIZE) && as.numeric(conf$MAXZIPSIZE)>0) {
    MAXZIPSIZE <- as.numeric(conf$MAXZIPSIZE)
}
options(shiny.maxRequestSize=MAXZIPSIZE*1024^2)
options(shiny.sanitize.errors = FALSE)

Logged = ifelse(conf$USRCONMGR==1, FALSE, TRUE)
USER <- list( email='anonymous', lastname='', firstname='', password='' )
USERS <- NULL
