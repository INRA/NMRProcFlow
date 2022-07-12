##---------------
## Samples
##---------------

    output$samplSelect <- reactive({
         input$condProcPanels
         isolate({repeat {
            if ( input$condProcPanels != "Samples" ) break;
            factors <- read.table( file.path(outDataViewer,'/factors'), header=F, sep=";", stringsAsFactors=FALSE)
            opts <- factors[-1,2]; if (length(opts)==1) opts <- c(opts,' ')
            update_autocomplete_input( session, id = "Facname", options = opts, 
                                       placeholder = "enter a factor name", hide_values = FALSE, create = TRUE )
            break
         }})
    })
    outputOptions(output, 'samplSelect', suspendWhenHidden=FALSE)
    outputOptions(output, 'samplSelect', priority=1)

    ##---------------
    ## Import Samples file
    ##---------------
    ImportSamples <- function(flg=0)
    {
         RET <- 0
         runjs( "document.getElementById('Exportmsg').style.display = 'block';" )
         isolate({ repeat {
            if ( input$bImpSampl==0 ) break;
            sep <- switch(input$importFormat2,  "csv" = ",", "ssv" = ";",  "tsv" = "\t")
            fheader <- input$fsamplheader
            mincol <- as.numeric(input$FacCol)

         # Read the samples List file
            samples <- read.table( file.path(outDataViewer,'/samples.csv'), header=F, sep=";", stringsAsFactors=FALSE)
            factors <- read.table( file.path(outDataViewer,'/factors'), header=F, sep=";", stringsAsFactors=FALSE)
            rawids <- read.table( file.path(outDataViewer,'/rawids.csv'), header=F, sep=";", stringsAsFactors=FALSE)
            importSamples <- read.table(  file.path(outDataViewer,'importSamplfile'),
                                          header=fheader, sep=sep, stringsAsFactors=FALSE)

         # Make some checks
            L1 <- simplify2array(lapply(1:length(importSamples[,1]), function(n) { paste0(importSamples[n,1],'_',importSamples[n,2]) }))
            L2 <- simplify2array(lapply(1:length(samples[,1]), function(n) { paste0(samples[n,1],'_',samples[n,2]) }))
            if ( sum(! L1 %in% L2)>0 ) {
                 ERROR$MsgErrProc <- "Some raw sample names in the import file do not match any raw sample names.\n\n"
                 break
            }
            if ( ! flg && sum(! L2 %in% L1)>0 ) {
                 RET <- 1
                 break
            }
            if (mincol>0) {
                 if ( dim(importSamples)[2]<mincol ) {
                      ERROR$MsgErrProc <- "The number of columns in the import file is less than specified\n\n"
                      break
                 }
                 maxcol <- dim(importSamples)[2]
                 if (input$FacOnlyOne) maxcol <- mincol
                 for (c in mincol:maxcol ) {
                      TestNull <- simplify2array( lapply( importSamples[,c],
                                       function(v) { if (is.null(v) || length(v)==0 || is.na(v)) { TRUE } else { FALSE } } ))
                      if (sum(TestNull)>0) {
                          ERROR$MsgErrProc <- "Some values in the specified column and beyond are null, empty or NA\n\n"
                          break
                      }
                 }
                 if (nchar(ERROR$MsgErrProc)) break
            }
         # Add factors defined from the column 'mincol' up to the end
            Sflg <- NULL
            if (input$fsamplappend) {
               new_samples <- samples
               new_factors <- factors
               lastidfac <- factors[ dim(factors)[1], 1 ]
            } else {
               new_samples <- samples[, 1:2]
               new_factors <- factors[1,]
               lastidfac <- 1
            }
            L1 <- simplify2array(lapply(1:length(importSamples[,1]), function(n) { paste0(importSamples[n,1],'_',importSamples[n,2]) }))
            L2 <- simplify2array(lapply(1:length(new_samples[,1]), function(n) { paste0(new_samples[n,1],'_',new_samples[n,2]) }))
            if ( sum(! L2 %in% L1)>0 ) {
                Sflg <- L2 %in% L1
                new_samples <- new_samples[Sflg, ]
            }
            if (mincol>0) {
                if (input$FacOnlyOne) {
                    facset <- mincol
                } else {
                    facset <- mincol:dim(importSamples)[2]
                }
                for ( c in facset ) {
                    V <- NULL
                    for ( s in L2 ) V <- c(V,importSamples[ which(s==L1), c])
                    if (fheader) {
                       label <- colnames(importSamples)[c]
                    } else {
                       label <- paste0("Factor ",lastidfac)
                    }
                    new_samples <- cbind( new_samples, V)
                    new_factors <- rbind( new_factors, c(lastidfac+1, label) )
                    lastidfac <- lastidfac+1
                }
            }
         # Write the new spectra matrix if needed
            if (! is.null(Sflg)) {
            # Read the specs.pack
                specPackFile <- file.path(outDataViewer,conf$SPEC_PACKED)
                specMat <-  Rnmr1D:::readSpecMatrix(specPackFile)
                specMat$dppm <- (specMat$ppm_max - specMat$ppm_min)/(specMat$size - 1)
                specMat$ppm <- rev(seq(from=specMat$ppm_min, to=specMat$ppm_max, by=specMat$dppm))
                Rnmr1D:::writeSpecMatrix(specMat$int[Sflg, ], specMat$ppm_min, specMat$ppm_max, specPackFile)
                write.table(rawids[Sflg, ], file=file.path(outDataViewer,'rawids.csv'), sep=';', row.names=F, col.names=F, quote=F)
                clean_STACK(outDataViewer, c(conf$SPEC_PACKED, conf$LOGFILE, conf$PPMRANGE, conf$Rnmr1D_PCMD, zonelist1))
            }

         # Write the metadata files : samples + factors
            write.table(new_samples, file=file.path(outDataViewer,'samples.csv'), sep=';', row.names=F, col.names=F, quote=F)
            write.table(new_factors, file=file.path(outDataViewer,'factors'), sep=';', row.names=F, col.names=F, quote=F)

         # Refresh samples + factors dropbox
            runjs( "refresh_select(1);" )
            break
         }})
         runjs( "document.getElementById('Exportmsg').style.display = 'none';" )
         RET
    }

    output$importSAMPL <- reactive({
         input$importSamplfile
         isolate({
            importSamplfile <- input$importSamplfile
            if ( is.null(importSamplfile) ) return (0)
            file.copy( importSamplfile$datapath, file.path(outDataViewer,'importSamplfile'),overwrite = TRUE )
            return(1)
         })
    })
    outputOptions(output, 'importSAMPL', suspendWhenHidden=FALSE)
    outputOptions(output, 'importSAMPL', priority=1)

    observeEvent(input$bImpSampl, {
      tryCatch({
         ret <- ImportSamples(0)
         if (ret) showModal( modalDialog(
            div(tags$b('Warning'), tags$hr(),
               tags$p('There are fewer samples in the imported file than in the ZIP file. In this case, the missing samples will be deleted with their spectrum and you will not be able to go back, including the processing steps.'),tags$br(),tags$p('Do you confirm ?')
            ),
            footer = tagList(
               modalButton("Cancel"),
               actionButton("okSflg", "OK")
            )
         ))
      }, error=function(e) {
         runjs( "document.getElementById('Exportmsg').style.display = 'none';" )
         ERROR$MsgErrProc <- e;
      })
    })

    observeEvent(input$okSflg, {
      tryCatch({
         removeModal()
         if (input$okSflg) ret <- ImportSamples(1)
      }, error=function(e) {
         runjs( "document.getElementById('Exportmsg').style.display = 'none';" )
         ERROR$MsgErrProc <- e;
      })
    })


    ##---------------
    ## Add a Factor
    ##---------------
    observeEvent(input$bAddFactor, {
      tryCatch({
         runjs( "document.getElementById('Exportmsg').style.display = 'block';" )
         isolate({ repeat {
            if ( input$bAddFactor==0 ) break
            nblevel <- round(as.numeric(input$facnblevel))

         # Read the specs.pack
            specPackFile <- file.path(outDataViewer,conf$SPEC_PACKED)
            specMat <-  Rnmr1D:::readSpecMatrix(specPackFile)
            specMat$dppm <- (specMat$ppm_max - specMat$ppm_min)/(specMat$size - 1)
            specMat$ppm <- rev(seq(from=specMat$ppm_min, to=specMat$ppm_max, by=specMat$dppm))

         # Read the samples List file
            samples <- read.table( file.path(outDataViewer,'/samples.csv'), header=F, sep=";", stringsAsFactors=FALSE)
            factors <- read.table( file.path(outDataViewer,'/factors'), header=F, sep=";", stringsAsFactors=FALSE)

            label <- input$Facname
            lastidfac <- factors[ dim(factors)[1], 1 ]
            if (nchar(label)==0) label <- paste0("Kmeans_",lastidfac)

         # Make some checks
            if ( nblevel<2 && length(which(label==factors[,2]))==0 ) {
                 ERROR$MsgErrProc <- "Nb of level must be greater than or equal to 2\n\n"
                 break
            }

         # Case of Deletion
            if (nblevel<2) {
               fid <- which(label==factors[,2])
               new_samples <- samples[ , -c(fid+1) ]
               new_factors <- factors[ -c(fid), ]
               new_factors[,1] <- c(1:dim(new_factors)[1])
               fid <- 1
            } else {
         # Case of addition or modification
             # Integration
                zone <- as.numeric(simplify2array(strsplit(trim(input$ppm_facrange), " ")))
                i1 <- ifelse( max(zone)>=specMat$ppm_max, 1, length(which(specMat$ppm>max(zone))) )
                i2 <- ifelse( min(zone)<=specMat$ppm_min, specMat$size - 1, which(specMat$ppm<=min(zone))[1] )
                Vint <- Rnmr1D:::C_spectra_integrate(specMat$int, i1, i2)

             # Builds the levels based on K-means
                out <- kmeans(Vint,nblevel)
                L <- simplify2array(lapply(out$cluster, function(v) { x<-paste0('0',v); paste0('L',substr(x,nchar(x)-1,nchar(x))) } ))

             # Write the metadata files : samples + factors
                if (input$ffacappend) {
                   if (length(which(label==factors[,2]))>0) {
                      fid <- which(label==factors[,2])
                      new_samples <- samples
                      new_samples[, fid+1] <- L
                      new_factors <- factors
                   } else {
                      new_samples <- cbind( samples, L)
                      new_factors <- rbind( factors, c(lastidfac+1, label) )
                      fid <- lastidfac+1
                   }
                } else {
                   fid <- 2
                   if (nchar(input$Facname)==0) label <- "Kmeans_1"
                   new_samples <- cbind(samples[, 1:2], L)
                   new_factors <- rbind(factors[1,], c(fid, label) )
                }
            }
            write.table(new_samples, file=file.path(outDataViewer,'samples.csv'), sep=';', row.names=F, col.names=F, quote=F)
            write.table(new_factors, file=file.path(outDataViewer,'factors'), sep=';', row.names=F, col.names=F, quote=F)

         # Refresh samples + factors dropbox
            runjs( paste0("refresh_select(",fid,");") )

         # Update  autocompletion list for the factor name
            opts <- new_factors[-1,2]; if (length(opts)==1) opts <- c(opts,' ')
            update_autocomplete_input( session, id = "Facname", options = opts, 
                                       placeholder = "enter a factor name", hide_values = FALSE, create = TRUE  )
            break
         }})
         runjs( "document.getElementById('Exportmsg').style.display = 'none';" )
      }, error=function(e) {
            runjs( "document.getElementById('Exportmsg').style.display = 'none';" )
            ERROR$MsgErrProc <- e;
      })
    }, priority=10 )

    ##---------------
    ## Modify Samplecode
    ##---------------
    observeEvent(input$bModSampl, {
      tryCatch({
         runjs( "document.getElementById('Exportmsg').style.display = 'block';" )
         isolate({ repeat {
            if ( input$bModSampl==0 ) break
            numfield <- round(as.numeric(input$numField))
            sep <- switch(input$sepField,  "1" = "-", "2" = "_",  "3" = "\\.", "4" = " ")

            samples <- read.table( file.path(outDataViewer,'/samples.csv'), header=F, sep=";", stringsAsFactors=FALSE)
            samples[,2] <- simplify2array(strsplit(samples[,1], sep))[numfield, ]
            write.table(samples, file=file.path(outDataViewer,'samples.csv'), sep=';', row.names=F, col.names=F, quote=F)

         # Refresh samples + factors dropbox
            runjs( paste0("refresh_select(1);") )
            break
         }})
         runjs( "document.getElementById('Exportmsg').style.display = 'none';" )
      }, error=function(e) {
         runjs( "document.getElementById('Exportmsg').style.display = 'none';" )
         ERROR$MsgErrProc <- e;
      })
    })

    ##---------------
    ## Export Samples
    ##---------------
    ES_ext <- reactive({
         switch(input$exportFormat2,  "csv" = ".csv", "ssv" = ".csv",  "tsv" = ".txt", "json"=".json")
    })
    ES_sep <- reactive({
         switch(input$exportFormat2,  "csv" = ",", "ssv" = ";",  "tsv" = "\t")
    })

    output$bExportSampl <- downloadHandler(
         filename = function() { paste0('samples_',gsub("\\..*$", "", NameZip), ES_ext() ) },
         content = function(file) { tryCatch({
            runjs( "document.getElementById('Exportmsg').style.display = 'block';" )
         # Read the samples List file
            samples <- read.table( file.path(outDataViewer,'/samples.csv'), header=F, sep=";", stringsAsFactors=FALSE)
            factors <- read.table( file.path(outDataViewer,'/factors'), header=F, sep=";", stringsAsFactors=FALSE)
            rawids <- read.table( file.path(outDataViewer,'/rawids.csv'), header=F, sep=";", stringsAsFactors=FALSE)
            M <- cbind( samples[, c(1:2)], rawids[, c(2:3)] )
            colnames(M) <- c("Spectrum", "Samplecode", "Expno", "Procno")
            DS <- dim(samples)[2]
            if (DS>2) {
               cnames <- colnames(M)
               M <- cbind( M, samples[, c(3:DS)] )
               colnames(M) <- c(cnames, factors[-1,2] )
            }
            if (input$faddphc) {
               acqpars <- read.table( file.path(outDataViewer,'/list_pars.csv'), header=T, sep=";", stringsAsFactors=FALSE)
               cnames <- colnames(M)
               M <- cbind( M, acqpars[, c(9:10)] )
               colnames(M) <- c(cnames, c('phc0','phc1') )
            }
            write.table(M, file, sep=ES_sep(), row.names=FALSE, col.names=TRUE)
            runjs( "document.getElementById('Exportmsg').style.display = 'none';" )
         }, error=function(e) { ERROR$MsgErrProc <- e; }) }
    )
