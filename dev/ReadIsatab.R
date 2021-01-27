
datadir <- "C:/Workdir/Metabolomic/NMRProcFlow/DATA"

sfile <- list.files(path = datadir, pattern = "s_", all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE)
stable <- read.table(sfile, sep="\t", header=TRUE, stringsAsFactors=FALSE)
stable.cols <- colnames(stable)
Factor.cols <- grep( 'Factor.Value.', stable.cols)
Factor.names <- gsub('\\.','_', gsub('\\.$', '', gsub ('Factor.Value.', '', stable.cols[Factor.cols])))


Samples <- cbind( stable$Source.Name, stable$Sample.Name,  rep(1,length(stable$Source.Name)),  rep(13,length(stable$Source.Name)), stable[, Factor.cols] )
colnames(Samples) <- c('Rawdata', 'Samplecode', 'expno', 'procno', Factor.names)

DATAID <- 'HUGO07'
txtfile <- file.path(datadir,paste0(DATAID,'.txt'))
write.table(Samples, txtfile, sep="\t", row.names=F, col.names=T, quote=F)
