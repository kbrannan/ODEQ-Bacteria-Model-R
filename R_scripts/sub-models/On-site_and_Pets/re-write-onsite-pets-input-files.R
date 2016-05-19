chr.dir.onsite.pets <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/On-site_and_Pets"

chr.files <- list.files(path = chr.dir.onsite.pets, pattern = "^OnSitePets([0-9]){2}\\.txt$",
                        full.names = TRUE)

grep("01\\.txt$", chr.files, value = TRUE)


df.input <- read.delim(grep("01\\.txt$", chr.files, value = TRUE), sep=":",
                       stringsAsFactors=FALSE, header=FALSE)
names(df.input) <- c("parameter","value(s)")


for(ii in 2:length(chr.files)) {
  tmp.sub <- gsub("[^0-9]","",substr(chr.files[ii], nchar(chr.files[ii])-5, nchar(chr.files[ii])))
  tmp.input <- read.delim(chr.files[ii], sep=":",
                          stringsAsFactors=FALSE, header=FALSE)
  names(tmp.input) <- c("parameter","value(s)")
  
  tmp.output <- df.input
  
  tmp.output$`value(s)`[tmp.output$parameter == "Watershed"] <- paste0("Big Elk Creek Sub-Wtsd ",tmp.sub)
  tmp.output$`value(s)`[tmp.output$parameter == "Number of House-Holds"] <- tmp.input$`value(s)`[tmp.input$parameter == "Number of House-Holds"]
  tmp.output$`value(s)`[tmp.output$parameter == "Residential/Agricultural Operration Area/Commercial/Urban/Transportation (ac)"] <- tmp.input$`value(s)`[tmp.input$parameter == "Residential/Agricultural Operration Area/Commercial/Urban/Transportation (ac)"]
  tmp.output$`value(s)`[tmp.output$parameter == "Number of near-stream structures"] <- tmp.input$`value(s)`[tmp.input$parameter == "Number of near-stream structures"]
  tmp.output$`value(s)`[tmp.output$parameter == "On-site Failure directly to stream (%)"] <- tmp.input$`value(s)`[tmp.input$parameter == "On-site Failure directly to stream (%)"]
  
  write.table(tmp.output, file = gsub("\\.txt","-update.txt", chr.files[ii]),
              quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ":")
  rm(list=ls(pattern="tmp\\."))
}
