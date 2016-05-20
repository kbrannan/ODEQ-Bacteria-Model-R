chr.dir.wildlife.beaver <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/Wildlife-Beaver"

chr.files <- list.files(path = chr.dir.wildlife.beaver, pattern = "^wildlifeBeaver([0-9]){2}\\.txt$",
                        full.names = TRUE)
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
  tmp.output$`value(s)`[tmp.output$parameter == "Forest Area in Watershed (ac)"] <- tmp.input$`value(s)`[tmp.input$parameter == "Forest Area in Watershed (ac)"]
  tmp.output$`value(s)`[tmp.output$parameter == "Animal Densities in buffer around streams/rivers (animal/ac)"] <- tmp.input$`value(s)`[tmp.input$parameter == "Animal Densities in buffer around streams/rivers (animal/ac)"]
  tmp.output$`value(s)`[tmp.output$parameter == "Percent of time defecating in streams"] <- tmp.input$`value(s)`[tmp.input$parameter == "Percent of time defecating in streams"]
  tmp.output$`value(s)`[tmp.output$parameter == "bacteria Production of adult per day (orgs/day)"] <- tmp.input$`value(s)`[tmp.input$parameter == "bacteria Production of adult per day (orgs/day)"]
  tmp.output$`value(s)`[tmp.output$parameter == "SQOLIM multiplcation factor: 9"] <- tmp.input$`value(s)`[tmp.input$parameter == "SQOLIM multiplcation factor: 9"]
  
  write.table(tmp.output, file = gsub("\\.txt","-update.txt", chr.files[ii]),
              quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ":")
  rm(list=ls(pattern="tmp\\."))
}
