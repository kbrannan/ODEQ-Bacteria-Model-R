chr.wildlife.elk.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-elk"

chr.script.file.name <- "Wildlife_Elk_Sub_Model_calculation_check-subwtsd-update.R"
chr.script <- scan(file = paste0(chr.wildlife.elk.dir, "/",
                                 chr.script.file.name),
                   what = "character", sep = "\n")

chr.files <- list.files(path = chr.wildlife.elk.dir, pattern = "^wildlifeElk([0-9]){2}\\.txt$",
                        full.names = TRUE)



ii <- 1

for(ii in 1:length(chr.files)) {
  
  tmp.sub <- gsub("[^0-9]","",substr(chr.files[ii], nchar(chr.files[ii])-5, nchar(chr.files[ii])))
  chr.input <- paste0("wildlifeElk", tmp.sub ,".txt")
  
  chr.input.file <- paste0(chr.wildlife.elk.dir, "/", chr.input)
  
  df.input <- read.delim(chr.input.file, sep=":", 
                         comment.char="*", stringsAsFactors=FALSE, 
                         header=FALSE)
  names(df.input) <- c("parameter","value(s)")
  
  tmp.script <- chr.script
  
  tmp.row.wtsd <- grep("XX", tmp.script)
  
  for(rr.w in 1:length(tmp.row.wtsd)) tmp.script[tmp.row.wtsd[rr.w]] <- 
    gsub("XX", tmp.sub,chr.script[tmp.row.wtsd[rr.w]])
  
  tmp.script[grep("chk.land.pasture.season.1( ){1,}<-", chr.script)] <- gsub("-999", df.input$`value(s)`[df.input$parameter == "Season 1 Pasture Area in Watershed (ac)"],
                                                              chr.script[grep("chk.land.pasture.season.1( ){1,}<-", chr.script)])
  tmp.script[grep("chk.land.forest.season.1( ){1,}<-", chr.script)] <- gsub("-999", df.input$`value(s)`[df.input$parameter == "Season 1 Forest Area in Watershed (ac)"],
                                                             chr.script[grep("chk.land.forest.season.1( ){1,}<-", chr.script)])
  tmp.script[grep("chk.land.pasture.season.2( ){1,}<-", chr.script)] <- gsub("-999", df.input$`value(s)`[df.input$parameter == "Season 2 Pasture Area in Watershed (ac)"],
                                                                       chr.script[grep("chk.land.pasture.season.2( ){1,}<-", chr.script)])
  tmp.script[grep("chk.land.forest.season.2( ){1,}<-", chr.script)] <- gsub("-999", df.input$`value(s)`[df.input$parameter == "Season 2 Forest Area in Watershed (ac)"],
                                                                      chr.script[grep("chk.land.forest.season.2( ){1,}<-", chr.script)])
  
  cat(tmp.script, file = paste0(chr.wildlife.elk.dir, "/", gsub("update", tmp.sub, chr.script.file.name)),
      sep = "\n")
  
  rm(list=ls(pattern="tmp\\."))

}