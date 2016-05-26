chr.wildlife.duck.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-duck"

chr.script.file.name <- "Wildlife_Duck_Sub_Model_calculation_check-subwtsd-update.R"
chr.script <- scan(file = paste0(chr.wildlife.duck.dir, "/",
                                 chr.script.file.name),
                   what = "character", sep = "\n")

chr.files <- list.files(path = chr.wildlife.duck.dir, pattern = "^wildlifeDuck([0-9]){2}\\.txt$",
                        full.names = TRUE)



for(ii in 1:length(chr.files)) {
  
  tmp.sub <- gsub("[^0-9]","",substr(chr.files[ii], nchar(chr.files[ii])-5, nchar(chr.files[ii])))
  chr.input <- paste0("wildlifeDuck", tmp.sub ,".txt")
  
  chr.input.file <- paste0(chr.wildlife.duck.dir, "/", chr.input)
  
  df.input <- read.delim(chr.input.file, sep=":", 
                         comment.char="*", stringsAsFactors=FALSE, 
                         header=FALSE)
  names(df.input) <- c("parameter","value(s)")
  
  tmp.script <- chr.script
  
  tmp.row.wtsd <- grep("XX", tmp.script)
  
  for(rr.w in 1:length(tmp.row.wtsd)) tmp.script[tmp.row.wtsd[rr.w]] <- 
    gsub("XX", tmp.sub,chr.script[tmp.row.wtsd[rr.w]])
  
  tmp.script[grep("chk.land.pasture( ){1,}<-", chr.script)] <- gsub("-999", df.input$`value(s)`[df.input$parameter == "Pasture Area in Watershed (ac)"],
                                                              chr.script[grep("chk.land.pasture( ){1,}<-", chr.script)])
  tmp.script[grep("chk.land.forest( ){1,}<-", chr.script)] <- gsub("-999", df.input$`value(s)`[df.input$parameter == "Forest Area in Watershed (ac)"],
                                                             chr.script[grep("chk.land.forest( ){1,}<-", chr.script)])
  tmp.script[grep("chk.land.RAOCUT( ){1,}<-", chr.script)] <- gsub("-999", df.input$`value(s)`[df.input$parameter == "Residential/Agricultural Operration Area/Commercial/Urban/Transportation (ac)"],
                                                             chr.script[grep("chk.land.RAOCUT( ){1,}<-", chr.script)])
  
  
##  cat(tmp.script, file = paste0(chr.wildlife.duck.dir, "/", gsub("update", tmp.sub, chr.script.file.name)),
##      sep = "\n")

  cat(tmp.script, file = paste0(chr.wildlife.duck.dir, "/", gsub("update", tmp.sub, chr.script.file.name)),
      sep = "\n")
  
    
  rm(list=ls(pattern="tmp\\."))

}