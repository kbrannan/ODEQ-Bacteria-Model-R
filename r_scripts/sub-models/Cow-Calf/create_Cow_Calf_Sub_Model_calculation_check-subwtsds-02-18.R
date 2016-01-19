## get new input file names
chr.new.f <- list.files(pattern = "*.new\\.txt")

## get calc check script for subwtsd 01
chr.chk.01 <- scan(file = "Cow_Calf_Sub_Model_calculation_check-subwtsd-01.R",
                   what = "character", sep = "\n")


  for(ii in 1:length(chr.new.f)) {
  ## change info from subwatershed 01 to current subwatershed
  tmp.chk <- chr.chk.01
  
  ## change subwatershed file name
  tmp.chk <- gsub("cowcalf01", 
                  paste0("cowcalf", 
                         gsub("[^0-9]","",chr.new.f[ii])),
                  tmp.chk)
  
  ## get rows on land use info in the calc check sheet
  lng.rp <- grep("^chk\\.lu", tmp.chk)
  
  ## get input file for current subwatershed
  tmp.in <- scan(file = chr.new.f[ii],
                 what = "character", sep = "\n")
  
  ## change acerage for pasture
  tmp.chk[lng.rp[1]] <- 
    gsub("\\-.*\\#",
         paste0("- ", 
                as.numeric(gsub("Pasture Area in Watershed \\(ac\\)\\:", "",
                                grep("Pasture Area in Watershed \\(ac\\)\\:",
                                     tmp.in, value = TRUE))), " #"),
         tmp.chk[lng.rp[1]])
  
  ## change acerage for forest
  tmp.chk[lng.rp[2]] <- 
    gsub("\\-.*\\#",
         paste0("- ", 
                as.numeric(gsub("Forest Area in Watershed \\(ac\\)\\:", "",
                                grep("Forest Area in Watershed \\(ac\\)\\:",
                                     tmp.in, value = TRUE))), " #"),
         tmp.chk[lng.rp[2]])
  
  ## change percent pasture with stream acccess
  tmp.chk[lng.rp[3]] <- 
    gsub("\\-.*\\#",
         paste0("- ",
                as.numeric(gsub("Percent of pasture with stream access\\:", "",
                                grep("Percent of pasture with stream access\\:",
                                     tmp.in, value = TRUE))), " #"),
         tmp.chk[lng.rp[3]])
  
  ## change percent forest with stream acccess
  tmp.chk[lng.rp[4]] <- 
    gsub("\\-.*\\#",
         paste0("- ",
                as.numeric(gsub("Percent of forest with stream access\\:", "",
                                grep("Percent of forest with stream access\\:",
                                     tmp.in, value = TRUE))), " #"),
         tmp.chk[lng.rp[4]])
  
  ## new chk calc filename
  tmp.fn <-  gsub("\\-01\\.R", 
                  paste0("-", 
                         gsub("[^0-9]","",chr.new.f[ii]), ".R"),
                  "Cow_Calf_Sub_Model_calculation_check-subwtsd-01.R")
  
  ## write new calc check file
  cat(tmp.chk, file = tmp.fn, sep = "\n")
  
  ## run the calc check for current subwatershed
  source(file = tmp.fn, local = TRUE)
  
  ## clean up work space
  rm(list=ls(pattern="(^chk\\.)|(^tmp\\.)"))
  rm(df.output)
  
}







