chr.calc.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/Wildlife-Beaver"

chr.master.file.name <- "Wildlife_Beaver_Sub_Model_calculation_check-subwtsd-"
chr.input.file.name <- "wildlifeBeaver"

chr.master.file <- paste0(chr.master.file.name,"01.R")

chr.master <- scan(file = paste0(chr.calc.dir, "/", chr.master.file),
                   what = "character", sep = "\n")
num.habitat.row <- grep("chk\\.habitat <- [0-9.]{1,}", chr.master)

ii <- 2

for(ii in c(2,4,5,6,7,8,9,10,11,12,13,14,16,17,18)) {
  tmp.script <- chr.master
  tmp.script <- gsub(paste0(chr.input.file.name,"01.txt"),
                     paste0(chr.input.file.name,sprintf(fmt = "%02i", ii),".txt"),
                     tmp.script)
  tmp.file <- scan(
    file = paste0(chr.calc.dir,"/","wildlifeBeaver", 
                  sprintf(fmt = "%02i", ii), ".txt"),
    what = "character", sep = "\n")
  tmp.habitat <- as.numeric(
    strsplit(grep("Forest Area in Watershed \\(ac\\)\\:", 
                  tmp.file, value = TRUE),
             split = ":")[[1]][2])
  
  tmp.script[num.habitat.row] <- gsub("\\- [0-9.]{1,}",paste0("- ",as.character(tmp.habitat)), tmp.script[num.habitat.row])
  cat(tmp.script, file = paste0(chr.calc.dir,"/",chr.master.file.name, 
                                sprintf(fmt = "%02i", ii), ".R"), sep = "\n")
}

