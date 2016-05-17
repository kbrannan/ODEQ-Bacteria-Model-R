chr.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/Cow-Calf" 
chr.files <- list.files(path = chr.dir, pattern = "*.update\\.R", full.names = TRUE)
chg.title <-function(chr.old, chr.new, chr.file.chg) {
  tmp.file <- chr.file.chg
  tmp.file[grep(chr.old, tmp.file)] <- gsub(chr.old, chr.new, tmp.file[grep(chr.old, tmp.file)])
  return(tmp.file)
}
for(ii in 1:length(chr.files)) {
  tmp.file <- scan(file = chr.files[ii], what = "character", sep = "\n",
                   quiet = TRUE)

  tmp.file <- chg.title(chr.old = "Cow-calf pairs on pasture without stream access", chr.new = "Cow-calf pairs on land for pasture without stream access", chr.file.chg = tmp.file)  
  tmp.file <- chg.title(chr.old = "Cow-calf pairs on pasture with stream access",    chr.new = "Cow-calf pairs on land for pasture with stream access",    chr.file.chg = tmp.file)
  tmp.file <- chg.title(chr.old = "Cow-calf pairs on pasture in stream",             chr.new = "Cow-calf pairs in stream for pasture",                     chr.file.chg = tmp.file)
  tmp.file <- chg.title(chr.old = "Cow-calf pairs in forest without stream access",         chr.new = "Cow-calf pairs on land for forest without stream access", chr.file.chg = tmp.file)    
  tmp.file <- chg.title(chr.old = "Cow-calf pairs in forest on land without stream access", chr.new = "Cow-calf pairs on land for forest with stream access",    chr.file.chg = tmp.file)    
  tmp.file <- chg.title(chr.old = "Cow-calf pairs in forest in stream access",              chr.new = "Cow-calf pairs in stream for forest",                     chr.file.chg = tmp.file)
  tmp.file <- chg.title(chr.old = "Bacteria on pasture without stream access", chr.new = "Bacteria on land for pasture without stream access", chr.file.chg = tmp.file)
  tmp.file <- chg.title(chr.old = "Bacteria on pasture with stream access",    chr.new = "Bacteria on land for pasture with stream access",    chr.file.chg = tmp.file)
  tmp.file <- chg.title(chr.old = "Bacteria on pasture in stream",             chr.new = "Bacteria in stream for pasture",                     chr.file.chg = tmp.file)
  tmp.file <- chg.title(chr.old = "Bacteria in forest on land with and without stream access", chr.new = "Bacteria on land for forest with and without stream access", chr.file.chg = tmp.file)
  tmp.file <- chg.title(chr.old = "Bacteria in forest in stream",                              chr.new = "Bacteria in stream for forest",                              chr.file.chg = tmp.file)
  cat(tmp.file, file = chr.files[ii], sep = "\n")
  rm(tmp.file)
}
