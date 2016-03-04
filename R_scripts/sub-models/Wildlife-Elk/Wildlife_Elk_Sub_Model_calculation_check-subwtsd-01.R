## step bt step calculation check for wildlife_elk_sub_model using input from
## wildlifeelk01.txt file
chr.wildlife.elk.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-elk"
source(paste0(chr.wildlife.elk.dir,"/Wildlife_Elk_Sub_Model.R"))
df.output <- wildlifeElk(chr.wrkdir=chr.wildlife.elk.dir,chr.input="wildlifeelk01.txt")
##
## get input
df.input <- read.table(file = paste0(chr.wildlife.elk.dir,"/wildlifeelk01.txt"), 
                      comment.char = "*", sep = ":")
