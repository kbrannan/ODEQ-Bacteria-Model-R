# Bacteria Source Model Control File
# Last Updated: 2014/01/21
# This script runs sub-models and then updates SUP file and writes MUTSIN files

## set options
options(stringsAsFactors = FALSE)

# define directories
##chr.PEST.dir <- "C:/Temp/PEST/BigElkPEST/host"
##chr.PEST.dir <- commandArgs(TRUE)
chr.PEST.dir <- gsub("(Model/.*$){1}","Model",getwd())
#chr.sub.models.dir <- "C:/Temp/PEST/BigElkPEST/host/sub-models"
chr.sub.models.dir <- paste0(chr.PEST.dir,"/sub-models")
#chr.source.control.dir <- "C:/Temp/PEST/BigElkPEST/host/SourceControl"
chr.source.control.dir <- paste0(chr.PEST.dir,"/SourceControl")
## get sub-model folders
##chr.sub.model.dirs <- paste0(chr.sub.models.dir,"/",list.dirs(path=chr.sub.models.dir))
chr.sub.model.dirs <- list.dirs(path=chr.sub.models.dir)
## exclude chr.sub.models.dir and the "General" folder because that does not have a sub-model, yet
tmp.dirs <- chr.sub.model.dirs[-grep(paste0("(./General)|(",chr.sub.models.dir,"$)"),chr.sub.model.dirs)]
## get sub-model files 
chr.sub.model.files <- paste0(tmp.dirs,"/",list.files(path=tmp.dirs,pattern="\\.R"))
rm(tmp.dirs)

## load sub-model functions
for(ii in 1:length(chr.sub.model.files)) {
  source(chr.sub.model.files[ii])
}
rm(ii)

# define number of subwatersheds
sub.wtsd.N <- 18

# create input vectors
chr.sub.model.input.files <- unique(gsub(pattern="[0-9]{2}\\.txt","",list.files(path=chr.sub.model.dirs,pattern="\\.txt")))

# update SUP file and write MUTSIN files using "WriteSup" function
source(paste0(chr.source.control.dir,"/","WriteSupMUT.R"))
WriteSup(sub.wtsd.num=sub.wtsd.N,sub.model.dirs=chr.sub.model.dirs,sub.model.input=chr.sub.model.input.files,sup.file="bigelkwq.sup",str.yr=1995,end.yr=2010, PEST.dir=chr.PEST.dir)

# remove objects
rm(list=ls(all=TRUE))