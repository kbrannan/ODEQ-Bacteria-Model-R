# Bacteria Source SubModel IO Check Script
# Developed by Tan Zi(Tan.Zi@tetratech.com)
# Last Updated: 2015/11/1
# This script runs original sub-models and modified ones, and then compares the outputs of each sub-model pair, prints out the different key variables.

io_check<- function(ori.model.wrkdir,mod.model.wrkdir,output.compare.wrkdir=ori.model.wrkdir){
# Load Original SubModel
## Get the path
setwd(ori.model.wrkdir)
chr.ori.dir <- gsub("(Model/.*$){1}","Model",getwd())
chr.sub.models.ori.dir <- paste0(chr.ori.dir,"/sub-models")
chr.sub.model.ori.dirs <- list.dirs(path=chr.sub.models.ori.dir )
## exclude chr.sub.models.dir and the "General" folder because that does not have a sub-model, yet
tmp.dirs <- chr.sub.model.ori.dirs[-grep(paste0("(./General)|(",chr.sub.models.ori.dir,"$)"),chr.sub.model.ori.dirs)]
## get sub-model files 
chr.sub.model.ori.files <- paste0(tmp.dirs,"/",list.files(path=tmp.dirs,pattern="\\.R"))
rm(tmp.dirs)

## load sub-model functions
for(ii in 1:length(chr.sub.model.ori.files)) {
  source(chr.sub.model.ori.files[ii])
}
rm(ii)


# Run Each Original SubModel
cow.calf.in <- "cowcalf01.txt"
cow.calf.ori.out<-cow.calf(chr.input=cow.calf.in,chr.wrkdir=grep("[Cc]ow",chr.sub.model.ori.dirs,value=TRUE))
# print the cowcalf output
foname <- paste0(grep("[Cc]ow",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",cow.calf.in)
write.table(cow.calf.ori.out, foname, row.names = FALSE, col.names = T, sep = ",")

onsite.pets.in <- "OnSitePets01.txt"
onsite.pets.ori.out <- onsite_pets(chr.input=onsite.pets.in,chr.wrkdir=grep("[Ss]ite",chr.sub.model.ori.dirs,value=TRUE))# 
#print the onsite pets output 
foname <- paste0(grep("[Ss]ite",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",onsite.pets.in)
write.table(onsite.pets.ori.out, foname, row.names = FALSE, col.names = T, sep = ",")

beaver.in <- "wildlifeBeaver01.txt"
beaver.ori.out <- wildlifeBeaver(chr.input=beaver.in,chr.wrkdir=grep("Wildlife-[Bb]eaver",chr.sub.model.ori.dirs,value=TRUE))
#print the beaver output 
foname <- paste0(grep("Wildlife-[Bb]eaver",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",beaver.in)
write.table(beaver.ori.out, foname, row.names = FALSE, col.names = T, sep = ",")

coyote.in <- "wildlifeCoyote01.txt"
coyote.ori.out <- wildlifeCoyote(chr.input=coyote.in,chr.wrkdir=grep("Wildlife-[Cc]oyote",chr.sub.model.ori.dirs,value=TRUE))
#print the coyote output 
foname <- paste0(grep("Wildlife-[Cc]oyote",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",coyote.in)
write.table(coyote.ori.out, foname, row.names = FALSE, col.names = T, sep = ",")

deer.in <- "wildlifeDeer01.txt"
deer.ori.out <- wildlifeDeer(chr.input=deer.in,chr.wrkdir=grep("Wildlife-[Dd]eer",chr.sub.model.ori.dirs,value=TRUE))
#print the deer output 
foname <- paste0(grep("Wildlife-[Dd]eer",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",deer.in)
write.table(deer.ori.out, foname, row.names = FALSE, col.names = T, sep = ",")

duck.in <- "wildlifeDuck01.txt"
duck.ori.out <- wildlifeDuck(chr.input=duck.in,chr.wrkdir=grep("Wildlife-[Dd]uck",chr.sub.model.ori.dirs,value=TRUE))
#print the duck output 
foname <- paste0(grep("Wildlife-[Dd]uck",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",duck.in)
write.table(duck.ori.out, foname, row.names = FALSE, col.names = T, sep = ",")

elk.in <- "wildlifeElk01.txt"
elk.ori.out <- wildlifeElk(chr.input=elk.in,chr.wrkdir=grep("Wildlife-[Ee]lk",chr.sub.model.ori.dirs,value=TRUE))
#print the elk output 
foname <- paste0(grep("Wildlife-[Ee]lk",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",elk.in)
write.table(elk.ori.out, foname, row.names = FALSE, col.names = T, sep = ",")

geese.in <- "wildlifeGeese01.txt"
geese.ori.out <- wildlifeGeese(chr.input=geese.in,chr.wrkdir=grep("Wildlife-[Gg]eese",chr.sub.model.ori.dirs,value=TRUE))#print the geese output 
foname <- paste0(grep("Wildlife-[Gg]eese",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",geese.in)
write.table(geese.ori.out, foname, row.names = FALSE, col.names = T, sep = ",")

gulls.in <- "wildlifeGulls01.txt"
gulls.ori.out <- wildlifeGulls(chr.input=gulls.in,chr.wrkdir=grep("Wildlife-[Gg]ulls",chr.sub.model.ori.dirs,value=TRUE))#print the gulls output 
foname <- paste0(grep("Wildlife-[Gg]ulls",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",gulls.in)
write.table(gulls.ori.out, foname, row.names = FALSE, col.names = T, sep = ",")

heregr.in <- "wildlifeHerEgr01.txt"
heregr.ori.out <- wildlifeHerEgr(chr.input=heregr.in,chr.wrkdir=grep("Wildlife-[Hh]erons_[Ee]grets",chr.sub.model.ori.dirs,value=TRUE))
#print the Herons_Egrets output 
foname <- paste0(grep("Wildlife-[Hh]erons_[Ee]grets",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",heregr.in)
write.table(heregr.ori.out, foname, row.names = FALSE, col.names = T, sep = ",")

otter.in <- "wildlifeOtter01.txt"
otter.ori.out <- wildlifeOtter(chr.input=otter.in,chr.wrkdir=grep("Wildlife-[Oo]tter",chr.sub.model.ori.dirs,value=TRUE))#print the Otters output 
foname <- paste0(grep("Wildlife-[Oo]tter",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",otter.in)
write.table(otter.ori.out, foname, row.names = FALSE, col.names = T, sep = ",")

racoon.in <- "wildlifeRacoon01.txt"
racoon.ori.out <- wildlifeRacoon(chr.input=racoon.in,chr.wrkdir=grep("Wildlife-[Rr]acoon",chr.sub.model.ori.dirs,value=TRUE))
#print the Racoon output 
foname <- paste0(grep("Wildlife-[Rr]acoon",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",racoon.in)
write.table(racoon.ori.out, foname, row.names = FALSE, col.names = T, sep = ",")


# Load Modified SubModel
## Get the path
setwd(mod.model.wrkdir)
chr.mod.dir <- gsub("(Model/.*$){1}","Model",getwd())
chr.sub.models.mod.dir <- paste0(chr.mod.dir,"/sub-models")
chr.sub.model.mod.dirs <- list.dirs(path=chr.sub.models.mod.dir )
## exclude chr.sub.models.dir and the "General" folder because that does not have a sub-model, yet
tmp.dirs <- chr.sub.model.mod.dirs[-grep(paste0("(./General)|(",chr.sub.models.mod.dir,"$)"),chr.sub.model.mod.dirs)]
## get modified sub-model files 
chr.sub.model.mod.files <- paste0(tmp.dirs,"/",list.files(path=tmp.dirs,pattern="\\.R"))
rm(tmp.dirs)

## load sub-model functions, this loop will replace original sub-models with modified ones
for(ii in 1:length(chr.sub.model.mod.files)) {
  source(chr.sub.model.mod.files[ii])
}
rm(ii)


# Run Each Modified SubModel with the inputs same as Original model runs, save the output text file at the same location as output from original model runs
cow.calf.mod.out<-cow.calf(chr.input=cow.calf.in,chr.wrkdir=grep("[Cc]ow",chr.sub.model.ori.dirs,value=TRUE))
# print the cowcalf output
foname <- paste0(grep("[Cc]ow",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",cow.calf.in)
write.table(cow.calf.mod.out, foname, row.names = FALSE, col.names = T, sep = ",")

onsite.pets.mod.out <- onsite_pets(chr.input=onsite.pets.in,chr.wrkdir=grep("[Ss]ite",chr.sub.model.ori.dirs,value=TRUE))# #print the onsite pets output 
foname <- paste0(grep("[Ss]ite",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",onsite.pets.in)
write.table(onsite.pets.mod.out, foname, row.names = FALSE, col.names = T, sep = ",")

beaver.mod.out <- wildlifeBeaver(chr.input=beaver.in,chr.wrkdir=grep("Wildlife-[Bb]eaver",chr.sub.model.ori.dirs,value=TRUE))
#print the beaver output 
foname <- paste0(grep("Wildlife-[Bb]eaver",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",beaver.in)
write.table(beaver.mod.out, foname, row.names = FALSE, col.names = T, sep = ",")


coyote.mod.out <- wildlifeCoyote(chr.input=coyote.in,chr.wrkdir=grep("Wildlife-[Cc]oyote",chr.sub.model.ori.dirs,value=TRUE))
#print the coyote output 
foname <- paste0(grep("Wildlife-[Cc]oyote",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",coyote.in)
write.table(coyote.mod.out, foname, row.names = FALSE, col.names = T, sep = ",")

deer.mod.out <- wildlifeDeer(chr.input=deer.in,chr.wrkdir=grep("Wildlife-[Dd]eer",chr.sub.model.ori.dirs,value=TRUE))
#print the deer output 
foname <- paste0(grep("Wildlife-[Dd]eer",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",deer.in)
write.table(deer.mod.out, foname, row.names = FALSE, col.names = T, sep = ",")

duck.mod.out <- wildlifeDuck(chr.input=duck.in,chr.wrkdir=grep("Wildlife-[Dd]uck",chr.sub.model.ori.dirs,value=TRUE))
#print the duck output 
foname <- paste0(grep("Wildlife-[Dd]uck",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",duck.in)
write.table(duck.mod.out, foname,row.names = FALSE, col.names = T, sep = ",")

elk.mod.out <- wildlifeElk(chr.input=elk.in,chr.wrkdir=grep("Wildlife-[Ee]lk",chr.sub.model.ori.dirs,value=TRUE))
#print the elk output 
foname <- paste0(grep("Wildlife-[Ee]lk",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",elk.in)
write.table(elk.mod.out, foname, row.names = FALSE, col.names = T,sep = ",")

geese.mod.out <- wildlifeGeese(chr.input=geese.in,chr.wrkdir=grep("Wildlife-[Gg]eese",chr.sub.model.ori.dirs,value=TRUE))#print the geese output 
foname <- paste0(grep("Wildlife-[Gg]eese",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",geese.in)
write.table(geese.mod.out, foname,row.names = FALSE, col.names = T, sep = ",")

gulls.mod.out <- wildlifeGulls(chr.input=gulls.in,chr.wrkdir=grep("Wildlife-[Gg]ulls",chr.sub.model.ori.dirs,value=TRUE))#print the gulls output 
foname <- paste0(grep("Wildlife-[Gg]ulls",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",gulls.in)
write.table(gulls.mod.out, foname,row.names = FALSE, col.names = T, sep = ",")

heregr.mod.out <- wildlifeHerEgr(chr.input=heregr.in,chr.wrkdir=grep("Wildlife-[Hh]erons_[Ee]grets",chr.sub.model.ori.dirs,value=TRUE))
#print the heregr output 
foname <- paste0(grep("Wildlife-[Hh]erons_[Ee]grets",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",heregr.in)
write.table(heregr.mod.out, foname, row.names = FALSE, col.names = T, sep = ",")

otter.mod.out <- wildlifeOtter(chr.input=otter.in,chr.wrkdir=grep("Wildlife-[Oo]tter",chr.sub.model.ori.dirs,value=TRUE))#print the otter output 
foname <- paste0(grep("Wildlife-[Oo]tter",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",otter.in)
write.table(otter.mod.out, foname,row.names = FALSE, col.names = T, sep = ",")

racoon.mod.out <- wildlifeRacoon(chr.input=racoon.in,chr.wrkdir=grep("Wildlife-[Rr]acoon",chr.sub.model.ori.dirs,value=TRUE))
#print the otter output 
foname <- paste0(grep("Wildlife-[Rr]acoon",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",racoon.in)
write.table(racoon.mod.out,foname, row.names = FALSE, col.names = T, sep = ",")


#compare outputs
setwd(output.compare.wrkdir)
fileCom<-file("output_compare.txt",'a')
#Cow_calf
write("Cow_Calf Submodel Key Variables:", fileCom, append=TRUE)
write(colnames( cow.calf.ori.out)[29:31],fileCom,append=TRUE)
a<-data.matrix(cow.calf.ori.out)[,29:31]
b<-data.matrix(cow.calf.mod.out)[,29:31]
test<-which(a != b, arr.ind=TRUE)
if (!is.null(dim(test))){
col<-unique(test[,2])
}else{
col<-test
}
write("Different Key Variables:", fileCom, append=TRUE)
write(colnames(a)[col],fileCom,append=TRUE)

# onsite-pet
write("", fileCom, append=TRUE)
write("Onsite Pets Submodel Key Variables:", fileCom, append=TRUE)
write(colnames(onsite.pets.ori.out)[17:18],fileCom,append=TRUE)
a<-data.matrix(onsite.pets.ori.out)[,17:18]
b<-data.matrix(onsite.pets.mod.out)[,17:18]
test<-which(a != b, arr.ind=TRUE)
if (!is.null(dim(test))){
col<-unique(test[,2])
write("Different Key Variables:", fileCom, append=TRUE)
write(colnames(a)[col],fileCom,append=TRUE)
}else{
col<-test
write("Different Key Variables:", fileCom, append=TRUE)
write(names(a)[col],fileCom,append=TRUE)
}

# Beaver
write("", fileCom, append=TRUE)
write("Wildlife-Beaver Submodel Key Variables:", fileCom, append=TRUE)
write(colnames(beaver.ori.out)[5:6],fileCom,append=TRUE)
a<-data.matrix(beaver.ori.out)[,5:6]
b<-data.matrix(beaver.mod.out)[,5:6]
test<-which(a != b, arr.ind=TRUE)
if (!is.null(dim(test))){
col<-unique(test[,2])
write("Different Key Variables:", fileCom, append=TRUE)
write(colnames(a)[col],fileCom,append=TRUE)
}else{
col<-test
write("Different Key Variables:", fileCom, append=TRUE)
write(names(a)[col],fileCom,append=TRUE)
}


# Coyote
write("", fileCom, append=TRUE)
write("Wildlife-Coyote Submodel Key Variables:", fileCom, append=TRUE)
write(colnames(coyote.ori.out)[cbind(8,12,13,14)],fileCom,append=TRUE)
a<-data.matrix(coyote.ori.out)[,cbind(8,12,13,14)]
b<-data.matrix(coyote.mod.out)[,cbind(8,12,13,14)]
test<-which(a != b, arr.ind=TRUE)
if (!is.null(dim(test))){
col<-unique(test[,2])
write("Different Key Variables:", fileCom, append=TRUE)
write(colnames(a)[col],fileCom,append=TRUE)
}else{
col<-test
write("Different Key Variables:", fileCom, append=TRUE)
write(names(a)[col],fileCom,append=TRUE)
}

# Deer
write("", fileCom, append=TRUE)
write("Wildlife-Deer Submodel Key Variables:", fileCom, append=TRUE)
write(colnames(deer.ori.out)[cbind(7,10,11)],fileCom,append=TRUE)
a<-data.matrix(deer.ori.out)[,cbind(7,10,11)]
b<-data.matrix(deer.mod.out)[,cbind(7,10,11)]
test<-which(a != b, arr.ind=TRUE)
if (!is.null(dim(test))){
col<-unique(test[,2])
write("Different Key Variables:", fileCom, append=TRUE)
write(colnames(a)[col],fileCom,append=TRUE)
}else{
col<-test
write("Different Key Variables:", fileCom, append=TRUE)
write(names(a)[col],fileCom,append=TRUE)
}

# Duck
write("", fileCom, append=TRUE)
write("Wildlife-Duck Submodel Key Variables:", fileCom, append=TRUE)
write(colnames(duck.ori.out)[8:11],fileCom,append=TRUE)
a<-data.matrix(duck.ori.out)[,8:11]
b<-data.matrix(duck.mod.out)[,8:11]
test<-which(a != b, arr.ind=TRUE)
if (!is.null(dim(test))){
col<-unique(test[,2])
write("Different Key Variables:", fileCom, append=TRUE)
write(colnames(a)[col],fileCom,append=TRUE)
}else{
col<-test
write("Different Key Variables:", fileCom, append=TRUE)
write(names(a)[col],fileCom,append=TRUE)
}

# Elk
write("", fileCom, append=TRUE)
write("Wildlife-Elk Submodel Key Variables:", fileCom, append=TRUE)
write(colnames(elk.ori.out)[cbind(19,32,33,34)],fileCom,append=TRUE)
a<-data.matrix(elk.ori.out)[,cbind(19,32,33,34)]
b<-data.matrix(elk.mod.out)[,cbind(19,32,33,34)]
test<-which(a != b, arr.ind=TRUE)
if (!is.null(dim(test))){
col<-unique(test[,2])
write("Different Key Variables:", fileCom, append=TRUE)
write(colnames(a)[col],fileCom,append=TRUE)
}else{
col<-test
write("Different Key Variables:", fileCom, append=TRUE)
write(names(a)[col],fileCom,append=TRUE)
}

# Geese
write("", fileCom, append=TRUE)
write("Wildlife-Geese Submodel Key Variables:", fileCom, append=TRUE)
write(colnames(geese.ori.out)[8:11],fileCom,append=TRUE)
a<-data.matrix(geese.ori.out)[,8:11]
b<-data.matrix(geese.mod.out)[,8:11]
test<-which(a != b, arr.ind=TRUE)
if (!is.null(dim(test))){
col<-unique(test[,2])
write("Different Key Variables:", fileCom, append=TRUE)
write(colnames(a)[col],fileCom,append=TRUE)
}else{
col<-test
write("Different Key Variables:", fileCom, append=TRUE)
write(names(a)[col],fileCom,append=TRUE)
}

# Gulls
write("", fileCom, append=TRUE)
write("Wildlife-Gulls Submodel Key Variables:", fileCom, append=TRUE)
write(colnames(gulls.ori.out)[cbind(8,12,13,14)],fileCom,append=TRUE)
a<-data.matrix(gulls.ori.out)[,cbind(8,12,13,14)]
b<-data.matrix(gulls.mod.out)[,cbind(8,12,13,14)]
test<-which(a != b, arr.ind=TRUE)
if (!is.null(dim(test))){
col<-unique(test[,2])
write("Different Key Variables:", fileCom, append=TRUE)
write(colnames(a)[col],fileCom,append=TRUE)
}else{
col<-test
write("Different Key Variables:", fileCom, append=TRUE)
write(names(a)[col],fileCom,append=TRUE)
}

# Herons_Egrets
write("", fileCom, append=TRUE)
write("Wildlife-Herons_Egrets Submodel Key Variables:", fileCom, append=TRUE)
write(colnames(heregr.ori.out)[cbind(8,12,13,14)],fileCom,append=TRUE)
a<-data.matrix(heregr.ori.out)[,cbind(8,12,13,14)]
b<-data.matrix(heregr.mod.out)[,cbind(8,12,13,14)]
test<-which(a != b, arr.ind=TRUE)
if (!is.null(dim(test))){
col<-unique(test[,2])
write("Different Key Variables:", fileCom, append=TRUE)
write(colnames(a)[col],fileCom,append=TRUE)
}else{
col<-test
write("Different Key Variables:", fileCom, append=TRUE)
write(names(a)[col],fileCom,append=TRUE)
}

# Otter
write("", fileCom, append=TRUE)
write("Wildlife-Otter Submodel Key Variables:", fileCom, append=TRUE)
write(colnames(otter.ori.out)[cbind(11,16,17)],fileCom,append=TRUE)
a<-data.matrix(otter.ori.out)[,cbind(11,16,17)]
b<-data.matrix(otter.mod.out)[,cbind(11,16,17)]
test<-which(a != b, arr.ind=TRUE)
if (!is.null(dim(test))){
col<-unique(test[,2])
write("Different Key Variables:", fileCom, append=TRUE)
write(colnames(a)[col],fileCom,append=TRUE)
}else{
col<-test
write("Different Key Variables:", fileCom, append=TRUE)
write(names(a)[col],fileCom,append=TRUE)
}

# Raccoon
write("", fileCom, append=TRUE)
write("Wildlife-Raccoon Submodel Key Variables:", fileCom, append=TRUE)
write(colnames(racoon.ori.out)[cbind(8,12,13,14)],fileCom,append=TRUE)
a<-data.matrix(racoon.ori.out)[,cbind(8,12,13,14)]
b<-data.matrix(racoon.mod.out)[,cbind(8,12,13,14)]
test<-which(a != b, arr.ind=TRUE)
if (!is.null(dim(test))){
col<-unique(test[,2])
write("Different Key Variables:", fileCom, append=TRUE)
write(colnames(a)[col],fileCom,append=TRUE)
}else{
col<-test
write("Different Key Variables:", fileCom, append=TRUE)
write(names(a)[col],fileCom,append=TRUE)
}

close(fileCom)
return(0)
}