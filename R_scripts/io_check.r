# Bacteria Source SubModel IO Check Script
# Developed by Tan Zi(Tan.Zi@tetratech.com)
# Last Updated: 2015/11/1
# This script runs sub-models from and then 

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
write.table(cow.calf.ori.out, foname, sep = ",")

onsite.pets.in <- "OnSitePets01.txt"
onsite.pets.ori.out <- onsite_pets(chr.input=onsite.pets.in,chr.wrkdir=grep("[Ss]ite",chr.sub.model.ori.dirs,value=TRUE))# 
#print the onsite pets output 
foname <- paste0(grep("[Ss]ite",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",onsite.pets.in)
write.table(onsite.pets.ori.out, foname, sep = ",")

beaver.in <- "wildlifeBeaver01.txt"
beaver.ori.out <- wildlifeBeaver(chr.input=beaver.in,chr.wrkdir=grep("Wildlife-[Bb]eaver",chr.sub.model.ori.dirs,value=TRUE))
#print the beaver output 
foname <- paste0(grep("Wildlife-[Bb]eaver",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",beaver.in)
write.table(beaver.ori.out, foname, sep = ",")

coyote.in <- "wildlifeCoyote01.txt"
coyote.ori.out <- wildlifeCoyote(chr.input=coyote.in,chr.wrkdir=grep("Wildlife-[Cc]oyote",chr.sub.model.ori.dirs,value=TRUE))
#print the coyote output 
foname <- paste0(grep("Wildlife-[Cc]oyote",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",coyote.in)
write.table(coyote.ori.out, foname, sep = ",")

deer.in <- "wildlifeDeer01.txt"
deer.ori.out <- wildlifeDeer(chr.input=deer.in,chr.wrkdir=grep("Wildlife-[Dd]eer",chr.sub.model.ori.dirs,value=TRUE))
#print the deer output 
foname <- paste0(grep("Wildlife-[Dd]eer",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",deer.in)
write.table(deer.ori.out, foname, sep = ",")

duck.in <- "wildlifeDuck01.txt"
duck.ori.out <- wildlifeDuck(chr.input=duck.in,chr.wrkdir=grep("Wildlife-[Dd]uck",chr.sub.model.ori.dirs,value=TRUE))
#print the duck output 
foname <- paste0(grep("Wildlife-[Dd]uck",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",duck.in)
write.table(duck.ori.out, foname, sep = ",")

elk.in <- "wildlifeElk01.txt"
elk.ori.out <- wildlifeElk(chr.input=elk.in,chr.wrkdir=grep("Wildlife-[Ee]lk",chr.sub.model.ori.dirs,value=TRUE))
#print the elk output 
foname <- paste0(grep("Wildlife-[Ee]lk",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",elk.in)
write.table(elk.ori.out, foname, sep = ",")

geese.in <- "wildlifeGeese01.txt"
geese.ori.out <- wildlifeGeese(chr.input=geese.in,chr.wrkdir=grep("Wildlife-[Gg]eese",chr.sub.model.ori.dirs,value=TRUE))#print the geese output 
foname <- paste0(grep("Wildlife-[Gg]eese",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",geese.in)
write.table(geese.ori.out, foname, sep = ",")

gulls.in <- "wildlifeGulls01.txt"
gulls.ori.out <- wildlifeGulls(chr.input=gulls.in,chr.wrkdir=grep("Wildlife-[Gg]ulls",chr.sub.model.ori.dirs,value=TRUE))#print the gulls output 
foname <- paste0(grep("Wildlife-[Gg]ulls",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",gulls.in)
write.table(gulls.ori.out, foname, sep = ",")

heregr.in <- "wildlifeHerEgr01.txt"
heregr.ori.out <- wildlifeHerEgr(chr.input=heregr.in,chr.wrkdir=grep("Wildlife-[Hh]erons_[Ee]grets",chr.sub.model.ori.dirs,value=TRUE))
#print the Herons_Egrets output 
foname <- paste0(grep("Wildlife-[Hh]erons_[Ee]grets",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",heregr.in)
write.table(heregr.ori.out, foname, sep = ",")

otter.in <- "wildlifeOtter01.txt"
otter.ori.out <- wildlifeOtter(chr.input=otter.in,chr.wrkdir=grep("Wildlife-[Oo]tter",chr.sub.model.ori.dirs,value=TRUE))#print the Otters output 
foname <- paste0(grep("Wildlife-[Oo]tter",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",otter.in)
write.table(otter.ori.out, foname, sep = ",")

racoon.in <- "wildlifeRacoon01.txt"
racoon.ori.out <- wildlifeRacoon(chr.input=racoon.in,chr.wrkdir=grep("Wildlife-[Rr]acoon",chr.sub.model.ori.dirs,value=TRUE))
#print the Racoon output 
foname <- paste0(grep("Wildlife-[Rr]acoon",chr.sub.model.ori.dirs,value=TRUE),"/ori_output_",racoon.in)
write.table(racoon.ori.out, foname, sep = ",")


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
write.table(cow.calf.mod.out, foname, sep = ",")

onsite.pets.mod.out <- onsite_pets(chr.input=onsite.pets.in,chr.wrkdir=grep("[Ss]ite",chr.sub.model.ori.dirs,value=TRUE))# #print the onsite pets output 
foname <- paste0(grep("[Ss]ite",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",onsite.pets.in)
write.table(onsite.pets.mod.out, foname, sep = ",")

beaver.mod.out <- wildlifeBeaver(chr.input=beaver.in,chr.wrkdir=grep("Wildlife-[Bb]eaver",chr.sub.model.ori.dirs,value=TRUE))
#print the beaver output 
foname <- paste0(grep("Wildlife-[Bb]eaver",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",beaver.in)
write.table(beaver.mod.out, foname, sep = ",")


coyote.mod.out <- wildlifeCoyote(chr.input=coyote.in,chr.wrkdir=grep("Wildlife-[Cc]oyote",chr.sub.model.ori.dirs,value=TRUE))
#print the coyote output 
foname <- paste0(grep("Wildlife-[Cc]oyote",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",coyote.in)
write.table(coyote.mod.out, foname, sep = ",")

deer.mod.out <- wildlifeDeer(chr.input=deer.in,chr.wrkdir=grep("Wildlife-[Dd]eer",chr.sub.model.ori.dirs,value=TRUE))
#print the deer output 
foname <- paste0(grep("Wildlife-[Dd]eer",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",deer.in)
write.table(deer.mod.out, foname, sep = ",")

duck.mod.out <- wildlifeDuck(chr.input=duck.in,chr.wrkdir=grep("Wildlife-[Dd]uck",chr.sub.model.ori.dirs,value=TRUE))
#print the duck output 
foname <- paste0(grep("Wildlife-[Dd]uck",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",duck.in)
write.table(duck.mod.out, foname, sep = ",")

elk.mod.out <- wildlifeElk(chr.input=elk.in,chr.wrkdir=grep("Wildlife-[Ee]lk",chr.sub.model.ori.dirs,value=TRUE))
#print the elk output 
foname <- paste0(grep("Wildlife-[Ee]lk",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",elk.in)
write.table(elk.mod.out, foname, sep = ",")

geese.mod.out <- wildlifeGeese(chr.input=geese.in,chr.wrkdir=grep("Wildlife-[Gg]eese",chr.sub.model.ori.dirs,value=TRUE))#print the geese output 
foname <- paste0(grep("Wildlife-[Gg]eese",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",geese.in)
write.table(geese.mod.out, foname, sep = ",")

gulls.mod.out <- wildlifeGulls(chr.input=gulls.in,chr.wrkdir=grep("Wildlife-[Gg]ulls",chr.sub.model.ori.dirs,value=TRUE))#print the gulls output 
foname <- paste0(grep("Wildlife-[Gg]ulls",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",gulls.in)
write.table(gulls.mod.out, foname, sep = ",")

heregr.mod.out <- wildlifeHerEgr(chr.input=heregr.in,chr.wrkdir=grep("Wildlife-[Hh]erons_[Ee]grets",chr.sub.model.ori.dirs,value=TRUE))
#print the heregr output 
foname <- paste0(grep("Wildlife-[Hh]erons_[Ee]grets",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",heregr.in)
write.table(heregr.mod.out, foname, sep = ",")

otter.mod.out <- wildlifeOtter(chr.input=otter.in,chr.wrkdir=grep("Wildlife-[Oo]tter",chr.sub.model.ori.dirs,value=TRUE))#print the otter output 
foname <- paste0(grep("Wildlife-[Oo]tter",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",otter.in)
write.table(otter.mod.out, foname, sep = ",")

racoon.mod.out <- wildlifeRacoon(chr.input=racoon.in,chr.wrkdir=grep("Wildlife-[Rr]acoon",chr.sub.model.ori.dirs,value=TRUE))
#print the otter output 
foname <- paste0(grep("Wildlife-[Rr]acoon",chr.sub.model.ori.dirs,value=TRUE),"/mod_output_",racoon.in)
write.table(racoon.mod.out, foname, sep = ",")


#compare outputs
setwd(output.compare.wrkdir)
fileCom<-file("output_compare.txt",'a')
#Cow_calf
write("Cow_Calf Submodel:", fileCom, append=TRUE)
a<-data.matrix(cow.calf.ori.out)
b<-data.matrix(cow.calf.mod.out)
test<-which(a != b, arr.ind=TRUE)
col<-unique(test[,2])
write(colnames( cow.calf.ori.out)[col],fileCom,append=TRUE)

# onsite-pet
write("", fileCom, append=TRUE)
write("Onsite Pets Submodel:", fileCom, append=TRUE)
a<-data.matrix(onsite.pets.ori.out)
b<-data.matrix(onsite.pets.mod.out)
test<-which(a != b, arr.ind=TRUE)
col<-unique(test[,2])
write(colnames(onsite.pets.ori.out)[col],fileCom,append=TRUE)

# Beaver
write("", fileCom, append=TRUE)
write("Wildlife-Beaver Submodel:", fileCom, append=TRUE)
a<-data.matrix(beaver.ori.out)
b<-data.matrix(beaver.mod.out)
test<-which(a != b, arr.ind=TRUE)
col<-unique(test[,2])
write(colnames(beaver.ori.out)[col],fileCom,append=TRUE)

# Coyote
write("", fileCom, append=TRUE)
write("Wildlife-Coyote Submodel:", fileCom, append=TRUE)
a<-data.matrix(coyote.ori.out)
b<-data.matrix(coyote.mod.out)
test<-which(a != b, arr.ind=TRUE)
col<-unique(test[,2])
write(colnames(coyote.ori.out)[col],fileCom,append=TRUE)

# Deer
write("", fileCom, append=TRUE)
write("Wildlife-Deer Submodel:", fileCom, append=TRUE)
a<-data.matrix(deer.ori.out)
b<-data.matrix(deer.mod.out)
test<-which(a != b, arr.ind=TRUE)
col<-unique(test[,2])
write(colnames(deer.ori.out)[col],fileCom,append=TRUE)

# Duck
write("", fileCom, append=TRUE)
write("Wildlife-Duck Submodel:", fileCom, append=TRUE)
a<-data.matrix(duck.ori.out)
b<-data.matrix(duck.mod.out)
test<-which(a != b, arr.ind=TRUE)
col<-unique(test[,2])
write(colnames(duck.ori.out)[col],fileCom,append=TRUE)

# Elk
write("", fileCom, append=TRUE)
write("Wildlife-Elk Submodel:", fileCom, append=TRUE)
a<-data.matrix(elk.ori.out)
b<-data.matrix(elk.mod.out)
test<-which(a != b, arr.ind=TRUE)
col<-unique(test[,2])
write(colnames(elk.ori.out)[col],fileCom,append=TRUE)

# Geese
write("", fileCom, append=TRUE)
write("Wildlife-Geese Submodel:", fileCom, append=TRUE)
a<-data.matrix(geese.ori.out)
b<-data.matrix(geese.mod.out)
test<-which(a != b, arr.ind=TRUE)
col<-unique(test[,2])
write(colnames(geese.ori.out)[col],fileCom,append=TRUE)

# Gulls
write("", fileCom, append=TRUE)
write("Wildlife-Gulls Submodel:", fileCom, append=TRUE)
a<-data.matrix(gulls.ori.out)
b<-data.matrix(gulls.mod.out)
test<-which(a != b, arr.ind=TRUE)
col<-unique(test[,2])
write(colnames(gulls.ori.out)[col],fileCom,append=TRUE)

# Herons_Egrets
write("", fileCom, append=TRUE)
write("Wildlife-Herons_Egrets Submodel:", fileCom, append=TRUE)
a<-data.matrix(heregr.ori.out)
b<-data.matrix(heregr.mod.out)
test<-which(a != b, arr.ind=TRUE)
col<-unique(test[,2])
write(colnames(heregr.ori.out)[col],fileCom,append=TRUE)

# Otter
write("", fileCom, append=TRUE)
write("Wildlife-Otter Submodel:", fileCom, append=TRUE)
a<-data.matrix(otter.ori.out)
b<-data.matrix(otter.mod.out)
test<-which(a != b, arr.ind=TRUE)
col<-unique(test[,2])
write(colnames(otter.ori.out)[col],fileCom,append=TRUE)

# Raccoon
write("", fileCom, append=TRUE)
write("Wildlife-Raccoon Submodel:", fileCom, append=TRUE)
a<-data.matrix(racoon.ori.out)
b<-data.matrix(racoon.mod.out)
test<-which(a != b, arr.ind=TRUE)
col<-unique(test[,2])
write(colnames(racoon.ori.out)[col],fileCom,append=TRUE)

close(fileCom)
return(0)
}