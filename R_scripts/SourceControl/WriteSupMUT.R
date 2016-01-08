WriteSup <- function(sub.wtsd.num=sub.wtsd.N,sub.model.dirs=chr.sub.model.dirs,sub.model.input=chr.sub.model.input.files,sup.file="bigelkwq.sup",str.yr=1995,end.yr=2010, PEST.dir=chr.PEST.dir) {
  
	# Last Updated: 2014/01/22
	# Description: WriteSup runs sub-models for sources specified in the "sub.model.input"
	# Arguments:
	# sub.wtsd.num - number of sub-wtsds
	# sub.model.dirs - folders where the sub-models are located
	# sub.model.input - file name or partial name for sub-model input files
	# sup.file - name of HSPF Supplemental File
	# str.yr - starting year for MUTSIN time series.
	# end.yr - ending year for MUTSIN time series.
  # PEST.dir - folder where PEST run will occur

  ## turn off warnings
  options(warn=-1)

  ## generate numbers for sub-wtsds 
  sub.wtsds <- formatC(1:sub.wtsd.N, width = 2, format = "d", flag = "0")
  sub.wtsds <- sub.wtsds[-c(3,15)]
  
	# load sup file
  chr.sup.file <- scan(paste0(PEST.dir,"/",sup.file), what = character(0), sep = "\n", quiet = TRUE)

  ## Update PERLND QUAL-INPUT Table
  plq.file <- paste0(grep("General",sub.model.dirs,value=TRUE),"/",grep("^per{1}",sub.model.input,value=TRUE))
	plq.input <- read.delim(plq.file, sep=":",comment.char="*",stringsAsFactors=FALSE, header=FALSE)
	
  FWSQOP.line <- 2*as.numeric(strsplit(plq.input[3,2],",")[[1]])
  PWSQOP.line <- 2*as.numeric(strsplit(plq.input[5,2],",")[[1]])
  RWSQOP.line <- 2*as.numeric(strsplit(plq.input[7,2],",")[[1]])
  
  if(nchar(gsub("^[0-9 ]{1,}","",chr.sup.file[FWSQOP.line-1]))>0) n.hdr <- FWSQOP.line-1
  if(nchar(gsub("^[0-9 ]{1,}","",chr.sup.file[PWSQOP.line-1]))>0) n.hdr <- PWSQOP.line-1
  if(nchar(gsub("^[0-9 ]{1,}","",chr.sup.file[RWSQOP.line-1]))>0) n.hdr <- RWSQOP.line-1

  PQUAL.INPUT.hdr <- chr.sup.file[n.hdr]
  PQUAL.INPUT.names <- do.call(cbind,strsplit(PQUAL.INPUT.hdr,split=" {1,}"))[-c(1,2)]
  
  rpl.val <- grep("WSQOP",PQUAL.INPUT.names)

  FWSQOP.vals <- as.numeric(do.call(cbind,strsplit(chr.sup.file[FWSQOP.line],split=" {1,}")))[-1]
  PWSQOP.vals <- as.numeric(do.call(cbind,strsplit(chr.sup.file[PWSQOP.line],split=" {1,}")))[-1]
  RWSQOP.vals <- as.numeric(do.call(cbind,strsplit(chr.sup.file[RWSQOP.line],split=" {1,}")))[-1]
  
  FWSQOP.vals[rpl.val] <- plq.input[4,2]
  PWSQOP.vals[rpl.val] <- plq.input[6,2]
  RWSQOP.vals[rpl.val] <- plq.input[8,2]
  
  chr.sup.file[FWSQOP.line] <- paste0("             ",paste(formatC(as.numeric(FWSQOP.vals),digits=8,format="E",width=16, flag="-"),collapse=""))
  chr.sup.file[PWSQOP.line] <- paste0("             ",paste(formatC(as.numeric(PWSQOP.vals),digits=8,format="E",width=16, flag="-"),collapse=""))
  chr.sup.file[RWSQOP.line] <- paste0("             ",paste(formatC(as.numeric(RWSQOP.vals),digits=8,format="E",width=16, flag="-"),collapse=""))
## Update PERLND QUAL-INPUT Table
##  rchq.file <- paste0(grep("General",chr.sub.model.dirs,value=TRUE),"/",grep("^rch{1}",chr.sub.model.input.files,value=TRUE))
##  rchq.input <- read.delim(rchq.file, sep=":",comment.char="*",stringsAsFactors=FALSE, header=FALSE)
##  FSTDEC.line <- 2*as.numeric(strsplit(rchq.input[3,2],",")[[1]])
##  substr(chr.sup.file[FSTDEC.line],13,23) <- formatC(as.numeric(rchq.input[4,2]),format="E",width=(23-12),digits=3)
  
  
  ## Create Direct Deposit data frame
	str.date <- as.POSIXct(paste0(str.yr,"-01-01 00:00"), tz="America/Los_Angeles") 
	end.date <- as.POSIXct(paste0(end.yr,"-12-31 24:00"), tz="America/Los_Angeles")
	steps <- seq(str.date,end.date,by="month")
	dfmut <- data.frame(year=rep(NA,length(steps)))
	dfmut$year <- paste0("      ",format(steps,format="%Y"))
	dfmut$month <- paste0(" ",format(steps,format="%m"))
	dfmut$day <- paste0(" ", format(steps,format="%d"))
	dfmut$hour <- paste0(" ", format(steps,format="%H"))
	dfmut$min <- " 00"
	dfmut$val <- ""
  dfmut$year[as.numeric(dfmut$hour) == 0]  <- paste0("      ",format(steps[as.numeric(dfmut$hour) == 0]-1,format="%Y"))
	dfmut$month[as.numeric(dfmut$hour) == 0] <- paste0(" ",format(steps[as.numeric(dfmut$hour) == 0]-1,format="%m"))
	dfmut$day[as.numeric(dfmut$hour) == 0]   <- paste0(" ", format(steps[as.numeric(dfmut$hour) == 0]-1,format="%d"))
	dfmut$hour[as.numeric(dfmut$hour) == 0]  <- paste0(" ", 24)
  
	# Run source models, update SUP file, and create MUTSIN file for each subwatershed
	for (ii in 1:length(sub.wtsds)) {
	# Run CowCalf model
	  cow.calf.in <- paste0(grep("^[Cc]ow",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
		cow.calf.out <- cow.calf(chr.input.file=cow.calf.in,chr.wrkdir=grep("[Cc]ow",sub.model.dirs,value=TRUE))
		# Change NaN & Inf to 0 in cow.calf.out
	  cow.calf.out$Accum.Pasture[!is.finite(cow.calf.out$Accum.Pasture)] <- 0
	  cow.calf.out$Accum.Forest[!is.finite(cow.calf.out$Accum.Forest)] <- 0
	  cow.calf.out$Bacteria.InForestInStream[!is.finite(cow.calf.out$Bacteria.InForestInStream)] <- 0
	  cow.calf.out$Bacteria.OnPastureInStream[!is.finite(cow.calf.out$Bacteria.OnPastureInStream)] <- 0
	  cow.calf.out <- data.frame(cow.calf.out,bac.total.in.stream=(cow.calf.out$Bacteria.InForestInStream + cow.calf.out$Bacteria.OnPastureInStream))
	# Run onsite_pets model
	  onsite.pets.in <- paste0(grep("^[Oo]n[Ss]ite",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  onsite.pets.out <- onsite_pets(chr.input=onsite.pets.in,chr.wrkdir=grep("[Ss]ite",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in onsite.pets.out
	  onsite.pets.out$Accum.RAOCUT[!is.finite(onsite.pets.out$Accum.RAOCUT)] <- 0
	  onsite.pets.out$bac.onsite.NearStrmStrctFailure[!is.finite(onsite.pets.out$bac.onsite.NearStrmStrctFailure)] <- 0
	  onsite.pets.out <- data.frame(onsite.pets.out,bac.total.in.stream=onsite.pets.out$bac.onsite.NearStrmStrctFailure.to.stream.load)
	# Run Wildlife-Beaver model
	  beaver.in <- paste0(grep("^[Bb]eaver",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  beaver.out <- wildlifeBeaver(chr.input=beaver.in,chr.wrkdir=grep("Wildlife-[Bb]eaver",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in beaver.out
	  beaver.out$Accum.Forest[!is.finite(beaver.out$Accum.Forest)] <- 0
	  beaver.out$bac.total.in.stream[!is.finite(beaver.out$bac.total.in.stream)] <- 0
	# Run Wildlife-Coyote model
	  coyote.in <- paste0(grep("^[Cc]oyote",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  coyote.out <- wildlifeCoyote(chr.input=coyote.in,chr.wrkdir=grep("Wildlife-[Cc]oyote",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in coyote.out
	  coyote.out$Accum.Pasture[!is.finite(coyote.out$Accum.Pasture)] <- 0
	  coyote.out$Accum.Forest[!is.finite(coyote.out$Accum.Forest)] <- 0
	  coyote.out$Accum.RAOCUT[!is.finite(coyote.out$Accum.RAOCUT)] <- 0
	  coyote.out$bac.total.in.stream[!is.finite(coyote.out$bac.total.in.stream)] <- 0
	# Run Wildlife-Deer model
	  deer.in <- paste0(grep("^[Dd]eer",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  deer.out <- wildlifeDeer(chr.input=deer.in,chr.wrkdir=grep("Wildlife-[Dd]eer",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in deer.out
	  deer.out$Accum.Pasture[!is.finite(deer.out$Accum.Pasture)] <- 0
	  deer.out$Accum.Forest[!is.finite(deer.out$Accum.Forest)] <- 0
	  deer.out$bac.total.in.stream[!is.finite(deer.out$bac.total.in.stream)] <- 0
	# Run Wildlife-Duck model
	  duck.in <- paste0(grep("[Dd]uck",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  duck.out <- wildlifeDuck(chr.input=duck.in,chr.wrkdir=grep("Wildlife-[Dd]uck",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in duck.out
	  duck.out$Accum.Pasture[!is.finite(duck.out$Accum.Pasture)] <- 0
	  duck.out$Accum.Forest[!is.finite(duck.out$Accum.Forest)] <- 0
	  duck.out$Accum.RAOCUT[!is.finite(duck.out$Accum.RAOCUT)] <- 0
	  duck.out$bac.total.in.stream[!is.finite(duck.out$bac.total.in.stream)] <- 0
  # Run Wildlife-Elk model
	  elk.in <- paste0(grep("^[Ee]lk",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
		elk.out <- wildlifeElk(chr.input=elk.in,chr.wrkdir=grep("Wildlife-[Ee]lk",sub.model.dirs,value=TRUE))
		# Change NaN & Inf to 0 in elk.out
	  elk.out$Accum.Pasture[!is.finite(elk.out$Accum.Pasture)] <- 0
	  elk.out$Accum.Forest[!is.finite(elk.out$Accum.Forest)] <- 0
	  elk.out$Accum.RAOCUT[!is.finite(elk.out$Accum.RAOCUT)] <- 0
	  elk.out$bac.total.in.stream[!is.finite(elk.out$bac.from.pasture.in.stream)] <- 0
	# Run Wildlife-Geese model
	  geese.in <- paste0(grep("[Gg]eese",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  geese.out <- wildlifeGeese(chr.input=geese.in,chr.wrkdir=grep("Wildlife-[Gg]eese",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in geese.out
	  geese.out$Accum.Pasture[!is.finite(geese.out$Accum.Pasture)] <- 0
	  geese.out$Accum.Forest[!is.finite(geese.out$Accum.Forest)] <- 0
	  geese.out$Accum.RAOCUT[!is.finite(geese.out$Accum.RAOCUT)] <- 0
	  geese.out$bac.total.in.stream[!is.finite(geese.out$bac.total.in.stream)] <- 0
  # Run Wildlife-Gulls model
	  gulls.in <- paste0(grep("^[Gg]ulls",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  gulls.out <- wildlifeGulls(chr.input=gulls.in,chr.wrkdir=grep("Wildlife-[Gg]ulls",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in gulls.out
	  gulls.out$Accum.Pasture[!is.finite(gulls.out$Accum.Pasture)] <- 0
	  gulls.out$Accum.Forest[!is.finite(gulls.out$Accum.Forest)] <- 0
	  gulls.out$Accum.RAOCUT[!is.finite(gulls.out$Accum.RAOCUT)] <- 0
	  gulls.out$bac.total.in.stream[!is.finite(gulls.out$bac.total.in.stream)] <- 0
  # Run Wildlife-HerEgr model
	  heregr.in <- paste0(grep("[Hh]er[Ee]gr",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  heregr.out <- wildlifeHerEgr(chr.input=heregr.in,chr.wrkdir=grep("Wildlife-[Hh]erons_[Ee]grets",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in heregr.out
	  heregr.out$Accum.Pasture[!is.finite(heregr.out$Accum.Pasture)] <- 0
	  heregr.out$Accum.Forest[!is.finite(heregr.out$Accum.Forest)] <- 0
	  heregr.out$Accum.RAOCUT[!is.finite(heregr.out$Accum.RAOCUT)] <- 0
	  heregr.out$bac.total.in.stream[!is.finite(heregr.out$bac.total.in.stream)] <- 0
	# Run Wildlife-Otter model
	  otter.in <- paste0(grep("[Oo]tter",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  otter.out <- wildlifeOtter(chr.input=otter.in,chr.wrkdir=grep("Wildlife-[Oo]tter",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in heregr.out
	  otter.out$Accum.Pasture[!is.finite(otter.out$Accum.Pasture)] <- 0
	  otter.out$Accum.Forest[!is.finite(otter.out$Accum.Forest)] <- 0
	  otter.out$bac.total.in.stream[!is.finite(otter.out$bac.total.in.stream)] <- 0
  # Run Wildlife-Racoon model
	  racoon.in <- paste0(grep("^[Rr]acoon",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  racoon.out <- wildlifeRacoon(chr.input=racoon.in,chr.wrkdir=grep("Wildlife-[Rr]acoon",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in heregr.out
	  racoon.out$Accum.Pasture[!is.finite(racoon.out$Accum.Pasture)] <- 0
	  racoon.out$Accum.Forest[!is.finite(racoon.out$Accum.Forest)] <- 0
	  racoon.out$Accum.RAOCUT[!is.finite(racoon.out$Accum.RAOCUT)] <- 0
	  racoon.out$bac.total.in.stream[!is.finite(racoon.out$bac.total.in.stream)] <- 0
  # Sum MON-ACCUM from individual sources
	  accum.pasture <- cow.calf.out$Accum.Pasture + rep(coyote.out$Accum.Pasture,12) + rep(deer.out$Accum.Pasture,12) + duck.out$Accum.Pasture + elk.out$Accum.Pasture + geese.out$Accum.Pasture + rep(gulls.out$Accum.Pasture,12) + rep(heregr.out$Accum.Pasture,12) + rep(otter.out$Accum.Pasture,12) + rep(racoon.out$Accum.Pasture,12)
	  accum.forest  <- cow.calf.out$Accum.Forest + rep(beaver.out$Accum.Forest,12) + rep(coyote.out$Accum.Forest,12) + rep(deer.out$Accum.Forest,12) + duck.out$Accum.Forest + elk.out$Accum.Forest + geese.out$Accum.Forest + rep(gulls.out$Accum.Forest,12) + rep(heregr.out$Accum.Forest,12) + rep(otter.out$Accum.Forest,12) + rep(racoon.out$Accum.Forest,12)
 	  accum.RAOCUT  <- onsite.pets.out$Accum.RAOCUT + rep(coyote.out$Accum.RAOCUT,12) + duck.out$Accum.RAOCUT + elk.out$Accum.RAOCUT + geese.out$Accum.RAOCUT + rep(gulls.out$Accum.RAOCUT,12) + rep(heregr.out$Accum.RAOCUT,12) + rep(racoon.out$Accum.RAOCUT,12)
  # Sum MON-SQOLIM from individual sources
		sqlim.pasture <- accum.pasture * coyote.out$SQLIM.factor
	  sqlim.forest  <- accum.forest *  coyote.out$SQLIM.factor
	  sqlim.RAOCUT  <- accum.RAOCUT *  coyote.out$SQLIM.factor 
  # Update MON-ACCUM in Sup
	  chr.sup.file[coyote.out$SUP.ACCUM.pastrure.line[1]*2] <- paste0(formatC(accum.pasture,format="E",width=(15),digits=7),collapse="")
	  chr.sup.file[coyote.out$SUP.ACCUM.forest.line[1]*2]   <- paste0(formatC(accum.forest,format="E",width=(15),digits=7),collapse="")
	  chr.sup.file[coyote.out$SUP.ACCUM.RAOCUT.line[1]*2]   <- paste0(formatC(accum.RAOCUT,format="E",width=(15),digits=7),collapse="")
	# Update MON-SQOLIM in Sup
	  chr.sup.file[coyote.out$SUP.SQLIM.pastrure.line[1]*2] <- paste0(formatC(sqlim.pasture,format="E",width=(15),digits=7),collapse="")
	  chr.sup.file[coyote.out$SUP.SQLIM.forest.line[1]*2]   <- paste0(formatC(sqlim.forest,format="E",width=(15),digits=7),collapse="")
	  chr.sup.file[coyote.out$SUP.SQLIM.RAOCUT.line[1]*2]   <- paste0(formatC(sqlim.RAOCUT,format="E",width=(15),digits=7),collapse="")
  # Sum Direct Deposit from individual sources
    total.bacteria.in.stream <- cow.calf.out$bac.total.in.stream + onsite.pets.out$bac.total.in.stream + rep(beaver.out$bac.total.in.stream,12) + rep(coyote.out$bac.total.in.stream,12) + rep(deer.out$bac.total.in.stream,12) + duck.out$bac.total.in.stream + elk.out$bac.total.in.stream + geese.out$bac.total.in.stream + rep(gulls.out$bac.total.in.stream,12) + rep(heregr.out$bac.total.in.stream,12) + rep(otter.out$bac.total.in.stream,12) + rep(racoon.out$bac.total.in.stream,12)
  # Update Direct Deposit data frame and write to file
		for (jj in 1:12) {
      dfmut$val[as.numeric(dfmut$month) == jj] <- paste0("   ",formatC(total.bacteria.in.stream[jj]/24,digits=5,width=11,format="E",flag="0"))
		}
		header <- c("**** Direct Deposit Fecal Coliform Load",paste0("**** Big Elk Subwatershed",formatC(ii, width = 2, format = "d", flag = "0")),"      Year Mo Da Hr Mi   FC")
		fname <- paste0(PEST.dir,"/directdep",sub.wtsds[ii],".mut")
		write(header,fname)
		write.table(dfmut,fname,append=TRUE,row.name=FALSE,col.names=FALSE,sep="",quote=FALSE)
  # Remove temporary objects and reset Direct Deposit values
		rm(cow.calf.out)
 		rm(onsite.pets.out)
		rm(beaver.out)
		rm(coyote.out)
		rm(duck.out)
		rm(deer.out)
		rm(geese.out)
		rm(gulls.out)
		rm(herger.out)
		rm(otter)
		rm(racoon)
		rm(accum.pasture,accum.forest,accum.RAOCUT)
    rm(sqlim.pasture,sqlim.forest,sqlim.RAOCUT)
    rm(header)
    rm(fname)
    dfmut$val <- ""
	}
# Write Sup to file
	write(chr.sup.file,file=paste0(PEST.dir,"/",sup.file))
}