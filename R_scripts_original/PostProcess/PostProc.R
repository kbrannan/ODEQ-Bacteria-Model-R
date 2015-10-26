##
## turn off warnings
options(warn=-1)

## simulations directory
##chr.sim.dir <- "C:/Temp/PEST/BigElkPEST/host"
##chr.sim.dir <- commandArgs(TRUE)
chr.sim.dir <- gsub("(Model/.*$){1}","Model",getwd())
## load packakge
## none
## Load functions
source(paste0(chr.sim.dir,"/PostProcess/PostProcFunctions.R"))

## bacteria criterion E. coli SSM org/100 ml
crit.SSM <- 406

## data window size days before and after plus the day of the sample
n.days <- 2

## get model output and observed data
df.data <- get.data.window.flw_ld(chr.sim.dir,n.days)

## get data for obs abov 406 crit
df.abv <- df.data[df.data$obs >= crit.SSM, ]

## use the mod.ave as the sim data and let PEST calculate the residuals
# junk <- with(df.data,paste0(as.character(rch),strftime(date,format="%Y%m%d"),
#                "    ",sprintf(fmt="%1.4E",mod.ave)))
junk <- with(df.data,paste0(as.character(rch),strftime(date,format="%Y%m%d"),"   ",
                            "    ",sprintf(fmt="%1.6E",log10(mod.ave))))
junk.abv <- with(df.abv,paste0(as.character(rch),strftime(date,format="%Y%m%d"),"dig",
                            "    ",sprintf(fmt="%1.6E",log10(mod.ave))))
junk <- c(junk,junk.abv)



cat(junk,file=paste0(chr.sim.dir,"/PostProcess/modout.out"),sep="\n")


# ## get stations and rch entries
# df.stn.rch <- unique(df.data[,c("rch","stn")])
# 
# ## get data above-at and below citeria
# df.above <- df.data[df.data$obs >= crit.SSM,]
# df.below <- df.data[df.data$obs <  crit.SSM,]
# 
# ## at or above criterion stats
# stats.above <- data.frame(mse=NA,rmse=NA,NS.E=NA,sensitivity=NA,accuracy=NA,win.above=NA,win.within=NA,win.below=NA)
# ## mean square error to mod average
# stats.above$mse <- MSE(df.above$obs,df.above$mod.ave)
# ## root mean square error to mod average
# stats.above$rmse <- RSE(df.above$obs,df.above$mod.ave)
# ## Nash-Suttclif E to mod average
# stats.above$NS.E <- MSE(df.above$obs,df.above$mod.ave)
# ## class scores to mod average
# c.stats <- class.stats(df.above$obs,df.above$mod.ave,crit.SSM,a=0.5)
# ### using sesitivity and accuracy for the values at or above crit
# stats.above$sensitivity <- class.score(c.stats$sensitivity,base=1000)
# stats.above$accuracy <- class.score(c.stats$accuracy,base=1000)
# ## within window scores
# n.above <- length(df.above$obs)
# ### % above window
# stats.above$win.above <- 100*(length(df.above$obs[df.above$obs > df.above$mod.max])/n.above)
# ### % winthin window
# stats.above$win.within <- 100*(length(df.above$obs[df.above$obs <= df.above$mod.max & df.above$obs >= df.above$mod.min])/n.above)
# ### % below window
# stats.above$win.below <- 100*(length(df.above$obs[df.above$obs < df.above$mod.min])/n.above)
# 
# ## below criterion stats
# stats.below <- data.frame(mse=NA,rmse=NA,NS.E=NA,specitivity=NA,accuracy=NA,win.above=NA,win.within=NA,win.below=NA)
# ## mean square error to mod average
# stats.below$mse <- MSE(df.below$obs,df.below$mod.ave)
# ## root mean square error to mod average
# stats.below$rmse <- RSE(df.below$obs,df.below$mod.ave)
# ## Nash-Suttclif E to mod average
# stats.below$NS.E <- MSE(df.below$obs,df.below$mod.ave)
# ## class scores to mod average
# c.stats <- class.stats(df.below$obs,df.below$mod.ave,crit.SSM,a=0.5)
# ### using specitivity and accuracy for the values below crit
# stats.below$specitivity <- class.score(c.stats$specitivity,base=1000)
# stats.below$accuracy <- class.score(c.stats$accuracy,base=1000)
# ## within window scores
# n.below <- length(df.below$obs)
# ### % above window
# stats.below$win.above <- 100*(length(df.below$obs[df.below$obs > df.below$mod.max])/n.below) + 1E-06
# ### % winthin window
# stats.below$win.within <- 100*(length(df.below$obs[df.below$obs <= df.below$mod.max & df.below$obs >= df.below$mod.min])/n.below)
# ### % below window
# stats.below$win.below <- 100*(length(df.below$obs[df.below$obs < df.below$mod.min])/n.below)
# 
# ## individual values
# pnts.max.min.all <- paste0(formatC(gsub("[-]","",df.data$mtch),format="s",width=20,flag="-"),formatC(max.min.pen(df.data$obs,df.data$mod.max,df.data$mod.min,a=1E-02),format="E",width=16,digits=10,,flag=" "))
# ##pnts.max.min.above <- paste0(formatC(gsub("[-]","",df.above$mtch),format="s",width=20,flag="-"),formatC(max.min.pen(df.above$obs,df.above$mod.max,df.above$mod.min,a=1E-02),format="E",width=16,digits=10,,flag=" "))
# ##pnts.max.min.below <- paste0(formatC(gsub("[-]","",df.below$mtch),format="s",width=20,flag="-"),formatC(max.min.pen(df.below$obs,df.below$mod.max,df.below$mod.min,a=1E-02),format="E",width=16,digits=10,,flag=" "))
# 
# ## create strings for output
# ##pest.out <- c(paste0(formatC(paste0("above_",names(stats.above)),format="s",width=20,flag="-"),formatC(as.numeric(stats.above),format="E",width=16,digits=10,,flag=" ")),paste0(formatC(paste0("below_",names(stats.below)),format="s",width=20,flag="-"),formatC(as.numeric(stats.below),format="E",width=16,digits=10,,flag=" ")),pnts.max.min.above,pnts.max.min.below)
# pest.out <- c(paste0(formatC(paste0("above_",names(stats.above)),format="s",width=20,flag="-"),formatC(as.numeric(stats.above),format="E",width=16,digits=10,,flag=" ")),paste0(formatC(paste0("below_",names(stats.below)),format="s",width=20,flag="-"),formatC(as.numeric(stats.below),format="E",width=16,digits=10,,flag=" ")),pnts.max.min.all)
# ## write output to file for pest
# write.table(pest.out,file=paste0(chr.sim.dir,"/PostProcess/modout.out"),col.names=FALSE,row.names=FALSE,quote=FALSE,sep="")

