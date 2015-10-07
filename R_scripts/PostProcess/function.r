get.data.window.flw_ld <- function(chr.sim.dir="C:/Temp/PEST/BigElkPEST/host",n.day=2) {
  ## get total daily load time-series from model output
  tmp.load <- getout(paste0(chr.sim.dir,"/","bewqsum.out"))
  ## get total daily flow time-series from model output (flow is already in 100 ml)
  tmp.flow <- getout(paste0(chr.sim.dir,"/","beflsum.out"))
  ## calculate daily cumlative average concentration 
  tmp.conc <- data.frame(tmp.flow[,c("date","rch")], val=fc2ec(tmp.load$val/tmp.flow$val))
  ## add mtch variable for selecting simulated data on dates of obs data
  df.modout <- data.frame(tmp.conc,mtch=paste0(tmp.conc$rch,strptime(tmp.conc$date,format="%F")),row=1:length(tmp.conc[,1]))
  ## read observed data
  load(file=paste0(chr.sim.dir,"/ObsData/obs.RData"))
  ## aggregate samples collected on the same day to max for the day
  obs.data$date <- strptime(obs.data$date,format="%Y-%m-%d")
  junk00 <- aggregate(formula= ec ~ mtch,data=obs.data,FUN="max")
  junk01 <- obs.data[,-grep("ec",names(obs.data))]
  junk02 <- unique(junk01)
  obs.data<-merge(junk02,junk00,by="mtch")
  rm(junk00,junk01,junk02)
  
  ids <- function(obs.data,n.win,df.modout) {
    idx <- df.modout$row[df.modout$rch == obs.data$rch & 
                           df.modout$date >= strptime(obs.data$date + 3600*24*-n.day,format="%F") &
                           df.modout$date <= strptime(obs.data$date + 3600*24*n.day,format="%F")]
    return(idx)
  }
  
  indx <- t(ids(obs.data[1,],n.day,df.modout))
  for(ii in 2:length(obs.data$date)) {
    indx <- rbind(indx,ids(obs.data[ii,],n.day,df.modout))
  }
  # get aggregated values based on window for observed data
  win.agg <- function(idx,dat,aggr) {
    switch(aggr,
           min = min(dat[idx]),
           mean = mean(dat[idx]),
           max = max(dat[idx]))
  }
  
  df.obs.mod <- data.frame(stn=obs.data$stn,date=obs.data$date,rch=obs.data$rch,obs=obs.data$ec,
                           mod.ave=apply(indx,MARGIN=1,win.agg,df.modout$val,"mean"),
                           mod.max=apply(indx,MARGIN=1,win.agg,df.modout$val,"max"),
                           mod.min=apply(indx,MARGIN=1,win.agg,df.modout$val,"min"),
                           mtch=obs.data$mtch)
  
  df.obs.mod[is.finite(df.obs.mod[,c("mod.min")]) == FALSE,"mod.min"] <- 1
  df.obs.mod[is.finite(df.obs.mod[,c("mod.ave")]) == FALSE,"mod.ave"] <- 1
  df.obs.mod[is.finite(df.obs.mod[,c("mod.max")]) == FALSE,"mod.max"] <- 1
  
  return(df.obs.mod)
}