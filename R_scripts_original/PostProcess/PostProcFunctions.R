 ## Post-prcessing functions
ssm.rate <- function(val,ssm=406) {
  
  N <- length(val)
  above <- sum(as.numeric(val >= crit.SSM))
  rate <- above/N
  
  out <- data.frame(N,above,rate)
  return(out)
}


ssm.rate.by <- function(x,ssm=406) {
  rchs <- unique(x$rch)
  out <- data.frame(rch=rchs,N=0,above=0,rate=0)
  
  for(ii in 1:length(rchs)) {
    junk <- ssm.rate(x$val[grep(rchs[ii],x$rch)],ssm)
    out$N[ii] <- junk$N
    out$above[ii] <- junk$above
    out$rate[ii] <- junk$rate
    rm(junk)
  }
  return(out)
}

getout <- function(chr.fn) {
  ## gets data from HSPF pltgen for a specific model setup for Big Elk Creek
  ## and returns a long format data frame with columns date, rch, and val
  
  ## read pltgen output file
  modout <- scan(chr.fn,what=character(),sep="\n",skip=26,quiet = TRUE)
  
  ## create date vector from pltgen file year, month, day, hour, and second columns
  tmp.date <- as.POSIXlt(substr(modout,7,22),format="%Y %m %d %H %S")
  
  ## create data frame in long form from pltgen data
  df.data <-rbind(data.frame(date=tmp.date, rch="rch04",val=as.numeric(substr(modout,23,36))),data.frame(date=tmp.date, rch="rch09",val=as.numeric(substr(modout,37,50))),data.frame(date=tmp.date, rch="rch12",val=as.numeric(substr(modout,51,64))),data.frame(date=tmp.date, rch="rch17",val=as.numeric(substr(modout,65,78))),data.frame(date=tmp.date, rch="rch18",val=as.numeric(substr(modout,79,92))))

  ## reasign negative number becuase this is an artifact of HSPF flags NaN and carried through the aggregation calculation
  df.data[df.data$val < 0, "val"] <- 10
  ## reasign non fintite numbers for same reason as above
  df.data$val[is.finite(df.data$val) != TRUE] <- 10
  ## return data frame
  return(df.data)
}

fc2ec <- function(fc) {
  ## translates fecal coliform (fc) consentrations to E. coli (ec) concentrations
  ## using the equation developed by Cude (2005), which is:
  ##                   ec = 0.531 * fc ^ 1.06
  ## Reference: Cude, C.G., 2005. Accomodating Change of Bacteria Indicators
  ##            in Long Term Water Quality Datasets. Journal of American
  ##            Water Resources Association. pp. 47-54
  ec <- 0.531*fc^1.06
  return(ec)
}

max.min.pen <- function(x,max,min,a=1) {

  if(length(x) > 1) {
    out <- rep(0,length(x))
    for(ii in 1:length(x)) {
      if(x[ii] > max[ii]) {out[ii] <- a*(x[ii]-max[ii])^2}
      if(x[ii] < min[ii]) {out[ii] <- a*(x[ii]-min[ii])^2}
    }
  }
  if(length(x) == 1) {
    out <- 0
    if(x > max) {out <- a*(x-max)^2}
    if(x < min) {out <- a*(x-min)^2}
  }
  return(out)
}

get.data <- function(chr.sim.dir="C:/Temp/PEST/BigElkPEST/host") {
  ## contruct data.frame for model output
  tmp.file <- "bewqave.out"
  tmp.df <- data.frame(getout(paste0(chr.sim.dir,"/",tmp.file)), trn="AVE")
  df.modout <- data.frame(date=tmp.df$date,rch=tmp.df$rch)
  rm(tmp.file,tmp.df)
  ## get average time-series from model output
  tmp.file <- "bewqave.out"
  tmp.df <- data.frame(getout(paste0(chr.sim.dir,"/",tmp.file)), trn="AVE")
  df.modout <- data.frame(df.modout,ave=fc2ec(tmp.df$val))
  rm(tmp.file,tmp.df)
  ## get max time-series from model output
  tmp.file <- "bewqmax.out"
  tmp.df <- data.frame(getout(paste0(chr.sim.dir,"/",tmp.file)), trn="MAX")
  df.modout <- data.frame(df.modout,max=fc2ec(tmp.df$val))
  rm(tmp.file,tmp.df)
  ## get min time-series from model output
  tmp.file <- "bewqmin.out"
  tmp.df <- data.frame(getout(paste0(chr.sim.dir,"/",tmp.file)), trn="MIN")
  df.modout <- data.frame(df.modout,min=fc2ec(tmp.df$val))
  rm(tmp.file,tmp.df)
  ## add mtch variable for selecting simulated data on dates of obs data
  df.modout <- data.frame(df.modout,mtch=paste0(df.modout$rch,strptime(df.modout$date,format="%F")))
  
  ## read observed data
  load(file=paste0(chr.sim.dir,"/ObsData/obs.RData"))
  ## aggregate samples collected on the same day to max for the day
  obs.data$date <- strptime(obs.data$date,format="%Y-%m-%d")
  junk00 <- aggregate(formula= ec ~ mtch,data=obs.data,FUN="max")
  junk01 <- obs.data[,-grep("ec",names(obs.data))]
  junk02 <- unique(junk01)
  obs.data<-merge(junk02,junk00,by="mtch")
  rm(junk00,junk01,junk02)
  
  ## get simulated data for the dates of obs data
  junk <- merge(obs.data,df.modout,by="mtch")
  ##df.obs.mod <- data.frame(stn=junk$stn,date=junk$date.x,rch=junk$rch.x,obs=log10(junk$ec),mod.ave=log10(junk$ave),mod.max=log10(junk$max),mod.min=log10(junk$min),mtch=junk$mtch)
  df.obs.mod <- data.frame(stn=junk$stn,date=junk$date.x,rch=junk$rch.x,obs=junk$ec,mod.ave=junk$ave,mod.max=junk$max,mod.min=junk$min,mtch=junk$mtch)
  for(kk in 1:nlevels(df.obs.mod$rch)) {
    df.obs.mod$mod.min[df.obs.mod$rch ==  levels(df.obs.mod$rch)[kk] & is.na(df.obs.mod$mod.min) == TRUE] <-  min(df.obs.mod$mod.min[df.obs.mod$rch == levels(df.obs.mod$rch)[kk]], na.rm=TRUE)
    df.obs.mod$mod.ave[df.obs.mod$rch ==  levels(df.obs.mod$rch)[kk] & is.na(df.obs.mod$mod.ave) == TRUE] <- mean(df.obs.mod$mod.min[df.obs.mod$rch == levels(df.obs.mod$rch)[kk]], na.rm=TRUE)
    df.obs.mod$mod.max[df.obs.mod$rch ==  levels(df.obs.mod$rch)[kk] & is.na(df.obs.mod$mod.max) == TRUE] <-  max(df.obs.mod$mod.min[df.obs.mod$rch == levels(df.obs.mod$rch)[kk]], na.rm=TRUE)
  }
  rm(junk,df.modout,obs.data)
  return(df.obs.mod)
}

get.data.window <- function(chr.sim.dir="C:/Temp/PEST/BigElkPEST/host",n.day=2) {
  ## contruct data.frame for model output
  tmp.file <- "bewqave.out"
  tmp.df <- data.frame(getout(paste0(chr.sim.dir,"/",tmp.file)), trn="AVE")
  df.modout <- data.frame(date=tmp.df$date,rch=tmp.df$rch)
  rm(tmp.file,tmp.df)
  ## get average time-series from model output
  tmp.file <- "bewqave.out"
  tmp.df <- data.frame(getout(paste0(chr.sim.dir,"/",tmp.file)), trn="AVE")
  df.modout <- data.frame(df.modout,ave=fc2ec(tmp.df$val))
  rm(tmp.file,tmp.df)
  ## get max time-series from model output
  tmp.file <- "bewqmax.out"
  tmp.df <- data.frame(getout(paste0(chr.sim.dir,"/",tmp.file)), trn="MAX")
  df.modout <- data.frame(df.modout,max=fc2ec(tmp.df$val))
  rm(tmp.file,tmp.df)
  ## get min time-series from model output
  tmp.file <- "bewqmin.out"
  tmp.df <- data.frame(getout(paste0(chr.sim.dir,"/",tmp.file)), trn="MIN")
  df.modout <- data.frame(df.modout,min=fc2ec(tmp.df$val))
  rm(tmp.file,tmp.df)
  ## add mtch variable for selecting simulated data on dates of obs data
  df.modout <- data.frame(df.modout,mtch=paste0(df.modout$rch,strptime(df.modout$date,format="%F")),row=1:length(df.modout[,1]))
  
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
                           mod.ave=apply(indx,MARGIN=1,win.agg,df.modout[,"ave"],"mean"),
                           mod.max=apply(indx,MARGIN=1,win.agg,df.modout[,"max"],"max"),
                           mod.min=apply(indx,MARGIN=1,win.agg,df.modout[,"min"],"min"),
                           mtch=obs.data$mtch)
  
  df.obs.mod[is.finite(df.obs.mod[,c("mod.min")]) == FALSE,"mod.min"] <- 1
  df.obs.mod[is.finite(df.obs.mod[,c("mod.ave")]) == FALSE,"mod.ave"] <- 1
  df.obs.mod[is.finite(df.obs.mod[,c("mod.max")]) == FALSE,"mod.max"] <- 1
  
  return(df.obs.mod)
}

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

sqr.log10.res <- function(obs,mod) {
  adj.val <- 1E-05
  alt.obs <- obs + adj.val # incase value is zero
  alt.mod <- mod + adj.val # incase value is zero
  sqr.res <- ((log10(alt.mod/alt.obs))^2)/adj.val
  return(sqr.res) 
}

MSE <- function(obs,mod) {
  ## calculates mean square error
  N.obs <- length(obs)
  N.mod <- length(mod)
  if(N.obs != N.mod) {
    print("Observed and modeled vectors must be same length!!")
    mse <- NA
    return(mse)
  } else { 
    N <- N.obs
    mse <- sum((obs - mod)^2)/N
    return(mse)
  }
}

RSE <- function(obs,mod) {
  ## calculates realtive square error
  N.obs <- length(obs)
  N.mod <- length(mod)
  if(N.obs != N.mod) {
    print("Observed and modeled vectors must be same length!!")
    rse <- NA
    return(rse)
  } else {
    N <- N.obs
    rse <- sum((obs - mod)^2)/sum((obs - mean(obs))^2)
    return(rse)
  }
}

class.stats <- function(obs,mod,crit=406,a=0.5) {
  ## calculated the classification statistics for comparing observed and 
  ## modeled vaules to a criterion
  N.obs <- length(obs)
  N.mod <- length(mod)
  if(N.obs != N.mod) {
    print("Observed and modeled vectors must be same length!!")
    cs <- NA
    return(cs)
  } else {
    N <- N.obs 
    c.obs <- rep(0,length(obs))
    c.mod <- rep(0,length(mod))
    
    c.obs[obs >= crit] <- 1
    c.mod[mod >= crit] <- 1
    
    
    cls <- data.frame(obs=obs,mod=mod,crit=crit,TP=0,TN=0,FP=0,FN=0)
    
    cls$TP[c.obs + c.mod == 2]  <- 1
    cls$TN[c.obs + c.mod == 0]  <- 1
    cls$FP[c.obs - c.mod == -1] <- 1
    cls$FN[c.obs - c.mod == 1]  <- 1
    
    TP <- sum(cls$TP)
    TN <- sum(cls$TN)
    FP <- sum(cls$FP)
    FN <- sum(cls$FN)
    
    precision <- TP/(TP + FP)
    sensitivity <- TP/(TP + FN)
    specitivity <- TN/(FP + TN)
    accuracy <- (TP + TN)/(TP + TN + FP + FN)
    invFm <- (a/sensitivity) + ((1-a)/precision)
    cs <- data.frame(precision=precision,sensitivity=sensitivity,
                     specitivity=specitivity,accuracy=accuracy,invFm=invFm,
                     TP=TP,TN=TN,FP=FP,FN=FN)
    return(cs)
  }
}

class.score <- function(c.stat,base=1000) {
  cs.scr <-   base^(1 - c.stat)
  return(cs.scr)
}

NS.E <- function(obs,mod) {
  ## calculates the Nash and Sutcliffe coefficient of efficiency
  N.obs <- length(obs)
  N.mod <- length(mod)
  if(N.obs != N.mod) {
    print("Observed and modeled vectors must be same length!!")
    E <- NA
    return(E)
  } else {
    N <- N.obs
    E <- 1 -(sum((obs - mod)^2)/sum((obs - mean(obs))^2))
    return(E)
  }
}