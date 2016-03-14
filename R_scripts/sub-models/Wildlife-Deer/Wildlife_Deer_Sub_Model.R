wildlifeDeer <- function(chr.input="wildlifeDeerxx.txt", chr.wrkdir=getwd()) {

  #setwd(chr.wrkdir)
  
  ##
  ## Read input file
  SubModelFile <- paste0(chr.wrkdir,"/",chr.input)
  SubModelData <- read.delim(SubModelFile, sep=":",comment.char="*",stringsAsFactors=FALSE, header=FALSE)
  #SubModelFilename <- strsplit(chr.input,".",fixed=TRUE)[[1]][[1]]
  names(SubModelData) <- c("parameter","value(s)")
  ##
  ### Getting input parameter values
  ### HSPF related information
  tmp.MUTSINStartYr <- as.numeric(SubModelData[1,2])
  tmp.MUTSINEndYr   <- as.numeric(SubModelData[2,2])
  tmp.HdrACCUMPasture <- as.numeric(SubModelData[3,2])
  tmp.HdrSQLIMPasture <- as.numeric(SubModelData[4,2])
  tmp.HdrACCUMForest  <- as.numeric(SubModelData[5,2])
  tmp.HdrSQLIMForest  <- as.numeric(SubModelData[6,2])
  tmp.HdrACCUMRAOCUT  <- as.numeric(SubModelData[7,2])
  tmp.HdrSQLIMRAOCUT  <- as.numeric(SubModelData[8,2])
  tmp.SQLIMFactor     <- as.numeric(SubModelData[9,2])
  ### Habitats
  tmp.HabitatArea  <- as.numeric(SubModelData[10,2])
  tmp.PastureArea  <- as.numeric(SubModelData[11,2])
  tmp.ForestArea   <- as.numeric(SubModelData[12,2])
  tmp.RAOCUTArea  <- as.numeric(SubModelData[13,2])
  ### percent of habitat with stream access
  tmp.HabitatAreaWStreamAcess <- as.numeric(SubModelData[14,2])/100
  ### percent time spent in around streams
  tmp.PercentStrmTime <- as.numeric(SubModelData[15,2])/100
  ### Animal Densities
  tmp.ADHabitat <- as.numeric(SubModelData[16,2])
  ### bacteria production per animal
  tmp.bac.prod  <- as.numeric(SubModelData[17,2])
  ##
  ### Calculations
  ### Populations
  tmp.PopTotal  <- round(tmp.HabitatArea * tmp.ADHabitat,digits=0)
  tmp.PopOnLand <- round((1 - tmp.HabitatAreaWStreamAcess) * tmp.HabitatArea * tmp.ADHabitat,digits=0) + round((1 - tmp.PercentStrmTime) * tmp.HabitatAreaWStreamAcess * tmp.HabitatArea * tmp.ADHabitat,digits=0)
  tmp.PopOnForest     <- round(tmp.PopOnLand * (tmp.ForestArea / tmp.HabitatArea),digits=0)
  tmp.PopOnPasture    <- round(tmp.PopOnLand * (tmp.PastureArea / tmp.HabitatArea),digits=0)
  tmp.PopOnRAOCUT    <- round(tmp.PopOnLand * (tmp.RAOCUTArea / tmp.HabitatArea),digits=0)
  tmp.PopInStream  <- round(tmp.PercentStrmTime * tmp.HabitatAreaWStreamAcess * tmp.HabitatArea * tmp.ADHabitat,digits=0)
  ### bacteria loads
  tmp.bacteria.Total    <- round(tmp.bac.prod * tmp.PopTotal,digits=0)
  tmp.bacteria.TotalOnLand  <- round(tmp.bac.prod * tmp.PopOnLand,digits=0)
  tmp.bacteria.TotalInStream  <- round(tmp.bac.prod * tmp.PopInStream,digits=0)/24
  tmp.bacteria.OnForest     <- round(tmp.bac.prod * tmp.PopOnForest,digits=0)
  tmp.bacteria.OnPasture    <- round(tmp.bac.prod * tmp.PopOnPasture ,digits=0)
  tmp.bacteria.OnRAOCUT    <- round(tmp.bac.prod * tmp.PopOnRAOCUT ,digits=0)

  ### accum values
  tmp.accum.Forest  <- round(tmp.bacteria.OnForest / tmp.ForestArea,digits=0)
  tmp.accum.Pasture <- round(tmp.bacteria.OnPasture / tmp.PastureArea,digits=0)
  tmp.accum.RAOCUT  <- round(tmp.bacteria.OnRAOCUT / tmp.RAOCUTArea,digits=0)

  
  ##
  ## Assemble output data frame
  SubModelOutput <- data.frame(pop.total=tmp.PopTotal,
                               pop.total.on.land=tmp.PopOnLand,
                               pop.total.in.Forest=tmp.PopOnForest,
                               pop.total.on.Pasture=tmp.PopOnPasture,
                               pop.total.in.RAOCUT=tmp.PopOnRAOCUT,
                               pop.total.in.stream=tmp.PopInStream,
                               bac.total.on.land=tmp.bacteria.TotalOnLand,
                               bac.total.in.stream=tmp.bacteria.TotalInStream,
                               bac.forest.on.land=tmp.bacteria.OnForest,
                               bac.pasture.on.land=tmp.bacteria.OnPasture,
                               bac.RAOCUT.on.land=tmp.bacteria.OnRAOCUT,
                               Accum.Forest=tmp.accum.Forest,
                               Accum.Pasture=tmp.accum.Pasture,
                               Accum.RAOCUT=tmp.accum.RAOCUT,
                               SQLIM.factor=tmp.SQLIMFactor,
                               MUTSIN.Start.Year=tmp.MUTSINStartYr,
                               MUTSIN.End.Year=tmp.MUTSINEndYr,
                               SUP.ACCUM.Forest.line=tmp.HdrACCUMForest,
                               SUP.SQLIM.Forest.line=tmp.HdrSQLIMForest,
                               SUP.ACCUM.Pastrure.line=tmp.HdrACCUMPasture,
                               SUP.SQLIM.Pastrure.line=tmp.HdrSQLIMPasture,
                               SUP.ACCUM.RAOCUT.line=tmp.HdrACCUMRAOCUT,
                               SUP.SQLIM.RAOCUT.line=tmp.HdrSQLIMRAOCUT,
                               stringsAsFactors=FALSE)

  return(SubModelOutput)
}