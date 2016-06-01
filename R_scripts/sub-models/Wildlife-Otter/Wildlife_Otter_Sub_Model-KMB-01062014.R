wildlifeOtter <- function(chr.input="wildlifeOtterxx.txt",
                     R.output=FALSE,
                     check.file=FALSE,
                     chr.wrkdir=getwd()) {
  #setwd(chr.wrkdir)
  
  SubModelFile <- paste0(chr.wrkdir,"/",chr.input)
  SubModelData <- read.delim(SubModelFile, sep=":",comment.char="*",
                             stringsAsFactors=FALSE, header=FALSE)
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
  tmp.SQLIMFactor  <- as.numeric(SubModelData[7,2])
  ### Habitats
  tmp.PastureArea  <- as.numeric(SubModelData[8,2])
  tmp.ForestArea   <- as.numeric(SubModelData[9,2])
  tmp.PastureStrmLength   <- as.numeric(SubModelData[10,2])
  tmp.ForestStrmLength   <- as.numeric(SubModelData[11,2])
  ### Population Densities
  tmp.PopStrmOnly  <- as.numeric(SubModelData[12,2])
  ### percent time spent in around streams
  tmp.PercentStrmTime <- as.numeric(SubModelData[13,2])/100
  ### bacteria production per animal
  tmp.bac.prod  <- as.numeric(SubModelData[14,2])
  ##
  ### Calculations
  ### Populations
  tmp.PopPasture <- round(tmp.PastureStrmLength * tmp.PopStrmOnly,digits=0)
  tmp.PopForest  <- round(tmp.ForestStrmLength * tmp.PopStrmOnly,digits=0)
  tmp.PopTotal   <- round((tmp.PastureStrmLength + tmp.ForestStrmLength) * tmp.PopStrmOnly,digits=0)
  tmp.PopOnPasture     <- round(((1-tmp.PercentStrmTime) * tmp.PopPasture),digits=0)
  tmp.PopPastureInStrm <- round((tmp.PercentStrmTime * tmp.PopPasture),digits=0)
  tmp.PopOnForest     <- round(((1-tmp.PercentStrmTime) * tmp.PopForest),digits=0)
  tmp.PopForestInStrm <- round((tmp.PercentStrmTime * tmp.PopForest),digits=0)
  tmp.PopTotalOnLand <- round(((1-tmp.PercentStrmTime) * tmp.PopTotal),digits=0)
  tmp.PopTotalInStrm <- round((tmp.PercentStrmTime * tmp.PopTotal),digits=0)
  ### bacteria loads
  tmp.bacteria.OnPasture    <- round(tmp.bac.prod * tmp.PopOnPasture,digits=0)
  tmp.bacteria.OnForest     <- round(tmp.bac.prod * tmp.PopOnForest,digits=0)
  tmp.bacteria.TotalOnLand  <- round(tmp.bac.prod * tmp.PopTotalOnLand,digits=0)
  tmp.bacteria.Pasture.InStrm <- round(tmp.bac.prod * tmp.PopPastureInStrm,digits=0)/24
  tmp.bacteria.Forest.InStrm  <- round(tmp.bac.prod * tmp.PopForestInStrm,digits=0)/24
  tmp.bacteria.Total.InStrm   <- round(tmp.bac.prod * tmp.PopTotalInStrm,digits=0)/24
  ### accum values
  tmp.accum.pasture <- round(tmp.bacteria.OnPasture / tmp.PastureArea,digits=0)
  tmp.accum.forest  <- round(tmp.bacteria.OnForest / tmp.ForestArea,digits=0)
  
  
  
  ##
  ## Assemble output data frame
  SubModelOutput <- data.frame(pop.total=tmp.PopTotal,
                               pop.total.on.land=tmp.PopTotalOnLand,
                               pop.total.in.stream=tmp.PopTotalInStrm,
                               pop.pasture=tmp.PopPasture,
                               pop.pasture.on.land=tmp.PopOnPasture,
                               pop.pasture.in.stream=tmp.PopPastureInStrm,
                               pop.forest=tmp.PopForest,
                               pop.forest.on.land=tmp.PopOnForest,
                               pop.forest.in.stream=tmp.PopForestInStrm,
                               bac.total.on.land=tmp.bacteria.TotalOnLand,
                               bac.total.in.stream=tmp.bacteria.Total.InStrm,
                               bac.pasture.on.land=tmp.bacteria.OnPasture,
                               bac.pasture.in.stream=tmp.bacteria.Pasture.InStrm,
                               bac.forest.on.land=tmp.bacteria.OnForest,
                               bac.forest.in.stream=tmp.bacteria.Forest.InStrm,
                               Accum.Pasture=tmp.accum.pasture,
                               Accum.Forest=tmp.accum.forest,
                               SQLIM.factor=tmp.SQLIMFactor,
                               MUTSIN.Start.Year=tmp.MUTSINStartYr,
                               MUTSIN.End.Year=tmp.MUTSINEndYr,
                               SUP.ACCUM.pastrure.line=tmp.HdrACCUMPasture,
                               SUP.SQLIM.pastrure.line=tmp.HdrSQLIMPasture,
                               SUP.ACCUM.forest.line=tmp.HdrACCUMForest,
                               SUP.SQLIM.forest.line=tmp.HdrSQLIMForest,
                               stringsAsFactors=FALSE)

  return(SubModelOutput)
}