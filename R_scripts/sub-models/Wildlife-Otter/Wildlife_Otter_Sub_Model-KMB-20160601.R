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
  tmp.PopPasture <- tmp.PastureStrmLength * tmp.PopStrmOnly
  tmp.PopForest  <- tmp.ForestStrmLength * tmp.PopStrmOnly
  tmp.PopTotal   <- (tmp.PastureStrmLength + tmp.ForestStrmLength) * tmp.PopStrmOnly
  tmp.PopOnPasture     <- (1-tmp.PercentStrmTime) * tmp.PopPasture
  tmp.PopPastureInStrm <- tmp.PercentStrmTime * tmp.PopPasture
  tmp.PopOnForest     <- (1-tmp.PercentStrmTime) * tmp.PopForest
  tmp.PopForestInStrm <- tmp.PercentStrmTime * tmp.PopForest
  tmp.PopTotalOnLand <- (1-tmp.PercentStrmTime) * tmp.PopTotal
  tmp.PopTotalInStrm <- tmp.PercentStrmTime * tmp.PopTotal
  tmp.pop.total <- tmp.PopOnPasture + tmp.PopOnForest + tmp.PopTotalInStrm
  ### bacteria loads
  tmp.bacteria.OnPasture    <- tmp.bac.prod * tmp.PopOnPasture
  tmp.bacteria.OnForest     <- tmp.bac.prod * tmp.PopOnForest
  tmp.bacteria.TotalOnLand  <- tmp.bac.prod * tmp.PopTotalOnLand
  tmp.bacteria.Pasture.InStrm <- tmp.bac.prod * tmp.PopPastureInStrm
  tmp.bacteria.Forest.InStrm  <- tmp.bac.prod * tmp.PopForestInStrm
  tmp.bacteria.Total.InStrm   <- tmp.bac.prod * tmp.PopTotalInStrm
  ### accum values
  tmp.accum.pasture <- tmp.bacteria.OnPasture / tmp.PastureArea
  tmp.accum.forest  <- tmp.bacteria.OnForest / tmp.ForestArea
  
  
  
  ##
  ## Assemble output data frame
  SubModelOutput <- data.frame(pop.total=tmp.pop.total,
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
                               bac.total=tmp.bacteria.TotalOnLand+
                                 tmp.bacteria.Total.InStrm,
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