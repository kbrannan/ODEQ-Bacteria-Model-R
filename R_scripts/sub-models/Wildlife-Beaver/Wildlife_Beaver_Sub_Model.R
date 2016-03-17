wildlifeBeaver <- function(chr.input="wildlifeBeaverxx.txt",chr.wrkdir=getwd()) {
  #setwd(chr.wrkdir)
  
  SubModelFile <- paste0(chr.wrkdir,"/",chr.input)
  SubModelData <- read.delim(SubModelFile, sep=":",comment.char="*",stringsAsFactors=FALSE, header=FALSE)
  SubModelFilename <- strsplit(chr.input,".",fixed=TRUE)[[1]][[1]]
  
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
  ### Habitat (only forest)
  tmp.HabitatArea   <- as.numeric(SubModelData[8,2])
  ### Population Densities
  tmp.ADinHabitat  <- as.numeric(SubModelData[9,2])
  ### percent time spent in around streams
  tmp.PercentStrmTime <- as.numeric(SubModelData[10,2])/100
  ### bacteria production per animal
  tmp.bac.prod  <- as.numeric(SubModelData[11,2])
  ##
  ### Calculations
  ### Populations
  tmp.PopTotal   <- tmp.HabitatArea * tmp.ADinHabitat
  tmp.PopOnLand     <- (1-tmp.PercentStrmTime) * tmp.PopTotal
  tmp.PopInStrm <- tmp.PercentStrmTime * tmp.PopTotal
  ### bacteria loads
  tmp.bacteria.TotalOnLand  <- tmp.bac.prod * tmp.PopOnLand
  tmp.bacteria.Total.InStrm   <- tmp.bac.prod * tmp.PopInStrm
  ### accum values
  tmp.accum.forest  <- tmp.bacteria.TotalOnLand / tmp.HabitatArea
  
  ##
  ## Assemble output data frame
  SubModelOutput <- data.frame(pop.total=tmp.PopTotal,
                               pop.total.on.land=tmp.PopOnLand,
                               pop.total.in.Forest=tmp.PopOnLand,
                               pop.total.in.stream=tmp.PopInStrm,
                               bac.total=tmp.bacteria.TotalOnLand + 
                                 tmp.bacteria.Total.InStrm,
                               bac.total.on.land=tmp.bacteria.TotalOnLand,
                               bac.total.in.Forest=tmp.bacteria.TotalOnLand,
                               bac.total.in.stream=tmp.bacteria.Total.InStrm,
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