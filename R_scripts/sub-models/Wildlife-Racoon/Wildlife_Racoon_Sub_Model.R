wildlifeRacoon <- function(chr.input="wildlifeRacoonxx.txt",chr.wrkdir=getwd()) {

  ## read input file
  SubModelFile <- paste0(chr.wrkdir,"/",chr.input)
  SubModelData <- read.delim(SubModelFile, sep=":",comment.char="*",stringsAsFactors=FALSE, header=FALSE)
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
  tmp.SQLIMFactor  <- as.numeric(SubModelData[9,2])
  ### Habitats
  tmp.PastureArea  <- as.numeric(SubModelData[10,2])
  tmp.ForestArea   <- as.numeric(SubModelData[11,2])
  tmp.RAOCUTArea   <- as.numeric(SubModelData[12,2])
  ### Animal Densities
  tmp.AD  <- as.numeric(SubModelData[13,2])
  ### Percent animals around streams and bacteria production per animal
  tmp.ArndStreams <- as.numeric(SubModelData[14,2])/100
  tmp.bac.prod  <- as.numeric(SubModelData[15,2])
  ##
  ### Calculations
  ###
  ### Animal Populations
  tmp.PopTotal     <-(tmp.PastureArea + tmp.ForestArea + tmp.RAOCUTArea) * tmp.AD
  tmp.PopOnPasture <- (1 - tmp.ArndStreams) * tmp.PastureArea * tmp.AD
  tmp.PopInForest  <- (1 - tmp.ArndStreams) * tmp.ForestArea * tmp.AD
  tmp.PopOnRAOCUT  <- (1 - tmp.ArndStreams) * tmp.RAOCUTArea * tmp.AD
  tmp.PopOnLand    <- tmp.PopOnPasture + tmp.PopInForest + tmp.PopOnRAOCUT
  tmp.PopInStream  <- tmp.ArndStreams * (tmp.PastureArea + tmp.ForestArea + tmp.RAOCUTArea) * tmp.AD
  ###
  ### Bacteria Production and Location
  ###
  ### total
  tmp.BacteriaTotal <- tmp.bac.prod * tmp.PopTotal
  ###
  ### Instream
  tmp.BacteriaInStream <- tmp.bac.prod * tmp.PopInStream
  ### on land
  tmp.BacteriaOnPasture <- tmp.bac.prod * tmp.PopOnPasture
  tmp.BacteriaInForest  <- tmp.bac.prod * tmp.PopInForest
  tmp.BacteriaOnRAOCUT <- tmp.bac.prod * tmp.PopOnRAOCUT
  tmp.BacteriaOnLand <- tmp.BacteriaOnPasture + tmp.BacteriaInForest + 
    tmp.BacteriaOnRAOCUT
  ###
  ### Accume table values
  tmp.Accum.Pasture <- tmp.BacteriaOnPasture / tmp.PastureArea
  tmp.Accum.Forest  <- tmp.BacteriaInForest  / tmp.ForestArea
  tmp.Accum.RAOCUT  <- tmp.BacteriaOnRAOCUT  / tmp.RAOCUTArea
  ##
  ### Assemble output data frame
  SubModelOutput <- data.frame(pop.total=tmp.PopTotal,
                               pop.total.on.land=tmp.PopOnLand,
                               pop.total.on.Pasture=tmp.PopOnPasture,
                               pop.total.in.Forest=tmp.PopInForest,
                               pop.total.on.RAOCUT=tmp.PopOnRAOCUT,
                               pop.total.in.stream=tmp.PopInStream,
                               bac.total=tmp.BacteriaTotal,
                               bac.total.on.land=tmp.BacteriaOnLand,
                               bac.total.in.stream=tmp.BacteriaInStream,
                               bac.pasture.on.land=tmp.BacteriaOnPasture,
                               bac.forest.on.land=tmp.BacteriaInForest,
                               bac.RAOCUT.on.land=tmp.BacteriaOnRAOCUT,
                               Accum.Pasture=tmp.Accum.Pasture,
                               Accum.Forest=tmp.Accum.Forest,
                               Accum.RAOCUT=tmp.Accum.RAOCUT,
                               SQLIM.factor=tmp.SQLIMFactor,
                               MUTSIN.Start.Year=tmp.MUTSINStartYr,
                               MUTSIN.End.Year=tmp.MUTSINEndYr,
                               SUP.ACCUM.pastrure.line=tmp.HdrACCUMPasture,
                               SUP.SQLIM.pastrure.line=tmp.HdrSQLIMPasture,
                               SUP.ACCUM.forest.line=tmp.HdrACCUMForest,
                               SUP.SQLIM.forest.line=tmp.HdrSQLIMForest,
                               SUP.ACCUM.RAOCUT.line=tmp.HdrACCUMRAOCUT,
                               SUP.SQLIM.RAOCUT.line=tmp.HdrSQLIMRAOCUT,
                               stringsAsFactors=FALSE)
  ##
  ### return results
  return(SubModelOutput)
}