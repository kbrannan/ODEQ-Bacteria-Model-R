wildlifeGeese <- function(chr.input="wildlifeGeesexx.txt",chr.wrkdir=getwd()) {

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
  ### months for seasons
  tmp.S1Months <- as.numeric(strsplit(SubModelData[10,2],",")[[1]])
  tmp.S2Months <- as.numeric(strsplit(SubModelData[11,2],",")[[1]])
  ### Habitats
  tmp.PastureArea  <- as.numeric(SubModelData[12,2])
  tmp.ForestArea   <- as.numeric(SubModelData[13,2])
  tmp.RAOCUTArea   <- as.numeric(SubModelData[14,2])
  ### Animal Densities
  tmp.S1AD  <- as.numeric(SubModelData[15,2])
  tmp.S2AD  <- as.numeric(SubModelData[16,2])
  ### Percent animals around streams and bacteria production per animal
  tmp.ArndStreams <- as.numeric(SubModelData[17,2])/100
  tmp.bac.prod  <- as.numeric(SubModelData[18,2])
  ##
  ### Calculations

  ###
  ### Animal Populations
  tmp.PopOnPasture <- rep(-1,12)
  tmp.PopOnPasture[tmp.S1Months] <- round((1 - tmp.ArndStreams) * tmp.PastureArea * tmp.S1AD,0)
  tmp.PopOnPasture[tmp.S2Months]  <- round((1 - tmp.ArndStreams) * tmp.PastureArea * tmp.S2AD,0)
  tmp.PopOnForest <- rep(-1,12)
  tmp.PopOnForest[tmp.S1Months]  <- round((1 - tmp.ArndStreams) * tmp.ForestArea * tmp.S1AD,0)
  tmp.PopOnForest[tmp.S2Months]  <- round((1 - tmp.ArndStreams) * tmp.ForestArea * tmp.S2AD,0)
  tmp.PopOnRAOCUT <- rep(-1,12)
  tmp.PopOnRAOCUT[tmp.S1Months]  <- round((1 - tmp.ArndStreams) * tmp.RAOCUTArea * tmp.S1AD,0)
  tmp.PopOnRAOCUT[tmp.S2Months]  <- round((1 - tmp.ArndStreams) * tmp.RAOCUTArea * tmp.S2AD,0)
  tmp.PopInStream <- rep(-1,12)
  tmp.PopInStream[tmp.S1Months]  <- round(tmp.ArndStreams * (tmp.PastureArea + tmp.ForestArea + tmp.RAOCUTArea) * tmp.S1AD,0)
  tmp.PopInStream[tmp.S2Months]  <- round(tmp.ArndStreams * (tmp.PastureArea + tmp.ForestArea + tmp.RAOCUTArea) * tmp.S2AD,0)
  ###
  ### Bacteria Production and Location
  ###
  ### Instream
  tmp.BacteriaInStream <- tmp.bac.prod * tmp.PopInStream
  ###
  ### Accume table values
  tmp.Accum.Pasture <- round(tmp.bac.prod * tmp.PopOnPasture / tmp.PastureArea,0)
  tmp.Accum.Forest  <- round(tmp.bac.prod * tmp.PopOnForest  / tmp.ForestArea,0)
  tmp.Accum.RAOCUT  <- round(tmp.bac.prod * tmp.PopOnRAOCUT  / tmp.RAOCUTArea,0)
  ##
  ### Assemble output data frame
  SubModelOutput <- data.frame(Month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                               pop.total=tmp.PopOnPasture + tmp.PopOnForest + tmp.PopOnRAOCUT + tmp.PopInStream,
                               pop.total.on.land=tmp.PopOnPasture + tmp.PopOnForest + tmp.PopOnRAOCUT,
                               pop.total.in.stream=tmp.PopInStream,
                               pop.total.on.pasture=tmp.PopOnPasture,
                               pop.total.in.forest=tmp.PopOnForest,
                               pop.total.on.raocut=tmp.PopOnRAOCUT,
                               bac.total.in.stream=tmp.BacteriaInStream,
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