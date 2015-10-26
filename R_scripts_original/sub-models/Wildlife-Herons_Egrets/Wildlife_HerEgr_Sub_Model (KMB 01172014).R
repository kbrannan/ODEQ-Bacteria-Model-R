wildlifeHerEgr <- function(chr.input="wildlifeHerEgrxx.txt",chr.wrkdir=getwd()) {

  ## read input file
  SubModelFile <- paste0(chr.wrkdir,"/",chr.input)
  SubModelData <- read.delim(SubModelFile, sep=":",comment.char="*",stringsAsFactors=FALSE, header=FALSE)
  names(SubModelData) <- c("parameter","value(s)")
  
# print out the matrix readed into the submodel
  fname <- paste0(chr.wrkdir,"/input_",chr.input)
  write.csv(SubModelData,fname)
  
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
  tmp.PopTotal     <- round((tmp.PastureArea + tmp.ForestArea + tmp.RAOCUTArea) * tmp.AD, 0)
  tmp.PopOnPasture <- round((1 - tmp.ArndStreams) * tmp.PastureArea * tmp.AD,0)
  tmp.PopOnForest  <- round((1 - tmp.ArndStreams) * tmp.ForestArea * tmp.AD,0)
  tmp.PopOnRAOCUT  <- round((1 - tmp.ArndStreams) * tmp.RAOCUTArea * tmp.AD,0)
  tmp.PopOnLand    <- tmp.PopOnPasture + tmp.PopOnForest + tmp.PopOnRAOCUT
  tmp.PopInStream  <- round(tmp.ArndStreams * (tmp.PastureArea + tmp.ForestArea + tmp.RAOCUTArea) * tmp.AD,0)
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
  SubModelOutput <- data.frame(pop.total=tmp.PopTotal,
                               pop.total.on.land=tmp.PopOnLand,
                               pop.total.on.pasture=tmp.PopOnPasture,
                               pop.total.on.forest=tmp.PopOnForest,
                               pop.total.on.RAOCUT=tmp.PopOnRAOCUT,
                               pop.total.in.stream=tmp.PopInStream,
                               bac.total.on.land=tmp.PopOnLand * tmp.AD,
                               bac.total.in.stream=tmp.BacteriaInStream,
                               bac.pasture.on.land=tmp.PopOnPasture * tmp.AD,
                               bac.forest.on.land=tmp.PopOnForest * tmp.AD,
                               bac.RAOCUT.on.land=tmp.PopOnRAOCUT * tmp.AD,
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
  
  # print the output dataframe
  foname <- paste0(chr.wrkdir,"/output_",chr.input)
  write.csv(SubModelOutput, foname)
  
  ##
  ### return results
  return(SubModelOutput)
}