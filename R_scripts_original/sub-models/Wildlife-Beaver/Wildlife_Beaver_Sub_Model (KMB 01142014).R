wildlifeBeaver <- function(chr.input="wildlifeBeaverxx.txt",chr.wrkdir=getwd()) {
  setwd(chr.wrkdir)
  
  SubModelFile <- paste0(chr.wrkdir,"/",chr.input)
  SubModelData <- read.delim(SubModelFile, sep=":",comment.char="*",stringsAsFactors=FALSE, header=FALSE)
  SubModelFilename <- strsplit(chr.input,".",fixed=TRUE)[[1]][[1]]
  
  names(SubModelData) <- c("parameter","value(s)")
  
  # print out the matrix readed into the Cow-Calf model
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
  tmp.PopTotal   <- round(tmp.HabitatArea * tmp.ADinHabitat,digits=0)
  tmp.PopOnLand     <- round(((1-tmp.PercentStrmTime) * tmp.PopTotal),digits=0)
  tmp.PopInStrm <- round((tmp.PercentStrmTime * tmp.PopTotal),digits=0)
  ### bacteria loads
  tmp.bacteria.TotalOnLand  <- round(tmp.bac.prod * tmp.PopOnLand,digits=0)
  tmp.bacteria.Total.InStrm   <- round(tmp.bac.prod * tmp.PopInStrm,digits=0)
  ### accum values
  tmp.accum.forest  <- round(tmp.bacteria.TotalOnLand / tmp.HabitatArea,digits=0)
  
  ##
  ## Assemble output data frame
  SubModelOutput <- data.frame(pop.total=tmp.PopTotal,
                               pop.total.on.land=tmp.PopOnLand,
                               pop.total.in.stream=tmp.PopInStrm,
                               bac.total.on.land=tmp.bacteria.TotalOnLand,
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

  
  # print the output dataframe
  foname <- paste0(chr.wrkdir,"/output_",chr.input)
  write.csv(SubModelOutput, foname)
  
  return(SubModelOutput)
}