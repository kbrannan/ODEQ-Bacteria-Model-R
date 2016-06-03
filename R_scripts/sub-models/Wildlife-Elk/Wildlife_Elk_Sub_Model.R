wildlifeElk <- function(chr.input.file) {
  #   This function is the bacteria source-model deer The model 
  #   generates input for HSPF. The specific outputs from this source model
  #   are loads from the beaver to land and directly to the stream
  #   The load to the land is in the form of load/acre for forest 
  #   PLS in a sub-watershed that the source-model contributes to and 
  #   the hourly load to the stream in the form of a MUTSIN file. The 
  #   input for the model is from an ASCII text file. Use the text below
  #   as a template for the input file. The symbol used for comments in
  #   the input file is "***". The definitions for the symbols used in
  #   the template are: YYYY is four-digit year, MM two-digit month, 
  #   DD is the two-digit day, ## is an integer, #.# is a floating point 
  #   number, and #.#E+## is a number in scientific notation

    ## read input files  
  df.input <- read.delim(chr.input.file, sep=":", 
                         comment.char="*", stringsAsFactors=FALSE, 
                         header=FALSE)
  names(df.input) <- c("parameter","value(s)")
  
  ##
  ## Getting input parameter values
  ##
  ## land use information
  lu.pasture.area.season.1   <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Pasture Area in Watershed (ac)"])
  lu.forest.area.season.1   <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Forest Area in Watershed (ac)"])
  lu.habitatarea.season.1 <- lu.pasture.area.seaon.1 + lu.forest.area.season.1
  lu.pasture.area.season.2   <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Pasture Area in Watershed (ac)"])
  lu.forest.area.season.2   <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Forest Area in Watershed (ac)"])
  lu.habitatarea.season.2 <- lu.pasture.area.seaon.2 + lu.forest.area.season.2
  ### Percent of Landuse with Stream access
  lu.pasture.area.w.seaon.1   <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Percent of Pasture with stream access"]) / 100
  lu.forest.area.w.season.1   <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Percent of Forest with stream access"]) / 100
  lu.pasture.area.w.seaon.2   <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Percent of Pasture with stream access"]) / 100
  lu.forest.area.w.season.2   <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Percent of Forest with stream access"]) / 100
  ## animal information
  ## months for seasons
  amn.months.season.1 <- as.numeric(strsplit(df.input$value[
    df.input$parameter == "Season 1 Months (numbers)"], split = ",")[[1]])
  amn.months.season.2 <- as.numeric(strsplit(df.input$value[
    df.input$parameter == "Season 1 Months (numbers)"], split = ",")[[1]])
  ## Animal Densities
  amn.animal.density.pasture.season.1  <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Animal Density for Pasture in watershed (animal/ac)"])
  amn.animal.density.forest.season.1  <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Animal Density for Forest in watershed (animal/ac)"])
  amn.animal.density.pasture.season.2  <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Animal Density for Pasture in watershed (animal/ac)"])
  amn.animal.density.forest.season.2  <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Animal Density for Forest in watershed (animal/ac)"])
    ## Percent animals around streams
  amn.percentstream.pasture.season.1 <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Percent of animals on Pasture in and around streams"]) / 100
  amn.percentstream.forest.season.1 <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Percent of animals on Forest in and around streams"]) / 100
  amn.percentstream.pasture.season.2 <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Percent of animals on Pasture in and around streams"]) / 100
  amn.percentstream.forest.season.2 <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Percent of animals on Forest in and around streams"]) / 100
  ## bacteria production per animal
  amn.bac.prod  <- as.numeric(df.input$value[
    df.input$parameter == "Bacteria Production of animal per day (orgs/day)"])
  amn.SQLIM.factor  <- as.numeric(df.input$value[
    df.input$parameter == "SQOLIM multiplcation factor"])

  ## Calculations
  ## populations
  ## overall locations
  pop.total.season.1 <-   lu.habitatarea * amn.animal.density.season.1
  pop.pasture.season.1 <- lu.pasture.area * amn.animal.density.season.1
  pop.forest.season.1 <-  lu.forest.area * amn.animal.density.season.1
  pop.RAOCUT.season.1 <-  lu.RAOCUT.area * amn.animal.density.season.1
  pop.total.season.2 <-   lu.habitatarea * amn.animal.density.season.2
  pop.pasture.season.2 <- lu.pasture.area * amn.animal.density.season.2
  pop.forest.season.2 <-  lu.forest.area * amn.animal.density.season.2
  pop.RAOCUT.season.2 <-  lu.RAOCUT.area * amn.animal.density.season.2
  ## on land
  pop.total.on.land.season.1 <-   (1 - amn.percentstream) * pop.total.season.1
  pop.pasture.on.land.season.1 <- (1 - amn.percentstream) * pop.pasture.season.1
  pop.forest.on.land.season.1 <-  (1 - amn.percentstream) * pop.forest.season.1
  pop.RAOCUT.on.land.season.1 <-  (1 - amn.percentstream) * pop.RAOCUT.season.1
  pop.total.on.land.season.2 <-   (1 - amn.percentstream) * pop.total.season.2
  pop.pasture.on.land.season.2 <- (1 - amn.percentstream) * pop.pasture.season.2
  pop.forest.on.land.season.2 <-  (1 - amn.percentstream) * pop.forest.season.2
  pop.RAOCUT.on.land.season.2 <-  (1 - amn.percentstream) * pop.RAOCUT.season.2
  ## in stream
  pop.total.in.stream.season.1 <-   amn.percentstream * pop.total.season.1
  pop.pasture.in.stream.season.1 <- amn.percentstream * pop.pasture.season.1
  pop.forest.in.stream.season.1 <-  amn.percentstream * pop.forest.season.1
  pop.RAOCUT.in.stream.season.1 <-  amn.percentstream * pop.RAOCUT.season.1
  pop.total.in.stream.season.2 <-   amn.percentstream * pop.total.season.2
  pop.pasture.in.stream.season.2 <- amn.percentstream * pop.pasture.season.2
  pop.forest.in.stream.season.2 <-  amn.percentstream * pop.forest.season.2
  pop.RAOCUT.in.stream.season.2 <-  amn.percentstream * pop.RAOCUT.season.2  
  
  
  ### Animal Densities
  ### Percent animals around streams and bacteria production per animal
  tmp.S1PastureArndStreams <- as.numeric(SubModelData[30,2])/100
  tmp.S1ForestArndStreams  <- as.numeric(SubModelData[31,2])/100
  tmp.S1RAOCUTArndStreams  <- as.numeric(SubModelData[32,2])/100
  tmp.S2PastureArndStreams <- as.numeric(SubModelData[33,2])/100
  tmp.S2ForestArndStreams  <- as.numeric(SubModelData[34,2])/100
  tmp.S2RAOCUTArndStreams  <- as.numeric(SubModelData[35,2])/100
  tmp.bac.prod  <- as.numeric(SubModelData[36,2])
  ##
  ### Calculations
  ### Migration to/within watershed
  tmp.PastureArea <- rep(-1,12)
  tmp.PastureArea[tmp.S1Months] <- tmp.S1PastureArea
  tmp.PastureArea[tmp.S2Months] <- tmp.S2PastureArea
  tmp.ForestArea <- rep(-1,12)
  tmp.ForestArea[tmp.S1Months]  <- tmp.S1ForestArea
  tmp.ForestArea[tmp.S2Months]  <- tmp.S2ForestArea
  tmp.RAOCUTArea <- rep(-1,12)
  tmp.RAOCUTArea[tmp.S1Months]  <- tmp.S1RAOCUTArea
  tmp.RAOCUTArea[tmp.S2Months]  <- tmp.S2RAOCUTArea
  ###
  ### Stream Access on Land
  tmp.PastureAreaWStreamAcess <- rep(-1,12)
  tmp.PastureAreaWStreamAcess[tmp.S1Months] <- tmp.S1PastureArea * tmp.S1PastureWStreamAcess
  tmp.PastureAreaWStreamAcess[tmp.S2Months] <- tmp.S2PastureArea * tmp.S2PastureWStreamAcess
  tmp.ForestAreaWStreamAcess <- rep(-1,12)
  tmp.ForestAreaWStreamAcess[tmp.S1Months]  <- tmp.S1ForestArea  * tmp.S1ForestWStreamAcess
  tmp.ForestAreaWStreamAcess[tmp.S2Months]  <- tmp.S2ForestArea  * tmp.S2ForestWStreamAcess
  tmp.RAOCUTAreaWStreamAcess <- rep(-1,12)
  tmp.RAOCUTAreaWStreamAcess[tmp.S1Months]  <- tmp.S1RAOCUTArea  * tmp.S1RAOCUTWStreamAcess
  tmp.RAOCUTAreaWStreamAcess[tmp.S2Months]  <- tmp.S2RAOCUTArea  * tmp.S2RAOCUTWStreamAcess
  ###
  ### Stream Access In Stream
  tmp.PastureAreaWStreamAcessInStream <- rep(-1,12)
  tmp.PastureAreaWStreamAcessInStream[tmp.S1Months] <- tmp.S1PastureArndStreams
  tmp.PastureAreaWStreamAcessInStream[tmp.S2Months]  <- tmp.S2PastureArndStreams
  tmp.ForestAreaWStreamAcessInStream <- rep(-1,12)
  tmp.ForestAreaWStreamAcessInStream[tmp.S1Months] <- tmp.S1ForestArndStreams
  tmp.ForestAreaWStreamAcessInStream[tmp.S2Months] <- tmp.S2ForestArndStreams
  tmp.RAOCUTAreaWStreamAcessInStream <- rep(-1,12)
  tmp.RAOCUTAreaWStreamAcessInStream[tmp.S1Months] <- tmp.S1RAOCUTArndStreams
  tmp.RAOCUTAreaWStreamAcessInStream[tmp.S2Months] <- tmp.S2RAOCUTArndStreams
  ###
  ### Without Stream Access
  tmp.PastureAreaWOStreamAcess <- tmp.PastureArea - tmp.PastureAreaWStreamAcess
  tmp.ForestAreaWOStreamAcess  <- tmp.ForestArea  - tmp.ForestAreaWStreamAcess
  tmp.RAOCUTAreaWOStreamAcess  <- tmp.RAOCUTArea  - tmp.RAOCUTAreaWStreamAcess
  ###
  ### Animal Densities  
  tmp.ADOnPasture <- rep(-1,12)
  tmp.ADOnPasture[tmp.S1Months] <- tmp.S1ADPasture
  tmp.ADOnPasture[tmp.S2Months] <- tmp.S2ADPasture
  tmp.ADOnForest <- rep(-1,12)
  tmp.ADOnForest[tmp.S1Months]  <- tmp.S1ADForest
  tmp.ADOnForest[tmp.S2Months]  <- tmp.S2ADForest
  tmp.ADOnRAOCUT <- rep(-1,12)
  tmp.ADOnRAOCUT[tmp.S1Months]  <- tmp.S1ADRAOCUT
  tmp.ADOnRAOCUT[tmp.S2Months]  <- tmp.S2ADRAOCUT
  ###
  ### Animal Populations
  ###
  ### Land without Stream Access
  tmp.ElkOnPastureWOStreamAcess <- tmp.ADOnPasture * tmp.PastureAreaWOStreamAcess
  tmp.ElkOnForestWOStreamAcess  <- tmp.ADOnForest  * tmp.ForestAreaWOStreamAcess
  tmp.ElkOnRAOCUTWOStreamAcess  <- tmp.ADOnRAOCUT  * tmp.RAOCUTAreaWOStreamAcess
  ###
  ### Instream from Land with Stream Access
  tmp.ElkOnPastureInStream <- tmp.ADOnPasture * tmp.PastureAreaWStreamAcess * tmp.PastureAreaWStreamAcessInStream
  tmp.ElkOnForestInStream  <- tmp.ADOnForest  * tmp.ForestAreaWStreamAcess  * tmp.ForestAreaWStreamAcessInStream
  tmp.ElkOnRAOCUTInStream  <- tmp.ADOnRAOCUT  * tmp.RAOCUTAreaWStreamAcess  * tmp.RAOCUTAreaWStreamAcessInStream
  ###
  ### On Land with Stream Access and Not in Stream
  tmp.ElkOnPastureWStreamAcess <- tmp.ADOnPasture * tmp.PastureAreaWStreamAcess - tmp.ElkOnPastureInStream
  tmp.ElkOnForestWStreamAcess  <- tmp.ADOnForest  * tmp.ForestAreaWStreamAcess  - tmp.ElkOnForestInStream
  tmp.ElkOnRAOCUTWStreamAcess  <- tmp.ADOnRAOCUT  * tmp.RAOCUTAreaWStreamAcess  - tmp.ElkOnRAOCUTInStream
  ###
  ### General
  tmp.ElkOnPasture <- tmp.ElkOnPastureWOStreamAcess + tmp.ElkOnPastureWStreamAcess
  tmp.ElkOnForest  <- tmp.ElkOnForestWOStreamAcess  + tmp.ElkOnForestWStreamAcess
  tmp.ElkOnRAOCUT  <- tmp.ElkOnRAOCUTWOStreamAcess  + tmp.ElkOnRAOCUTWStreamAcess
  tmp.ElkInStream  <- tmp.ElkOnPastureInStream      + tmp.ElkOnForestInStream  + tmp.ElkOnRAOCUTInStream
  tmp.ElkTotal     <- tmp.ElkOnPasture + tmp.ElkOnForest + tmp.ElkOnRAOCUT + tmp.ElkInStream
  ###
  ### Bacteria Production and Location
  ###
  ### Areas without Stream Access
  tmp.ElkBacteriaOnPastureWOStreamAcess <- tmp.bac.prod * tmp.ElkOnPastureWOStreamAcess
  tmp.ElkBacteriaOnForestWOStreamAcess  <- tmp.bac.prod * tmp.ElkOnForestWOStreamAcess
  tmp.ElkBacteriaOnRAOCUTWOStreamAcess  <- tmp.bac.prod * tmp.ElkOnRAOCUTWOStreamAcess
  ###
  ### Areas with Stream Access and Not in Stream
  tmp.ElkBacteriaOnPastureWStreamAcess <- tmp.bac.prod * tmp.ElkOnPastureWStreamAcess
  tmp.ElkBacteriaOnForestWStreamAcess  <- tmp.bac.prod * tmp.ElkOnForestWStreamAcess
  tmp.ElkBacteriaOnRAOCUTWStreamAcess  <- tmp.bac.prod * tmp.ElkOnRAOCUTWStreamAcess
  ###
  ### Land Areas
  tmp.ElkBacteriaOnPasture <- tmp.ElkBacteriaOnPastureWOStreamAcess + tmp.ElkBacteriaOnPastureWStreamAcess
  tmp.ElkBacteriaOnForest  <- tmp.ElkBacteriaOnForestWOStreamAcess  + tmp.ElkBacteriaOnForestWStreamAcess
  tmp.ElkBacteriaOnRAOCUT  <- tmp.ElkBacteriaOnRAOCUTWOStreamAcess  + tmp.ElkBacteriaOnRAOCUTWStreamAcess
  tmp.ElkBacteriaOnLand    <- tmp.ElkBacteriaOnPasture + tmp.ElkBacteriaOnForest + tmp.ElkBacteriaOnRAOCUT
  ###
  ### Instream
  tmp.ElkNacteriaOnPastureInStream <- tmp.bac.prod * tmp.ElkOnPastureInStream
  tmp.ElkBacteriaOnForestInStream  <- tmp.bac.prod * tmp.ElkOnForestInStream
  tmp.ElkBacteriaOnRAOCUTInStream  <- tmp.bac.prod * tmp.ElkOnRAOCUTInStream
  tmp.ElkBacteriaInStream <- tmp.ElkNacteriaOnPastureInStream + tmp.ElkBacteriaOnForestInStream + tmp.ElkBacteriaOnRAOCUTInStream
  ###
  ### Accume table values
  tmp.Accum.Pasture <- tmp.ElkBacteriaOnPasture / tmp.PastureArea
  tmp.Accum.Forest  <- tmp.ElkBacteriaOnForest  / tmp.ForestArea
  tmp.Accum.RAOCUT  <- tmp.ElkBacteriaOnRAOCUT  / tmp.RAOCUTArea
  ##
  ### Assemble output data frame
  SubModelOutput <- data.frame(Month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                               pop.total=tmp.ElkTotal,
                               pop.total.on.land=tmp.ElkTotal - tmp.ElkInStream,
                               pop.total.in.stream=tmp.ElkInStream,
                               pop.total.on.pasture= tmp.ElkOnPasture,
                               pop.total.in.forest= tmp.ElkOnForest,                               
                               pop.total.on.RAOCUT= tmp.ElkOnRAOCUT,
                               pop.on.pasture.wo.stream.access=tmp.ElkOnPastureWOStreamAcess,
                               pop.on.pasture.w.stream.access=tmp.ElkOnPastureWStreamAcess,
                               pop.from.pasture.in.stream=tmp.ElkOnPastureInStream,
                               pop.in.forest.wo.stream.access=tmp.ElkOnForestWOStreamAcess,
                               pop.in.forest.w.stream.access=tmp.ElkOnForestWStreamAcess,
                               pop.from.forest.in.stream=tmp.ElkOnForestInStream,
                               pop.on.raocut.wo.stream.access=tmp.ElkOnRAOCUTWOStreamAcess,
                               pop.on.raocut.w.stream.access=tmp.ElkOnRAOCUTWStreamAcess,
                               pop.from.raocut.in.stream=tmp.ElkOnRAOCUTInStream,
                               bac.total= tmp.ElkBacteriaOnLand + tmp.ElkBacteriaInStream,
                               bac.total.to.land=tmp.ElkBacteriaOnLand,
                               bac.total.in.stream=tmp.ElkBacteriaInStream/24,
                               bac.on.pasture.wo.stream.access=tmp.ElkBacteriaOnPastureWOStreamAcess,
                               bac.on.pasture.w.stream.access=tmp.ElkBacteriaOnPastureWStreamAcess,
                               bac.from.pasture.in.stream=tmp.ElkNacteriaOnPastureInStream,
                               bac.on.forest.wo.stream.access=tmp.ElkBacteriaOnForestWOStreamAcess,
                               bac.on.forest.w.stream.access=tmp.ElkBacteriaOnForestWStreamAcess,
                               bac.from.forest.in.stream=tmp.ElkBacteriaOnForestInStream,
                               bac.on.RAOCUT.wo.stream.access=tmp.ElkBacteriaOnRAOCUTWOStreamAcess,
                               bac.on.RAOCUT.w.stream.access=tmp.ElkBacteriaOnRAOCUTWStreamAcess,
                               bac.from.RAOCUT.in.stream=tmp.ElkBacteriaOnRAOCUTInStream,
                               bac.Pasture=tmp.ElkBacteriaOnPasture,
                               bac.Forest=tmp.ElkBacteriaOnForest,
                               bac.RAOCUT=tmp.ElkBacteriaOnRAOCUT,
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