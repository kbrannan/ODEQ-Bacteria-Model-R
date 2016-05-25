wildlifeDuck <- function(chr.input.file) {
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
  ## land use information
  lu.pasture.area   <- as.numeric(df.input$value[
    df.input$parameter == "Pasture Area in Watershed (ac)"])
  lu.forest.area   <- as.numeric(df.input$value[
    df.input$parameter == "Forest Area in Watershed (ac)"])
  lu.RAOCUT.area   <- as.numeric(df.input$value[
    df.input$parameter == "Residential/Agricultural Operration Area/Commercial/Urban/Transportation (ac)"])  ### Animal Densities
  lu.habitatarea <- lu.pasture.area + lu.forest.area + lu.RAOCUT.area
  ## animal information
  ## months for seasons
  amn.months.season.1 <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Months (numbers)"])
  amn.months.season.1 <- as.numeric(strsplit(df.input$value[
    df.input$parameter == "Season 1 Months (numbers)"], split = ",")[[1]])
  amn.months.season.2 <- as.numeric(strsplit(df.input$value[
    df.input$parameter == "Season 2 Months (numbers)"], split = ",")[[1]])
  ## Animal Densities
  amn.animal.density.season.1  <- as.numeric(df.input$value[
    df.input$parameter == "Season 1 Animal Density (animal/ac)"])
  amn.animal.density.season.2  <- as.numeric(df.input$value[
    df.input$parameter == "Season 2 Animal Density (animal/ac)"])
  ## Percent animals around streams
  amn.percentstream <- as.numeric(df.input$value[
    df.input$parameter == "Percent of animals on Pasture in and around streams"])/100
  ## bacteria production per animal
  amn.bac.prod  <- as.numeric(df.input$value[
    df.input$parameter == "Bacteria Production of animal per day (orgs/day)"])
  amn.SQLIM.factor  <- as.numeric(df.input$value[
    df.input$parameter == "SQOLIM multiplcation factor"])  ##

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
  pop.total.on.land.season.1 <-   amn.percentstream * pop.total.season.1
  pop.pasture.on.land.season.1 <- amn.percentstream * pop.pasture.season.1
  pop.forest.on.land.season.1 <-  amn.percentstream * pop.forest.season.1
  pop.RAOCUT.on.land.season.1 <-  amn.percentstream * pop.RAOCUT.season.1
  pop.total.on.land.season.2 <-   amn.percentstream * pop.total.season.2
  pop.pasture.on.land.season.2 <- amn.percentstream * pop.pasture.season.2
  pop.forest.on.land.season.2 <-  amn.percentstream * pop.forest.season.2
  pop.RAOCUT.on.land.season.2 <-  amn.percentstream * pop.RAOCUT.season.2
  ## in stream
  pop.total.in.stream.season.1 <-   (1 - amn.percentstream) * pop.total.season.1
  pop.pasture.in.stream.season.1 <- (1 - amn.percentstream) * pop.pasture.season.1
  pop.forest.in.stream.season.1 <-  (1 - amn.percentstream) * pop.forest.season.1
  pop.RAOCUT.in.stream.season.1 <-  (1 - amn.percentstream) * pop.RAOCUT.season.1
  pop.total.in.stream.season.2 <-   (1 - amn.percentstream) * pop.total.season.2
  pop.pasture.in.stream.season.2 <- (1 - amn.percentstream) * pop.pasture.season.2
  pop.forest.in.stream.season.2 <-  (1 - amn.percentstream) * pop.forest.season.2
  pop.RAOCUT.in.stream.season.2 <-  (1 - amn.percentstream) * pop.RAOCUT.season.2
  ### Animal Populations
  tmp.PopOnPasture <- rep(-1,12)
  tmp.PopOnPasture[tmp.S1Months] <- (1 - tmp.ArndStreams) * tmp.PastureArea * tmp.S1AD
  tmp.PopOnPasture[tmp.S2Months]  <- (1 - tmp.ArndStreams) * tmp.PastureArea * tmp.S2AD
  tmp.PopOnForest <- rep(-1,12)
  tmp.PopOnForest[tmp.S1Months]  <- (1 - tmp.ArndStreams) * tmp.ForestArea * tmp.S1AD
  tmp.PopOnForest[tmp.S2Months]  <- (1 - tmp.ArndStreams) * tmp.ForestArea * tmp.S2AD
  tmp.PopOnRAOCUT <- rep(-1,12)
  tmp.PopOnRAOCUT[tmp.S1Months]  <- (1 - tmp.ArndStreams) * tmp.RAOCUTArea * tmp.S1AD
  tmp.PopOnRAOCUT[tmp.S2Months]  <- (1 - tmp.ArndStreams) * tmp.RAOCUTArea * tmp.S2AD
  tmp.PopInStream <- rep(-1,12)
  tmp.PopInStream[tmp.S1Months]  <- tmp.ArndStreams * (tmp.PastureArea + tmp.ForestArea + tmp.RAOCUTArea) * tmp.S1AD
  tmp.PopInStream[tmp.S2Months]  <- tmp.ArndStreams * (tmp.PastureArea + tmp.ForestArea + tmp.RAOCUTArea) * tmp.S2AD
  ###
  ### Bacteria Production and Location
  ###
  ### Instream
  tmp.BacteriaInStream <- tmp.bac.prod * tmp.PopInStream
  ###
  ### On Land
  tmp.Bacteria.Pasture <- tmp.bac.prod * tmp.PopOnPasture
  tmp.Bacteria.Forest  <- tmp.bac.prod * tmp.PopOnForest
  tmp.Bacteria.RAOCUT  <- tmp.bac.prod * tmp.PopOnRAOCUT
  ###
  ### Bacteria Total
  tmp.Bacteria.Total <- tmp.BacteriaInStream + tmp.Bacteria.Pasture + 
    tmp.Bacteria.Forest + tmp.Bacteria.RAOCUT
  ###
  ### Accume table values
  tmp.Accum.Pasture <- tmp.Bacteria.Pasture / tmp.PastureArea
  tmp.Accum.Forest  <- tmp.Bacteria.Forest  / tmp.ForestArea
  tmp.Accum.RAOCUT  <- tmp.Bacteria.RAOCUT  / tmp.RAOCUTArea
  ##
  ### Assemble output data frame
  SubModelOutput <- data.frame(Month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                               pop.total=tmp.PopOnPasture + tmp.PopOnForest + tmp.PopOnRAOCUT + tmp.PopInStream,
                               pop.total.on.land=tmp.PopOnPasture + tmp.PopOnForest + tmp.PopOnRAOCUT,
                               pop.total.in.stream=tmp.PopInStream,
                               pop.total.on.pasture=tmp.PopOnPasture,
                               pop.total.in.forest=tmp.PopOnForest,
                               pop.total.on.RAOCUT=tmp.PopOnRAOCUT,
                               bac.total.in.stream=tmp.BacteriaInStream,
                               bac.on.pasture=tmp.Bacteria.Pasture,
                               bac.in.forest=tmp.Bacteria.Forest,
                               bac.on.RAOCUT=tmp.Bacteria.RAOCUT,
                               bac.total=tmp.Bacteria.Total,
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