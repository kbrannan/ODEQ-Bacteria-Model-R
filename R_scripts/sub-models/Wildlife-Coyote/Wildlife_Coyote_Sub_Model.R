wildlifeCoyote <- function(chr.input.file) {
  #   This function is the bacteria source-model Coyote. The model 
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
  
  
  df.input <- read.delim(chr.input.file, sep=":", 
                          comment.char="*", stringsAsFactors=FALSE, 
                          header=FALSE)
  names(df.input) <- c("parameter","value(s)")

##
## set values for variables

## land use information
  lu.pasture.area   <- as.numeric(df.input$value[
    df.input$parameter == "Pasture Area in Watershed (ac)"])
  lu.forest.area   <- as.numeric(df.input$value[
    df.input$parameter == "Forest Area in Watershed (ac)"])
  lu.RAOCUT.area   <- as.numeric(df.input$value[
    df.input$parameter == "Residential/Agricultural Operration Area/Commercial/Urban/Transportation (ac)"])
  ## coyote habitat set to all PLS area
  lu.habitatarea <- lu.pasture.area + lu.forest.area + lu.RAOCUT.area
## animal information
  ## population densities
  amn.density  <- as.numeric(df.input$value[
    df.input$parameter == "Animal Density (animal/ac)"])
  ## percent of time defecating in or around streams
  amn.percentstream <- as.numeric(df.input$value[
    df.input$parameter == "Percent of animals in and around streams"]) / 100
  ## all landuse has stream access for Coyotes
  ## bacteria production per animal
  amn.bac.prod  <- as.numeric(df.input$value[
    df.input$parameter == "Bacteria Production of animal per day (orgs/day)"])
  amn.SQLIM.factor  <- as.numeric(df.input$value[
    df.input$parameter == "SQOLIM multiplcation factor"])
  
  ##
  ### Calculations
  ### Populations
  pop.total   <- lu.habitatarea * amn.density
  pop.on.land     <- (1-amn.percentstream) * pop.total
  pop.in.stream <- amn.percentstream * pop.total
  ### bacteria loads
  bac.total <- pop.total * amn.bac.prod
  bac.on.land  <- amn.bac.prod * pop.on.land
  bac.in.stream   <- amn.bac.prod * pop.in.stream
  ### accum values
  accum.forest  <- (bac.on.land / lu.habitatarea) * (lu.forest.area / lu.habitatarea)
  accum.pasture  <- (bac.on.land / lu.habitatarea) * (lu.pasture.area / lu.habitatarea)
  accum.RAOCUT  <- (bac.on.land / lu.habitatarea) * (lu.RAOCUT.area / lu.habitatarea)
  ##
  ## Assemble output data frame
  df.output <- data.frame(
    Month=format(as.POSIXct(paste0("1967-",1:12,"-01")), format = "%b"),
    pop.total=pop.total,
    pop.on.land=pop.on.land,
    pop.in.stream=pop.in.stream,
    Bacteria.total=bac.total,
    Bacteria.on.land=bac.on.land,
    Bacteria.in.stream=bac.in.stream,
    Accum.pasture=accum.pasture,
    Accum.forest=accum.forest,
    Accum.RAOCUT=accum.RAOCUT,
    Lim.pasture=amn.SQLIM.factor * accum.pasture,
    Lim.forest=amn.SQLIM.factor * accum.forest,
    Lim.RAOCUT=amn.SQLIM.factor * accum.RAOCUT,
    stringsAsFactors=FALSE)

  return(df.output)
}