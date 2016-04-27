cow.calf <- function(chr.wrkdir="E:/PEST/BigElk/Sub_Models",
                     chr.input.file="cowcalf.txt") {
  
#   This function is the bacteria source-model cow-calf systems 
#   and generates input for input to HSPF. The specific outputs from 
#   this source model are loads from the cow-calf system to the land 
#   and directly to the stream. The load to the land is in the form 
#   of load/acre for each PLS in a sub-watershed that the source-model 
#   contributes to and the hourly load to the stream in the form of 
#   a MUTSIN file. The input for the model is from an ASCII text file.
#   Use the text below as a template for the input file. The symbol 
#   used for comments in the input file is "***". The definitions for 
#   the symbols used in the template are: YYYY is four-digit year, 
#   MM two-digit month, DD is the two-digit day, ## is an integer, 
#   #.# is a floating pint number, and #.#E+## is a number in 
#   scientific notation


  ## set for testing code in side function
##  chr.wrkdir <- getwd()
##  chr.input.file <- "cowcalf01.txt"
  
  ## read input file
  ## SubModelData => df.input 
  df.input <- read.delim(paste0(chr.wrkdir, "/", chr.input.file), 
                         sep=":", comment.char="*", stringsAsFactors=FALSE, 
                         header=FALSE)
  names(df.input) <- c("parameter","value")
  
  
  ## not sure what the output is for  
##  SubModelFilename <- strsplit(chr.input,".",fixed=TRUE)[[1]][[1]]
  
  
  ## set values for variables

  ## HSPF information
  hspf.AccumPasRow <- rep(df.input$value[5],12)
  hspf.LimPasRow   <- rep(df.input$value[6],12)
  hspf.AccumForRow <- rep(df.input$value[7],12)
  hspf.LimForRow   <- rep(df.input$value[8],12)
  
  ## land use information
  lu.pasture.area <- as.numeric(df.input$value[9])
  lu.forest.area  <- as.numeric(df.input$value[10])
  lu.pasture.w    <- as.numeric(df.input$value[11]) / 100
  lu.forest.w     <- as.numeric(df.input$value[12]) / 100
  
  ## animal management information
  amng.sd         <- as.numeric(df.input$value[13])
  amng.adj.size   <- as.numeric(strsplit(df.input$value[14],",")[[1]])
  amng.in.pasture <- as.numeric(strsplit(df.input$value[15],",")[[1]])
  amng.in.confine <- as.numeric(strsplit(df.input$value[16],",")[[1]])
  amng.in.forest  <- as.numeric(strsplit(df.input$value[17],",")[[1]])
  
  ## animal information
  ainfo.bac.prod        <- as.numeric(df.input$value[18])
  ainfo.sqolim.fac      <- as.numeric(df.input$value[19])
  ainfo.pasture.in.strm <- as.numeric(df.input$value[20]) / 100
  ainfo.forest.in.strm  <- as.numeric(df.input$value[21]) / 100
  

  
  ## calculations
  
  ## pairs
  am.pairs     <- lu.pasture.area / amng.sd
  am.pairs.adj <- am.pairs * amng.adj.size
  
  ## pair location
  loc.pasture <- amng.in.pasture * am.pairs.adj
  loc.confine <- amng.in.confine * am.pairs.adj
  loc.forest  <- amng.in.forest * am.pairs.adj
  # check
  abs(am.pairs.adj - (loc.pasture + loc.confine + loc.forest))
  
  ## pair location with or without stream
  loc.pasture.w  <- lu.pasture.w * loc.pasture
  loc.pasture.wo <- (1 - lu.pasture.w) * loc.pasture
  loc.forest.w   <- lu.forest.w * loc.forest
  loc.forest.wo  <- (1 - lu.forest.w) * loc.forest
  # check
  abs(loc.pasture - (loc.pasture.w + loc.pasture.wo))
  abs(loc.forest - (loc.forest.w + loc.forest.wo))
  
  ## pair location in stream or not 
  loc.pasture.w.strm <- loc.pasture.w * ainfo.pasture.in.strm
  loc.pasture.w.lnd  <- loc.pasture.w * (1 - ainfo.pasture.in.strm)
  loc.forest.w.strm   <- loc.forest.w * ainfo.forest.in.strm
  loc.forest.w.lnd    <- loc.forest.w * (1 - ainfo.forest.in.strm)
  # check
  abs(loc.pasture.w - (loc.pasture.w.strm + loc.pasture.w.lnd))
  abs(loc.forest.w - (loc.forest.w.strm + loc.forest.w.lnd))

  ## checks on pair calculations
  abs(loc.pasture - (loc.pasture.wo + loc.pasture.w.strm + loc.pasture.w.lnd))
  abs(loc.forest - (loc.forest.wo + loc.forest.w.strm + loc.forest.w.lnd))
  abs(am.pairs.adj - (loc.confine + 
                        (loc.pasture.wo + loc.pasture.w.strm + loc.pasture.w.lnd) +
                        (loc.forest.wo + loc.forest.w.strm + loc.forest.w.lnd)
                      )
      )

  ##
  ## bacteria load calculations
  bac.total.adj   <- am.pairs.adj * ainfo.bac.prod
  bac.total <- sum(bac.total.adj)
  bac.confine <- loc.confine * ainfo.bac.prod
  bac.pasture <- loc.pasture * ainfo.bac.prod
  bac.forest  <- loc.forest * ainfo.bac.prod
  # check
  abs(bac.total.adj - (bac.pasture + bac.confine + bac.forest))

  ## bacteria loads with or without stream
#   bac.pasture.w  <- lu.pasture.w * bac.pasture
#   bac.pasture.wo <- (1 - lu.pasture.w) * bac.pasture
#   bac.forest.w   <- lu.forest.w * bac.forest
#   bac.forest.wo  <- (1 - lu.forest.w) * bac.forest
#   # check
#   abs(bac.pasture - (bac.pasture.w + bac.pasture.wo))
#   abs(bac.forest - (bac.forest.w + bac.forest.wo))
  bac.pasture.w.lnd  <- loc.pasture.w.lnd * ainfo.bac.prod
  bac.pasture.wo <- loc.pasture.wo * ainfo.bac.prod
  bac.pasture.w.strm  <- loc.pasture.w.strm * ainfo.bac.prod
  bac.forest.w.lnd   <- loc.forest.w.lnd * ainfo.bac.prod
  bac.forest.wo  <- loc.forest.wo * ainfo.bac.prod
  bac.forest.w.strm  <- loc.forest.w.strm * ainfo.bac.prod
  
  # bac.pasture.w  <- loc.pasture.w.lnd * ainfo.bac.prod
  # bac.pasture.wo <- loc.pasture.wo * ainfo.bac.prod
  # bac.forest.w   <- loc.forest.w.lnd * ainfo.bac.prod
  # bac.forest.wo  <- loc.forest.wo * ainfo.bac.prod
  # check
  abs(bac.pasture - (bac.pasture.w.lnd + bac.pasture.wo + bac.pasture.w.strm))
  abs(bac.forest - (bac.forest.w.lnd + bac.forest.wo + bac.forest.w.strm))
  
  
  ## bacteria load in stream or not 
  # bac.pasture.w.strm  <- bac.pasture.w * ainfo.pasture.in.strm
  # bac.pasture.w.lnd   <- bac.pasture.w * (1 - ainfo.pasture.in.strm)
  # bac.forest.w.strm   <- bac.forest.w * ainfo.forest.in.strm
  # bac.forest.w.lnd    <- bac.forest.w * (1 - ainfo.forest.in.strm)
  # checks
  abs(bac.pasture.w - (bac.pasture.w.strm + bac.pasture.w.lnd))
  abs(bac.forest.w - (bac.forest.w.strm + bac.forest.w.lnd))
  
  ## bacteria loads to end points (besides confinement)
  bac.pasture.lnd <- bac.pasture.wo + bac.pasture.w.lnd
  bac.forest.lnd  <- bac.forest.wo + bac.forest.w.lnd
  bac.strm <- bac.pasture.w.strm + bac.forest.w.strm

  # checks on bacteria loads 
  abs(bac.pasture - (bac.pasture.w.strm + bac.pasture.lnd))
  abs(bac.forest - (bac.forest.w.strm + bac.forest.lnd))
  abs(bac.total - (bac.pasture.lnd + bac.confine + bac.forest.lnd + bac.strm))
  

  ## SubModelOutput => df.output  
  df.output <- data.frame(
    Month = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
            "Oct","Nov","Dec"),
    NumOfPairs = am.pairs * rep(1,12),
    AUvsTime = am.pairs.adj,
    pairs.OnPastureWOStreamAccess = loc.pasture.wo,
    pairs.OnPastureWStreamAccess = loc.pasture.w.lnd,
    pairs.OnPastureInStream = loc.pasture.w.strm,
    pairs.InConfinementvsTime = loc.confine,
    pairs.InForestWOStreamAccess = loc.forest.wo,
    pairs.InForestWStreamAccess = loc.forest.w.lnd,
    pairs.InForestInStream = loc.forest.w.strm,

    Bacteria.OnPastureWOStreamAccess = bac.pasture.wo,
    Bacteria.OnPastureWStreamAccess = bac.pasture.w,
    Bacteria.OnPastureInStream = bac.pasture.w.strm,
    Bacteria.InConfinementvsTime = bac.confine,
    Bacteria.InForest = bac.forest.lnd,
    Bacteria.InForestInStream = bac.forest.w.strm,

    Bacteria.Instream = bac.strm,
    Accum.Pasture = bac.pasture.lnd / lu.pasture.area,
    Accum.Forest = bac.forest.lnd / lu.forest.area,
    
    Lim.Pasture = ainfo.sqolim.fac * bac.pasture.lnd / lu.pasture.area,
    Lim.Forest = ainfo.sqolim.fac * bac.pasture.lnd / lu.forest.area,
    stringsAsFactors = FALSE)
    

  
  return(df.output)

}