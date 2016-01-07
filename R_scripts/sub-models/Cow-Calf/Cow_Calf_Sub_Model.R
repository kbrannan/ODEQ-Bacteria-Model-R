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

  ## set roundicant digits to d.pr and use sigdf
  d.pr <- 5
  
  # get machine floating point double precision
  l.p <- 10^-d.pr
  
  ## SubModelFile => chr.input.file

  ## set for testing code in side function
  chr.wrkdir <- getwd()
  chr.input.file <- "cowcalf01.txt"
  
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
  bac.pasture.w.strm <- bac.pasture.w * ainfo.pasture.in.strm
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
  bac.total   <- am.pairs.adj * ainfo.bac.prod
  bac.confine <- loc.confine * ainfo.bac.prod
  bac.pasture <- loc.pasture * ainfo.bac.prod
  bac.forest  <- loc.forest * ainfo.bac.prod
  # check
  abs(bac.total - (bac.pasture + bac.confine + bac.forest))

  ## bacteria loads with or without stream
  bac.pasture.w  <- lu.pasture.w * bac.pasture
  bac.pasture.wo <- (1 - lu.pasture.w) * bac.pasture
  bac.forest.w   <- lu.forest.w * bac.forest
  bac.forest.wo  <- (1 - lu.forest.w) * bac.forest
  # check
  abs(bac.pasture - (bac.pasture.w + bac.pasture.wo))
  abs(bac.forest - (bac.forest.w + bac.forest.wo))
  
  ## bacteria load in stream or not 
  bac.pasture.w.strm  <- bac.pasture.w * ainfo.pasture.in.strm
  bac.pasture.w.lnd   <- bac.pasture.w * (1 - ainfo.pasture.in.strm)
  bac.forest.w.strm   <- bac.forest.w * ainfo.forest.in.strm
  bac.forest.w.lnd    <- bac.forest.w * (1 - ainfo.forest.in.strm)
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
  

  
    
  tmp.PastureArea <- as.numeric(SubModelData[10,2])
  tmp.NumOfPairs <- tmp.PastureArea / as.numeric(SubModelData[12,2])
  tmp.PastureWStreamAcess <- as.numeric(SubModelData[20,2])
  tmp.PastureInStream <- as.numeric(SubModelData[22,2])
  tmp.ForestArea <- as.numeric(SubModelData[11,2])
  tmp.ForestWStreamAcess <- as.numeric(SubModelData[21,2])
  tmp.ForestInStream <- as.numeric(SubModelData[23,2])
  tmp.AU <- as.numeric(SubModelData[14,2])
  tmp.AUvsTime <- tmp.NumOfPairs*as.numeric(strsplit(SubModelData[13,2],",")[[1]])
  tmp.PasturevsTime <- as.numeric(strsplit(SubModelData[15,2],",")[[1]])
  tmp.ConfinementvsTime <- as.numeric(strsplit(SubModelData[16,2],",")[[1]])
  tmp.ForestvsTime <- as.numeric(strsplit(SubModelData[17,2],",")[[1]])
  tmp.BacteriaPerManurePerDay <- as.numeric(SubModelData[19,2])
  tmp.ManurePerDay <- as.numeric(SubModelData[18,2])

### Edit: KMB 2014-02-11 removed tmp.AUvsTime with for pairs distribution calculations
  tmp.OnPastureWOStreamAccess <- tmp.NumOfPairs*tmp.PasturevsTime*(1-tmp.PastureWStreamAcess/100)
  tmp.OnPastureWStreamAccess <- tmp.NumOfPairs*tmp.PasturevsTime*(tmp.PastureWStreamAcess/100)*(1-tmp.PastureInStream/100)
  tmp.OnPastureInStream <- tmp.NumOfPairs*tmp.PasturevsTime*(tmp.PastureWStreamAcess/100)*tmp.PastureInStream/100
  tmp.InConfinementvsTime <- tmp.NumOfPairs*tmp.ConfinementvsTime
  tmp.InForestWOStreamAccess <- tmp.NumOfPairs*(1-tmp.ForestWStreamAcess/100)*tmp.ForestvsTime
  tmp.InForestWStreamAccess <- tmp.NumOfPairs*(tmp.ForestWStreamAcess/100)*(1-tmp.ForestInStream/100)*tmp.ForestvsTime
  tmp.InForestInStream <- tmp.NumOfPairs*(tmp.ForestWStreamAcess/100)*(tmp.ForestInStream/100)*tmp.ForestvsTime

  
  ## SubModelOutput => df.output  
  df.output <- data.frame(Month=c("Jan","Feb","Mar","Apr",
                                  "May","Jun","Jul","Aug",
                                  "Sep","Oct","Nov","Dec"),
                          stringsAsFactors=FALSE
  )
  
  
  
  
  
### Edit: KMB 2013-12-03 removed the AU weight from calculation of manure and bacteria produced becuase the AU versus time are directly calculated in tmp.AUvsTime
  SubModelOutput <- data.frame(SubModelOutput,
                             AUvsTime=tmp.AUvsTime,
                             NumOfPairs=tmp.NumOfPairs*rep(1,12),
                             pairs.OnPastureWOStreamAccess=tmp.OnPastureWOStreamAccess, 
                             pairs.OnPastureWStreamAccess=tmp.OnPastureWStreamAccess,
                             pairs.OnPastureInStream=tmp.OnPastureInStream,
                             pairs.InConfinementvsTime=tmp.InConfinementvsTime,
                             pairs.InForestWOStreamAccess=tmp.InForestWOStreamAccess,
                             pairs.InForestWStreamAccess=tmp.InForestWStreamAccess,
                             pairs.InForestInStream=tmp.InForestInStream,
                             AU.OnPastureWOStreamAccess=tmp.OnPastureWOStreamAccess*tmp.AU,
                             AU.OnPastureWStreamAccess=tmp.OnPastureWStreamAccess*tmp.AU,
                             AU.OnPastureInStream=tmp.OnPastureInStream*tmp.AU,
                             AU.InConfinementvsTime=tmp.InConfinementvsTime*tmp.AU,
                             AU.InForest=(tmp.InForestWOStreamAccess+tmp.InForestWStreamAccess)*tmp.AU,
                             AU.InForestInStream=tmp.InForestInStream*tmp.AU,
                             Manure.OnPastureWOStreamAccess=tmp.ManurePerDay*tmp.OnPastureWOStreamAccess*tmp.AU,
                             Manure.OnPastureWStreamAccess=tmp.ManurePerDay*tmp.OnPastureWStreamAccess*tmp.AU,
                             Manure.OnPastureInStream=tmp.ManurePerDay*tmp.OnPastureInStream*tmp.AU,
                             Manure.InConfinementvsTime=tmp.ManurePerDay*tmp.InConfinementvsTime*tmp.AU,
                             Manure.InForest=tmp.ManurePerDay*(tmp.InForestWOStreamAccess+tmp.InForestWStreamAccess)*tmp.AU,
                             Manure.InForestInStream=tmp.ManurePerDay*tmp.InForestInStream*tmp.AU,
                             Bacteria.OnPastureWOStreamAccess=tmp.BacteriaPerManurePerDay*tmp.OnPastureWOStreamAccess*tmp.ManurePerDay*tmp.AU,
                             Bacteria.OnPastureWStreamAccess=tmp.BacteriaPerManurePerDay*tmp.OnPastureWStreamAccess*tmp.ManurePerDay*tmp.AU,
                             Bacteria.OnPastureInStream=tmp.BacteriaPerManurePerDay*tmp.OnPastureInStream/24*tmp.ManurePerDay*tmp.AU,
                             Bacteria.InConfinementvsTime=tmp.BacteriaPerManurePerDay*tmp.InConfinementvsTime*tmp.ManurePerDay*tmp.AU,
                             Bacteria.InForest=tmp.BacteriaPerManurePerDay*(tmp.InForestWOStreamAccess+tmp.InForestWStreamAccess)*tmp.ManurePerDay*tmp.AU,
                             Bacteria.InForestInStream=tmp.BacteriaPerManurePerDay*tmp.InForestInStream/24*tmp.ManurePerDay*tmp.AU,
                             stringsAsFactors=FALSE)
  
  tmp.Bacteria.InStream <- (SubModelOutput$Bacteria.OnPastureInStream+SubModelOutput$Bacteria.InForestInStream)
  tmp.Accum.Pasture <- (SubModelOutput$Bacteria.OnPastureWOStreamAccess+
                          SubModelOutput$Bacteria.OnPastureWStreamAccess)/tmp.PastureArea
  tmp.Accum.Forest <- SubModelOutput$Bacteria.InForest/tmp.ForestArea

  SubModelOutput <- data.frame(SubModelOutput,
                             Bacteria.Instream=tmp.Bacteria.InStream,
                             Accum.Pasture=tmp.Accum.Pasture,
                             Accum.Forest=tmp.Accum.Forest,
                             stringsAsFactors=FALSE)

  # NEW LINES
  tmp.SQOLIM.mult.fac <- as.numeric(SubModelData[9,2])
  tmp.ACCUM.Pasture.Row <- as.numeric(SubModelData[5,2])
  tmp.SQOLIM.Pasture.Row <- as.numeric(SubModelData[6,2])
  tmp.ACCUM.Forest.Row <- as.numeric(SubModelData[7,2])
  tmp.SQOLIM.Forest.Row <- as.numeric(SubModelData[8,2])

  SubModelOutput$Lim.Pasture <- tmp.Accum.Pasture * tmp.SQOLIM.mult.fac
  SubModelOutput$Lim.Forest  <- tmp.Accum.Forest  * tmp.SQOLIM.mult.fac
  
  return(SubModelOutput)

  #rm(list=ls(pattern="tmp.*"))
  #write.csv(SubModelOutput,file=paste0(SubModelFilename,sub(".","",format(Sys.time(), "%Y%m%d%H%M%OS4"),fixed=TRUE),".csv"))
}