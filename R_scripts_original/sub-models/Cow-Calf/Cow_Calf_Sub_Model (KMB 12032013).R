cow.calf <- function(chr.wrkdir="E:/PEST/BigElk/Sub_Models",
                     chr.input="cowcalf.txt") {
  
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
#Start template input file below
#   *** Cow-Calf Bacteria Source Model Input File
#   *** Program Version 3.1.12102012
#   Watershed: Big Elk Creek Sub-Wtsd 01.00
#   Date this Input File Created: YYYY-MM-DD
#   MUTSIN Start Year: YYYY
#   MUTSIN End Year: YYYY
#   HSPF-Sup File Header Number for Pasture in MON-ACCUM Table:  ##
#   HSPF-Sup File Header Number for Pasture in MON-SQOLIM Table: ##
#   HSPF-Sup File Header Number for Forest in MON-ACCUM Table:   ##
#   HSPF-Sup File Header Number for Forest in MON-SQOLIM Table:  ##
#   SQOLIM multiplication factor: #
#   Pasture Area in Watershed (ac):#.#
#   Forest Area in Watershed (ac): #.#
#   Average Stocking Density for Pasture in watershed (ac/Cow-Calf pair): #.#
#   *** Temporal Distribution of Cow-Calf Pair weight with Calf Growth:
#   *** Month: Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec
#   Adjusted AU: #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#
#   Cow Animal Unit Size (lbs): #.#
#   *** Temporal Distribution grazing Schedule:
#   *** Note: factors across pasture, confinement and forest
#   ***       should equal one for each month
#   *** Month: Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec
#   Pasture: #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#
#   Confinement: #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#
#   Forest: #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#
#   Manure Production of Cow-Calf Pair Animal Unit (AU) per day (Manure lbs/(AU day): #.#
#   Fecal Coliform in Manure (org/(day-AU)):   #.#E+##
#   Percent of pasture with stream access: ##
#   Percent of animals on pasture in and around streams: #
#   Percent of animals on forest in and around streams: #
#End template input file
  
  SubModelFile <- chr.input
#   SubModelData <- read.delim(SubModelFile, sep=":",comment.char="*",
#                    stringsAsFactors=FALSE, header=FALSE)
  SubModelData <- read.delim(paste0(chr.wrkdir,"/",SubModelFile)
, sep=":",comment.char="*",
                             stringsAsFactors=FALSE, header=FALSE)
  SubModelFilename <- strsplit(chr.input,".",fixed=TRUE)[[1]][[1]]
  
  names(SubModelData) <- c("parameter","value(s)")
  
  SubModelOutput <- data.frame(Month=c("Jan","Feb","Mar","Apr",
                                     "May","Jun","Jul","Aug",
                                     "Sep","Oct","Nov","Dec"),
                             stringsAsFactors=FALSE
                             )
  
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
  tmp.BacteriaPerAUperDay <- as.numeric(SubModelData[19,2])
  tmp.ManurePerDay <- as.numeric(SubModelData[18,2])

### Edit: KMB 2014-02-11 removed tmp.AUvsTime with for pairs distribution calculations
  tmp.OnPastureWOStreamAccess <- tmp.NumOfPairs*tmp.PasturevsTime*(1-tmp.PastureWStreamAcess/100)
  tmp.OnPastureWStreamAccess <- tmp.NumOfPairs*tmp.PasturevsTime*(tmp.PastureWStreamAcess/100)*(1-tmp.PastureInStream/100)
  tmp.OnPastureInStream <- tmp.NumOfPairs*tmp.PasturevsTime*(tmp.PastureWStreamAcess/100)*tmp.PastureInStream/100
  tmp.InConfinementvsTime <- tmp.NumOfPairs*tmp.ConfinementvsTime
  tmp.InForestWOStreamAccess <- tmp.NumOfPairs*(1-tmp.ForestWStreamAcess/100)*tmp.ForestvsTime
  tmp.InForestWStreamAccess <- tmp.NumOfPairs*(tmp.ForestWStreamAcess/100)*(1-tmp.ForestInStream/100)*tmp.ForestvsTime
  tmp.InForestInStream <- tmp.NumOfPairs*(tmp.ForestWStreamAcess/100)*(tmp.ForestInStream/100)*tmp.ForestvsTime

  
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
                             AU.InForest=(tmp.InForestWOStreamAccess+tmp.InForestWOStreamAccess)*tmp.AU,
                             AU.InForestInStream=tmp.InForestInStream*tmp.AU,
                             Manure.OnPastureWOStreamAccess=tmp.ManurePerDay*tmp.OnPastureWOStreamAccess,
                             Manure.OnPastureWStreamAccess=tmp.ManurePerDay*tmp.OnPastureWStreamAccess,
                             Manure.OnPastureInStream=tmp.ManurePerDay*tmp.OnPastureInStream,
                             Manure.InConfinementvsTime=tmp.ManurePerDay*tmp.InConfinementvsTime,
                             Manure.InForest=tmp.ManurePerDay*(tmp.InForestWOStreamAccess+tmp.InForestWOStreamAccess)*tmp.AU,
                             Manure.InForestInStream=tmp.ManurePerDay*tmp.InForestInStream,
                             Bacteria.OnPastureWOStreamAccess=tmp.BacteriaPerAUperDay*tmp.OnPastureWOStreamAccess,
                             Bacteria.OnPastureWStreamAccess=tmp.BacteriaPerAUperDay*tmp.OnPastureWStreamAccess,
                             Bacteria.OnPastureInStream=tmp.BacteriaPerAUperDay*tmp.OnPastureInStream/24,
                             Bacteria.InConfinementvsTime=tmp.BacteriaPerAUperDay*tmp.InConfinementvsTime,
                             Bacteria.InForest=tmp.BacteriaPerAUperDay*(tmp.InForestWOStreamAccess+tmp.InForestWOStreamAccess),
                             Bacteria.InForestInStream=tmp.BacteriaPerAUperDay*tmp.InForestInStream/24,
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
  SubModelOutput$AccumPasRow <- rep(tmp.ACCUM.Pasture.Row,12)
  SubModelOutput$AccumForRow <- rep(tmp.ACCUM.Forest.Row,12)
  SubModelOutput$LimPasRow <- rep(tmp.SQOLIM.Pasture.Row,12)
  SubModelOutput$LimForRow <- rep(tmp.SQOLIM.Forest.Row,12)
  return(SubModelOutput)

  #rm(list=ls(pattern="tmp.*"))
  #write.csv(SubModelOutput,file=paste0(SubModelFilename,sub(".","",format(Sys.time(), "%Y%m%d%H%M%OS4"),fixed=TRUE),".csv"))
}