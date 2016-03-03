onsite_pets <- function(chr.input="OnSitePetsxx.txt",chr.wrkdir=getwd()) {

  ## read input file
  SubModelFile <- paste0(chr.wrkdir,"/",chr.input)
  SubModelData <- read.delim(SubModelFile, sep=":",comment.char="*",stringsAsFactors=FALSE, header=FALSE)
  names(SubModelData) <- c("parameter","value(s)")
  ##
  ### Getting input parameter values
  ### HSPF related information
  tmp.MUTSINStartYr <- as.numeric(SubModelData[1,2])
  tmp.MUTSINEndYr   <- as.numeric(SubModelData[2,2])
  tmp.HdrACCUMRAOCUT  <- as.numeric(SubModelData[3,2])
  tmp.HdrSQLIMRAOCUT  <- as.numeric(SubModelData[4,2])
  tmp.SQLIMFactor  <- as.numeric(SubModelData[5,2])
  ### Bacteria production rates
  tmp.onsite.bac.prod  <- as.numeric(SubModelData[6,2])
  tmp.pets.bac.prod  <- as.numeric(SubModelData[7,2])
  ### Pet Information
  tmp.pets.NumOfHH  <- as.numeric(SubModelData[8,2])
  tmp.pets.PetsPerHH  <- as.numeric(SubModelData[9,2])
  tmp.RAOCUTArea  <- as.numeric(SubModelData[10,2])
  ### On-site Information
  tmp.onsite.NumNearStrmStrct  <- as.numeric(SubModelData[11,2])
  tmp.onsite.StrctPre1974      <- as.numeric(SubModelData[12,2])/100
  tmp.onsite.Strct1974to1986   <- as.numeric(SubModelData[13,2])/100
  tmp.onsite.StrctPost1986     <- as.numeric(SubModelData[14,2])/100
  tmp.onsite.FailRatePre1974      <- as.numeric(SubModelData[15,2])/100
  tmp.onsite.FailRate1974to1986   <- as.numeric(SubModelData[16,2])/100
  tmp.onsite.FailRatePost1986     <- as.numeric(SubModelData[17,2])/100
  tmp.percent.in.stream     <- as.numeric(SubModelData[18,2])/100
  ##
  ### Calculations
  ### Pets
  tmp.pets.pop <- tmp.pets.NumOfHH * tmp.pets.PetsPerHH
  tmp.pets.bacteria.load <- tmp.pets.pop * tmp.pets.bac.prod
  tmp.Accum.RAOCUT <- tmp.pets.bacteria.load / tmp.RAOCUTArea
  ### On-stie
  tmp.onsite.NearStrmStrctPre1974    <- tmp.onsite.NumNearStrmStrct * tmp.onsite.StrctPre1974
  tmp.onsite.NearStrmStrct1974to1986 <- tmp.onsite.NumNearStrmStrct * tmp.onsite.Strct1974to1986
  tmp.onsite.NearStrmStrctPost1986   <- tmp.onsite.NumNearStrmStrct * tmp.onsite.StrctPost1986
  tmp.onsite.NearStrmStrct <- tmp.onsite.NumNearStrmStrct
  tmp.onsite.NearStrmStrctFailurePre1974    <- tmp.onsite.NearStrmStrctPre1974 * tmp.onsite.FailRatePre1974
  tmp.onsite.NearStrmStrctFailure1974to1986 <- tmp.onsite.NearStrmStrct1974to1986 * tmp.onsite.FailRate1974to1986
  tmp.onsite.NearStrmStrctFailurePost1986   <- tmp.onsite.NearStrmStrctPost1986 * tmp.onsite.FailRatePost1986
  tmp.onsite.NearStrmStrctFailure <- tmp.onsite.NearStrmStrctFailurePre1974 + tmp.onsite.NearStrmStrctFailure1974to1986 + tmp.onsite.NearStrmStrctFailurePost1986
  ## adjust for structures near stream that may not have toilet facilities
  tmp.onsite.NearStrmStrctFailureInStream <- tmp.percent.in.stream * tmp.onsite.NearStrmStrctFailure
  tmp.onsite.NearStrmStrctFailurePre1974.load    <- tmp.onsite.NearStrmStrctFailurePre1974 * tmp.onsite.bac.prod 
  tmp.onsite.NearStrmStrctFailure1974to1986.load <- tmp.onsite.NearStrmStrctFailure1974to1986 * tmp.onsite.bac.prod 
  tmp.onsite.NearStrmStrctFailurePost1986.load   <- tmp.onsite.NearStrmStrctFailurePost1986 * tmp.onsite.bac.prod
  tmp.onsite.NearStrmStrctFailure.load <- tmp.onsite.NearStrmStrctFailurePre1974.load + tmp.onsite.NearStrmStrctFailure1974to1986.load + tmp.onsite.NearStrmStrctFailurePost1986.load
  ## adjust for structures near stream that may not have toilet facilities
  tmp.onsite.NearStrmStrctFailure.to.stream.load <- tmp.percent.in.stream * tmp.onsite.NearStrmStrctFailure.load / 24
  tmp.Accum.RAOCUT <- tmp.Accum.RAOCUT + (1- tmp.percent.in.stream) * tmp.onsite.NearStrmStrctFailure.load / tmp.RAOCUTArea
  ##
  ### Assemble output data frame
  SubModelOutput <- data.frame(Month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                               pop.pet.total=tmp.pets.pop,
					              		   num.onsite.NearStrmStrctPre1974=tmp.onsite.NearStrmStrctPre1974,
					              		   num.onsite.NearStrmStrct1974to1986=tmp.onsite.NearStrmStrct1974to1986,
				              			   num.onsite.NearStrmStrctPost1986=tmp.onsite.NearStrmStrctPost1986,
					              		   num.onsite.NearStrmStrct=tmp.onsite.NearStrmStrct,
							                 num.onsite.NearStrmStrctFailurePre1974=tmp.onsite.NearStrmStrctFailurePre1974,
							                 num.onsite.NearStrmStrctFailure1974to1986=tmp.onsite.NearStrmStrctFailure1974to1986,
						              	   num.onsite.NearStrmStrctFailurePost1986=tmp.onsite.NearStrmStrctFailurePost1986, 
							                 num.onsite.NearStrmStrctFailure=tmp.onsite.NearStrmStrctFailure,
					              		   num.onsite.NearStrmStrctFailureInStream=tmp.onsite.NearStrmStrctFailureInStream,
							                 bac.pets.load=tmp.pets.bacteria.load,
							                 bac.onsite.NearStrmStrctFailurePre1974=tmp.onsite.NearStrmStrctFailurePre1974.load,
							                 bac.onsite.NearStrmStrctFailure1974to1986=tmp.onsite.NearStrmStrctFailure1974to1986.load,
							                 bac.onsite.NearStrmStrctFailurePost1986=tmp.onsite.NearStrmStrctFailurePost1986.load,
							                 bac.onsite.NearStrmStrctFailure=tmp.onsite.NearStrmStrctFailure.load,
					              		   bac.onsite.NearStrmStrctFailure.to.stream.load=tmp.onsite.NearStrmStrctFailure.to.stream.load,
                               Accum.RAOCUT=tmp.Accum.RAOCUT,
                               SQLIM.factor=tmp.SQLIMFactor,
                               MUTSIN.Start.Year=tmp.MUTSINStartYr,
                               MUTSIN.End.Year=tmp.MUTSINEndYr,
                               AccumRAOCUTRow=tmp.HdrACCUMRAOCUT,
                               LimRAOCUTRow=tmp.HdrSQLIMRAOCUT,
                               stringsAsFactors=FALSE)
  ##
  ### return results
  return(SubModelOutput)
}