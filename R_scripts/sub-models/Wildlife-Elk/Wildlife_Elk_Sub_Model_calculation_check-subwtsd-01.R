## step bt step calculation check for wildlife_elk_sub_model using input from
## wildlifeelk01.txt file
chr.wildlife.elk.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-elk"
source(paste0(chr.wildlife.elk.dir,"/Wildlife_Elk_Sub_Model.R"))
df.output <- wildlifeElk(chr.wrkdir=chr.wildlife.elk.dir,chr.input="wildlifeelk01.txt")
##
## get input
## SQOLIM multiplcation factor: 
chk.sqolim <- 9
chk.Bacteria.Prod <- 6.6500000E+08
### two seasons
chk.Season.1.Months -> c(11,12,1,2,3)
chk.Season.2.Months -> c(4,5,6,7,8,9,10)
### Habitats
chk.Season.1.Pasture <- 365.29
chk.Season.1.Forest  <- 5700.35
chk.Season1.RAOUT    <- 1
chk.Season.2.Pasture <- 64.55
chk.Season.2.Fores   <- 2135.41
chk.Season.2.RAOUT   <- 1
### Percent of Landuse with Stream access
chk.Season.1.Percent.Pasture.with.Stream.Access <- 25
chk.Season.1.Percent.Forest.with.Stream.Access  <- 50
chk.Season.1.Percent.RAOUT.with.Stream.Access   <- 0
chk.Season.2.Percent.Pasture.with.Stream.Access <- 15
chk.Season.2.Percent.Forest.with.Stream.Access  <- 39
chk.Season.2.Percent.RAOUT.with.stream.Access   <- 0
### Animal Densities
chk.Season.1.Animal.Density.Pasture <- 3.0072900E-03
chk.Season.1.Animal.Density.Forest  <- 3.0072900E-03
chk.Season.1.Animal.Density.RAOUT   <- 0.0
chk.Season.2.Animal.Density.Pasture <- 4.5000000E-02
chk.Season.2.Animal.Density.Forest  <- 4.5000000E-02
chk.Season.2.Animal.Density.RAOUT   <- 0.0
### animals around streams
chk.Season.1.Percent.Pasture.in.and.around.streams <- 9.9500000E+00
chk.Season.1.Percent.Forest.in.and.around.streams  <- 1.9208850E+00
chk.Season.1.Percent.RAOUT.in.and.around.streams   <- 0
chk.Season.2.Percent.Pasture.in.and.around.streams <- 9.9500000E+00
chk.Season.2.Percent.Forest.in.and.around.streams  <- 9.9500000E+00
chk.Season.2.Percent.RAOUT.in.and.around.streams   <- 0
