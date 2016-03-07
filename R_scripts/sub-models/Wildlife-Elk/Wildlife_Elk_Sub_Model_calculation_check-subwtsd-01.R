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
### Animal Densities
chk.Season.1.Animal.Density.Pasture <- 3.0072900E-03
chk.Season.1.Animal.Density.Forest  <- 3.0072900E-03
chk.Season.1.Animal.Density.RAOUT   <- 0.0
chk.Season.2.Animal.Density.Pasture <- 4.5000000E-02
chk.Season.2.Animal.Density.Forest  <- 4.5000000E-02
chk.Season.2.Animal.Density.RAOUT   <- 0.0
### Habitats
chk.Season.1.Pasture <- 365.29
chk.Season.1.Forest  <- 5700.35
chk.Season.1.RAOUT    <- 1
chk.Season.2.Pasture <- 64.55
chk.Season.2.Forest   <- 2135.41
chk.Season.2.RAOUT   <- 1
### Percent of Landuse with Stream access
chk.Season.1.Percent.Pasture.with.Stream.Access <- 25
chk.Season.1.Percent.Forest.with.Stream.Access  <- 50
chk.Season.1.Percent.RAOUT.with.Stream.Access   <- 0
chk.Season.2.Percent.Pasture.with.Stream.Access <- 15
chk.Season.2.Percent.Forest.with.Stream.Access  <- 39
chk.Season.2.Percent.RAOUT.with.stream.Access   <- 0
### animals around streams
chk.Season.1.Percent.Pasture.in.and.around.streams <- 9.9500000E+00
chk.Season.1.Percent.Forest.in.and.around.streams  <- 1.9208850E+00
chk.Season.1.Percent.RAOUT.in.and.around.streams   <- 0
chk.Season.2.Percent.Pasture.in.and.around.streams <- 9.9500000E+00
chk.Season.2.Percent.Forest.in.and.around.streams  <- 9.9500000E+00
chk.Season.2.Percent.RAOUT.in.and.around.streams   <- 0
## calculations
## animal numbers
## season 1
chk.season.1.Pasture.elk <- chk.Season.1.Animal.Density.Pasture * 
  chk.Season.1.Pasture
chk.season.1.Forest.elk <- chk.Season.1.Animal.Density.Forest * 
  chk.Season.1.Forest
chk.season.1.RAOUT.elk <- chk.Season.1.Animal.Density.RAOUT * 
  chk.Season.1.RAOUT
chk.season.1.total.elk <- chk.season.1.Pasture.elk + chk.season.1.Forest.elk +
  chk.season.1.RAOUT.elk
## season 2
chk.season.2.Pasture.elk <- chk.Season.2.Animal.Density.Pasture * 
  chk.Season.2.Pasture
chk.season.2.Forest.elk <- chk.Season.2.Animal.Density.Forest * 
  chk.Season.2.Forest
chk.season.2.RAOUT.elk <- chk.Season.2.Animal.Density.RAOUT * 
  chk.Season.2.RAOUT
chk.season.2.total.elk <- chk.season.2.Pasture.elk + chk.season.2.Forest.elk +
  chk.season.2.RAOUT.elk
## with.without stream access
## without stream access
## Season 1
chk.season.1.Pasture.elk.wo.str.acc <- 
  (1 - chk.Season.1.Percent.Pasture.with.Stream.Access / 100) * 
  chk.season.1.Pasture.elk
chk.season.1.Forest.elk.wo.str.acc <- 
  (1 - chk.Season.1.Percent.Forest.with.Stream.Access / 100) * 
  chk.season.1.Forest.elk
chk.season.1.RAOUT.elk.wo.str.acc <- 
  (1 - chk.Season.1.Percent.RAOUT.with.Stream.Access / 100) * 
  chk.season.1.RAOUT.elk
## Season 2
chk.season.2.Pasture.elk.wo.str.acc <- 
  (1 - chk.Season.2.Percent.Pasture.with.Stream.Access / 100) * 
  chk.season.2.Pasture.elk
chk.season.2.Forest.elk.wo.str.acc <- 
  (1 - chk.Season.2.Percent.Forest.with.Stream.Access / 100) * 
  chk.season.2.Forest.elk
chk.season.2.RAOUT.elk.wo.str.acc <- 
  (1 - chk.Season.2.Percent.RAOUT.with.stream.Access / 100) * 
  chk.season.2.RAOUT.elk
## with stream access
## Season 1
chk.season.1.Pasture.elk.w.str.acc <- 
  (chk.Season.1.Percent.Pasture.with.Stream.Access / 100) * 
  chk.season.1.Pasture.elk
chk.season.1.Forest.elk.w.str.acc <- 
  (chk.Season.1.Percent.Forest.with.Stream.Access / 100) * 
  chk.season.1.Forest.elk
chk.season.1.RAOUT.elk.w.str.acc <- 
  (chk.Season.1.Percent.RAOUT.with.Stream.Access / 100) * 
  chk.season.1.RAOUT.elk
## Season 2
chk.season.2.Pasture.elk.w.str.acc <- 
  (chk.Season.2.Percent.Pasture.with.Stream.Access / 100) * 
  chk.season.2.Pasture.elk
chk.season.2.Forest.elk.w.str.acc <- 
  (chk.Season.2.Percent.Forest.with.Stream.Access / 100) * 
  chk.season.2.Forest.elk
chk.season.2.RAOUT.elk.w.str.acc <- 
  (chk.Season.2.Percent.RAOUT.with.stream.Access / 100) * 
  chk.season.2.RAOUT.elk
## with stream access on land
## Season 1
chk.season.1.Pasture.elk.w.str.acc.l <- 
  (1 - chk.Season.1.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.1.Pasture.elk.w.str.acc
chk.season.1.Forest.elk.w.str.acc.l <- 
  (1 - chk.Season.1.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.1.Forest.elk.w.str.acc
chk.season.1.RAOUT.elk.w.str.acc.l <- 
  (1 - chk.Season.1.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.1.RAOUT.elk.w.str.acc
## Season 2
chk.season.2.Pasture.elk.w.str.acc.l <- 
  (1 - chk.Season.2.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.2.Pasture.elk.w.str.acc
chk.season.2.Forest.elk.w.str.acc.l <- 
  (1 - chk.Season.2.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.2.Forest.elk.w.str.acc
chk.season.2.RAOUT.elk.w.str.acc.l <- 
  (1 - chk.Season.2.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.2.RAOUT.elk.w.str.acc
## with stream access in/around stream
## Season 1
chk.season.1.Pasture.elk.w.str.acc.s <- 
  (chk.Season.1.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.1.Pasture.elk.w.str.acc
chk.season.1.Forest.elk.w.str.acc.s <- 
  (chk.Season.1.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.1.Forest.elk.w.str.acc
chk.season.1.RAOUT.elk.w.str.acc.s <- 
  (chk.Season.1.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.1.RAOUT.elk.w.str.acc
## Season 2
chk.season.2.Pasture.elk.w.str.acc.s <- 
  (chk.Season.2.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.2.Pasture.elk.w.str.acc
chk.season.2.Forest.elk.w.str.acc.s <- 
  (chk.Season.2.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.2.Forest.elk.w.str.acc
chk.season.2.RAOUT.elk.w.str.acc.s <- 
  (chk.Season.2.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.2.RAOUT.elk.w.str.acc