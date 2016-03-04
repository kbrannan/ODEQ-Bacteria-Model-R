## step bt step calculation check for wildlife_elk_sub_model using input from
## wildlifeelk01.txt file
chr.wildlife.elk.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-elk"
source(paste0(chr.wildlife.elk.dir,"/Wildlife_Elk_Sub_Model.R"))
df.output <- wildlifeElk(chr.wrkdir=chr.wildlife.elk.dir,chr.input="wildlifeelk01.txt")
##
## get input
## SQOLIM multiplcation factor: 
chk.sqolim <- 9
### two seasons
chk.Season.1.Months -> c(11,12,1,2,3)
chk.Season.2.Months -> c(4,5,6,7,8,9,10)
### Habitats
chk.Season.1.Pasture <- 365.29
chk.Season.1.Forest <- 5700.35
chk.Season1.RAOUT <- 1
chk.Season.2.Pasture <- 64.55
chk.Season.2.Fores <- 2135.41
chk.Season.2.RAOUT <- 1
### Percent of Landuse with Stream access
chk.Season.1.Percent.Pasture.with.Stream.Access <- 25
chk.Season.1.Percent.Forest.with.Stream.Access <- 50
chk.Season.1.Percent.RAOUT.with.Stream.Access <- 0
chk.Season.2.Percent.Pasture.with.Stream.Access <- 15
chk.Season.2.Percent.Forest.with.Stream.Access <- 39
chk.Season 2.Percent.RAOUT.with.stream.Access <- 0
*** Animal Densities
Season 1 Animal Density for Pasture in watershed (animal/ac):     3.0072900E-03
Season 1 Animal Density for Forest in watershed (animal/ac):      3.0072900E-03
Season 1 Animal Density for Residential/Agricultural Operations/Commercial/Urban/Transportation in watershed (animal/ac): 0.0
Season 2 Animal Density for Pasture in watershed (animal/ac):     4.5000000E-02
Season 2 Animal Density for Forest in watershed (animal/ac):      4.5000000E-02
Season 2 Animal Density for Residential/Agricultural Operations/Commercial/Urban/Transportation in watershed (animal/ac): 0.0
*** General parameters
Season 1 Percent of animals on Pasture in and around streams:      9.9500000E+00
Season 1 Percent of animals on Forest in and around streams:       1.9208850E+00
Season 1 Percent of animals on Residential/Agricultural Operations/Commercial/Urban/Transportation in and around streams:  0
Season 2 Percent of animals on Pasture in and around streams:      9.9500000E+00
Season 2 Percent of animals on Forest in and around streams:       9.9500000E+00
Season 2 Percent of animals on Residential/Agricultural Operations/Commercial/Urban/Transportation in and around streams:  0
Bacteria Production of animal per day (orgs/day):     6.6500000E+08
