## step bt step calculation check for wildlife_elk_sub_model using input from
## wildlifeelk01.txt file
chr.wildlife.elk.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-elk"
source(paste0(chr.wildlife.elk.dir,"/Wildlife_Elk_Sub_Model.R"))
df.output <- wildlifeElk(chr.wrkdir=chr.wildlife.elk.dir,chr.input="wildlifeelk01.txt")
## packages
library(doBy)
##
## get input
## SQOLIM multiplcation factor: 
chk.sqolim <- 9
chk.Bacteria.Prod <- 6.6500000E+08
### two seasons
chk.Season.1.Months <- c(11,12,1,2,3)
chk.Season.2.Months <- c(4,5,6,7,8,9,10)
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
## Elk on land
chk.season.1.Pasture.elk <- chk.season.1.Pasture.elk.wo.str.acc +
  chk.season.1.Pasture.elk.w.str.acc.l
chk.season.1.Forest.elk <- chk.season.1.Forest.elk.wo.str.acc +
  chk.season.1.Forest.elk.w.str.acc.l
chk.season.1.RAOUT.elk <- chk.season.1.RAOUT.elk.wo.str.acc +
  chk.season.1.RAOUT.elk.w.str.acc.l
chk.season.2.Pasture.elk <- chk.season.2.Pasture.elk.wo.str.acc +
  chk.season.2.Pasture.elk.w.str.acc.l
chk.season.2.Forest.elk <- chk.season.2.Forest.elk.wo.str.acc +
  chk.season.2.Forest.elk.w.str.acc.l
chk.season.2.RAOUT.elk <- chk.season.2.RAOUT.elk.wo.str.acc +
  chk.season.2.RAOUT.elk.w.str.acc.l
## Elk in and around stream
chk.season.1.Stream.elk <- chk.season.1.Pasture.elk.w.str.acc.s + 
  chk.season.1.Forest.elk.w.str.acc.s + chk.season.1.RAOUT.elk.w.str.acc.s
chk.season.2.Stream.elk <- chk.season.2.Pasture.elk.w.str.acc.s + 
  chk.season.2.Forest.elk.w.str.acc.s + chk.season.2.RAOUT.elk.w.str.acc.s
##
## combining results
chk.months <- 
  data.frame(month = 1:12, 
             month.chr =  factor(strftime(
               as.POSIXct(paste0("2016-",1:12,"-01")), "%b"),
               levels = strftime(
                 as.POSIXct(paste0("2016-",1:12,"-01")), "%b")))
chk.months.season <- rbind(data.frame(month = chk.Season.1.Months, season = 1),
                           data.frame(month = chk.Season.2.Months, season = 2))
chk.months.season <- merge(chk.months.season, chk.months)

chk.location.season.elk <- rbind(
  data.frame(season = 1, 
             location = c("pasture", "forest", "RAOUT","stream"),
             elk = c(chk.season.1.Pasture.elk,
                     chk.season.1.Forest.elk,
                     chk.season.1.RAOUT.elk,
                     chk.season.1.Stream.elk)),
  data.frame(season = 2, 
             location = c("pasture", "forest", "RAOUT","stream"),
             elk = c(chk.season.2.Pasture.elk,
                     chk.season.2.Forest.elk,
                     chk.season.2.RAOUT.elk,
                     chk.season.2.Stream.elk)))
## populations
chk.elk <- merge(chk.months.season,chk.location.season.elk)
## bacteria loads
chk.elk.bac <- data.frame(chk.elk, total.bac = chk.elk$elk * chk.Bacteria.Prod)
chk.elk.bac <- data.frame(chk.elk.bac, accum.bac = -1)

## accum
## season 1
## on pasture
tmp.rows <- grep("TRUE", with(chk.elk.bac, 
                              c(season == 1 & location == "pasture")))
chk.elk.bac[tmp.rows, "accum.bac"] <- 
  chk.elk.bac[tmp.rows, "total.bac"] / chk.Season.1.Pasture
## on forest
tmp.rows <- grep("TRUE", with(chk.elk.bac, 
                              c(season == 1 & location == "forest")))
chk.elk.bac[tmp.rows, "accum.bac"] <- 
  chk.elk.bac[tmp.rows, "total.bac"] / chk.Season.1.Forest
## on RAOUT
tmp.rows <- grep("TRUE", with(chk.elk.bac, 
                              c(season == 1 & location == "RAOUT")))
chk.elk.bac[tmp.rows, "accum.bac"] <- 
  chk.elk.bac[tmp.rows, "total.bac"] / chk.Season.1.RAOUT
## season 2
## on pasture
tmp.rows <- grep("TRUE", with(chk.elk.bac, 
                              c(season == 2 & location == "pasture")))
chk.elk.bac[tmp.rows, "accum.bac"] <- 
  chk.elk.bac[tmp.rows, "total.bac"] / chk.Season.2.Pasture
## on forest
tmp.rows <- grep("TRUE", with(chk.elk.bac, 
                              c(season == 2 & location == "forest")))
chk.elk.bac[tmp.rows, "accum.bac"] <- 
  chk.elk.bac[tmp.rows, "total.bac"] / chk.Season.2.Forest
## on RAOUT
tmp.rows <- grep("TRUE", with(chk.elk.bac, 
                              c(season == 2 & location == "RAOUT")))
chk.elk.bac[tmp.rows, "accum.bac"] <- 
  chk.elk.bac[tmp.rows, "total.bac"] / chk.Season.2.RAOUT
## for stream 
chk.elk.bac[chk.elk.bac$location == "stream", "accum.bac"] = NA

## sqolim
chk.elk.bac <- data.frame(chk.elk.bac, 
                          sqolim.bac = chk.elk.bac$accum.bac * chk.sqolim)

##
## check model output
str(df.output)
str(chk.elk)
## totals by month
chk.total.pop <- merge(summaryBy(elk ~ month.chr, data = chk.elk, FUN = sum),
      df.output[ , c("Month", "pop.total")],
      by.x = "month.chr", by.y = "Month")

names(chk.total.pop) <- c("Month", "manual.calc.pop.total",
                          "model.pop.total")
chk.total.pop$Month <- factor(chk.total.pop$Month,
                              levels = strftime(
                                as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.total.pop
## pop in/around stream by month
chk.str.pop <- merge(summaryBy(elk ~ month.chr, 
                               data = chk.elk[chk.elk$location == "stream", ], 
                               FUN = sum),
                       df.output[ , c("Month", "pop.total.in.stream")],
                       by.x = "month.chr", by.y = "Month")

names(chk.str.pop) <- c("Month", "manual.calc.pop.total",
                          "model.pop.total")
chk.str.pop$Month <- factor(chk.str.pop$Month,
                              levels = strftime(
                                as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.str.pop






## stream bac load by month
str(chk.elk.bac)

chk.elk.bac[chk.elk.bac$location == "stream", 
             c("month.chr", "total.bac")]
chk.stream.bac.load <- merge(summaryBy( ~ month.chr, data = chk.elk, FUN = sum),
                       df.output[ , c("Month", "pop.total")],
                       by.x = "month.chr", by.y = "Month")

names(chk.total.pop) <- c("Month", "manual.calc.pop.total",
                          "model.pop.total")
chk.total.pop$Month <- factor(chk.total.pop$Month,
                              levels = strftime(
                                as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))


