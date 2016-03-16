## step bt step calculation check for wildlife_duck_sub_model using input from
## wildlifeDuck10.txt file
chr.wildlife.duck.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-Duck"
chr.input <- "wildlifeDuck10.txt"
source(paste0(chr.wildlife.duck.dir,"/Wildlife_Duck_Sub_Model.R"))
df.output <- wildlifeDuck(chr.wrkdir=chr.wildlife.duck.dir,chr.input=chr.input)
## packages
library(doBy, quietly = TRUE)
library(gridExtra, quietly = TRUE)
##
## get input
## SQOLIM multiplcation factor: 
chk.sqolim <- 9
chk.Bacteria.Prod <- 8.0000000E+08
### two seasons
chk.Season.1.Months <- c(11,12,1,5,6,7)
chk.Season.2.Months <- c(2,3,4,8,9,10)
### Animal Densities
chk.Season.1.Animal.Density <- 2.50E-02
chk.Season.2.Animal.Density <- 2.50E-04
### Habitats
chk.Pasture <- 0
chk.Forest  <- 0
chk.RAOCUT    <- 0
### All Landuse has Stream access for ducks
chk.Percent.with.Stream.Access <- 100
### same percent of ducks in and around streams for all landuse
chk.Percent.Pasture.in.and.around.streams <- 9.9999900E+01
## calculations
## animal numbers
## season 1
chk.season.1.Pasture.pop <- chk.Season.1.Animal.Density * chk.Pasture
chk.season.1.Forest.pop <-  chk.Season.1.Animal.Density * chk.Forest
chk.season.1.RAOCUT.pop <-  chk.Season.1.Animal.Density * chk.RAOCUT
chk.season.1.total.pop <-   chk.season.1.Pasture.pop + chk.season.1.Forest.pop +
  chk.season.1.RAOCUT.pop
## season 2
chk.season.2.Pasture.pop <- chk.Season.2.Animal.Density * chk.Pasture
chk.season.2.Forest.pop <-  chk.Season.2.Animal.Density * chk.Forest
chk.season.2.RAOCUT.pop <-  chk.Season.2.Animal.Density * chk.RAOCUT
chk.season.2.total.pop <-   chk.season.2.Pasture.pop + chk.season.2.Forest.pop +
  chk.season.2.RAOCUT.pop
## in/around stream
## season 1
chk.season.1.Pasture.pop.stream <- chk.season.1.Pasture.pop * 
  chk.Percent.Pasture.in.and.around.streams / 100
chk.season.1.Forest.pop.stream <-  chk.season.1.Forest.pop * 
  chk.Percent.Pasture.in.and.around.streams / 100
chk.season.1.RAOCUT.pop.stream <-  chk.season.1.RAOCUT.pop * 
  chk.Percent.Pasture.in.and.around.streams / 100
chk.season.1.total.pop.stream <-   chk.season.1.Pasture.pop.stream + 
  chk.season.1.Forest.pop.stream + chk.season.1.RAOCUT.pop.stream
## season 2
chk.season.2.Pasture.pop.stream <- chk.season.2.Pasture.pop * 
  chk.Percent.Pasture.in.and.around.streams / 100
chk.season.2.Forest.pop.stream <-  chk.season.2.Forest.pop * 
  chk.Percent.Pasture.in.and.around.streams / 100
chk.season.2.RAOCUT.pop.stream <-  chk.season.2.RAOCUT.pop * 
  chk.Percent.Pasture.in.and.around.streams / 100
chk.season.2.total.pop.stream <-   chk.season.2.Pasture.pop.stream + 
  chk.season.2.Forest.pop.stream + chk.season.2.RAOCUT.pop.stream

## on land
## season 1
chk.season.1.Pasture.pop.land <- chk.season.1.Pasture.pop - 
  chk.season.1.Pasture.pop.stream
chk.season.1.Forest.pop.land <-  chk.season.1.Forest.pop - 
  chk.season.1.Forest.pop.stream
chk.season.1.RAOCUT.pop.land <-  chk.season.1.RAOCUT.pop - 
  chk.season.1.RAOCUT.pop.stream
chk.season.1.total.pop.land <-   chk.season.1.Pasture.pop.land + 
  chk.season.1.Forest.pop.land + chk.season.1.RAOCUT.pop.land
## season 2
chk.season.2.Pasture.pop.land <- chk.season.2.Pasture.pop - 
  chk.season.2.Pasture.pop.stream
chk.season.2.Forest.pop.land <-  chk.season.2.Forest.pop - 
  chk.season.2.Forest.pop.stream
chk.season.2.RAOCUT.pop.land <-  chk.season.2.RAOCUT.pop - 
  chk.season.2.RAOCUT.pop.stream
chk.season.2.total.pop.land <-   chk.season.2.Pasture.pop.land + 
  chk.season.2.Forest.pop.land + chk.season.2.RAOCUT.pop.land
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

chk.location.season.pop <- rbind(
  data.frame(season = 1, 
             location = c("pasture", "forest", "RAOCUT","stream"),
             pop = c(chk.season.1.Pasture.pop.land,
                     chk.season.1.Forest.pop.land,
                     chk.season.1.RAOCUT.pop.land,
                     chk.season.1.total.pop.stream)),
  data.frame(season = 2, 
             location = c("pasture", "forest", "RAOCUT","stream"),
             pop = c(chk.season.2.Pasture.pop.land,
                     chk.season.2.Forest.pop.land,
                     chk.season.2.RAOCUT.pop.land,
                     chk.season.2.total.pop.stream)))
## populations
chk.pop <- merge(chk.months.season,chk.location.season.pop)
## bacteria loads
chk.bac <- data.frame(chk.pop, total.bac = chk.pop$pop * chk.Bacteria.Prod)
chk.bac <- data.frame(chk.bac, accum.bac = -1)

## accum
## season 1
## on pasture
tmp.rows <- grep("TRUE", with(chk.bac, 
                              c(season == 1 & location == "pasture")))
chk.bac[tmp.rows, "accum.bac"] <- 
  chk.bac[tmp.rows, "total.bac"] / chk.Pasture
## on forest
tmp.rows <- grep("TRUE", with(chk.bac, 
                              c(season == 1 & location == "forest")))
chk.bac[tmp.rows, "accum.bac"] <- 
  chk.bac[tmp.rows, "total.bac"] / chk.Forest
## on RAOCUT
tmp.rows <- grep("TRUE", with(chk.bac, 
                              c(season == 1 & location == "RAOCUT")))
chk.bac[tmp.rows, "accum.bac"] <- 
  chk.bac[tmp.rows, "total.bac"] / chk.RAOCUT
## season 2
## on pasture
tmp.rows <- grep("TRUE", with(chk.bac, 
                              c(season == 2 & location == "pasture")))
chk.bac[tmp.rows, "accum.bac"] <- 
  chk.bac[tmp.rows, "total.bac"] / chk.Pasture
## on forest
tmp.rows <- grep("TRUE", with(chk.bac, 
                              c(season == 2 & location == "forest")))
chk.bac[tmp.rows, "accum.bac"] <- 
  chk.bac[tmp.rows, "total.bac"] / chk.Forest
## on RAOCUT
tmp.rows <- grep("TRUE", with(chk.bac, 
                              c(season == 2 & location == "RAOCUT")))
chk.bac[tmp.rows, "accum.bac"] <- 
  chk.bac[tmp.rows, "total.bac"] / chk.RAOCUT
## for stream 
chk.bac[chk.bac$location == "stream", "accum.bac"] = NA

## sqolim
chk.bac <- data.frame(chk.bac, 
                          sqolim.bac = chk.bac$accum.bac * chk.sqolim)

##
## check model output
chk.dil <- 1E+06 # need to explain this
## population total and by locations
## total
chk.total.pop <- data.frame(
  manual.calc.pop.total = sum(chk.pop$pop),
  model.pop.total = sum(df.output$pop.total),
  dil = round(
    chk.dil * ( sum(df.output$pop.total) - sum(chk.pop$pop)) /
      sum(chk.pop$pop),
    digits = 0))

## total by month
chk.total.pop.by.month <- merge(summaryBy(pop ~ month.chr, data = chk.pop, FUN = sum),
      df.output[ , c("Month", "pop.total")],
      by.x = "month.chr", by.y = "Month")
names(chk.total.pop.by.month) <- c("Month", "manual.calc.pop.total",
                                   "model.pop.total")
tmp.month.num <- data.frame(Month = chk.total.pop.by.month$Month,
                            num = match(chk.total.pop.by.month$Month, month.abb))
chk.total.pop.by.month <- 
  chk.total.pop.by.month[with(tmp.month.num,order(num)),]
chk.total.pop.by.month <- 
  data.frame(chk.total.pop.by.month,
             dil = round(
               chk.dil * ( chk.total.pop.by.month$model.pop.total -
                                  chk.total.pop.by.month$manual.calc.pop.total) /
               chk.total.pop.by.month$manual.calc.pop.total,
               digits = 0))
chk.total.pop.by.month$Month <- factor(strftime(as.POSIXct(paste0("2016-",1:12,"-01")), "%b"),
                                          levels = strftime(
                                            as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
## pop in/around stream
chk.stream.pop <- data.frame(
  manual.calc.pop.total = sum(chk.pop[chk.pop$location == "stream", "pop"]),
  model.pop.total = sum(df.output[ , "pop.total.in.stream"]),
  dil = round(
    chk.dil * ( sum(df.output[ , "pop.total.in.stream"]) - 
                  sum(chk.pop[chk.pop$location == "stream", "pop"])) /
      sum(chk.pop[chk.pop$location == "stream", "pop"]),
    digits = 0))
## pop in/around stream by month
chk.stream.pop.by.month <- merge(summaryBy(pop ~ month.chr, 
                               data = chk.pop[chk.pop$location == "stream", ], 
                               FUN = sum),
                       df.output[ , c("Month", "pop.total.in.stream")],
                       by.x = "month.chr", by.y = "Month")
names(chk.stream.pop.by.month) <- c("Month", "manual.calc.pop.total",
                          "model.pop.total")
chk.stream.pop.by.month$Month <- factor(chk.stream.pop.by.month$Month,
                              levels = strftime(
                                as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.stream.pop.by.month <- 
  data.frame(chk.stream.pop.by.month, 
             dil = round(chk.dil * 
                           (chk.stream.pop.by.month$model.pop.total - 
                              chk.stream.pop.by.month$manual.calc.pop.total) /
                           chk.stream.pop.by.month$manual.calc.pop.total,
                         digits = 0))
chk.stream.pop.by.month <- 
  chk.stream.pop.by.month[with(tmp.month.num,order(num)),]
## pop on pasture
chk.pasture.pop <- data.frame(
  manual.calc.pop.total = sum(chk.pop[chk.pop$location == "pasture", "pop"]),
  model.pop.total = sum(df.output[ , "pop.total.on.pasture"]),
  dil = round(
    chk.dil * (sum(df.output[ , "pop.total.on.pasture"]) - 
                  sum(chk.pop[chk.pop$location == "pasture", "pop"])) /
      sum(chk.pop[chk.pop$location == "pasture", "pop"]),
    digits = 0))
## pop on pasture by month
chk.pasture.pop.by.month <- merge(summaryBy(pop ~ month.chr, 
                                        data = chk.pop[chk.pop$location == "pasture", ], 
                                        FUN = sum),
                              df.output[ , c("Month", "pop.total.on.pasture")],
                              by.x = "month.chr", by.y = "Month")
names(chk.pasture.pop.by.month) <- c("Month", "manual.calc.pop.total",
                                 "model.pop.total")
chk.pasture.pop.by.month$Month <- factor(chk.pasture.pop.by.month$Month,
                                     levels = strftime(
                                       as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.pasture.pop.by.month <- 
  data.frame(chk.pasture.pop.by.month, 
             dil = round(chk.dil * 
                           (chk.pasture.pop.by.month$model.pop.total - 
                              chk.pasture.pop.by.month$manual.calc.pop.total) /
                           chk.pasture.pop.by.month$manual.calc.pop.total,
                         digits = 0))
chk.pasture.pop.by.month <- 
  chk.pasture.pop.by.month[with(tmp.month.num,order(num)),]
## pop on forest
chk.forest.pop <- data.frame(
  manual.calc.pop.total = sum(chk.pop[chk.pop$location == "forest", "pop"]),
  model.pop.total = sum(df.output[ , "pop.total.in.forest"]),
  dil = round(
    chk.dil * (sum(df.output[ , "pop.total.in.forest"]) - 
                 sum(chk.pop[chk.pop$location == "forest", "pop"])) /
      sum(chk.pop[chk.pop$location == "forest", "pop"]),
    digits = 0))
## pop on forest by month
chk.forest.pop.by.month <- merge(summaryBy(pop ~ month.chr, 
                                            data = chk.pop[chk.pop$location == "forest", ], 
                                            FUN = sum),
                                  df.output[ , c("Month", "pop.total.in.forest")],
                                  by.x = "month.chr", by.y = "Month")
names(chk.forest.pop.by.month) <- c("Month", "manual.calc.pop.total",
                                     "model.pop.total")
chk.forest.pop.by.month$Month <- factor(chk.forest.pop.by.month$Month,
                                         levels = strftime(
                                           as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.forest.pop.by.month <- 
  data.frame(chk.forest.pop.by.month, 
             dil = round(chk.dil * 
                           (chk.forest.pop.by.month$model.pop.total - 
                              chk.forest.pop.by.month$manual.calc.pop.total) /
                           chk.forest.pop.by.month$manual.calc.pop.total,
                         digits = 0))
chk.forest.pop.by.month <- 
  chk.forest.pop.by.month[with(tmp.month.num,order(num)),]
## pop on RAOCUT
chk.RAOCUT.pop <- data.frame(
  manual.calc.pop.total = sum(chk.pop[chk.pop$location == "RAOCUT", "pop"]),
  model.pop.total = sum(df.output[ , "pop.total.on.RAOCUT"]),
  dil = round(
    chk.dil * (sum(df.output[ , "pop.total.on.RAOCUT"]) - 
                 sum(chk.pop[chk.pop$location == "RAOCUT", "pop"])) /
      sum(chk.pop[chk.pop$location == "RAOCUT", "pop"]),
    digits = 0))
## pop on RAOCUT by month
chk.RAOCUT.pop.by.month <- merge(summaryBy(pop ~ month.chr, 
                                           data = chk.pop[chk.pop$location == "RAOCUT", ], 
                                           FUN = sum),
                                 df.output[ , c("Month", "pop.total.on.RAOCUT")],
                                 by.x = "month.chr", by.y = "Month")
names(chk.RAOCUT.pop.by.month) <- c("Month", "manual.calc.pop.total",
                                    "model.pop.total")
chk.RAOCUT.pop.by.month$Month <- factor(chk.RAOCUT.pop.by.month$Month,
                                        levels = strftime(
                                          as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.RAOCUT.pop.by.month <- 
  data.frame(chk.RAOCUT.pop.by.month, 
             dil = round(chk.dil * 
                           (chk.RAOCUT.pop.by.month$model.pop.total - 
                              chk.RAOCUT.pop.by.month$manual.calc.pop.total) /
                           chk.RAOCUT.pop.by.month$manual.calc.pop.total,
                         digits = 0))
chk.RAOCUT.pop.by.month <- 
  chk.RAOCUT.pop.by.month[with(tmp.month.num,order(num)),]


## bacteria loads total and by locations
## total
chk.total.bac <- data.frame(
  manual.calc.bac.total = sum(chk.bac$total.bac),
  model.bac.total = sum(df.output$bac.total),
  dil = round(
    chk.dil * ( sum(df.output$bac.total) - sum(chk.bac$total.bac)) /
      sum(chk.bac$total.bac),
    digits = 0))

## total by month
chk.total.bac.by.month <- merge(summaryBy(total.bac ~ month.chr, data = chk.bac, FUN = sum),
                                df.output[ , c("Month", "bac.total")],
                                by.x = "month.chr", by.y = "Month")
names(chk.total.bac.by.month) <- c("Month", "manual.calc.bac.total",
                                   "model.bac.total")
tmp.month.num <- data.frame(Month = chk.total.bac.by.month$Month,
                            num = match(chk.total.bac.by.month$Month, month.abb))
chk.total.bac.by.month <- 
  chk.total.bac.by.month[with(tmp.month.num,order(num)),]
chk.total.bac.by.month <- 
  data.frame(chk.total.bac.by.month,
             dil = round(
               chk.dil * ( chk.total.bac.by.month$model.bac.total -
                             chk.total.bac.by.month$manual.calc.bac.total) /
                 chk.total.bac.by.month$manual.calc.bac.total,
               digits = 0))
chk.total.bac.by.month$Month <- factor(strftime(as.POSIXct(paste0("2016-",1:12,"-01")), "%b"),
                                        levels = strftime(
                                          as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
## bac in/around stream
chk.stream.bac <- data.frame(
  manual.calc.bac.total = sum(chk.bac[chk.bac$location == "stream", "total.bac"]),
  model.bac.total = sum(df.output[ , "bac.total.in.stream"]),
  dil = round(
    chk.dil * ( sum(df.output[ , "bac.total.in.stream"]) - 
                  sum(chk.bac[chk.bac$location == "stream", "total.bac"])) /
      sum(chk.bac[chk.bac$location == "stream", "total.bac"]),
    digits = 0))
## bac in/around stream by month
chk.stream.bac.by.month <- merge(summaryBy(total.bac ~ month.chr, 
                                           data = chk.bac[chk.bac$location == "stream", ], 
                                           FUN = sum),
                                 df.output[ , c("Month", "bac.total.in.stream")],
                                 by.x = "month.chr", by.y = "Month")
names(chk.stream.bac.by.month) <- c("Month", "manual.calc.bac.total",
                                    "model.bac.total")
chk.stream.bac.by.month$model.bac.total <- chk.stream.bac.by.month$model.bac.total
chk.stream.bac.by.month$Month <- factor(chk.stream.bac.by.month$Month,
                                        levels = strftime(
                                          as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.stream.bac.by.month <- 
  data.frame(chk.stream.bac.by.month, 
             dil = round(chk.dil * 
                           (chk.stream.bac.by.month$model.bac.total - 
                              chk.stream.bac.by.month$manual.calc.bac.total) /
                           chk.stream.bac.by.month$manual.calc.bac.total,
                         digits = 0))
chk.stream.bac.by.month <- 
  chk.stream.bac.by.month[with(tmp.month.num,order(num)),]
## bac load on pasture
chk.pasture.bac <- data.frame(
  manual.calc.bac.total = sum(chk.bac[chk.bac$location == "pasture", "total.bac"]),
  model.bac.total = sum(df.output[ , "bac.on.pasture"]),
  dil = round(
    chk.dil * (sum(df.output[ , "bac.on.pasture"]) - 
                 sum(chk.bac[chk.pop$location == "pasture", "total.bac"])) /
      sum(chk.bac[chk.bac$location == "pasture", "total.bac"]),
    digits = 0))
## bac load on pasture by month
chk.pasture.bac.by.month <- merge(summaryBy(total.bac ~ month.chr, 
                                            data = chk.bac[chk.bac$location == "pasture", ], 
                                            FUN = sum),
                                  df.output[ , c("Month", "bac.on.pasture")],
                                  by.x = "month.chr", by.y = "Month")
names(chk.pasture.bac.by.month) <- c("Month", "manual.calc.bac.total",
                                     "model.bac.total")
chk.pasture.bac.by.month$Month <- factor(chk.pasture.bac.by.month$Month,
                                         levels = strftime(
                                           as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.pasture.bac.by.month <- 
  data.frame(chk.pasture.bac.by.month, 
             dil = round(chk.dil * 
                           (chk.pasture.bac.by.month$model.bac.total - 
                              chk.pasture.bac.by.month$manual.calc.bac.total) /
                           chk.pasture.bac.by.month$manual.calc.bac.total,
                         digits = 0))
chk.pasture.bac.by.month <- 
  chk.pasture.bac.by.month[with(tmp.month.num,order(num)),]
## bac load in forest
chk.forest.bac <- data.frame(
  manual.calc.bac.total = sum(chk.bac[chk.bac$location == "forest", "total.bac"]),
  model.bac.total = sum(df.output[ , "bac.in.forest"]),
  dil = round(
    chk.dil * (sum(df.output[ , "bac.in.forest"]) - 
                 sum(chk.bac[chk.bac$location == "forest", "total.bac"])) /
      sum(chk.bac[chk.bac$location == "forest", "total.bac"]),
    digits = 0))
## bac load on forest by month
chk.forest.bac.by.month <- merge(summaryBy(total.bac ~ month.chr, 
                                           data = chk.bac[chk.bac$location == "forest", ], 
                                           FUN = sum),
                                 df.output[ , c("Month", "bac.in.forest")],
                                 by.x = "month.chr", by.y = "Month")
names(chk.forest.bac.by.month) <- c("Month", "manual.calc.bac.total",
                                    "model.bac.total")
chk.forest.bac.by.month$Month <- factor(chk.forest.bac.by.month$Month,
                                        levels = strftime(
                                          as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.forest.bac.by.month <- 
  data.frame(chk.forest.bac.by.month, 
             dil = round(chk.dil * 
                           (chk.forest.bac.by.month$model.bac.total - 
                              chk.forest.bac.by.month$manual.calc.bac.total) /
                           chk.forest.bac.by.month$manual.calc.bac.total,
                         digits = 0))
chk.forest.bac.by.month <- 
  chk.forest.bac.by.month[with(tmp.month.num,order(num)),]
## bac on RAOCUT
chk.RAOCUT.bac <- data.frame(
  manual.calc.bac.total = sum(chk.bac[chk.bac$location == "RAOCUT", "total.bac"]),
  model.bac.total = sum(df.output[ , "bac.on.RAOCUT"]),
  dil = round(
    chk.dil * (sum(df.output[ , "bac.on.RAOCUT"]) - 
                 sum(chk.bac[chk.bac$location == "RAOCUT", "total.bac"])) /
      sum(chk.bac[chk.bac$location == "RAOCUT", "total.bac"]),
    digits = 0))
## bac on RAOCUT by month
chk.RAOCUT.bac.by.month <- merge(summaryBy(total.bac ~ month.chr, 
                                           data = chk.bac[chk.bac$location == "RAOCUT", ], 
                                           FUN = sum),
                                 df.output[ , c("Month", "bac.on.RAOCUT")],
                                 by.x = "month.chr", by.y = "Month")
names(chk.RAOCUT.bac.by.month) <- c("Month", "manual.calc.bac.total",
                                    "model.bac.total")
chk.RAOCUT.bac.by.month$Month <- factor(chk.RAOCUT.bac.by.month$Month,
                                        levels = strftime(
                                          as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.RAOCUT.bac.by.month <- 
  data.frame(chk.RAOCUT.bac.by.month, 
             dil = round(chk.dil * 
                           (chk.RAOCUT.bac.by.month$model.bac.total - 
                              chk.RAOCUT.bac.by.month$manual.calc.bac.total) /
                           chk.RAOCUT.bac.by.month$manual.calc.bac.total,
                         digits = 0))
chk.RAOCUT.bac.by.month <- 
  chk.RAOCUT.bac.by.month[with(tmp.month.num,order(num)),]

## accum loads
## accum load on pasture by month
chk.pasture.accum.by.month <- merge(summaryBy(accum.bac ~ month.chr, 
                                           data = chk.bac[chk.bac$location == "pasture", ], 
                                           FUN = sum),
                                 df.output[ , c("Month", "Accum.Pasture")],
                                 by.x = "month.chr", by.y = "Month")
names(chk.pasture.accum.by.month) <- c("Month", "manual.calc.bac.total",
                                    "model.bac.total")
chk.pasture.accum.by.month$Month <- factor(chk.pasture.accum.by.month$Month,
                                        levels = strftime(
                                          as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.pasture.accum.by.month <- 
  data.frame(chk.pasture.accum.by.month, 
             dil = round(chk.dil * 
                           (chk.pasture.accum.by.month$model.bac.total - 
                              chk.pasture.accum.by.month$manual.calc.bac.total) /
                           chk.pasture.accum.by.month$manual.calc.bac.total,
                         digits = 0))
tmp.month.num <- data.frame(Month = chk.pasture.accum.by.month$Month,
                            num = match(chk.pasture.accum.by.month$Month, month.abb))
chk.pasture.accum.by.month <- 
  chk.pasture.accum.by.month[with(tmp.month.num,order(num)),]
##
## accum load on forest by month
chk.forest.accum.by.month <- merge(summaryBy(accum.bac ~ month.chr, 
                                              data = chk.bac[chk.bac$location == "forest", ], 
                                              FUN = sum),
                                    df.output[ , c("Month", "Accum.Forest")],
                                    by.x = "month.chr", by.y = "Month")
names(chk.forest.accum.by.month) <- c("Month", "manual.calc.bac.total",
                                       "model.bac.total")
chk.forest.accum.by.month$Month <- factor(chk.forest.accum.by.month$Month,
                                           levels = strftime(
                                             as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.forest.accum.by.month <- 
  data.frame(chk.forest.accum.by.month, 
             dil = round(chk.dil * 
                           (chk.forest.accum.by.month$model.bac.total - 
                              chk.forest.accum.by.month$manual.calc.bac.total) /
                           chk.forest.accum.by.month$manual.calc.bac.total,
                         digits = 0))
tmp.month.num <- data.frame(Month = chk.forest.accum.by.month$Month,
                            num = match(chk.forest.accum.by.month$Month, month.abb))
chk.forest.accum.by.month <- 
  chk.forest.accum.by.month[with(tmp.month.num,order(num)),]
##
## accum load on RAOCUT by month
chk.RAOCUT.accum.by.month <- merge(summaryBy(accum.bac ~ month.chr, 
                                             data = chk.bac[chk.bac$location == "RAOCUT", ], 
                                             FUN = sum),
                                   df.output[ , c("Month", "Accum.RAOCUT")],
                                   by.x = "month.chr", by.y = "Month")
names(chk.RAOCUT.accum.by.month) <- c("Month", "manual.calc.bac.total",
                                      "model.bac.total")
chk.RAOCUT.accum.by.month$Month <- factor(chk.RAOCUT.accum.by.month$Month,
                                          levels = strftime(
                                            as.POSIXct(paste0("2016-",1:12,"-01")), "%b"))
chk.RAOCUT.accum.by.month <- 
  data.frame(chk.RAOCUT.accum.by.month, 
             dil = round(chk.dil * 
                           (chk.RAOCUT.accum.by.month$model.bac.total - 
                              chk.RAOCUT.accum.by.month$manual.calc.bac.total) /
                           chk.RAOCUT.accum.by.month$manual.calc.bac.total,
                         digits = 0))
tmp.month.num <- data.frame(Month = chk.RAOCUT.accum.by.month$Month,
                            num = match(chk.RAOCUT.accum.by.month$Month, month.abb))
chk.RAOCUT.accum.by.month <- 
  chk.RAOCUT.accum.by.month[with(tmp.month.num,order(num)),]





## output results in tables to pdf
pdf(file = paste0(chr.wildlife.duck.dir, "/duck-bacteria-model-calc-check-",
                  gsub("\\.txt","-",chr.input) 
                  ,strftime(Sys.time(), format = "%Y%m%d%H%M"),
                  ".pdf"), height = 8.5, width = 11, onefile = TRUE)
## total population
tmp.table <- tableGrob(chk.total.pop, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total Duck Population (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## total population by month
tmp.table <- tableGrob(chk.total.pop.by.month, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total Duck Population by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population in/around stream
tmp.table <- tableGrob(chk.stream.pop, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total Duck Population in/around stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population in/around stream by month
tmp.table <- tableGrob(chk.stream.pop.by.month, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total Duck Population in/around stream by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))

## population on pasture
tmp.table <- tableGrob(chk.pasture.pop, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total Duck Population on pasture (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population in/around stream by month
tmp.table <- tableGrob(chk.pasture.pop.by.month, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total Duck Population on pasture by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))

## population in forest
tmp.table <- tableGrob(chk.forest.pop, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total Duck Population in forest (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population in forest stream by month
tmp.table <- tableGrob(chk.forest.pop.by.month, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total Duck Population in forest by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))

## population on RAOCUT
tmp.table <- tableGrob(chk.RAOCUT.pop, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total Duck Population on RAOCUT (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population on RAOCUT by month
tmp.table <- tableGrob(chk.RAOCUT.pop.by.month, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total Duck Population on RAOCUT by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))


## total bac load
tmp.table <- tableGrob(chk.total.bac, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total bacteria load from Duck (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## total bac load by month
tmp.table <- tableGrob(chk.total.bac.by.month, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total bacteria load from Duck by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## bac load in/around stream
tmp.table <- tableGrob(chk.stream.bac, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total bacteria load from Duck in/around stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## bac load in/around stream by month
tmp.table <- tableGrob(chk.stream.bac.by.month, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total bacteria load from Duck in/around stream by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))

## bac load on pasture
tmp.table <- tableGrob(chk.pasture.bac, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total bacteria load from Duck on pasture (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
##  bac load on pasture by month
tmp.table <- tableGrob(chk.pasture.bac.by.month, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total bacteria laod from Duck on pasture by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))

## bac load in forest
tmp.table <- tableGrob(chk.forest.bac, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total bacteria load from Duck in forest (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## bac load in forest stream by month
tmp.table <- tableGrob(chk.forest.bac.by.month, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total bacteria load from Duck in forest by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))

## bac load on RAOCUT
tmp.table <- tableGrob(chk.RAOCUT.pop, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total bacteria load from Duck on RAOCUT (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))

## accum load on pasture by month
tmp.table <- tableGrob(chk.pasture.accum.by.month, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Accum load from Duck on Pasture by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))

## accum load on forest by month
tmp.table <- tableGrob(chk.forest.accum.by.month, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Accum load from Duck in Forest by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))

## accum load on RAOCUT by month
tmp.table <- tableGrob(chk.RAOCUT.accum.by.month, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Accum load from Duck on RAOCUT by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
rm(list = ls(pattern = "tmp\\.*"))



## close the pdf file
dev.off()
