## step bt step calculation check for wildlife_deer_sub_model using input from
## wildlifedeer01.txt file
chr.wildlife.deer.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-deer"
chr.input <- "wildlifedeer01.txt"
source(paste0(chr.wildlife.deer.dir,"/Wildlife_Deer_Sub_Model.R"))
df.output <- wildlifeDeer(chr.wrkdir=chr.wildlife.deer.dir,chr.input=chr.input)
## packages
library(doBy, quietly = TRUE)
library(gridExtra, quietly = TRUE)
##
## get input
## SQOLIM multiplcation factor: 
chk.sqolim <- 9
chk.Bacteria.Prod <- 1.8373286E+08
### Animal Densities
chk.Animal.Density <- 4.7000000E-02
### Habitats
chk.habitat <- 9777.0 # where does this number come from it's 545 ac greater than the sum of pasture and forest
### land end pints
chk.land.Forest   <- 8790.81
chk.land.Pasture <- 441.35
chk.land.RAOCUT <- 435.35
chk.land.Total <- chk.land.Forest + chk.land.Pasture + chk.land.RAOCUT
### Percent of Landuse with Stream access
chk.Percent.habitat.with.Stream.Access <- 50
### percent of animals in/around streams
chk.in.and.around.streams <- 1.0000000E+00
## calculations
## animal numbers
chk.pop.total <- chk.Animal.Density *   chk.habitat
## with/without stream access
chk.pop.wo.stream.access <- chk.pop.total * (1 - chk.Percent.habitat.with.Stream.Access / 100)
chk.pop.w.stream.access <- chk.pop.total * (chk.Percent.habitat.with.Stream.Access / 100)
## with stream access on in/around atream
chk.pop.in.around.stream <- chk.pop.w.stream.access * (chk.in.and.around.streams / 100)
## deer on land
chk.pop.on.land <- chk.pop.wo.sream.access +
  chk.pop.w.stream.access * (1 - chk.in.and.around.streams / 100)

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
             location = c("pasture", "forest", "RAOCUT","stream"),
             elk = c(chk.season.1.Pasture.elk,
                     chk.season.1.Forest.elk,
                     chk.season.1.RAOCUT.elk,
                     chk.season.1.Stream.elk)),
  data.frame(season = 2, 
             location = c("pasture", "forest", "RAOCUT","stream"),
             elk = c(chk.season.2.Pasture.elk,
                     chk.season.2.Forest.elk,
                     chk.season.2.RAOCUT.elk,
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
## on RAOCUT
tmp.rows <- grep("TRUE", with(chk.elk.bac, 
                              c(season == 1 & location == "RAOCUT")))
chk.elk.bac[tmp.rows, "accum.bac"] <- 
  chk.elk.bac[tmp.rows, "total.bac"] / chk.Season.1.RAOCUT
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
## on RAOCUT
tmp.rows <- grep("TRUE", with(chk.elk.bac, 
                              c(season == 2 & location == "RAOCUT")))
chk.elk.bac[tmp.rows, "accum.bac"] <- 
  chk.elk.bac[tmp.rows, "total.bac"] / chk.Season.2.RAOCUT
## for stream 
chk.elk.bac[chk.elk.bac$location == "stream", "accum.bac"] = NA

## sqolim
chk.elk.bac <- data.frame(chk.elk.bac, 
                          sqolim.bac = chk.elk.bac$accum.bac * chk.sqolim)

##
## check model output
chk.dil <- 1E+06 # need to explain this
## population total and by locations
## total
chk.total.pop <- data.frame(
  manual.calc.pop.total = sum(chk.elk$elk),
  model.pop.total = sum(df.output$pop.total),
  dil = round(
    chk.dil * ( sum(df.output$pop.total) - sum(chk.elk$elk)) /
      sum(chk.elk$elk),
    digits = 0))

## total by month
chk.total.pop.by.month <- merge(summaryBy(elk ~ month.chr, data = chk.elk, FUN = sum),
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
  manual.calc.pop.total = sum(chk.elk[chk.elk$location == "stream", "elk"]),
  model.pop.total = sum(df.output[ , "pop.total.in.stream"]),
  dil = round(
    chk.dil * ( sum(df.output[ , "pop.total.in.stream"]) - 
                  sum(chk.elk[chk.elk$location == "stream", "elk"])) /
      sum(chk.elk[chk.elk$location == "stream", "elk"]),
    digits = 0))
## pop in/around stream by month
chk.stream.pop.by.month <- merge(summaryBy(elk ~ month.chr, 
                               data = chk.elk[chk.elk$location == "stream", ], 
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
  manual.calc.pop.total = sum(chk.elk[chk.elk$location == "pasture", "elk"]),
  model.pop.total = sum(df.output[ , "pop.total.on.pasture"]),
  dil = round(
    chk.dil * (sum(df.output[ , "pop.total.on.pasture"]) - 
                  sum(chk.elk[chk.elk$location == "pasture", "elk"])) /
      sum(chk.elk[chk.elk$location == "pasture", "elk"]),
    digits = 0))
## pop on pasture by month
chk.pasture.pop.by.month <- merge(summaryBy(elk ~ month.chr, 
                                        data = chk.elk[chk.elk$location == "pasture", ], 
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
  manual.calc.pop.total = sum(chk.elk[chk.elk$location == "forest", "elk"]),
  model.pop.total = sum(df.output[ , "pop.total.in.forest"]),
  dil = round(
    chk.dil * (sum(df.output[ , "pop.total.in.forest"]) - 
                 sum(chk.elk[chk.elk$location == "forest", "elk"])) /
      sum(chk.elk[chk.elk$location == "forest", "elk"]),
    digits = 0))
## pop on forest by month
chk.forest.pop.by.month <- merge(summaryBy(elk ~ month.chr, 
                                            data = chk.elk[chk.elk$location == "forest", ], 
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
  manual.calc.pop.total = sum(chk.elk[chk.elk$location == "RAOCUT", "elk"]),
  model.pop.total = sum(df.output[ , "pop.total.on.RAOCUT"]),
  dil = round(
    chk.dil * (sum(df.output[ , "pop.total.on.RAOCUT"]) - 
                 sum(chk.elk[chk.elk$location == "RAOCUT", "elk"])) /
      sum(chk.elk[chk.elk$location == "RAOCUT", "elk"]),
    digits = 0))
## pop on RAOCUT by month
chk.RAOCUT.pop.by.month <- merge(summaryBy(elk ~ month.chr, 
                                           data = chk.elk[chk.elk$location == "RAOCUT", ], 
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
  manual.calc.bac.total = sum(chk.elk.bac$total.bac),
  model.bac.total = sum(df.output$bac.total),
  dil = round(
    chk.dil * ( sum(df.output$bac.total) - sum(chk.elk.bac$total.bac)) /
      sum(chk.elk.bac$total.bac),
    digits = 0))

## total by month
chk.total.bac.by.month <- merge(summaryBy(total.bac ~ month.chr, data = chk.elk.bac, FUN = sum),
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
  manual.calc.bac.total = sum(chk.elk.bac[chk.elk.bac$location == "stream", "total.bac"]),
  model.bac.total = 24 * sum(df.output[ , "bac.total.in.stream"]),
  dil = round(
    chk.dil * ( 24 * sum(df.output[ , "bac.total.in.stream"]) - 
                  sum(chk.elk.bac[chk.elk.bac$location == "stream", "total.bac"])) /
      sum(chk.elk.bac[chk.elk.bac$location == "stream", "total.bac"]),
    digits = 0))
## bac in/around stream by month
chk.stream.bac.by.month <- merge(summaryBy(total.bac ~ month.chr, 
                                           data = chk.elk.bac[chk.elk.bac$location == "stream", ], 
                                           FUN = sum),
                                 df.output[ , c("Month", "bac.total.in.stream")],
                                 by.x = "month.chr", by.y = "Month")
names(chk.stream.bac.by.month) <- c("Month", "manual.calc.bac.total",
                                    "model.bac.total")
chk.stream.bac.by.month$model.bac.total <- 24 * chk.stream.bac.by.month$model.bac.total
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
  manual.calc.bac.total = sum(chk.elk.bac[chk.elk.bac$location == "pasture", "total.bac"]),
  model.bac.total = sum(df.output[ , "bac.Pasture"]),
  dil = round(
    chk.dil * (sum(df.output[ , "bac.Pasture"]) - 
                 sum(chk.elk.bac[chk.elk$location == "pasture", "total.bac"])) /
      sum(chk.elk.bac[chk.elk.bac$location == "pasture", "total.bac"]),
    digits = 0))
## bac load on pasture by month
chk.pasture.bac.by.month <- merge(summaryBy(total.bac ~ month.chr, 
                                            data = chk.elk.bac[chk.elk.bac$location == "pasture", ], 
                                            FUN = sum),
                                  df.output[ , c("Month", "bac.Pasture")],
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
  manual.calc.bac.total = sum(chk.elk.bac[chk.elk.bac$location == "forest", "total.bac"]),
  model.bac.total = sum(df.output[ , "bac.Forest"]),
  dil = round(
    chk.dil * (sum(df.output[ , "bac.Forest"]) - 
                 sum(chk.elk.bac[chk.elk.bac$location == "forest", "total.bac"])) /
      sum(chk.elk.bac[chk.elk.bac$location == "forest", "total.bac"]),
    digits = 0))
## bac load on forest by month
chk.forest.bac.by.month <- merge(summaryBy(total.bac ~ month.chr, 
                                           data = chk.elk.bac[chk.elk.bac$location == "forest", ], 
                                           FUN = sum),
                                 df.output[ , c("Month", "bac.Forest")],
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
  manual.calc.bac.total = sum(chk.elk.bac[chk.elk.bac$location == "RAOCUT", "total.bac"]),
  model.bac.total = sum(df.output[ , "bac.RAOCUT"]),
  dil = round(
    chk.dil * (sum(df.output[ , "bac.RAOCUT"]) - 
                 sum(chk.elk.bac[chk.elk.bac$location == "RAOCUT", "total.bac"])) /
      sum(chk.elk.bac[chk.elk.bac$location == "RAOCUT", "total.bac"]),
    digits = 0))
## bac on RAOCUT by month
chk.RAOCUT.bac.by.month <- merge(summaryBy(total.bac ~ month.chr, 
                                           data = chk.elk.bac[chk.elk.bac$location == "RAOCUT", ], 
                                           FUN = sum),
                                 df.output[ , c("Month", "bac.RAOCUT")],
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
                                           data = chk.elk.bac[chk.elk.bac$location == "pasture", ], 
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
                                              data = chk.elk.bac[chk.elk.bac$location == "forest", ], 
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
                                             data = chk.elk.bac[chk.elk.bac$location == "RAOCUT", ], 
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
pdf(file = paste0(chr.wildlife.elk.dir, "/elk-bacteria-model-calc-check-",
                  gsub("\\.txt","-",chr.input) 
                  ,strftime(Sys.time(), format = "%Y%m%d%H%M"),
                  ".pdf"), height = 8.5, width = 11, onefile = TRUE)
## total population
tmp.table <- tableGrob(chk.total.pop, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Total Elk Population (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total Elk Population by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total Elk Population in/around stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total Elk Population in/around stream by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total Elk Population on pasture (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total Elk Population on pasture by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total Elk Population in forest (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total Elk Population in forest by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total Elk Population on RAOCUT (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total Elk Population on RAOCUT by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total bacteria load from Elk (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total bacteria load from Elk by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total bacteria load from Elk in/around stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total bacteria load from Elk in/around stream by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total bacteria load from Elk on pasture (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total bacteria laod from Elk on pasture by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total bacteria load from Elk in forest (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total bacteria load from Elk in forest by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Total bacteria load from Elk on RAOCUT (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Accum load from Elk on Pasture by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Accum load from Elk in Forest by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Accum load from Elk on RAOCUT by month (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
rm(list = ls(pattern = "tmp\\.*"))



## close the pdf file
dev.off()
