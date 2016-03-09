## step bt step calculation check for wildlife_elk_sub_model using input from
## wildlifeelk01.txt file
chr.wildlife.elk.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-elk"
source(paste0(chr.wildlife.elk.dir,"/Wildlife_Elk_Sub_Model.R"))
df.output <- wildlifeElk(chr.wrkdir=chr.wildlife.elk.dir,chr.input="wildlifeelk01.txt")
## packages
library(doBy, quietly = TRUE)
library(gridExtra, quietly = TRUE)
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
chk.Season.1.Animal.Density.RAOCUT   <- 0.0
chk.Season.2.Animal.Density.Pasture <- 4.5000000E-02
chk.Season.2.Animal.Density.Forest  <- 4.5000000E-02
chk.Season.2.Animal.Density.RAOCUT   <- 0.0
### Habitats
chk.Season.1.Pasture <- 365.29
chk.Season.1.Forest  <- 5700.35
chk.Season.1.RAOCUT    <- 1
chk.Season.2.Pasture <- 64.55
chk.Season.2.Forest   <- 2135.41
chk.Season.2.RAOCUT   <- 1
### Percent of Landuse with Stream access
chk.Season.1.Percent.Pasture.with.Stream.Access <- 25
chk.Season.1.Percent.Forest.with.Stream.Access  <- 50
chk.Season.1.Percent.RAOCUT.with.Stream.Access   <- 0
chk.Season.2.Percent.Pasture.with.Stream.Access <- 15
chk.Season.2.Percent.Forest.with.Stream.Access  <- 39
chk.Season.2.Percent.RAOCUT.with.stream.Access   <- 0
### animals around streams
chk.Season.1.Percent.Pasture.in.and.around.streams <- 9.9500000E+00
chk.Season.1.Percent.Forest.in.and.around.streams  <- 1.9208850E+00
chk.Season.1.Percent.RAOCUT.in.and.around.streams   <- 0
chk.Season.2.Percent.Pasture.in.and.around.streams <- 9.9500000E+00
chk.Season.2.Percent.Forest.in.and.around.streams  <- 9.9500000E+00
chk.Season.2.Percent.RAOCUT.in.and.around.streams   <- 0
## calculations
## animal numbers
## season 1
chk.season.1.Pasture.elk <- chk.Season.1.Animal.Density.Pasture * 
  chk.Season.1.Pasture
chk.season.1.Forest.elk <- chk.Season.1.Animal.Density.Forest * 
  chk.Season.1.Forest
chk.season.1.RAOCUT.elk <- chk.Season.1.Animal.Density.RAOCUT * 
  chk.Season.1.RAOCUT
chk.season.1.total.elk <- chk.season.1.Pasture.elk + chk.season.1.Forest.elk +
  chk.season.1.RAOCUT.elk
## season 2
chk.season.2.Pasture.elk <- chk.Season.2.Animal.Density.Pasture * 
  chk.Season.2.Pasture
chk.season.2.Forest.elk <- chk.Season.2.Animal.Density.Forest * 
  chk.Season.2.Forest
chk.season.2.RAOCUT.elk <- chk.Season.2.Animal.Density.RAOCUT * 
  chk.Season.2.RAOCUT
chk.season.2.total.elk <- chk.season.2.Pasture.elk + chk.season.2.Forest.elk +
  chk.season.2.RAOCUT.elk
## with.without stream access
## without stream access
## Season 1
chk.season.1.Pasture.elk.wo.str.acc <- 
  (1 - chk.Season.1.Percent.Pasture.with.Stream.Access / 100) * 
  chk.season.1.Pasture.elk
chk.season.1.Forest.elk.wo.str.acc <- 
  (1 - chk.Season.1.Percent.Forest.with.Stream.Access / 100) * 
  chk.season.1.Forest.elk
chk.season.1.RAOCUT.elk.wo.str.acc <- 
  (1 - chk.Season.1.Percent.RAOCUT.with.Stream.Access / 100) * 
  chk.season.1.RAOCUT.elk
## Season 2
chk.season.2.Pasture.elk.wo.str.acc <- 
  (1 - chk.Season.2.Percent.Pasture.with.Stream.Access / 100) * 
  chk.season.2.Pasture.elk
chk.season.2.Forest.elk.wo.str.acc <- 
  (1 - chk.Season.2.Percent.Forest.with.Stream.Access / 100) * 
  chk.season.2.Forest.elk
chk.season.2.RAOCUT.elk.wo.str.acc <- 
  (1 - chk.Season.2.Percent.RAOCUT.with.stream.Access / 100) * 
  chk.season.2.RAOCUT.elk
## with stream access
## Season 1
chk.season.1.Pasture.elk.w.str.acc <- 
  (chk.Season.1.Percent.Pasture.with.Stream.Access / 100) * 
  chk.season.1.Pasture.elk
chk.season.1.Forest.elk.w.str.acc <- 
  (chk.Season.1.Percent.Forest.with.Stream.Access / 100) * 
  chk.season.1.Forest.elk
chk.season.1.RAOCUT.elk.w.str.acc <- 
  (chk.Season.1.Percent.RAOCUT.with.Stream.Access / 100) * 
  chk.season.1.RAOCUT.elk
## Season 2
chk.season.2.Pasture.elk.w.str.acc <- 
  (chk.Season.2.Percent.Pasture.with.Stream.Access / 100) * 
  chk.season.2.Pasture.elk
chk.season.2.Forest.elk.w.str.acc <- 
  (chk.Season.2.Percent.Forest.with.Stream.Access / 100) * 
  chk.season.2.Forest.elk
chk.season.2.RAOCUT.elk.w.str.acc <- 
  (chk.Season.2.Percent.RAOCUT.with.stream.Access / 100) * 
  chk.season.2.RAOCUT.elk
## with stream access on land
## Season 1
chk.season.1.Pasture.elk.w.str.acc.l <- 
  (1 - chk.Season.1.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.1.Pasture.elk.w.str.acc
chk.season.1.Forest.elk.w.str.acc.l <- 
  (1 - chk.Season.1.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.1.Forest.elk.w.str.acc
chk.season.1.RAOCUT.elk.w.str.acc.l <- 
  (1 - chk.Season.1.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.1.RAOCUT.elk.w.str.acc
## Season 2
chk.season.2.Pasture.elk.w.str.acc.l <- 
  (1 - chk.Season.2.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.2.Pasture.elk.w.str.acc
chk.season.2.Forest.elk.w.str.acc.l <- 
  (1 - chk.Season.2.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.2.Forest.elk.w.str.acc
chk.season.2.RAOCUT.elk.w.str.acc.l <- 
  (1 - chk.Season.2.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.2.RAOCUT.elk.w.str.acc
## with stream access in/around stream
## Season 1
chk.season.1.Pasture.elk.w.str.acc.s <- 
  (chk.Season.1.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.1.Pasture.elk.w.str.acc
chk.season.1.Forest.elk.w.str.acc.s <- 
  (chk.Season.1.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.1.Forest.elk.w.str.acc
chk.season.1.RAOCUT.elk.w.str.acc.s <- 
  (chk.Season.1.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.1.RAOCUT.elk.w.str.acc
## Season 2
chk.season.2.Pasture.elk.w.str.acc.s <- 
  (chk.Season.2.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.2.Pasture.elk.w.str.acc
chk.season.2.Forest.elk.w.str.acc.s <- 
  (chk.Season.2.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.2.Forest.elk.w.str.acc
chk.season.2.RAOCUT.elk.w.str.acc.s <- 
  (chk.Season.2.Percent.Pasture.in.and.around.streams / 100) * 
  chk.season.2.RAOCUT.elk.w.str.acc
## Elk on land
chk.season.1.Pasture.elk <- chk.season.1.Pasture.elk.wo.str.acc +
  chk.season.1.Pasture.elk.w.str.acc.l
chk.season.1.Forest.elk <- chk.season.1.Forest.elk.wo.str.acc +
  chk.season.1.Forest.elk.w.str.acc.l
chk.season.1.RAOCUT.elk <- chk.season.1.RAOCUT.elk.wo.str.acc +
  chk.season.1.RAOCUT.elk.w.str.acc.l
chk.season.2.Pasture.elk <- chk.season.2.Pasture.elk.wo.str.acc +
  chk.season.2.Pasture.elk.w.str.acc.l
chk.season.2.Forest.elk <- chk.season.2.Forest.elk.wo.str.acc +
  chk.season.2.Forest.elk.w.str.acc.l
chk.season.2.RAOCUT.elk <- chk.season.2.RAOCUT.elk.wo.str.acc +
  chk.season.2.RAOCUT.elk.w.str.acc.l
## Elk in and around stream
chk.season.1.Stream.elk <- chk.season.1.Pasture.elk.w.str.acc.s + 
  chk.season.1.Forest.elk.w.str.acc.s + chk.season.1.RAOCUT.elk.w.str.acc.s
chk.season.2.Stream.elk <- chk.season.2.Pasture.elk.w.str.acc.s + 
  chk.season.2.Forest.elk.w.str.acc.s + chk.season.2.RAOCUT.elk.w.str.acc.s
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
chk.total.pop.by.month <- 
  data.frame(chk.total.pop.by.month,
             dil = round(
               chk.dil * ( chk.total.pop.by.month$model.pop.total -
                                  chk.total.pop.by.month$manual.calc.pop.total) /
               chk.total.pop.by.month$manual.calc.pop.total,
               digits = 0))


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



## output results in tables to pdf
pdf(file = paste0(chr.wildlife.elk.dir, "/elk-bacteria-model-calc-check", 
                  strftime(Sys.time(), format = "%Y%m%d%H%M"), ".pdf"),
    height = 8.5, width = 11, onefile = TRUE)
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
rm(list = ls(pattern = "tmp\\.*"))

## close the pdf file
dev.off()
