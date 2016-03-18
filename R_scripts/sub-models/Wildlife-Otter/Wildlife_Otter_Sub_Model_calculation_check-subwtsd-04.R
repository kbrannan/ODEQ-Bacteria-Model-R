## step bt step calculation check for wildlife_otter_sub_model using input from
## wildlifeotter04.txt file
chr.wildlife.otter.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-otter"
chr.input <- "wildlifeotter04.txt"
source(paste0(chr.wildlife.otter.dir,"/Wildlife_Otter_Sub_Model.R"))
df.output <- wildlifeOtter(chr.wrkdir=chr.wildlife.otter.dir,chr.input=chr.input)
## packages
library(doBy, quietly = TRUE)
library(gridExtra, quietly = TRUE)
##
## get input
## SQOLIM multiplcation factor: 
chk.sqolim <- 9
chk.Bacteria.Prod <- 1.9665000E+07
### Animal Densities
chk.Animal.Density <- 3.0000000E-01
### habitat
chk.land.Forest   <- 70.99
chk.land.Pasture <- 18.16
Stream.Habitat.Pasture <- 4
Stream.Habitat.Forest <- 12
### Percent of Landuse with Stream access
chk.Percent.habitat.with.Stream.Access <- 100
### percent of animals in/around streams
chk.in.and.around.streams <- 3.1213069E+01
## calculations
## animal numbers
chk.pop.Forest <- chk.Animal.Density * Stream.Habitat.Forest
chk.pop.Pasture <- chk.Animal.Density * Stream.Habitat.Pasture
chk.pop.total <- chk.pop.Forest + chk.pop.Pasture
## with/without stream access
chk.pop.forest.wo.stream.access <- chk.pop.Forest * (1 - chk.Percent.habitat.with.Stream.Access / 100)
chk.pop.forest.w.stream.access <- chk.pop.Forest * (chk.Percent.habitat.with.Stream.Access / 100)
chk.pop.pasture.wo.stream.access <- chk.pop.Pasture * (1 - chk.Percent.habitat.with.Stream.Access / 100)
chk.pop.pasture.w.stream.access <- chk.pop.Pasture * (chk.Percent.habitat.with.Stream.Access / 100)
## with stream access on in/around atream
chk.pop.forest.in.around.stream <- chk.pop.forest.w.stream.access * (chk.in.and.around.streams / 100)
chk.pop.pasture.in.around.stream <- chk.pop.pasture.w.stream.access * (chk.in.and.around.streams / 100)
chk.pop.in.around.stream <- chk.pop.forest.in.around.stream + chk.pop.pasture.in.around.stream
## otter on land
chk.pop.forest.on.land.w.stream.access <- chk.pop.forest.w.stream.access -
  chk.pop.forest.in.around.stream
chk.pop.pasture.on.land.w.stream.access <- chk.pop.pasture.w.stream.access -
  chk.pop.pasture.in.around.stream
chk.pop.forest.on.land <- chk.pop.forest.wo.stream.access + 
  chk.pop.forest.on.land.w.stream.access
chk.pop.pasture.on.land <- chk.pop.pasture.wo.stream.access +
  chk.pop.pasture.on.land.w.stream.access
chk.pop.on.land <- chk.pop.forest.on.land + chk.pop.pasture.on.land
##
## combining results
chk.pop <- rbind(data.frame(location = "forest", pop = chk.pop.forest.on.land),
                 data.frame(location = "pasture", pop = chk.pop.pasture.on.land),
                 data.frame(location = "stream", pop = chk.pop.in.around.stream))
## bacteria loads
chk.bac <- data.frame(chk.pop, total.bac = chk.pop$pop * chk.Bacteria.Prod)
chk.bac <- data.frame(chk.bac, accum.bac = -1)

## accum
## on forest
tmp.rows <- grep("TRUE", with(chk.bac, location == "forest"))
chk.bac[tmp.rows, "accum.bac"] <- chk.bac[tmp.rows, "total.bac"] / 
  chk.land.Forest
## on pasture
tmp.rows <- grep("TRUE", with(chk.bac, location == "pasture"))
chk.bac[tmp.rows, "accum.bac"] <- chk.bac[tmp.rows, "total.bac"] / 
  chk.land.Pasture
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

## pop in/around stream
chk.stream.pop <- data.frame(
  manual.calc.pop.total = chk.pop[chk.pop$location == "stream", "pop"],
  model.pop.total = df.output[ , "pop.total.in.stream"],
  dil = round(
    chk.dil * (df.output[ , "pop.total.in.stream"] - 
                  chk.pop[chk.pop$location == "stream", "pop"]) /
      chk.pop[chk.pop$location == "stream", "pop"],
    digits = 0))

## pop on pasture
chk.pasture.pop <- data.frame(
  manual.calc.pop.total = chk.pop[chk.pop$location == "pasture", "pop"],
  model.pop.total = df.output[ , "pop.pasture.on.land"],
  dil = round(
    chk.dil * (df.output[ , "pop.pasture.on.land"] - 
                  chk.pop[chk.pop$location == "pasture", "pop"]) /
      chk.pop[chk.pop$location == "pasture", "pop"],
    digits = 0))

## pop on forest
chk.forest.pop <- data.frame(
  manual.calc.pop.total = chk.pop[chk.pop$location == "forest", "pop"],
  model.pop.total = df.output[ , "pop.forest.on.land"],
  dil = round(
    chk.dil * (df.output[ , "pop.forest.on.land"] - 
                 chk.pop[chk.pop$location == "forest", "pop"]) /
      chk.pop[chk.pop$location == "forest", "pop"],
    digits = 0))
## all pop
chk.all.pop <- rbind(chk.total.pop, chk.stream.pop, chk.forest.pop, 
                     chk.pasture.pop)
chk.all.pop <- data.frame(cat = c("total", "stream", "forest", "pasture"), 
                          chk.all.pop)
## bacteria loads total and by locations
## total
chk.total.bac <- data.frame(
  manual.calc.bac.total = sum(chk.bac$total.bac),
  model.bac.total = df.output$bac.total,
  dil = round(
    chk.dil * (df.output$bac.total - sum(chk.bac$total.bac)) /
      sum(chk.bac$total.bac),
    digits = 0))

## bac in/around stream
chk.stream.bac <- data.frame(
  manual.calc.bac.total = chk.bac[chk.bac$location == "stream", "total.bac"],
  model.bac.total = df.output[ , "bac.total.in.stream"],
  dil = round(
    chk.dil * (df.output[ , "bac.total.in.stream"] - 
                  chk.bac[chk.bac$location == "stream", "total.bac"]) /
      chk.bac[chk.bac$location == "stream", "total.bac"],
    digits = 0))

## bac load in forest
chk.forest.bac <- data.frame(
  manual.calc.bac.total = chk.bac[chk.bac$location == "forest", "total.bac"],
  model.bac.total = df.output[ , "bac.forest.on.land"],
  dil = round(
    chk.dil * (df.output[ , "bac.forest.on.land"] - 
                 chk.bac[chk.bac$location == "forest", "total.bac"]) /
      chk.bac[chk.bac$location == "forest", "total.bac"],
    digits = 0))

## bac load on pasture
chk.pasture.bac <- data.frame(
  manual.calc.bac.total = chk.bac[chk.bac$location == "pasture", "total.bac"],
  model.bac.total = df.output[ , "bac.pasture.on.land"],
  dil = round(
    chk.dil * (df.output[ , "bac.pasture.on.land"] - 
                 chk.bac[chk.bac$location == "pasture", "total.bac"]) /
      chk.bac[chk.bac$location == "pasture", "total.bac"],
    digits = 0))

## all bac
chk.all.bac <- rbind(chk.total.bac, chk.stream.bac, chk.forest.bac, 
                     chk.pasture.bac)
chk.all.bac <- data.frame(cat = c("total", "stream", "forest", "pasture"), 
                          chk.all.bac)

## accum loads
## accum load in forest
chk.forest.accum <- data.frame(
  manual.calc.bac.total = chk.bac[chk.bac$location == "forest", "accum.bac"],
  model.bac.total = df.output[ , "Accum.Forest"],
  dil = round(
    chk.dil * (df.output[ , "Accum.Forest"] - 
      chk.bac[chk.bac$location == "forest", "accum.bac"]) /
      chk.bac[chk.bac$location == "forest", "accum.bac"],
    digits = 0))

## accum load on pasture
chk.pasture.accum <- data.frame(
  manual.calc.bac.total = chk.bac[chk.bac$location == "pasture", "accum.bac"],
  model.bac.total = df.output[ , "Accum.Pasture"],
  dil = round(
    chk.dil * (df.output[ , "Accum.Pasture"] - 
                 chk.bac[chk.bac$location == "pasture", "accum.bac"]) /
      chk.bac[chk.bac$location == "pasture", "accum.bac"],
    digits = 0))

## all accum
chk.all.accum <- rbind(chk.forest.accum, 
                     chk.pasture.accum)
chk.all.accum <- data.frame(cat = c("forest", "pasture"), chk.all.accum)

## output results in tables to pdf
pdf(file = paste0(chr.wildlife.otter.dir, "/otter-bacteria-model-calc-check-",
                  gsub("\\.txt","-",chr.input) 
                  ,strftime(Sys.time(), format = "%Y%m%d%H%M"),
                  ".pdf"), height = 8.5, width = 11, onefile = TRUE)
## population
tmp.table <- tableGrob(chk.all.pop, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Otter Population (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))

## bac load
tmp.table <- tableGrob(chk.all.bac, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Bacteria loads from Otter (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))

## accum load
tmp.table <- tableGrob(chk.all.accum, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Accum loads from Otter (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
rm(list = ls(pattern = "tmp\\.*"))

## close the pdf file
dev.off()
