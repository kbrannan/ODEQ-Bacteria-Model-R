## step bt step calculation check for wildlife_coyote_sub_model using input from
## wildlifecoyote14.txt file
chr.wildlife.coyote.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-coyote"
chr.input <- "wildlifecoyote14.txt"
source(paste0(chr.wildlife.coyote.dir,"/Wildlife_Coyote_Sub_Model.R"))
df.output <- wildlifeCoyote(chr.wrkdir=chr.wildlife.coyote.dir,chr.input=chr.input)
## packages
library(doBy, quietly = TRUE)
library(gridExtra, quietly = TRUE)
##
## get input
## SQOLIM multiplcation factor: 
chk.sqolim <- 9
chk.Bacteria.Prod <- 9.0000115E+08
### Animal Densities
chk.Animal.Density <- 3.6063000E-02
### habitat
chk.land.Forest   <- 1367.68
chk.land.Pasture <- 193.04
chk.land.RAOCUT <- 73.50
chk.land.Total <- chk.land.Forest + chk.land.Pasture + chk.land.RAOCUT
### Percent of Landuse with Stream access
chk.Percent.habitat.with.Stream.Access <- 100
### percent of animals in/around streams
chk.in.and.around.streams <- 1.0000000E+00
## calculations
## animal numbers
chk.pop.total <- chk.Animal.Density *   chk.land.Total
## with/without stream access
chk.pop.wo.stream.access <- chk.pop.total * (1 - chk.Percent.habitat.with.Stream.Access / 100)
chk.pop.w.stream.access <- chk.pop.total * (chk.Percent.habitat.with.Stream.Access / 100)
## with stream access on in/around atream
chk.pop.in.around.stream <- chk.pop.w.stream.access * (chk.in.and.around.streams / 100)
## coyote on land
chk.pop.on.land <- chk.pop.wo.stream.access +
  chk.pop.w.stream.access * (1 - chk.in.and.around.streams / 100)
chk.pop.on.land.Forest <- chk.pop.on.land * (chk.land.Forest / chk.land.Total)
chk.pop.on.land.Pasture <- chk.pop.on.land * (chk.land.Pasture / chk.land.Total)
chk.pop.on.land.RAOCUT <- chk.pop.on.land * (chk.land.RAOCUT / chk.land.Total)

##
## combining results
chk.pop <- rbind(data.frame(location = "forest", pop = chk.pop.on.land.Forest),
                 data.frame(location = "pasture", pop = chk.pop.on.land.Pasture),
                 data.frame(location = "RAOCUT", pop = chk.pop.on.land.RAOCUT),
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
## on RAOCUT
tmp.rows <- grep("TRUE", with(chk.bac, location == "RAOCUT"))
chk.bac[tmp.rows, "accum.bac"] <- chk.bac[tmp.rows, "total.bac"] / 
  chk.land.RAOCUT
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
  model.pop.total = df.output[ , "pop.total.on.Pasture"],
  dil = round(
    chk.dil * (df.output[ , "pop.total.on.Pasture"] - 
                  chk.pop[chk.pop$location == "pasture", "pop"]) /
      chk.pop[chk.pop$location == "pasture", "pop"],
    digits = 0))

## pop on forest
chk.forest.pop <- data.frame(
  manual.calc.pop.total = chk.pop[chk.pop$location == "forest", "pop"],
  model.pop.total = df.output[ , "pop.total.in.Forest"],
  dil = round(
    chk.dil * (df.output[ , "pop.total.in.Forest"] - 
                 chk.pop[chk.pop$location == "forest", "pop"]) /
      chk.pop[chk.pop$location == "forest", "pop"],
    digits = 0))
## pop on RAOCUT
chk.RAOCUT.pop <- data.frame(
  manual.calc.pop.total = chk.pop[chk.pop$location == "RAOCUT", "pop"],
  model.pop.total = df.output[ , "pop.total.on.RAOCUT"],
  dil = round(
    chk.dil * (df.output[ , "pop.total.on.RAOCUT"] - 
                 chk.pop[chk.pop$location == "RAOCUT", "pop"]) /
      chk.pop[chk.pop$location == "RAOCUT", "pop"],
    digits = 0))
## all pop
chk.all.pop <- rbind(chk.total.pop, chk.stream.pop, chk.forest.pop, 
                     chk.pasture.pop, chk.RAOCUT.pop)
chk.all.pop <- data.frame(cat = c("total", "stream", "forest", "pasture", 
                                  "RAOCUT"), chk.all.pop)
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

## bac on RAOCUT
chk.RAOCUT.bac <- data.frame(
  manual.calc.bac.total = chk.bac[chk.bac$location == "RAOCUT", "total.bac"],
  model.bac.total = df.output[ , "bac.RAOCUT.on.land"],
  dil = round(
    chk.dil * (df.output[ , "bac.RAOCUT.on.land"] - 
                 chk.bac[chk.bac$location == "RAOCUT", "total.bac"]) /
      chk.bac[chk.bac$location == "RAOCUT", "total.bac"],
    digits = 0))

## all bac
chk.all.bac <- rbind(chk.total.bac, chk.stream.bac, chk.forest.bac, 
                     chk.pasture.bac, chk.RAOCUT.bac)
chk.all.bac <- data.frame(cat = c("total", "stream", "forest", "pasture", 
                                  "RAOCUT"), chk.all.bac)

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

## accum load on RAOCUT
chk.RAOCUT.accum <- data.frame(
  manual.calc.bac.total = chk.bac[chk.bac$location == "RAOCUT", "accum.bac"],
  model.bac.total = df.output[ , "Accum.RAOCUT"],
  dil = round(
    chk.dil * (df.output[ , "Accum.RAOCUT"] - 
                 chk.bac[chk.bac$location == "RAOCUT", "accum.bac"]) /
      chk.bac[chk.bac$location == "RAOCUT", "accum.bac"],
    digits = 0))

## all accum
chk.all.accum <- rbind(chk.forest.accum, 
                     chk.pasture.accum, chk.RAOCUT.accum)
chk.all.accum <- data.frame(cat = c("forest", "pasture", 
                                  "RAOCUT"), chk.all.accum)

## output results in tables to pdf
pdf(file = paste0(chr.wildlife.coyote.dir, "/coyote-bacteria-model-calc-check-",
                  gsub("\\.txt","-",chr.input) 
                  ,strftime(Sys.time(), format = "%Y%m%d%H%M"),
                  ".pdf"), height = 8.5, width = 11, onefile = TRUE)
## population
tmp.table <- tableGrob(chk.all.pop, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Coyote Population (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Bacteria loads from Coyote (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Accum loads from Coyote (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
rm(list = ls(pattern = "tmp\\.*"))

## close the pdf file
dev.off()
