## step bt step calculation check for wildlife_beaver_sub_model using input from
## wildlifeBeaver10.txt file
chr.wildlife.beaver.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-beaver"
chr.input <- "wildlifebeaver10.txt"
source(paste0(chr.wildlife.beaver.dir,"/Wildlife_Beaver_Sub_Model.R"))
df.output <- wildlifeBeaver(chr.wrkdir=chr.wildlife.beaver.dir,chr.input=chr.input)
## packages
library(doBy, quietly = TRUE)
library(gridExtra, quietly = TRUE)
##
## get input
## SQOLIM multiplcation factor: 
chk.sqolim <- 9
chk.Bacteria.Prod <- 1.9665000E+05
### Animal Densities
chk.Animal.Density <- 7.5000000E-03
### Habitats
chk.land.Forest   <- 43.49
chk.land.Total <- chk.land.Forest
chk.habitat <- chk.land.Forest
### Percent of Landuse with Stream access
chk.Percent.habitat.with.Stream.Access <- 100
### percent of animals in/around streams
chk.in.and.around.streams <- 3.1245634E+01
## calculations
## animal numbers
chk.pop.total <- chk.Animal.Density *   chk.habitat
## with/without stream access
chk.pop.wo.stream.access <- chk.pop.total * (1 - chk.Percent.habitat.with.Stream.Access / 100)
chk.pop.w.stream.access <- chk.pop.total * (chk.Percent.habitat.with.Stream.Access / 100)
## with stream access on in/around atream
chk.pop.in.around.stream <- chk.pop.w.stream.access * (chk.in.and.around.streams / 100)
## Beaver on land
chk.pop.on.land <- chk.pop.wo.stream.access +
  chk.pop.w.stream.access * (1 - chk.in.and.around.streams / 100)
chk.pop.on.land.Forest <- chk.pop.on.land * (chk.land.Forest / chk.land.Total)

##
## combining results
chk.pop <- rbind(data.frame(location = "forest", pop = chk.pop.on.land.Forest),
                 data.frame(location = "stream", pop = chk.pop.in.around.stream))
## bacteria loads
chk.bac <- data.frame(chk.pop, total.bac = chk.pop$pop * chk.Bacteria.Prod)
chk.bac <- data.frame(chk.bac, accum.bac = -1)

## accum
## on forest
tmp.rows <- grep("TRUE", with(chk.bac, location == "forest"))
chk.bac[tmp.rows, "accum.bac"] <- chk.bac[tmp.rows, "total.bac"] / 
  chk.land.Forest
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

## pop on forest
chk.forest.pop <- data.frame(
  manual.calc.pop.total = chk.pop[chk.pop$location == "forest", "pop"],
  model.pop.total = df.output[ , "pop.total.in.Forest"],
  dil = round(
    chk.dil * (df.output[ , "pop.total.in.Forest"] - 
                 chk.pop[chk.pop$location == "forest", "pop"]) /
      chk.pop[chk.pop$location == "forest", "pop"],
    digits = 0))

## all pop
chk.all.pop <- rbind(chk.total.pop, chk.stream.pop, chk.forest.pop)
chk.all.pop <- data.frame(cat = c("total", "stream", "forest"), chk.all.pop)
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
  model.bac.total = df.output[ , "bac.total.in.Forest"],
  dil = round(
    chk.dil * (df.output[ , "bac.total.in.Forest"] - 
                 chk.bac[chk.bac$location == "forest", "total.bac"]) /
      chk.bac[chk.bac$location == "forest", "total.bac"],
    digits = 0))

## all bac
chk.all.bac <- rbind(chk.total.bac, chk.stream.bac, chk.forest.bac)
chk.all.bac <- data.frame(cat = c("total", "stream", "forest"), chk.all.bac)

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

## all accum
chk.all.accum <- rbind(chk.forest.accum)
chk.all.accum <- data.frame(cat = c("forest"), chk.all.accum)

## output results in tables to pdf
pdf(file = paste0(chr.wildlife.beaver.dir, "/beaver-bacteria-model-calc-check-",
                  gsub("\\.txt","-",chr.input) 
                  ,strftime(Sys.time(), format = "%Y%m%d%H%M"),
                  ".pdf"), height = 8.5, width = 11, onefile = TRUE)
## population
tmp.table <- tableGrob(chk.all.pop, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = paste0("Beaver Population (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Bacteria loads from Beaver (dil = ", sprintf("%1.0E", chk.dil), ")"),
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
tmp.title <- textGrob(label = paste0("Accum loads from Beaver (dil = ", sprintf("%1.0E", chk.dil), ")"),
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
rm(list = ls(pattern = "tmp\\.*"))

## close the pdf file
dev.off()
