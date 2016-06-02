## step bt step calculation check for wildlife_racoon_sub_model using input from
## wildlifeRacoon17.txt file
chr.wildlife.racoon.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-racoon"
chr.input <- "wildlifeRacoon17.txt"
## file for model
chr.input.file <- paste0(chr.wildlife.racoon.dir, "/", chr.input)
## run model
source(paste0(chr.wildlife.racoon.dir,"/Wildlife_Racoon_Sub_Model.R"))
df.output <- wildlifeRacoon(chr.input.file)
## packages
library(doBy, quietly = TRUE)
library(gridExtra, quietly = TRUE)
## function for creating tables for output of results
table.grob <- function(chr.col, df.output = df.output,
                       df.output.chk = df.outout.chk,
                       df.comp = df.comp, chr.title = NULL, chk.dil = 1E+06) {
  tmp.mod <- eval(parse(text = paste0("df.output$", chr.col)))
  tmp.man <- eval(parse(text = paste0("df.output.chk$", chr.col)))
  tmp.com <- eval(parse(text = paste0("df.comp$", chr.col)))
  tmp.df <- data.frame(Month = df.output$Month,
                       Manual = tmp.man,
                       Model = tmp.mod, 
                       dil = round(chk.dil * tmp.com/tmp.man, digits = 0))
  tmp.table <- tableGrob(tmp.df, show.rownames = FALSE)
  tmp.h <- grobHeight(tmp.table)
  tmp.w <- grobWidth(tmp.table)
  tmp.title <- textGrob(label = chr.title,
                        y=unit(0.5,"npc") + 0.5*tmp.h, 
                        vjust=0, gp=gpar(fontsize=20))
  tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
}
##
## get input
## SQOLIM multiplcation factor: 
chk.sqolim <- 9
chk.Bacteria.Prod <- 4.7500000E+07
### Animal Densities
chk.Animal.Density <- 4.6839755E-03
### Habitats
chk.land.pasture <-    197.5
chk.land.forest <-    828
chk.land.RAOCUT <-    5.9
chk.habitat <- chk.land.pasture + chk.land.forest + chk.land.RAOCUT
### percent of animals in/around streams
chk.in.and.around.streams <- 5.5526555E+00
## calculations
## animal numbers
chk.pop.total <- chk.Animal.Density *   chk.habitat
chk.pop.pasture <- chk.Animal.Density *   chk.land.pasture
chk.pop.forest <- chk.Animal.Density *   chk.land.forest
chk.pop.RAOCUT <- chk.Animal.Density *   chk.land.RAOCUT
## with stream access on in/around atream
chk.pop.in.around.stream <- chk.pop.total * (chk.in.and.around.streams / 100)
chk.pop.in.around.stream.pasture <- chk.pop.pasture * (chk.in.and.around.streams / 100)
chk.pop.in.around.stream.forest <- chk.pop.forest * (chk.in.and.around.streams / 100)
chk.pop.in.around.stream.RAOCUT <- chk.pop.RAOCUT * (chk.in.and.around.streams / 100)
## Racoon on land
chk.pop.on.land <- chk.pop.total - chk.pop.in.around.stream
chk.pop.on.land.pasture <- chk.pop.pasture - chk.pop.in.around.stream.pasture
chk.pop.on.land.forest <- chk.pop.forest - chk.pop.in.around.stream.forest
chk.pop.on.land.RAOCUT <- chk.pop.RAOCUT - chk.pop.in.around.stream.RAOCUT
## bacteria loads
chk.bac.total <- chk.pop.total * chk.Bacteria.Prod
chk.bac.pasture <- chk.pop.pasture * chk.Bacteria.Prod
chk.bac.forest <- chk.pop.forest * chk.Bacteria.Prod
chk.bac.RAOCUT <- chk.pop.RAOCUT * chk.Bacteria.Prod
## with stream access on in/around atream
chk.bac.in.around.stream <- chk.pop.in.around.stream * chk.Bacteria.Prod
## Racoon on land
chk.bac.on.land <- chk.pop.on.land * chk.Bacteria.Prod
chk.bac.on.land.pasture <- chk.pop.on.land.pasture * chk.Bacteria.Prod
chk.bac.on.land.forest <- chk.pop.on.land.forest * chk.Bacteria.Prod
chk.bac.on.land.RAOCUT <- chk.pop.on.land.RAOCUT * chk.Bacteria.Prod
## accum
chk.accum.pasture <- chk.bac.on.land.pasture / chk.land.pasture
chk.accum.forest <- chk.bac.on.land.forest / chk.land.forest
chk.accum.RAOCUT <- chk.bac.on.land.RAOCUT / chk.land.RAOCUT
## lim
chk.lim.pasture <- chk.accum.pasture * chk.sqolim
chk.lim.forest <- chk.accum.forest * chk.sqolim
chk.lim.RAOCUT <- chk.accum.RAOCUT * chk.sqolim
## put together
df.chk <- data.frame(
  Month=format(as.POSIXct(paste0("1967-",1:12,"-01")), format = "%b"),
  pop.total=chk.pop.total,
  pop.on.land=chk.pop.on.land,
  pop.in.stream=chk.pop.in.around.stream,
  Bacteria.total=chk.bac.total,
  Bacteria.on.land=chk.bac.on.land,
  Bacteria.in.stream=chk.bac.in.around.stream,
  Accum.pasture=chk.accum.pasture,
  Accum.forest=chk.accum.forest,
  Accum.RAOCUT=chk.accum.RAOCUT,
  Lim.pasture=chk.lim.pasture,
  Lim.forest=chk.lim.forest,
  Lim.RAOCUT=chk.lim.RAOCUT,
  stringsAsFactors=FALSE)
## compare
df.comp <- data.frame(
  Month=format(as.POSIXct(paste0("1967-",1:12,"-01")), format = "%b"),
  df.output[, -1] - df.chk[, -1])
##
## check model output
chk.dil <- 1E+06 # need to explain this
## output results in tables to pdf
pdf(file = paste0(chr.wildlife.racoon.dir, "/racoon-bacteria-model-calc-check-",
                  gsub("\\.txt","-",chr.input) 
                  ,strftime(Sys.time(), format = "%Y%m%d%H%M"),
                  ".pdf"), height = 8.5, width = 11, onefile = TRUE)
## total population
tmp.gt <- table.grob(chr.col = "pop.total", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Total number of racoons (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population on land
tmp.gt <- table.grob(chr.col = "pop.on.land", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Number of racoons on land (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population in stream
tmp.gt <- table.grob(chr.col = "pop.in.stream", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Number of racoons in stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Total bacteria load
tmp.gt <- table.grob(chr.col = "Bacteria.total", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Total bacteria load from racoons (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## bacteria load on land
tmp.gt <- table.grob(chr.col = "Bacteria.on.land", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load on land from racoons (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## bacteria load to stream
tmp.gt <- table.grob(chr.col = "Bacteria.in.stream", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to stream from racoons (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Accum bacteria load to pasture
tmp.gt <- table.grob(chr.col = "Accum.pasture", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to land as accum for pasture from racoons (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Accum bacteria load to forest
tmp.gt <- table.grob(chr.col = "Accum.forest", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to land as accum for forest from racoons (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Accum bacteria load to RAOCUT
tmp.gt <- table.grob(chr.col = "Accum.RAOCUT", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to land as accum for RAOCUT from racoons (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Lim bacteria load for pasture
tmp.gt <- table.grob(chr.col = "Lim.pasture", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load limit to land as accum for pasture from racoons (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Lim bacteria load for forest
tmp.gt <- table.grob(chr.col = "Lim.forest", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load limit to land as accum for forest from racoons (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Lim bacteria load for RAOCUT
tmp.gt <- table.grob(chr.col = "Lim.RAOCUT", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load limit to land as accum for RAOCUT from racoons (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
rm(list = ls(pattern = "tmp\\.*"))
## close the pdf file
dev.off()
