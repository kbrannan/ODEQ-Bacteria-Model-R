## step bt step calculation check for wildlife_duck_sub_model using input from
## wildlifeDuck01.txt file
chr.wildlife.duck.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-Duck"
chr.input <- "wildlifeDuck01.txt"
## file for model
chr.input.file <- paste0(chr.wildlife.duck.dir, "/", chr.input)
## run model
source(paste0(chr.wildlife.duck.dir,"/Wildlife_Duck_Sub_Model.R"))
df.output <- wildlifeDuck(chr.input.file)
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
chk.bacteria.prod <- 8.0000000E+08
### two seasons
chk.season.1.Months <- c(11,12,1,5,6,7)
chk.season.2.Months <- c(2,3,4,8,9,10)
### Animal Densities
chk.Animal.Density.season.1 <- 2.50E-02
chk.Animal.Density.season.2 <- 2.50E-04
### Habitats
chk.land.pasture <-    323.06
chk.land.forest  <-    2030.6
chk.land.RAOCUT    <-    168.44
chk.habitat <- chk.land.pasture + chk.land.forest + chk.land.RAOCUT
### All Landuse has Stream access for ducks
chk.land.stream.w.access <- 100
### same percent of ducks in and around streams for all landuse
chk.in.and.around.streams <- 9.9999900E+01
## calculations
## land use stream access
## without stream access
chk.land.wo.stream.access.habitat <- (1 - chk.land.stream.w.access / 100) * chk.habitat
chk.land.wo.stream.access.pasture <- (1 - chk.land.stream.w.access / 100) * chk.land.pasture
chk.land.wo.stream.access.forest <-  (1 - chk.land.stream.w.access / 100) * chk.land.forest
chk.land.wo.stream.access.RAOCUT <-  (1 - chk.land.stream.w.access / 100) * chk.land.RAOCUT
## with stream access
chk.land.w.stream.access.habitat <- (chk.land.stream.w.access / 100) * chk.habitat
chk.land.w.stream.access.pasture <- (chk.land.stream.w.access / 100) * chk.land.pasture
chk.land.w.stream.access.forest <-  (chk.land.stream.w.access / 100) * chk.land.forest
chk.land.w.stream.access.RAOCUT <-  (chk.land.stream.w.access / 100) * chk.land.RAOCUT
## animal numbers
## season 1
## on land with out stream access
chk.pop.on.land.wo.stream.access.habitat.season.1 <- chk.Animal.Density.season.1 * chk.land.wo.stream.access.habitat
chk.pop.on.land.wo.stream.access.pasture.season.1 <- chk.Animal.Density.season.1 * chk.land.wo.stream.access.pasture
chk.pop.on.land.wo.stream.access.forest.season.1  <- chk.Animal.Density.season.1 * chk.land.wo.stream.access.forest
chk.pop.on.land.wo.stream.access.RAOCUT.season.1  <- chk.Animal.Density.season.1 * chk.land.wo.stream.access.RAOCUT
## on land with stream access
chk.pop.on.land.w.stream.access.habitat.season.1 <- chk.Animal.Density.season.1 * (1 - chk.in.and.around.streams / 100) * chk.land.w.stream.access.habitat
chk.pop.on.land.w.stream.access.pasture.season.1 <- chk.Animal.Density.season.1 * (1 - chk.in.and.around.streams / 100) * chk.land.w.stream.access.pasture
chk.pop.on.land.w.stream.access.forest.season.1  <- chk.Animal.Density.season.1 * (1 - chk.in.and.around.streams / 100) * chk.land.w.stream.access.forest
chk.pop.on.land.w.stream.access.RAOCUT.season.1  <- chk.Animal.Density.season.1 * (1 - chk.in.and.around.streams / 100) * chk.land.w.stream.access.RAOCUT
## in stream
chk.pop.in.stream.habitat.season.1 <- chk.Animal.Density.season.1 * (chk.in.and.around.streams / 100) * chk.land.w.stream.access.habitat
chk.pop.in.stream.pasture.season.1 <- chk.Animal.Density.season.1 * (chk.in.and.around.streams / 100) * chk.land.w.stream.access.pasture
chk.pop.in.stream.forest.season.1  <- chk.Animal.Density.season.1 * (chk.in.and.around.streams / 100) * chk.land.w.stream.access.forest
chk.pop.in.stream.RAOCUT.season.1  <- chk.Animal.Density.season.1 * (chk.in.and.around.streams / 100) * chk.land.w.stream.access.RAOCUT
## on land
chk.pop.on.land.habitat.season.1 <- chk.pop.on.land.wo.stream.access.habitat.season.1 + chk.pop.on.land.w.stream.access.habitat.season.1
chk.pop.on.land.pasture.season.1 <- chk.pop.on.land.wo.stream.access.pasture.season.1 + chk.pop.on.land.w.stream.access.pasture.season.1
chk.pop.on.land.forest.season.1  <- chk.pop.on.land.wo.stream.access.forest.season.1  + chk.pop.on.land.w.stream.access.forest.season.1 
chk.pop.on.land.RAOCUT.season.1  <- chk.pop.on.land.wo.stream.access.RAOCUT.season.1  + chk.pop.on.land.w.stream.access.RAOCUT.season.1 
## season 2
## on land with out stream access
chk.pop.on.land.wo.stream.access.habitat.season.2 <- chk.Animal.Density.season.2 * chk.land.wo.stream.access.habitat
chk.pop.on.land.wo.stream.access.pasture.season.2 <- chk.Animal.Density.season.2 * chk.land.wo.stream.access.pasture
chk.pop.on.land.wo.stream.access.forest.season.2  <- chk.Animal.Density.season.2 * chk.land.wo.stream.access.forest
chk.pop.on.land.wo.stream.access.RAOCUT.season.2  <- chk.Animal.Density.season.2 * chk.land.wo.stream.access.RAOCUT
## on land with stream access
chk.pop.on.land.w.stream.access.habitat.season.2 <- chk.Animal.Density.season.2 * (1 - chk.in.and.around.streams / 100) * chk.land.w.stream.access.habitat
chk.pop.on.land.w.stream.access.pasture.season.2 <- chk.Animal.Density.season.2 * (1 - chk.in.and.around.streams / 100) * chk.land.w.stream.access.pasture
chk.pop.on.land.w.stream.access.forest.season.2  <- chk.Animal.Density.season.2 * (1 - chk.in.and.around.streams / 100) * chk.land.w.stream.access.forest
chk.pop.on.land.w.stream.access.RAOCUT.season.2  <- chk.Animal.Density.season.2 * (1 - chk.in.and.around.streams / 100) * chk.land.w.stream.access.RAOCUT
## in stream
chk.pop.in.stream.habitat.season.2 <- chk.Animal.Density.season.2 * (chk.in.and.around.streams / 100) * chk.land.w.stream.access.habitat
chk.pop.in.stream.pasture.season.2 <- chk.Animal.Density.season.2 * (chk.in.and.around.streams / 100) * chk.land.w.stream.access.pasture
chk.pop.in.stream.forest.season.2  <- chk.Animal.Density.season.2 * (chk.in.and.around.streams / 100) * chk.land.w.stream.access.forest
chk.pop.in.stream.RAOCUT.season.2  <- chk.Animal.Density.season.2 * (chk.in.and.around.streams / 100) * chk.land.w.stream.access.RAOCUT
## on land
chk.pop.on.land.habitat.season.2 <- chk.pop.on.land.wo.stream.access.habitat.season.2 + chk.pop.on.land.w.stream.access.habitat.season.2
chk.pop.on.land.pasture.season.2 <- chk.pop.on.land.wo.stream.access.pasture.season.2 + chk.pop.on.land.w.stream.access.pasture.season.2
chk.pop.on.land.forest.season.2  <- chk.pop.on.land.wo.stream.access.forest.season.2  + chk.pop.on.land.w.stream.access.forest.season.2 
chk.pop.on.land.RAOCUT.season.2  <- chk.pop.on.land.wo.stream.access.RAOCUT.season.2  + chk.pop.on.land.w.stream.access.RAOCUT.season.2 
## bacteria loads
## season 1
## on land with out stream access
chk.bac.on.land.wo.stream.access.habitat.season.1 <- chk.bacteria.prod * chk.pop.on.land.wo.stream.access.habitat.season.1
chk.bac.on.land.wo.stream.access.pasture.season.1 <- chk.bacteria.prod * chk.pop.on.land.wo.stream.access.pasture.season.1
chk.bac.on.land.wo.stream.access.forest.season.1  <- chk.bacteria.prod * chk.pop.on.land.wo.stream.access.forest.season.1 
chk.bac.on.land.wo.stream.access.RAOCUT.season.1  <- chk.bacteria.prod * chk.pop.on.land.wo.stream.access.RAOCUT.season.1 
## on land with stream access
chk.bac.on.land.w.stream.access.habitat.season.1 <- chk.bacteria.prod * chk.pop.on.land.w.stream.access.habitat.season.1
chk.bac.on.land.w.stream.access.pasture.season.1 <- chk.bacteria.prod * chk.pop.on.land.w.stream.access.pasture.season.1
chk.bac.on.land.w.stream.access.forest.season.1  <- chk.bacteria.prod * chk.pop.on.land.w.stream.access.forest.season.1 
chk.bac.on.land.w.stream.access.RAOCUT.season.1  <- chk.bacteria.prod * chk.pop.on.land.w.stream.access.RAOCUT.season.1 
## in stream
chk.bac.in.stream.habitat.season.1 <- chk.bacteria.prod * chk.pop.in.stream.habitat.season.1
chk.bac.in.stream.pasture.season.1 <- chk.bacteria.prod * chk.pop.in.stream.pasture.season.1
chk.bac.in.stream.forest.season.1  <- chk.bacteria.prod * chk.pop.in.stream.forest.season.1 
chk.bac.in.stream.RAOCUT.season.1  <- chk.bacteria.prod * chk.pop.in.stream.RAOCUT.season.1 
## on land
chk.bac.on.land.habitat.season.1 <- chk.bac.on.land.wo.stream.access.habitat.season.1 + chk.bac.on.land.w.stream.access.habitat.season.1
chk.bac.on.land.pasture.season.1 <- chk.bac.on.land.wo.stream.access.pasture.season.1 + chk.bac.on.land.w.stream.access.pasture.season.1
chk.bac.on.land.forest.season.1  <- chk.bac.on.land.wo.stream.access.forest.season.1  + chk.bac.on.land.w.stream.access.forest.season.1 
chk.bac.on.land.RAOCUT.season.1  <- chk.bac.on.land.wo.stream.access.RAOCUT.season.1  + chk.bac.on.land.w.stream.access.RAOCUT.season.1 
## accum loads
chk.bac.accum.pasture.season.1 <- chk.bac.on.land.pasture.season.1 / chk.land.pasture
chk.bac.accum.forest.season.1  <- chk.bac.on.land.forest.season.1  / chk.land.forest
chk.bac.accum.RAOCUT.season.1  <- chk.bac.on.land.RAOCUT.season.1  / chk.land.RAOCUT
## season 2
## on land with out stream access
chk.bac.on.land.wo.stream.access.habitat.season.2 <- chk.bacteria.prod * chk.pop.on.land.wo.stream.access.habitat.season.2
chk.bac.on.land.wo.stream.access.pasture.season.2 <- chk.bacteria.prod * chk.pop.on.land.wo.stream.access.pasture.season.2
chk.bac.on.land.wo.stream.access.forest.season.2  <- chk.bacteria.prod * chk.pop.on.land.wo.stream.access.forest.season.2 
chk.bac.on.land.wo.stream.access.RAOCUT.season.2  <- chk.bacteria.prod * chk.pop.on.land.wo.stream.access.RAOCUT.season.2 
## on land with stream access
chk.bac.on.land.w.stream.access.habitat.season.2 <- chk.bacteria.prod * chk.pop.on.land.w.stream.access.habitat.season.2
chk.bac.on.land.w.stream.access.pasture.season.2 <- chk.bacteria.prod * chk.pop.on.land.w.stream.access.pasture.season.2
chk.bac.on.land.w.stream.access.forest.season.2  <- chk.bacteria.prod * chk.pop.on.land.w.stream.access.forest.season.2 
chk.bac.on.land.w.stream.access.RAOCUT.season.2  <- chk.bacteria.prod * chk.pop.on.land.w.stream.access.RAOCUT.season.2 
## in stream
chk.bac.in.stream.habitat.season.2 <- chk.bacteria.prod * chk.pop.in.stream.habitat.season.2
chk.bac.in.stream.pasture.season.2 <- chk.bacteria.prod * chk.pop.in.stream.pasture.season.2
chk.bac.in.stream.forest.season.2  <- chk.bacteria.prod * chk.pop.in.stream.forest.season.2 
chk.bac.in.stream.RAOCUT.season.2  <- chk.bacteria.prod * chk.pop.in.stream.RAOCUT.season.2 
## on land
chk.bac.on.land.habitat.season.2 <- chk.bac.on.land.wo.stream.access.habitat.season.2 + chk.bac.on.land.w.stream.access.habitat.season.2
chk.bac.on.land.pasture.season.2 <- chk.bac.on.land.wo.stream.access.pasture.season.2 + chk.bac.on.land.w.stream.access.pasture.season.2
chk.bac.on.land.forest.season.2  <- chk.bac.on.land.wo.stream.access.forest.season.2  + chk.bac.on.land.w.stream.access.forest.season.2 
chk.bac.on.land.RAOCUT.season.2  <- chk.bac.on.land.wo.stream.access.RAOCUT.season.2  + chk.bac.on.land.w.stream.access.RAOCUT.season.2 
## accum loads
chk.bac.accum.pasture.season.2 <- chk.bac.on.land.pasture.season.2 / chk.land.pasture
chk.bac.accum.forest.season.2  <- chk.bac.on.land.forest.season.2  / chk.land.forest
chk.bac.accum.RAOCUT.season.2  <- chk.bac.on.land.RAOCUT.season.2  / chk.land.RAOCUT
##
## combining results
chk.output.season.1 <- data.frame(
  Month=format(as.POSIXct(paste0("1967-",chk.season.1.Months,"-01")), format = "%b"),
  pop.total=chk.pop.on.land.habitat.season.1 + chk.pop.in.stream.habitat.season.1,
  pop.on.land=chk.pop.on.land.habitat.season.1,
  pop.in.stream=chk.pop.in.stream.habitat.season.1,
  Bacteria.total=chk.bac.on.land.habitat.season.1 + chk.bac.in.stream.habitat.season.1,
  Bacteria.on.land=chk.bac.on.land.habitat.season.1,
  Bacteria.in.stream=chk.bac.in.stream.habitat.season.1,
  Accum.pasture=chk.bac.accum.pasture.season.1,
  Accum.forest=chk.bac.accum.forest.season.1,
  Accum.RAOCUT=chk.bac.accum.RAOCUT.season.1,
  Lim.pasture=chk.sqolim * chk.bac.accum.pasture.season.1,
  Lim.forest=chk.sqolim * chk.bac.accum.forest.season.1,
  Lim.RAOCUT=chk.sqolim * chk.bac.accum.RAOCUT.season.1,
  month.order=chk.season.1.Months,
  stringsAsFactors=FALSE)
chk.output.season.2 <- data.frame(
  Month=format(as.POSIXct(paste0("1967-",chk.season.2.Months,"-01")), format = "%b"),
  pop.total=chk.pop.on.land.habitat.season.2 + chk.pop.in.stream.habitat.season.2,
  pop.on.land=chk.pop.on.land.habitat.season.2,
  pop.in.stream=chk.pop.in.stream.habitat.season.2,
  Bacteria.total=chk.bac.on.land.habitat.season.2 + chk.bac.in.stream.habitat.season.2,
  Bacteria.on.land=chk.bac.on.land.habitat.season.2,
  Bacteria.in.stream=chk.bac.in.stream.habitat.season.2,
  Accum.pasture=chk.bac.accum.pasture.season.2,
  Accum.forest=chk.bac.accum.forest.season.2,
  Accum.RAOCUT=chk.bac.accum.RAOCUT.season.2,
  Lim.pasture=chk.sqolim * chk.bac.accum.pasture.season.2,
  Lim.forest=chk.sqolim * chk.bac.accum.forest.season.2,
  Lim.RAOCUT=chk.sqolim * chk.bac.accum.RAOCUT.season.2,
  month.order=chk.season.2.Months,
  stringsAsFactors=FALSE)
chk.output <- rbind(chk.output.season.1, chk.output.season.2)
chk.output <- chk.output[order(chk.output$month.order), ]
df.chk <- chk.output[, -1 * grep("month.order", names(chk.output))]
rm(chk.output)
## compare
df.comp <- data.frame(
  Month=format(as.POSIXct(paste0("1967-",1:12,"-01")), format = "%b"),
  df.output[, -1] - df.chk[, -1])
##
## check model output
chk.dil <- 1E+06 # need to explain this
## output results in tables to pdf
pdf(file = paste0(chr.wildlife.duck.dir, "/duck-bacteria-model-calc-check-",
                  gsub("\\.txt","-",chr.input) 
                  ,strftime(Sys.time(), format = "%Y%m%d%H%M"),
                  ".pdf"), height = 8.5, width = 11, onefile = TRUE)
## total population
tmp.gt <- table.grob(chr.col = "pop.total", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Total number of duck (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population on land
tmp.gt <- table.grob(chr.col = "pop.on.land", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Number of duck on land (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population in stream
tmp.gt <- table.grob(chr.col = "pop.in.stream", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Number of duck in stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Total bacteria load
tmp.gt <- table.grob(chr.col = "Bacteria.total", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Total bacteria load from duck (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## bacteria load on land
tmp.gt <- table.grob(chr.col = "Bacteria.on.land", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load on land from duck (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## bacteria load to stream
tmp.gt <- table.grob(chr.col = "Bacteria.in.stream", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to stream from duck (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Accum bacteria load to pasture
tmp.gt <- table.grob(chr.col = "Accum.pasture", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to land as accum for pasture from duck (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Accum bacteria load to forest
tmp.gt <- table.grob(chr.col = "Accum.forest", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to land as accum for forest from duck (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Accum bacteria load to RAOCUT
tmp.gt <- table.grob(chr.col = "Accum.RAOCUT", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to land as accum for RAOCUT from duck (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Lim bacteria load for pasture
tmp.gt <- table.grob(chr.col = "Lim.pasture", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load limit to land as accum for pasture from duck (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Lim bacteria load for forest
tmp.gt <- table.grob(chr.col = "Lim.forest", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load limit to land as accum for forest from duck (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Lim bacteria load for RAOCUT
tmp.gt <- table.grob(chr.col = "Lim.RAOCUT", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load limit to land as accum for RAOCUT from duck (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
rm(list = ls(pattern = "tmp\\.*"))
## close the pdf file
dev.off()
