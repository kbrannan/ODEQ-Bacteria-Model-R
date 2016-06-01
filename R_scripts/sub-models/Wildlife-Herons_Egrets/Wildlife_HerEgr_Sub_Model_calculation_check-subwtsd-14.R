## step bt step calculation check for wildlife_heregr_sub_model using input from
## wildlifeHerEgr14.txt file
chr.wildlife.heregr.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/Wildlife-Herons_Egrets"
chr.input <- "wildlifeHerEgr14.txt"
## file for model
chr.input.file <- paste0(chr.wildlife.heregr.dir, "/", chr.input)
## run model
source(paste0(chr.wildlife.heregr.dir,"/Wildlife_HerEgr_Sub_Model.R"))
df.output <- wildlifeHerEgr(chr.input.file)
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
chk.Bacteria.Prod <- 4.3020000E+09
### Animal Densities
chk.Animal.Density <- 1.3794000E-02
### Habitats
chk.land.pasture <-    218.1
chk.land.forest <-     2245.3
chk.land.RAOCUT <-     14.8
chk.habitat <- chk.land.pasture + chk.land.forest + chk.land.RAOCUT
chk.land.stream.w.access <- 1.0000000E+02
### percent of animals in/around streams
chk.in.and.around.streams <- 9.9000000E+01
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
## overall
chk.pop.total <-   chk.Animal.Density * chk.habitat
chk.pop.pasture <- chk.Animal.Density * chk.land.pasture
chk.pop.forest <-  chk.Animal.Density * chk.land.forest
chk.pop.RAOCUT <-  chk.Animal.Density * chk.land.RAOCUT
## on land without stream access
chk.pop.wo.stream.on.land.total <-   chk.Animal.Density * chk.land.wo.stream.access.habitat
chk.pop.wo.stream.on.land.pasture <- chk.Animal.Density * chk.land.wo.stream.access.pasture
chk.pop.wo.stream.on.land.forest <-  chk.Animal.Density * chk.land.wo.stream.access.forest
chk.pop.wo.stream.on.land.RAOCUT <-  chk.Animal.Density * chk.land.wo.stream.access.RAOCUT
## on land with stream access
chk.pop.w.stream.on.land.total <-   chk.Animal.Density * chk.land.w.stream.access.habitat * (1 - chk.in.and.around.streams / 100)
chk.pop.w.stream.on.land.pasture <- chk.Animal.Density * chk.land.w.stream.access.pasture * (1 - chk.in.and.around.streams / 100)
chk.pop.w.stream.on.land.forest <-  chk.Animal.Density * chk.land.w.stream.access.forest *  (1 - chk.in.and.around.streams / 100)
chk.pop.w.stream.on.land.RAOCUT <-  chk.Animal.Density * chk.land.w.stream.access.RAOCUT *  (1 - chk.in.and.around.streams / 100)
## pop on land 
chk.pop.on.land.total <-   chk.pop.wo.stream.on.land.total + chk.pop.w.stream.on.land.total
chk.pop.on.land.pasture <- chk.pop.wo.stream.on.land.pasture + chk.pop.w.stream.on.land.pasture
chk.pop.on.land.forest <-  chk.pop.wo.stream.on.land.forest + chk.pop.w.stream.on.land.forest
chk.pop.on.land.RAOCUT <-  chk.pop.wo.stream.on.land.RAOCUT + chk.pop.w.stream.on.land.RAOCUT
## in stream
chk.pop.in.stream.total <-   chk.Animal.Density * chk.land.w.stream.access.habitat * chk.in.and.around.streams / 100
chk.pop.in.stream.pasture <- chk.Animal.Density * chk.land.w.stream.access.pasture * chk.in.and.around.streams / 100
chk.pop.in.stream.forest <-  chk.Animal.Density * chk.land.w.stream.access.forest *  chk.in.and.around.streams / 100
chk.pop.in.stream.RAOCUT <-  chk.Animal.Density * chk.land.w.stream.access.RAOCUT *  chk.in.and.around.streams / 100
## bacteria loads
## overall
chk.bac.total <-   chk.Bacteria.Prod * chk.pop.total
chk.bac.pasture <- chk.Bacteria.Prod * chk.pop.pasture
chk.bac.forest <-  chk.Bacteria.Prod * chk.pop.forest
chk.bac.RAOCUT <-  chk.Bacteria.Prod * chk.pop.RAOCUT
## on land
chk.bac.on.land.total <-   chk.Bacteria.Prod * chk.pop.on.land.total
chk.bac.on.land.pasture <- chk.Bacteria.Prod * chk.pop.on.land.pasture
chk.bac.on.land.forest <-  chk.Bacteria.Prod * chk.pop.on.land.forest
chk.bac.on.land.RAOCUT <-  chk.Bacteria.Prod * chk.pop.on.land.RAOCUT
## in stream
chk.bac.in.stream.total <-   chk.Bacteria.Prod * chk.pop.in.stream.total
chk.bac.in.stream.pasture <- chk.Bacteria.Prod * chk.pop.in.stream.pasture
chk.bac.in.stream.forest <-  chk.Bacteria.Prod * chk.pop.in.stream.forest
chk.bac.in.stream.RAOCUT <-  chk.Bacteria.Prod * chk.pop.in.stream.RAOCUT
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
  pop.on.land=chk.pop.on.land.total,
  pop.in.stream=chk.pop.in.stream.total,
  Bacteria.total=chk.bac.total,
  Bacteria.on.land=chk.bac.on.land.total,
  Bacteria.in.stream=chk.bac.in.stream.total,
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
pdf(file = paste0(chr.wildlife.heregr.dir, "/heregr-bacteria-model-calc-check-",
                  gsub("\\.txt","-",chr.input) 
                  ,strftime(Sys.time(), format = "%Y%m%d%H%M"),
                  ".pdf"), height = 8.5, width = 11, onefile = TRUE)
## total population
tmp.gt <- table.grob(chr.col = "pop.total", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Total number of heregr (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population on land
tmp.gt <- table.grob(chr.col = "pop.on.land", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Number of heregr on land (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population in stream
tmp.gt <- table.grob(chr.col = "pop.in.stream", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Number of heregr in stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Total bacteria load
tmp.gt <- table.grob(chr.col = "Bacteria.total", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Total bacteria load from heregr (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## bacteria load on land
tmp.gt <- table.grob(chr.col = "Bacteria.on.land", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load on land from heregr (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## bacteria load to stream
tmp.gt <- table.grob(chr.col = "Bacteria.in.stream", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to stream from heregr (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Accum bacteria load to pasture
tmp.gt <- table.grob(chr.col = "Accum.pasture", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to land as accum for pasture from heregr (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Accum bacteria load to forest
tmp.gt <- table.grob(chr.col = "Accum.forest", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to land as accum for forest from heregr (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Accum bacteria load to RAOCUT
tmp.gt <- table.grob(chr.col = "Accum.RAOCUT", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to land as accum for RAOCUT from heregr (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Lim bacteria load for pasture
tmp.gt <- table.grob(chr.col = "Lim.pasture", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load limit to land as accum for pasture from heregr (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Lim bacteria load for forest
tmp.gt <- table.grob(chr.col = "Lim.forest", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load limit to land as accum for forest from heregr (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Lim bacteria load for RAOCUT
tmp.gt <- table.grob(chr.col = "Lim.RAOCUT", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load limit to land as accum for RAOCUT from heregr (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
rm(list = ls(pattern = "tmp\\.*"))
## close the pdf file
dev.off()
