## step bt step calculation check for wildlife_beaver_sub_model using input from
## wildlifeBeaver01.txt file
chr.wildlife.beaver.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-beaver"
chr.input <- "wildlifebeaver01.txt"

## file for model
chr.input.file <- paste0(chr.wildlife.beaver.dir, "/", chr.input)

## run model
source(paste0(chr.wildlife.beaver.dir,"/Wildlife_Beaver_Sub_Model.R"))
df.output <- wildlifeBeaver(chr.input.file)

## packages
library(doBy, quietly = TRUE)
library(gridExtra, quietly = TRUE)

## funct for creating tables for output of results
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
chk.Bacteria.Prod <- 1.9665000E+05
### Animal Densities
chk.Animal.Density <- 7.5000000E-03
### Habitats
chk.habitat <- 1034.01
chk.land.Forest <- chk.habitat
### percent of animals in/around streams
chk.in.and.around.streams <- 3.1245634E+01
## calculations
## animal numbers
chk.pop.total <- chk.Animal.Density *   chk.habitat
## with stream access on in/around atream
chk.pop.in.around.stream <- chk.pop.total * (chk.in.and.around.streams / 100)
## Beaver on land
chk.pop.on.land <- chk.pop.total - chk.pop.in.around.stream
## bacteria loads
chk.bac.total <- chk.pop.total * chk.Bacteria.Prod
## with stream access on in/around atream
chk.bac.in.around.stream <- chk.pop.in.around.stream * chk.Bacteria.Prod
## Beaver on land
chk.bac.on.land <- chk.pop.on.land * chk.Bacteria.Prod
## accum
chk.accum <- chk.bac.on.land / chk.habitat
## lim
chk.lim <- chk.accum * chk.sqolim
## put together
df.chk <- data.frame(
  pop.total = chk.pop.total,
  pop.on.land = chk.pop.on.land,
  pop.in.stream = chk.pop.in.around.stream,
  Bacteria.total = chk.bac.total,
  Bacteria.on.land = chk.bac.on.land,
  Bacteria.in.stream = chk.bac.in.around.stream,
  Accum.forest = chk.accum,
  Lim.forest = chk.lim,
  stringsAsFactors = FALSE)

## compare
df.comp <- df.output - df.chk

##
## check model output
chk.dil <- 1E+06 # need to explain this


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
