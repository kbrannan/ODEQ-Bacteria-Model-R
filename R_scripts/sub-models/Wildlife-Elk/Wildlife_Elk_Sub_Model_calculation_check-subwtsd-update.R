## step bt step calculation check for wildlife_elk_sub_model using input from
## wildlifeElkXX.txt file
chr.wildlife.elk.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-Elk"
chr.input <- "wildlifeElkXX.txt"
## file for model
chr.input.file <- paste0(chr.wildlife.elk.dir, "/", chr.input)
## run model
source(paste0(chr.wildlife.elk.dir,"/Wildlife_Elk_Sub_Model.R"))
df.output <- wildlifeElk(chr.input.file)
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
chk.bacteria.prod <- 6.6500000E+08
### two seasons
chk.season.1.Months <- c(11,12,1,2,3)
chk.season.2.Months <- c(4,5,6,7,8,9,10)
### Animal Densities
chk.Animal.Density.pasture.season.1 <- 3.0072900E-03
chk.Animal.Density.forest.season.1  <- 3.0072900E-03
chk.Animal.Density.pasture.season.2 <- 4.5000000E-02
chk.Animal.Density.forest.season.2  <- 4.5000000E-02
### Habitats
chk.land.pasture.season.1 <- -999
chk.land.forest.season.1  <- -999
chk.habitat.season.1 <- chk.land.pasture.season.1 + chk.land.forest.season.1
chk.land.pasture.season.2 <- -999
chk.land.forest.season.2  <- -999
chk.habitat.season.2 <- chk.land.pasture.season.2 + chk.land.forest.season.2
### Percent landuse with stream access
chk.land.pasture.w.stream.access.season.1 <- 25
chk.land.forest.w.stream.access.season.1  <- 50
chk.land.pasture.w.stream.access.season.2 <- 15
chk.land.forest.w.stream.access.season.2  <- 39
### same percent of ducks in and around streams for all landuse
chk.pasture.in.and.around.streams.season.1 <- 9.9500000E+00
chk.forest.in.and.around.streams.season.1  <- 1.9208850E+00
chk.pasture.in.and.around.streams.season.2 <- 9.9500000E+00
chk.forest.in.and.around.streams.season.2  <- 9.9500000E+00
## calculations
## land use stream access
## without stream access
chk.land.wo.stream.access.pasture.season.1 <- (1 - chk.land.pasture.w.stream.access.season.1 / 100) * chk.land.pasture.season.1
chk.land.wo.stream.access.forest.season.1  <- (1 - chk.land.forest.w.stream.access.season.1 / 100) * chk.land.forest.season.1
chk.land.wo.stream.access.total.season.1   <- chk.land.wo.stream.access.pasture.season.1 + chk.land.wo.stream.access.forest.season.1
chk.land.wo.stream.access.pasture.season.2 <- (1 - chk.land.pasture.w.stream.access.season.2 / 100) * chk.land.pasture.season.2
chk.land.wo.stream.access.forest.season.2  <- (1 - chk.land.forest.w.stream.access.season.2 / 100) * chk.land.forest.season.2
chk.land.wo.stream.access.total.season.2   <- chk.land.wo.stream.access.pasture.season.2 + chk.land.wo.stream.access.forest.season.2
## with stream access
chk.land.w.stream.access.pasture.season.1 <- (chk.land.pasture.w.stream.access.season.1 / 100) * chk.land.pasture.season.1
chk.land.w.stream.access.forest.season.1  <- (chk.land.forest.w.stream.access.season.1 / 100) * chk.land.forest.season.1
chk.land.w.stream.access.total.season.1   <- chk.land.w.stream.access.pasture.season.1 + chk.land.w.stream.access.forest.season.1
chk.land.w.stream.access.pasture.season.2 <- (chk.land.pasture.w.stream.access.season.2 / 100) * chk.land.pasture.season.2
chk.land.w.stream.access.forest.season.2  <- (chk.land.forest.w.stream.access.season.2 / 100) * chk.land.forest.season.2
chk.land.w.stream.access.total.season.2   <- chk.land.w.stream.access.pasture.season.2 + chk.land.w.stream.access.forest.season.2
## animal numbers
## on land with out stream access
chk.pop.on.land.wo.stream.access.pasture.season.1 <- chk.Animal.Density.pasture.season.1 * chk.land.wo.stream.access.pasture.season.1
chk.pop.on.land.wo.stream.access.forest.season.1  <- chk.Animal.Density.forest.season.1 * chk.land.wo.stream.access.forest.season.1
chk.pop.on.land.wo.stream.access.total.season.1   <- chk.pop.on.land.wo.stream.access.pasture.season.1 + chk.pop.on.land.wo.stream.access.forest.season.1
chk.pop.on.land.wo.stream.access.pasture.season.2 <- chk.Animal.Density.pasture.season.2 * chk.land.wo.stream.access.pasture.season.2
chk.pop.on.land.wo.stream.access.forest.season.2  <- chk.Animal.Density.forest.season.2 * chk.land.wo.stream.access.forest.season.2
chk.pop.on.land.wo.stream.access.total.season.2   <- chk.pop.on.land.wo.stream.access.pasture.season.2 + chk.pop.on.land.wo.stream.access.forest.season.2
## on land with stream access
chk.pop.on.land.w.stream.access.pasture.season.1 <- chk.Animal.Density.pasture.season.1 * (1 - chk.pasture.in.and.around.streams.season.1 / 100) * chk.land.w.stream.access.pasture.season.1
chk.pop.on.land.w.stream.access.forest.season.1  <- chk.Animal.Density.forest.season.1 * (1 - chk.forest.in.and.around.streams.season.1 / 100) * chk.land.w.stream.access.forest.season.1
chk.pop.on.land.w.stream.access.total.season.1   <- chk.pop.on.land.w.stream.access.pasture.season.1 + chk.pop.on.land.w.stream.access.forest.season.1
chk.pop.on.land.w.stream.access.pasture.season.2 <- chk.Animal.Density.pasture.season.2 * (1 - chk.pasture.in.and.around.streams.season.2 / 100) * chk.land.w.stream.access.pasture.season.2
chk.pop.on.land.w.stream.access.forest.season.2  <- chk.Animal.Density.forest.season.2 * (1 - chk.forest.in.and.around.streams.season.2 / 100) * chk.land.w.stream.access.forest.season.2
chk.pop.on.land.w.stream.access.total.season.2   <- chk.pop.on.land.w.stream.access.pasture.season.2 + chk.pop.on.land.w.stream.access.forest.season.2
## in stream
chk.pop.in.stream.pasture.season.1 <- chk.Animal.Density.pasture.season.1 * (chk.pasture.in.and.around.streams.season.1 / 100) * chk.land.w.stream.access.pasture.season.1
chk.pop.in.stream.forest.season.1  <- chk.Animal.Density.forest.season.1 * (chk.forest.in.and.around.streams.season.1 / 100) * chk.land.w.stream.access.forest.season.1
chk.pop.in.stream.total.season.1   <- chk.pop.in.stream.pasture.season.1 + chk.pop.in.stream.forest.season.1
chk.pop.in.stream.pasture.season.2 <- chk.Animal.Density.pasture.season.2 * (chk.pasture.in.and.around.streams.season.2 / 100) * chk.land.w.stream.access.pasture.season.2
chk.pop.in.stream.forest.season.2  <- chk.Animal.Density.forest.season.2 * (chk.forest.in.and.around.streams.season.2 / 100) * chk.land.w.stream.access.forest.season.2
chk.pop.in.stream.total.season.2   <- chk.pop.in.stream.pasture.season.2 + chk.pop.in.stream.forest.season.2
## on land
chk.pop.on.land.pasture.season.1 <- chk.pop.on.land.wo.stream.access.pasture.season.1 + chk.pop.on.land.w.stream.access.pasture.season.1
chk.pop.on.land.forest.season.1  <- chk.pop.on.land.wo.stream.access.forest.season.1  + chk.pop.on.land.w.stream.access.forest.season.1 
chk.pop.on.land.total.season.1   <- chk.pop.on.land.pasture.season.1 + chk.pop.on.land.forest.season.1
chk.pop.on.land.pasture.season.2 <- chk.pop.on.land.wo.stream.access.pasture.season.2 + chk.pop.on.land.w.stream.access.pasture.season.2
chk.pop.on.land.forest.season.2  <- chk.pop.on.land.wo.stream.access.forest.season.2  + chk.pop.on.land.w.stream.access.forest.season.2 
chk.pop.on.land.total.season.2   <- chk.pop.on.land.pasture.season.2 + chk.pop.on.land.forest.season.2
## bacteria loads
## on land with out stream access
chk.bac.on.land.wo.stream.access.pasture.season.1 <- chk.bacteria.prod * chk.pop.on.land.wo.stream.access.pasture.season.1
chk.bac.on.land.wo.stream.access.forest.season.1  <- chk.bacteria.prod * chk.pop.on.land.wo.stream.access.forest.season.1 
chk.bac.on.land.wo.stream.access.total.season.1   <- chk.bac.on.land.wo.stream.access.pasture.season.1 + chk.bac.on.land.wo.stream.access.forest.season.1
chk.bac.on.land.wo.stream.access.pasture.season.2 <- chk.bacteria.prod * chk.pop.on.land.wo.stream.access.pasture.season.2
chk.bac.on.land.wo.stream.access.forest.season.2  <- chk.bacteria.prod * chk.pop.on.land.wo.stream.access.forest.season.2 
chk.bac.on.land.wo.stream.access.total.season.2   <- chk.bac.on.land.wo.stream.access.pasture.season.2 + chk.bac.on.land.wo.stream.access.forest.season.2
## on land with stream access
chk.bac.on.land.w.stream.access.pasture.season.1 <- chk.bacteria.prod * chk.pop.on.land.w.stream.access.pasture.season.1
chk.bac.on.land.w.stream.access.forest.season.1  <- chk.bacteria.prod * chk.pop.on.land.w.stream.access.forest.season.1
chk.bac.on.land.w.stream.access.total.season.1   <- chk.bac.on.land.w.stream.access.pasture.season.1 + chk.bac.on.land.w.stream.access.forest.season.1
chk.bac.on.land.w.stream.access.pasture.season.2 <- chk.bacteria.prod * chk.pop.on.land.w.stream.access.pasture.season.2
chk.bac.on.land.w.stream.access.forest.season.2  <- chk.bacteria.prod * chk.pop.on.land.w.stream.access.forest.season.2
chk.bac.on.land.w.stream.access.total.season.2   <- chk.bac.on.land.w.stream.access.pasture.season.2 + chk.bac.on.land.w.stream.access.forest.season.2
## in stream
chk.bac.in.stream.pasture.season.1 <- chk.bacteria.prod * chk.pop.in.stream.pasture.season.1
chk.bac.in.stream.forest.season.1  <- chk.bacteria.prod * chk.pop.in.stream.forest.season.1 
chk.bac.in.stream.total.season.1   <- chk.bac.in.stream.pasture.season.1 + chk.bac.in.stream.forest.season.1
chk.bac.in.stream.pasture.season.2 <- chk.bacteria.prod * chk.pop.in.stream.pasture.season.2
chk.bac.in.stream.forest.season.2  <- chk.bacteria.prod * chk.pop.in.stream.forest.season.2 
chk.bac.in.stream.total.season.2   <- chk.bac.in.stream.pasture.season.2 + chk.bac.in.stream.forest.season.2
## on land
chk.bac.on.land.pasture.season.1 <- chk.bac.on.land.wo.stream.access.pasture.season.1 + chk.bac.on.land.w.stream.access.pasture.season.1
chk.bac.on.land.forest.season.1  <- chk.bac.on.land.wo.stream.access.forest.season.1 + chk.bac.on.land.w.stream.access.forest.season.1
chk.bac.on.land.total.season.1   <- chk.bac.on.land.pasture.season.1 + chk.bac.on.land.forest.season.1
chk.bac.on.land.pasture.season.2 <- chk.bac.on.land.wo.stream.access.pasture.season.2 + chk.bac.on.land.w.stream.access.pasture.season.2
chk.bac.on.land.forest.season.2  <- chk.bac.on.land.wo.stream.access.forest.season.2 + chk.bac.on.land.w.stream.access.forest.season.2
chk.bac.on.land.total.season.2   <- chk.bac.on.land.pasture.season.2 + chk.bac.on.land.forest.season.2
## accum loads
chk.bac.accum.pasture.season.1 <- chk.bac.on.land.pasture.season.1 / chk.land.pasture.season.1
chk.bac.accum.forest.season.1  <- chk.bac.on.land.forest.season.1  / chk.land.forest.season.1
chk.bac.accum.pasture.season.2 <- chk.bac.on.land.pasture.season.2 / chk.land.pasture.season.2
chk.bac.accum.forest.season.2  <- chk.bac.on.land.forest.season.2  / chk.land.forest.season.2

##
## combining results
chk.output.season.1 <- data.frame(
  Month=format(as.POSIXct(paste0("1967-",chk.season.1.Months,"-01")), format = "%b"),
  pop.total=chk.pop.on.land.total.season.1 + chk.pop.in.stream.total.season.1,
  pop.on.land=chk.pop.on.land.total.season.1,
  pop.in.stream=chk.pop.in.stream.total.season.1,
  Bacteria.total=chk.bac.on.land.total.season.1 + chk.bac.in.stream.total.season.1,
  Bacteria.on.land=chk.bac.on.land.total.season.1,
  Bacteria.in.stream=chk.bac.in.stream.total.season.1,
  Accum.pasture=chk.bac.accum.pasture.season.1,
  Accum.forest=chk.bac.accum.forest.season.1,
  Lim.pasture=chk.sqolim * chk.bac.accum.pasture.season.1,
  Lim.forest=chk.sqolim * chk.bac.accum.forest.season.1,
  month.order=chk.season.1.Months,
  stringsAsFactors=FALSE)
chk.output.season.2 <- data.frame(
  Month=format(as.POSIXct(paste0("1967-",chk.season.2.Months,"-01")), format = "%b"),
  pop.total=chk.pop.on.land.total.season.2 + chk.pop.in.stream.total.season.2,
  pop.on.land=chk.pop.on.land.total.season.2,
  pop.in.stream=chk.pop.in.stream.total.season.2,
  Bacteria.total=chk.bac.on.land.total.season.2 + chk.bac.in.stream.total.season.2,
  Bacteria.on.land=chk.bac.on.land.total.season.2,
  Bacteria.in.stream=chk.bac.in.stream.total.season.2,
  Accum.pasture=chk.bac.accum.pasture.season.2,
  Accum.forest=chk.bac.accum.forest.season.2,
  Lim.pasture=chk.sqolim * chk.bac.accum.pasture.season.2,
  Lim.forest=chk.sqolim * chk.bac.accum.forest.season.2,
  month.order=chk.season.2.Months,
  stringsAsFactors=FALSE)
chk.output <- rbind(chk.output.season.1, chk.output.season.2)
chk.output <- chk.output[order(chk.output$month.order), ]
df.chk <- chk.output[, -1 * grep("month.order", names(chk.output))]
rm(chk.output)
## may get NaN in accum becuase of zero areas for habitat. replace these
## these NaN with 0
df.nan <- df.chk[, -1]
df.nan[is.na(df.nan)] <- 0
df.chk <- cbind(Month = df.chk[, 1], df.nan)
rm(df.nan)
## compare
df.comp <- data.frame(
  Month=format(as.POSIXct(paste0("1967-",1:12,"-01")), format = "%b"),
  df.output[, -1] - df.chk[, -1])

##
## check model output
chk.dil <- 1E+06 # need to explain this
## output results in tables to pdf
pdf(file = paste0(chr.wildlife.elk.dir, "/elk-bacteria-model-calc-check-",
                  gsub("\\.txt","-",chr.input) 
                  ,strftime(Sys.time(), format = "%Y%m%d%H%M"),
                  ".pdf"), height = 8.5, width = 11, onefile = TRUE)
## total population
tmp.gt <- table.grob(chr.col = "pop.total", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Total number of elk (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population on land
tmp.gt <- table.grob(chr.col = "pop.on.land", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Number of elk on land (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## population in stream
tmp.gt <- table.grob(chr.col = "pop.in.stream", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Number of elk in stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Total bacteria load
tmp.gt <- table.grob(chr.col = "Bacteria.total", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Total bacteria load from elk (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## bacteria load on land
tmp.gt <- table.grob(chr.col = "Bacteria.on.land", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load on land from elk (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## bacteria load to stream
tmp.gt <- table.grob(chr.col = "Bacteria.in.stream", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to stream from elk (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Accum bacteria load to pasture
tmp.gt <- table.grob(chr.col = "Accum.pasture", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to land as accum for pasture from elk (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Accum bacteria load to forest
tmp.gt <- table.grob(chr.col = "Accum.forest", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load to land as accum for forest from elk (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Lim bacteria load for pasture
tmp.gt <- table.grob(chr.col = "Lim.pasture", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load limit to land as accum for pasture from elk (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
## Lim bacteria load for forest
tmp.gt <- table.grob(chr.col = "Lim.forest", df.output = df.output,
                     df.output.chk = df.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria load limit to land as accum for forest from elk (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
rm(list = ls(pattern = "tmp\\.*"))
## close the pdf file
dev.off()
