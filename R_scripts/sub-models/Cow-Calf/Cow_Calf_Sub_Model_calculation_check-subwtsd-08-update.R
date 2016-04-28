## step bt step calculation check for cow_calf_sub_model using input from
## cowcalf08.txt file
chr.cowcalf.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/COw-Calf"
chr.input <- "cowcalf08.txt"

## for model
chr.wrkdir <- chr.cowcalf.dir
chr.input.file <- chr.input

source(paste0(chr.cowcalf.dir,"/Cow_Calf_Sub_Model.R"))
df.output <- df.output <- cow.calf(chr.wrkdir=chr.cowcalf.dir,
                                   chr.input.file=chr.input)
## packages
library(doBy, quietly = TRUE)
library(gridExtra, quietly = TRUE)
library(reshape2, quietly = TRUE)

## funct for creating tables for output of results
table.grob <- function(chr.col, df.output = df.output,
                       df.output.chk = df.outout.chk,
                       df.comp = df.comp, chr.title = NULL, chk.dil = 1E+06) {
  df.output$pairs.OnPastureWStreamAccess - df.output.chk$pairs.OnPastureWStreamAccess
  df.output$pairs.OnPastureWOStreamAccess - df.output.chk$pairs.OnPastureWOStreamAccess
  df.output$AUvsTime - df.output.chk$AUvsTime
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
## land use information
chk.lu.pasture.area <- 116.1 # in acres
chk.lu.forest.area  <- 3585.7 # in acres
chk.lu.pasture.w    <- 32 # as percent
chk.lu.forest.w     <- 0.02190678 # as percent
## animal management information
chk.amng.sd         <- 1.0000000E+01 # as acres per animal
# next 4 variables are by month
chk.amng.adj.size   <- c(1.15,1.35,1.39,1.43,1.47,1.51,1.55,1.59,1.63,
                         1,1.05,1.1) # unitless
# columns of the next three variables should sum to 1
chk.amng.in.pasture <- c(0.25,0.25,0.75,0.75,0.75,1,0.75,0.75,0.75,0.25,
                         0.25,0.25) # as fraction of 1
chk.amng.in.confine <- c(0.75,0.75,0.25,0.25,0.25,0,0,0,0,0.75,
                         0.75,0.75) # as fraction of 1
chk.amng.in.forest  <- c(0,0,0,0,0,0,0.25,0.25,0.25,0,
                         0,0) # as fraction of 1
## animal information
chk.ainfo.bac.prod        <- 2.5650000E+09 # orgs per one beef cow
chk.ainfo.sqolim.fac      <- 9 # unitless
chk.ainfo.pasture.in.strm <- 1.2500000E+01 # as percent
chk.ainfo.forest.in.strm  <- 3.7235884E+01 # as percent
##
## create output check data.frame
df.output.chk <- data.frame(Month = df.output$Month)
##
# calculations
# Number of pairs is rea of pasture divided by stocking density
df.output.chk <- cbind(df.output.chk, 
                       NumOfPairs = chk.lu.pasture.area / chk.amng.sd)
# adjust size of pairs for calf growth by multiplying number of pairs by monthly
# growth vector to get number of pairs (adjusted) by month
df.output.chk <- cbind(df.output.chk,
                       AUvsTime = df.output.chk$NumOfPairs * chk.amng.adj.size)
# distribute the pairs among pasture, forest or confinement across months
chk.loc.pasture <- df.output.chk$AUvsTime * chk.amng.in.pasture
chk.loc.forest <- df.output.chk$AUvsTime * chk.amng.in.forest
chk.loc.confine <- df.output.chk$AUvsTime * chk.amng.in.confine
## on land with stream access
chk.loc.pasture.w <- (chk.lu.pasture.w / 100) * chk.loc.pasture
chk.loc.forest.w <- (chk.lu.forest.w / 100) * chk.loc.forest
# distribute pairs on forest or pasture with or without stream access or in-stream
df.output.chk <- cbind(df.output.chk, 
                       pairs.OnPastureWOStreamAccess = 
                         (1 - (chk.lu.pasture.w / 100)) * chk.loc.pasture,
                       pairs.OnPastureWStreamAccess = 
                         (1 - (chk.ainfo.pasture.in.strm / 100)) * 
                         chk.loc.pasture.w,
                       pairs.OnPastureInStream = 
                         (chk.ainfo.pasture.in.strm / 100) * chk.loc.pasture.w,
                       pairs.InConfinementvsTime = 
                         df.output.chk$AUvsTime * chk.amng.in.confine,
                       pairs.InForestWOStreamAccess = 
                         (1 - (chk.lu.forest.w / 100)) * chk.loc.forest,
                       pairs.InForestWStreamAccess = 
                         (1 - (chk.ainfo.forest.in.strm / 100)) * 
                         chk.loc.forest.w,
                       pairs.InForestInStream = 
                         (chk.ainfo.forest.in.strm / 100) * chk.loc.forest.w)
# distribute bacteria loads on forest or pasture with or without stream access or in-stream
df.output.chk <- cbind(df.output.chk, 
                       Bacteria.OnPastureWOStreamAccess = chk.ainfo.bac.prod * 
                         (1 - (chk.lu.pasture.w / 100)) * chk.loc.pasture,
                       Bacteria.OnPastureWStreamAccess = chk.ainfo.bac.prod * 
                         (1 - (chk.ainfo.pasture.in.strm / 100)) * 
                         chk.loc.pasture.w,
                       Bacteria.OnPastureInStream = chk.ainfo.bac.prod * 
                         (chk.ainfo.pasture.in.strm / 100) * chk.loc.pasture.w,
                       Bacteria.InConfinementvsTime = chk.ainfo.bac.prod * 
                         df.output.chk$AUvsTime * chk.amng.in.confine,
                       Bacteria.InForest = chk.ainfo.bac.prod * 
                         ((1 - (chk.lu.forest.w / 100)) * chk.loc.forest +
                         (1 - (chk.ainfo.forest.in.strm / 100)) * 
                         chk.loc.forest.w),
                       Bacteria.InForestInStream = chk.ainfo.bac.prod * 
                         (chk.ainfo.forest.in.strm / 100) * chk.loc.forest.w)
# bacteria total loads
chk.bac.strm <- df.output.chk$Bacteria.OnPastureInStream +
  df.output.chk$Bacteria.InForestInStream
chk.bac.pasture.lnd <- df.output.chk$Bacteria.OnPastureWOStreamAccess +
  df.output.chk$Bacteria.OnPastureWStreamAccess
chk.bac.forest.lnd <- df.output.chk$Bacteria.InForest
# total load in-stream, accum and lim
df.output.chk <- cbind(df.output.chk, 
                       Bacteria.Instream = chk.bac.strm,
                       Accum.Pasture = chk.bac.pasture.lnd / chk.lu.pasture.area,
                       Accum.Forest = chk.bac.forest.lnd / chk.lu.forest.area,
                       Lim.Pasture = chk.ainfo.sqolim.fac * 
                         chk.bac.pasture.lnd / chk.lu.pasture.area,
                       Lim.Forest = chk.ainfo.sqolim.fac * 
                         chk.bac.forest.lnd / chk.lu.forest.area)
                       


## compare manual and model output
df.comp <- data.frame(Month = df.output$Month, diff = df.output[, -1] - df.output.chk[, -1])
names(df.comp) <- names(df.output)
chk.dil <- 1E+06 # need to explain this


## output results in tables to pdf
pdf(file = paste0(chr.cowcalf.dir, "/cow-cal-bacteria-model-calc-check-",
                  gsub("\\.txt","-",chr.input) 
                  ,strftime(Sys.time(), format = "%Y%m%d%H%M"),
                  ".pdf"), height = 8.5, width = 11, onefile = TRUE)

## number of pairs
tmp.gt <- table.grob(chr.col = "NumOfPairs", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Total number of Cow-calf pairs (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## AUvsTime
tmp.gt <- table.grob(chr.col = "AUvsTime", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Animal Units by month of Cow-calf pairs (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## on pasture without stream access
tmp.gt <- table.grob(chr.col = "pairs.OnPastureWOStreamAccess", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Cow-calf pairs on pasture without stream access (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)



## on pasture with stream access (on land)
tmp.gt <- table.grob(chr.col = "pairs.OnPastureWStreamAccess", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Cow-calf pairs on pasture with stream access (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## on pasture in stream
tmp.gt <- table.grob(chr.col = "pairs.OnPastureInStream", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Cow-calf pairs on pasture in stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## in confinement
tmp.gt <- table.grob(chr.col = "pairs.InConfinementvsTime", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Cow-calf pairs in confinement (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## in forest without stream access
tmp.gt <- table.grob(chr.col = "pairs.InForestWOStreamAccess", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Cow-calf pairs in forest without stream access (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## in forest without stream access on land
tmp.gt <- table.grob(chr.col = "pairs.InForestWStreamAccess", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Cow-calf pairs in forest on land without stream access (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## in forest without stream access in stream
tmp.gt <- table.grob(chr.col = "pairs.InForestInStream", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Cow-calf pairs in forest in stream access (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## bacteria on pasture without stream access
tmp.gt <- table.grob(chr.col = "Bacteria.OnPastureWOStreamAccess", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria on pasture without stream access (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## bacteria on pasture with stream access
tmp.gt <- table.grob(chr.col = "Bacteria.OnPastureWStreamAccess", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria on pasture with stream access (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## bacteria on pasture in stream access in stream
tmp.gt <- table.grob(chr.col = "Bacteria.OnPastureInStream", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria on pasture in stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## bacteria in confinement
tmp.gt <- table.grob(chr.col = "pairs.InConfinementvsTime", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria in confinement (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## bacteria in forest without and with stream access
tmp.gt <- table.grob(chr.col = "Bacteria.InForest", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria in forest on land with and without stream access (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## bacteria in forest in stream
tmp.gt <- table.grob(chr.col = "Bacteria.InForestInStream", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria in forest in stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## bacteria in stream
tmp.gt <- table.grob(chr.col = "Bacteria.Instream", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Bacteria in stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## Accum for pasture
tmp.gt <- table.grob(chr.col = "Accum.Pasture", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Accum for pasture (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## Accum for forest
tmp.gt <- table.grob(chr.col = "Accum.Forest", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Accum for Forest (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## Lim for pasture
tmp.gt <- table.grob(chr.col = "Lim.Pasture", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("SQOLIM for pasture (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## Lim for forest
tmp.gt <- table.grob(chr.col = "Lim.Forest", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("SQOLIM for Forest (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
rm(tmp.gt)

## close pdf file
dev.off()