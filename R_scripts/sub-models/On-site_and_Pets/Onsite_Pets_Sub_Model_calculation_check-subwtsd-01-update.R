## step bt step calculation check for onsite_pets_sub_model using input from
## onsitepets01.txt file
chr.onsitepets.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/on-site_and_pets"
chr.input <- "onsitepets01.txt"

## for model
chr.wrkdir <- chr.onsitepets.dir
chr.input.file <- chr.input

source(paste0(chr.onsitepets.dir,"/onsite_pets_Sub_Model.R"))
df.output <- onsite_pets(chr.wrkdir=chr.onsitepets.dir,
                                   chr.input=chr.input)
## packages
library(doBy, quietly = TRUE)
library(gridExtra, quietly = TRUE)
library(reshape2, quietly = TRUE)

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
## land use information
chk.lu.RAOCUT.area <- 435.5 # in acres
## general
chk.sqolim.fac      <- 9 # unitless
## onsite information
chk.onsite.bac.prod        <- 7.6000000E+11 # orgs/sys-day
chk.num.hh <- 259
chk.near.stream.strc <- 23
chk.strc.age.pre.1974 <- 47 # in percent
chk.strc.age.1974.1986 <- 21 # in percent
chk.strc.age.post.1986 <- 33 # in percent
chk.failure.rate.pre.1974 <- 22.5 # in percent
chk.failure.rate.1974.1986 <- 15 # in percent
chk.failure.rate.post.1986 <- 3.75 # in percent
chk.onsite.to.stream <- 100 # in percent
# pets
chk.pets.hh <- 1
chk.pets.bac.prod <- 4.7500000E+09 # orgs/sys-day
##
## create output check data.frame
df.output.chk <- data.frame(Month = df.output$Month)
##
# calculations
## pets
# Number of pets
df.output.chk <- cbind(df.output.chk, 
                       pop.pet.total = chk.num.hh * chk.pets.hh)
## onsite
## structure by age
df.output.chk <- cbind(df.output.chk,
                       num.onsite.NearStrmStrctPre1974 = chk.near.stream.strc * chk.strc.age.pre.1974 / 100,
                       num.onsite.NearStrmStrct1974to1986 = chk.near.stream.strc * chk.strc.age.1974.1986 / 100,
                       num.onsite.NearStrmStrctPost1986 = chk.near.stream.strc * chk.strc.age.post.1986 / 100,
                       num.onsite.NearStrmStrct = chk.near.stream.strc)
## failing systems by age
df.output.chk <- cbind(df.output.chk, 
                       num.onsite.NearStrmStrctFailurePre1974 = df.output.chk$num.onsite.NearStrmStrctPre1974 * chk.failure.rate.pre.1974 / 100,
                       num.onsite.NearStrmStrctFailure1974to1986 = df.output.chk$num.onsite.NearStrmStrct1974to1986 * chk.failure.rate.1974.1986 / 100,
                       num.onsite.NearStrmStrctFailurePost1986 = df.output.chk$num.onsite.NearStrmStrctPost1986 * chk.failure.rate.post.1986 / 100)
df.output.chk <- cbind(df.output.chk, num.onsite.NearStrmStrctFailure = df.output.chk$num.onsite.NearStrmStrctFailurePre1974 +
                         df.output.chk$num.onsite.NearStrmStrctFailure1974to1986 + df.output.chk$num.onsite.NearStrmStrctFailurePost1986)
## failed system direct to stream
chk.failure.pre.1974.to.stream <- df.output.chk$num.onsite.NearStrmStrctFailurePre1974 * chk.onsite.to.stream / 100
chk.failure.1974.1986.to.stream <- df.output.chk$num.onsite.NearStrmStrctFailure1974to1986 * chk.onsite.to.stream / 100
chk.failure.post.1986.to.stream <- df.output.chk$num.onsite.NearStrmStrctFailurePost1986 * chk.onsite.to.stream / 100
df.output.chk <- cbind(df.output.chk, num.onsite.NearStrmStrctFailureInStream = chk.failure.pre.1974.to.stream + 
                         chk.failure.1974.1986.to.stream + chk.failure.post.1986.to.stream)
## failed system to land
chk.failure.pre.1974.to.land <- df.output.chk$num.onsite.NearStrmStrctFailurePre1974 * (100 - chk.onsite.to.stream) / 100
chk.failure.1974.1986.to.land <- df.output.chk$num.onsite.NearStrmStrctFailure1974to1986 * (100 - chk.onsite.to.stream) / 100
chk.failure.post.1986.to.land <- df.output.chk$num.onsite.NearStrmStrctFailurePost1986 * (100 - chk.onsite.to.stream) / 100
##
## bacteria loads
## pets
df.output.chk <- cbind(df.output.chk, bac.pets.load = df.output.chk$pop.pet.total * chk.pets.bac.prod)
##
## onsite
## all failures
df.output.chk <- cbind(df.output.chk, 
                       bac.onsite.NearStrmStrctFailurePre1974 = df.output.chk$num.onsite.NearStrmStrctFailurePre1974 * chk.onsite.bac.prod,
                       bac.onsite.NearStrmStrctFailure1974to1986 = df.output.chk$num.onsite.NearStrmStrctFailure1974to1986 * chk.onsite.bac.prod,
                       bac.onsite.NearStrmStrctFailurePost1986 = df.output.chk$num.onsite.NearStrmStrctFailurePost1986 * chk.onsite.bac.prod,
                       bac.onsite.NearStrmStrctFailure = df.output.chk$num.onsite.NearStrmStrctFailure * chk.onsite.bac.prod)
## to stream
df.output.chk <- cbind(df.output.chk,
                       bac.onsite.NearStrmStrctFailure.to.stream.load = df.output.chk$num.onsite.NearStrmStrctFailureInStream * chk.onsite.bac.prod)
## 
## accum
df.output.chk <- cbind(df.output.chk,
                       Accum.RAOCUT = (chk.onsite.bac.prod * (chk.failure.pre.1974.to.land + chk.failure.1974.1986.to.land + chk.failure.post.1986.to.land) +
                                         df.output.chk$bac.pets.load)/ chk.lu.RAOCUT.area)


## compare manual and model output
df.comp <- data.frame(Month = df.output$Month, diff = df.output[, c(-1, -1 * 19:23)] - df.output.chk[, -1])
names(df.comp) <- names(df.output.chk)
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