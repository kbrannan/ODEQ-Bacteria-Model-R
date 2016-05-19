## step bt step calculation check for onsite_pets_sub_model using input from
## onsitepets14.txt file
chr.onsitepets.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/on-site_and_pets"
chr.input <- "onsitepets14.txt"

## for model
chr.input.file <- paste0(chr.onsitepets.dir, "/", chr.input)

source(paste0(chr.onsitepets.dir,"/onsite_pets_Sub_Model.R"))
df.output <- onsite_pets(chr.input.file)

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
chk.lu.RAOCUT.area <- 14.8 # in acres
## general
chk.sqolim.fac      <- 9 # unitless
## onsite information
chk.onsite.bac.prod        <- 7.6000000E+11 # orgs/sys-day
chk.num.hh <- 68
chk.near.stream.strc <- 5
chk.strc.age.pre.1974 <- 47 # in percent
chk.strc.age.1974.1986 <- 21 # in percent
chk.strc.age.post.1986 <- 33 # in percent
chk.failure.rate.pre.1974 <- 22.5 # in percent
chk.failure.rate.1974.1986 <- 15 # in percent
chk.failure.rate.post.1986 <- 3.75 # in percent
chk.onsite.to.stream <- 5.0879000E-02 # in percent
# pets
chk.pets.hh <- 1
chk.pets.bac.prod <- 4.7500000E+09 # orgs/sys-day
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
df.output.chk <- cbind(df.output.chk, Bacteria.pets.load = df.output.chk$pop.pet.total * chk.pets.bac.prod)
##
## onsite
## all failures
df.output.chk <- cbind(df.output.chk, 
                       Bacteria.onsite.NearStrmStrctFailurePre1974 = df.output.chk$num.onsite.NearStrmStrctFailurePre1974 * chk.onsite.bac.prod,
                       Bacteria.onsite.NearStrmStrctFailure1974to1986 = df.output.chk$num.onsite.NearStrmStrctFailure1974to1986 * chk.onsite.bac.prod,
                       Bacteria.onsite.NearStrmStrctFailurePost1986 = df.output.chk$num.onsite.NearStrmStrctFailurePost1986 * chk.onsite.bac.prod,
                       Bacteria.onsite.NearStrmStrctFailure = df.output.chk$num.onsite.NearStrmStrctFailure * chk.onsite.bac.prod)
## to stream
df.output.chk <- cbind(df.output.chk,
                       Bacteria.onsite.NearStrmStrctFailure.to.stream.load = df.output.chk$num.onsite.NearStrmStrctFailureInStream * chk.onsite.bac.prod)
## 
## accum
df.output.chk <- cbind(df.output.chk,
                       Accum.RAOCUT = (chk.onsite.bac.prod * (chk.failure.pre.1974.to.land + chk.failure.1974.1986.to.land + chk.failure.post.1986.to.land) +
                                         df.output.chk$Bacteria.pets.load)/ chk.lu.RAOCUT.area)


## compare manual and model output
df.comp <- data.frame(Month = df.output$Month, diff = df.output[, c(-1, -1 * 19:23)] - df.output.chk[, -1])
names(df.comp) <- names(df.output.chk)
chk.dil <- 1E+06 # need to explain this


## output results in tables to pdf
pdf(file = paste0(chr.onsitepets.dir, "/onsite-pets-bacteria-model-calc-check-",
                  gsub("\\.txt","-",chr.input) 
                  ,strftime(Sys.time(), format = "%Y%m%d%H%M"),
                  ".pdf"), height = 8.5, width = 11, onefile = TRUE)

## Pet population
tmp.gt <- table.grob(chr.col = "pop.pet.total", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Total number of pets (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## onsite
## structures near-stream built pre 1974
tmp.gt <- table.grob(chr.col = "num.onsite.NearStrmStrctPre1974", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Number of structures near-stream structures built pre 1974 (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## structures near-stream built between 1974 to 1986
tmp.gt <- table.grob(chr.col = "num.onsite.NearStrmStrct1974to1986", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Number of structures near-stream structures built between 1974 to 1986 (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)



## structures near-stream built after 1986
tmp.gt <- table.grob(chr.col = "num.onsite.NearStrmStrctPost1986", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Number of structures near-stream structures built after 1986 (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## total near stream structures
tmp.gt <- table.grob(chr.col = "num.onsite.NearStrmStrct", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Total number of structures near-stream structures (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## failing on-site systems for structures near-stream built before 1974
tmp.gt <- table.grob(chr.col = "num.onsite.NearStrmStrctFailurePre1974", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Failing on-site systems for structures near-stream built before 197 (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## failing on-site systems for structures near-stream built between 1974 and 1986
tmp.gt <- table.grob(chr.col = "num.onsite.NearStrmStrctFailure1974to1986", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Failing on-site systems for structures near-stream built between 1974 and 1986 (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## failing on-site systems for structures near-stream built after 1986
tmp.gt <- table.grob(chr.col = "num.onsite.NearStrmStrctFailurePost1986", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Failing on-site systems for structures near-stream built after 1986 (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## total failing on-site systems for structures near-stream 
tmp.gt <- table.grob(chr.col = "num.onsite.NearStrmStrctFailure", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Total failing on-site systems for structures near-stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## total failing on-site systems for structures near-stream discharging to stream 
tmp.gt <- table.grob(chr.col = "num.onsite.NearStrmStrctFailureInStream", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Total failing on-site systems for structures near-stream discharging to stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## bacteria load from pets 
tmp.gt <- table.grob(chr.col = "Bacteria.pets.load", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Total bacteria load from pets (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## onsite system bacteria load from near-stream structures built before 1974
tmp.gt <- table.grob(chr.col = "Bacteria.onsite.NearStrmStrctFailurePre1974", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Total onsite system bacteria load from near-stream structures built before 1974 (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## onsite system bacteria load from near-stream structures built between 1974 and 1986
tmp.gt <- table.grob(chr.col = "Bacteria.onsite.NearStrmStrctFailure1974to1986", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Total onsite system failure bacteria load from near-stream structures built between 1974 and 1986 (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## onsite system bacteria load from near-stream structures built after 1986
tmp.gt <- table.grob(chr.col = "Bacteria.onsite.NearStrmStrctFailurePost1986", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Total onsite system failure bacteria load from near-stream structures built after 1986 (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## total onsite system bacteria load from near-stream structures
tmp.gt <- table.grob(chr.col = "Bacteria.onsite.NearStrmStrctFailure", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Total onsite system failure bacteria load from near-stream structures (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## total onsite system bacteria load from near-stream structures to stream
tmp.gt <- table.grob(chr.col = "Bacteria.onsite.NearStrmStrctFailure.to.stream.load", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Total onsite system failure bacteria load from near-stream structures to stream (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
grid.newpage()
rm(tmp.gt)

## accum to RAOCUT
tmp.gt <- table.grob(chr.col = "Accum.RAOCUT", df.output = df.output,
                     df.output.chk = df.output.chk, df.comp = df.comp,
                     chr.title = paste0("Accum to RAOCUT (dil = ", sprintf("%1.0E", chk.dil), ")"),
                     chk.dil = chk.dil)
grid.draw(tmp.gt)
rm(tmp.gt)


## close pdf file
dev.off()
