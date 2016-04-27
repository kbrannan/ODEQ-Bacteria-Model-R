## step bt step calculation check for cow_calf_sub_model using input from
## cowcalf01.txt file
chr.cowcalf.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/COw-Calf"
chr.input <- "cowcalf01.txt"

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
##
## get input
## land use information
chk.lu.pasture.area <- 445.3 # in acres
chk.lu.forest.area  <- 8739.8 # in acres
chk.lu.pasture.w    <- 48 # as percent
chk.lu.forest.w     <- 37.235884 # as percent
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
                         chk.am.pairs.adj * chk.amng.in.confine,
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
                         chk.am.pairs.adj * chk.amng.in.confine,
                       Bacteria.InForest = chk.ainfo.bac.prod * 
                         (1 - (chk.lu.forest.w / 100)) * chk.loc.forest +
                         (1 - (chk.ainfo.forest.in.strm / 100)) * 
                         chk.loc.forest.w,
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

## output results in tables to pdf
pdf(file = paste0(chr.cowcalf.dir, "/cow-calf-bacteria-model-calc-check-",
                  gsub("\\.txt","-",chr.input) 
                  ,strftime(Sys.time(), format = "%Y%m%d%H%M"),
                  ".pdf"), height = 8.5, width = 11, onefile = TRUE)
## population
tmp.table <- tableGrob(df.comp, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "Compare Manual and Model Output for Cow-Calf Calcs",
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
grid.newpage()
rm(list = ls(pattern = "tmp\\.*"))
dev.off()


df.output[, -1] - df.output.chk[, -1]

## number of pairs
df.output$NumOfPairs - df.output.chk$NumOfPairs

## AUvsTime
df.output$AUvsTime - df.output.chk$AUvsTime


## on pasture without stream access
df.output$pairs.OnPastureWOStreamAccess - df.output.chk$pairs.OnPastureWOStreamAccess

## on pasture with stream access (on land)
df.output$pairs.OnPastureWStreamAccess - df.output.chk$pairs.OnPastureWStreamAccess

## on pasture in stream
df.output$pairs.OnPastureInStream - df.output.chk$pairs.OnPastureInStream


num.cols <- c(4,5,6)
df.comp.on.pasture <- data.frame(Month = df.output$Month,
                                 mod = rowSums(subset(df.output, select = num.cols)),
                                 man = rowSums(subset(df.output.chk, select = num.cols)))

## in forest
num.cols <- c(8,9,10)
df.comp.in.forest <- data.frame(Month = df.output$Month,
                                 mod = rowSums(subset(df.output, select = num.cols)),
                                 man = rowSums(subset(df.output.chk, select = num.cols)))




junk <- cbind(
  melt.data.frame(
    data = cbind(df.output, 
                 src = factor("mod", levels = c("mod", "man")), 
                 id.vars = c("Month", "src"))),
    melt.data.frame(
      data = cbind(df.output.chk, 
                   src = factor("mod", levels = c("mod", "man")), 
                   id.vars = c("Month", "src"))))

junk <- rbind(
  cbind(df.output, 
              data.frame(src = 
                           rep("mod", length(df.output$Month)), 
                         stringsAsFactors = FALSE)), 
  cbind(df.output.chk, 
              data.frame(src = 
                           rep("man", length(df.output$Month)), 
                         stringsAsFactors = FALSE))
)

junk$src <- factor(junk$src, levels = c("mod", "man"))

junk.melt <- melt.data.frame(data = junk, id.vars = c("Month", "src"))

junk.pairs <- summaryBy(pairs.OnPastureWOStreamAccess + pairs.OnPastureWStreamAccess + pairs.OnPastureInStream + pairs.InConfinementvsTime + pairs.InForestWOStreamAccess + pairs.InForestWStreamAccess + pairs.InForestInStream ~ Month + src, data = junk.melt, FUN = sum)

junk.By <- summaryBy(value ~ Month + src + variable, data = junk.melt, FUN = sum)

subset(junk.melt, c("pairs.OnPastureWOStreamAccess", "pairs.OnPastureWStreamAccess", "pairs.OnPastureInStream", "pairs.InConfinementvsTime", "pairs.InForestWOStreamAccess", "pairs.InForestWStreamAccess", "pairs.InForestInStream"))
junk.melt[, c("pairs.OnPastureWOStreamAccess", "pairs.OnPastureWStreamAccess", "pairs.OnPastureInStream", "pairs.InConfinementvsTime", "pairs.InForestWOStreamAccess", "pairs.InForestWStreamAccess", "pairs.InForestInStream")]

junk.pairs <- summaryBy(variable ~ Month + src, 
                        data = junk.melt, FUN = sum)


# accum
chk.Accum.Pasture <- chk.bac.pasture.lnd / chk.lu.pasture.area
chk.Accum.forest <- chk.bac.forest.lnd / chk.lu.forest.area
# sqolim
chk.Lim.Pasture <- chk.ainfo.sqolim.fac * chk.Accum.Pasture
chk.Lim.Forest <- chk.ainfo.sqolim.fac * chk.Accum.forest

##
## get input
## SQOLIM multiplcation factor: 
chk.sqolim <- 9
chk.Bacteria.Prod <- 1.9665000E+05
### Animal Densities
chk.Animal.Density <- 7.5000000E-03
### Habitats
chk.land.Forest   <- 1034.01
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
df.output[2, c(1, grep("pairs", names(df.output)))]
df.output$Month <- factor(df.output$Month,
                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
df.output.chk$Month <- factor(df.output.chk$Month,
                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
summaryBy( .~ Month, 
           data = df.output[, c(1, grep("pairs", names(df.output)))], 
           FUN = sum)

summaryBy( Month ~., 
           data = df.output.chk[, c(1, grep("pairs", names(df.output.chk)))], 
           FUN = sum)

df.output[, -1] - df.output.chk[, -1]





summaryBy(pairs.OnPastureWOStreamAccess + pairs.OnPastureWStreamAccess +
            pairs.OnPastureInStream + pairs.InConfinementvsTime +
            pairs.InForestWOStreamAccess + pairs.InForestWStreamAccess +
            pairs.InForestInStream ~ Month, data = df.output, FUN = sum)



df.output[ , "AUvsTime"] - df.output.chk[ , "AUvsTime"]
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
