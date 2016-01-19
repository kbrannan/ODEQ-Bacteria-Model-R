## step bt step calculation check for cow_calf_sub_model using input from
## cowcalf08.txt file
source("Cow_Calf_Sub_Model.R")
df.output <- cow.calf(chr.wrkdir=getwd(),chr.input.file="cowcalf08.txt")
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
# calculations
# Number of pairs is rea of pasture divided by stocking density
chk.am.pairs     <- chk.lu.pasture.area / chk.amng.sd
# adjust size of pairs for calf growth by multiplying number of pairs by monthly
# growth vector to get number of pairs (adjusted) by month
chk.am.pairs.adj <- chk.am.pairs * chk.amng.adj.size
# distribute the pairs among pasture, forest or confinement across months
chk.loc.pasture <- chk.am.pairs.adj * chk.amng.in.pasture
chk.loc.forest <- chk.am.pairs.adj * chk.amng.in.forest
chk.loc.confine <- chk.am.pairs.adj * chk.amng.in.confine
# check location of pairs to total
chk.am.pairs - sum((chk.loc.pasture + chk.loc.forest + chk.loc.confine) / chk.amng.adj.size) / 12
# distribute pairs on forest or pasture with or without stream access
chk.loc.pasture.w <- (chk.lu.pasture.w / 100) * chk.loc.pasture
chk.loc.pasture.wo <- (1 - (chk.lu.pasture.w / 100)) * chk.loc.pasture
chk.loc.forest.w <- (chk.lu.forest.w / 100) * chk.loc.forest
chk.loc.forest.wo <- (1 - (chk.lu.forest.w / 100)) * chk.loc.forest
# distribute pairs on lu with stream access between in stream and land
chk.loc.pasture.w.strm <- (chk.ainfo.pasture.in.strm / 100) * 
  chk.loc.pasture.w
chk.loc.pasture.w.lnd <- (1 - (chk.ainfo.pasture.in.strm / 100)) * 
  chk.loc.pasture.w
chk.loc.forest.w.strm <- (chk.ainfo.forest.in.strm / 100) * 
  chk.loc.forest.w
chk.loc.forest.w.lnd <- (1 - (chk.ainfo.forest.in.strm / 100)) * 
  chk.loc.forest.w
# check all location end points comapred to total pairs
chk.am.pairs - sum((chk.loc.confine + chk.loc.pasture.wo + chk.loc.forest.wo +
  chk.loc.pasture.w.strm + chk.loc.pasture.w.lnd + 
  chk.loc.forest.w.strm + chk.loc.forest.w.lnd) / chk.amng.adj.size) / 12
# bacteria loads
chk.bac.strm <- (chk.loc.pasture.w.strm + chk.loc.forest.w.strm) * 
  chk.ainfo.bac.prod
chk.bac.pasture.lnd <- (chk.loc.pasture.wo + chk.loc.pasture.w.lnd) * 
  chk.ainfo.bac.prod
chk.bac.forest.lnd <- (chk.loc.forest.wo + chk.loc.forest.w.lnd) * 
  chk.ainfo.bac.prod
chk.bac.confine <- chk.loc.confine * chk.ainfo.bac.prod
# check bacteria loads to total
sum((chk.am.pairs.adj * chk.ainfo.bac.prod) - 
  sum(chk.bac.strm + chk.bac.pasture.lnd + chk.bac.forest.lnd + chk.bac.confine))
# accum
chk.Accum.Pasture <- chk.bac.pasture.lnd / chk.lu.pasture.area
chk.Accum.forest <- chk.bac.forest.lnd / chk.lu.forest.area
# sqolim
chk.Lim.Pasture <- chk.ainfo.sqolim.fac * chk.Accum.Pasture
chk.Lim.Forest <- chk.ainfo.sqolim.fac * chk.Accum.forest
# compare manual bacteria loads to function outputs
sum((chk.bac.strm - df.output$Bacteria.Instream)^2)/12
sum((chk.Accum.Pasture - df.output$Accum.Pasture)^2)/12
sum((chk.Accum.forest - df.output$Accum.Forest)^2)/12
# write average error to file "cow-calf-std-error.txt"
chr.chk <- c(paste0("Standard Error for In-Stream     = ", 
                    sum((chk.bac.strm - df.output$Bacteria.Instream)^2)/12),
             paste0("Standard Error for Pasture Accum = ", 
                    sum((chk.Accum.Pasture - df.output$Accum.Pasture)^2)/12),
             paste0("Standard Error for Forest Accum  = ", 
                    sum((chk.Accum.forest - df.output$Accum.Forest)^2)/12))
cat(chr.chk, file = "cow-calf-std-error-cowcalf08.txt", sep="\n")
