## step bt step calculation check for cow_calf_sub_model using input from
## OnSitePets01.txt file
chr.onsite.pets.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/On-site_and_pets"
source(paste0(chr.onsite.pets.dir,"/OnSite_Pets_Sub_Model.R"))
df.output <- onsite_pets(chr.wrkdir=chr.onsite.pets.dir,chr.input="OnSitePets01.txt")
## land use information
chk.lu.racut.area <- 435.5 # in acres
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
# calculations
# pets
chk.num.pets <- chk.num.hh * chk.pets.hh
chk.bact.pets <- chk.num.pets * chk.pets.bac.prod
## onsite
## structure by age
chk.near.stream.strcs.pre.1974 <- chk.near.stream.strc * chk.strc.age.pre.1974 / 100
chk.near.stream.strcs.1974.1986 <- chk.near.stream.strc * chk.strc.age.1974.1986 / 100
chk.near.stream.strcs.post.1986 <- chk.near.stream.strc * chk.strc.age.post.1986 / 100
## failing systems by age
chk.failure.pre.1974 <- chk.near.stream.strcs.pre.1974 * chk.failure.rate.pre.1974 / 100
chk.failure.1974.1986 <- chk.near.stream.strcs.1974.1986 * chk.failure.rate.1974.1986 / 100
chk.failure.post.1986 <- chk.near.stream.strcs.post.1986 * chk.failure.rate.post.1986 / 100
## failed system direct to stream
chk.failure.pre.1974.to.stream <- chk.failure.pre.1974 * chk.onsite.to.stream / 100
chk.failure.1974.1986.to.stream <- chk.failure.1974.1986 * chk.onsite.to.stream / 100
chk.failure.post.1986.to.stream <- chk.failure.post.1986 * chk.onsite.to.stream / 100
## failed system to land
chk.failure.pre.1974.to.land <- chk.failure.pre.1974 * (100 - chk.onsite.to.stream) / 100
chk.failure.1974.1986.to.land <- chk.failure.1974.1986 * (100 - chk.onsite.to.stream) / 100
chk.failure.post.1986.to.land <- chk.failure.post.1986 * (100 - chk.onsite.to.stream) / 100
## loads
## to stream
chk.bac.strm <- (chk.failure.pre.1974.to.stream + 
                   chk.failure.1974.1986.to.stream + 
                   chk.failure.post.1986.to.stream) * chk.onsite.bac.prod
## to land 
chk.bac.land <- (chk.failure.pre.1974.to.land + 
                   chk.failure.1974.1986.to.land + 
                   chk.failure.post.1986.to.land) * chk.onsite.bac.prod +
  chk.bact.pets



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
cat(chr.chk, file = "cow-calf-std-error-cowcalf01.txt", sep="\n")
