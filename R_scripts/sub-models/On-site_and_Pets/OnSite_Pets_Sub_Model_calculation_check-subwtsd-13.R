## step bt step calculation check for cow_calf_sub_model using input from
## OnSitePets13.txt file
chr.onsite.pets.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/On-site_and_pets"
source(paste0(chr.onsite.pets.dir,"/OnSite_Pets_Sub_Model.R"))
df.output <- onsite_pets(chr.wrkdir=chr.onsite.pets.dir,chr.input="OnSitePets13.txt")
## land use information
chk.lu.RAOCUT.area <- 465.8 # in acres
## general
chk.sqolim.fac      <- 9 # unitless
## onsite information
chk.onsite.bac.prod        <- 7.6000000E+11 # orgs/sys-day
chk.num.hh <- 148
chk.near.stream.strc <- 14
chk.strc.age.pre.1974 <- 47 # in percent
chk.strc.age.1974.1986 <- 21 # in percent
chk.strc.age.post.1986 <- 33 # in percent
chk.failure.rate.pre.1974 <- 22.5 # in percent
chk.failure.rate.1974.1986 <- 15 # in percent
chk.failure.rate.post.1986 <- 3.75 # in percent
chk.onsite.to.stream <- 3.9142475E+00 # in percent
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
                   chk.failure.post.1986.to.stream) * chk.onsite.bac.prod / 24
## to land 
chk.bac.land <- (chk.failure.pre.1974.to.land + 
                   chk.failure.1974.1986.to.land + 
                   chk.failure.post.1986.to.land) * chk.onsite.bac.prod +
  chk.bact.pets 


## accum and sqlim
chk.accum.RAOCUT <- rep(chk.bac.land / chk.lu.RAOCUT.area, 12)
chk.sqlim.RAOCUT <- chk.accum.RAOCUT * chk.sqolim.fac


# compare manual bacteria loads to function outputs
sum((chk.bac.strm - df.output$bac.onsite.NearStrmStrctFailure.to.stream.load)^2)/12
sum((chk.accum.RAOCUT - df.output$Accum.RAOCUT)^2)/12
# write average error to file "cow-calf-std-error.txt"
chr.chk <- c(paste0("Standard Error for In-Stream     = ", 
                    sum((chk.bac.strm - df.output$bac.onsite.NearStrmStrctFailure.to.stream.load)^2)/12),
             paste0("Standard Error for RAOCUT Accum = ", 
                    sum((chk.accum.RAOCUT - df.output$Accum.RAOCUT)^2)/12))
cat(chr.chk, file = paste0(chr.onsite.pets.dir, "/onsite-pets-std-error-cowcalf13.txt"), sep="\n")


