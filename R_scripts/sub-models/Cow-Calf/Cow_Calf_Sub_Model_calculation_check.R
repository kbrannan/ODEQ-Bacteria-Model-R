## step bt step calculation check for cow_calf_sub_model using input from
## cowcalf01.txt file



## land use information
chk.lu.pasture.area <- 445.3 # in acres
chk.lu.forest.area  <- 8739.8 # in acres
chk.lu.pasture.w    <- 4.8000000E+01 # as percent
chk.lu.forest.w     <- 3.7235884E+01 # as percent

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


