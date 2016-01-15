## get new input file names
chr.new.f <- list.files(pattern = "*.new\\.txt")

## get calc check script for subwtsd 01
chr.chk.01 <- scan(file = "Cow_Calf_Sub_Model_calculation_check-subwtsd-01.R",
                   what = "character", sep = "\n")


ii <- 5

chr.subwtsd <- gsub("[^0-9]","",chr.new.f[ii])


tmp.scpt <- gsub("01", chr.subwtsd, chr.chk.01)

grep("^chk\\.lu", chr.chk.01)

tmp.in <- scan(file = chr.new.f[ii],
               what = "character", sep = "\n")

as.numeric(gsub("Pasture Area in Watershed \\(ac\\)\\:", "", 
     grep("Pasture Area in Watershed \\(ac\\)\\:",tmp.in, value = TRUE)))

as.numeric(gsub("Forest Area in Watershed \\(ac\\)\\:", "", 
                grep("Forest Area in Watershed \\(ac\\)\\:",tmp.in, value = TRUE)))

as.numeric(gsub("Percent of pasture with stream access\\:", "", 
                grep("Percent of pasture with stream access\\:",tmp.in, value = TRUE)))

as.numeric(gsub("Percent of forest with stream access\\:", "", 
                grep("Percent of forest with stream access\\:",tmp.in, value = TRUE)))


"Percent of pasture with stream access\\:"
"Percent of forest with stream access\\:"