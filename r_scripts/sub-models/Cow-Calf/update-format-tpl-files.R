## read the the input file for subwtsd 01. this file is updated but does not
## have "new" added to the file name
chr.inp.f.01 <- scan(file = "cowcalf01.txt", sep = "\n", what = "character")

## read the the template file for subwtsd 01. this file is updated but does not
## have "new" added to the file name
chr.tpl.f.01 <- scan(file = "cowcalf01.tpl", sep = "\n", what = "character")

## get the rows of file that have PEST variables in them
grep("^.*\\$.*\\$$", chr.tpl.f.01)


chr.inp.f <- list.files(pattern = "*.new\\.txt")
