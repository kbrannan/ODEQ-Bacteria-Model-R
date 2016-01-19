## read the the input file for subwtsd 01. this file is updated but does not
## have "new" added to the file name
chr.inp.f.01 <- scan(file = "cowcalf01.txt", sep = "\n", what = "character")

## read the the template file for subwtsd 01. this file is updated but does not
## have "new" added to the file name
chr.tpl.f.01 <- scan(file = "cowcalf01.tpl", sep = "\n", what = "character")

## get the rows of file that have PEST variables in them
lng.rows <- grep("^.*\\$.*\\$$", chr.tpl.f.01)

# get files names of new input files
chr.inp.f <- list.files(pattern = "*.new\\.txt")

## for each subwtsd 02-18, replace lines in input file with pest variable names
## from subwtsd 01 template file and save as *new.tpl file
for(ii in 1:length(chr.inp.f)) {
  tmp.tpl.f <- c("ptf $",scan(chr.inp.f[ii], sep = "\n", what = "character"))
  tmp.tpl.f[lng.rows] <- chr.tpl.f.01[lng.rows]
  cat(file = gsub("txt$","tpl",chr.inp.f[ii]), tmp.tpl.f, sep = "\n")
}
