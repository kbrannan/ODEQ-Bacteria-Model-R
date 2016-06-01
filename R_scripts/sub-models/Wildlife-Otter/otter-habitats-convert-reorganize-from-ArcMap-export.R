## otter habitats convert and reorganize info from ArcMap export
chr.wildlife.otter.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/Wildlife-Otter"
chr.input <- "Otter-Habitat-ArcMap-Export.txt"
chr.output <- "Otter-Habitat-areas-lengths.csv"
## export file
chr.input.file <- paste0(chr.wildlife.otter.dir, "/", chr.input)
## read export file
df.raw <- read.delim(chr.input.file, sep=",", 
                       comment.char="*", stringsAsFactors=FALSE, 
                       header=TRUE)[, c(2,3,4,5)]
names(df.raw) <- c("ls", "wtsd", "len", "area")

num.ft.in.mile <- 5280
num.ft2.in.acre <- 43560

## data in correct units
df.data <- df.raw

df.data$len <- round(as.numeric(df.raw$len) / num.ft.in.mile, 2)
df.data$area <- round(as.numeric(df.raw$area) / num.ft2.in.acre, 2)

write.csv(x = df.data, file = paste0(chr.wildlife.otter.dir, "/", chr.output),
          row.names = FALSE)
