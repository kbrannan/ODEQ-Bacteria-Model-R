

chr.wildlife.elk.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-elk"
chr.habitat.summer.raw.file.name <- "Elk-summer-habitat-ArcMap-Export.txt"
chr.habitat.winter.raw.file.name <- "Elk-winter-habitat-ArcMap-Export.txt"
chr.output <- "elk-Habitat-areas.csv"

df.input.summer.raw <- data.frame(
  read.csv(paste0(chr.wildlife.elk.dir, "/", chr.habitat.summer.raw.file.name),
           stringsAsFactors=FALSE, header=TRUE)[, c("hspf_id", "Seg_Name", "Shape_Area")],
  season = "summer", stringsAsFactors = FALSE)

names(df.input.summer.raw) <- c("wtsd", "ls", "area", "season")

df.input.winter.raw <- data.frame(
  read.csv(paste0(chr.wildlife.elk.dir, "/", chr.habitat.winter.raw.file.name),
           stringsAsFactors=FALSE, header=TRUE)[, c("hspf_id", "Seg_Name", "Shape_Area")],
  season = "winter", stringsAsFactors = FALSE)

names(df.input.winter.raw) <- c("wtsd", "ls", "area", "season")

df.raw <- rbind(df.input.winter.raw, df.input.summer.raw)


num.ft.in.mile <- 5280
num.ft2.in.acre <- 43560

## data in correct units
df.data <- df.raw
df.data$wtsd <- sprintf(fmt = "%02i", df.raw$wtsd)
df.data$area <- round(as.numeric(df.raw$area) / num.ft2.in.acre, 2)

write.csv(x = df.data, file = paste0(chr.wildlife.elk.dir, "/", chr.output),
          row.names = FALSE)

