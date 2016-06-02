

chr.wildlife.elk.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/sub-models/wildlife-elk"
chr.habitat.raw.file.name.season.1 <- "Elk-season-1-habitat-ArcMap-Export.txt"
chr.habitat.raw.file.name.season.2 <- "Elk-season-2-habitat-ArcMap-Export.txt"
chr.output <- "elk-Habitat-areas.csv"

df.raw.season.1 <- data.frame(
  read.csv(paste0(chr.wildlife.elk.dir, "/", chr.habitat.raw.file.name.season.1),
           stringsAsFactors=FALSE, header=TRUE)[, c("hspf_id", "Seg_Name", "Shape_Area")],
  season = "season 1", stringsAsFactors = FALSE)

df.raw.season.2 <- data.frame(
  read.csv(paste0(chr.wildlife.elk.dir, "/", chr.habitat.raw.file.name.season.2),
           stringsAsFactors=FALSE, header=TRUE)[, c("hspf_id", "Seg_Name", "Shape_Area")],
  season = "season 2", stringsAsFactors = FALSE)

df.raw <- rbind(df.raw.season.1, df.raw.season.2)

names(df.raw) <- c("wtsd", "ls", "area", "season")

num.ft2.in.acre <- 43560

## data in correct units
df.data <- df.raw
df.data$wtsd <- sprintf(fmt = "%02i", df.raw$wtsd)
df.data$area <- round(as.numeric(df.raw$area) / num.ft2.in.acre, 2)

write.csv(x = df.data, file = paste0(chr.wildlife.elk.dir, "/", chr.output),
          row.names = FALSE)

