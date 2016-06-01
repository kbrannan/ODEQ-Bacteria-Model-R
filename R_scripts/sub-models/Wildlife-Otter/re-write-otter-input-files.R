chr.rscripts.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts"

chr.dir.wildlife.otter <- paste0(chr.rscripts.dir, "/sub-models/Wildlife-Otter")

chr.template.file.name <- "wildlifeOtterUpdate.txt"

chr.habitat.file.name <- "Otter-Habitat-areas-lengths.csv"

chr.files <- list.files(path = chr.dir.wildlife.otter, pattern = "^wildlifeOtter([0-9]){2}\\.txt$",
                        full.names = TRUE)
df.input <- read.delim(file = paste0(chr.dir.wildlife.otter, "/", chr.template.file.name), sep=":",
                       stringsAsFactors=FALSE, header=FALSE)

names(df.input) <- c("parameter","value(s)")

df.habitats <- read.csv(paste0(chr.dir.wildlife.otter, "/", chr.habitat.file.name),
                        colClasses = c("character", "character", "numeric", "numeric"))

get.area <- function(df.a, wtsd, ls) {
  num.area <- df.a$area[df.a$wtsd == wtsd & df.a$ls == ls]
  if(length(num.area) == 0) num.area <- 0
  return(num.area)
}
get.len <- function(df.l, wtsd, ls) {
  num.len <- df.l$len[df.l$wtsd == wtsd & df.l$ls == ls]
  if(length(num.len) == 0) num.len <- 0
  return(num.len)
}

for(ii in 1:length(chr.files)) {
  
  tmp.sub <- gsub("[^0-9]","",substr(chr.files[ii], nchar(chr.files[ii])-5, nchar(chr.files[ii])))

  tmp.output <- df.input
  
  tmp.output$`value(s)`[tmp.output$parameter == "Watershed"] <- paste0(" Big Elk Creek Sub-Wtsd ", tmp.sub)
  tmp.output$`value(s)`[tmp.output$parameter == "Forest Area in Watershed (ac)"] <- paste0("   ", get.area(df.a = df.habitats, wtsd = tmp.sub, ls = "Forest"))
  tmp.output$`value(s)`[tmp.output$parameter == "Pasture Area in Watershed (ac)"] <- paste0("   ", get.area(df.a = df.habitats, wtsd = tmp.sub, ls = "Pasture"))
  tmp.output$`value(s)`[tmp.output$parameter == "Stream Habitat in Forest (mile)"] <- paste0("   ", get.len(df.l = df.habitats, wtsd = tmp.sub, ls = "Forest"))
  tmp.output$`value(s)`[tmp.output$parameter == "Stream Habitat in Pasture (mile)"] <- paste0("   ", get.len(df.l = df.habitats, wtsd = tmp.sub, ls = "Pasture"))
  
  
  write.table(tmp.output, file = gsub("\\.txt","-update.txt", chr.files[ii]),
              quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ":")
  rm(list=ls(pattern="tmp\\."))
}
