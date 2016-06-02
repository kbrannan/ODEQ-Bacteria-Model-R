chr.rscripts.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts"
chr.dir.wildlife.elk <- paste0(chr.rscripts.dir, "/sub-models/Wildlife-Elk")
chr.areas.file.name <- "elk-Habitat-areas.csv"
chr.tpl <- "wildlifeElkUpdate.txt"
chr.files <- list.files(path = chr.dir.wildlife.elk, pattern = "^wildlifeElk([0-9]){2}\\.txt$",
                        full.names = TRUE)
df.input <- read.delim(file = paste0(chr.dir.wildlife.elk, "/", chr.tpl), sep=":",
                       stringsAsFactors=FALSE, header=FALSE, comment.char = "*")
names(df.input) <- c("parameter","value(s)")
df.areas.wq <- read.csv(file = paste0(chr.dir.wildlife.elk, "/", chr.areas.file.name),
                        header = TRUE)
df.areas.wq$wtsd <- sprintf(fmt = "%02i", df.areas.wq$wtsd)
get.area <- function(df.a, wtsd, ls, season) {
  num.area <- df.a$area[df.a$wtsd == wtsd & df.a$ls == ls & df.a$season == season]
  if(length(num.area) == 0) num.area <- 0
  return(num.area)
}
for(ii in 1:length(chr.files)) {
  
  tmp.sub <- gsub("[^0-9]","",substr(chr.files[ii], nchar(chr.files[ii])-5, nchar(chr.files[ii])))
  tmp.output <- df.input
  
  tmp.output$`value(s)`[tmp.output$parameter == "Watershed"] <- paste0(" Big Elk Creek Sub-Wtsd ", tmp.sub)
  tmp.output$`value(s)`[tmp.output$parameter == "Season 1 Pasture Area in Watershed (ac)"] <- paste0("   ", get.area(df.a = df.areas.wq, wtsd = tmp.sub, ls = "Pasture", season = "season 1"))
  tmp.output$`value(s)`[tmp.output$parameter == "Season 2 Pasture Area in Watershed (ac)"] <- paste0("   ", get.area(df.a = df.areas.wq, wtsd = tmp.sub, ls = "Pasture", season = "season 2"))
  tmp.output$`value(s)`[tmp.output$parameter == "Season 1 Forest Area in Watershed (ac)"] <- paste0("   ", get.area(df.a = df.areas.wq, wtsd = tmp.sub, ls = "Forest", season = "season 1"))
  tmp.output$`value(s)`[tmp.output$parameter == "Season 2 Forest Area in Watershed (ac)"] <- paste0("   ", get.area(df.a = df.areas.wq, wtsd = tmp.sub, ls = "Forest", season = "season 2"))
  
  write.table(tmp.output, file = gsub("\\.txt","-update.txt", chr.files[ii]),
              quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ":")
  rm(list=ls(pattern="tmp\\."))
}
