chr.rscripts.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts"

chr.dir.wildlife.racoon <- paste0(chr.rscripts.dir, "/sub-models/Wildlife-Racoon")

chr.areas.file.name <- "sub-wtsd-areas.RData"

chr.template.file <- "wildlifeRacoonUpdate.txt"

chr.files <- list.files(path = chr.dir.wildlife.racoon, pattern = "^wildlifeRacoon([0-9]){2}\\.tpl$",
                        full.names = TRUE)
df.input <- read.delim(paste0(chr.dir.wildlife.racoon, "/", chr.template.file), sep=":",
                       stringsAsFactors=FALSE, header=FALSE, comment.char = "*")
names(df.input) <- c("parameter","value(s)")

load(file = paste0(chr.dir.wildlife.racoon, "/", chr.areas.file.name))

get.area <- function(df.a, wtsd, ls) {
  num.area <- df.a$area[df.a$wtsd == wtsd & df.a$ls == ls]
  if(length(num.area) == 0) num.area <- 0
  return(num.area)
}


for(ii in 1:length(chr.files)) {
  
  tmp.sub <- gsub("[^0-9]","",substr(chr.files[ii], nchar(chr.files[ii])-5, nchar(chr.files[ii])))

  tmp.output <- df.input
  
  tmp.output$`value(s)`[tmp.output$parameter == "Watershed"] <- paste0("Big Elk Creek Sub-Wtsd ", tmp.sub)
  tmp.output$`value(s)`[tmp.output$parameter == "Forest Area in Watershed (ac)"] <- paste0("   ", get.area(df.a = df.areas.wq, wtsd = tmp.sub, ls = "forest"))
  tmp.output$`value(s)`[tmp.output$parameter == "Pasture Area in Watershed (ac)"] <- paste0("   ", get.area(df.a = df.areas.wq, wtsd = tmp.sub, ls = "pasture"))
  tmp.output$`value(s)`[tmp.output$parameter == "Residential/Agricultural Operration Area/Commercial/Urban/Transportation (ac)"] <- paste0("   ", get.area(df.a = df.areas.wq, wtsd = tmp.sub, ls = "developed"))
  
  write.table(tmp.output, file = gsub("\\.tpl","-update.txt", chr.files[ii]),
              quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ":")
  rm(list=ls(pattern="tmp\\."))
}
