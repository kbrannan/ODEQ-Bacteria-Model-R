chr.rscripts.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts"

chr.dir.wildlife.geese <- paste0(chr.rscripts.dir, "/sub-models/Wildlife-Geese")

chr.areas.file.name <- "Goose-Duck-habitat-by-PLS-sub-wtsd.csv"

chr.files <- list.files(path = chr.dir.wildlife.geese, pattern = "^wildlifeGeese([0-9]){2}\\.txt$",
                        full.names = TRUE)
df.input <- read.delim(grep("01\\.txt$", chr.files, value = TRUE), sep=":",
                       stringsAsFactors=FALSE, header=FALSE)
names(df.input) <- c("parameter","value(s)")


df.areas.wq <- read.csv(file = paste0(chr.dir.wildlife.geese, "/", chr.areas.file.name))[, c(2, 3, 4)]

df.areas.wq$hspf_id <- sprintf(fmt = "%02i", df.areas.wq$hspf_id)

get.area <- function(df.a, wtsd, ls) {
  num.area <- df.a$area[df.a$wtsd == wtsd & df.a$ls == ls]
  if(length(num.area) == 0) num.area <- 0
  return(num.area)
}

names(df.areas.wq) <- c("wtsd", "ls", "area")

df.areas.wq$area <- round(df.areas.wq$area, 2)


for(ii in 1:length(chr.files)) {
  
  tmp.sub <- gsub("[^0-9]","",substr(chr.files[ii], nchar(chr.files[ii])-5, nchar(chr.files[ii])))

  tmp.output <- df.input
  
  tmp.output$`value(s)`[tmp.output$parameter == "Watershed"] <- paste0(" Big Elk Creek Sub-Wtsd ", tmp.sub)
  tmp.output$`value(s)`[tmp.output$parameter == "Forest Area in Watershed (ac)"] <- paste0("   ", get.area(df.a = df.areas.wq, wtsd = tmp.sub, ls = "Forest"))
  tmp.output$`value(s)`[tmp.output$parameter == "Pasture Area in Watershed (ac)"] <- paste0("   ", get.area(df.a = df.areas.wq, wtsd = tmp.sub, ls = "Pasture"))
  tmp.output$`value(s)`[tmp.output$parameter == "Residential/Agricultural Operration Area/Commercial/Urban/Transportation (ac)"] <- paste0("   ", get.area(df.a = df.areas.wq, wtsd = tmp.sub, ls = "RAOCUT"))
  
  write.table(tmp.output, file = gsub("\\.txt","-update.txt", chr.files[ii]),
              quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ":")
  rm(list=ls(pattern="tmp\\."))
}
