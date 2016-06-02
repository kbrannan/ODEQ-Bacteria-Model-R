## get area for pls and ils from uci
## first two numbers of pls are watershed number
## third digit in pls number is lu type

## path to uci
chr.uci.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts"

## uci file
chr.uci.file <- "bigelkwq.uci"

## get file
chr.uci.raw <- scan(file = paste0(chr.uci.dir, "/", chr.uci.file),
                    what = "character", sep = "\n")

## get schematic block
chr.schematic.blk <- chr.uci.raw[grep("^SCHEMATIC", chr.uci.raw):
                                   grep("^END SCHEMATIC", chr.uci.raw)]
## get rid of copy lines
chr.schematic.blk <- chr.schematic.blk[-1 * grep("COPY", chr.schematic.blk)]

## PERLND
## get PERLND block
chr.perlnd.blk <- chr.uci.raw[grep("^PERLND {0,}$", chr.uci.raw):
              grep("^END PERLND {0,}$", chr.uci.raw)]
## get gen-info block
chr.perlnd.gen.info.blk <- chr.perlnd.blk[(grep("^ {1,}GEN-INFO", chr.perlnd.blk) + 1):
              (grep("^ {1,}END GEN-INFO", chr.perlnd.blk) - 1)]

## IMPLND
## get IMPLND block
chr.implnd.blk <- chr.uci.raw[grep("^IMPLND {0,}$", chr.uci.raw):
                                grep("^END IMPLND {0,}$", chr.uci.raw)]
## get gen-info block
chr.implnd.gen.info.blk <- chr.implnd.blk[(grep("^ {1,}GEN-INFO", chr.implnd.blk) + 1):
                                            (grep("^ {1,}END GEN-INFO", chr.implnd.blk) - 1)]

## wtsd numbers
num.wtsd <- 1:18


df.areas.wq <- data.frame()

## get pls lines for sub-wtsd
for(ii in 1:length(num.wtsd)) {
  tmp.schem.chr <- chr.schematic.blk[grep(paste0(" RCHRES {1,}",sprintf(fmt = " %i ", num.wtsd[ii])), chr.schematic.blk)]
  if(1 * length(grep("^ {0,}RCHRES", tmp.schem.chr)) > 0) tmp.schem.chr <- tmp.schem.chr[-1 * grep("^ {0,}RCHRES", tmp.schem.chr)]
  tmp.schem <- data.frame(
    do.call(
      rbind,strsplit(gsub(" {1,}",",",tmp.schem.chr), split = ","))[, c(2,3)], stringsAsFactors = FALSE)
  names(tmp.schem) <- c("ls", "area")
  tmp.schem$area <- as.numeric(tmp.schem$area)
  
  tmp.pls.gen.info <- data.frame(do.call(rbind,strsplit(gsub(" {1,}",",",chr.perlnd.gen.info.blk[grep(paste0("^ {0,} ",sprintf(fmt = "%02i", num.wtsd[ii])), chr.perlnd.gen.info.blk)]), split = ","))[, c(2,5)], stringsAsFactors = FALSE)
  names(tmp.pls.gen.info) <- c("ls", "desc")
  
  tmp.pls.merge <- merge(tmp.pls.gen.info,tmp.schem)
  
  df.areas.wq <- rbind(
    df.areas.wq,
    data.frame(wtsd = sprintf(fmt = "%02i", num.wtsd[ii]), 
               ls = "forest",
               area = sum(tmp.pls.merge$area[grep("[fF]orest", 
                                                  tmp.pls.merge$desc)])),
    data.frame(wtsd = sprintf(fmt = "%02i", num.wtsd[ii]), 
               ls = "pasture",
               area = sum(tmp.pls.merge$area[grep("[pP]asture", 
                                                  tmp.pls.merge$desc)])),
    data.frame(wtsd = sprintf(fmt = "%02i", num.wtsd[ii]),
               ls = "developed",
               area = sum(tmp.pls.merge$area[grep("[dD]eveloped", 
                                                  tmp.pls.merge$desc)])),
    data.frame(wtsd = sprintf(fmt = "%02i", num.wtsd[ii]),
               ls = "ils",
               area = sum(tmp.schem$area[grep("301|302", tmp.schem$ls)])))
  
  
  rm(list=ls(pattern="^tmp\\."))
}


df.areas.wq <- df.areas.wq[df.areas.wq$area != 0,]

## save areas data.frame
save(df.areas.wq, file = paste0(chr.uci.dir, "/sub-models/Wildlife-Racoon/sub-wtsd-areas.RData"))

rm(list = ls()[-1 * grep("df.areas.wq|df.areas.hyd", ls())])

