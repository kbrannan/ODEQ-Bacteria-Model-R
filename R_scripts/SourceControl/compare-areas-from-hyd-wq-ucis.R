## check to see the areas for forest, pasture, raocut and ils from wq-uci equal 
## the areas in the hyd-uci

## packages
library(gridExtra, quietly = TRUE)

## get areas from ucis
## hydrologic uci
## path for analysis
chr.compare.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/SourceControl"
source(file = paste0(chr.compare.dir, "/get-areas-from-hyd-uci.R"))
## wq uci
## path for analysis
chr.compare.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/SourceControl"
source(file = paste0(chr.compare.dir, "/get-areas-from-wq-uci.R"))

## compare and report
df.compare <- cbind(df.areas.hyd[, c("wtsd","ls")],
                    match.wtsd = df.areas.hyd$wtsd == df.areas.wq$wtsd,
                    match.ls = df.areas.hyd$ls == df.areas.wq$ls,
                    match.area = df.areas.hyd$area == df.areas.wq$area,
                    hyd.area = df.areas.hyd$area,
                    wq.area = df.areas.wq$area)
## output coamparison table
chr.compare.dir <- "M:/Models/Bacteria/HSPF/ODEQ-Bacteria-Model-R/R_scripts/SourceControl"
pdf(file = paste0(chr.compare.dir, "/compare-areas-from-hyd-wq-ucis.pdf"), 
    height = 11, width = 8.5, onefile = TRUE)

tmp.table <- tableGrob(df.compare[1:30, ], show.rownames = FALSE,
                       gp=gpar(fontsize=10))
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "Compare areas from Hyd and WQ UCI files",
                      y=unit(0.51,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=10))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.newpage()
grid.draw(tmp.gt)
rm(list=ls(pattern="^tmp\\."))

tmp.table <- tableGrob(df.compare[31:60, ], show.rownames = FALSE,
                       gp=gpar(fontsize=10))
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "Compare areas from Hyd and WQ UCI files (continued)",
                      y=unit(0.51,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=10))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.newpage()
grid.draw(tmp.gt)
rm(list=ls(pattern="^tmp\\."))

tmp.table <- tableGrob(df.compare[61:69, ], show.rownames = FALSE,
                       gp=gpar(fontsize=10))
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "Compare areas from Hyd and WQ UCI files (continued)",
                      y=unit(0.51,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=10))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.newpage()
grid.draw(tmp.gt)
rm(list=ls(pattern="^tmp\\."))
dev.off()