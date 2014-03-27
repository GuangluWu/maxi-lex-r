setwd("D:/Dropbox/Maxi/R/r\ code/output/")

library(XML)
library(plyr)
# Read in audiotimings file and store item id
atime <- read.csv("audiotimings.csv")
atime$ItemID <- as.numeric(substr(atime$Wavfile, 2, 3))
options(stringsAsFactors=FALSE)

# Helper function for loadit and loaddat
getfile <- function(x, ext=NULL) {
  return(paste(paste(mypath, x$base, sep="/"), ext, "txt", sep="."))
}

# Trial BEGINs
# participant moves fingers (MOVE | DOWN)
# word.onset of audio recording (SYNCTIME)
# image selection is made # (SELECT)

# This info is contained in one txt file
# which is separated into smaller files, namely:
# Events that occur consistantly but rarely
# [sessID].begin.txt
# [sessID].select.txt
# [sessID].sync.txt
#
# Events that occur inconsistantly and rapidly
# [sessID].dat.txt

# This function is too complex break it down into separate ones
loadit <- function(x) {
    trial.start <- read.table(getfile(x, "begin"))
    if (is.null(tryCatch(trial.end <- read.table(getfile(x, "select")),error=function(e) {NULL}))) {
        res <- data.frame(SessionID=NA,ItemID=NA,trial.start=NA,trial.end=NA,sync=NA,wav=NA,Choice=NA)[-1,]
    } else {
        if (nrow(trial.end)!=96) {
            res <- data.frame(SessionID=NA,ItemID=NA,trial.start=NA,trial.end=NA,sync=NA,wav=NA,Choice=NA)[-1,]
        } else {
            t2 <- read.table(getfile(x, "sync"))
            res0 <- data.frame(SessionID=x$SessionID, ItemID=trial.start$V3, trial.start=trial.start$V1, trial.end=trial.end$V1)
            res <- ddply(res0, .(SessionID, ItemID, trial.start, trial.end), function(piece) {
                selectedrow <- subset(t2, V1>piece$trial.start & V1<piece$trial.end)
                if (nrow(selectedrow)==1) {
                    piece$sync <- selectedrow$V1
                    piece$wav <- selectedrow$V3
                } else {
                    piece$sync <- NA
                    piece$wav <- NA
                }
                return(piece)
            }, .inform=TRUE)
            res$Choice <- trial.end$V3
        }
    }
    return(res)
}

# Get the movement information (MOVE | DOWN)
loaddat <- function(x) {
  ff <- read.table(getfile(x, "dat"))
  data.frame(SessionID=x$SessionID, Msec=ff$V1, Loc=(ff$V4 > 0)*2 + (ff$V3 > 0))
}

# Read items.csv 
item.display <- read.csv(file="items.csv", header=FALSE)
colnames(item.display) <- c("ItemID","CfgID","Targ","PComp","SComp","Unr")

itm2 <- with(item.display,
             rbind(data.frame(ItemID=ItemID, AOI="Targ", Res=Targ),
                   data.frame(ItemID=ItemID, AOI="PComp", Res=PComp),
                   data.frame(ItemID=ItemID, AOI="SComp", Res=SComp),
                   data.frame(ItemID=ItemID, AOI="Unrl", Res=Unr)))
itm2 <- itm2[order(itm2$ItemID, itm2$AOI),]

# Read items.xml file from Android app
# item is an experimental display which cointains 4 x AOIs and 1 x Speech
# It contains item IDs, AOIs, Speech
item.location <- xmlToDataFrame("items.xml")
item.location2 <- with(item.location,
                rbind(data.frame(ItemID=1:nrow(item.location), Loc=0, Res=aoi0),
                      data.frame(ItemID=1:nrow(item.location), Loc=1, Res=aoi1),
                      data.frame(ItemID=1:nrow(item.location), Loc=2, Res=aoi2),
                      data.frame(ItemID=1:nrow(item.location), Loc=3, Res=aoi3)))

itmall <- merge(itm2, item.location2)
itmall <- itmall[order(itmall$ItemID, itmall$AOI),]
itmall <- subset(itmall, ItemID<=48)

basefiles <- sub("\\.txt$", "", list.files("response", "[^\\.begin|\\.dat|\\.sync]\\.txt$"))
mypath <- "response"

sessIDs <- as.numeric(substr(basefiles, 2, 8))

todo <- data.frame(SessionID=sessIDs,
                   base=basefiles, stringsAsFactors=FALSE)

tinfo <- subset(ddply(todo, .(SessionID), loadit, .inform=TRUE), ItemID<=48 & !is.na(sync))


#tinfo <- subset(do.call("rbind", by(todo, todo$SessionID, loadit)), ItemID<=48)

todo2 <- merge(todo, data.frame(SessionID=unique(tinfo$SessionID)))
dat <- do.call("rbind", by(todo2, todo2$SessionID, loaddat))

tinfo$RespID <- 1:nrow(tinfo)
dat2 <- do.call("rbind", by(tinfo, tinfo$RespID, function(x) {
  ff <- subset(dat, Msec>=x$trial.start & Msec<=x$trial.end)
  ff$RespID <- x$RespID
  return(ff)
}))
rownames(dat2) <- NULL

dat2a <- rbind(dat2,
               data.frame(SessionID=tinfo$SessionID, Msec=tinfo$trial.end,
                          Loc=tinfo$Choice, RespID=tinfo$RespID))
dat2a <- dat2a[order(dat2a$RespID, dat2a$Msec),]

tinfo.atime <- merge(tinfo[ ,c("RespID","ItemID","sync")], atime[ ,c("ItemID","word.onset_CW")])
tinfo.atime$word.onset <- tinfo.atime$sync+tinfo.atime$word.onset_CW
tinfo2 <- merge(tinfo.atime[ ,c("RespID","word.onset")], tinfo)
tinfo3 <- merge(tinfo2, itmall[ ,c("ItemID","AOI","Loc")], by.x=c("ItemID","Choice"),
      by.y=c("ItemID","Loc"))
tinfo4 <- tinfo3[order(tinfo3$RespID),c("SessionID","RespID","ItemID","TrialStart","WordOnset","TrialEnd","AOI")]
colnames(tinfo4) <- sub("AOI","Choice",colnames(tinfo4))
rownames(tinfo4) <- NULL
                                                             
dat3 <- merge(dat2a, tinfo.atime[ ,c("RespID","ItemID","word.onset")])
dat3$ms <- dat3$Msec-dat3$word.onset
dat4 <- merge(dat3[ ,c("RespID","ms","ItemID","Loc")], itmall[ ,c("ItemID","Loc","AOI")])
dat4 <- dat4[order(dat4$RespID, dat4$ms),c("RespID","ms","AOI")]
rownames(dat4) <- NULL

save(tinfo4, dat4, file="preprocessed.RData")
