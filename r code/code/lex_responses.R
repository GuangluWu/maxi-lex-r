## This fuction stores the responses for all trials
## It creates frames (intervals of 24ms)
# head()
#   RespID Msec  AOI
#1      1    0 Targ
#2      1   24 Targ
#3      1   48 Targ
#4      1   72 Targ
#5      1   96 Targ
#6      1  120 Targ


load(file="preprocessed.RData")

library(plyr)

toFrames <- function(x) {
  ff <- subset(x, ms < 0)
  if (nrow(ff) == 0) {
    return (NULL)
  } else {}
  min.ms <- max(subset(x, ms < 0)$ms)
  if (max(x$ms) < 1500) {
    # add in "fake" frame after
    x <- rbind(x, data.frame(RespID=x[1,"RespID"], ms=1501, AOI=x[nrow(x),"AOI"]))
  }
  ff <- subset(x, ms>1500)
  if (nrow(ff)==0) {
    return (NULL)
  } else {}
  max.ms <- min(subset(x, ms > 1500)$ms)
  ff <- subset(x, ms >= min.ms & ms <= max.ms)
  ff2 <- merge(cbind(ff,ix=2:(nrow(ff)+1)), cbind(ff,ix=1:nrow(ff)), by="ix")
  ff3 <- merge(ff2, data.frame(frame=seq(0,1500,24)))
  subset(ff3, Msec >= ms.x & frame < ms.y)[ ,c("RespID.x","Frame","AOI.x")]    
}

alldat <- ddply(dat4, .(RespID), to.frames(), .inform=TRUE)

save(alldat, file="alldat111.RData")