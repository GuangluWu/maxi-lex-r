

# TODO fix file paths
ff <- load(file="preprocessed.RData", verbose=TRUE)
ff <- load(file="alldat.RData")

# New line here
droid <- merge(dat4, tinfo4[,c("RespID", "SessionID", "ItemID")])

dat.trans <- do.call("rbind", by(dat4, dat4$RespID, function(x) {
  x.runs <- rle(x$AOI)
  run.ix <- rep(1:length(x.runs$lengths), x.runs$lengths)
  x.tmx <- with(x, aggregate(list(ms=ms), list(RunID=run.ix, AOI=AOI), min))
  x.tmx <- cbind(RespID=x[1,"RespID"], x.tmx[order(x.tmx$ms),])
  rbind(x.tmx, data.frame(RespID=x[1,"RespID"],RunID=x.tmx[nrow(x.tmx),"RunID"]+1,
                          AOI=x[nrow(x),"AOI"], ms=x[nrow(x),"ms"]))
}))
rownames(dat.trans) <- NULL

dat.state <- do.call("rbind", by(dat.trans, dat.trans$RespID, function(x) {
  ff <- cbind(x[1:(nrow(x)-1),], ms2=x[2:nrow(x),"ms"])
  ff$dur <- ff$ms2-ff$ms
  ff
}))
rownames(dat.state) <- NULL

mins <- tapply(dat.trans$ms, list(RespID=dat.trans$RespID), min)
prelen <- data.frame(RespID=as.numeric(names(mins)), plen=mins)

sess.pre <- with(merge(prelen, tinfo4[,c("SessionID", "RespID")]),
                 aggregate(list(mPre=-plen), list(SessionID=SessionID), mean))

sess.pre.sd <- with(merge(prelen, tinfo4[,c("SessionID", "RespID")]),
                    aggregate(list(sPre=plen), list(SessionID=SessionID), sd))

resp.pre.trans <- do.call("rbind", by(dat.state, dat.state$RespID, function(x) {
  data.frame(RespID=x[1,"RespID"], nt=nrow(subset(x, ms>=-5000 & ms2<=0)))
}))

resp.trans.rate <- do.call("rbind", by(dat.state, dat.state$RespID, function(x) {
  data.frame(RespID=x[1,"RespID"], mGaze=mean(subset(x, ms>=-5000)$dur))
}))

sess.pre.trans <- with(merge(resp.pre.trans, tinfo4[,c("SessionID","RespID")]),
                       aggregate(list(mNT=nt), list(SessionID=SessionID), mean))

sess.gaze <- with(merge(resp.trans.rate, tinfo4[,c("SessionID","RespID")]),
                  aggregate(list(mGaze=mGaze), list(SessionID=SessionID), mean))

sessinfo <- merge(merge(merge(sess.pre, sess.pre.sd), sess.pre.trans), sess.gaze)

write.csv(sessinfo, file="sessinfo.csv", row.names=FALSE)
