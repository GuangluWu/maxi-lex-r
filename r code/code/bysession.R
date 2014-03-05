system("rm -v bysession/*")
load(file="preprocessed.RData")
load(file="alldat.RData")

colnames(alldat) <- sub("\\.x$", "", colnames(alldat))
rownames(alldat) <- NULL

# Remove duplicate RespID
alldat[1] <- NULL
dat.session <- merge(alldat, tinfo4[,c("SessionID","RespID")])

bins <- seq(0,1488,24)
lineinfo <- list(PComp=list(mpch=1,mcol='blue'),
                 SComp=list(mpch=2,mcol='green'),
                 Targ=list(mpch=3,mcol='red'),
                 Unrl=list(mpch=4,mcol='gray50'))

by(dat.session, dat.session$SessionID, function(x) {  
  mx <- as.matrix(xtabs(~AOI+Msec, x))
  props <- mx / apply(mx, 2, sum)
  sname <- sprintf("s%07d", x[1,"SessionID"])
  sfile <- paste("bysession/", sname, ".pdf", sep="")
  pdf(file=sfile)
  plot(bins, rep(NA, length(bins)), ylim=c(0,1), type='n',
       xlab="Time from Onset (ms)", ylab="pGaze",
       main=sname)
  #lapply(setdiff(rownames(props), "SComp"), function(x) {
  lapply(rownames(props), function(x) {
    inf <- lineinfo[[x]]
    points(bins, props[x,], type='b', pch=inf$mpch, col=inf$mcol)
  })
  dev.off()
})
