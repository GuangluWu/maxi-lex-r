ff <- load(file="preprocessed.RData")
ff <- load(file="alldat.RData")

colnames(alldat) <- sub("\\.x$", "", colnames(alldat))
rownames(alldat) <- NULL

############## Fixation times per subject ###################

fixtime <- do.call("rbind", by(alldat, alldat$RespID, function(x) {
  ff <- aggregate(list(Msec=x$Msec), list(AOI=x$AOI), length)
  ttime <- 0
  ptime <- 0
  stime <- 0
  if ("Targ" %in% ff$AOI) & ("PComp" %in% ff$AOI) & ("SComp" %in% ff$AOI){
    ttime <- ff[ff$AOI=="Targ","Msec"]*24
    ptime <- ff[ff$AOI=="PComp", "Msec")*24
    stime <- ff[ff$AOI=="SComp", "Msec")*24
  } else {}
  ftime <- 0
  if ("Unrl" %in% ff$AOI) {
    ftime <- ff[ff$AOI=="Unrl","Msec"]*24
  } else {}
  data.frame(RespID=x[1,"RespID"], Targ=ttime, PComp=ptime, SComp=stime, Unrl=ftime)
}))

#fixation.time <- do.call("rbind", by(alldat, alldat$RespID, function(

fixdat <- merge(tinfo4[ ,c("SessionID","RespID")], fixtime)[ ,c("SessionID","RespID","Targ", "PComp", "SComp", "Unrl")]
write.csv(fixdat, file="fixdat.csv", row.names=FALSE)
# interference


# Fixation times over the whole experiment

scomp.avg <- mean(fixdat$Targ)
scomp.sd <- sd(fixdat$Targ)

unrl.avg <- mean(fixdat$Unrl)
unrl.sd <- sd(fixdat$Unrl)


# Crude measure of effect of Phon on Unrl
fixdat$eff <- fixdat$Phon-fixdat$Unrl
fixdat.agg <- with(fixdat, aggregate(list(eff=eff), list(SessionID=SessionID), mean))

# Cross tabulate SessionID (trial number) with Choice (Target, PComp, SComp, Unrl)
ff2 <- xtabs(~SessionID+Choice, tinfo4)
accs <- data.frame(SessionID=as.numeric(rownames(ff2)), Accuracy=ff2[,"Targ"]/48)

############### Accuratcy and Reaction Times ###################

# Reaction times
# mRT Mean reaction time after onset of the target word
# mPre Mean preview time before selecting the target word

# t0 is the Begining (Begin) of the trial
# t1 is the End (Select) of the trial
# Onset is the onset of the target word
rts <- do.call("rbind", by(tinfo4, tinfo4$SessionID, function(x) {
  data.frame(SessionID=x[1,"SessionID"], mRT=mean(x$t1-x$Onset), mPre=mean(x$Onset-x$t0))
}))

accinf <- merge(accs, rts)

tinfo4$Acc <- ifelse(tinfo4$Choice=="Targ",1,0)
tinfo4$RT <- tinfo4$t1-tinfo4$Onset
tinfo4$Preview <- tinfo4$Onset-tinfo4$t0

pdf(file="boxplots.pdf", width=16, height=8)
par(mfrow=c(1,2))
boxplot(Preview ~ SessionID, tinfo4, ylim=c(0,20000), main="Preview times for ")
boxplot(RT ~ SessionID, tinfo4, main="rt")
dev.off()

write.csv(accinf, file="accuracyrt.csv")