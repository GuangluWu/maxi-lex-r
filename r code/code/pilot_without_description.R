library(plyr)

getRuns <- function(x) {
    x$ID <- as.character(x$ID)
    ff <- rle(x$ID)
    ix <- c(1, (cumsum(ff$lengths)+1)[-length(ff$lengths)])
    data.frame(lengths=ff$lengths, values=ff$values, ms=x[ix,"ms"])
}

# we are counting transitions between regions
countTransitions <- function(x) {
    d1 <- subset(x, ms<0)
    d2 <- subset(x, ms>=0)
    data.frame(before=nrow(d1), after=nrow(d2))
}

# just convenience function (returns a vector from xtabs)
binCount <- function(x) {
    xtabs(~binf, x)
}

# same, but returns a data frame
binCount2 <- function(x) {
    as.data.frame(xtabs(~binf, x))
}

# given x (a single trial), calculate mean number of transitions per bin
avgByTrial <- function(x) {
    aggregate(Freq~binf, x, mean)
}


load(file="pog.RData", verbose=TRUE)
load(file="stg.RData", verbose=TRUE)
load(file="droid.RData", verbose=TRUE)

# colnames(droid) <- sub("AOI", "ID", colnames(droid))

# just renaming things
lookup <- data.frame(ID=c("Unrl","PComp","SComp","Targ"), newID=c("F","P","S","T"))
droid2 <- merge(droid, lookup, by="ID")
droid2 <- droid2[,setdiff(colnames(droid2),"ID")]
colnames(droid2) <- sub("newID","ID",colnames(droid2))
droid2 <- droid2[order(droid2$RespID, droid2$ms),]

# FOR TESTING FUNCTIONS:
# x <- subset(droid2, RespID==1)

# 'res' is the eye data, we first get runs
res <- ddply(pog, .(RespID,SessionID,ItemID), getRuns)
res.nox <- subset(res, values!="X")

# now we need to get the runs for the droid data
droid2.runs <- ddply(droid2, .(RespID, SessionID, ItemID), getRuns)

binwidth <- 200

# bin up the eye data
res.nox$bin <- floor((res.nox$ms+(binwidth/2))/binwidth)*binwidth
res.nox$binf <- factor(res.nox$bin)

# bin up the droid data
droid2.runs$bin <- floor((droid2.runs$ms+(binwidth/2))/binwidth)*binwidth
droid2.runs$binf <- factor(droid2.runs$bin)

# now, count the number of transitions per bin PER SESSION
res.bin <- ddply(res.nox, .(SessionID), binCount)
res.bin.mx <- daply(res.nox, .(SessionID), binCount)
bins <- as.numeric(dimnames(res.bin.mx)$binf)

# now, do the same thing, but do it by TRIAL (SessionID/ItemID)
res.bin.trial <- ddply(res.nox, .(SessionID, ItemID), binCount2)
# this is averaging over trials for each session, eye data
res.bin.avg <- ddply(res.bin.trial, .(SessionID), avgByTrial)

# this is counting number of transitions per bin in the droid data BY SESSION
droid.bin <- ddply(droid2.runs, .(SessionID), binCount)
droid.bin.mx <- daply(droid2.runs, .(SessionID), binCount)

# let's reduce the data, get rid of data from really long trials
droid3 <- subset(droid2.runs, ms>-9000 & ms<3000)
# now that we've reduced the data, we need to redefine binf
# because it will still have crazy levels (e.g., -81000 ms, +9000ms)
droid3$binf <- factor(droid3$binf)
bins.droid <- levels(droid3$binf)

# counting number of transitions per bin in droid data BY TRIAL
droid.bin.trial <- ddply(droid3, .(SessionID, ItemID), binCount2)
# now for each SessionID, average over all the trials
droid.bin.avg <- ddply(droid.bin.trial, .(SessionID), avgByTrial)

# break up the data into separate dataframes for plotting
subj <- split(res.bin.avg, res.bin.avg$SessionID)
colors <- rainbow(length(subj), alpha=.5)

subj.droid <- split(droid.bin.avg, droid.bin.avg$SessionID)
colors.droid <- rainbow(length(subj.droid), alpha=.5)

#pdf("subj2.pdf")

ytop <- 4
par(mfrow=c(1,2))
plot(bins, rep(NA, length(bins)), ylim=c(0,ytop), type='n', xlim=c(-500,2000))
mapply(function(x, y) {
    points(bins, x$Freq, type='l', col=y)
}, subj, colors)
#apply(res.bin.mx, 1, function(x) {
#    points(bins, x, type='l')
#})
#dev.off()
plot(bins.droid, rep(NA, length(bins.droid)), type='n', ylim=c(0,ytop), xlim=c(-500,2000))
mapply(function(x, y) {
    points(bins.droid, x$Freq, type='l', col=y, xlim=c(-500,2000))
}, subj.droid, colors.droid)


###########################################

binnedData <- xtabs(~bin, res.nox)

pdf("binnedData.pdf")

plot(binnedData, 
    main="Binned Data", 
    sub="A comparison of 3 data sets",
    xlab ="Time of onset",
    ylab ="Frequency", type='l', ylim=c(0,1000))
points(binnedData-100,
    type='l', 
    ylim=c(0,1000), 
    col='red')
abline(v=0, lty=2)
legend("topleft", legend=c("Eye tracking", "Button"), col=c("black","red"), lty=1)

dev.off()


res.count <- ddply(res.nox, .(RespID, SessionID, ItemID), countTransitions)
res.nrow <- ddply(res.nox, .(RespID, SessionID, ItemID), nrow)

droid.count <- ddply(droid2, .(RespID, SessionID, ItemID), countTransitions)
droid.nrow <- ddply(droid2, .(RespID, SessionID, ItemID), nrow)



# colnames(droid) <- sub("AOI", "ID", colnames(droid))

#Targ
#SComp
#PComp
#Unrl


# droid is a data frame
# How I want to map the values:
#
# Urnl Scomp Pcomp Targ
# F	 S	 P	 T

# map<-setNames(c("S","P","T","F"),c("SComp","PComp","Targ","Unrl"))
# droid <-within(droid, ID<-map[ID])
# head(droid, 10)

save(droid2, file="droid2.RData")

summary(res.nox$ms)
summary(stg$ms)
summary(droid$ms)

b.mean <- mean(res.count$before)
a.mean <- mean(res.count$after)

pdf("cat.pdf")

cat <- barplot(c(b.mean, a.mean), main="test",col=c("blue", "red"),)

dev.off()


barplot(c(b.mean,a.mean), horiz=TRUE, c)

pie(c(b.mean,a.mean))
