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
setwd("D:/Dropbox/Maxi/R/r\ code/output")
load(file="pog.RData", verbose=TRUE)
load(file="stg.RData", verbose=TRUE)
load(file="droid.RData", verbose=TRUE)

#head(pog.nox)
#head(res.nox)
#head(stg)
#head(droid)

# colnames(droid) <- sub("AOI", "ID", colnames(droid))

# just renaming things
lookup <- data.frame(ID=c("Unrl","PComp","SComp","Targ"), newID=c("F","P","S","T"))
droid2 <- merge(droid, lookup, by="ID")
droid2 <- droid2[,setdiff(colnames(droid2),"ID")]
colnames(droid2) <- sub("newID","ID",colnames(droid2))
droid <- droid[order(droid$RespID, droid$ms),]

# FOR TESTING FUNCTIONS:
# x <- subset(droid2, RespID==1)

# 'res' is the eye data, we first get runs
res <- ddply(pog, .(RespID,SessionID,ItemID), getRuns)
res.nox <- subset(res, values!="X")

# stg is the scroll tracking data, we get runs for it too
stg.runs <- ddply(stg, .(RespID,SessionID,ItemID), getRuns)

# now we need to get the runs for the droid data
droid.runs <- ddply(droid, .(RespID, SessionID, ItemID), getRuns)

binwidth <- 200

# bin up the eye data
res.nox$bin <- floor((res.nox$ms+(binwidth/2))/binwidth)*binwidth
res.nox$binf <- factor(res.nox$bin)

# bin up the scroll data
stg.runs$bin <- floor((stg.runs$ms+(binwidth/2))/binwidth)*binwidth
stg.runs$binf <- factor(stg.runs$bin)

# bin up the droid data
droid.runs$bin <- floor((droid.runs$ms+(binwidth/2))/binwidth)*binwidth
droid.runs$binf <- factor(droid.runs$bin)

# now, count the number of transitions per bin PER SESSION
res.bin <- ddply(res.nox, .(SessionID), binCount)
res.bin.mx <- daply(res.nox, .(SessionID), binCount)
bins <- as.numeric(dimnames(res.bin.mx)$binf)

# now, do the same thing, but do it by TRIAL (SessionID/ItemID)
res.bin.trial <- ddply(res.nox, .(SessionID, ItemID), binCount2)
# this is averaging over trials for each session, eye data
res.bin.avg <- ddply(res.bin.trial, .(SessionID), avgByTrial)

# counting the number of transitions per bin in the stg data PER SESSION
stg.bin <- ddply(stg.runs, .(SessionID), binCount)
stg.bin.mx <- daply(stg.runs, .(SessionID), binCount)
bins.stg <- as.numeric(dimnames(stg.bin.mx)$binf) 

# same but for the STG data
stg.bin.trial <- ddply(stg.runs, .(SessionID, ItemID), binCount2)
stg.bin.avg <- ddply(stg.bin.trial, .(SessionID), avgByTrial)

# this is counting number of transitions per bin in the droid data PER SESSION
droid.bin <- ddply(droid.runs, .(SessionID), binCount)
droid.bin.mx <- daply(droid.runs, .(SessionID), binCount)

# let's reduce the data, get rid of data from really long trials
droid.red <- subset(droid.runs, ms>-9000 & ms<3000)
# now that we've reduced the data, we need to redefine binf
# because it will still have crazy levels (e.g., -81000 ms, +9000ms)
droid.red$binf <- factor(droid.red$binf)
bins.droid <- levels(droid.red$binf)

# counting number of transitions per bin in droid data BY TRIAL
droid.bin.trial <- ddply(droid.red, .(SessionID, ItemID), binCount2)
# now for each SessionID, average over all the trials
droid.bin.avg <- ddply(droid.bin.trial, .(SessionID), avgByTrial)

# break up the data into separate dataframes for plotting
subj <- split(res.bin.avg, res.bin.avg$SessionID)
colors <- rainbow(length(subj), alpha=.5)

subj.stg <- split(stg.bin.avg, stg.bin.avg$SessionID)
colors.stg <- rainbow(length(subj.stg), alpha=.5)

subj.droid <- split(droid.bin.avg, droid.bin.avg$SessionID)
colors.droid <- rainbow(length(subj.droid), alpha=.5)

pdf("3-way comparison.pdf")

ytop <- 4
par(mfrow=c(3,1))

# Plot for Eye tracking data (POG)
plot(bins, rep(NA, length(bins)), ylim=c(0,ytop), type='n', xlim=c(-500,2000),
	main = "Eye tracking", xlab = "Number of transitions", ylab = "Frequency")
mapply(function(x, y) {
    points(bins, x$Freq, type='l', col=y)
}, subj, colors)

# Plot for Scroll tracking data (STG)
plot(bins.stg, rep(NA, length(bins.stg)), type='n', ylim=c(0,ytop), xlim=c(-500,2000),
	main = "Scroll tracking", xlab = "Number of transitions", ylab = "Frequency")
mapply(function(x, y) {
    points(bins.stg, x$Freq, type='l', col=y, xlim=c(-500,2000))
}, subj.stg, colors.stg)

# Plot Touch tracking (DROID)
plot(bins.droid, rep(NA, length(bins.droid)), type='n', ylim=c(0,ytop), xlim=c(-500,2000),
	main = "Touch tracking", xlab = "Number of transitions", ylab = "Frequency")
mapply(function(x, y) {
    points(bins.droid, x$Freq, type='l', col=y, xlim=c(-500,2000))
}, subj.droid, colors.droid)

dev.off()
###########################################

binnedData <- function(df){
    xtabs(~bin, df)
}

a <- binnedData(res.nox)
#b <- binnedData(bins.droid)

pdf("binnedData.pdf")

plot(a, 
    main="Individual Differences", 
    sub="A comparison of 3 data sets",
    xlab ="Time of onset",
    ylab ="Frequency", type='l', ylim=c(0,1000))
points(b,
    type='l', 
    ylim=c(0,1000), 
    col='red')
abline(v=0, lty=2)
legend("topleft", 
    legend=c("Eye tracking", "Scroll tracking", "Touch tracking"), 
    col=c("black","red"), lty=1)

dev.off()

#pdf("cat.pdf")
res.count <- ddply(res.nox, .(RespID, SessionID, ItemID), countTransitions)
stg.count <- ddply(stg, .(RespID, SessionID, ItemID), countTransitions)
droid.count <- ddply(droid.red, .(RespID, SessionID, ItemID), countTransitions)

# Barplots
pog.bar <- barplot(c(mean(res.count$before), mean(res.count$after)), main="test",col=c("blue", "red"), width = c(1,1), ylim = c(0, 16))
stg.bar <- barplot(c(mean(stg.count$before), mean(stg.count$after)), main="test",col=c("blue", "red"), width = c(1,1), ylim = c(0, 16))
droid.bar <- barplot(c(mean(droid.count$before), mean(droid.count$after)), main="test",col=c("blue", "red"), width = c(1,1), ylim = c(0, 16))

#dev.off()