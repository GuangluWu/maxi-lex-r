#TODO
# avgByTrial not working, variable Freq not found?

library(plyr)

# Reutrns the number of transitions 
getRuns <- function(df) {
    df$ID <- as.character(df$ID)
    encoding <- rle(df$ID)
    ix <- c(1, (cumsum(encoding$lengths) + 1)[-length(encoding$lengths)])
    data.frame(lengths=encoding$lengths, values=encoding$values, ms=df[ix,"ms"])
}

# binning the data
binData <- function(df, binwidth=200){
    # Store Runs
    df.runs <- ddply(df, .(RespID, SessionID, ItemID), getRuns)
    df.runs$bin <- floor((df.runs$ms + (binwidth/2)) / binwidth) * binwidth
    df.runs$binf <- factor(df.runs$bin)
    data.frame(df.runs)
}

# helper function (returns a vector from xtabs)
binCount <- function(x) {
    xtabs(~binf, x)
}

#count the number of transitions per bin PER SESSION
binPerSess <- function(df){
    df.bin <- ddply(df, .(SessionID), binCount)
    df.bin.mx <- daply(df, .(SessionID), binCount)
    bins <- as.numeric(dimnames(df.bin.mx)$binf)
    return(bins)
}

# given a data frame, calculate mean number of transitions per bin
avgByTrial <- function(df) {
    # counting number of transitions per bin in df data BY TRIAL
    trial <- ddply(df, .(SessionID, ItemID), as.data.frame(binCount))
    # now for each SessionID, average over all the trials
    avg <- ddply(trial, .(SessionID), aggregate(Freq~binf, trial, mean))
    return(avg)
}

# we are counting transitions between regions
countTransitions <- function(df) {
    d1 <- subset(df, ms<0)
    d2 <- subset(df, ms>=0)
    data.frame(before=nrow(d1), after=nrow(d2))
}

setwd("D:/Dropbox/Maxi/R/r\ code/output")
load(file="pog.RData", verbose=TRUE)
load(file="stg.RData", verbose=TRUE)
load(file="droid.RData", verbose=TRUE)

pog.bins <- binPerSess(binData(pog.nox))
stg.bins <- binPerSess(binData(stg))
ttg.bins <- binPerSess(binData(droid))

pog.bin.avg <- avgByTrial(pog.runs)
stg.bin.avg <- avgByTrial(pog.runs)
ttg.bin.avg <- avgByTrial(pog.runs)

# let's reduce the data, get rid of data from really long trials
droid.red <- subset(droid.runs, ms>-9000 & ms<3000)
# now that we've reduced the data, we need to redefine binf
# because it will still have crazy levels (e.g., -81000 ms, +9000ms)
droid.red$binf <- factor(droid.red$binf)
bins.droid <- levels(droid.red$binf)


to.pdf <- function(expr, filename, ..., verbose=TRUE) {
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

graph <- function(df, bins, main = "Tracking", alpha=.5, xtop=2000, ytop=4, type='n'){
    subject <- split(df, df$SessionID)
    color <- rainbow(length(subject), alpha=alpha)
    plot(bins, rep(NA, length(bins)), xlim=c(-500,xtop), ylim=c(0,ytop), type=type,
        main = main, xlab = "Time before and after Word Onset (ms)", ylab = "# of Transitions")
    mapply(function(x, y) {
        points(bins, x$Freq, type='l', col=y)
    }, subject, color)
}
par(mfrow=c(3,1))

graph(df=res.bin.avg, bins=bins.pog, main="Eye Tracking", ytop=0.4)
graph(df=stg.bin.avg, bins=bins.stg, main="Scroll Tracking", ytop=0.4)
graph(df=droid.bin.avg, bins=bins.droid, main="Touch Tracking", ytop=0.4)