# TODO(mickey)
# barplot of the means of the transitions before/after onset for each data set
# subject variability ddply, subset functions SessionID = subject
# comparison of the transition probabilities

#http://tab.bz/5zlfv

# load		Reload datasets written with the function save
# verbose	should item names be printed during loading
# pog		point of gaze 			eyetracking data set
# stg		scroll tracking gaze	controller data set
# droid								smartphone data set
load(file="pog.sess.RData", verbose=TRUE)
load(file="stg.RData", verbose=TRUE)
load(file="droid.RData", verbose=TRUE)

# loads package
# plyr		uses the split-apply-combine pattern
library(plyr)

# Takes in a data frame and returns a data frame with the number of transitions
getRuns <- function(x) {
		# What is x
		# What is ID
		x$ID <- as.character(x$ID)	# coerces the argument (ie x$ID) to type char
		ff <- rle(x$ID)				# Run Lenght Encoding, computes lenght and vals of runs of equal vals in a vector
		# ix is a vector from 1 to 
		# Cumulative sums returns a vector whose elements are the cumulative sums (a,b,c...) -> (a,a+b, a+b+c, ...)
		ix <- c(1, (cumsum(ff$lenght)+1)[-lenght(ff$lenght)])
		# creates a data frame with row names lenghts | values | ms
		# lenghts is an int of occurances of values
		# values is a char that stores (X, T, P, F. S)
		# ms is the onset of the transition
		data.frame(lenghts=ff$lenghts, values=ff$values, ms=x[ix,"ms"])
}

# I/O d - data frame
# split pog data frame by RespID, SessionID, ItemID
# apply the getRuns function
res <- ddply(pog.sess, .RespID,SessionID,ItemID),getRuns)

# Data from with no X (ie nox)
res.nox <- subset(res, values!="X")

# Bin is like a time window (onset at -8000ms to 200-250-500ms)
# binwidth denotes the size of the bin in ms
binwidth <- 200
res.nox$bin <- floor((res.nox$ms+(binwidth/2))binwidth*binwidth)
res.nox$binf <- factor(res.nox$bin)

res.nox <- subset(res, values!="X")

binwidth <- 200
res.nox$bin <- floor((res.nox$ms+(binwidth/2))/binwidth)*binwidth
res.nox$binf <- factor(res.nox$bin)

binCount <- function(x) {
    xtabs(~binf, x)
}

#colnames(droid) <- sub("AOI", "ID", colnames(droid))

# just renaming things
lookup <- data.frame(ID=c("Unrl","PComp","SComp","Targ"), newID=c("F","P","S","T"))
droid2 <- merge(droid, lookup, by="ID")
droid2 <- droid2[,setdiff(colnames(droid2),"ID")]
colnames(droid2) <- sub("newID","ID",colnames(droid2))
droid <- droid[order(droid$RespID, droid$ms),]


res.bin <- ddply(res.nox, .(SessionID), binCount)
res.bin.mx <- daply(res.nox, .(SessionID), binCount)
bins <- as.numeric(dimnames(res.bin.mx)$binf)

subj <- split(t(res.bin.mx), rep(1:ncol(t(res.bin.mx)), each=nrow(t(res.bin.mx))))
colors <- rainbow(length(subj), alpha=.5)
pdf("subj2.pdf")
plot(bins, rep(NA, length(bins)), type='n', ylim=c(0,50), xlim=c(-500,2000))
mapply(function(x, y) {
    points(bins, x, type='l', col=y)
}, subj, colors)

apply(res.bin.mx, 1, function(x) {
    points(bins, x, type='l')
})
dev.off()
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

countTransitions <- function(x) {
    d1 <- subset(x, ms<0)
    d2 <- subset(x, ms>=0)
    data.frame(before=nrow(d1), after=nrow(d2))
}

ddply(res.nox, .(RespID, SessionID, ItemID), countTransitions)
ddply(stg, .(RespID, SessionID, ItemID), countTransitions)

ddply(res.nox, .(RespID, SessionID, ItemID), nrow)
dev.off()


# Last itteration

res.count <- ddply(res.nox, .(RespID, SessionID, ItemID), countTransitions)

res.nrow <- ddply(res.nox, .(RespID, SessionID, ItemID), nrow)

############################################

#Targ
#SComp
#PComp
#Unrl

is.factor(droid$ID)
class(droid$ID)

# droid is a data frame
# How I want to map the values:
# http://tab.bz/j5rez
# Urnl Scomp Pcomp Targ
# F  S   P   T

map<-setNames(c("S","P","T","F"),c("SComp","PComp","Targ","Unrl"))
droid <-within(droid, ID<-map[ID])
head(droid, 10)
save(droid, file="droid.RData")

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

# Maxi fork
library(plyr)

binEyeData <- function(filename) {
    if (file.exists(filename)){
    vis.dat <- ddply(load("filename"), .(RespID,SessionID,ItemID), getRuns)

    }
    else{
    warning("File does not exist:", filename)
    }
}

getRuns <- function(vis.dat) {
    vis.dat$ID <- as.character(x$ID)
    fixations <- rle(vis.dat$ID)
    ix <- c(1, (cumsum(ff$lenghts)+1)[-lenght(fixations$lenght)])
    data.frame(lenghts=fixations$lenghts, values=fixations$values, ms=x[ix,"ms"])
}

binCount <- function(vis.dat, binwidth=200) {
    vis.dat$bin <- floor((vis.dat$ms+(binwidth/2))/binwidth)*binwidth
}

storeRuns <- function(df){
    ddply(df, .(RespID, SessionID, ItemID), getRuns)
}