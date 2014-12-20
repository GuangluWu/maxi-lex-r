## T-test

library(plyr)
setwd("/output")
load(file = "preprocessed.RData", verbose = TRUE)
load(file = "alldat.RData", verbose = TRUE)

# Read data
trials <- tinfo4
responses <- alldat

# Change column names
names(trials)[4] <- "Start"
names(trials)[6] <- "End"

# Merge in the actual observations
cols.keep <- setdiff(colnames(trials), c("Start", "Onset", "End"))
all.data <- merge(trials[ ,cols.keep], responses, by = "RespID")

all.data$phon <- 1*(all.data$AOI=="PComp")
all.data$sem <- 1*(all.data$AOI=="SComp")
all.data$unrl <- 1*(all.data$AOI=="Unrl")

prop.mean <- with(all.data, aggregate(list(PComp=phon, Unrl=unrl, SComp=sem),
                         list(SessionID=SessionID),
                         mean))

t.test(prop.mean$PComp, prop.mean$Unrl, paired=TRUE)
t.test(prop.mean$SComp, prop.mean$Unrl, paired=TRUE)

g.means <- c(mean(prop.mean$PComp), mean(prop.mean$SComp), mean(prop.mean$Unrl))

g.sd <- c(sd(prop.mean$PComp), sd(prop.mean$SComp), sd(prop.mean$Unrl))

l.means <- logit(g.means) * (-1)
logit(g.sd)

b.means <- c (0.2356355, 0.2294562, 0.2197686)

barplot(g.means,ylim = c(0,1), main="Mean proportion for competitors", 
        names.arg = c("Phonological", "Semantic", "Distractor"), col = c("gray1", "gray45", "gray90"))

#library(ggplot2)
#gg.means <- data.frame(PComp=mean(prop.mean$PComp), SComp=mean(prop.mean$SComp), Unrl=mean(prop.mean$Unrl))
#ggplot(melt(gg.means,id.var=c()), aes(x=,y=Value)) + geom_bar()

# When the data contains y values in a column, use stat="identity"
#library(plyr)
# Calculate the mean mpg for each level of cyl
#mm <- ddply(mtcars, "cyl", summarise, mmpg = mean(mpg))
#ggplot(mm, aes(x = factor(), y = mmpg)) + geom_bar(stat = "identity")
