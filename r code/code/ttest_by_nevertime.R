library(plyr)
# Read data
trials <- read.csv("alldat.csv")
responses <- read.csv("dat.csv")
# Compute relative Onset
trials <- transform(trials, End = End - Start)
# Remove unneeded columns
trials$Start <- NULL
trials$Onset <- NULL
trials$Choice <- NULL
trials$X <- NULL
responses$X <- NULL
# Merge in the actual observations
all.data <- merge(trials, responses, by="RespID", all=TRUE)
# Compute time realtive to start
all.data <- transform(all.data, ms = End + ms)
# Group each session/trial and sort them interally by ms
all.data <- all.data[with(all.data, order(SessionID, RespID, ms)),]
# Find the ending time for each response (i.e. the starting time of the
# next response or the end of the trial).
# The following call results in the function transform being applied every
# each group of values (grouped by common SessionID/RespID), with an extra
# argument (apart from the group of values) to=c(tail(ms,-1), head(End,1))
# , that is to say, it adds a column called "to" to every group separately
# containing the the values specified below.
all.data <- ddply(all.data, c(.(SessionID), .(RespID)), transform, to=c(tail(ms,-1), head(End,1)))
#                                all but the first response start time ----^               ^
#                                                                 One copy of end time ----+
#                                This gives the starting times shifted one step back with
#                                the trial end time tucked on at the end.
# Clean up a little
all.data <- all.data[,c("SessionID", "RespID", "ms", "to", "AOI", "ItemID")]
# Compute amount of time in the interval 200-1000ms
clamp <- function(xs, from, to) pmax(from, pmin(to, xs))
all.data <- transform(all.data, in.interval = clamp(to, 200, 1000) - clamp(ms, 200, 1000))
# Extract the value with highest "in.interval" for each RespID, including none whenever
# there is no observation in the interval
all.data <- ddply( all.data, .(RespID), transform
                 , is.most = max(in.interval) > 0 & in.interval == max(in.interval) )
# Check whether you have to break ties, this will give an error if you do
stopifnot(all(!duplicated(subset(all.data, is.most, RespID))))
#
# Now, here's where I'm not exactly sure what it is you're after, but it sounded like you
# needed something like a table containing the proportion of each 200-1000ms response per
# participant, here we compute, seperately, the proportions for the 200-1000ms responses
# and the non 200-1000ms responses, for each participant
per.participant.props <- ddply( all.data, c(.(SessionID), .(is.most))
                              , function(xs) table(xs$AOI)/nrow(xs) )
# Make sure each SessionID/is.most combination is in the data, filling missing data with NA
per.participant.props <-
    merge(expand.grid( SessionID=unique(per.participant.props$SessionID)
                     , is.most=c(TRUE, FALSE) ), per.participant.props, all=TRUE)
# I can't think of a reasonable way to use a T-test, the closest I could imagine
# is comparing the different proportion informations to each other
with( subset(per.participant.props, is.most) # Extract the 200-1000ms responses
    , t.test(PComp, Unrl) )                  # Compare mean for PComp with mean for Unrl
# NOTE THAT AS FAR AS I CAN TELL THIS MAKES VERY LITTLE SENSE STATISTICALLY,
# AND I INCLUDE IT PURELY TO ILLUSTRATE HOW A CODE PERFORMING A T TEST LOOKS!
#
# Alternatively you might produce a paired T-test comparing the proportion of
# PComp that appear as "most viewed in 200-1000ms" to the proportion of PComp
# outwide "200-1000ms" for each participant. This still seems highly dubious
# to me, but at least makes slightly more sense.
# Note that since there's something fishy with the times, most sessions don't
# have ANYTHING observed in the 200-1000ms interval, just to make the code work
# I'll fill the missing values with 1/4, you should figure out what's wrong with
# the time computations instead
t.test( paired=T
      # Proportion of PComp among PComp Unrl observations within 200-1000ms interval
      , with(subset(per.participant.props,  is.most), PComp/(PComp+Unrl))
      # Proportion of PComp among PComp Unrl observations outside 200-1000ms interval
      , with(subset(per.participant.props, !is.most), PComp/(PComp+Unrl)) )
# NOTE AGAIN, THIS DOESN'T REALLY MAKE SENSE STATISTICALLY, BUT AT LEAST ILLUSTRATES
# THE RELEVANT R CODE!