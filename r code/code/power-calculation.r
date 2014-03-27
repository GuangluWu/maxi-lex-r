library(pwr)

# Compute the effect size for two proportions
ES.h(prop.pcomp, prop.scomp)

# Cohen's Effect Size for a t-test
es <- cohen.ES(test = "t", size = "medium")$effect.size

pwr.t.test(n=26,d=0.5,sig.level=.05, type= "paired", alternative = "greater")


subset(mtcars, cyl==6)
subset(mtcars, wt==2.770)

#setwd("D:/Dropbox/Maxi/R/r\ code/output")
