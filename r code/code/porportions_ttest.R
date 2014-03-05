## Classic house keeing

#setwd("D:/Dropbox/Maxi/R/preprocessing")
#load(file="preprocessed.RData")
head(tinfo4)

## Working out the proportions for looking at a specific object

# Phonological competitors (storing proportions in a new column)
tinfo4$PProp <- 1*(tinfo4$Choice == "PComp")
prop.phon <- tinfo4$PProp
cat('The proportion for the phonological competitors is', mean(prop.phon), '\n')
#tinfo4$New <- with(tinfo4, ave(PProp, SessionID) )

# Semantic competitors (storing proportions in a new colum)
tinfo4$SProp <- 1*(tinfo4$Choice == "SComp")
prop.sem <- tinfo4$SProp
cat('The proportion for the sematic competitors is', mean(prop.sem), '\n')

# Unrelated word (and again...)
tinfo4$UProp <- 1*(tinfo4$Choice == "Unrl")
prop.unrl <- tinfo4$UProp
cat('The proportion for the unrelated word is', mean(prop.unrl), '\n')

## Performing a PAIRED T-test on the proportions

t.test(prop.phon, prop.unrl, paired=TRUE)
t.test(prop.phon, prop.unrl, paired=TRUE)