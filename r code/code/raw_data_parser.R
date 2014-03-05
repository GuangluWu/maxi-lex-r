##Set working dir
##C:/Users/Mickey/Desktop/data
setwd("")

##Read files named s0000010.txt, s0000011.txt etc.
filenames <- list.files(path="../Data/raw_data",
    pattern="s000000+.*txt")

##Create list of data frame names without the ".txt" part 
namez <-substr(filenames,1,7)

###Load all files
for(i in namez){
    filepath <- file.path("C:/Users/Mickey/Desktop",paste(i,".txt",sep=""))
    assign(i, read.delim(filepath,
    colClasses=c("character","factor",rep("numeric",4)),
    sep = "\t"))
}

### Working out quadrants
quadrant = function(x,y,l){l[[paste1(x>0,y>0)]]}

quadrant = function(x,y,l){l[[paste(x>0,y>0,collapse="",sep="")]]}
li=list("TRUETRUE"=1,"TRUEFALSE"=2,"FALSETRUE"=3,"FALSEFALSE"=4)
quadrant(3,-9,li)


## Less house keeping more code...
items <- read.csv("items.csv")
audio <- read.csv("audiotimings.csv")
load("droid.RData")


head(items)
head(audio)
head(droid)


## use readlines then filter then read.csv
dwell.time <- sync.time - select.time

##use ggplot
##calculate raw session time