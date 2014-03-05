
## This script generates figures for activation over time for
## the four images

# Setwd
#load(file="preprocessed.RData")
#load(file="alldat.RData")

#colnames(alldat) <- sub("\\.x$", "", colnames(alldat))
#rownames(alldat) <- NULL

# Useful function for creating pdfs
to.pdf <- function(expr, filename, ..., verbose=TRUE) {
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}


# Cross tabulation 
mx <- as.matrix(xtabs(~AOI+Msec, alldat))
props <- mx / apply(mx, 2, sum)

# Use ggplot to make it cool?

# Code for generating the figure for lexical activation
fig.lex <- function(){
	bins <- seq(0,1488,24)
	plot(bins, rep(NA, length(bins)),
		xlim=c(400,1400), 
		ylim=c(0,1), 
		type='l', 
		xlab="Time from Onset (ms)", 
		ylab="Probability of Gaze Latency")
	lineinfo <- list(PComp=list(mpch=1,mcol='blue'),
	                 SComp=list(mpch=2,mcol='green'),
	                 Targ=list(mpch=3,mcol='red'),
	                 Unrl=list(mpch=4,mcol='gray50'))

	legend("topleft", 
    		legend=c("Target", "Cohort", "Semantic", "Distractor"), 
    		col=c("red","blue", "green", "gray50"), lty=1)
	lapply(rownames(props), function(x) {
	  inf <- lineinfo[[x]]
	  #points(bins, props[x,], type='b', pch=inf$mpch, col=inf$mcol)
	  points(bins, props[x,], 
	  	type='l', 
	  	xlim=c(400,1400), 
	  	col=inf$mcol)
	}) 
}

# This saves the figure to pdf
to.pdf(fig.lex(), "plot-line3.pdf", height=6, width=6)