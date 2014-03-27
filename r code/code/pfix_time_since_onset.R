## This script generates figures for activation over time for
## the four images

# Setwd
setwd("D:/Dropbox/Maxi/R/r\ code/output")
load(file="preprocessed.RData")
load(file="alldat.RData")

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
# Code for generating the figure for lexical activation
fig.lex <- function(){
	line.width <- 1
	line.type <- 'b'
	bins <- seq(0,1488,24)
	plot(bins, rep(NA, length(bins)),
		xlim=c(400,1400), 
		ylim=c(0,1), 
		#type=line.type, 
		xlab="Time from Word Onset (ms)", 
		ylab="Target Probability")
	lineinfo <- list(PComp=list(mpch=1,mcol=2),
	                 SComp=list(mpch=2,mcol=22),
	                 Targ=list(mpch=3,mcol=1),
	                 Unrl=list(mpch=4,mcol=20))

	draw.lines <- function(x) {
	  inf <- lineinfo[[x]]
	  points(bins, props[x, ], 
	  	type=line.type, 
	  	#xlim=c(500,1400), 
	  	pch=inf$mcol,
	  	lwd=line.width
	  	)
	  abline(v=700, lty=2)
	  axis(side=1, at=700, label="")
	  grid()
	}
	lapply(rownames(props), draw.lines)
	legend("topleft", 
    		legend=c("Target", "Phonological", "Semantic", "Unrelated"), 
    		pch=c(1, 2, 22, 20), lty=1, lwd=line.width) 
}

# Preview
fig.lex()
# This saves the figure to pdf
to.pdf(fig.lex(), "fixations.pdf", height=6, width=10)