library(ggplot2)
library(gridExtra)
set.seed(10005)

xvar <- c(rnorm(1500, mean = -1), rnorm(1500, mean = 1.5))
yvar <- c(rnorm(1500, mean = 1), rnorm(1500, mean = 1.5))
zvar <- as.factor(c(rep(1, 1500), rep(2, 1500)))
xy <- data.frame(xvar, yvar, zvar)

#counts on y-axis
g1<-ggplot(xy, aes(xvar)) + geom_histogram()                                      #horribly ugly default
g2<-ggplot(xy, aes(xvar)) + geom_histogram(binwidth=1)                            #change binwidth
g3<-ggplot(xy, aes(xvar)) + geom_histogram(fill=NA, color="black") + theme_bw()   #nicer looking

#density on y-axis
g4<-ggplot(xy, aes(x=xvar)) + geom_histogram(aes(y = ..density..), color="black", fill=NA) + theme_bw()

grid.arrange(g1, g2, g3, g4, nrow=1)

## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust
## this. stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to
## adjust this. stat_bin: binwidth defaulted to range/30. Use 'binwidth = x'
## to adjust this.

#basic density
p1<-ggplot(xy, aes(xvar)) + geom_density()

#histogram with density line overlaid
p2<-ggplot(xy, aes(x=xvar)) + 
  geom_histogram(aes(y = ..density..), color="black", fill=NA) +
  geom_density(color="blue")

#split and color by third variable, alpha fades the color a bit
p3<-ggplot(xy, aes(xvar, fill = zvar)) + geom_density(alpha = 0.2)

grid.arrange(p1, p2, p3, nrow=1)

#boxplot
b1<-ggplot(xy, aes(zvar, xvar)) + 
  geom_boxplot(aes(fill = zvar)) +
  theme(legend.position = "none")

#jitter plot
b2<-ggplot(xy, aes(zvar, xvar)) + 
  geom_jitter(alpha=I(1/4), aes(color=zvar)) +
  theme(legend.position = "none")

#volcano plot
b3<-ggplot(xy, aes(x = xvar)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density..,
               fill = zvar, color = zvar),
               geom = "ribbon", position = "identity") +
  facet_grid(. ~ zvar) +
  coord_flip() +
  theme(legend.position = "none")

grid.arrange(b1, b2, b3, nrow=1)

#rug plot
ggplot(xy,aes(xvar,yvar))  + geom_point() + geom_rug(col="darkred",alpha=.1)

#placeholder plot - prints nothing at all
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
     theme(                              
       plot.background = element_blank(), 
       panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(), 
       panel.border = element_blank(), 
       panel.background = element_blank(),
       axis.title.x = element_blank(),
       axis.title.y = element_blank(),
       axis.text.x = element_blank(),
       axis.text.y = element_blank(),
       axis.ticks = element_blank()
     )

#scatterplot of x and y variables
scatter <- ggplot(xy,aes(xvar, yvar)) + 
  geom_point(aes(color=zvar)) + 
  scale_color_manual(values = c("orange", "purple")) + 
  theme(legend.position=c(1,1),legend.justification=c(1,1)) 

#marginal density of x - plot on top
plot_top <- ggplot(xy, aes(xvar, fill=zvar)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("orange", "purple")) + 
  theme(legend.position = "none")

#marginal density of y - plot on the right
plot_right <- ggplot(xy, aes(yvar, fill=zvar)) + 
  geom_density(alpha=.5) + 
  coord_flip() + 
  scale_fill_manual(values = c("orange", "purple")) + 
  theme(legend.position = "none") 

#arrange the plots together, with appropriate height and width for each row and column
grid.arrange(plot_top, empty, scatter, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))