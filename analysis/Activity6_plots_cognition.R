# 
sem <- function(x) {sd(x) / sqrt(length(x))}
ci95 <- function(x) {sem(x)*1.96}


##Read in data
d = read.csv("/Documents/CaLL/from work computer/Experiments/Activity/Paper:Presentation/Paper/Language and Cognition/Analysis/data/A6_data.csv")
summary(d)

#Read in item data
item = read.csv("/Documents/CaLL/from work computer/Experiments/Activity/Paper:Presentation/Paper/Language and Cognition/Analysis/data/A6_item_data.csv") 
summary(item)

#####################
#DATA PREP
# Add subject column
d$Subject = substr(d$Filename, regexpr("S",d$Filename)+1, regexpr("T",d$Filename)-1)
d$Subject <- as.factor(d$Subject)
summary(d)

# Add item column
d$Item = substr(d$Filename, regexpr("T",d$Filename)+1, regexpr("W",d$Filename)-1)
d$Item <- as.numeric(d$Item)
d$Item <- as.factor(d$Item)
summary(d)

# Add word column
d$words = substr(d$Filename, regexpr("W",d$Filename)+1, regexpr("_",d$Filename)-1)
d$words<- as.factor(d$words)
summary(d)

# Add item information
index <- match(d$words, item$words)
d$syllables <- item$syllables[index]
d$Lg10WF  <- item$Lg10WF[index]
d$filler_activity  <- item$filler_activity[index]

# Remove strange duplicates
d=subset(d, d$Segmentlabel != '0')
d=subset(d, d$Filename != 'S510T7Wstaring_F_')
d= d[d$Segmentlabel != 'sky-ing',]
summary(d)

# Add condition information
d$Condition2 <- ifelse(
					d$word == 'cheering'
					|d$word == 'hollering'
					|d$word == 'screaming'
					|d$word == 'shouting'
					|d$word == 'singing'
					|d$word == 'yelling',
					'hv', ifelse(
						d$word == 'chatting' 
						|d$word == 'discussing'
						|d$word == 'gossiping'
						|d$word == 'mumbling'
						|d$word == 'talking'
						|d$word == 'whispering',
						'lv', ifelse(
							d$word == 'debating'
							|d$word == 'lecturing'
							|d$word == 'arguing',
							'hvn', ifelse(
								d$word == 'articulating' 
								|d$word == 'babbling'
								|d$word == 'bantering'
								|d$word == 'commenting'
								|d$word == 'conversing'
								|d$word == 'mentioning'
								|d$word == 'muttering'
								|d$word == 'remarking'
								|d$word == 'speaking'
								|d$word == 'telling'
								|d$word == 'uttering'
								|d$word == 'murmuring'
								|d$word == 'reciting'
								|d$word == 'humming'
								|d$word == 'saying',
								'lvn', ifelse(
									d$word == 'tasting' 
									|d$word == 'biting'
									|d$word == 'chewing'
									|d$word == 'coughing'
									|d$word == 'eating'
									|d$word == 'frowning'
									|d$word == 'grinning'
									|d$word == 'licking'
									|d$word == 'puckering'
									|d$word == 'smiling'
									|d$word == 'smirking'
									|d$word == 'whistling',
								'm', d$Condition)
								)
							)						
						)
					)
					
d$Condition2 <- as.factor(d$Condition2)

d$Condition3 <- ifelse(d$Condition2 == '1', "Filler", 
				ifelse(d$Condition2 == '2', "Foot", 
				ifelse(d$Condition2 == 'hvn' | d$Condition2 == 'hv', "High Vocal", 
				ifelse(d$Condition2 == 'lv' | d$Condition2== 'lvn', "Low Vocal",
				ifelse(d$Condition2=="m", "Mouth",'x')))))
				
d$Condition3 <- factor(d$Condition3, c("Filler", "Foot", "Mouth", "Low Vocal", "High Vocal"))

d$Condition4 = ifelse(d$Condition == 'V', 'V', ifelse(d$Condition3 == 'Mouth', "M", "other")) 
d$Condition4<- as.factor(d$Condition4)

################
#PLOTS

### means
min_mean = myTapply(d$TargetMin,list(d$Condition3),mean)
max_mean = myTapply(d$TargetMax,list(d$Condition3),mean)
int_mean2 = myTapply(d$TargetIntensity,list(d$Condition),mean)
pitch_mean = myTapply(d$TargetPitch,list(d$Condition3),mean)
duration_mean = myTapply(d$Duration,list(d$Condition3),mean)
dif_mean = myTapply(d$TargetDifference,list(d$Condition3),mean)

int_mean = myTapply(d$TargetIntensity,list(d$Condition3),mean)

## Just intensity plot
	#pdf("A6plot.pdf")
	#---int
	# SETUP Y-AXIS VALUES
	#Y-range
	max = max(int_mean) # Max y value
	min = min(int_mean) # Min y value
	ymax = max+.02*max # Sets max y-axis value based on max y value
	ymin = 55 # Sets min y-axis based on min y value

    #PLOT GRAPH
	plot = barplot(int_mean, #
	ylim=c(ymin,ymax), #sets limits of y-axis
	xpd=FALSE, 
	xlab = "Semantics",
	#main = "Intensity",
	ylab = "Intensity (dB)")
	
	# add standard error bars
	ci = myTapply(d$TargetIntensity,list(d$Condition3),ci95)
	arrows(plot,int_mean+ci, plot, int_mean-ci, angle=90, code=3, length=0)
  #dev.off()
	
	
#WRITE TO PDF?
pdf("/Users/mll/Desktop/Lewis_Fig2.pdf", width = 6, height = 6) #Uncomment; Also uncomment dev.off at end

DVMeans = int_mean[c(2:5)]

# SETUP Y-AXIS VALUES
#Y-range
max = max(DVMeans) # Max y value
min = min(DVMeans) # Min y value
ymax = 59 # Sets max y-axis value based on max y value
ymin = 53 # Sets min y-axis based on min y value

#Y-tick marks
fymin=floor(ymin)
cymax=ceiling(ymax)
ticks=pretty(fymin:cymax, n=5) #n=desired number of tick marks

#SETUP OTHER GRAPHIC PARAMETERS
# "par" = graphics parameters (set by default but can be changed with par() function)
par(family="sans") #sets font (few options)
par(mar=c(6,6,6,6)) #sets margins(bottom,left,top,right)
par(mgp=c(4,1,0)) #sets axis margins(label,ticks,axis)

#PLOT GRAPH
graph <- barplot(DVMeans, #data matrix
	#width=.5, #width of bar (unnecessary unless xlim is specified)
	beside=T, #groups bars (rather than stack)
	ylab="", #empty label in order to specify below
	xlab="",#empty label in order to specify below
	cex.main=1, #title font size
	ylim=c(ymin,ymax), #sets limits of y-axis
	xpd=FALSE, #disables bars to extend beyod graph region
	xaxt = "n",
	border = NA, #bar border color (NA removes border)
	axes=F, 
  main = "Experiment 2:\n Intensity vs. Semantic Condition") # supresses axes inorder to draw below

# ADD GRID LINES
#abline(h=ticks, lwd = 1, col="gray") # Adds horizontal grid marks based on ticks location (adds ontop of graph)
#allows graph to be replotted on top of grids
par(new = TRUE)
#replots graph
graph <- barplot(DVMeans, width=.5,beside=T, ylab="",xlab="", xaxt = "n",,ylim=c(ymin,ymax),cex.main=2,lwd=3, xpd=FALSE, border = NA, axes=F)

graph <- matrix(c(.35,.95, 1.55, 2.15), 5, byrow = T)

# CUSTOMIZE CERTAIN GRAPH FEATURES
#X- and Y- axis labels
title(ylab="Intensity (dB)", cex.lab=1.75,font.lab=1)
#X- and Y-axes
axis(1, at=c(0,50),labels=FALSE,lwd=3) #x-axis; somehow supresses ticks, lwd=line width
text(graph, 53, labels = c("Foot  ", "Mouth" ,"Low-effort\nVocal", "High-effort\nVocal"), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=1.2)
axis(2, at = ticks, las=2,lwd=3) #y-axis; las- determines orientation of labels- horizontial(1), vertical(2)

# add standard error bars

ci = myTapply(d$TargetIntensity,list(d$Condition3),ci95)[2:5]

arrows(graph,DVMeans+ci, graph, DVMeans-ci, angle=90, code=3, length=0)

dev.off() #closes pdf device

## All measures plot
#pdf("Activity6(N=67).pdf",12,7)
par(mfrow=c(2,3))
	#---min
	# SETUP Y-AXIS VALUES
	#Y-range
	ymin = 134 # Sets min y-axis based on min y value
	ymax = ymin+15 
	
    #PLOT GRAPH
	barplot(min_mean, #
	ylim=c(ymin,ymax), #sets limits of y-axis
	xpd=FALSE, 
	xlab = "Semantics",
	main = "Min",
	ylab = "Min(Hz)")
	
	#---max
	# SETUP Y-AXIS VALUES
	#Y-range
	ymin = 210 # Sets min y-axis based on min y value
	ymax = ymin+15 
	
    #PLOT GRAPH
	barplot(max_mean, #
	ylim=c(ymin,ymax), #sets limits of y-axis
	xpd=FALSE, 
	xlab = "Semantics",
	main = "Max",
	ylab = "Max(Hz)")
	
	#---int
	# SETUP Y-AXIS VALUES
	#Y-range
	max = max(int_mean) # Max y value
	min = min(int_mean) # Min y value
	ymax = max+.02*max # Sets max y-axis value based on max y value
	ymin = min-.05*min # Sets min y-axis based on min y value

    #PLOT GRAPH
	barplot(int_mean, #
	ylim=c(ymin,ymax), #sets limits of y-axis
	xpd=FALSE, 
	xlab = "Semantics",
	main = "Intensity",
	ylab = "Intensity (dB)")
	
	#---pitch
	# SETUP Y-AXIS VALUES
	#Y-range
	ymin = 166 # Sets min y-axis based on min y value
	ymax = ymin+15  

    #PLOT GRAPH
	barplot(pitch_mean, #
	ylim=c(ymin,ymax), #sets limits of y-axis
	xpd=FALSE, 
	xlab = "Semantics",
	main = "Pitch",
	ylab = "Pitch(Hz)")
	
	#---duration
	# SETUP Y-AXIS VALUES
	#Y-range
	max = max(duration_mean) # Max y value
	min = min(duration_mean) # Min y value
	ymax = max+.02*max # Sets max y-axis value based on max y value
	ymin = min-.05*min # Sets min y-axis based on min y value

    #PLOT GRAPH
	barplot(duration_mean, #
	ylim=c(ymin,ymax), #sets limits of y-axis
	xpd=FALSE, 
	xlab = "Semantics",
	main = "Duration",
	ylab = "Duration (ms)")
	
	#---dif
	# SETUP Y-AXIS VALUES
	#Y-range
	ymin = 68 # Sets min y-axis based on min y value
	ymax = ymin+15 # Sets max y-axis value based on max y value

    #PLOT GRAPH
	barplot(dif_mean, #
	ylim=c(ymin,ymax), #sets limits of y-axis
	xpd=FALSE, 
	xlab = "Semantics",
	main = "Pitch difference",
	ylab = "Pitch Difference (Hz)")	
#dev.off()
