# Analyze Activity #4
# (Exp#1 in  paper)

## LOAD REQUIRED PACKAGES
library(languageR)
library(lme4)
sem <- function(x) {sd(x) / sqrt(length(x))}
ci95 <- function(x) {sem(x)*1.96}

## READ DATASET
# Experimental
Activity= read.csv("/Documents/CaLL/from work computer/Experiments/Activity/Paper:Presentation/Paper/Language and Cognition/Analysis/data/A4_data.csv")
# Norms
loud = read.csv('/Documents/CaLL/from work computer/Experiments/Activity/Paper:Presentation/Paper/Language and Cognition/Analysis/data/loudness_item_means.csv')
effort = read.csv('/Documents/CaLL/from work computer/Experiments/Activity/Paper:Presentation/Paper/Language and Cognition/Analysis/data/effort_item_means.csv')

# MAKE FACTORS
Activity$Subject = factor(Activity$Subject)
Activity$Item = factor(Activity$Item)
Activity$Semantics = factor(Activity$Semantics)
Activity$Semantics2 <- factor(Activity$Semantics2,levels=c('Foot','LowVocal','HighVocal')) # reorders
summary(Activity)

# MERGE NORMS
index <- match(Activity$Item, Norms$word)
Activity$loud <- Norms$rating[index]

index <- match(Activity$Item, effort$word)
Activity$effort <- effort$rating[index]


## SET CONTRASTS
#Semantics and Semantics2
contrasts(Activity$Semantics) = c(-.5,.5)
contrasts(Activity$Semantics) # check
contrasts(Activity$Semantics2) = contr.helmert
contrasts(Activity$Semantics2) # check

#Subject Gender
counttotal = length(Activity$Gender)
countM = length(Activity$Gender[Activity$Gender=='M'])
countF = length(Activity$Gender[Activity$Gender=='F'])
weightF = countM/counttotal
weightM = countF/counttotal
contrasts(Activity$Gender) <- cbind(c(weightF,-1*weightM))
contrasts(Activity$Gender) # display to check

#RUN MODELS AND COMPARE AIC - Best model has lowest AIC
# Predict intensity with condition
#Intensity Models
#Semantics
Intensity1 = lmer(TargetIntensity ~ Semantics+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity1)

Intensity1s = lmer(TargetIntensity ~ Semantics+ (1+Semantics|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity1s)

Intensity2 = lmer(TargetIntensity ~ Semantics+ Gender+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity2)

Intensity2s = lmer(TargetIntensity ~ Semantics+ Gender+(1+Semantics|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity2s)

Intensity3 = lmer(TargetIntensity ~ Semantics+ Gender+ Syllables+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity3)

Intensity3s = lmer(TargetIntensity ~ Semantics+ Gender+ Syllables+(1+Semantics|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity3s)

Intensity4 = lmer(TargetIntensity ~ Semantics+ Gender+ Syllables+Freq.logwf+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity4)

Intensity4s = lmer(TargetIntensity ~ Semantics+ Gender+ Syllables+Freq.logwf+(1+Semantics|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity4s)

Intensity5 = lmer(TargetIntensity ~ Semantics+Syllables+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity5)

Intensity5s = lmer(TargetIntensity ~ Semantics+Syllables+(1+Semantics|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity5s)

Intensity6 = lmer(TargetIntensity ~ Semantics+Syllables+Freq.logwf+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity6)

Intensity6s = lmer(TargetIntensity ~ Semantics+Syllables+Freq.logwf+(1+Semantics|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity6s)

Intensity7 = lmer(TargetIntensity ~ Semantics+Freq.logwf+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity7)

Intensity7s = lmer(TargetIntensity ~ Semantics+Freq.logwf+(1+Semantics|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity7s)

Intensity8 = lmer(TargetIntensity ~ Semantics+Freq.logwf+Gender+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity8)

Intensity8s = lmer(TargetIntensity ~ Semantics+Freq.logwf+Gender+(1+Semantics|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity8s)

Intensity8s = lmer(TargetIntensity ~ Semantics*Freq.logwf*Gender*(1+Semantics|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Intensity8s)


lmercompare(list(Intensity1, Intensity1s, Intensity2, Intensity2s, Intensity3,Intensity3s, Intensity4,Intensity4s, Intensity5, Intensity5s, Intensity6,Intensity6s,Intensity7,Intensity7s, Intensity8,Intensity8s))

#[1] MODEL        AIC         BIC         PREDICTORS
#[1] Intensity1   7321.901 *  7348.417 *  Semantics + (1 | Subject) + (1 | Item)
#[1] Intensity1s  7325.762    7362.884    Semantics + (1 + Semantics | Subject) + (1 | Item)
#[1] Intensity2   7323.421    7355.240    Semantics + Gender + (1 | Subject) + (1 | Item)
#[1] Intensity2s  7327.264    7369.690    Semantics + Gender + (1 + Semantics | Subject) + (1 | Item)
#[1] Intensity3   7324.179    7361.302    Semantics + Gender + Syllables + (1 | Subject) + (1 | Item)
#[1] Intensity3s  7328.022    7375.751    Semantics + Gender + Syllables + (1 + Semantics | Subject) + (1 | Item)
#[1] Intensity4   7326.179    7368.604    Semantics + Gender + Syllables + Freq.logwf + (1 | Subject) + (1 | Item)
#[1] Intensity4s  7330.022    7383.053    Semantics + Gender + Syllables + Freq.logwf + (1 + Semantics | Subject) + (1 | Item)
#[1] Intensity5   7322.659    7354.478    Semantics + Syllables + (1 | Subject) + (1 | Item)
#[1] Intensity5s  7326.526    7368.952    Semantics + Syllables + (1 + Semantics | Subject) + (1 | Item)
#[1] Intensity6   7324.659    7361.781    Semantics + Syllables + Freq.logwf + (1 | Subject) + (1 | Item)
#[1] Intensity6s  7328.520    7376.248    Semantics + Syllables + Freq.logwf + (1 + Semantics | Subject) + (1 | Item)
#[1] Intensity7   7323.755    7355.574    Semantics + Freq.logwf + (1 | Subject) + (1 | Item)
#[1] Intensity7s  7327.616    7370.042    Semantics + Freq.logwf + (1 + Semantics | Subject) + (1 | Item)
#[1] Intensity8   7325.275    7362.398    Semantics + Freq.logwf + Gender + (1 | Subject) + (1 | Item)
#[1] Intensity8s  7329.118    7376.847    Semantics + Freq.logwf + Gender + (1 + Semantics | Subject) + (1 | Item)

summary(Intensity1) # THIS IS THE BEST MODEL

summary(Intensity4) # Results don't change when all controls included in model

summary(Intensity4s) # Results don't change when all controls included in model

myTapply(Activity$TargetIntensity,list(Activity$Semantics),mean)

# t-test (reported in Appendix)
boxplot(TargetIntensity~Semantics,data=Activity)
a = aggregate( Activity$TargetIntensity, by = list(Activity$Subject, Activity$Semantics), FUN = mean)
names(a) = c("Subject", "Semantics","TargetIntensity")

Intensity_P = a[a$Semantics == "P", c("Subject", "TargetIntensity")]
Intensity_V = a[a$Semantics == "V", c("Subject", "TargetIntensity")]
t.test(Intensity_V$TargetIntensity, Intensity_P$TargetIntensity, paired = TRUE)

## Predict Intensity with Semantics2
IntensityS1 = lmer(TargetIntensity ~ Semantics2 +(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS1)

IntensityS1s = lmer(TargetIntensity ~ Semantics2+ (1+Semantics2|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS1s)

IntensityS2 = lmer(TargetIntensity ~ Semantics2+ Gender+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS2)

IntensityS2s = lmer(TargetIntensity ~ Semantics2+ Gender+(1+Semantics2|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS2s)

IntensityS3 = lmer(TargetIntensity ~ Semantics2+ Gender+ Syllables+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS3)

IntensityS3s = lmer(TargetIntensity ~ Semantics2+ Gender+ Syllables+(1+Semantics2|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS3s)

IntensityS4 = lmer(TargetIntensity ~ Semantics2+ Gender+ Syllables+Freq.logwf+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS4)

IntensityS4s = lmer(TargetIntensity ~ Semantics2+ Gender+ Syllables+Freq.logwf+(1+Semantics2|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS4s)

IntensityS5 = lmer(TargetIntensity ~ Semantics2+Syllables+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS5)

IntensityS5s = lmer(TargetIntensity ~ Semantics2+Syllables+(1+Semantics2|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS5s)

IntensityS6 = lmer(TargetIntensity ~ Semantics2+Syllables+Freq.logwf+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS6)

IntensityS6s = lmer(TargetIntensity ~ Semantics2+Syllables+Freq.logwf+(1+Semantics2|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS6s)

IntensityS7 = lmer(TargetIntensity ~ Semantics2+Freq.logwf+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS7)

IntensityS7s = lmer(TargetIntensity ~ Semantics2+Freq.logwf+(1+Semantics2|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS7s)

IntensityS8 = lmer(TargetIntensity ~ Semantics2+Freq.logwf+Gender+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS8)

IntensityS8s = lmer(TargetIntensity ~ Semantics2+Freq.logwf+Gender+(1+Semantics2|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(IntensityS8s)

lmercompare(list(IntensityS1, IntensityS1s, IntensityS2, IntensityS2s, IntensityS3,IntensityS3s, IntensityS4,IntensityS4s, IntensityS5, IntensityS5s, IntensityS6,IntensityS6s,IntensityS7,IntensityS7s, IntensityS8,IntensityS8s))

#[1] MODEL         AIC         BIC         PREDICTORS
#[1] IntensityS1   7323.112 *  7354.931 *  Semantics2 + (1 | Subject) + (1 | Item)
#[1] IntensityS1s  7325.244    7383.579    Semantics2 + (1 + Semantics2 | Subject) + (1 | Item)
#[1] IntensityS2   7324.632    7361.755    Semantics2 + Gender + (1 | Subject) + (1 | Item)
#[1] IntensityS2s  7327.118    7390.756    Semantics2 + Gender + (1 + Semantics2 | Subject) + (1 | Item)
#[1] IntensityS3   7325.886    7368.311    Semantics2 + Gender + Syllables + (1 | Subject) + (1 | Item)
#[1] IntensityS3s  7328.372    7397.314    Semantics2 + Gender + Syllables + (1 + Semantics2 | Subject) + (1 | Item)
#[1] IntensityS4   7327.886    7375.614    Semantics2 + Gender + Syllables + Freq.logwf + (1 | Subject) + (1 | Item)
#[1] IntensityS4s  7330.372    7404.616    Semantics2 + Gender + Syllables + Freq.logwf + (1 + Semantics2 | Subject) + (1 | Item)
#[1] IntensityS5   7324.366    7361.488    Semantics2 + Syllables + (1 | Subject) + (1 | Item)
#[1] IntensityS5s  7326.498    7390.136    Semantics2 + Syllables + (1 + Semantics2 | Subject) + (1 | Item)
#[1] IntensityS6   7326.365    7368.791    Semantics2 + Syllables + Freq.logwf + (1 | Subject) + (1 | Item)
#[1] IntensityS6s  7328.498    7397.439    Semantics2 + Syllables + Freq.logwf + (1 + Semantics2 | Subject) + (1 | Item)
#[1] IntensityS7   7325.035    7362.157    Semantics2 + Freq.logwf + (1 | Subject) + (1 | Item)
#[1] IntensityS7s  7327.166    7390.804    Semantics2 + Freq.logwf + (1 + Semantics2 | Subject) + (1 | Item)
#[1] IntensityS8   7326.555    7368.980    Semantics2 + Freq.logwf + Gender + (1 | Subject) + (1 | Item)
#[1] IntensityS8s  7329.041    7397.982    Semantics2 + Freq.logwf + Gender + (1 + Semantics2 | Subject) + (1 | Item)

summary(IntensityS1) # THIS IS THE BEST MODEL
pvals.fnc(IntensityS1,addPlot=FALSE)

summary(IntensityS4) # Results don't change when all controls included in model
pvals.fnc(IntensityS4,addPlot=FALSE)

summary(IntensityS4s) # Results don't change when all controls included in model
pvals.fnc(IntensityS4s,addPlot=FALSE)

myTapply(Activity$TargetIntensity,list(Activity$Semantics2),mean)

# by subject anova (reported in appendix)
bysubj = aggregate(Activity$TargetIntensity, by = list(Activity$Subject, Activity$Semantics2), FUN = mean)
names(bysubj) = c("Subject", "Semantics2","TargetIntensity")
summary(aov(TargetIntensity~Semantics2 + Error(Subject/Semantics2), data = bysubj))

# t.tests
Intensity_Foot = bysubj[bysubj$Semantics2 == "Foot", "TargetIntensity"]
Intensity_LV = bysubj[bysubj$Semantics2== "LowVocal", "TargetIntensity"]
Intensity_HV = bysubj[bysubj$Semantics2 == "HighVocal", "TargetIntensity"]

t.test(Intensity_Foot, Intensity_LV, paired = TRUE)
t.test(Intensity_LV, Intensity_HV, paired = TRUE)

## Pitch, Duration -- not reliable
TargetMin = lmer(TargetMin ~ Semantics + Syllables + Freq.logwf+Gender+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(TargetMin)
pvals.fnc(TargetMin)

Duration = lmer(Duration ~ Semantics + Syllables + Freq.logwf+Gender+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Duration)
pvals.fnc(Duration)

TargetMax = lmer(TargetMax ~ Semantics + Syllables + Freq.logwf+Gender+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(TargetMax)

TargetPitch = lmer(TargetPitch ~ Semantics + Syllables + Freq.logwf+Gender+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(TargetPitch)

TargetDifference = lmer(TargetDifference ~ Semantics + Syllables + Freq.logwf+Gender+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(TargetDifference)

## Pitch, Duration -- not reliable with Semantics2
TargetMin = lmer(TargetMin ~ Semantics2 + (1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(TargetMin)

Duration = lmer(Duration ~ Semantics2 + (1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(Duration)

TargetMax = lmer(TargetMax ~ Semantics2+ (1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(TargetMax)

TargetPitch = lmer(TargetPitch ~ Semantics2 + (1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(TargetPitch)

TargetDifference = lmer(TargetDifference ~ Semantics2+(1|Subject) + (1|Item), REML=FALSE, data=Activity)
summary(TargetDifference)


##PLOT #1
d4 = Activity[Activity$Semantics2 != 'NonVocal',]
DVMeans <- myTapply(d4$TargetIntensity,list(d4$Semantics2),mean) 
#DVMeans = DVMeans[c(2:4)]


#WRITE TO PDF?
pdf("/Users/mll/Desktop/Lewis_Fig1.pdf", width = 6, height = 6) #Uncomment; Also uncomment dev.off at end

# SETUP Y-AXIS VALUES
#Y-range
max = max(DVMeans) # Max y value
min = min(DVMeans) # Min y value
ymax = 55 # Sets max y-axis value based on max y value
ymin = 49 # Sets min y-axis based on min y value

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
 barplot(DVMeans, #data matrix
	#width=.5, #width of bar (unnecessary unless xlim is specified)
	beside=T, #groups bars (rather than stack)
	ylab="", #empty label in order to specify below
	xlab="",#empty label in order to specify below
	#cex.names=0, #adjusts size of name font
	cex.main=1, #title font size
	ylim=c(ymin,ymax), #sets limits of y-axis
	xpd=FALSE, #disables bars to extend beyod graph region
	xaxt = "n",
	border = NA, #bar border color (NA removes border)
	axes=F , # supresses axes inorder to draw below
  main= "Experiment 1:\n Intensity vs. Semantic Condition")

# ADD GRID LINES
#abline(h=ticks, lwd = 1, col="gray") # Adds horizontal grid marks based on ticks location (adds ontop of graph)
#allows graph to be replotted on top of grids
par(new = TRUE)
#replots graph
graph <- barplot(DVMeans, width=.5,beside=T, ylab="",xlab="",ylim=c(ymin,ymax), xaxt = "n",,lwd=3, xpd=FALSE, border = NA, axes=F) #c("blue","red") c("lightgrey","grey","darkgrey")

# CUSTOMIZE CERTAIN GRAPH FEATURES
#X- and Y- axis labels
#title(xlab="Semantics", cex.lab=1.75, font.lab=1) #cex.lab - font size; font.lab - unbold(1), bold(2), italics(3)
title(ylab="Intensity (dB)", cex.lab=1.75,font.lab=1)
#X- and Y-axes
axis(1, at=c(0,50),labels=FALSE,lwd=3) #x-axis; somehow supresses ticks, lwd=line width
text(graph, 49, labels = c("Foot  ", "Low-effort\nVocal", "High-effort\nVocal"), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=1.4)
axis(2, at = ticks, las=2,lwd=3) #y-axis; las- determines orientation of labels- horizontial(1), vertical(2)

# add # error bars
ci = myTapply(d4$TargetIntensity,list(d4$Semantics2),ci95)
arrows(graph,DVMeans+ci, graph, DVMeans-ci, angle=90, code=3, length=0)
dev.off() #closes pdf device

##PLOT #2
DVMeans <- myTapply(Activity$TargetIntensity,list(Activity$Semantics),mean) 


#WRITE TO PDF?
pdf("/Users/mll/Desktop/A4plot_new2.pdf") #Uncomment; Also uncomment dev.off at end

# SETUP Y-AXIS VALUES
#Y-range
max = max(DVMeans) # Max y value
min = min(DVMeans) # Min y value
ymax = 55 # Sets max y-axis value based on max y value
ymin = 49 # Sets min y-axis based on min y value

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
barplot(DVMeans, #data matrix
        #width=.5, #width of bar (unnecessary unless xlim is specified)
        beside=T, #groups bars (rather than stack)
        ylab="", #empty label in order to specify below
        xlab="",#empty label in order to specify below
        #cex.names=0, #adjusts size of name font
        ylim=c(ymin,ymax), #sets limits of y-axis
        xpd=FALSE, #disables bars to extend beyod graph region
        xaxt = "n",
        border = NA, #bar border color (NA removes border)
        axes=F) # supresses axes inorder to draw below

# ADD GRID LINES
#abline(h=ticks, lwd = 1, col="gray") # Adds horizontal grid marks based on ticks location (adds ontop of graph)
#allows graph to be replotted on top of grids
par(new = TRUE)
#replots graph
graph <- barplot(DVMeans, width=.5,beside=T, ylab="",xlab="",ylim=c(ymin,ymax), xaxt = "n",,lwd=3, xpd=FALSE, border = NA, axes=F) #c("blue","red") c("lightgrey","grey","darkgrey")

# CUSTOMIZE CERTAIN GRAPH FEATURES
#X- and Y- axis labels
#title(xlab="Semantics", cex.lab=1.75, font.lab=1) #cex.lab - font size; font.lab - unbold(1), bold(2), italics(3)
title(ylab="Intensity (dB)", cex.lab=1.75,font.lab=1)
#X- and Y-axes
axis(1, at=c(0,50),labels=FALSE,lwd=3) #x-axis; somehow supresses ticks, lwd=line width
text(graph, 49, labels = c("Foot  ", "Vocal"), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=1.4)
axis(2, at = ticks, las=2,lwd=3) #y-axis; las- determines orientation of labels- horizontial(1), vertical(2)

# add standard error bars
int_sd = myTapply(Activity$TargetIntensity,list(Activity$Semantics),sd)
int_length = myTapply(Activity$TargetIntensity,list(Activity$Semantics),length)

se =  int_sd/sqrt(int_length)
arrows(graph,DVMeans+se, graph, DVMeans-se, angle=90, code=3, length=0)

dev.off() #closes pdf device

## Means for all measures
myTapply(Activity$TargetIntensity,list(Activity$Semantics),mean)
myTapply(Activity$Duration,list(Activity$Semantics),mean)
myTapply(Activity$TargetMin,list(Activity$Semantics),mean)
myTapply(Activity$TargetMax,list(Activity$Semantics),mean)
myTapply(Activity$TargetPitch,list(Activity$Semantics),mean)

## SDs for all measures
myTapply(Activity$TargetIntensity,list(Activity$Semantics),sd)
myTapply(Activity$Duration,list(Activity$Semantics),sd)
myTapply(Activity$TargetMin,list(Activity$Semantics),sd)
myTapply(Activity$TargetMax,list(Activity$Semantics),sd)
myTapply(Activity$TargetPitch,list(Activity$Semantics),sd)

## Look for outliers
alt.est_e2 <- estex(IntensityS1, "Item")
# criterion = 4/n 
n = length(unique(Activity$Item)) 
cutoff =  4/n
cooks_2= ME.cook(alt.est_e2, sort=TRUE, plot=TRUE, cutoff= cutoff)


