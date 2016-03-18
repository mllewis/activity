##Activity 7 Analysis
#M.Lewis 2/9/13

rm(list=ls())

#Load libraries
library(lme4)
library(stringr)
library(ggplot2)
library(bootstrap)

theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

##Read in data
setwd('//Documents/CaLL/from work computer/Experiments/Activity/Paper:Presentation/Paper/Language and Cognition/Analysis/data/')
M1 = read.table("A7_results_male1.txt", header = TRUE, fill = TRUE)
M1$gender = 'M'
M2 = read.table("A7_results_male2.txt", header = TRUE, fill = TRUE)
M2$gender = 'M'
F1 = read.table("A7_results_female1.txt", header = TRUE, fill = TRUE)
F1$gender = 'F'
F2 = read.table("A7_results_female2.txt", header = TRUE, fill = TRUE)
F2$gender = 'F'
F3 = read.table("A7_results_female3.txt", header = TRUE, fill = TRUE)
F3$gender = 'F'

#merge all datasets  together
d <- rbind(M1, M2, F1, F2, F3)

#extra condition and item information
d$SubjectNum <- as.factor(sapply(str_split(matrix(sapply(str_split(d$Filename,"S"),function(x) {x[2]})),"-"),function(x){x[1]}))
d$Condition<- as.factor(sapply(str_split(matrix(sapply(str_split(d$Filename,"_"),function(x) {x[2]})),"_"),function(x){x[1]}))
d$TrialNum<- as.factor(sapply(str_split(d$Filename,"N"),function(x) {x[2]}))
d$Distractor<- as.factor(sapply(str_split(matrix(sapply(str_split(d$Filename,"D_"),function(x) {x[2]})),"-"),function(x){x[1]}))
d$Target<- as.factor(sapply(str_split(matrix(sapply(str_split(d$Filename,"T_"),function(x) {x[2]})),"-"),function(x){x[1]}))
d$gender <- as.factor(d$gender)

#add NAs
d$TargetMax <- ifelse(d$TargetMax == '--undefined--', NA, d$TargetMax)
d$TargetMin <- ifelse(d$TargetMin == '--undefined--', NA, d$TargetMin)
d$TargetPitch <- ifelse(d$TargetPitch == '--undefined--', NA, d$TargetPitch)
d$TargetDifference<- ifelse(d$TargetDifference == '--undefined--', NA, d$TargetDifference)
d$TargetIntensity <- as.numeric(as.character(d$TargetIntensity)) #make intensity numeric

#reorder  labels 
d$Segmentlabel = factor(d$Segmentlabel, levels = c("o",'co',"p","c","v","pv")) 
#levels(d$Segmentlabel) <- c("onset",'\"click on\"','\"the person\"  ','(\"that\'s\")',"verb","post-verb") # for graphing only


#### Stats for paper
#Set up contrast codes
#gender
counttotal = length(d$gender)
countM = length(d$gender[d$gender=='M'])
countF = length(d$gender[d$gender=='F'])
weightF = countM/counttotal
weightM = countF/counttotal

contrasts(d$gender) <- cbind(c(weightF,-1*weightM))
contrasts(d$gender) # display to check

#condition
contrasts(d$Condition) = c(-.5,.5)
contrasts(d$Condition) # check

#Intensity (Models m1-m6 are reported in paper)
m1 = lmer(TargetIntensity ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target), REML=FALSE, data=d[d$Segmentlabel == "o",]) #ns
m2 = lmer(TargetIntensity ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target), REML=FALSE, data=d[d$Segmentlabel == "co",]) #ns
m4 = lmer(TargetIntensity ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target), REML=FALSE, data=d[d$Segmentlabel == "c",]) #s
m5 = lmer(TargetIntensity ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target), REML=FALSE, data=d[d$Segmentlabel == "v",]) #s
m6 = lmer(TargetIntensity ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target), REML=FALSE, data=d[d$Segmentlabel == "pv",]) #ns

m7 = lmer(TargetIntensity ~ Condition + (1+Condition|SubjectNum) + (1|Target), REML=FALSE, data=d[d$Segmentlabel == "o",]) #ns
m8 =lmer(TargetIntensity ~ Condition + (1+Condition|SubjectNum) + (1|Target), REML=FALSE, data=d[d$Segmentlabel == "co",]) #ns
m9 =lmer(TargetIntensity ~ Condition + (1+Condition|SubjectNum) + (1|Target), REML=FALSE, data=d[d$Segmentlabel == "p",]) #s
m10= lmer(TargetIntensity ~ Condition + (1+Condition|SubjectNum) + (1|Target), REML=FALSE, data=d[d$Segmentlabel == "c",]) #s
m11= lmer(TargetIntensity ~ Condition + (1+Condition|SubjectNum) + (1|Target), REML=FALSE, data=d[d$Segmentlabel == "v",]) #s
m12 = lmer(TargetIntensity ~ Condition + (1+Condition|SubjectNum) + (1|Target), REML=FALSE, data=d[d$Segmentlabel == "pv",]) #s


lmer(TargetIntensity ~ Condition + Duration +  (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "co",]) # duration -sig
lmer(TargetIntensity ~ Condition + Duration + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "p",]) # all sig
lmer(TargetIntensity ~ Condition + Duration + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "c",]) # all sig
lmer(TargetIntensity ~ Condition + Duration + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "v",]) # all sig

#Pitch
lmer(TargetPitch ~ Condition +  gender + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "co",]) #ns
lmer(TargetPitch ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "p",]) #ns
lmer(TargetPitch ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "c",]) #ns
lmer(TargetPitch ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "v",]) #ns
lmer(TargetPitch ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "p" |d$Segmentlabel == "c" ,]) #ns
lmer(TargetPitch ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "p" |d$Segmentlabel == "c" |d$Segmentlabel == "v",]) #ns

#PitchMax
lmer(TargetPitch ~ Condition + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "co",]) #ns
lmer(TargetPitch ~ Condition + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "p",]) #ns

# Duration
lmer(Duration ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "o",]) #s
lmer(Duration ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "co",]) #ns
lmer(Duration ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "p",]) #ns
lmer(Duration ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "c",]) #s
lmer(Duration ~ Condition + gender + (1+Condition|SubjectNum) + (1|Target) + (1|Distractor), REML=FALSE, data=d[d$Segmentlabel == "v",]) #ns

#no ononset and duration less than 1
#pitch and duration positively correlation  r=.07
#duration and intensity negatively correlated  r = -.07
#pitch and intensity positivel correlated r = .18

# Duration and intensity negatively correlated in the the preverbal range

# by subject paired t-tests (reported in appendix)
co = d[d$Segmentlabel == "co",]
bysubj_co = aggregate(co$TargetIntensity, by = list(co$SubjectNum, co$Condition), FUN = mean)
names(bysubj_co) = c("Subject", "Condition","TargetIntensity")
Intensity_Foot = bysubj_co[bysubj_co$Condition == "f", "TargetIntensity"]
Intensity_Vocal = bysubj_co[bysubj_co$Condition== "v", "TargetIntensity"]
t.test(Intensity_Foot, Intensity_Vocal, paired = TRUE)

p = d[d$Segmentlabel == "p",]
bysubj_p = aggregate(p$TargetIntensity, by = list(p$SubjectNum, p$Condition), FUN = mean)
names(bysubj_p) = c("Subject", "Condition","TargetIntensity")
Intensity_Foot = bysubj_p[bysubj_p$Condition == "f", "TargetIntensity"]
Intensity_Vocal = bysubj_p[bysubj_p$Condition== "v", "TargetIntensity"]
t.test(Intensity_Foot, Intensity_Vocal, paired = TRUE)

c =  d[d$Segmentlabel == "c",]
bysubj_c = aggregate(c$TargetIntensity, by = list(c$SubjectNum, c$Condition), FUN = mean)
names(bysubj_c) = c("Subject", "Condition","TargetIntensity")

# get rid of subjects for which dont hae obs in both v and f conditions
vs = unique(bysubj_c[bysubj_c$Condition == "v","Subject"])
fs = unique(bysubj_c[bysubj_c$Condition == "f","Subject"])
bysubj_c = bysubj_c[which(bysubj_c$Subject %in% intersect(vs,fs)), ]

Intensity_Foot = bysubj_c[bysubj_c$Condition == "f", "TargetIntensity"]
Intensity_Vocal = bysubj_c[bysubj_c$Condition== "v", "TargetIntensity"]
t.test(Intensity_Foot, Intensity_Vocal, paired = TRUE)

v = d[d$Segmentlabel == "v",]
bysubj_v = aggregate(v$TargetIntensity, by = list(v$SubjectNum, v$Condition), FUN = mean)
names(bysubj_v) = c("Subject", "Condition","TargetIntensity")
Intensity_Foot = bysubj_v[bysubj_v$Condition == "f", "TargetIntensity"]
Intensity_Vocal = bysubj_v[bysubj_v$Condition== "v", "TargetIntensity"]
t.test(Intensity_Foot, Intensity_Vocal, paired = TRUE)

pv = d[d$Segmentlabel == "pv",]
bysubj_pv = aggregate(pv$TargetIntensity, by = list(pv$SubjectNum, pv$Condition), FUN = mean)
names(bysubj_pv) = c("Subject", "Condition","TargetIntensity")

# get rid of subjects for which dont hae obs in both v and f conditions
vs = unique(bysubj_pv[bysubj_pv$Condition == "f","Subject"])
fs = unique(bysubj_pv[bysubj_pv$Condition == "v","Subject"])
bysubj_pv = bysubj_pv[which(bysubj_pv$Subject %in% intersect(vs,fs)), ]

Intensity_Foot = bysubj_pv[bysubj_pv$Condition == "f", "TargetIntensity"]
Intensity_Vocal = bysubj_pv[bysubj_pv$Condition== "v", "TargetIntensity"]
t.test(Intensity_Foot, Intensity_Vocal, paired = TRUE)



##misc stats
# total number of files used
length(unique(d$Filename))
total = 30 * 64

# look at number excluded
setwd('/Documents/CaLL/from work computer/Experiments/Activity/Data_Organized/Experiment #7 /Data/')
log = read.csv("Activity_7_Coding_Log.csv")
xlog = log[log$Affected.Region == "NC",]
xlog$Affected.Region <- droplevels(xlog$Affected.Region )
missing = as.data.frame(table(xlog$Subject, xlog$Affected.Region))
names(missing) = c("S", "code", "num.miss.files")

missingd = margin.table((table(d$SubjectNum, d$Target) == 0),1)
missingd  = as.data.frame(as.table(missingd))
names(missingd) = c("S", "num.miss.files")
#165 missing

xlog$Condition<- as.factor(sapply(str_split(matrix(sapply(str_split(xlog$Item,"_"),function(x) {x[1]})),"_"),function(x){x[1]}))
xlog$Issue <- droplevels(xlog$Issue)
margin.table(table(xlog$Condition, xlog$Issue),1)
table(xlog$Condition, xlog$Issue)
#F: missing verb phrase = 10; wrong verb =51; V: missing: 2; wrong verb = 102


# PLOTS
mIntensity <- aggregate(TargetIntensity ~ Condition + Segmentlabel, d, mean)
mIntensity$cih <- aggregate(TargetIntensity ~ Condition + Segmentlabel, data=d,FUN=ci.high)$TargetIntensity
mIntensity$cil <- aggregate(TargetIntensity ~ Condition + Segmentlabel, data=d,FUN=ci.low)$TargetIntensity
mIntensity$Condition = factor(mIntensity$Condition, levels = c("v", "f")) 
levels(mIntensity$Condition) <- c("vocal", "foot")
rect <- data.frame (xmin=2.9, xmax=5.1, ymin=-Inf, ymax=Inf)

pdf("/Users/mll/Desktop/Lewis_Fig3.pdf" ,width = 9, height = 6)
pd <- position_dodge(.1) # move them .05 to the left and right

ggplot(mIntensity, aes(x=Segmentlabel, y=TargetIntensity, group=Condition)) + 
  geom_errorbar(aes(ymin=TargetIntensity-cil, ymax= TargetIntensity+cih), 
                colour="black", width=.1, position=pd ) +
  geom_line(aes(linetype=Condition), position=pd) +
  geom_point(position=pd, size=3) +
  ggtitle("Experiment 3: Intensity by Sentence Region \n") +
  theme_bw(base_size=25)  + 
  labs (y = "Intensity (dB)", x= "Sentence Region") + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=30, vjust=.7), 
        axis.ticks = element_line(size = 1), 
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black', size = 1.1),
        plot.title = element_text(size=12, face="bold")) +
  geom_rect(aes(xmin=2.9, xmax=5.1, ymin=-Inf, ymax=Inf),
            color="grey",
            alpha = .01,
            size = 0, show_guide=FALSE) 

# qplot(data=mIntensity,
#       y=TargetIntensity,
#       x=Segmentlabel,
#       group=Condition,
#       ymin=TargetIntensity-cil,
#       ymax=TargetIntensity+cih
#       geom=c("point", "errorbar", "line"),
#       linetype=Condition,
#       width=0.2) + 
#   ggtitle("Experiment 3: Intensity by Sentence Region \n") +
#   theme_bw(base_size=25)  + 
#   labs (y = "Intensity (dB)", x= "Sentence Region") + 
#   theme(text = element_text(size=20),
#         axis.text.x = element_text(angle=30, vjust=.7), 
#         axis.ticks = element_line(size = 1), 
#         plot.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line(color = 'black', size = 1.1),
#         plot.title = element_text(size=12, face="bold")) +
#   geom_rect(aes(xmin=2.9, xmax=5.1, ymin=-Inf, ymax=Inf),
#             color="grey",
#             alpha = .01,
#             size = 0, show_guide=FALSE) 
dev.off()

mPitch <- aggregate(TargetPitch ~ Condition + Segmentlabel, d, mean)
mPitch$cih <- aggregate(TargetPitch ~ Condition + Segmentlabel, data=d,FUN=ci.high)$TargetPitch
mPitch$cil <- aggregate(TargetPitch ~ Condition + Segmentlabel, data=d,FUN=ci.low)$TargetPitch
qplot(data=mPitch,
      y=TargetPitch,
      x=Segmentlabel,
      group=Condition,
      ymin=TargetPitch-cil,
      ymax=TargetPitch+cih,
      geom=c("point", "errorbar", "line"),
      color=Condition,
      width=0.25) + 
  theme_bw(base_size=18)  + 
  labs (y = "Pitch (Hz)", x= "Sentence Region") 

mPitchMax <- aggregate(TargetMax ~ Condition + Segmentlabel, d, mean)
ggplot(mPitchMax, aes(x=Segmentlabel, y=TargetMax), fill=Condition) + geom_bar(stat="identity",position = "dodge", aes(fill=Condition))

mPitchMin <- aggregate(TargetMin ~ Condition + Segmentlabel, d, mean)
ggplot(mPitchMin, aes(x=Segmentlabel, y=TargetMin), fill=Condition) + geom_bar(stat="identity",position = "dodge", aes(fill=Condition))

mPitchDif <- aggregate(TargetDifference ~ Condition + Segmentlabel, d, mean)
ggplot(mPitchDif, aes(x=Segmentlabel, y=TargetDifference), fill=Condition) + geom_bar(stat="identity",position = "dodge", aes(fill=Condition))

mDuration <- aggregate(Duration ~ Condition + Segmentlabel, d, mean)
mDuration$cih <- aggregate(Duration ~ Condition + Segmentlabel, data=d,FUN=ci.high)$Duration
mDuration$cil <- aggregate(Duration ~ Condition + Segmentlabel, data=d,FUN=ci.low)$Duration
qplot(data=mDuration,
      y=Duration,
      x=Segmentlabel,
      group=Condition,
      ymin=Duration-cil,
      ymax=Duration+cih,
      geom=c("point", "errorbar", "line"),
      color=Condition,
      width=0.25) + 
  theme_bw(base_size=18)  + 
  labs (y = "Pitch (Hz)", x= "Sentence Region")


