##Activity 6 Analysis
#M.Lewis 11/8/12

#Load libraries
library(languageR)

##Read in data
d = read.csv("/Documents/CaLL/from work computer/Experiments/Activity/Data_Organized/Experiment #6/Data/Praat_output/Activity6_data.csv")
summary(d)

#Read in item data
item = read.csv("/Documents/CaLL/from work computer/Experiments/Activity/Data_Organized/Experiment #6/a6_item_data.csv") 
summary(item)

# Norms
loud = read.csv('/Documents/CaLL/from work computer/Experiments/Activity/Data_Organized/Norming Studies/data/loudness_item_means.csv')
effort = read.csv('/Documents/CaLL/from work computer/Experiments/Activity/Data_Organized/Norming Studies/data/effort_item_means.csv')

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

# MERGE NORMS
index <- match(d$words, loud$word)
d$loud <- loud$rating[index]

index <- match(d$words, effort$word)
d$effort <- effort$rating[index]

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

ggplot(d[d$Condition != "F",], aes(x=TargetIntensity, group = Condition, fill = Condition)) + geom_density(alpha = .4) + facet_grid(~Gender)
levels(d$Condition) <- c("filler","foot","vocal")
ggplot(d[d$Condition != "filler",], aes(Condition, TargetIntensity)) + 
  geom_boxplot(aes(fill = Condition)) + 
  geom_jitter() +
  ggtitle("Critical Items in Exp. 2")
#####################
#STATS

### Set up contrasts
#constrast code gender, mean centered
counttotal = length(d$Gender)
countM = length(d$Gender[d$Gender=='m'])
countF = length(d$Gender[d$Gender=='f'])
weightF = countM/counttotal
weightM = countF/counttotal

contrasts(d$Gender) <- cbind(c(weightF,-1*weightM))
contrasts(d$Gender) # display to check

# (1) P vs. V -- Analyis #1 in paper
d_crit = subset(d, d$Condition!='F') 
d_crit$Condition = factor(d_crit$Condition, levels = c("V","P")) 

counttotal = length(d_crit$Condition)
countP = length(d_crit$Condition[d_crit$Condition=='P'])
countV = length(d_crit$Condition[d_crit$Condition=='V'])
weightP = countP/counttotal
weightV = countV/counttotal
contrasts(d_crit$Condition) <- cbind(c(weightP,-1*weightV))
contrasts(d_crit$Condition) # display to check

# (2) Foot  vs. Mouth  vs. Low Vocal vs. High Vocal  -- Analysis #2 in paper
d_crit2 = subset(d, d$Condition3!='Filler')  
d_crit2$Condition3 = factor(d_crit2$Condition3) 
d_crit2$Item = factor(d_crit2$Item)
contrasts(d_crit2$Condition3) = contr.helmert

# (3) F vs. P vs. V
contrasts(d$Condition) = contr.helmert 

# (4) V vs. M
d_crit3 = subset(d, d$Condition4 != "other") 
d_crit3$Condition4 = factor(d_crit3$Condition4) 
contrasts(d_crit3$Condition4) # display to check

# (5) P vs. F
d_crit4 = subset(d, d$Condition!='V') 
#d_crit4 = subset(d_crit4, d_crit4$filler_activity!='high')
d_crit4 = subset(d_crit4, d_crit4$Condition4 != "M")
d_crit4$Condition = factor(d_crit4$Condition, levels = c("F","P")) # weight by number of items -> doesn't matter

# (6) P vs. L vs. H

d_crit5 = subset(d, d$Condition3!='Filler')
d_crit5 = subset(d_crit5, d_crit5$Condition3!='Mouth')
d_crit5$Condition3 = factor(d_crit5$Condition3) 
contrasts(d_crit5$Condition3) = contr.helmert

# (7) P vs. M vs. V
d_crit2$Condition5 <- ifelse(d_crit2$Condition3 == "High Vocal", "Vocal", 
				ifelse(d_crit2$Condition3 == "Low Vocal", "Vocal", 
				ifelse(d_crit2$Condition3 == "Mouth", "Mouth",
				ifelse (d_crit2$Condition3 == "Foot", "Foot", "x"))))
d_crit2$Condition5 <- factor(d_crit2$Condition5, levels = c("Foot","Mouth", "Vocal")) 
contrasts(d_crit2$Condition5) = contr.helmert 


### Run models for each comparision
# (1) compare P vs. V
m1= lmer(TargetIntensity ~ Condition + (1|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m1s= lmer(TargetIntensity ~ Condition + (1+Condition|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m2= lmer(TargetIntensity ~  Condition + Gender + (1|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m2s= lmer(TargetIntensity ~  Condition + Gender + (1+Condition|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m3= lmer(TargetIntensity ~  Condition + syllables + (1|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m3s= lmer(TargetIntensity ~  Condition + syllables + (1+Condition|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m4= lmer(TargetIntensity ~  Condition + Lg10WF + (1|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m4s= lmer(TargetIntensity ~  Condition + Lg10WF + (1+Condition|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m5= lmer(TargetIntensity ~  Condition + Gender + syllables + (1|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m5s= lmer(TargetIntensity ~  Condition + Gender + syllables + (1+Condition|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m6= lmer(TargetIntensity ~  Condition + Gender + Lg10WF + (1|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m6s= lmer(TargetIntensity ~  Condition + Gender + Lg10WF + (1+Condition|Subject) + (1|Item), REML=FALSE, data=d_crit) #<-- this is the best model

m7= lmer(TargetIntensity ~  Condition + syllables + Lg10WF + (1|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m7s= lmer(TargetIntensity ~  Condition + syllables + Lg10WF + (1+Condition|Subject) + (1|Item), REML=FALSE, data=d_crit) 

m8= lmer(TargetIntensity ~  Condition + Gender + syllables + Lg10WF  +  (1|Subject) + (1|Item), REML=FALSE, data=d_crit)  

m8s= lmer(TargetIntensity ~  Condition +  Gender + syllables + Lg10WF  +  (1+Condition|Subject) + (1|Item), REML=FALSE, data=d_crit) 

cont= lmer(scale(TargetIntensity) ~  Condition + effort+Gender + scale(syllables) + scale(Lg10WF)  +  (1|Subject) + (1|Item), REML=FALSE, data=d_crit) 


lmercompare(list(m1, m1s, m2, m2s, m3, m3s, m4, m4s, m5, m5s, m6, m6s, m7, m7s, m8, m8s))

#[1] MODEL       AIC         BIC         PREDICTORS
#[1] m1       15650.183    15680.756 *  Condition + (1 | Subject) + (1 | Item)
#[1] m1s       15639.364    15682.167    Condition + (1 + Condition | Subject) + (1 | Item)
#[1] m2       15648.247    15684.935    Condition + Gender + (1 | Subject) + (1 | Item)
#[1] m2s       15637.759    15686.676    Condition + Gender + (1 + Condition | Subject) + (1 | Item)
#[1] m3       15650.220    15686.908    Condition + syllables + (1 | Subject) + (1 | Item)
#[1] m3s       15639.401    15688.318    Condition + syllables + (1 + Condition | Subject) + (1 | Item)
#[1] m4       15649.258    15685.946    Condition + Lg10WF + (1 | Subject) + (1 | Item)
#[1] m4s       15638.438    15687.355    Condition + Lg10WF + (1 + Condition | Subject) + (1 | Item)
#[1] m5       15648.284    15691.086    Condition + Gender + syllables + (1 | Subject) + (1 | Item)
#[1] m5s       15637.796    15692.827    Condition + Gender + syllables + (1 + Condition | Subject) + (1 | Item)
#[1] m6       15647.322    15690.124    Condition + Gender + Lg10WF + (1 | Subject) + (1 | Item)
#[1] m6s       15636.833 *  15691.864    Condition + Gender + Lg10WF + (1 + Condition | Subject) + (1 | Item)
#[1] m7       15650.990    15693.792    Condition + syllables + Lg10WF + (1 | Subject) + (1 | Item)
#[1] m7s       15640.170    15695.202    Condition + syllables + Lg10WF + (1 + Condition | Subject) + (1 | Item)
#[1] m8       15649.054    15697.971    Condition + Gender + syllables + Lg10WF + (1 | Subject) + (1 | Item)
#[1] m8s       15638.565    15699.711    Condition + Gender + syllables + Lg10WF + (1 + Condition | Subject) + (1 | Item)

summary(m6s) #Best Model

myTapply(d_crit$TargetIntensity,list(d_crit$Condition),mean)

# t-test (reported in appendix)
boxplot(TargetIntensity~Condition,data=d_crit)
bysubj = aggregate(d_crit$TargetIntensity, by = list(d_crit$Subject, d_crit$Condition), FUN = mean)
names(bysubj) = c("Subject", "Semantics","TargetIntensity")

Intensity_P = bysubj[bysubj$Semantics == "P", "TargetIntensity"]
Intensity_V = bysubj[bysubj$Semantics == "V", "TargetIntensity"]
t.test(Intensity_V, Intensity_P, paired = TRUE)

#(2) Foot  vs. Mouth  vs. Low Vocal vs. High Vocal 
all_1= lmer(TargetIntensity ~ Condition3 + (1|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

all_1s= lmer(TargetIntensity ~ Condition3 + (1+Condition3|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

all_2= lmer(TargetIntensity ~  Condition3 + Gender + (1|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

all_2s= lmer(TargetIntensity ~  Condition3 + Gender + (1+Condition3|Subject) + (1|Item), REML=FALSE, data=d_crit2)

all_3= lmer(TargetIntensity ~  Condition3 + syllables + (1|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

all_3s= lmer(TargetIntensity ~  Condition3 + syllables + (1+Condition3|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

all_4= lmer(TargetIntensity ~  Condition3 + Lg10WF + (1|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

all_4s= lmer(TargetIntensity ~  Condition3 + Lg10WF + (1+Condition3|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

all_5= lmer(TargetIntensity ~  Condition3 + Gender + syllables + (1|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

all_5s= lmer(TargetIntensity ~  Condition3 + Gender + syllables + (1+Condition3|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

all_6= lmer(TargetIntensity ~  Condition3 + Gender + Lg10WF + (1|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

all_6s= lmer(TargetIntensity ~  Condition3 + Gender + Lg10WF + (1+Condition3|Subject) + (1|Item), REML=FALSE, data=d_crit2)  #<-- this is the best model

all_7= lmer(TargetIntensity ~  Condition3 + syllables + Lg10WF + (1|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

all_7s= lmer(TargetIntensity ~  Condition3 + syllables + Lg10WF + (1+Condition3|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

all_8= lmer(TargetIntensity ~  Condition3 + Gender + syllables + Lg10WF  +  (1|Subject) + (1|Item), REML=FALSE, data=d_crit2)  

all_8s= lmer(TargetIntensity ~  Condition3 +  Gender + syllables + Lg10WF  +  (1+Condition3|Subject) + (1|Item), REML=FALSE, data=d_crit2) 

lmercompare(list(all_1, all_1s, all_2, all_2s, all_3, all_3s, all_4, all_4s, all_5, all_5s, all_6, all_6s, all_7, all_7s, all_8, all_8s))

#[1] MODEL       AIC         BIC         PREDICTORS
#[1] all_1       19376.965    19421.276 *  Condition3 + (1 | Subject) + (1 | Item)
#[1] all_1s       19374.080    19475.362    Condition3 + (1 + Condition3 | Subject) + (1 | Item)
#[1] all_2       19375.058    19425.699    Condition3 + Gender + (1 | Subject) + (1 | Item)
#[1] all_2s       19370.990    19478.602    Condition3 + Gender + (1 + Condition3 | Subject) + (1 | Item)
#[1] all_3       19376.663    19427.305    Condition3 + syllables + (1 | Subject) + (1 | Item)
#[1] all_3s       19373.778    19481.391    Condition3 + syllables + (1 + Condition3 | Subject) + (1 | Item)
#[1] all_4       19376.739    19427.380    Condition3 + Lg10WF + (1 | Subject) + (1 | Item)
#[1] all_4s       19372.580    19480.192    Condition3 + Lg10WF + (1 + Condition3 | Subject) + (1 | Item)
#[1] all_5       19374.756    19431.727    Condition3 + Gender + syllables + (1 | Subject) + (1 | Item)
#[1] all_5s       19371.976    19485.918    Condition3 + Gender + syllables + (1 + Condition3 | Subject) + (1 | Item)
#[1] all_6       19374.832    19431.803    Condition3 + Gender + Lg10WF + (1 | Subject) + (1 | Item)
#[1] all_6s       19370.763 *  19484.706    Condition3 + Gender + Lg10WF + (1 + Condition3 | Subject) + (1 | Item)
#[1] all_7       19378.057    19435.028    Condition3 + syllables + Lg10WF + (1 | Subject) + (1 | Item)
#[1] all_7s       19375.171    19489.113    Condition3 + syllables + Lg10WF + (1 + Condition3 | Subject) + (1 | Item)
#[1] all_8       19376.149    19439.451    Condition3 + Gender + syllables + Lg10WF + (1 | Subject) + (1 | Item)
#[1] all_8s       19372.081    19492.354    Condition3 + Gender + syllables + Lg10WF + (1 + Condition3 | Subject) + (1 | Item)

summary(all_6s) #Best model
myTapply(d_crit2$TargetIntensity,list(d_crit2$Condition3),mean)

# by subject anova (reported in appendix)
bysubj = aggregate(d_crit2$TargetIntensity, by = list(d_crit2$Subject, d_crit2$Condition3), FUN = mean)
names(bysubj) = c("Subject", "Condition3","TargetIntensity")
summary(aov(TargetIntensity~Condition3 + Error(Subject/Condition3), data = bysubj))

# t.tests
Intensity_Foot = bysubj[bysubj$Condition3 == "Foot", "TargetIntensity"]
Intensity_Mouth = bysubj[bysubj$Condition3 == "Mouth", "TargetIntensity"]
Intensity_LV = bysubj[bysubj$Condition3 == "Low Vocal", "TargetIntensity"]
Intensity_HV = bysubj[bysubj$Condition3 == "High Vocal", "TargetIntensity"]

t.test(Intensity_Foot, Intensity_Mouth, paired = TRUE)
t.test(Intensity_Mouth, Intensity_LV, paired = TRUE)
t.test(Intensity_LV, Intensity_HV, paired = TRUE)


