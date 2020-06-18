# Analysis of size distributions for reshelling/restoration analysis in Apalachicola
# March 2020 
# Data from David - see email on 3/16/2020

library(ggplot2)
library(bbmle)
library(RColorBrewer)

# Load data
D <- read.csv('survey_sizedistribution_13March2020.csv')
D <- subset(D,D$live.newg=='live')

# A simple plot...this could be made to look nicer. 
# Add a new column that will alphabetize the treatments in the desired way:
D$comp2 <- NA
D$comp2[D$complexity=='low']<-'1.l'
D$comp2[D$complexity=='moderate']<-'2.m'
D$comp2[D$complexity=='high']<-'3.h'
D$comp2 <- as.factor(D$comp2)

# This plots the histo as counts. To show the *distribution* (independent of sample size), adjust to stat(density) below
ggplot(data=D,aes(x=size.mm))+
  geom_freqpoly(aes(color=comp2,x=size.mm,stat(count)),binwidth=5,size=1)+
  scale_color_brewer(palette='Blues')+
  xlab('Oyster size (mm)')+
  ylab('Frequency')+
  theme_classic(base_size = 20)+
  theme(legend.position='none')


  
# Bootstrap analysis:
Boots = 1e4
thresh = c(FALSE,TRUE,TRUE)
complexity = c(0,153,506)
DD<-data.frame(complexity=rep(complexity,Boots),thresh=rep(thresh,Boots),median.size = rep(NA,Boots*3))
for (i in 1:Boots){
 Index = (i-1)*3+1 
 DD$median.size[Index] = median(sample(D$size.mm[D$complexity=='low'],replace=TRUE,size=sum(D$complexity=='low')))
 DD$median.size[Index+1] = median(sample(D$size.mm[D$complexity=='moderate'],replace=TRUE,size=sum(D$complexity=='moderate')))
 DD$median.size[Index+2] = median(sample(D$size.mm[D$complexity=='high'],replace=TRUE,size=sum(D$complexity=='high')))
}

m1 = lm(median.size~1,data=DD)
m2 = lm(median.size~thresh,data=DD)
m3 = lm(median.size~complexity,data=DD)

AICtab(m1,m2,m3,weights=TRUE)

#Output:
#dAIC df weight
#m2  0.0 3  1     
#m3 21.2 3  <0.001
#m1 21.3 2  <0.001

# I checked and this result is completely insensitive to the number of bootstraps, so this is not a problem with computational overkill.

