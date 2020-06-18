library(lme4)
library(car)
library(bbmle)

# Field Experiment Survival ####
exp = read.csv("all.exps.csv")
surv.a = exp[exp$treatment != 'cage',] # exclude cage so I can test for caging effects within each site

# model selection to evaluate for procedural control
surv.null.a <- glmer(cbind(nlives,dead)~ 1 + (1|experiment), data = surv.a, family ="binomial")
surv1.a <- glmer(cbind(nlives,dead)~ treatment + (1|experiment), data = surv.a, family ="binomial")
AICctab(logLik(surv.null.a), logLik(surv1.a), weights=TRUE, delta=TRUE)
#dAICc df weight
#logLik(surv.null.a) 0.0   2  0.65  
#logLik(surv1.a)     1.2   3  0.35  

surv.b = exp[exp$treatment != 'cage.control',]# exclude cage-controls
surv.null <- glmer(cbind(nlives,dead)~ 1 + (1|experiment), data = surv.b, family ="binomial")
surv1 <- glmer(cbind(nlives,dead)~ treatment + (1|experiment), data = surv.b, family ="binomial")
surv2 <- glmer(cbind(nlives,dead)~ complexity + (1|experiment), data = surv.b, family ="binomial")
surv3 <- glmer(cbind(nlives,dead)~ treatment + complexity + (1|experiment), data = surv.b, family ="binomial")
surv4 <- glmer(cbind(nlives,dead)~ treatment*complexity + (1|experiment), data = surv.b, family ="binomial")
surv5 <- glmer(cbind(nlives,dead)~ density + (1|experiment), data = surv.b, family ="binomial")
surv6 <- glmer(cbind(nlives,dead)~ density + complexity + (1|experiment), data = surv.b, family ="binomial")
surv7 <- glmer(cbind(nlives,dead)~ density*complexity + (1|experiment), data = surv.b, family ="binomial")
surv8 <- glmer(cbind(nlives,dead)~ density + treatment + (1|experiment), data = surv.b, family ="binomial")
surv9 <- glmer(cbind(nlives,dead)~ density*treatment + (1|experiment), data = surv.b, family ="binomial")
surv10 <- glmer(cbind(nlives,dead)~ treatment*shelled + (1|experiment), data = surv.b, family ="binomial")
surv11 <- glmer(cbind(nlives,dead)~ treatment + shelled + (1|experiment), data = surv.b, family ="binomial")
surv12 <- glmer(cbind(nlives,dead)~ shelled + (1|experiment), data = surv.b, family ="binomial")



AICctab(logLik(surv.null), logLik(surv1),logLik(surv2), logLik(surv3), logLik(surv4),logLik(surv5),logLik(surv6),logLik(surv7),logLik(surv8),logLik(surv9), logLik(surv10),logLik(surv11),logLik(surv12),weights=TRUE, delta=TRUE)

#dAICc df weight
#logLik(surv4)       0.0 7  0.86  
#logLik(surv3)       3.6 5  0.14  
#logLik(surv8)      39.5 4  <0.001
#logLik(surv1)      41.2 3  <0.001
#logLik(surv9)      41.2 5  <0.001
#logLik(surv6)     266.8 5  <0.001
#logLik(surv2)     267.3 4  <0.001
#logLik(surv7)     269.0 7  <0.001
#logLik(surv5)     293.2 3  <0.001
#logLik(surv.null) 294.1 2  <0.001

#treat.complex      surv
#1        cage.high 0.9242424 ####40% difference
#2     control.high 0.5222222
 effect.high =  (0.924 - 0.522)/0.522
# 0.7701149
 
#3         cage.low 0.8969697 ######70% difference
#4      control.low 0.1936937
 effect.low =  (0.897 - 0.193)/0.193
 # 3.647668
#5    cage.moderate 0.9416667 ##### 30% difference
#6 control.moderate 0.6072222
 effect.moderate =  (0.942 - 0.607)/0.607
 #0.5518946
 
 # Model selection with stuff that does not distinguish between moderate and high complexity
 #dAICc df weight
 #logLik(surv10)      0.0 5  0.562 
 #logLik(surv4)       1.0 7  0.349 
 #logLik(surv3)       4.6 5  0.057 
 #logLik(surv11)      5.7 4  0.033 
 #logLik(surv8)      40.5 4  <0.001
 #logLik(surv1)      42.1 3  <0.001
 #logLik(surv9)      42.2 5  <0.001
 #logLik(surv6)     267.7 5  <0.001
 #logLik(surv2)     268.3 4  <0.001
 #logLik(surv12)    268.6 3  <0.001
 #logLik(surv7)     269.9 7  <0.001
 #logLik(surv5)     294.2 3  <0.001
 #logLik(surv.null) 295.0 2  <0.001
 
 # calculate effect size of just shelled
 surv.b$treat.shelled = interaction(surv.b$treatment,surv.b$shelled) # concatenate factors of region and treatment
 avg.surv.shelled = aggregate(surv~treat.shelled, data = surv.b, FUN = mean)
 treat.shelled      surv
 #1       cage.no 0.8969697
 #2    control.no 0.1936937
 #3      cage.yes 0.9325397
 #4   control.yes 0.5626984
 
 # No shelled effect size = # 3.647668
 # Shelled effect size = 
 shelled.effect = (0.932 - 0.562)/0.562
 # = 0.658363
 
 # Field Experiment Recruitment ####
 exp.recruit = read.csv("all.exp.recruit.csv")
 # number of adults is already factored into the response variable so will not include it as predictor in this model
 model.recruit <- lmer(avg.recruit~complexity + (1|experiment), data = exp.recruit)

 recruit.null <- lmer(avg.recruit~ (1|experiment), data = exp.recruit)
 recruit1 <- lmer(avg.recruit~ complexity + (1|experiment), data = exp.recruit)
 
 AICctab(logLik(recruit.null), logLik(recruit1), weights=TRUE, delta=TRUE)
 #dAICc df weight
 #logLik(recruit.null) 0.0   3  0.68  
 #logLik(recruit1)     1.5   5  0.32 
 
 # Survey 2016 for Restoration Success ####
 survey = read.csv("apalach_restoration_survey_summary_14Jan2020.csv")
 # collpase to transect level and use this as independent replicates
 
 # collapse this to total number of total.recruits for each field.site*reef*treatment*id*density
 # this only gives three replicates per site, so will unfortunately just use the quadrat level
 survey$log.adults = log(1+survey$total.adults)
 survey$log.spat = log(1+survey$total.spat)
 
 survey.adult.null = lm(log.adults~1, data = survey)
 survey.adult1 = lm(log.adults~sample.wt, data = survey)
 survey.adult2 = lm(log.adults~sample.wt + I(sample.wt^2), data = survey)
 
 AICctab(logLik(survey.adult.null), logLik(survey.adult1), logLik(survey.adult2), weights=TRUE, delta=TRUE)
 #dAICc df weight
 #logLik(survey.adult2)      0.0  4  0.91  
 #logLik(survey.adult1)      4.6  3  0.09  
 #logLik(survey.adult.null) 65.5  2  <0.001
 

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6121 -0.6485 -0.2754  0.6201  1.9856 
 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -0.50050    0.25565  -1.958   0.0552 .  
# sample.wt       0.88208    0.16024   5.505 9.16e-07 ***
#   I(sample.wt^2) -0.04183    0.01585  -2.639   0.0107 *  

# Residual standard error: 1.041 on 57 degrees of freedom
# Multiple R-squared:  0.6884,	Adjusted R-squared:  0.6775 
# F-statistic: 62.97 on 2 and 57 DF,  p-value: 3.685e-15
 

 juv.surv.null = lm(log.spat~1, data = survey)
 juv.surv1 = lm(log.spat~sample.wt, data = survey)
 juv.surv2 = lm(log.spat~sample.wt + I(sample.wt^2), data = survey)
 
 AICctab(logLik(juv.surv.null), logLik(juv.surv1), logLik(juv.surv2), weights=TRUE, delta=TRUE)
 #dAICc df weight
 #logLik(juv.surv2)      0.0  4  0.9916
 #logLik(juv.surv1)      9.5  3  0.0084
 #logLik(juv.surv.null) 66.1  2  <0.001
 
 
 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -0.17455    0.17166  -1.017 0.313517    
# sample.wt       0.67963    0.10759   6.317 4.33e-08 ***
#   I(sample.wt^2) -0.03753    0.01064  -3.526 0.000841 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6992 on 57 degrees of freedom
## Multiple R-squared:  0.692,	Adjusted R-squared:  0.6812 
# F-statistic: 64.04 on 2 and 57 DF,  p-value: 2.646e-15
 
 
# Figures for field experiment ####
 #rename and re-order sites
 surv.b$complexity=factor(surv.b$complexity, levels=c("low", "moderate", "high"))
 levels(surv.b$complexity)[levels(surv.b$complexity)=="low"] <- "Low"
 levels(surv.b$complexity)[levels(surv.b$complexity)=="moderate"] <- "Moderate"
 levels(surv.b$complexity)[levels(surv.b$complexity)=="high"] <- "High"

 
 #pd <- position_dodge(0.50)
 # produce Figure 2a-c. This was further modified in Adobe 
 fig2a <-ggplot(data=surv.b,
                        mapping = aes(x=complexity, 
                                      y = surv)) +
   #facet_grid(rows = vars(site),cols=vars(tile.type)) +
   geom_boxplot(aes(fill=treatment),outlier.shape = NA,alpha=0.5) +
   scale_fill_manual(values=c("blue", "white"))+
   #geom_jitter(color = 'black', width = 0.1, size=0.4,height=0.05) +
   # I could not get the data points to overlay the correct box plot. I varied "width" in the above line of code
   ylab("Adult oyster survival\n(proportional)")+
   xlab("Structual complexity")+
   theme_classic(base_size = 18) +
   theme(
     strip.background = element_blank(),
     strip.text.x = element_blank(),
     strip.text.y = element_blank()) +
   theme(legend.position = "none")
 
 
exp.recruit = read.csv("all.exp.recruit.csv")
 #rename and re-order sites
exp.recruit$complexity=factor(exp.recruit$complexity, levels=c("low", "moderate","high"))
levels(exp.recruit$complexity)[levels(exp.recruit$complexity)=="low"] <- "Low"
levels(exp.recruit$complexity)[levels(exp.recruit$complexity)=="moderate"] <- "Moderate"
levels(exp.recruit$complexity)[levels(exp.recruit$complexity)=="high"] <- "High"

 
 #pd <- position_dodge(0.50)
 # produce Figure 2a-c. This was further modified in Adobe 
 fig2b <-ggplot(data=exp.recruit,
                mapping = aes(x=complexity, 
                              y = avg.recruit)) +
   #facet_grid(rows = vars(site),cols=vars(tile.type)) +
   geom_boxplot(aes(fill=treatment),outlier.shape = NA,alpha=0.5) +
   scale_fill_manual(values=c("blue"), 
                     name=NULL, labels=c("cage"))+
   geom_jitter(color='black',width=0.3,size=0.4,height=0.05) +
   ylab("Monthly oyster\nrecruitment")+
   xlab("Structual complexity")+
   theme_classic(base_size = 20) +
   theme(
     strip.background = element_blank(),
     strip.text.x = element_blank(),
     strip.text.y = element_blank()) +
   theme(legend.position = "none")
 
 grid.arrange(fig2a,fig2b, ncol = 1)
 
 fig3a = ggplot(survey, aes(sample.wt, log.adults)) +
   geom_point(size = 3) +
   stat_smooth(method = "lm", formula = y ~ x + I(x^2), color = 'blue', size = 1) +
   ylim(0,5) +
   xlim(0,11) +
   ylab("Adult oyster density\n(log transformed)")+
   xlab("Structual complexity [reef mass kg]")+
 theme_classic(base_size = 20) 
 
 fig3b = ggplot(survey, aes(sample.wt, log.spat)) +
   geom_point(size = 3) +
   stat_smooth(method = "lm", formula = y ~ x + I(x^2), color = 'blue', size = 1) +
   ylim(0,5) +
   xlim(0,11) +
   ylab("Juvenile oyster density\n(log transformed)")+
   xlab("Structual complexity [reef mass kg]")+
   theme_classic(base_size = 20) 

 grid.arrange(fig3a,fig3b, ncol = 1)
 