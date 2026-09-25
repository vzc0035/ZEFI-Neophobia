###################################################################
# Title: "Neophobia in zebra finches is driven by the novelty, not developmental environment"
#
# Cox Proportional Hazard Models & Kaplan-Meir Survival Curves
# using coxme and survminer packages
# Last updated: July 27, 2026
##################################################################
# for help on plots: https://cran.r-project.org/web/packages/survminer/readme/README.html

####load necessary packages
library(survival)
library(survminer)
library(coxme)

####upload data
neophobia=read.csv(file.choose()) #2026_07_27_Neophobia.csv
object=read.csv(file.choose()) #2026_07_27_ObjectNeophobia.csv
food=read.csv(file.choose()) #2026_07_27_FoodNeophobia.csv
startle=read.csv(file.choose()) #2026_07_27_StartleTest.csv

####make appropriate data types (must be numeric for survival models)
##Note: r1food = just food neophobia; r1object = just object neophobia
##Note: r1neophobia = food and objects; r1startle = startle test
object$Age<-factor(object$Age)                                   #juvenile (=0) or adult (=1)
food$Age<-factor(food$Age)                                       #juvenile (=0) or adult (=1)
neophobia$Age<-factor(neophobia$Age)                             #juvenile (=0) or adult (=1)
startle$Age<-factor(startle$Age)                                 #juvenile (=0) or adult (=1)
object$BirdID<-factor(object$BirdID)                             #subject ID
food$BirdID<-factor(food$BirdID)                                 #subject ID
startle$BirdID<-factor(startle$BirdID)                           #subject ID
neophobia$BirdID<-factor(neophobia$BirdID)                       #subject ID
object$TrialOrder<-as.integer(object$TrialOrder)                 #order trials were performed
food$TrialOrder<-as.integer(food$TrialOrder)                     #order trials were performed
neophobia$TrialOrder<-as.integer(neophobia$TrialOrder)           #order trials were performed
startle$TrialOrder<-as.integer(startle$TrialOrder)               #order trials were performed
object$Trial<-factor(object$Trial)                               #object (0=control,1=blueberries,2=broccoli,3=pen,4=hairtie,5=apples,6=zucchini,7=film canister,8=hair clip)
food$Trial<-factor(food$Trial)                                   #object (0=control,1=blueberries,2=broccoli,3=pen,4=hairtie,5=apples,6=zucchini,7=film canister,8=hair clip)
neophobia$Trial<-factor(neophobia$Trial)                         #object (0=control,1=blueberries,2=broccoli,3=pen,4=hairtie,5=apples,6=zucchini,7=film canister,8=hair clip)
food$Object_noobject<-as.numeric(food$Object_noobject)           #was there a novel food (=1) or not (=0)
object$Object_noobject<-as.numeric(object$Object_noobject)       #was there a novel food (=1) or not (=0)
startle$Startle_nostartle<-as.numeric(startle$Startle_nostartle) #was this a startle test (=1) or control trial (=0)
object$Treatment<-factor(object$Treatment)                       #food-restricted (=1) or control (=0)
food$Treatment<-factor(food$Treatment)                           #food-restricted (=1) or control (=0)
neophobia$Treatment<-factor(neophobia$Treatment)                 #food-restricted (=1) or control (=0)
startle$Treatment<-factor(startle$Treatment)                     #food-restricted (=1) or control (=0)
object$TimeBowl<-as.numeric(object$TimeBowl)                     #time bird approached bowl
food$TimeBowl<-as.numeric(food$TimeBowl)                         #time bird approached bowl
neophobia$TimeBowl<-as.numeric(neophobia$TimeBowl)               #time bird approached bowl
startle$TimeBowl<-as.numeric(startle$TimeBowl)                   #time bird approached bowl
object$TimeFeed<-as.numeric(object$TimeFeed)                     #time fed from dish
food$TimeFeed<-as.numeric(food$TimeFeed)                         #time fed from dish
neophobia$TimeFeed<-as.numeric(neophobia$TimeFeed)               #time fed from dish
startle$TimeFeed<-as.numeric(startle$TimeFeed)                   #time fed from dish
object$StatusBowl<-as.numeric(object$StatusBowl)                 #did subject approach(=1) or not (=0)
food$StatusBowl<-as.numeric(food$StatusBowl)                     #did subject approach(=1) or not (=0)
neophobia$StatusBowl<-as.numeric(neophobia$StatusBowl)           #did subject approach (=1) or not (=0)
startle$StatusBowl<-as.numeric(startle$StatusBowl)               #did subject approach (=1) or not (=0)
startle$StatusFeed<-as.numeric(startle$StatusFeed)               #did subject feed (=1) or not (=0)
neophobia$StatusFeed<-as.numeric(neophobia$StatusFeed)           #did subject feed (=1) or not (=0)
object$StatusFeed<-as.numeric(object$StatusFeed)                 #did subject feed (=1) or not (=0)
food$StatusFeed<-as.numeric(food$StatusFeed)                     #did subject feed (=1) or not (=0)



########################
##CHECKING ASSUMPTIONS##
########################
#From: http://www.sthda.com/english/wiki/cox-model-assumptions
#Check Schoenfeld residuals to check the proportional hazards assumption
#Do not have continuous covariates, so no need to check Martingale residuals to assess nonlinearity
#Can also check deviance residuals (symmetric transformation of the Martingale residuals), to examine influential observations
#These tests can only be used for fixed effects, not random effects

#Make a Cox model with fixed effects only
res.cox1<-coxph(Surv(TimeBowl,StatusBowl)~Age*Treatment,data=object)
res.cox1
#Call:
#coxph(formula = Surv(TimeBowl, StatusBowl) ~ Age * Treatment, 
#    data = object)
#
#                   coef exp(coef) se(coef)      z     p
#Age1             0.1341    1.1435   0.2206  0.608 0.543
#Treatment1      -0.1236    0.8838   0.2013 -0.614 0.539
#Age1:Treatment1 -0.1116    0.8944   0.3139 -0.356 0.722
#
#Likelihood ratio test=1.52  on 3 df, p=0.6785
#n= 238, number of events= 168 

res.cox2<-coxph(Surv(TimeFeed,StatusFeed)~Age*Treatment,data=object)
res.cox2
#Call:
#coxph(formula = Surv(TimeFeed, StatusFeed) ~ Age * Treatment, 
#    data = object)
#
#                   coef exp(coef) se(coef)      z      p
#Age1             0.2451    1.2778   0.2394  1.024 0.3058
#Treatment1      -0.4470    0.6395   0.2346 -1.905 0.0567
#Age1:Treatment1  0.2122    1.2364   0.3505  0.606 0.5448
#
#Likelihood ratio test=7.98  on 3 df, p=0.04648
#n= 237, number of events= 134 
#   (1 observation deleted due to missingness)

res.cox3<-coxph(Surv(TimeBowl,StatusBowl)~Age*Treatment,data=food)
res.cox3
#Call:
#coxph(formula = Surv(TimeBowl, StatusBowl) ~ Age * Treatment, 
#    data = food)
#
#                    coef exp(coef) se(coef)      z        p
#Age1             1.04998   2.85758  0.19703  5.329 9.87e-08
#Treatment1      -0.08056   0.92260  0.18373 -0.438    0.661
#Age1:Treatment1 -0.05040   0.95085  0.26928 -0.187    0.852
#
#Likelihood ratio test=49.96  on 3 df, p=8.165e-11
#n= 249, number of events= 223 

res.cox4<-coxph(Surv(TimeFeed,StatusFeed)~Age*Treatment,data=food)
res.cox4
#Call:
#coxph(formula = Surv(TimeFeed, StatusFeed) ~ Age * Treatment, 
#    data = food)
#
#                   coef exp(coef) se(coef)      z        p
#Age1             1.2180    3.3806   0.2241  5.435 5.48e-08
#Treatment1      -0.4375    0.6457   0.2533 -1.727   0.0842
#Age1:Treatment1  0.1381    1.1481   0.3293  0.419   0.6749
#
#Likelihood ratio test=61.48  on 3 df, p=2.839e-13
#n= 248, number of events= 156 
#   (1 observation deleted due to missingness)

res.cox5<-coxph(Surv(TimeBowl,StatusBowl)~Age*Treatment,data=startle)
res.cox5
#Call:
#coxph(formula = Surv(TimeBowl, StatusBowl) ~ Age * Treatment, 
#    data = startle)
#
#                    coef exp(coef) se(coef)      z      p
#Age1             0.46164   1.58668  0.19012  2.428 0.0152
#Treatment1      -0.16753   0.84575  0.16751 -1.000 0.3172
#Age1:Treatment1 -0.01371   0.98638  0.26420 -0.052 0.9586
#
#Likelihood ratio test=12.63  on 3 df, p=0.00552
#n= 247, number of events= 247 

res.cox6<-coxph(Surv(TimeFeed,StatusFeed)~Age*Treatment,data=startle)
res.cox6
#Call:
#coxph(formula = Surv(TimeFeed, StatusFeed) ~ Age * Treatment, 
#    data = startle)
#
#                   coef exp(coef) se(coef)      z      p
#Age1             0.1289    1.1375   0.1881  0.685 0.4932
#Treatment1      -0.2954    0.7442   0.1666 -1.773 0.0762
#Age1:Treatment1  0.1168    1.1239   0.2615  0.447 0.6550
#
#Likelihood ratio test=5.73  on 3 df, p=0.1253
#n= 247, number of events= 246 

object_juv <- object[object$Age == 0, ]
object_juv$Trial <- droplevels(object_juv$Trial)
res.cox7<-coxph(Surv(TimeBowl,StatusBowl)~Trial,data=object_juv)
res.cox7
#Call:
#coxph(formula = Surv(TimeBowl, StatusBowl) ~ Trial, data = object_juv)
#
#         coef exp(coef) se(coef)     z        p
#Trial4 0.7322    2.0797   0.2085 3.512 0.000444
#
#Likelihood ratio test=12.77  on 1 df, p=0.0003514
#n= 141, number of events= 99 

object_adult <- object[object$Age == 1, ]
object_adult$Trial <- droplevels(object_adult$Trial)
res.cox8<-coxph(Surv(TimeBowl,StatusBowl)~Trial,data=object_adult)
res.cox8
#Call:
#coxph(formula = Surv(TimeBowl, StatusBowl) ~ Trial, data = object_adult)
#
#          coef exp(coef) se(coef)     z        p
#Trial8  2.3529   10.5157   0.2984 7.885 3.14e-15
#
#Likelihood ratio test=70.52  on 1 df, p=< 2.2e-16
#n= 97, number of events= 69 

object_juv <- object[object$Age == 0, ]
object_juv$Trial <- droplevels(object_juv$Trial)
res.cox9<-coxph(Surv(TimeFeed,StatusFeed)~Trial,data=object_juv)
res.cox9
#Call:
#coxph(formula = Surv(TimeFeed, StatusFeed) ~ Trial, data = object_juv)
#
#         coef exp(coef) se(coef)     z       p
#Trial4 0.7076    2.0290   0.2394 2.956 0.00311
#
#Likelihood ratio test=9.09  on 1 df, p=0.002567
#n= 141, number of events= 75 

object_adult <- object[object$Age == 1, ]
object_adult$Trial <- droplevels(object_adult$Trial)
res.cox10<-coxph(Surv(TimeFeed,StatusFeed)~Trial,data=object_adult)
res.cox10
#Call:
#coxph(formula = Surv(TimeFeed, StatusFeed) ~ Trial, data = object_adult)
#
#          coef exp(coef) se(coef)     z        p
#Trial8  2.7622   15.8351   0.3676 7.514 5.73e-14
#
#Likelihood ratio test=78.31  on 1 df, p=< 2.2e-16
#n= 96, number of events= 59 
#   (1 observation deleted due to missingness)

food_juv <- food[food$Age == 0, ]
food_juv$Trial <- droplevels(food_juv$Trial)
res.cox11<-coxph(Surv(TimeBowl,StatusBowl)~Trial,data=food_juv)
res.cox11
#Call:
#coxph(formula = Surv(TimeBowl, StatusBowl) ~ Trial, data = food_juv)
#
#         coef exp(coef) se(coef)     z       p
#Trial2 1.2918    3.6393   0.1969 6.562 5.3e-11
#
#Likelihood ratio test=43.97  on 1 df, p=3.339e-11
#n= 143, number of events= 119 

food_adult <- food[food$Age == 1, ]
food_adult$Trial <- droplevels(food_adult$Trial)
res.cox12<-coxph(Surv(TimeBowl,StatusBowl)~Trial,data=food_adult)
res.cox12
#Call:
#coxph(formula = Surv(TimeBowl, StatusBowl) ~ Trial, data = food_adult)
#
#          coef exp(coef) se(coef)      z     p
#Trial6 -0.1517    0.8593   0.1980 -0.766 0.444
#
#Likelihood ratio test=0.59  on 1 df, p=0.444
#n= 106, number of events= 104 

food_juv <- food[food$Age == 0, ]
food_juv$Trial <- droplevels(food_juv$Trial)
res.cox13<-coxph(Surv(TimeFeed,StatusFeed)~Trial,data=food_juv)
res.cox13
#Call:
#coxph(formula = Surv(TimeFeed, StatusFeed) ~ Trial, data = food_juv)
#
#         coef exp(coef) se(coef)     z        p
#Trial2 2.1257    8.3789   0.3342 6.361 2.01e-10
#
#Likelihood ratio test=56.65  on 1 df, p=5.198e-14
#n= 143, number of events= 65 

food_adult <- food[food$Age == 1, ]
food_adult$Trial <- droplevels(food_adult$Trial)
res.cox14<-coxph(Surv(TimeFeed,StatusFeed)~Trial,data=food_adult)
res.cox14
#Call:
#coxph(formula = Surv(TimeFeed, StatusFeed) ~ Trial, data = food_adult)
#
#         coef exp(coef) se(coef)     z     p
#Trial6 0.2735    1.3146   0.2108 1.298 0.194
#
#Likelihood ratio test=1.69  on 1 df, p=0.194
#n= 105, number of events= 91 
#   (1 observation deleted due to missingness)

#Proportional hazards assumption 
#using the function cox.zph() in the survival package
#significant p-values indicate this assumption is violated
test.ph1<-cox.zph(res.cox1)
test.ph1
#              chisq df     p
#Age           1.496  1 0.221
#Treatment     4.628  1 0.031
#Age:Treatment 0.117  1 0.732
#GLOBAL        6.323  3 0.097

test.ph2<-cox.zph(res.cox2)
test.ph2
#              chisq df     p
#Age           4.383  1 0.036
#Treatment     2.442  1 0.118
#Age:Treatment 0.604  1 0.437
#GLOBAL        8.196  3 0.042

test.ph3<-cox.zph(res.cox3)
test.ph3
#                 chisq df    p
#Age           5.88e-01  1 0.44
#Treatment     5.08e-01  1 0.48
#Age:Treatment 7.47e-05  1 0.99
#GLOBAL        1.16e+00  3 0.76

test.ph4<-cox.zph(res.cox4)
test.ph4
#               chisq df    p
#Age           0.0102  1 0.92
#Treatment     0.9551  1 0.33
#Age:Treatment 0.8343  1 0.36
#GLOBAL        1.3842  3 0.71

test.ph5<-cox.zph(res.cox5)
test.ph5
#              chisq df     p
#Age           4.345  1 0.037
#Treatment     0.392  1 0.531
#Age:Treatment 0.608  1 0.436
#GLOBAL        5.085  3 0.166

test.ph6<-cox.zph(res.cox6)
test.ph6
#              chisq df     p
#Age           3.938  1 0.047
#Treatment     0.162  1 0.687
#Age:Treatment 4.587  1 0.032
#GLOBAL        5.558  3 0.135

test.ph7<-cox.zph(res.cox7)
test.ph7
#       chisq df    p
#Trial  0.412  1 0.52
#GLOBAL 0.412  1 0.52

test.ph8<-cox.zph(res.cox8)
test.ph8
#       chisq df    p
#Trial    4.2  1 0.04
#GLOBAL   4.2  1 0.04

test.ph9<-cox.zph(res.cox9)
test.ph9
#       chisq df    p
#Trial  0.946  1 0.33
#GLOBAL 0.946  1 0.33

test.ph10<-cox.zph(res.cox10)
test.ph10
#       chisq df    p
#Trial   0.69  1 0.41
#GLOBAL  0.69  1 0.41

test.ph11<-cox.zph(res.cox11)
test.ph11
#       chisq df    p
#Trial   1.33  1 0.25
#GLOBAL  1.33  1 0.25

test.ph12<-cox.zph(res.cox12)
test.ph12
#       chisq df    p
#Trial  0.396  1 0.53
#GLOBAL 0.396  1 0.53

test.ph13<-cox.zph(res.cox13)
test.ph13
#       chisq df    p
#Trial   1.81  1 0.18
#GLOBAL  1.81  1 0.18

test.ph14<-cox.zph(res.cox14)
test.ph14
#       chisq df    p
#Trial  0.262  1 0.61
#GLOBAL 0.262  1 0.61

#Schoenfeld residuals indicate a mild violation of the proportional hazards assumption for Treatment (p = 0.031); 
#given data constraints, we report the average hazard ratio over the observation period."

#Checking the scaled Schoenfeld residuals against the transformed time 
#Using ggcoxzph() in the survminer package
ggcoxzph(test.ph1)
ggcoxzph(test.ph2)
ggcoxzph(test.ph3)
ggcoxzph(test.ph4)
ggcoxzph(test.ph5)
ggcoxzph(test.ph6)
ggcoxzph(test.ph7)
ggcoxzph(test.ph8)
ggcoxzph(test.ph9)
ggcoxzph(test.ph10)
ggcoxzph(test.ph11)
ggcoxzph(test.ph12)
ggcoxzph(test.ph13)
ggcoxzph(test.ph14)

#Checking for influential observations
#Using the function ggcoxdiagnostics() in the survminer package: 
ggcoxdiagnostics(res.cox1,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox2,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox3,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox4,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox5,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox6,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox7,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox8,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox9,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox10,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox11,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox12,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox13,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())
ggcoxdiagnostics(res.cox14,type="deviance", linear.predictions=FALSE, ggtheme=theme_bw())


##############################################################
##ARE OBJECTS AND NOVEL FOOD PROVIDING A NEOPHOBIC RESPONSE?##
##############################################################
survdiff(Surv(TimeBowl, StatusBowl) ~ Object_noobject, data = object)
#Call:
#survdiff(formula = Surv(TimeBowl, StatusBowl) ~ Object_noobject, 
#    data = object)
#
#                    N Observed Expected (O-E)^2/E (O-E)^2/V
#Object_noobject=0 120      120     42.3     142.5       239
#Object_noobject=1 238      168    245.7      24.6       239
#
# Chisq= 240  on 1 degrees of freedom, p= <2e-16 

survdiff(Surv(TimeFeed, StatusFeed) ~ Object_noobject, data = object)
#Call:
#survdiff(formula = Surv(TimeFeed, StatusFeed) ~ Object_noobject, 
#    data = object)
#
#n=357, 1 observation deleted due to missingness.
#
#                    N Observed Expected (O-E)^2/E (O-E)^2/V
#Object_noobject=0 120      120     36.5     190.9       273
#Object_noobject=1 237      134    217.5      32.1       273
#
# Chisq= 273  on 1 degrees of freedom, p= <2e-16 
plot(survfit(Surv(TimeFeed, StatusFeed) ~ Object_noobject, data = object))

survdiff(Surv(TimeBowl, StatusBowl) ~ Object_noobject, data = food)
#Call:
#survdiff(formula = Surv(TimeBowl, StatusBowl) ~ Object_noobject, 
#    data = food)
#
#                    N Observed Expected (O-E)^2/E (O-E)^2/V
#Object_noobject=0 120      120     49.9      98.3       152
#Object_noobject=1 249      223    293.1      16.7       152
#
# Chisq= 152  on 1 degrees of freedom, p= <2e-16 

survdiff(Surv(TimeFeed, StatusFeed) ~ Object_noobject, data = food)
#Call:
#survdiff(formula = Surv(TimeFeed, StatusFeed) ~ Object_noobject, 
#    data = food)
#
#n=368, 1 observation deleted due to missingness.
#
#                    N Observed Expected (O-E)^2/E (O-E)^2/V
#Object_noobject=0 120      120     38.2     175.2       241
#Object_noobject=1 248      156    237.8      28.1       241
#
# Chisq= 242  on 1 degrees of freedom, p= <2e-16 
plot(survfit(Surv(TimeFeed, StatusFeed) ~ Object_noobject, data = food))

all.novel=coxme(Surv(TimeBowl,StatusBowl)~Trial+(1|Nest/BirdID),data=neophobia)
all.novel
emmeans(all.novel,pairwise~Trial)
#$contrasts
# contrast        estimate    SE  df z.ratio p.value
# Trial0 - Trial1   2.8953 0.194 Inf  14.904  <.0001
# Trial0 - Trial2   1.5602 0.167 Inf   9.350  <.0001
# Trial0 - Trial3   3.0291 0.208 Inf  14.591  <.0001
# Trial0 - Trial4   2.2272 0.180 Inf  12.355  <.0001
# Trial0 - Trial5   1.1711 0.183 Inf   6.415  <.0001
# Trial0 - Trial6   1.2719 0.181 Inf   7.025  <.0001
# Trial0 - Trial7   3.8258 0.261 Inf  14.675  <.0001
# Trial0 - Trial8   1.3683 0.189 Inf   7.227  <.0001


all.novel=coxme(Surv(TimeFeed,StatusFeed)~Trial+(1|Nest/BirdID),data=neophobia)
all.novel
emmeans(all.novel,pairwise~Trial)
#$emmeans
# Trial emmean    SE  df asymp.LCL asymp.UCL
# 0      2.244 0.119 Inf    2.0118    2.4769
# 1     -2.415 0.276 Inf   -2.9554   -1.8744
# 2     -0.034 0.137 Inf   -0.3029    0.2348
# 3     -1.158 0.182 Inf   -1.5151   -0.8006
# 4     -0.239 0.146 Inf   -0.5246    0.0475
# 5      0.245 0.161 Inf   -0.0709    0.5609
# 6      0.644 0.153 Inf    0.3439    0.9437
# 7     -1.994 0.289 Inf   -2.5595   -1.4278
# 8      0.914 0.157 Inf    0.6073    1.2210
#
#Results are given on the log (not the response) scale. 
#Confidence level used: 0.95 
#
#$contrasts
# contrast        estimate    SE  df z.ratio p.value
# Trial0 - Trial1    4.659 0.336 Inf  13.878  <.0001
# Trial0 - Trial2    2.278 0.189 Inf  12.061  <.0001
# Trial0 - Trial3    3.402 0.236 Inf  14.444  <.0001
# Trial0 - Trial4    2.483 0.197 Inf  12.616  <.0001
# Trial0 - Trial5    1.999 0.207 Inf   9.665  <.0001
# Trial0 - Trial6    1.601 0.193 Inf   8.282  <.0001
# Trial0 - Trial7    4.238 0.337 Inf  12.589  <.0001
# Trial0 - Trial8    1.330 0.192 Inf   6.934  <.0001


###########################
##TESTING FOR HABITUATION##
###########################
##Remove control trials from sheet
all.habit=coxme(Surv(TimeFeed,StatusFeed)~TrialOrder+Trial+(1|Nest/BirdID),data=neophobia)
all.habit
#Cox mixed-effects model fit by maximum likelihood
#  Data: neophobia
#  events, n = 290, 485 (2 observations deleted due to missingness)
#  Iterations= 10 65 
#                    NULL Integrated    Fitted
#Log-likelihood -1681.533  -1548.721 -1487.209
#
#                   Chisq    df p    AIC    BIC
#Integrated loglik 265.62 10.00 0 245.62 208.93
# Penalized loglik 388.65 53.96 0 280.73  82.71
#
#Model:  Surv(TimeFeed, StatusFeed) ~ TrialOrder + Trial + (1 | Nest/BirdID) 
#Fixed coefficients
#                   coef  exp(coef)  se(coef)     z       p
#TrialOrder  0.085584895  1.0893540 0.0376149  2.28 2.3e-02
#Trial2      2.506236129 12.2587029 0.3381401  7.41 1.2e-13
#Trial3      1.310526886  3.7081270 0.3581515  3.66 2.5e-04
#Trial4      2.282290982  9.7991043 0.3421470  6.67 2.5e-11
#Trial5      2.382786931 10.8350574 0.4049093  5.88 4.0e-09
#Trial6      2.737808145 15.4530770 0.4085569  6.70 2.1e-11
#Trial7     -0.006705265  0.9933172 0.4751472 -0.01 9.9e-01
#Trial8      3.041431621 20.9351931 0.4060048  7.49 6.8e-14
#
#Random effects
# Group       Variable    Std Dev   Variance 
# Nest/BirdID (Intercept) 0.5665985 0.3210339
# Nest        (Intercept) 0.4946127 0.2446417

#################################################################
##EFFECT OF FOOD RESTRICTION ON NEOPHOBIA AND STARTLE BEHAVIOUR##
#################################################################
####remove control trials from files before running stats
###Note: One limitation of this study is that the novel foods and objects are confounded with Age.
###Because of this, Age and Trial cannot be in the same model.
###To fix this, have a primary model that tests the primary hypothesis (Age:Treatment, Age, and Treatment)
###and a secondary model that tests the novel objects/foods themselves to test for object/food effects.

##Primary model: object neophobia, approaching the bowl
object.bowl.primary <- coxme(Surv(TimeBowl, StatusBowl) ~ Age + Treatment + Age:Treatment + TrialOrder + (1|Nest/BirdID),
                   data = object)
object.bowl.primary
#Cox mixed-effects model fit by maximum likelihood
#  Data: object
#  events, n = 168, 238
#  Iterations= 6 27 
#                    NULL Integrated    Fitted
#Log-likelihood -837.6168  -834.2997 -821.4616
#
#                  Chisq    df         p   AIC    BIC
#Integrated loglik  6.63  6.00 0.3560000 -5.37 -24.11
# Penalized loglik 32.31 14.09 0.0037861  4.13 -39.89
#
#Model:  Surv(TimeBowl, StatusBowl) ~ Age + Treatment + Age:Treatment +      TrialOrder + (1 | Nest/BirdID) 
#Fixed coefficients
#                       coef exp(coef)   se(coef)     z    p
#Age1            -0.36718756 0.6926797 0.34585033 -1.06 0.29
#Treatment1      -0.25192272 0.7773048 0.26026791 -0.97 0.33
#TrialOrder       0.05746614 1.0591494 0.04627398  1.24 0.21
#Age1:Treatment1  0.02583907 1.0261758 0.32513999  0.08 0.94
#
#Random effects
# Group       Variable    Std Dev      Variance    
# Nest/BirdID (Intercept) 0.0198386142 0.0003935706
# Nest        (Intercept) 0.3717580992 0.1382040843
confint(object.bowl.primary)
#                      2.5 %    97.5 %
#Age1            -1.04504175 0.3106666
#Treatment1      -0.76203846 0.2581930
#TrialOrder      -0.03322919 0.1481615
#Age1:Treatment1 -0.61142360 0.6631017

##Secondary model: object neophobia, approaching the bowl, juvenile
object_juv <- object[object$Age == 0, ]
object_juv$Trial <- droplevels(object_juv$Trial)
object.bowl.sec.j <- coxme(Surv(TimeBowl, StatusBowl) ~ Trial + (1|Nest/BirdID), data = object_juv)
object.bowl.sec.j
#Cox mixed-effects model fit by maximum likelihood
#  Data: object_juv
#  events, n = 99, 141
#  Iterations= 20 86 
#                    NULL Integrated   Fitted
#Log-likelihood -442.3972   -424.184 -353.982
#
#                   Chisq    df          p   AIC    BIC
#Integrated loglik  36.43  3.00 6.0846e-08 30.43  22.64
# Penalized loglik 176.83 51.55 1.2212e-15 73.73 -60.05
#
#Model:  Surv(TimeBowl, StatusBowl) ~ Trial + (1 | Nest/BirdID) 
#Fixed coefficients
#           coef exp(coef)  se(coef)    z       p
#Trial4 1.161007  3.193146 0.2421874 4.79 1.6e-06
#
#Random effects
# Group       Variable    Std Dev   Variance 
# Nest/BirdID (Intercept) 1.2206016 1.4898683
# Nest        (Intercept) 0.7476261 0.5589448
confint(object.bowl.sec.j)
#           2.5 %   97.5 %
#Trial4 0.6863281 1.635685

##Secondary model: object neophobia, approaching the bowl, adult
object_adult <- object[object$Age == 1, ]
object_adult$Trial <- droplevels(object_adult$Trial)
object.bowl.sec.a <- coxme(Surv(TimeBowl, StatusBowl) ~ Trial + (1|Nest/BirdID), data = object_adult)
object.bowl.sec.a
#Cox mixed-effects model fit by maximum likelihood
#  Data: object_adult
#  events, n = 69, 97
#  Iterations= 7 39 
#                    NULL Integrated    Fitted
#Log-likelihood -282.0644  -245.2335 -231.1686
#
#                   Chisq    df          p   AIC   BIC
#Integrated loglik  73.66  3.00 6.6613e-16 67.66 60.96
# Penalized loglik 101.79 13.03 7.7716e-16 75.74 46.63
#
#Model:  Surv(TimeBowl, StatusBowl) ~ Trial + (1 | Nest/BirdID) 
#Fixed coefficients
#           coef exp(coef)  se(coef)    z       p
#Trial8 2.791456  16.30475 0.3442375 8.11 5.6e-16
#
#Random effects
# Group       Variable    Std Dev   Variance 
# Nest/BirdID (Intercept) 0.3244389 0.1052606
# Nest        (Intercept) 0.4898538 0.2399567
confint(object.bowl.sec.a)
#          2.5 %  97.5 %
#Trial8 2.116763 3.46615

##Primary model: food neophobia, approaching the bowl
food.bowl.primary <- coxme(Surv(TimeBowl, StatusBowl) ~ Age + Treatment + Age:Treatment + TrialOrder + (1|Nest/BirdID),
                   data = food)
food.bowl.primary
#Cox mixed-effects model fit by maximum likelihood
#  Data: food
#  events, n = 223, 249
#  Iterations= 8 43 
#                    NULL Integrated    Fitted
#Log-likelihood -1067.262  -1040.893 -1030.765
#
#                  Chisq    df          p   AIC   BIC
#Integrated loglik 52.74  6.00 1.3243e-09 40.74 20.30
# Penalized loglik 72.99 12.17 1.0378e-10 48.65  7.16
#
#Model:  Surv(TimeBowl, StatusBowl) ~ Age + Treatment + Age:Treatment +      TrialOrder + (1 | Nest/BirdID) 
#Fixed coefficients
#                       coef exp(coef)   se(coef)     z      p
#Age1             0.97106065 2.6407439 0.31514008  3.08 0.0021
#Treatment1      -0.08050597 0.9226494 0.22073986 -0.36 0.7200
#TrialOrder       0.02537490 1.0256996 0.04234503  0.60 0.5500
#Age1:Treatment1 -0.10434040 0.9009186 0.28083292 -0.37 0.7100
#
#Random effects
# Group       Variable    Std Dev      Variance    
# Nest/BirdID (Intercept) 0.0200611257 0.0004024488
# Nest        (Intercept) 0.2704870817 0.0731632614
confint(food.bowl.primary)
#                      2.5 %    97.5 %
#Age1             0.35339744 1.5887239
#Treatment1      -0.51314814 0.3521362
#TrialOrder      -0.05761983 0.1083696
#Age1:Treatment1 -0.65476282 0.4460820

##Secondary model: food neophobia, approaching the bowl, juvenile
food_juv <- food[food$Age == 0, ]
food_juv$Trial <- droplevels(food_juv$Trial)
food.bowl.sec.j <- coxme(Surv(TimeBowl, StatusBowl) ~ Trial + (1|Nest/BirdID), data = food_juv)
food.bowl.sec.j
#Cox mixed-effects model fit by maximum likelihood
#  Data: food_juv
#  events, n = 119, 143
#  Iterations= 8 47 
#                   NULL Integrated    Fitted
#Log-likelihood -515.303  -487.5016 -434.2506
#
#                  Chisq   df          p   AIC    BIC
#Integrated loglik  55.6  3.0 5.1062e-12 49.60  41.27
# Penalized loglik 162.1 42.8 8.8818e-16 76.51 -42.44
#
#Model:  Surv(TimeBowl, StatusBowl) ~ Trial + (1 | Nest/BirdID) 
#Fixed coefficients
#           coef exp(coef)  se(coef)   z       p
#Trial2 1.828349  6.223604 0.2343955 7.8 6.2e-15
#
#Random effects
# Group       Variable    Std Dev    Variance  
# Nest/BirdID (Intercept) 0.91637422 0.83974172
# Nest        (Intercept) 0.25543767 0.06524841
confint(food.bowl.sec.j)
#          2.5 %   97.5 %
#Trial2 1.368942 2.287756

#Secondary model: food neophobia, approaching the bowl, adult
food_adult <- food[food$Age == 1, ]
food_adult$Trial <- droplevels(food_adult$Trial)
food.bowl.sec.a <- coxme(Surv(TimeBowl, StatusBowl) ~ Trial + (1|Nest/BirdID), data = food_adult)
food.bowl.sec.a
#Cox mixed-effects model fit by maximum likelihood
#  Data: food_adult
#  events, n = 104, 106
#  Iterations= 7 31 
#                    NULL Integrated    Fitted
#Log-likelihood -390.8828  -389.9176 -384.1856
#
#                  Chisq   df        p   AIC    BIC
#Integrated loglik  1.93 3.00 0.586970 -4.07 -12.00
# Penalized loglik 13.39 6.13 0.039933  1.14 -15.06
#
#Model:  Surv(TimeBowl, StatusBowl) ~ Trial + (1 | Nest/BirdID) 
#Fixed coefficients
#             coef exp(coef)  se(coef)     z    p
#Trial6 -0.1540528 0.8572268 0.2029446 -0.76 0.45
#
#Random effects
# Group       Variable    Std Dev      Variance    
# Nest/BirdID (Intercept) 0.0199238528 0.0003969599
# Nest        (Intercept) 0.2836439352 0.0804538820
confint(food.bowl.sec.a)
#            2.5 %    97.5 %
#Trial6 -0.5518169 0.2437113

##startle test, approaching the bowl
startle.bowl <- coxme(Surv(TimeBowl, StatusBowl) ~ Age + Treatment + Age:Treatment +
                    + (1|Nest/BirdID),
                   data = startle)
startle.bowl
#Cox mixed-effects model fit by maximum likelihood
#  Data: startle
#  events, n = 127, 127
#  Iterations= 7 39 
#                    NULL Integrated    Fitted
#Log-likelihood -491.5534  -486.1277 -469.8549
#
#                  Chisq    df          p  AIC    BIC
#Integrated loglik 10.85  5.00 0.05440700 0.85 -13.37
# Penalized loglik 43.40 16.73 0.00036357 9.94 -37.64
#
#Model:  Surv(TimeBowl, StatusBowl) ~ Age + Treatment + Age:Treatment +      +(1 | Nest/BirdID) 
#Fixed coefficients
#                      coef exp(coef)  se(coef)     z    p
#Age1             0.4100966  1.506963 0.2968327  1.38 0.17
#Treatment1      -0.4359098  0.646676 0.2944985 -1.48 0.14
#Age1:Treatment1  0.2134810  1.237980 0.4052937  0.53 0.60
#
#Random effects
# Group       Variable    Std Dev   Variance 
# Nest/BirdID (Intercept) 0.2731205 0.0745948
# Nest        (Intercept) 0.3524056 0.1241897
confint(startle.bowl)
#                     2.5 %    97.5 %
#Age1            -0.1716847 0.9918780
#Treatment1      -1.0131164 0.1412967
#Age1:Treatment1 -0.5808801 1.0078421

##Primary model: object neophobia, feeding
object.feed.primary=coxme(Surv(TimeFeed,StatusFeed)~Age + Treatment + Age:Treatment + TrialOrder +
                     (1|Nest/BirdID),data=object)
object.feed.primary
#Cox mixed-effects model fit by maximum likelihood
#  Data: object
#  events, n = 134, 237 (1 observation deleted due to missingness)
#  Iterations= 8 36 
#                    NULL Integrated    Fitted
#Log-likelihood -684.9694  -677.6822 -663.3516
#
#                  Chisq    df          p   AIC    BIC
#Integrated loglik 14.57  6.00 0.02383900  2.57 -14.81
# Penalized loglik 43.24 15.03 0.00014772 13.17 -30.40
#
#Model:  Surv(TimeFeed, StatusFeed) ~ Age + Treatment + Age:Treatment +      TrialOrder + (1 | Nest/BirdID) 
#Fixed coefficients
#                       coef exp(coef)   se(coef)     z     p
#Age1            -0.33439938 0.7157679 0.38029127 -0.88 0.380
#Treatment1      -0.58751775 0.5557050 0.30796383 -1.91 0.056
#TrialOrder       0.06841647 1.0708112 0.05185879  1.32 0.190
#Age1:Treatment1  0.36801317 1.4448611 0.36240150  1.02 0.310
#
#Random effects
# Group       Variable    Std Dev      Variance    
# Nest/BirdID (Intercept) 0.0198839831 0.0003953728
# Nest        (Intercept) 0.4581095243 0.2098643363
confint(object.feed.primary)
#                      2.5 %     97.5 %
#Age1            -1.07975657 0.41095781
#Treatment1      -1.19111578 0.01608027
#TrialOrder      -0.03322488 0.17005783
#Age1:Treatment1 -0.34228073 1.07830706

##Secondary model: object neophobia, feeding, juvenile
object_juv <- object[object$Age == 0, ]
object_juv$Trial <- droplevels(object_juv$Trial)
object.feed.sec.j <- coxme(Surv(TimeFeed, StatusFeed) ~ Trial + (1|Nest/BirdID), data = object_juv)
object.feed.sec.j
#Cox mixed-effects model fit by maximum likelihood
#  Data: object_juv
#  events, n = 75, 141
#  Iterations= 24 103 
#                    NULL Integrated    Fitted
#Log-likelihood -346.6368  -326.5635 -261.5899
#
#                   Chisq    df          p   AIC    BIC
#Integrated loglik  40.15  3.00 9.9190e-09 34.15  27.19
# Penalized loglik 170.09 48.33 1.7764e-15 73.43 -38.57
#
#Model:  Surv(TimeFeed, StatusFeed) ~ Trial + (1 | Nest/BirdID) 
#Fixed coefficients
#           coef exp(coef)  se(coef)    z       p
#Trial4 1.319359  3.741022 0.2883467 4.58 4.7e-06
#
#Random effects
# Group       Variable    Std Dev  Variance
# Nest/BirdID (Intercept) 1.247605 1.556519
# Nest        (Intercept) 1.143400 1.307364
confint(object.feed.sec.j)
#           2.5 %   97.5 %
#Trial4 0.7542098 1.884508

##Secondary model: object neophobia, feeding, adult
object_adult <- object[object$Age == 1, ]
object_adult$Trial <- droplevels(object_adult$Trial)
object.feed.sec.a <- coxme(Surv(TimeFeed, StatusFeed) ~ Trial + (1|Nest/BirdID), data = object_adult)
object.feed.sec.a
#Cox mixed-effects model fit by maximum likelihood
#  Data: object_adult
#  events, n = 59, 96 (1 observation deleted due to missingness)
#  Iterations= 7 32 
#                    NULL Integrated    Fitted
#Log-likelihood -246.0488  -205.7799 -192.0952
#
#                   Chisq    df p   AIC   BIC
#Integrated loglik  80.54  3.00 0 74.54 68.31
# Penalized loglik 107.91 12.86 0 82.19 55.47
#
#Model:  Surv(TimeFeed, StatusFeed) ~ Trial + (1 | Nest/BirdID) 
#Fixed coefficients
#           coef exp(coef)  se(coef)    z     p
#Trial8 3.201035  24.55795 0.4074425 7.86 4e-15
#
#Random effects
# Group       Variable    Std Dev   Variance 
# Nest/BirdID (Intercept) 0.3798609 0.1442943
# Nest        (Intercept) 0.4809805 0.2313422
confint(object.feed.sec.a)
#          2.5 %   97.5 %
#Trial8 2.402463 3.999608

##Primary model: food neophobia, feeding
food.feed.primary=coxme(Surv(TimeFeed,StatusFeed)~Age + Treatment + Age:Treatment + TrialOrder +
                     (1|Nest/BirdID),data=food)
food.feed.primary
#Cox mixed-effects model fit by maximum likelihood
#  Data: food
#  events, n = 156, 248 (1 observation deleted due to missingness)
#  Iterations= 8 43 
#                   NULL Integrated    Fitted
#Log-likelihood -795.821   -761.562 -748.9101
#
#                  Chisq    df          p   AIC   BIC
#Integrated loglik 68.52  6.00 8.2290e-13 56.52 38.22
# Penalized loglik 93.82 13.86 6.2172e-14 66.10 23.83
#
#Model:  Surv(TimeFeed, StatusFeed) ~ Age + Treatment + Age:Treatment +      TrialOrder + (1 | Nest/BirdID) 
#Fixed coefficients
#                       coef exp(coef)   se(coef)     z     p
#Age1             0.89911230  2.457421 0.36114403  2.49 0.013
#Treatment1      -0.39479220  0.673820 0.30467757 -1.30 0.200
#TrialOrder       0.07316406  1.075907 0.05030127  1.45 0.150
#Age1:Treatment1  0.13387040  1.143245 0.34258067  0.39 0.700
#
#Random effects
# Group       Variable    Std Dev     Variance   
# Nest/BirdID (Intercept) 0.019963141 0.000398527
# Nest        (Intercept) 0.384363729 0.147735476
confint(food.feed.primary)
#                      2.5 %    97.5 %
#Age1             0.19128300 1.6069416
#Treatment1      -0.99194927 0.2023649
#TrialOrder      -0.02542461 0.1717527
#Age1:Treatment1 -0.53757538 0.8053162

##Secondary model: object neophobia, feeding, juvenile
food_juv <- food[food$Age == 0, ]
food_juv$Trial <- droplevels(food_juv$Trial)
food.feed.sec.j <- coxme(Surv(TimeFeed, StatusFeed) ~ Trial + (1|Nest/BirdID), data = food_juv)
food.feed.sec.j
#Cox mixed-effects model fit by maximum likelihood
#  Data: food_juv
#  events, n = 65, 143
#  Iterations= 9 50 
#                    NULL Integrated    Fitted
#Log-likelihood -305.1661  -270.6671 -226.4637
#
#                  Chisq    df          p   AIC   BIC
#Integrated loglik  69.0  3.00 6.9944e-15 63.00 56.47
# Penalized loglik 157.4 35.66 0.0000e+00 86.08  8.53
#
#Model:  Surv(TimeFeed, StatusFeed) ~ Trial + (1 | Nest/BirdID) 
#Fixed coefficients
#           coef exp(coef)  se(coef)    z       p
#Trial2 3.097673  22.14635 0.3883727 7.98 1.6e-15
#
#Random effects
# Group       Variable    Std Dev   Variance 
# Nest/BirdID (Intercept) 0.8318653 0.6920000
# Nest        (Intercept) 0.9568343 0.9155319
confint(food.feed.sec.j)
#          2.5 %   97.5 %
#Trial2 2.336476 3.858869

##Secondary model: object neophobia, feeding, adult
food_adult <- food[food$Age == 1, ]
food_adult$Trial <- droplevels(food_adult$Trial)
food.feed.sec.a <- coxme(Surv(TimeFeed, StatusFeed) ~ Trial + (1|Nest/BirdID), data = food_adult)
food.feed.sec.a
#Cox mixed-effects model fit by maximum likelihood
#  Data: food_adult
#  events, n = 91, 105 (1 observation deleted due to missingness)
#  Iterations= 18 78 
#                    NULL Integrated    Fitted
#Log-likelihood -361.7213  -356.3558 -318.2743
#
#                  Chisq    df          p   AIC    BIC
#Integrated loglik 10.73  3.00 1.3273e-02  4.73  -2.80
# Penalized loglik 86.89 30.72 2.8826e-07 25.45 -51.69
#
#Model:  Surv(TimeFeed, StatusFeed) ~ Trial + (1 | Nest/BirdID) 
#Fixed coefficients
#            coef exp(coef)  se(coef)    z    p
#Trial6 0.3447424  1.411626 0.2372789 1.45 0.15
#
#Random effects
# Group       Variable    Std Dev   Variance 
# Nest/BirdID (Intercept) 0.7642194 0.5840312
# Nest        (Intercept) 0.5775666 0.3335832
confint(food.feed.sec.a)
#            2.5 %    97.5 %
#Trial6 -0.1203156 0.8098005

##startle test, feeding
startle.feed=coxme(Surv(TimeFeed,StatusFeed)~Age + Treatment + Age:Treatment +
                     (1|Nest/BirdID),data=startle)
startle.feed
#Cox mixed-effects model fit by maximum likelihood
#  Data: startle
#  events, n = 126, 127
#  Iterations= 7 33 
#                    NULL Integrated    Fitted
#Log-likelihood -491.5534  -486.2658 -460.2067
#
#                  Chisq    df          p   AIC    BIC
#Integrated loglik 10.58  5.00 6.0482e-02  0.58 -13.61
# Penalized loglik 62.69 24.26 3.0176e-05 14.17 -54.63
#
#Model:  Surv(TimeFeed, StatusFeed) ~ Age + Treatment + Age:Treatment +      (1 | Nest/BirdID) 
#Fixed coefficients
#                       coef exp(coef)  se(coef)     z    p
#Age1             0.14079669 1.1511906 0.2994956  0.47 0.64
#Treatment1      -0.49982604 0.6066362 0.3283204 -1.52 0.13
#Age1:Treatment1  0.05045164 1.0517460 0.4192032  0.12 0.90
#
#Random effects
# Group       Variable    Std Dev   Variance 
# Nest/BirdID (Intercept) 0.4050262 0.1640462
# Nest        (Intercept) 0.4522404 0.2045214
confint(startle.feed)
#                     2.5 %    97.5 %
#Age1            -0.4462039 0.7277973
#Treatment1      -1.1433221 0.1436700
#Age1:Treatment1 -0.7711716 0.8720749

##########
##GRAPHS##
##########
fit.all.neo<-survfit(Surv(TimeFeed,StatusFeed)~Trial,data=neophobia)
ggsurvplot(fit.all.neo,
           data=neophobia,
           size=0.5, #change line size
           #conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
		   palette = c("200101","#0A75AD","#A3E90B","#CC0B4E","#6DC89E","#81425F","#E59625","#DF6280","#C6C1C6"), #custom colour
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Control","Blueberries", "Broccoli", "Pen", "Hairtie", "Apples", "Zucchini", "Film canister", "Hair clip"), #change legend labels
           legend.title = "Novel Object",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to feed", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

###primary models
fit.object.bowl<-survfit(Surv(TimeBowl,StatusBowl)~Treatment,data=object)
ggsurvplot(fit.object.bowl,
           data=object,
           size=0.5, #change line size
           palette = c("#0004FF","#FF0008"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Control","Food-restricted"), #change legend labels
           legend.title = "Treatment",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to approach bowl", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.object.feed<-survfit(Surv(TimeFeed,StatusFeed)~Treatment,data=object)
ggsurvplot(fit.object.feed,
           data=object,
           size=0.5, #change line size
           palette = c("#0004FF","#FF0008"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Control","Food-restricted"), #change legend labels
           legend.title = "Treatment",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to feed", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)


fit.food.bowl<-survfit(Surv(TimeBowl,StatusBowl)~Treatment,data=food)
ggsurvplot(fit.food.bowl,
           data=food,
           size=0.5, #change line size
           palette = c("#0004FF","#FF0008"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Control","Food-restricted"), #change legend labels
           legend.title = "Treatment",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to approach bowl", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.food.feed<-survfit(Surv(TimeFeed,StatusFeed)~Treatment,data=food)
ggsurvplot(fit.food.feed,
           data=food,
           size=0.5, #change line size
           palette = c("#0004FF","#FF0008"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Control","Food-restricted"), #change legend labels
           legend.title = "Treatment",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to feed", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.food.bowl.age<-survfit(Surv(TimeBowl,StatusBowl)~Age,data=food)
ggsurvplot(fit.food.bowl.age,
           data=food,
           size=0.5, #change line size
           palette = c("#0004FF","#FF0008"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Juvenile","Adult"), #change legend labels
           legend.title = "Treatment",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to approach bowl", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.food.feed.age<-survfit(Surv(TimeFeed,StatusFeed)~Age,data=food)
ggsurvplot(fit.food.feed.age,
           data=food,
           size=0.5, #change line size
           palette = c("#0004FF","#FF0008"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Juvenile","Adult"), #change legend labels
           legend.title = "Treatment",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to feed", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.startle.bowl<-survfit(Surv(TimeBowl,StatusBowl)~Treatment,data=startle)
ggsurvplot(fit.startle.bowl,
           data=startle,
           size=0.5, #change line size
           palette = c("#0004FF","#FF0008"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Control","Food-restricted"), #change legend labels
           legend.title = "Treatment",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to approach bowl", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.startle.feed<-survfit(Surv(TimeFeed,StatusFeed)~Treatment,data=startle)
ggsurvplot(fit.startle.feed,
           data=startle,
           size=0.5, #change line size
           palette = c("#0004FF","#FF0008"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Control","Food-restricted"), #change legend labels
           legend.title = "Treatment",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to feed", #change y-axis label
           break.time.by = 60,
           ggtheme = theme_bw() #change ggplot theme
)

###secondary Models
object_juv <- object[object$Age == 0, ]
object_juv$Trial <- droplevels(object_juv$Trial)

object_adult <- object[object$Age == 1, ]
object_adult$Trial <- droplevels(object_adult$Trial)

food_juv <- food[food$Age == 0, ]
food_juv$Trial <- droplevels(food_juv$Trial)

food_adult <- food[food$Age == 1, ]
food_adult$Trial <- droplevels(food_adult$Trial)

fit.object.bowl.j<-survfit(Surv(TimeBowl,StatusBowl)~Trial,data=object_juv)
ggsurvplot(fit.object.bowl.j,
           data=object_juv,
           size=0.5, #change line size
           palette = c("#81425F","#E59625"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Pen","Hairtie"), #change legend labels
           legend.title = "Novel Object",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to approach bowl", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.object.bowl.a<-survfit(Surv(TimeBowl,StatusBowl)~Trial,data=object_adult)
ggsurvplot(fit.object.bowl.a,
           data=object_adult,
           size=0.5, #change line size
           palette = c("#DF6280","#C6C1C6"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Film Canister","Hair clip"), #change legend labels
           legend.title = "Novel Object",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to approach bowl", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.object.feed.j<-survfit(Surv(TimeFeed,StatusFeed)~Trial,data=object_juv)
ggsurvplot(fit.object.feed.j,
           data=object_juv,
           size=0.5, #change line size
           palette = c("#81425F","#E59625"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Pen","Hairtie"), #change legend labels
           legend.title = "Novel Object",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to feed", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.object.feed.a<-survfit(Surv(TimeFeed,StatusFeed)~Trial,data=object_adult)
ggsurvplot(fit.object.feed.a,
           data=object_adult,
           size=0.5, #change line size
           palette = c("#DF6280","#C6C1C6"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Film Canister","Hair clip"), #change legend labels
           legend.title = "Novel Object",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to feed", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.food.bowl.j<-survfit(Surv(TimeBowl,StatusBowl)~Trial,data=food_juv)
ggsurvplot(fit.food.bowl.j,
           data=food_juv,
           size=0.5, #change line size
           palette = c("#0A75AD", "#A3E90B"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Blueberries","Broccoli"), #change legend labels
           legend.title = "Novel Food",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to approach bowl", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.food.bowl.a<-survfit(Surv(TimeBowl,StatusBowl)~Trial,data=food_adult)
ggsurvplot(fit.food.bowl.a,
           data=food_adult,
           size=0.5, #change line size
           palette = c("#CC0B4E","#6DC89E"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Apples","Zucchini"), #change legend labels
           legend.title = "Novel Food",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to approach bowl", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.food.feed.j<-survfit(Surv(TimeFeed,StatusFeed)~Trial,data=food_juv)
ggsurvplot(fit.food.feed.j,
           data=food_juv,
           size=0.5, #change line size
           palette = c("#0A75AD","#A3E90B"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Blueberries","Broccoli"), #change legend labels
           legend.title = "Novel Food",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to feed", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)

fit.food.feed.a<-survfit(Surv(TimeFeed,StatusFeed)~Trial,data=food_adult)
ggsurvplot(fit.food.feed.a,
           data=food_adult,
           size=0.5, #change line size
           palette = c("#CC0B4E","#6DC89E"), #custom colour
           conf.int = TRUE, #add confidence interval
           #conf.int.style = "ribbon", #"ribbon or "step"
           #conf.int.alpha = 0.3, #0 = transparent & 1 = not transparent
           pval=FALSE, #add p-value
           pval.coord=c(2700,0.9), #change position of p-value
           legend.labs = c("Apples","Zucchini"), #change legend labels
           legend.title = "Novel Food",
           xlab = "Time (seconds)", #change x-axis label
           ylab = "Proportion yet to feed", #change y-axis label
           break.time.by = 300,
           ggtheme = theme_bw() #change ggplot theme
)