################################################################################
####clear everything.
rm(list=ls())

#### package check
pkg <- c("car", "psych", "ggplot2", "sandwich", "estimatr", "summarytools", 
         "tidyverse",  "expss", "flextable", "sjPlot", "lmtest",
         "huxtable", "haven", "BFpack", "dotwhisker", "ggeffects")
new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
if(length(new.pkg)) install.packages(new.pkg)

#### packages.
library(car)
library(psych)
library(ggplot2)
library(sandwich)
library(estimatr)
library(expss)
library(summarytools)
library(tidyverse)
library(lmtest)
library(huxtable)
library(haven)
library(dotwhisker)
library(sjPlot)
library(ggeffects)
library(BFpack)

################################################################################
######### utility functions

std01<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)
}

################################################################################
######### persistent Weber ggplot() theme

ggtheme = theme(
  plot.title=element_text(face="bold",hjust=0,vjust=0,colour="#3C3C3C",size=18),
  axis.text.x=element_text(size=12,colour="#535353",face="bold"),
  axis.text.y=element_text(size=12,colour="#535353",face="bold"),
  axis.title = element_text(size=14,colour="#535353",face="bold"),
  axis.title.y=element_text(size=14,colour="#535353",face="bold",vjust=1.5),
  axis.ticks=element_blank(),
  strip.text.x = element_text(size = 14),
  panel.grid.major=element_line(colour="#D0D0D0",linewidth=.25),
  panel.background=element_rect(fill="white"),
  legend.text=element_text(size=14),
  legend.title=element_text(size=14)) 

################################################################################

##### load Rdata if coded and saved previously
load("exp1.Rdata")

################################################################################

##### load data from csv
wff1<-read.csv("wff1.csv")
demo1<-read.csv("demo1.csv")

##### make sure case ID variable has same name in both dataframes
demo1$RESPONDENT_ID<-demo1$hash_value

##### merge survey + demographics
exp1 <- left_join(wff1, demo1, by="RESPONDENT_ID")

##### retain check-passed cases only
data <- exp1 %>% dplyr::filter(exp1$atnchk=="2") 

################################################################################
######## coding

# convert variables to numeric
data <- data %>% mutate_at(c("pk1", "pk2", "pk3", "pk4", "pk5",
                             "choice", "choice.b", "Q121", "ft_1", "ft_2",
                             "effb_1", "effs_1", "valb_1", "vals_1", 
                             "checkb", "checks", "int", "age", "education",
                             "gender", "income", "political_ideology",
                             "political_party_preference", "religious_identity",
                             "region", "neighborhood", "hispanic"), 
                             as.numeric)

# age = age

# religion indicators
data$jewish<-ifelse(data$religious_identity==5, 1, 0)
data$cath<-ifelse(data$religious_identity==2, 1, 0)
data$prot<-ifelse(data$religious_identity==1, 1, 0)
data$other<-car::recode(data$religious_identity, "1:2=0; 3:4=1; 5=0; 6:10=1; 
                        98:99=1")

# white respondent indicator
data$white<-ifelse(data$ethnicity==1 & data$hispanic==2, 1, 0) 

# south indicator
data$south<-factor(ifelse(data$region==3, 1, 0))

# urban/rural/suburban
data$resid<-ifelse(data$neighborhood<99, data$neighborhood, NA)
data$resid<-factor(data$resid, labels = c("Urban", 
                                          "Suburban", 
                                          "Rural"))

# education
data$reduc<-car::recode(data$education, "1=0; 2=1; 3:5=2; 6=3; 7:9=4") 
data$college<-ifelse(data$education>5, 1, 0 )

# gender
data$male<-factor(ifelse(data$gender==1, 1, 0))
data$female<-factor(ifelse(data$gender==2, 1, 0))

# income
data$inc<-ifelse(data$income<99, data$income, NA)
data$rinc<-std01(data$inc)

# ideology
data$ideo7<-data$political_ideology-1
data$rideo7<-std01(data$political_ideology)

# party id, 7 point
data$pid7<-ifelse(data$political_party_preference<98, 
                  data$political_party_preference-1, NA)
data$pid7<-replace(data$pid7, 
                   data$political_party_preference==98 |  
                   data$political_party_preference==99, 3)
data$rpid7<-std01(data$pid7)

# party id, three categories (with leaners)
data$pid3<-NA
data$pid3<-replace(data$pid3, data$pid7<3, 0)
data$pid3<-replace(data$pid3, data$pid7==3, 1)
data$pid3<-replace(data$pid3, data$pid7>3, 2)

data$pid3<-factor(data$pid3, labels = c("Democrat", 
                                        "Independent", 
                                        "Republican"))

# party id, Democratic and Republican dummy variables (with leaners)
data$idem<-ifelse(data$pid3=="Democrat", 1 , 0)
data$irep<-ifelse(data$pid3=="Republican", 1 , 0)
data$iind<-ifelse(data$pid3=="Independent", 1 , 0)

# condition
data$cond<-NA
data$cond<-replace(data$cond, data$FL_101_DO=="Control", 0)
data$cond<-replace(data$cond, data$FL_101_DO=="Treatment1", 1)
data$cond<-replace(data$cond, data$FL_101_DO=="Treatment2", 2)
var_lab(data$cond) <- "Condition"
val_lab(data$cond) <- num_lab("0 cont
                               1 treat1
                               2 treat2")
data$cond<-factor(data$cond)
freq(data$cond)

# interest
data$rint<-1-std01(data$int) 

# knowledge
data$kn1<-ifelse(data$pk1==2, 1, 0)
data$kn2<-ifelse(data$pk2==5, 1, 0)
data$kn3<-ifelse(data$pk3==2, 1, 0)
data$kn4<-ifelse(data$pk4==7, 1, 0)
data$kn5<-ifelse(data$pk5==1, 1, 0)
psych::alpha(with(data, cbind(kn1, kn2, kn3, kn4, kn5)))
data$rkn<-rowMeans(with(data, cbind(kn1, kn2, kn3, kn4, kn5)))

# authoritarianism
data$a2<-data$X2
data$aut1<-ifelse(data$a1==2, 1, ifelse(data$a1==1, 0, NA))
data$aut2<-ifelse(data$a2==1, 1, ifelse(data$a2==2, 0, NA))
data$aut3<-ifelse(data$a3==1, 1, ifelse(data$a3==2, 0, NA))
data$aut4<-ifelse(data$a4==2, 1, ifelse(data$a4==1, 0, NA))
data$aut5<-ifelse(data$a5==2, 1, ifelse(data$a5==1, 0, NA))
data$aut6<-ifelse(data$a6==1, 1, ifelse(data$a6==2, 0, NA))
data$aut7<-ifelse(data$a7==2, 1, ifelse(data$a7==1, 0, NA))
data$aut8<-ifelse(data$a8==1, 1, ifelse(data$a8==2, 0, NA))
psych::alpha(with(data, cbind(aut1, aut2, aut3, aut4, aut5, aut6, aut7, aut8)))
data$raut<-rowMeans(with(data, cbind(aut1, aut2, aut3, aut4, aut5, aut6, aut7, 
                                     aut8)))
var_lab(data$raut) <- "Authoritarianism"

# vote choice (high = Baker)
data$vc<-NA
data$vc<-replace(data$vc, data$choice==2 & data$Q121==1, 0)
data$vc<-replace(data$vc, data$choice==2 & data$Q121==2, 1)
data$vc<-replace(data$vc, data$choice==2 & data$Q121==3, 2)
data$vc<-replace(data$vc, data$choice==1 & data$choice.b==3, 3)
data$vc<-replace(data$vc, data$choice==1 & data$choice.b==2, 4)
data$vc<-replace(data$vc, data$choice==1 & data$choice.b==1, 5)
data$rvc<-data$vc/5

# candidate FTs (0-100; 0-1)
data$ftb<-data$ft_1
data$fts<-data$ft_2
data$rftb<-data$ft_1/100
data$rfts<-data$ft_2/100

# effective as representative?
data$effb<-data$effb_1
data$effs<-data$effs_1
data$reffb<-std01(data$effb)
data$reffs<-std01(data$effs)

# shares your values?
data$valb<-data$valb_1
data$vals<-data$vals_1
data$rvalb<-std01(data$valb)
data$rvals<-std01(data$vals)

# difference scores
data$ftdif<-std01(data$rftb-data$rfts)
data$efdif<-std01(data$reffb-data$reffs)
data$vadif<-std01(data$rvalb-data$rvals)

# outcome composites (alpha = 0.92)
psych::alpha(with(data, cbind(rvc, ftdif, efdif, vadif)))
corr.test(with(data, cbind(rvc, ftdif, efdif, vadif)))
data$rcsup1<-rowMeans(with(data, cbind(rvc, ftdif, efdif, vadif)))

items <- with(data, cbind(rvc, ftdif, efdif, vadif))
fa.parallel(items, fa="pc", n.iter=50) # 1 factor
fa <- fa(items, nfactors=1,  fm="pa", rotate = "oblimin") 
fa
fa$values
fs <- fa$scores
data$rcsup2<-std01(as.numeric(fs))

# checks -- 
# baker = 2; stevenson = 1
data$bc1<-ifelse(data$checkb==2, 1, 0)
data$sc1<-ifelse(data$checks==1, 1, 0)

################################################################################
######### save coded dataframe as .Rdata 
save(data, file="exp1.Rdata")

################################################################################
######### analysis

###### subset to white respondents + party check passers
wdata<-subset(data, white==1 & bc1==1 & sc1==1)

###### subset to white respondents
wdata<-subset(data, white==1)

###### same results in both cases; use checked

################################################################################
###### composite dependent variable (quadratic)

# experimental condition 
# control = party only
# treat1 = social con R -- expect null
# treat2 = social con D 

# fit
m1q <- lm(rcsup2 ~ female+age+rinc+college+jewish+cath+other+raut*cond+
           I(raut^2)*cond, data=wdata)
coeftest(m1q, vcovHC(m1q, type = "HC3"))

# interaction tests
# 2 expected significant; 1 not expected significant
lht(m1q, test="F", c("raut:condtreat1=0", "condtreat1:I(raut^2)=0"), 
    white.adjust = "hc3")
lht(m1q, test="F", c("raut:condtreat2=0", "condtreat2:I(raut^2)=0"), 
    white.adjust = "hc3")

# Bayes factor for treatment 1, for which null is expected:
# H1 = null of no interaction; h1/h2 is correct BF
# BF() does not like polynomial terms; do substitution and re-run
wdata$q<-I(wdata$raut^2)
bt <- lm(rcsup2 ~ female+age+rinc+college+jewish+cath+other+raut*cond+
            q*cond, data=wdata)
# Bayes factor computation
bnm1<-BF(bt$coefficients, Sigma=vcovHC(bt, type = "HC3"), n=nobs(bt), 
         hypothesis="raut:condtreat1=0 & condtreat1:q = 0")
summary(bnm1)

# marginal effects 
me1<-ggeffects::ggpredict(m1q, vcov.type = "HC3",
           terms = c("cond", "raut [1, 0]"))
me1
me2<-ggeffects::hypothesis_test(me1)
me2[c(1, 10, 15),]

# figure 7.1: predicted values
f71 <- plot_model(m1q, vcov.type = "HC3",
                  type = "pred", line.size=.5,
                  terms = c("cond", "raut [0, 1]")) + 
  ggtheme +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0, 1)) +
  scale_x_discrete(limits=c("Control\n(Party Only)", 
                            "Socially\nConservative\nRepublican", 
                            "Socially\nConservative\nDemocrat")) +
  scale_color_manual(labels = c("Non-Authoritarian", "Authoritarian"), 
                     values = c("darkgray", "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +  
  geom_hline(yintercept = 0.5, colour = "black", linetype = 2) +  
  labs(title = "Candidate Support by Condition",
       x = "",
       y = "Support for Republican over Democrat") 
f1q$layers[[2]]$geom_params$width <- 0.05
ggsave(file="f71.png", f71, width = 11, height = 6)

# extra marginal effect plot -- not used in final manuscript
# wrangle marginal effects
me3<-data.frame(rbind(me2[1, 3:5], me2[10, 3:5], me2[15, 3:5]))
me3$term<-c("cond", "treat1", "treat2")
names(me3)[names(me3) == "Contrast"] <- "estimate"
# plot
fs71 <- me3 %>% ggplot(aes(y=term, 
                          x = estimate, 
                          xmin=conf.low, 
                          xmax=conf.high)) +
  geom_point(size = 2.5, alpha = 0.85,  
             position = position_dodge(width = 0.5)) +
  geom_errorbar(width = 0.05, alpha = 0.85, 
                position = position_dodge(width = 0.5)) +
  ggtheme +
  scale_x_continuous(breaks=seq(-0.4,0.6,0.2), limits=c(-0.4, 0.6)) +
  scale_y_discrete(labels=c("Control\n(Party Only)", 
                   "Socially\nConservative\nRepublican", 
                   "Socially\nConservative\nDemocrat")) +
  theme(plot.title = element_text(hjust = 0.5)) +  
  labs(title = "Candidate Support by Condition: Marginal Effects",
       x = "Marginal Effect of Authoritarianism",
       y = "") +
  geom_vline(xintercept = 0, colour = "black", linetype = 2) 
ggsave(file="fs71.png", fs71, width = 11, height = 6)

# table of results
# re-estimate with lm_robust() to get objects with correct SEs
m1 <- lm_robust(rcsup2~female+age+rinc+college+jewish+cath+other+raut*cond, 
                data=wdata, se_type = "HC3")

# print table
t1<-huxreg(m1q,
           statistics = c("N" = "nobs", 
                          "R squared" = "r.squared"),
           number_format = 2)
quick_docx(t1, file='st1.docx')

