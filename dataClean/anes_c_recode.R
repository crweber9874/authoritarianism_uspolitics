#####  Description ####
####    Recoding the ANES #####
# This script takes the cumulative data and recodes it into a format that is
# easier to work with. It also creates the variables that are used in the analysis.
# Saves to "clean"

#### The Cumulative Data through 2016 ####
rm(list = ls())
require(readstata13)
library(foreign)
library(dplyr)
source("functions/common_functions.r")

# Location of data

data_location <- ""
setwd("")

anes <- read.dta13(paste0(data_location, "/anes_timeseries_cdf_stata12.dta"),
  convert.factors = FALSE
)

CD <- as.numeric(car::recode(anes$VCF0900, "0=NA; 99=NA")) ## 00 no district, 99 DC
STATE <- as.numeric(car::recode(anes$VCF0901a, "00=NA; 99=NA")) ## 00 Missing, 99 NA
year <- anes$VCF0004
rid <- anes$VCF0006


## Demographics and poliics
ideology <- car::recode(anes$VCF0803, "0=NA; 9=NA") ## Conservative
pid <- (car::recode(anes$VCF0301, "0=NA; 9=NA") - 1) / 6 ## Republican
protestant <- car::recode(anes$VCF0128, "1=1;  2=0; 3=0; 4=0; else=NA")
catholic <- car::recode(anes$VCF0128, "1=0;  2=1; 3=0; 4=0; else=NA")
jewish <- car::recode(anes$VCF0128, "1=0;  2=0; 3=1; 4=0; else=NA")
other <- car::recode(anes$VCF0128, "1=0;  2=0; 3=0; 4=1; else=NA")
## Church attendance
church <- car::recode(anes$VCF0130, "1=1; 2:7=0; else=NA") # Attend church at least once per week

bible <- car::recode(anes$VCF0850, "1=3; 2=2; 3=1; else=NA") ### Biblical literalism
income <- rep(NA, length(bible))
income <- car::recode(anes$VCF0114, "0=NA; 4:5=1; 1:3=0; else=NA") # Greater than 68 percentile
primary <- car::recode(anes$VCF9026, "1:5=1; 7=0; 9=NA")
#### Religion importance
religious.importance <- car::recode(anes$VCF0846, "1=1; 2=0; else=NA")

### Demographics
female <- car::recode(anes$VCF0104, "1=0; 2=1; 0=NA")
white <- car::recode(anes$VCF0105a, "2:7=0; 1=1; else=NA")
nonwhite <- car::recode(anes$VCF0105a, "2:7=1; 1=0; else=NA")
black <- car::recode(anes$VCF0105a, "1=0; 3:7=0; 2=1; else=NA")
asian <- car::recode(anes$VCF0105a, "1:2=0; 4:7=0; 3=1; else=NA")
hispanic <- car::recode(anes$VCF0105a, "1:4=0; 6:7=0; 5=1; else=NA")
other.race <- car::recode(anes$VCF0105a, "1=0; 2=0; 3:4=1; 5=0; 6:7=1; else=NA")
age <- car::recode(anes$VCF0101, "0=NA")
college <- car::recode(anes$VCF0110, "1:3=0; 4=1; else=NA")
northeast <- car::recode(anes$VCF0112, "1=1; 2=0; 3=0; 4=0; else=NA")
north.central <- car::recode(anes$VCF0112, "1=0; 2=1; 3=0; 4=0; else=NA")
south <- car::recode(anes$VCF0112, "1=0; 2=0; 3=1; 4=0; else=NA")
west <- car::recode(anes$VCF0112, "1=0; 2=0; 3=0; 4=1; else=NA")


## Evaluations of groups
ideology.dems <- car::recode(anes$VCF0503, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") # ideology democratic party
ideology.reps <- car::recode(anes$VCF0504, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") # ideology Republican party
knowledge.ideology <- rep(NA, length(ideology.dems))
knowledge.ideology[ideology.reps > ideology.dems] <- 1
knowledge.ideology[ideology.reps <= ideology.dems] <- 0

## Insurance plan
insurance.dems <- car::recode(anes$VCF0508, "0=NA; 8=NA") # Private insurance plan, Dems
insurance.reps <- car::recode(anes$VCF0509, "0=NA; 8=NA") # Private insurance plan, Reps
knowledge.insurance <- rep(NA, length(insurance.dems))
knowledge.insurance[insurance.reps > insurance.dems] <- 1
knowledge.insurance[insurance.reps <= insurance.dems] <- 0

## Guaranteed jobs and standard of living
government.dems <- car::recode(anes$VCF0513, "0=NA; 8=NA; 9=NA") # Government living, Dems
government.reps <- car::recode(anes$VCF0514, "0=NA; 8=NA; 9=NA") # Government living, Reps
knowledge.government <- rep(NA, length(government.dems))
knowledge.government[government.reps > government.dems] <- 1
knowledge.government[government.reps <= government.dems] <- 0

## Feeling items
feeling.dem <- car::recode(anes$VCF0218, "98:99=NA") # Feeling Dems
feeling.rep <- car::recode(anes$VCF0224, "98:99=NA") # Feeling Reps
feeling.demc <- car::recode(anes$VCF0424, "98:99=NA") # Feeling Dems
feeling.repc <- car::recode(anes$VCF0426, "98:99=NA") # Feeling Reps

feel.black <- ifelse(anes$VCF0206 >= 97, NA, anes$VCF0206)
feel.latino <- ifelse(anes$VCF0217 >= 97, NA, anes$VCF0217)
feel.asian <- ifelse(anes$VCF0227 >= 97, NA, anes$VCF0227)

aid.blacks <- car::recode(anes$VCF0830, "0=NA; 8=NA; 9=NA")

affirmatve.action <- car::recode(anes$VCF0867a, "1=1; 2=2; 4=3;  5=4; 7=NA; 8=NA; 9=NA")

## Government services, coded in conservative direction
gov.dems <- car::recode(anes$VCF0541, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 0=NA; 8=NA; 9=NA") # Services, Dems
gov.reps <- car::recode(anes$VCF0542, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1;0=NA; 8=NA; 9=NA") # Services, Reps
knowledge.gov <- rep(NA, length(gov.dems))
knowledge.gov[gov.reps > gov.dems] <- 1
knowledge.gov[gov.reps <= gov.dems] <- 0

## Defense spending
defense.dems <- car::recode(anes$VCF0549, "0=NA; 8=NA; 9=NA") # Defense, Dems
defense.reps <- car::recode(anes$VCF0550, "0=NA; 8=NA; 9=NA") # Defnse, Reps
knowledge.defense <- rep(NA, length(defense.dems))
knowledge.defense[defense.reps > defense.dems] <- 1
knowledge.defense[defense.reps <= defense.dems] <- 0

know.interview.pre <- car::recode(anes$VCF0050a, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
know.interview.post <- car::recode(anes$VCF0050b, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")

interest.elections <- car::recode(anes$VCF0310, "1=1; 2=2; 3=2; else=NA") # Interest in campaign
interest.politics <- car::recode(anes$VCF0311, "1=0; 2=1;  else=NA") # Interest in party winner

## Efficacy###
voted <- car::recode(anes$VCF0702, "2=1; 1=0; else=NA") # Did Respondent Vote in the National Elections
vote.house <- car::recode(anes$VCF0707, "1=0; 2=1; else=NA") # Vote for Congress, Republican
vote.senate <- car::recode(anes$VCF0708, "1=0; 2=1; else=NA") # Republican
split.house <- car::recode(anes$VCF0709, "1='DP-DC'; 2='DP-RC'; 3='RP-DC'; 4='RP-RC'; else=NA") # Republican
split.senate <- car::recode(anes$VCF0710, "1='DP-DS'; 2='DP-RS'; 3='RP-DS'; 4='RP-RS'; else=NA") # Republican
efficacy1 <- car::recode(anes$VCF0624, "1=1;2=2; 3=3; else=NA") # How Much Elections Make Government Pay Attention to People
efficacy2 <- car::recode(anes$VCF0609, "1=1; 2=3; 3=2; else=NA") # People care about what I think
efficacy3 <- car::recode(anes$VCF0606, "1=1;2=2; 3=3; else=NA") # Government does not waste money
efficacy4 <- car::recode(anes$VCF0605, "1=1;2=2; else=NA") # Government run for benefit of all
efficacy5 <- car::recode(anes$VCF0604, "1=1;2=2; 3=3; 4=4; else=NA") # Trust government to do what is right
efficacy6 <- car::recode(anes$VCF0613, "1=1;2=3; 3=2; else=NA") # People like me have a say
efficacy7 <- car::recode(anes$VCF0614, "1=1;2=3; 3=2; else=NA") # government is not too complicated

# Engagement
p1 <- car::recode(anes$VCF0718, "1=0; 2=1; else=NA") # Attend rally
p2 <- car::recode(anes$VCF0719, "1=0; 2=1; else=NA") # Work for candidate
p3 <- car::recode(anes$VCF0720, "1=0; 2=1; else=NA") # Button
p4 <- car::recode(anes$VCF0721, "1=0; 2=1; else=NA") # Donate money
p5 <- car::recode(anes$VCF0717, "1=0; 2=1; else=NA") # INfluence others vote

# Policy Items
protect.gays <- car::recode(anes$VCF0876a, "1=0; 5=1; else=NA") # Do you favor or oppose laws to protect homosexuals against job discrimination?
gays.military <- car::recode(anes$VCF0877, "1=0; 5=1; else=NA") # Do you favor or oppose laws to protect homosexuals against job discrimination?
gays.adoption <- car::recode(anes$VCF0878, "1=0; 5=1; else=NA") # Gay Adoption
## Immigration
immigrants <- car::recode(anes$VCF0879, "8=NA; 9=NA") # Decrease immigration

### Racial Resentment
rr1 <- car::recode(anes$VCF9040, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks work hard (IRish)
rr2 <- car::recode(anes$VCF9039, "1=1; 2=2; 3=3; 4=4; 5=5; 8=NA; 9=NA; else=NA") ### Generations of slavery
rr3 <- car::recode(anes$VCF9042, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Blacks gotten less than they deserve
rr4 <- car::recode(anes$VCF0508, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks try harder


public.insurance <- car::recode(as.numeric(anes$VCF0806), "0=NA; 8:9=NA") # Public insurance
public.insuranceCD <- car::recode(as.numeric(anes$VCF9085), "0=NA; 8:9=NA") # Private insurance
public.insuranceCR <- car::recode(as.numeric(anes$VCF9093), "0=NA; 8:9=NA") # public insurance
women.role <- car::recode(as.numeric(anes$VCF0834), "0=NA; 8:9=NA") # women's role in home
women.roleCD <- car::recode(as.numeric(anes$VCF9083), "0=NA; 8:9=NA") # women's role in home
women.roleCR <- car::recode(as.numeric(anes$VCF9091), "0=NA; 8:9=NA") # women's role in home
# Party not candidate
aid.blacks <- car::recode(as.numeric(anes$VCF0830), "0=NA; 9=NA") # government should not help blacks
aid.blacksD <- car::recode(as.numeric(anes$VCF0517), "0=NA; 8:9=NA") #

aid.blacksR <- car::recode(as.numeric(anes$VCF0518), "0=NA; 8:9=NA")

aid.blacksCD <- car::recode(as.numeric(anes$VCF9084), "0=NA; 8:9=NA") #

aid.blacksCR <- car::recode(as.numeric(anes$VCF9092), "0=NA; 8:9=NA")
gov.services <- car::recode(as.numeric(anes$VCF0839), "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1;  else=NA") # Fewer services
gov.servicesCD <- car::recode(as.numeric(anes$VCF9086), "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA")
gov.servicesCR <- car::recode(as.numeric(anes$VCF9094), "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA")
gov.servicesD <- car::recode(as.numeric(anes$VCF0541), "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA")
gov.servicesR <- car::recode(as.numeric(anes$VCF0542), "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA")
# Jobs and standard of living
jobs <- car::recode(as.numeric(anes$VCF0809), "0=NA; 9=NA") # People should get ahead on own
jobsCD <- car::recode(as.numeric(anes$VCF9087), "0=NA; 8:9=NA")
jobsCR <- car::recode(as.numeric(anes$VCF9095), "0=NA; 8:9=NA")
jobsD <- car::recode(as.numeric(anes$VCF0513), "0=NA; 8:9=NA")
jobsR <- car::recode(as.numeric(anes$VCF0514), "0=NA; 8:9=NA")
defense.spending <- car::recode(as.numeric(anes$VCF0843), "0=NA; 9=NA") # government should provide more defense spending
defense.spendingCD <- car::recode(as.numeric(anes$VCF9081), "0=NA; 8:9=NA")
defense.spendingCR <- car::recode(as.numeric(anes$VCF9089), "0=NA; 8:9=NA")
defense.spendingD <- car::recode(as.numeric(anes$VCF0549), "0=NA; 8:9=NA")
defense.spendingR <- car::recode(as.numeric(anes$VCF0550), "0=NA; 8:9=NA")

strong.gov <- car::recode(as.numeric(anes$VCF0829), "1=0; 2=1; else=NA")

ideologyD <- car::recode(as.numeric(anes$VCF0503), "0=NA; 8:9=NA")
ideologyR <- car::recode(as.numeric(anes$VCF0504), "0=NA; 8:9=NA")
ideologyCD <- car::recode(as.numeric(anes$VCF9088), "0=NA; 8:9=NA")
ideologyCR <- car::recode(as.numeric(anes$VCF9096), "0=NA; 8:9=NA")


gays.adoption <- car::recode(as.numeric(anes$VCF0878), "1=0; 5=1; else=NA") # Ban gay adoptionng
immigrants <- car::recode(as.numeric(anes$VCF0879), "8=NA; 9=NA") # immigrants
ideology_respondent <- car::recode(anes$VCF0803, "0=NA; 9=NA")
gays.discrimination <- car::recode(as.numeric(anes$VCF0876), "1=0; 5=1; else=NA") #  Jobs
abortion <- car::recode(as.numeric(anes$VCF0838), "1=4; 2=3; 3=2; 4=1; 0=NA; 9=NA") # Ban abortion
# Abortion and women role lacking
public.insuranceD <- car::recode(as.numeric(anes$VCF0508), "0=NA; 8:9=NA")
public.insuranceR <- car::recode(as.numeric(anes$VCF0509), "0=NA; 8:9=NA")
vote <- car::recode(anes$VCF0704, "1=0; 2=1; else=NA")
### Moral Traditionalism ###
moral1 <- car::recode(anes$VCF0852, "1=1; 2=2; 3=3; 4=4; 5=5 ;else=NA") # Should Adjust View of Moral Behavior to Changes
moral2 <- car::recode(anes$VCF0853, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") # Should be More Emphasis on Traditional Values
moral3 <- car::recode(anes$VCF0854, "1=1; 2=2; 3=3; 4=4; 5=5 ;else=NA") # Tolerance of Different Moral Standards
moral4 <- car::recode(anes$VCF0851, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") # Newer lifestyles contributing to societal breakeown
### Egalitarianism ###
egal1 <- car::recode(anes$VCF9013, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ## Unequal opprotunity
egal2 <- car::recode(anes$VCF9017, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ## Worry less about eqwuality
egal3 <- car::recode(anes$VCF9016, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ## Not a big problem if everyone doesn't have chance
egal4 <- car::recode(anes$VCF9018, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ## Fewer propnblems if fair treat
mode <- car::recode(anes$VCF0017, "0='FTF'; 1='Tpre'; 2='Tpost'; 3='Tboth'; 4='Internet'")


### Feeling Thermometer
gay.therm <- car::recode(anes$VCF0232, "98=NA; 99=NA")
feminists.therm <- car::recode(anes$VCF0253, "98=NA; 99=NA")
fundamental.therm <- car::recode(anes$VCF0234, "98=NA; 99=NA")
union.therm <- car::recode(anes$VCF0210, "98=NA; 99=NA")

weights.all <- anes$VCF0009z
weights.ftf <- anes$VCF0009x
weights.web <- anes$VCF0009y


data.cumulative.anes <- data.frame(
  female, college,
  church, catholic,
  jewish, other, bible, year,
  rid, CD, STATE, pid,
  feeling.dem, feeling.rep, feeling.demc, feeling.repc, abortion, vote,
  rr1, rr2, rr3, rr4, white, nonwhite, black, hispanic, other.race,
  age, primary, income,
  split.house, split.senate, vote.house,
  vote.senate,
  moral1, moral2, moral3, moral4, voted,
  interest.elections, interest.politics,
  know.interview.pre, know.interview.post,
  p1, p2, p3, p4, p5, efficacy1, efficacy2, efficacy3,
  efficacy4, efficacy5, efficacy6, efficacy7,
  public.insurance, public.insuranceCD, public.insuranceCR, women.role, women.roleCD, women.roleCR,
  aid.blacksD, aid.blacksR, aid.blacksCD, aid.blacksCR,
  gov.services, gov.servicesCD, gov.servicesCR,
  gov.servicesR, gov.servicesD, jobs, jobsCR, jobsCD,
  jobsR, jobsD, defense.spendingCD, defense.spendingCR,
  defense.spendingD, defense.spendingR, ideology, ideologyR,
  ideologyD, ideologyCD, ideologyCR, egal1, egal2, egal3, egal4,
  mode, gay.therm, feminists.therm, fundamental.therm,
  protect.gays, gays.military, gays.adoption, immigrants, union.therm,
  weights.all, weights.ftf, weights.web,
  feel.black, feel.latino, feel.asian, aid.blacks
)

#### Authoritarianism Segments
#### The Cross sections
anes.1990 <- read.dta(paste0(data_location, "/NES1990.dta"), convert.factors = FALSE)
anes.1992 <- read.dta(paste0(data_location, "/NES1992.dta"), convert.factors = FALSE)
anes.1994 <- read.dta(paste0(data_location, "/NES1994.dta"), convert.factors = FALSE)
anes.2000 <- read.dta(paste0(data_location, "/anes2000TS.dta"), convert.factors = FALSE)
anes.2004 <- read.dta(paste0(data_location, "/anes2004TS.dta"), convert.factors = FALSE)
anes.2008 <- read.dta(paste0(data_location, "/anes_timeseries_2008_stata12.dta"), convert.factors = FALSE)
anes.2012 <- read.dta(paste0(data_location, "/anes_timeseries_2012_Stata12.dta"), convert.factors = FALSE)
### 1990##
id.1990 <- anes.1990$V900004 # Identifier
year.1990 <- rep(1990, times = (length(id.1990)))
auth1.1990 <- car::recode(anes.1990$V900330, "1=1; 2=2; 3=1; else=NA") # Obey authority

### 1992###
id.1992 <- anes.1992$V923004 # Identifier
year.1992 <- rep(1992, times = (length(id.1992)))
auth1.1992 <- car::recode(anes.1992$V926020, "1=2; 3=1; 5=1; else=NA") # Obey authority
auth2.1992 <- car::recode(anes.1992$V926019, "1=1; 3=1; 5=2; else=NA") # Respect for Elders
auth3.1992 <- car::recode(anes.1992$V926021, "1=1; 3=1; 5=2; else=NA") # Good Manners
auth4.1992 <- car::recode(anes.1992$V926022, "1=1; 3=1; 5=2; else=NA") # Well behaved

### 1994###
id.1994 <- anes.1994$V940001 # Identifier
year.1994 <- rep(1994, times = (length(id.1994)))
auth1.1994 <- car::recode(anes.1994$V926020, "1=2; 3=1; 5=1; else=NA") # Obey Authority
auth2.1994 <- car::recode(anes.1994$V926019, "1=1;3=1; 5=2; else=NA") # Respect for elders
auth3.1994 <- car::recode(anes.1994$V926021, "1=1;3=1; 5=2; else=NA") # Good manners
auth4.1994 <- car::recode(anes.1994$V926022, "1=1;3=1; 5=2; else=NA") # Well behaved
### 2000###
id.2000 <- anes.2000$V000001 # Identifier
year.2000 <- rep(2000, times = (length(id.2000)))
auth1.2000 <- car::recode(anes.2000$V001587, "1=2;3=1; 5=1; else=NA") # Obey Authority
auth2.2000 <- car::recode(anes.2000$V001586, "1=1;3=1; 5=2; else=NA") # Respect for elders
auth3.2000 <- car::recode(anes.2000$V001588, "1=1;3=1; 5=2; else=NA") # Good manners
auth4.2000 <- car::recode(anes.2000$V001589, "1=1;3=1; 5=2; else=NA") # Well behaved
### 2004###
id.2004 <- anes.2004$V040001 # Identifier
year.2004 <- rep(2004, times = (length(id.2004)))
auth1.2004 <- car::recode(anes.2004$V045210, "1=2;3=1; 5=1; else=NA") # Obey Authority
auth2.2004 <- car::recode(anes.2004$V045208, "1=1;3=1; 5=2; else=NA") # Respect for elders
auth3.2004 <- car::recode(anes.2004$V045209, "1=1;3=1; 5=2; else=NA") # Good manners
auth4.2004 <- car::recode(anes.2004$V045211, "1=1;3=1; 5=2; else=NA") # Well behaved


### 2008###
id.2008 <- anes.2008$V080001 # Identifier
year.2008 <- rep(2008, times = (length(id.2008)))
auth1.2008 <- car::recode(anes.2008$V085160, "1=2;3=1; 5=1; else=NA") # Obey Authority
auth2.2008 <- car::recode(anes.2008$V085158, "1=1;3=1; 5=2; else=NA") # Respect for elders
auth3.2008 <- car::recode(anes.2008$V085159, "1=1;3=1; 5=2; else=NA") # Good manners
auth4.2008 <- car::recode(anes.2008$V085161, "1=1;3=1; 5=2; else=NA") # Well behaved
### 212###
id.2012 <- anes.2012$caseid # Identifier
year.2012 <- rep(2012, times = (length(id.2012)))
auth1.2012 <- car::recode(anes.2012$auth_obed, "1=2; 2=1; 3=1; else=NA") # Obey Authority
auth2.2012 <- car::recode(anes.2012$auth_ind, "1=1;  2=2; 3=1; else=NA") # Respect for elders
auth3.2012 <- car::recode(anes.2012$auth_cur, "1=1;   2=2; 3=1; else=NA") # Good manners
auth4.2012 <- car::recode(anes.2012$auth_consid, "1=1; 2=2; 3=1; else=NA") # Well behaved

authoritarianism.data <- data.frame(cbind(
  c(id.1990, id.1992, id.1994, id.2000, id.2004, id.2008, id.2012),
  c(year.1990, year.1992, year.1994, year.2000, year.2004, year.2008, year.2012),
  c(auth1.1990, auth1.1992, auth1.1994, auth1.2000, auth1.2004, auth1.2008, auth1.2012),
  c(rep(NA, times = (length(id.1990))), auth2.1992, auth2.1994, auth2.2000, auth2.2004, auth2.2008, auth2.2012),
  c(rep(NA, times = (length(id.1990))), auth3.1992, auth3.1994, auth3.2000, auth3.2004, auth3.2008, auth3.2012),
  c(rep(NA, times = (length(id.1990))), auth4.1992, auth4.1994, auth4.2000, auth4.2004, auth4.2008, auth4.2012)
))
names(authoritarianism.data) <- c("rid", "year", "auth.1", "auth.2", "auth.3", "auth.4")
## Merge the cross sectional data with the authoritarianism data
auth.data <- merge(data.cumulative.anes, authoritarianism.data, by = c("rid", "year")) ### Merge the authoritarianism data

auth.data$term <- NA
auth.data$term[auth.data$year == 1990] <- 101
auth.data$term[auth.data$year == 1992] <- 102
auth.data$term[auth.data$year == 1994] <- 103
auth.data$term[auth.data$year == 1996] <- 104
auth.data$term[auth.data$year == 1998] <- 105
auth.data$term[auth.data$year == 2000] <- 106
auth.data$term[auth.data$year == 2002] <- 107
auth.data$term[auth.data$year == 2004] <- 108
auth.data$term[auth.data$year == 2006] <- 109
auth.data$term[auth.data$year == 2008] <- 110
auth.data$term[auth.data$year == 2010] <- 111
auth.data$term[auth.data$year == 2012] <- 112

###  Data Description ##
### This is the 2016 data, which I attach to the main data file ###
#### The 2016 data
# require(foreign)
data <- read.dta(paste0(data_location, "/anes_timeseries_2016_Stata12.dta"), convert.factors = FALSE)

rid <- data$V160001

weights.all <- data$V160102
weights.ftf <- data$V160102f
weights.web <- data$V160102w
## Survey Mode
mode <- car::recode(data$V160501, "1='FTC/CASI'; 2='Internet'; else=NA")
## Religion
protestanta <- car::recode(data$V161247a, "1=1; 2:4=0; -1=0; else=NA")
catholica <- car::recode(data$V161247a, "2=1; 1=0; 3:4=0; -1=0; else=NA")
jewisha <- car::recode(data$V161247a, "3=1; 1:2=0; 4=0; -1=0; else=NA")
othera <- car::recode(data$V161247a, "4=1; 1:3=0; -1=0; else=NA")

protestantb <- car::recode(data$V161247b, "1=1; 2:4=0; -1=0; else=NA")
catholicb <- car::recode(data$V161247b, "2=1; 1=0; 3:4=0; -1=0; else=NA")
jewishb <- car::recode(data$V161247b, "3=1; 1:2=0; 4=0; -1=0; else=NA")
otherb <- car::recode(data$V161247b, "4=1; 1:3=0; -1=0; else=NA")

protestant <- ifelse(protestanta == 1 | protestantb == 1, 1, 0)
catholic <- ifelse(catholica == 1 | catholicb == 1, 1, 0)
jewish <- ifelse(jewisha == 1 | jewishb == 1, 1, 0)
other <- ifelse(othera == 1 | otherb == 1, 1, 0)

### Attend religious service
church <- car::recode(data$V161245, "-1=0; 5=0; 4=0; 3=0; 2=0; 1=1; -9=0") ## Attend services at least once per week
### religious importance
religious.importance <- car::recode(data$V161242, "-1=0; 1=1; 2=2; 3=3; else=NA") ## Religious importance. Don't include this
### Trump Character Traits
### trump traits.
trump.strong <- car::recode(data$V161164, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
clinton.strong <- car::recode(data$V161159, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
trump.care <- car::recode(data$V161165, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
clinton.care <- car::recode(data$V161160, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
trump.knowledge <- car::recode(data$V161166, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
clinton.knowledge <- car::recode(data$V161161, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
trump.honest <- car::recode(data$V161167, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
clinton.honest <- car::recode(data$V161162, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
trump.speaks <- car::recode(data$V161168, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
clinton.speaks <- car::recode(data$V161163, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
clinton.even <- car::recode(data$V161169, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") # Even tempered
trump.even <- car::recode(data$V161170, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
psych::alpha(cbind(trump.strong, trump.care, trump.knowledge, trump.honest, trump.speaks, trump.even))
psych::alpha(cbind(clinton.strong, clinton.care, clinton.knowledge, clinton.honest, clinton.speaks, clinton.even))
trump.character <- (rowMeans(cbind(trump.strong, trump.care, trump.knowledge, trump.honest, trump.speaks, trump.even), na.rm = T) - 1) / 4
clinton.character <- (rowMeans(cbind(clinton.strong, clinton.care, clinton.knowledge, clinton.honest, clinton.speaks, clinton.even), na.rm = T) - 1) / 4
#### Income###
income <- car::recode(data$V161361x, "16:28=1; 1:15=0; else=NA") ## 68 percentile and higher, 55k, source dydqj.com calculator for 2015 income
#### ideology and pidi
pid <- (car::recode(data$V161158x, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") - 1) / 6 ### PID 01

voted <- car::recode(data$V162031x, "1=1; 0=0; else=NA") # Did Respondent Vote in ## Vote direction in house and senate -- need to manually construct the split ballot measure
vote.house <- car::recode(data$V162068x, "1=0; 2=1; else=NA") # Rep vote
vote.senate <- car::recode(data$V162067x, "1=0; 2=1; else=NA")
vote <- car::recode(data$V162034a, "2=1; 1=0; else=NA") ### Two party vote, DT=1

split.house <- rep(NA, length(vote.senate))
split.house[vote.house == 1 & vote == 1] <- "RP-RC"
split.house[vote.house == 0 & vote == 1] <- "RP-DC"
split.house[vote.house == 0 & vote == 0] <- "DP-DC"
split.house[vote.house == 1 & vote == 0] <- "DP-RC"

split.senate <- rep(NA, length(vote.senate))
split.senate[vote.senate == 1 & vote == 1] <- "RP-RC"
split.senate[vote.senate == 0 & vote == 1] <- "RP-DC"
split.senate[vote.senate == 0 & vote == 0] <- "DP-DC"
split.senate[vote.senate == 1 & vote == 0] <- "DP-RC"

p1 <- car::recode(data$V162011, "1=1; 2=0; else=NA") # Attend rally
p2 <- car::recode(data$V162013, "1=1; 2=0; else=NA") # Work for candidate
p3 <- car::recode(data$V162012, "1=1; 2=0; else=NA") # Button
p4a <- car::recode(data$V162014, "1=1; 2=0; else=NA") # Donate money
p4b <- car::recode(data$V162016, "1=1; 2=0; else=NA") # Donate money
p4 <- ifelse(p4a == 1 | p4b == 1, 1, 0)
p5 <- car::recode(data$V162010, "1=1; 2=0; else=NA") # Button

### Code Efficacy 1 -7
efficacy1 <- car::recode(data$V161220, "1=3; 2=2; 3=1; else=NA") # How Much Elections Make Government Pay Attention to People

efficacy2 <- car::recode(data$V162215, "1=1; 2=1; 3=2; 4=3; 5=3; else=NA") # People care about what I think

efficacy3 <- car::recode(data$V161217, "1=1;2=2; 3=3; else=NA") # Government does not waste money

efficacy4 <- car::recode(data$V161216, "1=1;2=2; else=NA") # Government run for benefit of all

efficacy5 <- car::recode(data$V161215, "1=4; 2=4; 3=3; 4=2; 5=1; else=NA") # Trust government to do what is right

efficacy6 <- car::recode(data$V162216, "1=1; 2=2; 3=2; 4=3; 5=3; else=NA") # People like me have a say

efficacy7 <- car::recode(data$V162217, "1=1; 2=1; 3=2; 4=3; 5=3; else=NA") # government is not too complicated

# FTF only
know.interview.pre <- car::recode(data$V168016, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
know.interview.post <- car::recode(data$V168112, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")


interest.elections <- car::recode(data$V161004, "1=3; 2=2; 3=1; else=NA") # Interest in campaign
interest.politics <- car::recode(data$V161145, "1=1; 2:5=0; -8=0;  else=NA") # Interest in party winner


## Salient Issues
# issue.morality<-recode(anes$VCF0875, "7=1; 1:6=0; 8:97=0; else=NA") # Vote Republican
## These aren't the branched graded questions
protect.gays <- car::recode(data$V161229, "1=0; 2=1; else=NA") # Do you favor or oppose laws to protect homosexuals against job discrimination?
gays.military <- NA
gays.adoption <- car::recode(data$V161230, "1=0; 2=1; else=NA") # Gay Adoption

## Immigration
immigrants <- car::recode(data$V161192, "1=4; 2=3; 3=2; 4=1; else=NA") # Decrease immigration

### moral traditionalism.
moral1 <- car::recode(data$V162207, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ## We should adjust, Reversed
moral2 <- car::recode(data$V162210, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") # Should be More Emphasis on Traditional Values
moral3 <- car::recode(data$V162209, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") # Tolerance of Different Moral Standards
moral4 <- car::recode(anes$V162208, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") # Newer lifestyles contributing to societal breakeown

psych::alpha(cbind(moral1, moral2, moral3))
moral.traditionalism <- (rowMeans(cbind(moral1, moral2, moral3), na.rm = T) - 1) / 4



### Racial Resentment
rr1 <- car::recode(data$V162211, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks work hard (Irish)
rr2 <- car::recode(data$V162212, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Generations of slavery
rr3 <- car::recode(data$V162213, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Blacks gotten less
rr4 <- car::recode(data$V162214, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks try harder
### Interest
interest <- (car::recode(data$V161003, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") - 1) / 4 ### Interest in politics
### vote Trump.
vote <- car::recode(data$V162034a, "2=1; 1=0; else=NA") ### Two party vote
vote.primary <- car::recode(data$V161021a, "1='AHC'; 2='BS'; 4='DT'; 5='TC';
                      6='JK'; 7='MR'; 3='Other'; 8='Other'; 9='Other'; else=NA") ### Primary Vote

### voted Obama
vote.obama <- car::recode(data$V161006, "1=1; 2=0; else=NA") ### Two party vote
## Authoritarianism -- Numbering matches the code above
auth.1 <- car::recode(data$V162241, "1=2; 2=1; 3=1; else=NA") ### Respect
auth.2 <- car::recode(data$V162239, "1=1; 2=2; 3=1; else=NA") ### Child repsect versus indepdnence
auth.3 <- car::recode(data$V162240, "1=1; 2=2; 3=1; else=NA") ### Manners
auth.4 <- car::recode(data$V162242, "1=1; 2=2; 3=1; else=NA") ### Behaved
psych::alpha(cbind(auth.1, auth.2, auth.3, auth.4))
authoritarianism <- (rowMeans(cbind(auth.1, auth.2, auth.3, auth.4), na.rm = T) - 1) / 2
### hostile sexism.
hostile1 <- car::recode(data$V161507, "1=5; 2=4 ; 3=3 ;4=2; 5=1;  else=NA") ### Inerpret everything as sexist
hostile2 <- car::recode(data$V161508, "1=5; 2=4 ; 3=3 ;4=2; 5=1;  else=NA") ### Consider what men do.
hostile3 <- car::recode(data$V161509, "1=5; 2=4 ; 3=3 ;4=2; 5=1;  else=NA") ### Gain power through feminism
hostile4 <- car::recode(data$V161510, "1=5; 2=4 ; 3=3 ;4=2; 5=1;  else=NA") ### Put on tight leash
psych::alpha(cbind(hostile1, hostile2, hostile3, hostile4))
hostile <- (rowMeans(cbind(hostile1, hostile2, hostile3, hostile4), na.rm = T) - 1) / 4


### Demographics
### I can't locate census region
age <- car::recode(data$V161267, "-9=NA; -8=NA")
college <- car::recode(data$V161270, "13:16=1; 1:12=0; else=NA") ## College degree
media <- car::recode(data$V162002, "1=0; 2:4=1; else=NA") ## Watch tv about campaign, one or two, good many, 3 or 4.
white <- car::recode(data$V161310x, "1=1; 2:6=0; else=NA")
nonwhite <- car::recode(data$V161310x, "1=0; 2:6=1; else=NA")
black <- car::recode(data$V161310x, "2=1; 1=0; 3:6=0; else=NA")
hispanic <- car::recode(data$V161310x, "5=1; 1:4=0; 6=0; else=NA")
other.race <- car::recode(data$V161310x, "3:4=1; 1:2=0; 5=0; 6=1; else=NA")

female <- car::recode(data$V161342, "1=0; 2=1; else=NA")
#### Feeling Therms ####
feeling.demc <- car::recode(data$V161086, "-98=NA; -99=NA; -88=NA") # Feeling Dems Candidate
feeling.repc <- car::recode(data$V161087, "-98=NA; -99=NA; -88=NA") # Feeling Reps Candidate
feeling.dem <- car::recode(data$V161095, "-98=NA; -99=NA; -88=NA; -89=NA") # Feeling Dems
feeling.rep <- car::recode(data$V161096, "-98=NA; -99=NA; -88=NA; -89=NA") # Feeling Reps

feel.black <- ifelse(data$V162312 < 0, NA, data$V162312)
feel.latino <- ifelse(data$V162311 < 0, NA, data$V162311)
feel.asian <- ifelse(data$V162310 < 0, NA, data$V162310)

#####
primary <- car::recode(data$V161021, "1=1; 2=0; else=NA")


#### Policy Items####
women.role <- NA
women.roleCD <- NA
women.roleCR <- NA
aid.blacksD <- NA
aid.blacksR <- NA
gov.servicesR <- NA
gov.servicesD <- NA
jobsR <- NA
jobsD <- NA
defense.spendingD <- NA
defense.spendingR <- NA

jobs <- car::recode(data$V161189, "-9=NA; -8=NA; 99=NA") # Government should see to jobs and standard of living, conservative direction
jobsCD <- car::recode(data$V161190, "-9=NA; -8=NA; 99=NA") # Government should see to jobs and standard of living, conservative direction
jobsCR <- car::recode(data$V161191, "-9=NA; -8=NA; 99=NA") # Government should see to jobs and standard of living, conservative direction
public.insurance <- car::recode(data$V161184, "-9=NA; -8=NA; 99=NA") # Private insurance plan
public.insuranceCD <- car::recode(data$V161185, "-9=NA; -8=NA; 99=NA") # Private insurance plan
public.insuranceCR <- car::recode(data$V161186, "-9=NA; -8=NA; 99=NA") # Private insurance plan
aid.blacks <- car::recode(data$V161198, "-9=NA; -8=NA; 99=NA") # government should not help blacks
aid.blacksCD <- car::recode(data$V161199, "-9=NA; -8=NA; 99=NA") # government should not help blacks
aid.blacksCR <- car::recode(data$V161200, "-9=NA; -8=NA; 99=NA") # government should not help blacks
gov.services <- car::recode(data$V161178, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else =NA") # government should provide fewer services
gov.servicesCD <- car::recode(data$V161179, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 0=NA; 99=NA; -9=NA; -8=NA") # government should provide fewer services
gov.servicesCR <- car::recode(data$V161180, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; 0=NA; 99=NA; -9=NA; -8=NA") # government should provide fewer services
defense.spending <- car::recode(data$V161181, "0=NA; 99=NA; -9=NA; -8=NA") # government should provide more defense spending
defense.spendingCD <- car::recode(data$V161182, "0=NA; 99=NA; -9=NA; -8=NA") # government should provide more defense spending
defense.spendingCR <- car::recode(data$V161183, "0=NA; 99=NA; -9=NA; -8=NA") # government should provide more defense spending
ideology <- car::recode(data$V161126, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyCD <- car::recode(data$V161128, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyCR <- car::recode(data$V161129, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyD <- car::recode(data$V161130, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyR <- car::recode(data$V161131, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")

# women.role<-recode(as.numeric(anes$VCF0834), "0=NA; 9=NA") #women's role in home
abortion <- car::recode(data$V161232, "1=4; 2=3; 3=2; 4=1; -9=NA; -8=NA; 5=NA") # Ban abortion
gays.discrimination <- car::recode(as.numeric(data$V161229), "1=0; 2=1; else=NA") # Gays Jobs




### Better or worse off than year ago#####
personal.situation <- car::recode(data$V161110, "-9=NA; -8=NA") # Am worse off than year ago
personal.fsituation <- car::recode(data$V161111, "-9=NA; -8=NA") # Am worse off than year ago
large.gap <- car::recode(data$V161138x, "-1=NA; 1=5; 2=4; 3=3; 4=2; 5=1") # Larger income gap toeay
large.unemployment <- car::recode(data$V161138x, "-1=NA") # Larger unemployment


child <- car::recode(data$V161324, "-9=NA") # Number of Children
child.1plus <- car::recode(data$V161324, "0=0; -9=NA; 1:9=1") # at least one




### Non cumulative policy items#####
aca <- car::recode(data$V161114x, "-1=NA") # Oppose ACA
# immigrants<-recode(data$V161192, "-8=NA; -9=NA; 1=4; 2=3; 3=2; 4=1") #Make immigrants felons and send home
birthright <- car::recode(data$V161194x, "-8=NA; -9=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") # End Birthright citizenship
dreamers <- car::recode(data$V161194x, "-8=NA; -9=NA; 1=6; 2=5; 3=4; 4=3; 5=2; 6=1") # Send Children back
wall <- car::recode(data$V161194x, "-8=NA; -9=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") # Favor a wall

affirmatve.action <- car::recode(data$V161204x, "-8=NA; -9=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") # favor affirmative action

### Spending Budget Battery ####
social.security <- car::recode(data$V161205, "-8=NA; -9=NA; 2=3; 3=2") # Reduce spending
public.schools <- car::recode(data$V161206, "-8=NA; -9=NA; 2=3; 3=2") # Reduce spending
science <- car::recode(data$V161207, "-8=NA; -9=NA; 2=3; 3=2") # Reduce spending
crime <- car::recode(data$V161208, "-8=NA; -9=NA; 2=3; 3=2") # Reduce spending
welfare <- car::recode(data$V161209, "-8=NA; -9=NA; 2=3; 3=2") # Welfare
child.care <- car::recode(data$V161210, "-8=NA; -9=NA; 2=3; 3=2") # Reduce spending
aid.poor <- car::recode(data$V161211, "-8=NA; -9=NA; 2=3; 3=2") # Reduce spending
environment <- car::recode(data$V161212, "-8=NA; -9=NA; 2=3; 3=2") # Reduce spending
isis <- car::recode(data$V161213x, "-8=NA; -9=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") # Favor troops to fight isis
syrian.refugees <- car::recode(data$V161213x, "-8=NA; -9=NA") # Oppose taking in Syrian refugees



global1 <- car::recode(data$V161221, "-8=NA; -9=NA") # Global warming hasn't been happening
global2 <- car::recode(data$V161222, "-8=NA; -9=NA;2=3; 3=2") # Anthropengenic climate change
fracking <- car::recode(data$V161222, "-8=NA; -9=NA;1=3; 2=1; 3=2") # Support Fracking
global3 <- car::recode(data$V161225x, "-8=NA; -9=NA") # Do less about climate change

paid.leave <- car::recode(data$V161226x, "-8=NA; -9=NA") # Oppose paid leave

gay.services <- car::recode(data$V161227x, "-8=NA; -9=NA; 1=6; 2=5; 3=4; 4=3; 5=2; 6=1") # Oppose services to same sex couples
transgender.bathroom <- car::recode(data$V161228x, "-8=NA; -9=NA; 1=6; 2=5; 3=4; 4=3; 5=2; 6=1") # Bathroom of biological sex
gays.discrimination <- car::recode(data$V161229x, "-8=NA; -9=NA; -1=NA") # Oppose protection of homosexual couples

gay.marriage <- car::recode(data$V161231, "-8=NA; -9=NA") # Oppose gay marriage
abortion <- car::recode(data$V161232, "-8=NA; -9=NA; 1=4; 2=3; 3=2; 4=1; 5=NA") # Oppose abortion
death.penalty <- car::recode(data$V161233x, "-8=NA; -9=NA;-1=NA; 1=4; 2=3; 3=2; 4=1; 5=NA") # Favor capital punishment

torture <- car::recode(data$V162295x, "-8=NA; -9=NA; -6=NA; -4=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") # Favor torture
### Patriotism and Nationalism ###
patriotism <- car::recode(data$V162125x, "-8=NA; -9=NA; -7=NA; -6=NA;-7=NA;  1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1") # Flag makes feel gooed


### anti-egalitarianism.
egal1 <- car::recode(data$V162243, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ## Unequal opprotunity
egal2 <- car::recode(data$V162244, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ## Worry less about eqwuality
egal3 <- car::recode(data$V162245, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ## Tradition
egal4 <- car::recode(data$V162246, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ## Fewer propnblems if fair treat
psych::alpha(cbind(egal1, egal2, egal3, egal4))
anti.egalitarianism <- (rowMeans(cbind(egal1, egal2, egal3, egal4), na.rm = T) - 1) / 4
### knwoeldge
k1 <- car::recode(data$V161513, "6=1; else=0") ## Senate term
k2 <- car::recode(data$V161515, "2=1; else=0") ## Party with most members in house
k3 <- car::recode(data$V161516, "2=1; else=0") ## Party with most members in senate
k4 <- car::recode(data$V162072, "1=1; 0=0; else=NA") ## VP
k5 <- car::recode(data$V162073a, "1=1; 0=0; else=NA") ## Speaker of house
k6 <- car::recode(data$V162074a, "1=1; 0=0; else=NA") ## Angela merkel
k7 <- car::recode(data$V162075a, "1=1; 0=0; else=NA") ## Putin
k8 <- car::recode(data$V162076a, "1=1; 0=0; else=NA") ## CHief justice
psych::alpha(data.frame(k1, k2, k3, k4, k5, k6, k7, k8))
# knowledge<-rowMeans(cbind(k1, k2, k3, k4, k5, k6, k7, k8), na.rm=T)

### Feeling Thermometer
gay.therm <- car::recode(data$V162103, "-9=NA; -7=NA; -6=NA; 998=NA; 999=NA")
feminists.therm <- car::recode(data$V162096, "-9=NA; -7=NA; -6=NA; 998=NA; 999=NA")
fundamental.therm <- car::recode(data$V162095, "-9=NA; -7=NA; -6=NA; 998=NA; 999=NA")
union.therm <- car::recode(data$V162098, "-9=NA; -7=NA; -6=NA; 998=NA; 999=NA")
####


### White identity
white.identity <- (car::recode(data$V162327, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") - 1) / 4 ## White identity
black.identity <- (car::recode(data$V162328, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") - 1) / 4 ## Black identity
data$identity <- NA
data$identity[white == 1 & !is.na(white.identity)] <- white.identity[white == 1 & !is.na(white.identity)]
data$identity[black == 1 & !is.na(black.identity)] <- black.identity[black == 1 & !is.na(black.identity)]
identity <- data$identity


strong.gov <- car::recode(data$V161124, "1=0; 2=1; else=NA")

bible <- car::recode(data$V161243, "1=1; 2=2; 3=3; else=NA")

#

#### traits

data$dem.leader <- car::recode(data$V161159, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$dem.know <- car::recode(data$V161161, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$dem.cares <- car::recode(data$V161160, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$dem.decent <- car::recode(data$V161162, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.leader <- car::recode(data$V161164, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.know <- car::recode(data$V161166, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.cares <- car::recode(data$V161165, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.decent <- car::recode(data$V161167, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA") ## honest





#### Data save and merge

data <- data.frame(
  age, female, college, church, catholic, jewish,
  bible,
  pid, feeling.dem, feeling.rep, feeling.demc, feeling.repc,
  abortion,
  vote, auth.1, auth.2, auth.3, auth.4,
  rr1, rr2, rr3, rr4, white, nonwhite, black, hispanic,
  other.race, primary, income,
  split.house, split.senate, vote.house,
  vote.senate,
  moral1, moral2, moral3, moral4, voted,
  interest.elections, interest.politics,
  know.interview.pre, know.interview.post,
  p1, p2, p3, p4, p5, efficacy1, efficacy2, efficacy3,
  efficacy4, efficacy5, efficacy6, efficacy7,
  rid, other,
  public.insurance, public.insuranceCD, public.insuranceCR,
  women.role, women.roleCD, women.roleCR,
  aid.blacksD, aid.blacksR, aid.blacksCD, aid.blacksCR,
  gov.services, gov.servicesCD, gov.servicesCR,
  gov.servicesR, gov.servicesD, jobs, jobsCR, jobsCD,
  jobsR, jobsD, defense.spendingCD, defense.spendingCR,
  defense.spendingD, defense.spendingR, ideology, ideologyR,
  ideologyD, ideologyCD, ideologyCR, egal1, egal2, egal3, egal4,
  mode, gay.therm, feminists.therm, fundamental.therm,
  protect.gays, gays.military, gays.adoption, immigrants, union.therm,
  weights.all, weights.ftf, weights.web,
  feel.black, feel.latino, feel.asian, aid.blacks
)
data$year <- 2016
data_2016 <- data



library(car)

###  Data Description ##
## FTF is infeasible in 2016. I just use all the data. Recall we truncated the rest to be face-to-face
data <- read.dta13(paste0(data_location, "/t2020anes.dta"), convert.factors = TRUE)
data$case_id <- data$V160001_orig
protestant <- recode(data$V201435, "1=1; 2:12=0; else=NA")
catholic <- recode(data$V201435, "2=1; 1=0; 3:12=0; else=NA")
jewish <- recode(data$V201435, "5=1; 1:4=0; 6:12=0; else=NA")
other <- recode(data$V201435, "11:12=1; 1:10=0; else=NA")
church <- recode(data$V201453, "1=1; 2:5=0; else=NA") # Attend church at least once per week

bible <- recode(data$V201434, "1=3; 2=2; 3=1; else=NA")
income <- rep(NA, length(bible))
income <- recode(data$V202468x, "11:22=1; 1:10=0; else=NA")
pid <- (recode(data$V201231x, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") - 1) / 6
primary <- recode(data$V201020, "1=1; 2=0; else=NA")
religious.importance <- recode(data$V201433, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")
female <- recode(data$V201600, "1=0; 2=1; else=NA") #
white <- ifelse(data$V201549x == 1, 1, ifelse(data$V201549x != 1 & data$V201549x > 0, 0, NA))
nonwhite <- abs(white - 1)
black <- ifelse(data$V201549x == 2, 1, ifelse(data$V201549x != 2 & data$V201549x > 0, 0, NA))
hispanic <- ifelse(data$V201549x == 3, 1, ifelse(data$V201549x != 3 & data$V201549x > 0, 0, NA))
asian <- ifelse(data$V201549x == 4, 1, ifelse(data$V201549x != 4 & data$V201549x > 0, 0, NA))
other.race <- ifelse(black == 0 & white == 0 & hispanic == 0 & asian == 0, 1, 0)
age <- data$V201507x
college <- recode(data$V201511x, "4:5 =1; 1:3 = 0; else= NA")
media <- recode(data$V201629a, "1=0; 2=1; else=NA") ## Watch tv about campaign, one or two, good many, 3 or 4.
rid <- seq(1:length(media))
year <- 2020
feeling.demc <- recode(data$V201151, "-9=NA; 998=NA; -999=NA; -4=NA") # Feeling Dems Candidate
feeling.repc <- recode(data$V201152, "-9=NA; 998=NA; -999=NA; -4=NA") # Feeling Rep Candidate
feeling.dem <- recode(data$V201156, "-9=NA; 998=NA; -999=NA; -4=NA") # Feeling Dems
feeling.rep <- recode(data$V201157, "-9=NA; 998=NA; -999=NA;-4=NA") # Feeling Reps



abortion <- recode(data$V201336, "1=4; 2=3; 3=2; 4=1; -9=NA; -8=NA; 5=NA") # Ban abortion
gays.discrimination <- recode(as.numeric(data$V201414x), "1:2=0; 3:4=1; else=NA") # Gays Jobs
voted <- recode(data$V202073, "2=1; 1=0; else=NA") # Did Respondent vote for president
### Racial Resentment
rr1 <- recode(data$V202300, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks work hard (IRish)
rr2 <- recode(data$V202301, "1=1; 2=2; 3=3; 4=4; 5=5; 8=NA; 9=NA; else=NA") ### Generations of slavery
rr3 <- recode(data$V202302, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Blacks gotten less than they deserve
rr4 <- recode(data$V202303, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks try harder

vote <- recode(data$V202105x, "11=1; 10=0;else=NA")
vote.house <- recode(data$V202106x, "11=1; 10=0;else=NA")
vote.senate <- recode(data$V202107x, "11=1; 10=0;else=NA")
split.house <- ifelse(vote != vote.house, 1, 0)
split.senate <- ifelse(vote != vote.senate, 1, 0)
efficacy1 <- recode(data$V201005, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") # How Much Elections Make Government Pay Attention to People
efficacy2 <- recode(data$V202212, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") # People care about what I think
efficacy3 <- recode(data$V201235, "1=1;2=2; 3=3; else=NA") # Government does not waste money
efficacy4 <- recode(data$V201234, "1=1;2=2; else=NA") # Government run for benefit of all
efficacy5 <- recode(data$V201233, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") # Trust government to do what is right
efficacy6 <- recode(data$V202213, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") # People like me have a say
efficacy7 <- recode(data$V202214, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") # government is not too complicated
p1 <- recode(data$V202014, "1=1; 2=0; else=NA") # Attend rally
p2 <- recode(data$V202016, "1=1; 2=0; else=NA") # Work for candidate
p3 <- recode(data$V202015, "1=1; 2=0; else=NA") # Button
p4 <- recode(data$V202017, "1=1; 2=0; else=NA") # Donate money
p5 <- recode(data$V202022, "1=1; 2=0; else=NA") # INfluence others vote



moral1 <- recode(data$V202264, "1=1; 2=2; 3=3; 4=4; 5=5 ;else=NA") # Should Adjust View of Moral Behavior to Changes
moral2 <- recode(data$V202265, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") # Should be More Emphasis on Traditional Values
moral3 <- NA
moral4 <- NA
# moral3<- rep(NA, length(moral1)) ## Can't locate these questions
# moral4<- rep(NA, length(moral1))
interest.elections <- recode(data$V201006, "1=3; 2=2; 3=1; else=NA") # Interest in campaign
interest.politics <- recode(data$V202407, "1=4; 2=3; 3=2; 4=1;  else=NA") # Interest in party winner
## Can't locate these:
know.interview.pre <- rep(NA, length(moral1))
know.interview.post <- rep(NA, length(moral1))
jobs <- recode(data$V201255, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") # Government should see to jobs and standard of living, conservative direction
jobsCD <- recode(data$V201256, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") # Government should see to jobs and standard of living, conservative direction
jobsCR <- recode(data$V201257, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") # Government should see to jobs and standard of living, conservative direction
public.insurance <- recode(data$V201252, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") # Private insurance plan
public.insuranceCD <- recode(data$V201253, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") # public insurance plan
public.insuranceCR <- recode(data$V201254, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") # public insurance plan
aid.blacks <- recode(data$V201258, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
aid.blacksCD <- recode(data$V201259, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #
aid.blacksCR <- recode(data$V201260, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #

gov.services <- recode(data$V201246, "1=7; 2=6; 3=4; 4=4; 5=3; 6=2; 7=1; else=NA") #

gov.servicesCD <- recode(data$V201247, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #
gov.servicesCR <- recode(data$V201248, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #
defense.spending <- recode(data$V201249, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #
defense.spendingCD <- recode(data$V201250, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #
defense.spendingCR <- recode(data$V201251, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA") #
ideology <- recode(data$V201200, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyCD <- recode(data$V201202, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyCR <- recode(data$V201203, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyD <- recode(data$V201206, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
ideologyR <- recode(data$V201207, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; else=NA")
women.role <- rep(NA, length(moral1))
women.roleCD <- rep(NA, length(moral1))
women.roleCR <- rep(NA, length(moral1))
## Anti-Egalitarianism
egal1 <- recode(data$V202260, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ## Unequal opprotunity
egal2 <- recode(data$V202261, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ## Worry less about eqwuality
egal3 <- recode(data$V202262, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ##
egal4 <- recode(data$V202263, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ## Fewer propnblems if fair treat
mode <- data$V200002
gay.therm <- recode(data$V202166, "-9:1=NA; 998=NA; 999=NA")
feminists.therm <- recode(data$V202160, "-9:1=NA; 998=NA; 999=NA")
union.therm <- recode(data$V202162, "-9:1=NA; 998=NA; 999=NA")
fundamental.therm <- recode(data$V202159, "-9:1=NA; 998=NA; 999=NA")
immigrants <- recode(data$V202232, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") # Decrease immigration
protect.gays <- recode(data$V201414x, "1:2=0; 3:4=1; else=NA") # Do you favor or oppose laws to protect homosexuals against job discrimination?
gays.military <- recode(data$V202388, "1=0; 3=0; 2=1; else=NA") # Trans military
gays.adoption <- recode(data$V201415, "1=0; 2=1; else=NA") # Gay Adoption
weights.all <- rep(NA, length(mode))
weights.ftf <- rep(NA, length(mode))
weights.web <- rep(NA, length(mode))

## Authoritarianism -- Numbering matches the code above
auth.2 <- recode(data$V202266, "1=1; 2=2; 3=1; else=NA") ### Child repsect versus indepdnence
auth.3 <- recode(data$V202267, "1=1; 2=2; 3=1; else=NA") ### Manners
auth.1 <- recode(data$V202268, "1=2; 2=1; 3=1; else=NA") ### Respect
auth.4 <- recode(data$V202269, "1=1; 2=2; 3=1; else=NA") ### Behaved


aid.blacksD <- rep(NA, length(mode))
aid.blacksR <- rep(NA, length(mode))
gov.servicesD <- rep(NA, length(mode))
gov.servicesR <- rep(NA, length(mode))
jobsD <- rep(NA, length(mode))
jobsR <- rep(NA, length(mode))
defense.spendingD <- rep(NA, length(mode))
defense.spendingR <- rep(NA, length(mode))
sample_type <- data$V200003
## year indicator ##

feel.black <- ifelse(data$V202480 < 0, NA, data$V202480)
feel.latino <- ifelse(data$V202479 < 0, NA, data$V202479)
feel.asian <- ifelse(data$V202478 < 0, NA, data$V202478)

aid.blacks <- recode(data$V201260, "0=NA; -8=NA; -9=NA") # Government living, Dems


data$dem.leader <- recode(data$V201208, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$dem.know <- recode(data$V201210, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$dem.cares <- recode(data$V201209, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$dem.decent <- recode(data$V201211, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")


data$rep.leader <- recode(data$V201212, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.know <- recode(data$V201214, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.cares <- recode(data$V201213, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA")
data$rep.decent <- recode(data$V201215, "1=4; 2=3; 3=2; 4=1; 5=1; else=NA") ## honest



data <- data.frame(
  age, female, college, church, catholic, jewish, bible,
  pid, feeling.dem, feeling.rep, feeling.demc, feeling.repc,
  abortion,
  vote, auth.1, auth.2, auth.3, auth.4,
  rr1, rr2, rr3, rr4, white, nonwhite, black, hispanic, other.race, primary, income,
  split.house, split.senate, vote.house,
  vote.senate,
  moral1, moral2, moral3, moral4, voted,
  interest.elections, interest.politics,
  know.interview.pre, know.interview.post,
  p1, p2, p3, p4, p5, efficacy1, efficacy2, efficacy3,
  efficacy4, efficacy5, efficacy6, efficacy7,
  rid, other,
  public.insurance, public.insuranceCD, public.insuranceCR,
  women.role, women.roleCD, women.roleCR, aid.blacks,
  aid.blacksD, aid.blacksR, aid.blacksCD, aid.blacksCR,
  gov.services, gov.servicesCD, gov.servicesCR,
  gov.servicesR, gov.servicesD, jobs, jobsCR, jobsCD,
  jobsR, jobsD, defense.spendingCD, defense.spendingCR,
  defense.spendingD, defense.spendingR, ideology, ideologyR,
  ideologyD, ideologyCD, ideologyCR, egal1, egal2, egal3, egal4,
  mode, gay.therm, feminists.therm, fundamental.therm,
  protect.gays, gays.military, gays.adoption, immigrants, union.therm,
  weights.all, weights.ftf, weights.web,
  sample_type,
  feel.black, feel.latino, feel.asian
)
data <- subset(data, sample_type != 2)
data$year <- 2020
data_2020 <- data

auth.data <- auth.data[, !(names(auth.data) %in% c("CD", "STATE", "term"))]



### Alphabetical sort and merge!
names(auth.data)
d1 <- auth.data[, order(names(auth.data))]
d2 <- data_2016[, order(names(data_2016))]
d3 <- data_2020[, order(names(data_2020))]
d3 <- d3[, !(names(d3) %in% "sample_type")]
data <- rbind(d1, d2, d3)
# Data check
for (i in 1:dim(data)[2]) {
  print(i)
  print(table(data[, i]))
}

# Save data
data$rid2 <- seq(1:nrow(data))

##### Measures ######
save(data, file = paste0(data_location, "/clean/cumulativeANES.rda"))
write.csv(data, file = paste0(data_location, "/clean/cumulativeANES.csv"))
