rm(list = ls())

require(readstata13)
library(foreign)
detach("package:dplyr")
library(car)

source("functions/common_functions.r")


### Set working directory


data_location <- ""

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
latino <- ifelse(data$V201549x == 3, 1, ifelse(data$V201549x != 3 & data$V201549x > 0, 0, NA))
asian <- ifelse(data$V201549x == 4, 1, ifelse(data$V201549x != 4 & data$V201549x > 0, 0, NA))
other.race <- ifelse(black == 0 & white == 0 & latino == 0 & asian == 0, 1, 0)
age <- data$V201507x
college <- recode(data$V201511x, "4:5 =1; 1:3 = 0; else= NA")
media <- recode(data$V201629a, "1=0; 2=1; else=NA") ## Watch tv about campaign, one or two, good many, 3 or 4.
rid <- seq(1:length(media))
year <- 2020

race <- ifelse(black == 1, "black", ifelse(latino == 1, "latino", ifelse(white == 1, "white", "other")))

feeling.demc <- recode(data$V201151, "-9=NA; 998=NA; -999=NA; -4=NA") # Feeling Dems Candidate
feeling.repc <- recode(data$V201152, "-9=NA; 998=NA; -999=NA; -4=NA") # Feeling Rep Candidate
feeling.dem <- recode(data$V201156, "-9=NA; 998=NA; -999=NA; -4=NA") # Feeling Dems
feeling.rep <- recode(data$V201157, "-9=NA; 998=NA; -999=NA;-4=NA") # Feeling Reps

abortion <- recode(data$V201336, "1=1; 2=0; 3=0; 4=0; -9=NA; -8=NA; 5=NA") # Ban abortion

######## 1. By law, abortion should never be permitted.
######### 2. The law should permit abortion only in case of rape, incest, or
######### when the woman’s life is in danger.
######### 3. The law should permit abortion for reasons other than rape, incest,
######### or danger to the woman’s life, but only after the need for the
######### abortion has been clearly established.
######### 4. By law, a woman should always be able to obtain an abortion as a
######### matter of personal choice.
gay.discrimination <- recode(as.numeric(data$V201414x), "1=0; 2=0; 3:4=1; else=NA") # Gays Jobs
############ : FAVOR/OPPOSE LAWS PROTECT, GAYS LESBIANS AGAINST JOB DISCRIMINATION
gay.marriage <- recode(as.numeric(data$V201416), "1=0; 2=0; 3=1; -9=NA; -8=NA") # 3 category, civil unions
aid.blacks <- recode(data$V201258, "1=0; 2=0; 3=0; 4=0; 5=0; 6=1; 7=1; else=NA") #
###############
birthright.citizenship <- recode(data$V201418, "1=1; 2=0; 3=0; else=NA") #
dreamers <- recode(data$V201421, "1=1; 2=0; 3=0; else=NA") #
border.wall <- recode(data$V201424, "1=1; 2=0; 3=0; else=NA") #
urban.unrest <- recode(data$V201429, "6:7=1; 1:5=0; else=NA") #
conservative <- recode(data$V201429, "6:7=1; 1:5=0; else=NA") #


### Racial Resentment
rr1 <- recode(data$V202300, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks work hard (IRish)
rr2 <- recode(data$V202301, "1=1; 2=2; 3=3; 4=4; 5=5; 8=NA; 9=NA; else=NA") ### Generations of slavery
rr3 <- recode(data$V202302, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA") ### Blacks gotten less than they deserve
rr4 <- recode(data$V202303, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") ### Blacks try harder
racial.resentment <- rowMeans(cbind(rr1, rr2, rr3, rr4), na.rm = T) # 0.33
racial.resentment <- zero.one(racial.resentment)

moral1 <- recode(data$V202264, "1=1; 2=2; 3=3; 4=4; 5=5 ;else=NA") # Should Adjust View of Moral Behavior to Changes
moral2 <- recode(data$V202265, "1=5; 2=4; 3=3; 4=2; 5=1; else=NA") # Should be More Emphasis on Traditional Values
moral.traditionalism <- zero.one(rowMeans(cbind(moral1, moral2), na.rm = T))

gay.therm <- recode(data$V202166, "-9:1=NA; 998=NA; 999=NA")
gay.therm <- zero.one(gay.therm)
immigrants <- recode(data$V202232, "1:2=0; 3=0; 4:5=1; else=NA") # Decrease immigration

## Anti-Egalitarianism
mode <- data$V200002
trans.military <- recode(data$V202388, "1=0; 3=1; 2=0; else=NA") # Trans military
gays.adoption <- recode(data$V201415, "1=0; 2=1; else=NA") # Gay Adoption

weights.all <- rep(NA, length(mode))
weights.ftf <- rep(NA, length(mode))
weights.web <- rep(NA, length(mode))

## Authoritarianism -- Numbering matches the code above
auth.2 <- car::recode(data$V202266, "1=1; 2=2; 3=1; else=NA") ### Child repsect versus indepdnence
auth.3 <- recode(data$V202267, "1=1; 2=2; 3=1; else=NA") ### Manners
auth.1 <- recode(data$V202268, "1=2; 2=1; 3=1; else=NA") ### Respect
auth.4 <- recode(data$V202269, "1=1; 2=2; 3=1; else=NA") ### Behaved
psych::alpha(cbind(auth.1, auth.2, auth.3, auth.4), na.rm = T) # 0.66
authoritarianism <- zero.one(rowMeans(cbind(auth.1, auth.2, auth.3, auth.4), na.rm = T))

sample_type <- data$V200003


data <- data.frame(
  age, female, college, church, catholic, jewish, bible, income,
  pid, media, feeling.dem, feeling.demc, feeling.repc,
  feeling.rep, abortion, black, latino, white,
  gay.marriage, gay.discrimination, racial.resentment,
  moral.traditionalism, gay.therm, immigrants,
  gays.adoption, trans.military, authoritarianism, aid.blacks,
  sample_type, race, birthright.citizenship, dreamers, border.wall,
  urban.unrest, conservative
)

##### Measures ######

### Only the 2020 data
save(data, file = paste0(data_location, "/clean/ch3ANES2000.rda"))
write.csv(data, file = paste0(data_location, "/clean/ch3ANES2000.csv"))



### load cumulative data
rm(list = ls())
library(dplyr)

load("/clean/cumulativeANES.rda")

full_data <- data

party <- full_data$pid * 6 + 1
full_data$party <- car::recode(party, "1:2= 'Democrat'; 3:5 = 'Independent'; 6:7='Moderate'; else=NA")

full_data$authoritarianism <- with(full_data, rowMeans(cbind(auth.1, auth.2, auth.3, auth.4), na.rm = T))

full_data$authoritarianism <- zero.one(full_data$authoritarianism)

library(dplyr)
full_data <- with(full_data, (data.frame(
  age, female, college, church, catholic, jewish, bible,
  pid, year, black, hispanic, white, authoritarianism, income
))) %>% na.omit()


save(full_data, file = "/cumulativeANESCH3.rda")
