rm(list = ls())
###  Data Description ##

load("~/cumulativeANES.rda")
source("functions/common_functions.r")

data$group <- ifelse(data$white == 1, 1, 0)
data$group <- ifelse(data$black == 1, 2, data$group)
data$group <- ifelse(data$hispanic == 1, 3, data$group)



data <- subset(data, group == 1)
#### Drop mids
data <- subset(data, year != 1990) ## Drop 1990
data <- subset(data, year != 1994) ## Drop 1990
data$mode <- as.character(data$mode)
data <- subset(data, year == 2020 | (mode == "FTF" | mode == "FTC/CASI")) ## Drop 1990
data$ideology <- zero.one(data$ideology)
psych::alpha(cbind(data$rr1, data$rr2, data$rr3, data$rr4)) # .86
data$racial.resentment <- zero.one(rowMeans(cbind(data$rr1, data$rr2, data$rr3, data$rr4), na.rm = T))


##### Create a latent authoritarianism score #####
data$authoritarianism <- zero.one(rowMeans(cbind(data$auth.1, data$auth.2, data$auth.3, data$auth.4), na.rm = T))


data$split.ticket <- recode(as.character(data$split.house), "'DP-DC'=0; 'DP-RC'=1; 'RP-DC'=1; 'RP-RC'=0; else=NA")

### Overall Efficacy

psych::alpha(cbind(data$efficacy1, data$efficacy2, data$efficacy3, data$efficacy5, data$efficacy6, data$efficacy7))
cor.test(data$efficacy5, data$efficacy6)

data$efficacy <- rowMeans(cbind(data$efficacy1, data$efficacy2, data$efficacy3, data$efficacy5, data$efficacy6, data$efficacy7), na.rm = T)
data$knowledge <- ((rowMeans(cbind(data$know.interview.pre, data$know.interview.post), na.rm = T) - 1) / 4)
data$age <- (data$age - 17) / 80

data$republican <- recode(data$pid * 6 + 1, "1:2=0; 3:5=0; 6:7=1")
data$democrat <- recode(data$pid * 6 + 1, "1:2=1; 3:5=0; 6:7=0")
data$independent <- recode(data$pid * 6 + 1, "1:2=0; 3:5=1; 6:7=0")
data$party3 <- recode(data$pid * 6 + 1, "1:2='Democrat'; 3:5='Independent'; 6:7='Republican'; else=NA")




data$participation <- rowSums(cbind(data$p1, data$p2, data$p3, data$p4, data$p5), na.rm = T)
data$AuthXRep <- data$authoritarianism * data$republican
data$AuthXInd <- data$authoritarianism * data$independent
psych::alpha(with(data, cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data[data$year == 1992, ], cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data[data$year == 2000, ], cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data[data$year == 2004, ], cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data[data$year == 2008, ], cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data[data$year == 2012, ], cbind(egal1, egal2, egal3, egal4)))
psych::alpha(with(data[data$year == 2016, ], cbind(egal1, egal2, egal3, egal4)))

psych::alpha(with(data, cbind(moral1, moral2, moral3, moral4)))

data$egalitarianism <- zero.one(rowMeans(with(data, cbind(egal1, egal2, egal3, egal4)),
  na.rm = T
))
data$moral.traditionalism <- zero.one(rowMeans(with(data, cbind(moral1, moral2, moral3, moral4)),
  na.rm = T
))
data$services <- zero.one(data$gov.services)

### Construct Emotion Measures ###


##### Effects, Figures 1 -3, Vote, Candidate Affect, Partisan Affect

# Anti-Egalitarianism

# subset to white electorate
data <- subset(data, white == 1)

save(data, file = paste0(data_location, "/clean/cumulativeANES_White.rda"))
write.csv(data, file = paste0(data_location, "/clean/cumulativeANES_White.csv"))
