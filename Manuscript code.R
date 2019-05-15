#############################################################
# April 2019
# Frances Stewart
# Code to conduct the analysis in Nowak et al.
# Manuscript prepared for the Journal of Wildlife Management
#############################################################

# based on the analysis and herds used in Sorensen et al. (2008).
# herds include WSA (West side athabasca river), LS (little smoky), CL (cold lake), RE (Red Earth), and CM (Caribou Mountains)


source(workspace_for_Steve.RData) # load in the RData file I was provided
# Source all files in the R folder
aa <- lapply(dir("R", full.names = TRUE), function(x) {print(x); source(x)})


## Reproduce Table 1 ----
# WSA
WSA$all.data$AREA[1] # in ha. Divide by 100 to get km^2 (which is presented in Table 1)
WSA_InitialPop = (WSA$all.data$AREA[1])/100*0.06 # assumes carry capacity is 0.06 females/km^2
WSA_Fire = WSA$all.data$PROP_BURN[(1940-1917+1):length(WSA$all.data$PROP_BURN)] # fire events from 1940s onwards
WSA_Fire_mean = mean(WSA$all.data$PROP_BURN[(1940-1917+1):length(WSA$all.data$PROP_BURN)]) # mean annual porportion burned from 1940 onwards
WSA_Fire_sd = sd(WSA$all.data$PROP_BURN[(1940-1917+1):length(WSA$all.data$PROP_BURN)]) # sd annual porportion burned from 1940 onwards
BetaMomentEst(WSA_Fire)

#LS
LS$all.data$AREA[1] # in ha. Divide by 100 to get km^2 (which is presented in Table 1)
LS_InitialPop = (LS$all.data$AREA[1])/100*0.06 # assumes carry capacity is 0.06 females/km^2
LS_Fire = LS$all.data$PROP_BURN[(1940-1917+1):length(LS$all.data$PROP_BURN)] # fire events from 1940s onwards
LS_Fire_mean = mean(LS$all.data$PROP_BURN[(1940-1917+1):length(LS$all.data$PROP_BURN)]) # mean annual porportion burned from 1940 onwards
LS_Fire_sd = sd(LS$all.data$PROP_BURN[(1940-1917+1):length(LS$all.data$PROP_BURN)]) # sd annual porportion burned from 1940 onwards
BetaMomentEst(LS_Fire)

#CL - Use the area from f.CLAWR - this include all the land, rather than just the federal portion of CL Air Weapons Range.
f.clawr$AREA_HERD[1] # in ha. Divide by 100 to get km^2 (which is presented in Table 1)
CL_InitialPop = (f.clawr$AREA_HERD[1])/100*0.06 # assumes carry capacity is 0.06 females/km^2
CL_Fire = CLAWR$all.data$PROP_BURN[(1940-1917+1):length(CLAWR$all.data$PROP_BURN)] # fire events from 1940s onwards
CL_Fire_mean = mean(CL_Fire)*(CLAWR$all.data$AREA[1]/f.clawr$AREA_HERD[1]) # mean annual porportion burned from 1940 onwards
CL_Fire_sd = sd(CL_Fire)*(CLAWR$all.data$AREA[1]/f.clawr$AREA_HERD[1]) # sd annual porportion burned from 1940 onwards
BetaMomentEst((CL_Fire*(CLAWR$all.data$AREA[1]/f.clawr$AREA_HERD[1])))
# for Cold lake, some data were calculated on the whole range (feder + provincial lands) and some only on provincial lands
# we therefore have to adjust those vlaues calculated on provincial lands by values calulated on all lands
# which is why we incorporate the ratio between the all.data$AREA values and the f.clawr$AREA_HERD values

#RE
RE$all.data$AREA[1] # in ha. Divide by 100 to get km^2 (which is presented in Table 1)
RE_InitialPop = (RE$all.data$AREA[1])/100*0.06 # assumes carry capacity is 0.06 females/km^2
RE_Fire = RE$all.data$PROP_BURN[(1940-1917+1):length(RE$all.data$PROP_BURN)] # fire events from 1940s onwards
RE_Fire_mean = mean(RE$all.data$PROP_BURN[(1940-1917+1):length(RE$all.data$PROP_BURN)]) # mean annual porportion burned from 1940 onwards
RE_Fire_sd = sd(RE$all.data$PROP_BURN[(1940-1917+1):length(RE$all.data$PROP_BURN)]) # sd annual porportion burned from 1940 onwards
BetaMomentEst(RE_Fire)

#CM
CM$all.data$AREA[1] # in ha. Divide by 100 to get km^2 (which is pCMsented in Table 1)
CM_InitialPop = (CM$all.data$AREA[1])/100*0.06 # assumes carry capacity is 0.06 females/km^2
CM_Fire = CM$all.data$PROP_BURN[(1940-1917+1):length(CM$all.data$PROP_BURN)] # fire events from 1940s onwards
CM_Fire_mean = mean(CM$all.data$PROP_BURN[(1940-1917+1):length(CM$all.data$PROP_BURN)]) # mean annual porportion burned from 1940 onwards
CM_Fire_sd = sd(CM$all.data$PROP_BURN[(1940-1917+1):length(CM$all.data$PROP_BURN)]) # sd annual porportion burned from 1940 onwards
BetaMomentEst(CM_Fire)

################################################################################
# Run analysis on WSA, LS, CM, or RE herds ---- 
Herd = WSA
# To run through the below code, substitute the herd name into the above line. Herd names include WSA, LS, CM, and RE
# Code for CLAWR is an excpetion - see code for this herd before, as it is mashed from a few different data files.

# STEP 1: re-calculate the cummulative burn for each year, as this was not originally provided. Run the function from
Herd2 <- Burn_F(Herd$all.data, lagYears = 50, colToUse = "PROP_BURN", startYear = 1940)
# ASSUMPTION - fires do not superimpose across the time lag window

# STEP 2: Recreating Table 1 - Summary data ----
Area = Herd2$AREA[1]/100 # in ha. Divide by 100 to get km^2 (which is presented in Table 1)
Herd_InitialPop = (Area)*0.06 # assumes carry capacity is 0.06 caribou/km^2
Herd_Fire = Herd2$PROP_BURN[(1940-1917+1):length(Herd2$PROP_BURN)-1] # fire events from 1940s onwards
Herd_Fire_mean = mean(Herd2$PROP_BURN[(1940-1917+1):length(Herd2$PROP_BURN)-1]) # mean annual porportion burned from 1940 onwards
Herd_Fire_sd = sd(Herd2$PROP_BURN[(1940-1917+1):length(Herd2$PROP_BURN)-1]) # sd annual porportion burned from 1940 onwards
Herd_Beta<- BetaMomentEst(Herd_Fire)
Herd2$HOOF[Herd2$YEAR == 1980] # 0.117
Herd2$HOOF[Herd2$YEAR == 2007] # 0.851


# STEP 3: specify the population structure, based on the above information
# Set vital rates to average number from recorded from Alberta Caribou committee data: CaribouLambda.csv (2002-2008)
caribou<-read.csv("data/CaribouLambda.csv", header = T)
setwd("outputs")
caribouHerd<-subset(caribou, caribou$herd == "West_Side_Athabasca_River")
# Change for each herd
SadF_Herd<-mean(caribouWSA$Adult_Female_Survival)/100 #0.8564. Adult female survival
Rec_Herd<-mean(caribouWSA$Calf_Recruitment)/100 #0.2024. Juvenile recruitment - TODO should this number be 1/2?
# ASSUMPTION  - these rates are held consistent through time.

# population carrying capacity is 0.06 caribou/km^2
K = Area*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_Herd) # adult females, and juvenile females
# ASSUMPITON: 50: sex ratio of calves at survey. Some publications use 60% male.

# STEP 4: Performing analyses ----
# FIRST FUNCTION: Calcualtes demographics without stochasticity, and only for the duration of time that we have data (69 years here)
# Annual proportion of area burned, all years from 1940s onwards, but not the last year
burn = (Herd2$SUM_CUM[(1940-1917+1):length(Herd2$SUM_CUM)-1]) # this should be cummulative. 
hoof = Herd2$HOOF[(1940-1917 +1):length(Herd2$HOOF)-1]# all years from 1940s onwards, but not the last year
HerdCaribou<-Caribou_F(K, burn, hoof, Pop, adult = SadF_Herd, fecun = Rec_Herd) 

# SECOND FUNCTION: this function brings in the period of time before our data collection (older than 69 years ago), 
# and a projected period of time to 2050 (500 years total)
Area
# enter the annual proportion of area burned.
Regime = Herd2$PROP_BURN[(1940-1917+1):length(Herd2$PROP_BURN)-1] 
# enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year
IND = Herd2$HOOF[(1940-1917 +1):length(Herd2$HOOF)-1] 
HerdScenarios<- ScenarioS_F(Area, Regime, IND, Density = 0.06)

# THIRD FUNCTION: this function adds environmental stochasticity to the simulation by repeating it 300 times
Area 
Regime 
IND 
WSAruns<-MCRUNS_F(Area, Regime, IND, Density = 0.06)

# plot this scenario (regular scenario). Change names to reflect the herd that you have run.
#pdf("WSA_RS.pdf", height = 5, width  = 4)
pLambda(WSAruns, "WSA_RS")
#dev.off()

# Performing experiments ----
# EXPERIMENT 1: RS  - done above
# EXPERIMENT 2: LD (low density) - change carrying capacity from 0.03 to 0.02
K = Area*0.04# carrying capacity is 0.02 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_Herd) # recalculate population structure with new K
HerdCaribou_LD<-Caribou_F(K, burn, hoof, Pop, adult = SadF_Herd, fecun = Rec_Herd) 
IND = IND # inustrial footprint stays the same as the RS scenario
HerdScenarioS_LD<- ScenarioS_F(Area, Regime, IND, Density = 0.04)# Set Simfire  = TRUE, FALSE, TRUE (empirical data from 1940-2007)
Herdruns_LD<-MCRUNS_F(Area, Regime, IND, Density = 0.04)
#pdf("WSA_LD.pdf", height = 5, width  = 4)
pLambda(Herdruns_LD, "WSA_LD") # change name to reflect the herd you've just run
#dev.off()
# EXPERIMENT 3: LB - increase fire burn rate to 0.01 (ScenarioS and MCRUNS)
K = Area*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_Herd) # adult females, and juvenile females
HerdCaribou_LB<-Caribou_F(K, burn, hoof, Pop, adult = SadF_Herd, fecun = Rec_Herd) 
Regime = rnorm(length(Herd_Fire), mean = 0.01, sd = Herd_Fire_sd)
IND = IND
HerdScenarioS_LB<- ScenarioS_F(Area, Regime, IND, Density = 0.06)# Simfire  = TRUE, TRUE, TRUE (no empirical data)
Herdruns_LB<-MCRUNS_F(Area, Regime, IND, Density = 0.06)
#pdf("WSA_LB.pdf", height = 5, width  = 4)
pLambda(Herdruns_LB, "LB") # change name to reflect the herd you've just run
#dev.off()
# EXPERIMENT 4: HB - increase fire burn rate to 0.016
K = Area*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_Herd) 
HerdCaribou_LB<-Caribou_F(K, burn, hoof, Pop, adult = SadF_Herd, fecun = Rec_Herd) 
Regime = rnorm(length(Herd_Fire), mean = 0.016, sd = Herd_Fire_sd)
IND = IND
HerdScenarioS_HB<- ScenarioS_F(Area, Regime, IND, Density = 0.06)# Simfire  = TRUE, TRUE, TRUE (no empirical data)
Herdruns_HB<-MCRUNS_F(Area, Regime, IND, Density = 0.06)
#pdf("WSA_HB.pdf", height = 5, width  = 4)
pLambda(Herdruns_HB, "WSA_HB") # change name to reflect the herd you've just run
#dev.off()
#Experiment 5: NI - no industry. Set IND to zero in regular scenario
K = Area*0.06# carrying capacity is 0.06 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_Herd)
HerdCaribou_NI<-Caribou_F(K, burn, hoof, Pop, adult = SadF_Herd, fecun = Rec_Herd) 
Regime = Herd2$PROP_BURN[(1940-1917+1):length(Herd2$PROP_BURN)-1] # regular fire regime
HerdScenarioS_NI<- ScenarioS_F(Area, Regime, IND = rep(0, 69), Density = 0.06)# Simfire  = TRUE, FALSE, TRUE (empirical data from 1940-2007)
Herdruns_NI<-MCRUNS_F(Area, Regime, IND = rep(0, 69), Density = 0.06)
#pdf("WSA_NI.pdf", height = 5, width  = 4)
pLambda(Herdruns_NI, "WSA_NI")
#dev.off()
#Experiment 6: NIHB - no industry, but a high burn rate (60 year fire return interval)
K = Area*0.06# carrying capacity is 0.06 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_Herd)
HerdCaribou_NIHB<-Caribou_F(K, burn, hoof, Pop, adult = SadF_Herd, fecun = Rec_Herd) 
Regime = rnorm(length(Herd_Fire), mean = 0.016, sd = Herd_Fire_sd) # enhanced fire regime
HerdScenarioS_NIHB<- ScenarioS_F(Area, Regime, IND = rep(0, 69), Density = 0.06)# Simfire  = TRUE, FALSE, TRUE (empirical data from 1940-2007)
Herdruns_NIHB<-MCRUNS_F(Area, Regime, IND = rep(0, 69), Density = 0.06)
#pdf("WSA_NI.pdf", height = 5, width  = 4)
pLambda(Herdruns_NIHB, "WSA_NIHB")
#dev.off()

# calculate extinctions from different experiments (Table 2): ----
# RS
# YCRIT date - date that lambda falls below 1.0
2057 - length(HerdScenarios$Lambda[HerdScenarios$Lambda < 1.0]) #The final simulation year, minus the years where lambda < 1.0
# year of extinction - where there are less than 10 females
2057 - length(HerdScenarios$Nt[HerdScenarios$Nt == "EXTINCT"]) #2042
# population size at 2017
HerdScenarios$Nt[length(HerdScenarios$Nt)-40]
# lambda estimate at 2017
HerdScenarios$Lambda[length(HerdScenarios$Lambda)-40]
# LD
2057 - length(WSAScenarioS_LD$Lambda[WSAScenarioS_LD$Lambda < 1.0]) 
2057 - length(WSAScenarioS_LD$Nt[WSAScenarioS_LD$Nt == "EXTINCT"]) 
WSAScenarioS_LD$Nt[length(WSAScenarioS_LD$Nt)-40] # for the year 2017
WSAScenarioS_LD$Lambda[length(WSAScenarioS_LD$Lambda)-40] # for the year 2017
#LB
2057 - length(WSAScenarioS_LB$Lambda[WSAScenarioS_LB$Lambda < 1.0]) # 1975
2057 - length(WSAScenarioS_LB$Nt[WSAScenarioS_LB$Nt == "EXTINCT"]) # 2001
WSAScenarioS_LB$Nt[length(WSAScenarioS_LB$Nt)-40]
WSAScenarioS_LB$Lambda[length(WSAScenarioS_LB$Lambda)-40]
#HB
2057 - length(WSAScenarioS_HB$Lambda[WSAScenarioS_HB$Lambda < 1.0]) # 1961
2057 - length(WSAScenarioS_HB$Nt[WSAScenarioS_HB$Nt == "EXTINCT"]) # 1996
WSAScenarioS_HB$Nt[length(WSAScenarioS_HB$Nt)-40]
WSAScenarioS_HB$Lambda[length(WSAScenarioS_HB$Lambda)-40]
# NI
2057 - length(WSAScenarioS_NI$Lambda[WSAScenarioS_NI$Lambda < 1.0]) # 1961
2057 - length(WSAScenarioS_NI$Nt[WSAScenarioS_NI$Nt == "EXTINCT"]) # 1996
WSAScenarioS_NI$Nt[length(WSAScenarioS_NI$Nt)-40]
WSAScenarioS_NI$Lambda[length(WSAScenarioS_NI$Lambda)-40]
# NIHB
2057 - length(HerdScenarioS_NIHB$Lambda[WSAScenarioS_NIHB$Lambda < 1.0]) # 1961
2057 - length(WSAScenarioS_NIHB$Nt[WSAScenarioS_NIHB$Nt == "EXTINCT"]) # 1996
WSAScenarioS_NIHB$Nt[length(WSAScenarioS_NIHB$Nt)-40]
WSAScenarioS_NIHB$Lambda[length(WSAScenarioS_NIHB$Lambda)-40]

# Create a file of all WSA graphs ----
pdf("WSA_graphs.pdf", height = 4, width = 5, onefile = TRUE)
Fires(WSA2, "WSA Annual Proportion Area Burned")
pHoof(WSA2, "WSA Cummulative Intustrial Footprint")
pLambda(WSAruns, "WSA_RS")
pLambda(WSAruns_LD, "WSA_LD")
pLambda(WSAruns_LB, "WSA_LF")
pLambda(WSAruns_HB, "WSA_HF")
pLambda(WSAruns_NI, "WSA_NI")
pLambda(WSAruns_NIHB, "WSA_NIHB")
dev.off()
