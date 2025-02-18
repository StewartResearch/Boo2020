#############################################################
# May 2019
# Frances Stewart
# Code to conduct the analysis in Nowak et al.
# Manuscript prepared for the Journal of Wildlife Management
#############################################################

# based on the analysis and herds used in Sorensen et al. (2008).
# herds include WSA (West side athabasca river), LS (little smoky), CL (cold lake), RE (Red Earth), and CM (Caribou Mountains)

setwd('Z:/GitHub/Boo2019')
load("Workspace_for_Steve.RData") # load in the RData file I was provided
# Source all files in the R folder
library(R.utils)
library(dplyr)
#sourceDirectory("Z:/GitHub/Boo2019/R")
aa <- lapply(dir("R", full.names = TRUE), function(x) {print(x); source(x)})

###############################################################################
# Run analysis on WSA, LS, CM, or RE herds ---- 
Herd = CM
# TODO: CHANGE FOR EACH HERD

# To run through the below code, substitute the herd name into the above line. Herd names include WSA, LS, CM, and RE
# Code for CLAWR is an exception - see code for this herd before, as it is mashed from a few different data files.

# for CLAWR ##
#Herd = CLAWR
#Area = f.clawr$AREA_HERD[1]/100 # in ha. Divide by 100 to get km^2 (which is presented in Table 1)

# STEP 1: re-calculate the cummulative burn for each year, as this was not originally provided. Run the function from
Herd2 <- Burn_F(Herd$all.data, lagYears = 50, colToUse = "PROP_BURN", startYear = 1940)
# ASSUMPTION - fires do not superimpose across the time lag window

# STEP 2: Recreating Table 1 - Summary data ----
Area = Herd2$AREA[1]/100 # in ha. Divide by 100 to get km^2 (which is presented in Table 1)


##
Herd_InitialPop = Area*0.06 # assumes carry capacity is 0.06 caribou/km^2
Herd_Fire = Herd2$PROP_BURN[(1940-1917+1):length(Herd2$PROP_BURN)-1] # fire events from 1940s onwards
Herd_Fire_mean = mean(Herd2$PROP_BURN[(1940-1917+1):length(Herd2$PROP_BURN)-1]) # mean annual porportion burned from 1940 onwards
Herd_Fire_sd = sd(Herd2$PROP_BURN[(1940-1917+1):length(Herd2$PROP_BURN)-1]) # sd annual porportion burned from 1940 onwards
Herd_Beta<- BetaMomentEst(Herd_Fire)
Herd2$YEAR[Herd2$HOOF > 0] # Provide the years when footprint is greater than 0
Herd2$HOOF[Herd2$YEAR == 1940] # 1940 IND footprint
Herd2$HOOF[Herd2$YEAR == 1980] # 1980 IND footprint
Herd2$HOOF[Herd2$YEAR == 2006] # 2006 IND footprint

# STEP 3: specify the population structure, based on the above information
# Set vital rates to average number from recorded from Alberta Caribou committee data: CaribouLambda.csv (2002-2008)
setwd("Z:/GitHub/Boo2019")
caribou<-read.csv("data/CaribouLambda.csv", header = T)
setwd("outputs")
caribouHerd<-na.omit(subset(caribou, caribou$herd == "Caribou_Mountains")) 
# TODO: CHANGE FOR EACH HERD
SadF_Herd<-mean(caribouHerd$Adult_Female_Survival)/100 # Adult female survival
Rec_Herd<-mean(caribouHerd$Calf_Recruitment)/100 # Juvenile recruitment - TODO should this number be 1/2?
# ASSUMPTION  - these rates are held consistent through time.

# population carrying capacity is 0.06 caribou/km^2
K = Area*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_Herd) # adult females, and juvenile females
# ASSUMPITON: 50:50 sex ratio of calves at survey. Some publications use 60% male.

# STEP 4: Performing analyses ----
# FIRST FUNCTION: Calcualtes demographics without stochasticity, and only for the duration of time that we have data (69 years here)
# Annual proportion of area burned, all years from 1940s onwards, but not the last year
burn = (Herd2$SUM_CUM[(1940-1917+1):length(Herd2$SUM_CUM)-1]) # this should be cummulative. 
hoof = Herd2$HOOF[(1940-1917 +1):length(Herd2$HOOF)-1]# all years from 1940s onwards, but not the last year
HerdCaribou<-Caribou_F(K, burn, hoof, Pop, adult = SadF_Herd, fecun = Rec_Herd) 

# SECOND FUNCTION: this function brings in the period of time before our data collection (older than 69 years ago), 
# and a projected period of time to 2050 (500 years total). Unless changed, it simulates fire in the first sectin of years,
# uses the historic disturbance data in the second section of years, and then simulates fires again in the third section of years
Area
# enter the annual proportion of area burned, for the specified years of data (1940 onwards, but not the last year)
Regime = Herd2$PROP_BURN[(1940-1917+1):length(Herd2$PROP_BURN)-1] 
# enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year
IND = Herd2$HOOF[(1940-1917 +1):length(Herd2$HOOF)-1] 
HerdScenarios<- ScenarioS_F(Area, Regime, IND, Density = 0.06)

# THIRD FUNCTION: this function adds environmental stochasticity to the simulation by repeating it 300 times
Area 
Regime 
IND 
Herdruns<-MCRUNS_F(Area, Regime, IND, Density = 0.06)

# plot this scenario (regular scenario). Change names to reflect the herd that you have run.
#pdf("WSA_Reg.pdf", height = 5, width  = 4)
#pLambda(Herdruns, "Reg")
#dev.off()
#saveRDS(Herdruns, "CL_RS.rds") - example of how to save these objects.

# Performing experiments ----
# EXPERIMENT 1: RS (Reg)  - done above

# EXPERIMENT 2: LD (-Dens) - change carrying capacity from 0.03 to 0.02
K = Area*0.04# carrying capacity is 0.02 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_Herd) # recalculate population structure with new K
burn = (Herd2$SUM_CUM[(1940-1917+1):length(Herd2$SUM_CUM)-1]) # Observed burn history
HerdCaribou_LD<-Caribou_F(K, burn, hoof, Pop, adult = SadF_Herd, fecun = Rec_Herd) 
IND = IND # inustrial footprint stays the same as the RS scenario
HerdScenarioS_LD<- ScenarioS_F(Area, Regime, IND, Density = 0.04)# Set Simfire  = TRUE, FALSE, TRUE (empirical data from 1940-2007)
Herdruns_LD<-MCRUNS_F(Area, Regime, IND, Density = 0.04)
#pdf("WSA_LD.pdf", height = 5, width  = 4)
#pLambda(Herdruns_LD, "-Dens") # change name to reflect the herd you've just run
#dev.off()

# EXPERIMENT 3: MB (+Fire) - increase fire burn rate to 0.01 (ScenarioS and MCRUNS) throughout simulations
K = Area*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_Herd) # adult females, and juvenile females
burn = rnorm(length(Herd_Fire), mean = 0.01, sd = Herd_Fire_sd)
HerdCaribou_MB<-Caribou_F(K, burn, hoof, Pop, adult = SadF_Herd, fecun = Rec_Herd) 
Regime = rnorm(length(Herd_Fire), mean = 0.01, sd = Herd_Fire_sd)
IND = Herd2$HOOF[(1940-1917 +1):length(Herd2$HOOF)-1]  # inustrial footprint stays the same as the RS scenario
HerdScenarioS_MB<- ScenarioS_F(Area, Regime, IND, Density = 0.06)# Simfire  = TRUE, TRUE, TRUE (no empirical data)
Herdruns_MB<-MCRUNS_F(Area, Regime, IND, Density = 0.06)
#pdf("WSA_LB.pdf", height = 5, width  = 4)
#pLambda(Herdruns_MB, "+Fire") # change name to reflect the herd you've just run
#dev.off()

# EXPERIMENT 4: HB (++Fire) - increase fire burn rate to 0.016
K = Area*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_Herd) 
burn = rnorm(length(Herd_Fire), mean = 0.016, sd = Herd_Fire_sd)
HerdCaribou_LB<-Caribou_F(K, burn, hoof, Pop, adult = SadF_Herd, fecun = Rec_Herd) 
Regime = rnorm(length(Herd_Fire), mean = 0.016, sd = Herd_Fire_sd)
IND = Herd2$HOOF[(1940-1917 +1):length(Herd2$HOOF)-1]  # inustrial footprint stays the same as the RS scenario
HerdScenarioS_HB<- ScenarioS_F(Area, Regime, IND, Density = 0.06)# Simfire  = TRUE, TRUE, TRUE (no empirical data)
Herdruns_HB<-MCRUNS_F(Area, Regime, IND, Density = 0.06)
#pdf("WSA_HB.pdf", height = 5, width  = 4)
#pLambda(Herdruns_HB, "++Fire") # change name to reflect the herd you've just run
#dev.off()

#Experiment 5: NI (0Ind) - no industry. Set IND to zero in regular scenario
K = Area*0.06# carrying capacity is 0.06 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_Herd)
burn = (Herd2$SUM_CUM[(1940-1917+1):length(Herd2$SUM_CUM)-1]) # Observed burn history
HerdCaribou_NI<-Caribou_F(K, burn, hoof, Pop, adult = SadF_Herd, fecun = Rec_Herd) 
Regime = Herd2$PROP_BURN[(1940-1917+1):length(Herd2$PROP_BURN)-1] # Same fire regime as the RS scenario
IND = rep(0, 69) # set Industry to 0 for the 69 years that we have data
HerdScenarioS_NI<- ScenarioS_F(Area, Regime, IND, Density = 0.06)# Simfire  = TRUE, FALSE, TRUE (empirical data from 1940-2007)
Herdruns_NI<-MCRUNS_F(Area, Regime, IND = rep(0, 69), Density = 0.06)
#pdf("WSA_NI.pdf", height = 5, width  = 4)
#pLambda(Herdruns_NI, "0Ind")
#dev.off()

#Experiment 6: NIHB (0Ind++Fire) - no industry, but a high burn rate (60 year fire return interval)
K = Area*0.06# carrying capacity is 0.06 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_Herd)
burn = rnorm(length(Herd_Fire), mean = 0.016, sd = Herd_Fire_sd)
HerdCaribou_NIHB<-Caribou_F(K, burn, hoof, Pop, adult = SadF_Herd, fecun = Rec_Herd) 
Regime = rnorm(length(Herd_Fire), mean = 0.016, sd = Herd_Fire_sd) # enhanced fire regime
IND = rep(0, 69) # # set Industry to 0 for the 69 years that we have data
HerdScenarioS_NIHB<- ScenarioS_F(Area, Regime, IND, Density = 0.06)# Simfire  = TRUE, FALSE, TRUE (empirical data from 1940-2007)
Herdruns_NIHB<-MCRUNS_F(Area, Regime, IND = rep(0, 69), Density = 0.06)
#pdf("WSA_NI.pdf", height = 5, width  = 4)
#pLambda(Herdruns_NIHB, "0Ind++Fire")
#dev.off()

###########################################################################################################################
# Create a file of all Herd graphs (In preparation for Figure 2) ----
pdf("CL_graphs2.pdf", height = 4, width = 5, onefile = TRUE) 
# TODO: Change name of herds for which ever herd you have just run above
# Fires(Herd2, "Annual Proportion Area Burned")
# pHoof(Herd2, "Cummulative Intustrial Footprint")
# pLambda(Herdruns, "Reg")
# pLambda(Herdruns_LD, "-Dens")
# pLambda(Herdruns_MB, "+Fire")
# pLambda(Herdruns_HB, "++Fire")
# pLambda(Herdruns_NI, "0Ind") # no industry, and observed burns
# pLambda(Herdruns_NIHB, "0Ind++Fire") # no industry and high burns
# dev.off()

#Save all simulations as rds objects so that they can be used in future
# TODO: change the herd name in the label of each
saveRDS(Herdruns, file = "CM_Reg.rds")
saveRDS(Herdruns_LD, file = "CM_-Dens.rds")
saveRDS(Herdruns_MB, file = "CM_+Fire.rds")
saveRDS(Herdruns_HB, file = "CM_++Fire.rds")
saveRDS(Herdruns_NI, file = "CM_0Ind.rds")
saveRDS(Herdruns_NIHB, file = "CM_0Ind++Fire.rds")


############################################################################################################################
# Getting averaged values and their confidence intervals For Table 1

scenario = Herdruns_NIHB

# COLUMN 3, TABLE 3
# 1. Get the year where the averaged lambda first was less than 1
Lam<- LambdaDates(scenario$Lambda)
Lam
# these are the years where lambda is less than 1
YearsLam<-as.vector(Lam$Year[Lam$Lambda == "TRUE"])
# this is the first year where lambda was less than 1
minYear<-min(YearsLam)
message('this is the first year where lambda < 1.0: ', minYear)

# this is the averaged year of lambdas less than 1 - not currently included in Table 3.
#mean(YearsLam)
# CIs(YearsLam, 0.95)

#COLUMN 4, TABLE 3
# 2. get the year, and confidence intervalls around Nt across all replications for 2017 - column 4, Table 3
# for 2017 (year 180)
2076-2017 # 2017 is 59 years prior to the end of the simulation
#Year2017<-239-59 # all lambda estimates (300 of them from the simulation) for year 2017
Year2017 <-239-40
Nt<-as.vector(scenario$Nt[,Year2017])
Nt<-(na.omit(as.numeric(Nt)))
mean(Nt)
CIs(Nt, 0.95)

#COLUMN 5, TABLE 3
# 3. calculate the year, and confidence interval of lambda for across replications at the year 2017
vector <- scenario$Lambda[,Year2017]
mean(vector)
CIs(vector, 0.95)

#COLUMN 6, TABLE 3
# 4. calculate the confifence intevals for the year of quasi-extinction
# Make a vector of all the years where an extinction showed up within the simulations:
ext <- ExtinctionDates(scenario$Nt)
# now calculate the 95%CI for this vector, and their means
Yearsext<-as.vector(ext$Year[ext$EXTINCT == "TRUE"])
# this is the first year where herd quasi-extinction occurred
minYear<-min(Yearsext)
message('this is the first year where Nt adult females < 10: ', minYear)


# this is the averaged year of lambdas less than 1 - not currently included in Table 3
mean(Yearsext)
CIs(Yearsext, 0.95)


#repeat this for all other Simulations (i.e. Experiments 1 through 6) by substitution out the scenario line for each
# SCENARIOS INCLUDE:
#WSA_Reg, WSA_-Dens, WSA_+Fire, WSA_++Fire, WSA_0Ind, WSA_0Ind++Fire
# same for: RE_, CL, LS, and CM_
#######################################################################

# For RE I had to pull directly from Tati's lambda table for the lambda values. N values (and QE value)
# are based on the above RDS files:
lambdaTableAllScenarios_V3
lambdaTable<-lambdaTableAllScenarios_V3[herd %in% 'RE']
scenario <- lambdaTable[scenario %in% '0Ind++Fire']

scenario$mean

# # these are the years where lambda is less than 1
YearsLam<-as.vector(scenario$mean[scenario$mean < 1.0])
length(YearsLam)
2076-length(YearsLam)
# this is the first year where lambda was less than 1
message('this is the first year where lambda < 1.0: ', 2076-length(YearsLam))

# lambda and CIs in 2017
scenario[year %in% 2017]


