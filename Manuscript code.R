#############################################################
# April 2019
# Frances Stewart
# Code to conduct the analysis in Nowak et al.
# Manuscript prepared for the Journal of Wildlife Management
#############################################################

# based on the analysis and herds used in Sorensen et al. (2008).
# herds include WSA (West side athabasca river), ESA (East side athabasca river), LS (little smoky), CL (cold lake), RE (Red Earth), and CM (Caribou Mountains)

##
# code and data sets provided in workspace_for_Steve.Rmd
##

## Table 1 ----
# WSA
WSA$all.data$AREA[1] # in ha. Divide by 100 to get km^2 (which is presented in Table 1)
WSA_InitialPop = (WSA$all.data$AREA[1])/100*0.06 # assumes carry capacity is 0.06 females/km^2
WSA_Fire = WSA$all.data$PROP_BURN[(1940-1917+1):length(WSA$all.data$PROP_BURN)] # fire events from 1940s onwards
WSA_Fire_mean = mean(WSA$all.data$PROP_BURN[(1940-1917+1):length(WSA$all.data$PROP_BURN)]) # mean annual porportion burned from 1940 onwards
WSA_Fire_sd = sd(WSA$all.data$PROP_BURN[(1940-1917+1):length(WSA$all.data$PROP_BURN)]) # sd annual porportion burned from 1940 onwards
BetaMomentEst(WSA_Fire)

#ESA - REMOVE HERD UNTIL FUTHER DATA BECOMES AVAILABLE - none of the below values are what are reported in Table 1.
ESA$all.data$AREA[1] # in ha. Divide by 100 to get km^2 (which is presented in Table 1) #NOT FROM TABLE 1
ESA_InitialPop = (ESA$all.data$AREA[1])/100*0.06 # assumes carry capacity is 0.06 females/km^2
ESA_Fire = ESA$all.data$PROP_BURN[(1940-1917+1):length(ESA$all.data$PROP_BURN)] # fire events from 1940s onwards
ESA_Fire_mean = mean(ESA$all.data$PROP_BURN[(1940-1917+1):length(ESA$all.data$PROP_BURN)]) # mean annual porportion burned from 1940 onwards
ESA_Fire_sd = sd(ESA$all.data$PROP_BURN[(1940-1917+1):length(ESA$all.data$PROP_BURN)]) # sd annual porportion burned from 1940 onwards
BetaMomentEst(ESA_Fire)
#ESA and w.esa both give 14,729 as the area
# f.esa seems to have variable values for the ESA
# the fire regime data could not be repeated for ESA - we are not convinced this GIS analysis was completed or done appropriately
## this is deposite trying to account for federal vs. provincial lands like in CL below
# we have therefore removed the ESA herd from the analysis to date.

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
# next, run the functions for each population

#Example TODO: change this to the correct code.
Pop<-c(180, 722)
cp <- as.numeric(names(IND = WSA$all.data$HOOF[cp]))>1939
WSACaribou<-Caribou_F(K = 902, p50s = WSA$all.data$PROP_BURN[cp], hoof = WSA$all.data$HOOF[cp], Pop = c(611, 921)) #adults, juveniles
WSAScenarios<- ScenarioS_F(WSA$all.data$AREA[1]/100, WSA_Regime, IND = WSA$all.data$HOOF[cp])
WSAruns<-MCRUNS_F(WSA$all.data$AREA[1]/100, WSA_Regime)



## Table 2 ----
# determine the probability of extinction across all herds and time frames

# use the Caribou function - produces 