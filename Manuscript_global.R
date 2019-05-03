# Steve SCB graphs ----

# Steve, presentation graphs Edmonton
#  Two herds, LS and WSA


# First load the data you will need
# setwd("Z:/Publications/Nowak et al/data")
#load(file = "LS_Data.gz")
#load(file = "WSA_Data.gz")
# load(file = "LS_logic_runs.gz")
# load(file = "WSA_logic_runs.gz")
#########################################
# Set working directory for export of files you will create
setwd("Z:/GitHub/Boo2019/outputs")

#Run the following code to re-make Steve's SCB presentation graphs:
############################################
#  1) Industrial Footprint


pHoof <- function(place, Title){      # Hoof print = Example: pHoof(CLAWR$HOOF, "CLAWR Industrial Footprint")
  plot(place$YEAR, place$HOOF, xlab = "", ylab= "",
       ylim = c(0,1),
       type = "h",
       cex.axis = 1.2)
  title(main = Title,
        xlab = "Year", ylab = "%IND",
        font.main = 2, font.lab = 2,
        col.main = "blue", col.lab = "blue",
        cex.lab = 1.3, cex.main = 1.4)
}


#
pdf(file = "LS and WSA Hoofprint.pdf", onefile = T)
pHoof(LS, "LS Industrial Footprint")
pHoof(WSA, "WSA Industrial Footprint")
dev.off()
#########################################
# Frances additions

pHoof <- function(place, Title){      # Hoof print = Example: pHoof(CLAWR$HOOF, "CLAWR Industrial Footprint")
  plot(place$YEAR, place$HOOF, xlab = "", ylab= "",
       ylim = c(0,1),
       type = "h",
       cex.axis = 1.2)
  title(main = Title,
        xlab = "Year", ylab = "%IND",
        font.main = 2, font.lab = 2,
        col.main = "black", col.lab = "black",
        cex.lab = 1.3, cex.main = 1.4)
}

pdf(file = "Industrial disturbance.pdf", onefile = T)
pHoof(CLAWR$all.data, "Cold Lake Industrial Footprint")
pHoof(CM$all.data, "Caribou Mountains Industrial Footprint")
pHoof(ESA$all.data, "East Side Athabasca Industrial Footprint")
pHoof(WSA, "West Side Athabasca Industrial Footprint") # overwritten from above Steve code
pHoof(LS, "Little Smoky Industrial Footprint") # overwritten from above Steve code
pHoof(RE$all.data, "Red Earth Industrial Footprint")
dev.off()

################################################################################
# 2)  Annual Prop Area Burned

Fires <- function(place, Title){      # Actual fires == Example: Fires(CLAWR$PROP_BURN, "CLAWR Fires")
  plot(place$YEAR, place$BURN_PROP, xlab = "", ylab= "",
       ylim = c(0,max(place$BURN_PROP)), type = "h", cex.axis = 1.2)
  title(main = Title, xlab = "Year", ylab = "Proportion Area Burned",
        font.main = 2, font.lab = 2,
        col.main = "darkblue", col.lab = "darkblue",
        cex.lab = 1.3, cex.main = 1.4)
}


pdf(file = "LS and WSA Prop. Area Burn.pdf", onefile = T)
Fires(LS, "LS Annual Proportion Area Burned")
Fires(WSA2, "WSA Annual Proportion Area Burned")
dev.off()
#########################################
# Frances additions
pdf(file = "Natural disturbance.pdf", onefile = T)
Fires <- function(place, Title){      # Actual fires == Example: Fires(CLAWR$PROP_BURN, "CLAWR Fires")
  plot(place$YEAR, place$PROP_BURN, xlab = "", ylab= "",
       ylim = c(0,max(place$PROP_BURN)), type = "h", cex.axis = 1.2)
  title(main = Title, xlab = "Year", ylab = "Proportion Area Burned",
        font.main = 2, font.lab = 2,
        col.main = "Black", col.lab = "Black",
        cex.lab = 1.3, cex.main = 1.4)
}
Fires(CLAWR$all.data, "Cold Lake Annual Proportion Area Burned")
Fires(CM$all.data, "Caribou Mountains Annual Proportion Area Burned")
Fires(ESA$all.data, "East Side Annual Proportion Area Burned")
Fires(RE$all.data, "Red Earth Annual Proportion Area Burned")

Fires <- function(place, Title){      # Actual fires == Example: Fires(CLAWR$PROP_BURN, "CLAWR Fires")
  plot(place$YEAR, place$BURN_PROP, xlab = "", ylab= "",
       ylim = c(0,max(place$BURN_PROP)), type = "h", cex.axis = 1.2)  #slightly different burn prop labeling between data sets
  title(main = Title, xlab = "Year", ylab = "Proportion Area Burned",
        font.main = 2, font.lab = 2,
        col.main = "Black", col.lab = "Black",
        cex.lab = 1.3, cex.main = 1.4)
}

Fires(LS, "Little Smoky Annual Proportion Area Burned")
Fires(WSA2, "West Side Athabasca Annual Proportion Area Burned")
dev.off()

################################################################################
#  3)  Lambda

pLambda<-function(Herd, Title = ""){      #plot mean Lambda  Example:  pLambda(clawrmc, "CLAWR")
  Lamb.CI <- apply(Herd$Lambda, 2, function(x){
    c(quantile(x,0.05),  mean(x), quantile(x, 0.95))
  })
  PP<-function(x = Lambda){
    Y1<-1837
    x0<-seq(1,100)
    plot(x = 0, y = 0,
         xlim=c(1,ncol(x))+Y1,ylim=range(x),
         xlab="", ylab="",
         cex.axis = 1.2)
    polygon(c(x0+Y1, rev(x0+Y1)), c(x[1,x0], rev(x[3,x0])), col = "lightgray")
    lines((x0+Y1),x[2,x0], type="l",lwd=3, col = "black")
    x1<-seq(100,170)
    polygon(c(x1+Y1, rev(x1+Y1)), c(x[1,x1], rev(x[3,x1])), col = "cornflowerblue")
    lines(x1+Y1,x[2,x1],col="darkblue",lwd=3)
    x2<-seq(170,ncol(x))
    polygon(c(x2+Y1, rev(x2+Y1)), c(x[1,x2], rev(x[3,x2])), col = "brown1")
    lines(x2+Y1,x[2,x2],col="darkred",lwd=3)
  }
  PP(Lamb.CI)
  title(main = Title, xlab = "Year", ylab = "Lambda",
        font.main = 2, font.lab = 4,
        col.main = "blue", col.lab = "blue",
        cex.lab = 1.3, cex.main = 1.4)
}


pdf(file = "LS and WSA Mean Lambda.pdf", onefile = T)
pLambda(logic.lsmc, "LS Mean Lambda")
pLambda(logic.wsamc, "WSA Mean Lambda")
#pLambda(WSAScenarios, "WSA Mean Lambda") # dosent currently work. TODO: Figure out how Steve got the logic.mc files
dev.off()
#########################################
# Frances additions
pLambda<-function(Herd, Title = ""){      #plot mean Lambda  Example:  pLambda(clawrmc, "CLAWR")
  Lamb.CI <- apply(Herd$Lambda, 2, function(x){
    c(quantile(x,0.05),  mean(x), quantile(x, 0.95))
  })
  PP<-function(x = Lambda){
    Y1<-1837
    x0<-seq(1,100)
    plot(x = 0, y = 0,
         xlim=c(1,ncol(x))+Y1,ylim=range(x),
         xlab="", ylab="",
         cex.axis = 1.2)
    polygon(c(x0+Y1, rev(x0+Y1)), c(x[1,x0], rev(x[3,x0])), col = "lightgray")
    lines((x0+Y1),x[2,x0], type="l",lwd=3, col = "black")
    x1<-seq(100,170)
    polygon(c(x1+Y1, rev(x1+Y1)), c(x[1,x1], rev(x[3,x1])), col = "cornflowerblue")
    lines(x1+Y1,x[2,x1],col="darkblue",lwd=3)
    x2<-seq(170,ncol(x))
    polygon(c(x2+Y1, rev(x2+Y1)), c(x[1,x2], rev(x[3,x2])), col = "brown1")
    lines(x2+Y1,x[2,x2],col="darkred",lwd=3)
  }
  PP(Lamb.CI)
  title(main = Title, xlab = "Year", ylab = "Lambda",
        font.main = 2, font.lab = 4,
        col.main = "black", col.lab = "black",
        cex.lab = 1.3, cex.main = 1.4)
}

#plotting
pLambda(WSAruns, "WSA Mean Lambda") # Working


################################################################################
# 4) Probability of Extinction
ExtProb<-function(Herd, Title = ""){       #plot extinction probability ExtProb(clawrmc, "CLAWR Pr(Extinction)")
  
  PP<-function(x){
    Y1<-1837
    x0<-seq(1,100)
    plot((x0+Y1),x[x0],type="l",lwd=3,
         xlim=c(1,length(x))+Y1,ylim=c(0,1),
         xlab = "", ylab = "",
         cex.axis = 1.2)
    x1<-seq(101,170)
    lines(x1+Y1,x[x1],col="blue",lwd=3)
    x2<-seq(171,length(x))
    lines(x2+Y1,x[x2],col="red",lwd=3)
  }
  
  
  PP(apply(Herd$Nt,2,function(x, N = 10)
    sum(x<10)/length(x)))  #Extinction Probability
  title(main = Title, xlab = "Year", ylab = "Prob. of Extinction",
        font.main = 2, font.lab = 4,
        col.main = "blue", col.lab = "blue",
        cex.lab = 1.3, cex.main = 1.4)
}


pdf(file = "LS and WSA Extinction Probability.pdf", onefile = T)
ExtProb(logic.lsmc, "LS Extinction Probability")
ExtProb(logic.wsamc, "WSA Extinction Probability")
dev.off()

#########################################
# Frances additions
ExtProb<-function(Herd, Title = ""){       #plot extinction probability ExtProb(clawrmc, "CLAWR Pr(Extinction)")
  
  PP<-function(x){
    Y1<-1837
    x0<-seq(1,100)
    plot((x0+Y1),x[x0],type="l",lwd=3,
         xlim=c(1,length(x))+Y1,ylim=c(0,1),
         xlab = "", ylab = "",
         cex.axis = 1.2)
    x1<-seq(101,170)
    lines(x1+Y1,x[x1],col="blue",lwd=3)
    x2<-seq(171,length(x))
    lines(x2+Y1,x[x2],col="red",lwd=3)
  }
  
  
  PP(apply(Herd$Nt,2,function(x, N = 10)
    sum(x<10)/length(x)))  #Extinction Probability
  title(main = Title, xlab = "Year", ylab = "Prob. of Extinction",
        font.main = 2, font.lab = 4,
        col.main = "black", col.lab = "black",
        cex.lab = 1.3, cex.main = 1.4)
}

#plot
ExtProb(WSAScenarios, "WSA Extinction Probability") # TODO: NOT CURRENTLY WORKING

################################################################################
# 5)  Mean Herd Size

MeanHerd<-function(Herd, Title = ""){ #plot mean herd size   MeanHerd(clawrmc, "CLAWR Mean Herd Size")
  
  
  MH.CI <- apply(Herd$Nt, 2, function(x){
    c(quantile(x,0.05),  mean(x), quantile(x, 0.95))
  })
  PP<-function(x = MH.CI){
    Y1<-1837
    x0<-seq(1,100)
    plot(x = 0, y = 0,
         xlim=c(1,ncol(x))+Y1,ylim=range(x),
         xlab="", ylab="",
         cex.axis = 1.2)
    polygon(c(x0+Y1, rev(x0+Y1)), c(x[1,x0], rev(x[3,x0])), col = "lightgray")
    lines((x0+Y1),x[2,x0], type="l",lwd=3, col = "black")
    x1<-seq(100,170)
    polygon(c(x1+Y1, rev(x1+Y1)), c(x[1,x1], rev(x[3,x1])), col = "cornflowerblue")
    lines(x1+Y1,x[2,x1],col="darkblue",lwd=3)
    x2<-seq(170,ncol(x))
    polygon(c(x2+Y1, rev(x2+Y1)), c(x[1,x2], rev(x[3,x2])), col = "brown1")
    lines(x2+Y1,x[2,x2],col="darkred",lwd=3)
  }
  PP(MH.CI)
  
  
  title(main = Title, xlab = "Year", ylab = "Mean N(females)",
        font.main = 2, font.lab = 4,
        col.main = "darkblue", col.lab = "darkred",
        cex.lab = 1.3, cex.main = 1.4)
}


pdf(file = "LS and WSA Mean Herd Size.pdf", onefile = T)
MeanHerd(logic.lsmc, "LS Mean Herd Size")
MeanHerd(logic.wsamc, "WSA Mean Herd Size")
dev.off()


#########################################
# Frances additions
MeanHerd<-function(Herd, Title = ""){ #plot mean herd size   MeanHerd(clawrmc, "CLAWR Mean Herd Size")
  
  
  MH.CI <- apply(Herd$Nt, 2, function(x){
    c(quantile(x,0.05),  mean(x), quantile(x, 0.95))
  })
  PP<-function(x = MH.CI){
    Y1<-1837
    x0<-seq(1,100)
    plot(x = 0, y = 0,
         xlim=c(1,ncol(x))+Y1,ylim=range(x),
         xlab="", ylab="",
         cex.axis = 1.2)
    polygon(c(x0+Y1, rev(x0+Y1)), c(x[1,x0], rev(x[3,x0])), col = "lightgray")
    lines((x0+Y1),x[2,x0], type="l",lwd=3, col = "black")
    x1<-seq(100,170)
    polygon(c(x1+Y1, rev(x1+Y1)), c(x[1,x1], rev(x[3,x1])), col = "cornflowerblue")
    lines(x1+Y1,x[2,x1],col="darkblue",lwd=3)
    x2<-seq(170,ncol(x))
    polygon(c(x2+Y1, rev(x2+Y1)), c(x[1,x2], rev(x[3,x2])), col = "brown1")
    lines(x2+Y1,x[2,x2],col="darkred",lwd=3)
  }
  PP(MH.CI)
  
  
  title(main = Title, xlab = "Year", ylab = "Mean N(females)",
        font.main = 2, font.lab = 4,
        col.main = "black", col.lab = "black",
        cex.lab = 1.3, cex.main = 1.4)
}

#plot
MeanHerd(WSAScenarios, "WSA Mean Herd Size") #TODO: Not currently working

################################################################################
# 6)  Difference in Extimction Prob between expert guess and current fire regime
ExtProb2 <- function(Herd1, Herd2, Title = ""){       #plot extinction probability ExtProb(clawrmc, "CLAWR Pr(Extinction)")
  
  
  PP2<-function(H1, H2){
    Y1<-1837
    x2<-seq(171,length(H1))
    plot((x2+Y1),H1[x2],type="l",lwd=3,
         xlim=c(171,length(H1))+Y1,ylim=c(0,1),
         xlab = "", ylab = "",
         cex.axis = 1.2, lty = 5)
    lines(x2+Y1,H2[x2],col="red",lwd=3, lty = 3)
  }
  
  
  PP2(apply(Herd1$Nt,2,function(x, N = 10)sum(x<10)/length(x)), apply(Herd2$Nt,2,
                                                                      function(x, N = 10)sum(x<10)/length(x)))  #Extinction Probability
  title(main = Title, xlab = "Year", ylab = "Prob. of Extinction",
        font.main = 2, font.lab = 4,
        col.main = "blue", col.lab = "blue",
        cex.lab = 1.3, cex.main = 1.4)
}


pFires2<-function(Herd1,Herd2,Title = ""){      #plot mean Fire  Example:  pFires(clawrmc, "CLAWR")
  PP<-function(H1, H2){
    Y1<-1837
    x2<-seq(171,length(H1))
    lines((x2+Y1),H1[x2],type="l",lwd=3)
    lines(x2+Y1,H2[x2],col="red",lwd=3)
  }
  PP(apply(Herd1$pYoung,2,mean), apply(Herd2$pYoung,2,mean))  #PYoung mean
}


ExtProb2(logic.wsamc, cf.wsamc, "Difference in WSA Extinction Probabilities")
pFires2(logic.wsamc, cf.wsamc)


legend(x = 2010, y = 1, pch = c(15, 15, NA, NA, NA, NA), lty = c(NA, NA, 5,3,1,1),
       lwd = c(NA, NA, 3,3,3,3),
       col = c("black", "red", "black", "red","black", "red"),
       legend = c("Fire simulations estimated from data",
                  "Fire simulations created from a burn rate of 0.01",
                  "Pr(Extinction)",
                  "Pr(Extinction)",
                  "Proportion of forest < 50 years old",
                  "Proportion of forest < 50 years old"))

#########################################
# Frances additions
# Still TODO



###########################################################################################################################################
###########################################################################################################################################
#Eliot Putsing Fire Growth Model ----

b = vector()
f = vector()


growth = function(N, age = NULL, b= 0.01) {
  if (is.null(age)) {
    res = vector(length = N)
  } else {
    res = age #
  }
  for (n in 0:N) {
    if (n == 0) {
      res[n+1] = b
    } else if (n < N) {
      res[n+1] = (1-b)*res[n]
    } else {
      res[N+1] = (1-b)*(res[N] + ifelse(is.na(res[N+1]),0,res[N+1]))
    }
  }
  res
}

f = growth(350)

for (i in 1:300) {
  f  = growth(350, age = f)
}


print(sum(f))


plot(f)

#################################################################################################################################################
#Caribou analysis Steve Josh ----

library(nlme)
library(lme4)
library(Cairo)

# Make up data
n.herds = 14
n.years = 9


herd = rep(1:n.herds, each = n.years)
year = rep(2001:(2001-1+n.years), n.herds)
lam.h = rnorm(n.herds, 1, 0.05)
lam.y = sort(rnorm(n.years, 1, 0.05))
lam.y = rnorm(n.years, 1, 0.1)
lam = rnorm(n.herds*n.years, rep(lam.h, each = n.years)*rep(lam.y, n.herds), 0.1)
plot(year, lam, col = herd)
rm(herd, year)


# Use real data
setwd("Z:/Publications/Nowak et al/data")
caribou = read.csv("CaribouLambda.csv", header = T)
caribou$herd.num = as.numeric(as.factor(caribou$herd))


cols = rainbow(length(unique(caribou$herd.num))+2)
#cols = 0:(length(unique(caribou$herd.num))+2)/(length(unique(caribou$herd.num))+2)
attach(caribou)
plot(year, lambda, pch = 19, col = cols[herd.num], xlim = c(2002, 2013), axes = F)
axis(1, at = c(2002, 2004, 2006, 2008))
axis(2)
detach(caribou)


# Try a few models, for fun
car.1 = glm(lambda ~ 1 , na.action = na.exclude, data = caribou) # No random effect
car.2 = lmer(lambda ~ 1 + (1|herd) , na.action = na.exclude, data = caribou) # just herd random effect
car.3 = lmer(lambda ~ 1 + (1|herd) + (1|year), na.action = na.exclude, data = caribou, REML=T) # year and herd random effect
car.4 = lmer(lambda ~ 1 + (1|year), na.action = na.exclude, data = caribou) # just year random effect
car.5 = lmer(lambda ~ year + (1|herd) + (1|year), na.action = na.exclude, data = caribou) # herd random effect and year fixed effect



print(c(fixed.effect.AIC = AIC(car.2), mixed.effect = summary(car.3)@AICtab[1]))
print(summary(car.3))


#plot model 3
fi = fitted(car.3)
for (i in unique(caribou$herd.num)) {
  lines(caribou$year[caribou$herd.num == i], fi[caribou$herd.num == i],
        col = cols[i])
  print(data.frame(herd = caribou$herd.num[caribou$herd.num == i],
                   year = caribou$year[caribou$herd.num == i],
                   fit = fi[caribou$herd.num == i]))
}
legend(x = 2009, y = 1.2, legend = unique(caribou$herd), title = "herd", lty = "solid", col = cols, pch = 19, xpd = T)
mtext(side = 3, line = 2, "Lambda, by year, and herd")


year.re = rnorm(100, 0, 0.033062)

#car.3@ranef

######################################################################################################################################################
######################################################################################################################################################
# FRANCES ADDITIONS
# April 8, 2019

# comparing model AICs does not give a metric of model fit, yet in the current version of the manuscript we argue to use car.3 because it "fits" better
# than other models based on AIC. Instead, use a liklihood ratio test to compare the significance of model fit:

anova(car.3, car.1, test = "LRT")
# to warrant using  model 3 over the simple model 1

# It seems odd to test models using year as both random and fixed effects - I dont belive this is a valid statisical test of GLMMS
# instead, test models with and without year as a a random vs, fixed effect to see which one is statistically better
# however, due to the definition of a 
# still, let us test:
car.6 = lmer(lambda ~ year + (1|herd), na.action = na.exclude, data = caribou) 
anova(car.3, car.6, test = "LRT")
# it is actually the exact same...

##################################################################################################################################################

# All of the above code helps to reproduce the analysis for SCB 2010 presentation
# This analysis is really only on LS and WSAB
# it does not include the analysis done for the rough draft of the manuscript

# need data from another 4 herds - at minimum to repeat the analysis done for the presentation on all herds

# I also need code to repeat the analysis, and plots, made for the manuscript
# This will help to put numbers into the results.
# ie code to repeat:
## Table 2, Probability of extinction for each year and experiment - including the critical year.
## Figure 1 - four herds probability of extinction - maybe make these into nice graphs sensu the presentation
## Figure 2 - experiments

# in BooManuscript2016>JoshBooCode>Final
# see Workspace_for_Dteve.RData for a lot of the work going on here - but it is hard to decipher what was done.


#############################################################
# RESULTS ----
# Re-creating Table 1 - OLD
WSA$HERD_AREA # same area as table 1
Initialpopulation <- 0.06*(WSA$HERD_AREA/100) # same initial population as Table 1. GOOD.
mean(WSA$BURN_PROP) # same as the Fire regime in Table 1. GOOD.
sd(WSA$BURN_PROP)
WSA.beta<-BetaMomentEst(WSA$BURN_PROP) # ok - these values are the same as in Table 1 of the ms. GOOD.

ESA$all.data$AREA # NOT THE SAME AS TABLE 1...
Initialpopulation<-0.06*(ESA$all.data$AREA/100)
mean(ESA$all.data$PROP_BURN)
sd(ESA$all.data$PROP_BURN)
ESA.beta<-BetaMomentEst(ESA$all.data$PROP_BURN) # ok - these values are the same as in Table 1 of the ms. GOOD.

LS$HERD_AREA # same as Table 1!
Initialpopulation <- 0.06*(LS$HERD_AREA/100) # same initial population as Table 1. GOOD.
mean(LS$BURN_PROP) # same as the Fire regime in Table 1. GOOD.
sd(LS$BURN_PROP)
LS.beta<-BetaMomentEst(LS$BURN_PROP) 

dat.CLAWR$AREA# same as Table 1!
Initialpopulation <- 0.06*(dat.CLAWR$AREA/100) # same initial population as Table 1. GOOD.
mean(dat.CLAWR$PROP_BURN) # same as the Fire regime in Table 1. GOOD.
sd(dat.CLAWR$PROP_BURN)
LS.beta<-BetaMomentEst(dat.CLAWR$PROP_BURN) 
dat.CLAWR$HOOF

RE$all.data$AREA # NOT THE SAME AS TABLE 1...
Initialpopulation<- 0.06*(RE$herdarea/100)
mean(RE$all.data$PROP_BURN) 
sd(RE$all.data$PROP_BURN)
RE.beta<-BetaMomentEst(RE$all.data$PROP_BURN)

dat.CM$AREA # some what the same as table 1?!
Initialpopulation <- 0.06*(dat.CM$AREA/100) # same initial population as Table 1. GOOD.
mean(dat.CM$PRO_BURN) # same as the Fire regime in Table 1. GOOD.
sd(dat.CM$PRO_BURN)
LS.beta<-BetaMomentEst(dat.CM$PRO_BURN) 
dat.CLAWR$HOOF


############################################################################
# April 15, 2019
# After a chat with Steve, we clarified how to calculate a few of these things. See below for values generated in Table 1 from Nowak old version.
# They now also check out.

# Recreating Table 1
# WSA



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
#############################################################################################################################################




# WSA ----
# re-calculate the cummulative burn for each year, as this was not originally provided. Run the function from
WSA2 <- Burn_F()
# ASSUMPTION - fires do not superimpose across the time lag window

# Recreating Table 1 - Summary data ----
WSA2$AREA[1] # in ha. Divide by 100 to get km^2 (which is presented in Table 1)
WSA_InitialPop = (WSA2$AREA[1])/100*0.06 # assumes carry capacity is 0.06 females/km^2
WSA_Fire = WSA2$PROP_BURN[(1940-1917+1):length(WSA2$PROP_BURN)-1] # fire events from 1940s onwards
WSA_Fire_mean = mean(WSA2$PROP_BURN[(1940-1917+1):length(WSA2$PROP_BURN)-1]) # mean annual porportion burned from 1940 onwards
WSA_Fire_sd = sd(WSA2$PROP_BURN[(1940-1917+1):length(WSA2$PROP_BURN)-1]) # sd annual porportion burned from 1940 onwards
WSA_Beta<- BetaMomentEst(WSA_Fire)
# TODO: need to change PROP_CUM_BRUN TO PROP_BURN - ie need annual burn proportion for simulation model.

# Set vital rates to average number from recorded from Alberta Caribou committee data: CaribouLambda.csv (2002-2008)
setwd("Z:/GitHub/Boo2019/data")
caribou<-read.csv("CaribouLambda.csv", header = T)
setwd("Z:/GitHub/Boo2019/outputs")
caribouWSA<-subset(caribou, caribou$herd == "West_Side_Athabasca_River")
SadF<-mean(caribouWSA$Adult_Female_Survival)/100 #0.8564. Adult female survival
Rec<-mean(caribouWSA$Calf_Recruitment)/100 #0.2024. Juvenile recruitment - TODO should this number be 1/2?
# ASSUMPTION  - these rates are held consistent through time.
## Alternately, we could set SadF to 0.85, and Rec to 0.3 in accordance with Environment Canada assumptions


# specify the population structure, based on the above information 
# population carrying capacity is 0.06 caribou/km^2
K = WSA2$AREA[1]/100*0.03 # carrying capacity is 0.03 females/km^2 as the upper limit
Pop <- c(K, K*(Rec)) # adult females, and juveniles
#

# summary plots
Fires(WSA2, "WSA proportion cumulative burn")
pHoof(WSA2, "WSA proportion industrial")


# Performing analyses ----
# FIRST FUNCTION: Calcualtes demographics without stochasticity, and only for the duration of time that we have data (69 years here)
p50s = (WSA2$PROP_CUM_BURN[(1940-1917+1):length(WSA2$PROP_CUM_BURN)-1])
# Annual proportion of area burned, all years from 1940s onwards, but not the last year
hoof = WSA2$HOOF[(1940-1917 +1):length(WSA2$HOOF)-1]# all years from 1940s onwards, but not the last year

WSACaribou<-Caribou_F(K, p50s, hoof, Pop, adult = SadF, fecun = Rec) 


# SECOND FUNCTION: this function brings in the period of time before our data collection (older than 69 years ago), 
# and a projected period of time to 2050 (500 years total)
Area = WSA2$AREA[1]/100# enter herd area size as one number in km^2
Regime = WSA_Fire # enter the mean of the fire regime (from Table 1, or above area specific code (i.e. WSA_Fire))
IND = WSA2$HOOF[(1940-1917 +1):length(WSA2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year

WSAScenarios<- ScenarioS_F(Area, Regime, IND) # TODO: ask steve about the density estimate (K)


# THIRD FUNCTION: this function adds environmental stochasticity to the simulation by repeating it 300 times
Area = WSA2$AREA[1]/100 # enter herd area size as one number in km^2
Regime = WSA_Fire # enter the mean of the fire regime (from Table 1, or above area specific code (i.e. WSA_Fire))
IND = WSA2$HOOF[(1940-1917 +1):length(WSA2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year

WSAruns<-MCRUNS_F(Area, Regime, IND) # TODO: ask steve about mmp

length(WSAruns$Lambda)/300 # 300 runs
# 196 years - correct

#plot
pLambda(WSAruns, "WSA")


# Performing experiments ----
# TODO: perform the experiments here, by altering variables from ScenarioS independently and plotting?

# Calculate the extinctions and YCRITS ----

# TODO: Ask Steve how to repeat Table 2 from the Nowak manuscript (i.e. calculate the probability of persistence)
# TODO: Ask Steve how he got his logic.gz files. I'll need to do this for all populations so that I can re-make the figures
# TODO: Go through plots with Steve - Mean lambda currently working, but Extinction, Nt, not




