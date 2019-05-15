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
# STEP 1: re-calculate the cummulative burn for each year, as this was not originally provided. Run the function from
WSA2 <- Burn_F(WSA$all.data, lagYears = 50, colToUse = "PROP_BURN", startYear = 1940)
WSA2<- b
# ASSUMPTION - fires do not superimpose across the time lag window

# STEP 2: Recreating Table 1 - Summary data ----
WSA2$AREA[1] # in ha. Divide by 100 to get km^2 (which is presented in Table 1)
WSA_InitialPop = (WSA2$AREA[1])/100*0.06 # assumes carry capacity is 0.06 caribou/km^2
WSA_Fire = WSA2$PROP_BURN[(1940-1917+1):length(WSA2$PROP_BURN)-1] # fire events from 1940s onwards
WSA_Fire_mean = mean(WSA2$PROP_BURN[(1940-1917+1):length(WSA2$PROP_BURN)-1]) # mean annual porportion burned from 1940 onwards
WSA_Fire_sd = sd(WSA2$PROP_BURN[(1940-1917+1):length(WSA2$PROP_BURN)-1]) # sd annual porportion burned from 1940 onwards
WSA_Beta<- BetaMomentEst(WSA_Fire)
WSA2$HOOF[WSA2$YEAR == 1980] # 0.117
WSA2$HOOF[WSA2$YEAR == 2007] # 0.851

# Source all files in the R folder
aa <- lapply(dir("R", full.names = TRUE), function(x) {print(x); source(x)})

# summary plots

Fires(WSA2, "WSA Annual Proportion Area Burned")
pHoof(WSA2, "WSA Cummulative Intustrial Footprint")

# STEP 3: specify the population structure, based on the above information
# Set vital rates to average number from recorded from Alberta Caribou committee data: CaribouLambda.csv (2002-2008)
caribou<-read.csv("Z:/GitHub/Boo2019/data/CaribouLambda.csv", header = T)
setwd("Z:/GitHub/Boo2019/outputs")
caribouWSA<-subset(caribou, caribou$herd == "West_Side_Athabasca_River")
SadF_WSA<-mean(caribouWSA$Adult_Female_Survival)/100 #0.8564. Adult female survival
Rec_WSA<-mean(caribouWSA$Calf_Recruitment)/100 #0.2024. Juvenile recruitment - TODO should this number be 1/2?
# ASSUMPTION  - these rates are held consistent through time.
## Alternately, we could set SadF to 0.85, and Rec to 0.3 in accordance with Environment Canada assumptions

# population carrying capacity is 0.06 caribou/km^2
K = (WSA2$AREA[1]/100)*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_WSA) # adult females, and juvenile females
# ASSUMPITON: 50: sex ratio of calves at survey. Some publications use 60% male.

# STEP 4: Performing analyses ----
# FIRST FUNCTION: Calcualtes demographics without stochasticity, and only for the duration of time that we have data (69 years here)
# p50s = (WSA2$PROP_BURN[(1940-1917+1):length(WSA2$PROP_BURN)-1]) # I dont believe this is cummulative
# Annual proportion of area burned, all years from 1940s onwards, but not the last year
burn = (WSA2$SUM_CUM[(1940-1917+1):length(WSA2$SUM_CUM)-1]) # this should be cummulative. 
# Annual proportion of area burned, all year from 19402 onwards, but not the last year
hoof = WSA2$HOOF[(1940-1917 +1):length(WSA2$HOOF)-1]# all years from 1940s onwards, but not the last year

WSACaribou<-Caribou_F(K, burn, hoof, Pop, adult = SadF_WSA, fecun = Rec_WSA) 

# SECOND FUNCTION: this function brings in the period of time before our data collection (older than 69 years ago), 
# and a projected period of time to 2050 (500 years total)
Area = WSA2$AREA[1]/100# enter herd area size as one number in km^2
Regime = WSA2$PROP_BURN[(1940-1917+1):length(WSA2$PROP_BURN)-1] # enter the annual proportion of area burned. 
# TODO: I might need to change this to SUM_CUM?
IND = WSA2$HOOF[(1940-1917 +1):length(WSA2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year

WSAScenarios<- ScenarioS_F(Area, Regime, IND, Density = 0.06)

# THIRD FUNCTION: this function adds environmental stochasticity to the simulation by repeating it 300 times
Area = WSA2$AREA[1]/100 # enter herd area size as one number in km^2
Regime = WSA2$PROP_BURN[(1940-1917+1):length(WSA2$PROP_BURN)-1] # enter the mean of the fire regime (from Table 1, or above area specific code (i.e. WSA_Fire))
IND = WSA2$HOOF[(1940-1917 +1):length(WSA2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year

WSAruns<-MCRUNS_F(Area, Regime, IND, Density = 0.06)

pdf("WSA_RS.pdf", height = 5, width  = 4)
pLambda(WSAruns, "WSA_RS")
dev.off()

length(WSAruns$Lambda)/300 # 300 runs
# 220 years - correct

# Performing experiments ----
# EXPERIMENT 1: RS  - done above
# EXPERIMENT 2: LD (low density) - change carrying capacity from 0.03 to 0.02
K = (WSA2$AREA[1]/100)*0.04# carrying capacity is 0.02 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_WSA) # adult females, and juvenile females
WSACaribou_LD<-Caribou_F(K, burn, hoof, Pop, adult = SadF_WSA, fecun = Rec_WSA) 
IND = WSA2$HOOF[(1940-1917 +1):length(WSA2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year
WSAScenarioS_LD<- ScenarioS_F(Area, Regime, IND, Density = 0.04)# Simfire  = TRUE, FALSE, TRUE (empirical data from 1940-2007)
WSAruns_LD<-MCRUNS_F(Area, Regime, IND, Density = 0.04)
pdf("WSA_LD.pdf", height = 5, width  = 4)
pLambda(WSAruns_LD, "WSA_LD")
dev.off()
# EXPERIMENT 3: LB - increase fire burn rate to 0.01 (ScenarioS and MCRUNS)
K = (WSA2$AREA[1]/100)*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_WSA) # adult females, and juvenile females
WSACaribou_LB<-Caribou_F(K, burn, hoof, Pop, adult = SadF_WSA, fecun = Rec_WSA) 
Regime = rnorm(length(WSA_Fire), mean = 0.01, sd = WSA_Fire_sd)
IND = WSA2$HOOF[(1940-1917 +1):length(WSA2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year
WSAScenarioS_LB<- ScenarioS_F(Area, Regime, IND, Density = 0.06)# Simfire  = TRUE, TRUE, TRUE (no empirical data)
WSAruns_LB<-MCRUNS_F(Area, Regime, IND, Density = 0.06)
pdf("WSA_LB.pdf", height = 5, width  = 4)
pLambda(WSAruns_LB, "WSA_LB")
dev.off()
# EXPERIMENT 4: HB - increase fire burn rate to 0.016
K = (WSA2$AREA[1]/100)*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_WSA) # adult females, and juvenile females
WSACaribou_HB<-Caribou_F(K, burn, hoof, Pop, adult = SadF_WSA, fecun = Rec_WSA) 
Regime = rnorm(length(WSA_Fire), mean = 0.016, sd = WSA_Fire_sd)
WSAScenarioS_HB<- ScenarioS_F(Area, Regime, IND, Density = 0.06)# Simfire  = TRUE, TRUE, TRUE (no empirical data)
WSAruns_HB<-MCRUNS_F(Area, Regime, IND, Density = 0.06)
pdf("WSA_HB.pdf", height = 5, width  = 4)
pLambda(WSAruns_HB, "WSA_HB")
dev.off()
#Experiment 5: NI - no industry. Set IND to zero in regular scenario
K = (WSA2$AREA[1]/100)*0.06# carrying capacity is 0.06 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_WSA) # adult females, and juvenile females
WSACaribou_NI<-Caribou_F(K, burn, hoof, Pop, adult = SadF_WSA, fecun = Rec_WSA) 
WSAScenarioS_NI<- ScenarioS_F(Area, Regime, IND = rep(0, 69), Density = 0.06)# Simfire  = TRUE, FALSE, TRUE (empirical data from 1940-2007)
WSAruns_NI<-MCRUNS_F(Area, Regime, IND = rep(0, 69), Density = 0.06)
pdf("WSA_NI.pdf", height = 5, width  = 4)
pLambda(WSAruns_NI, "WSA_NI")
dev.off()

# calculate extinctions from different experiments (Table 2): ----
# RS
# YCRIT date - date that lambda falls below 1.0
2057 - length(WSAScenarios$Lambda[WSAScenarios$Lambda < 1.0]) #The final simulation year, minus the years where lambda < 1.0
# year of extinction - where there are less than 10 females
2057 - length(WSAScenarios$Nt[WSAScenarios$Nt == "EXTINCT"]) #2042
# population size at 2017
WSAScenarios$Nt[length(WSAScenarios$Nt)-40]
# lambda estimate at 2017
WSAScenarios$Lambda[length(WSAScenarios$Lambda)-40]
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


# Create a file of all WSA graphs ----
pdf("WSA_graphs.pdf", height = 4, width = 5, onefile = TRUE)
Fires(WSA2, "WSA Annual Proportion Area Burned")
pHoof(WSA2, "WSA Cummulative Intustrial Footprint")
pLambda(WSAruns, "WSA_RS")
pLambda(WSAruns_LD, "WSA_LD")
pLambda(WSAruns_LB, "WSA_LF")
pLambda(WSAruns_HB, "WSA_HF")
pLambda(WSAruns_NI, "WSA_NI")
dev.off()



#####################################################################################################################################
# LS ----
LS2 <- Burn_F(LS$all.data, lagYears = 50, colToUse = "PROP_BURN", startYear = 1940)

# STEP 2: Recreating Table 1 - Summary data ----
LS2$AREA[1] 
LS_InitialPop = (LS2$AREA[1])/100*0.06 
LS_Fire = LS2$PROP_BURN[(1940-1917+1):length(LS2$PROP_BURN)-1] 
LS_Fire_mean = mean(LS2$PROP_BURN[(1940-1917+1):length(LS2$PROP_BURN)-1])
LS_Fire_sd = sd(LS2$PROP_BURN[(1940-1917+1):length(LS2$PROP_BURN)-1]) 
LS_Beta<- BetaMomentEst(LS_Fire)
LS2$HOOF[LS2$YEAR == 1980]
LS2$HOOF[LS2$YEAR == 2007]

# summary plots
Fires(LS2, "LS Annual Proportion Area Burned")
pHoof(LS2, "LS Cummulative Intustrial Footprint")

# STEP 3: specify the population structure, based on the above information
# Set vital rates to average number from recorded from Alberta Caribou committee data: CaribouLambda.csv (2002-2008)
setwd("Z:/GitHub/Boo2019/data")
caribou<-read.csv("CaribouLambda.csv", header = T)
setwd("Z:/GitHub/Boo2019/outputs")
caribouLS<-subset(caribou, caribou$herd == "Little_Smoky")
SadF_LS<-mean(caribouLS$Adult_Female_Survival)/100 #0.8461
Rec_LS<-mean(caribouLS$Calf_Recruitment)/100 #0.1452 

# population carrying capacity is 0.06 caribou/km^2
K = (LS2$AREA[1]/100)*0.06
Pop <- c(K*0.5, K*0.5*Rec_LS) 

# STEP 4: Performing analyses ----
# FIRST FUNCTION: Calcualtes demographics without stochasticity, and only for the duration of time that we have data (69 years here)
burn = (LS2$SUM_CUM[(1940-1917+1):length(LS2$SUM_CUM)-1]) # this should be cummulative. 
# Annual proportion of area burned, all year from 19402 onwards, but not the last year
hoof = LS2$HOOF[(1940-1917 +1):length(LS2$HOOF)-1]# all years from 1940s onwards, but not the last year

LSCaribou<-Caribou_F(K, burn, hoof, Pop, adult = SadF, fecun = Rec) 

# SECOND FUNCTION: this function brings in the period of time before our data collection (older than 69 years ago), 
# and a projected period of time to 2050 (500 years total)
Area = LS2$AREA[1]/100# enter herd area size as one number in km^2
Regime = LS2$PROP_BURN[(1940-1917+1):length(LS2$PROP_BURN)-1] # enter the annual proportion of area burned. 
# TODO: I might need to change this to SUM_CUM?
IND = LS2$HOOF[(1940-1917 +1):length(LS2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year

LSScenarios<- ScenarioS_F(Area, Regime, IND, Density = 0.06)

# THIRD FUNCTION: this function adds environmental stochasticity to the simulation by repeating it 300 times
Area = LS2$AREA[1]/100 # enter herd area size as one number in km^2
Regime = LS2$PROP_BURN[(1940-1917+1):length(LS2$PROP_BURN)-1] # enter the mean of the fire regime (from Table 1, or above area specific code (i.e. LS_Fire))
IND = LS2$HOOF[(1940-1917 +1):length(LS2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year

LSruns<-MCRUNS_F(Area, Regime, IND)

#plot
pdf("LS_RS.pdf", height = 5, width = 4)
pLambda(LSruns, "LS")
dev.off()

# Performing experiments ----
# EXPERIMENT 1: RE  - done above
# EXPERIMENT 2: LD - change carrying capacity from 0.03 to 0.02
K = (LS2$AREA[1]/100)*0.04# carrying capacity is 0.02 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_LS) # adult females, and juvenile females
LSCaribou_LD<-Caribou_F(K, burn, hoof, Pop, adult = SadF, fecun = Rec) 
LSScenarioS_LD<- ScenarioS_F(Area, Regime, IND, Density = 0.04)
LSruns_LD<-MCRUNS_F(Area, Regime, IND, Density = 0.04)
pdf("LS_LD.pdf", height = 5, width  = 4)
pLambda(LSruns_LD, "LS_LD")
dev.off()
# EXPERIMENT 3: HF - increase fire burn rate to 0.01 (ScenarioS and MCRUNS)
K = (LS2$AREA[1]/100)*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_LS) # adult females, and juvenile females
LSCaribou_HF<-Caribou_F(K, burn, hoof, Pop, adult = SadF, fecun = Rec) 
Regime = rnorm(length(LS_Fire), mean = 0.01, sd = LS_Fire_sd)
LSScenarioS_HF<- ScenarioS_F(Area, Regime, IND, Density = 0.06)
LSruns_HF<-MCRUNS_F(Area, Regime, IND, Density = 0.06)
pdf("LS_HF.pdf", height = 5, width  = 4)
pLambda(LSruns_HF, "LS_HF")
dev.off()

# calculate extinctions: ----
# RS
# YCRIT date - date that lambda falls below 1.0
2057 - length(LSScenarios$Lambda[LSScenarios$Lambda < 1.0])
# year of extinction - where there are less than 10 females
2057 - length(LSScenarios$Nt[LSScenarios$Nt == "EXTINCT"]) #2012?
LSScenarios$Nt[length(LSScenarios$Nt)-40]
LSScenarios$Lambda[length(LSScenarios$Lambda)-40]
# LD
2057 - length(LSScenarioS_LD$Lambda[LSScenarioS_LD$Lambda < 1.0]) 
2057 - length(LSScenarioS_LD$Nt[LSScenarioS_LD$Nt == "EXTINCT"]) 
LSScenarioS_LD$Nt[length(LSScenarioS_LD$Nt)-40]
LSScenarioS_LD$Lambda[length(LSScenarioS_LD$Lambda)-40]
#HF
2057 - length(LSScenarioS_HF$Lambda[LSScenarioS_HF$Lambda < 1.0]) 
2057 - length(LSScenarioS_HF$Nt[LSScenarioS_HF$Nt == "EXTINCT"])
LSScenarioS_HF$Nt[length(LSScenarioS_HF$Nt)-40]
LSScenarioS_HF$Lambda[length(LSScenarioS_HF$Lambda)-40]

# Table 3 ---
# is lambda correlated to either burn or industrial development? years 1940 through 2007
cor.test(LS2$BURN_INC[(1940-1917+1):length(LS2$SUM_CUM)-1], LSScenarios$Lambda[102:170]) 
cor.test(LS2$CUM_WELL[(1940-1917+1):length(LS2$SUM_CUM)-1], LSScenarios$Lambda[102:170])

# Create a file of all LS graphs ----
pdf("LS graphs.pdf", height = 4, width = 5, onefile = TRUE)
Fires(LS2, "LS Annual Proportion Area Burned")
pHoof(LS2, "LS Cummulative Intustrial Footprint")
pLambda(LSruns, "LS_RS")
pLambda(LSruns_LD, "LS_LD")
pLambda(LSruns_HF, "LS_HF")
dev.off()


##################################################################################################################################
# CLAWR ----
CLAWR2 <- Burn_F(CLAWR$all.data, lagYears = 50, colToUse = "PROP_BURN", startYear = 1940)
# ASSUMPTION - fires do not superimpose across the time lag window

# STEP 2: Recreating Table 1 - Summary data ----
f.clawr$AREA_HERD[1] # in ha. Divide by 100 to get km^2 (which is presented in Table 1)
CLAWR_InitialPop = (f.clawr$AREA_HERD[1])/100*0.06 # assumes carry capacity is 0.06 caribou/km^2
CLAWR_Fire = CLAWR2$PROP_BURN[(1940-1917+1):length(CLAWR2$PROP_BURN)-1] # fire events from 1940s onwards
CLAWR_Fire_mean = mean(CLAWR2$PROP_BURN[(1940-1917+1):length(CLAWR2$PROP_BURN)-1]) # mean annual porportion burned from 1940 onwards
CLAWR_Fire_sd = sd(CLAWR2$PROP_BURN[(1940-1917+1):length(CLAWR2$PROP_BURN)-1]) # sd annual porportion burned from 1940 onwards
CLAWR_Beta<- BetaMomentEst(CLAWR_Fire)
CLAWR2$HOOF[CLAWR2$YEAR == 1980]
CLAWR2$HOOF[CLAWR2$YEAR == 2007]

# summary plots
Fires(CLAWR2, "CLAWR Annual Proportion Area Burned")
pHoof(CLAWR2, "CLAWR Cummulative Intustrial Footprint")

# STEP 3: specify the population structure, based on the above information
# Set vital rates to average number from recorded from Alberta Caribou committee data: CaribouLambda.csv (2002-2008)
setwd("Z:/GitHub/Boo2019/data")
caribou<-read.csv("CaribouLambda.csv", header = T)
setwd("Z:/GitHub/Boo2019/outputs")
caribouCLAWR<-na.omit(subset(caribou, caribou$herd == "Cold_Lake_Air_Weapons_Range"))
SadF_CLAWR<-mean(caribouCLAWR$Adult_Female_Survival)/100 #0.801 Adult female survival
Rec_CLAWR<-mean(caribouCLAWR$Calf_Recruitment)/100 #0.169 Juvenile recruitment - TODO should this number be 1/2?
# interesting  - the exact same as LS
# ASSUMPTION  - these rates are held consistent through time.
## Alternately, we could set SadF to 0.85, and Rec to 0.3 in accordance with Environment Canada assumptions

# population carrying capacity is 0.06 caribou/km^2
K = (f.clawr$AREA_HERD[1]/100)*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_CLAWR) # adult females, and juvenile females
# ASSUMPITON: 50:50 sex ratio of calves at survey. Some publications use 60% male.


# STEP 4: Performing analyses ----
# FIRST FUNCTION: Calcualtes demographics without stochasticity, and only for the duration of time that we have data (69 years here)
# p50s = (CLAWR2$PROP_BURN[(1940-1917+1):length(CLAWR2$PROP_BURN)-1]) # I dont believe this is cummulative
# Annual proportion of area burned, all years from 1940s onwards, but not the last year
burn = (CLAWR2$SUM_CUM[(1940-1917+1):length(CLAWR2$SUM_CUM)-1]) # this should be cummulative. 
# Annual proportion of area burned, all year from 19402 onwards, but not the last year
hoof = CLAWR2$HOOF[(1940-1917 +1):length(CLAWR2$HOOF)-1]# all years from 1940s onwards, but not the last year

CLAWRCaribou<-Caribou_F(K, burn, hoof, Pop, adult = SadF, fecun = Rec) 

# SECOND FUNCTION: this function brings in the period of time before our data collection (older than 69 years ago), 
# and a projected period of time to 2050 (500 years total)
Area = f.clawr$AREA_HERD[1]/100# enter herd area size as one number in km^2
Regime = CLAWR2$PROP_BURN[(1940-1917+1):length(CLAWR2$PROP_BURN)-1] # enter the annual proportion of area burned. 
# TODO: I might need to change this to SUM_CUM?
IND = CLAWR2$HOOF[(1940-1917 +1):length(CLAWR2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year

CLAWRScenarios<- ScenarioS_F(Area, Regime, IND, Density = 0.06)

# THIRD FUNCTION: this function adds environmental stochasticity to the simulation by repeating it 300 times
Area = CLAWR2$AREA[1]/100 # enter herd area size as one number in km^2
Regime = CLAWR2$PROP_BURN[(1940-1917+1):length(CLAWR2$PROP_BURN)-1] # enter the mean of the fire regime (from Table 1, or above area specific code (i.e. CLAWR_Fire))
IND = CLAWR2$HOOF[(1940-1917 +1):length(CLAWR2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year

CLAWRruns<-MCRUNS_F(Area, Regime, IND)

#plot
pdf("CL_RS.pdf", height = 5, width = 4)
pLambda(CLAWRruns, "CL_RS")
dev.off()

# Performing experiments ----
# EXPERIMENT 1: RE  - done above
# EXPERIMENT 2: LD - change carrying capacity from 0.03 to 0.02
K = (f.clawr$AREA_HERD[1]/100)*0.04# carrying capacity is 0.02 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_CLAWR) # adult females, and juvenile females
CLAWRCaribou_LD<-Caribou_F(K, burn, hoof, Pop, adult = SadF, fecun = Rec) 
CLAWRScenarioS_LD<- ScenarioS_F(Area, Regime, IND, Density = 0.04)
CLAWRruns_LD<-MCRUNS_F(Area, Regime, IND, Density = 0.04)
pdf("CL_LD.pdf", height = 5, width  = 4)
pLambda(CLAWRruns_LD, "CL_LD")
dev.off()
# EXPERIMENT 3: HF - increase fire burn rate to 0.01 (ScenarioS and MCRUNS)
K = (f.clawr$AREA_HERD[1]/100)*0.06
Pop <- c(K*0.5, K*0.5*Rec_CLAWR) 
CLAWRCaribou_HF<-Caribou_F(K, burn, hoof, Pop, adult = SadF, fecun = Rec) 
Regime = rnorm(length(CLAWR_Fire), mean = 0.01, sd = CLAWR_Fire_sd)
CLAWRScenarioS_HF<- ScenarioS_F(Area, Regime, IND, Density = 0.06)
CLAWRruns_HF<-MCRUNS_F(Area, Regime, IND, Density = 0.06)
pdf("CLAWR_HF.pdf", height = 5, width  = 4)
pLambda(CLAWRruns_HF, "CLAWR_HF")
dev.off()

# Table 2 ---
# calculate extinctions: ----
# RS
# YCRIT date - date that lambda falls below 1.0
2057 - length(CLAWRScenarios$Lambda[CLAWRScenarios$Lambda < 1.0])
# year of extinction - where there are less than 10 females
2057 - length(CLAWRScenarios$Nt[CLAWRScenarios$Nt == "EXTINCT"]) 
CLAWRScenarios$Nt[length(CLAWRScenarios$Nt)-40]
CLAWRScenarios$Lambda[length(CLAWRScenarios$Lambda)-40]
# LD
2057 - length(CLAWRScenarioS_LD$Lambda[CLAWRScenarioS_LD$Lambda < 1.0]) 
2057 - length(CLAWRScenarioS_LD$Nt[CLAWRScenarioS_LD$Nt == "EXTINCT"]) 
CLAWRScenarioS_HF$Nt[length(CLAWRScenarioS_HF$Nt)-40]
CLAWRScenarioS_HF$Lambda[length(CLAWRScenarioS_HF$Lambda)-40]
#HF
2057 - length(CLAWRScenarioS_HF$Lambda[CLAWRScenarioS_HF$Lambda < 1.0]) 
2057 - length(CLAWRScenarioS_HF$Nt[CLAWRScenarioS_HF$Nt == "EXTINCT"])
CLAWRScenarioS_LD$Nt[length(CLAWRScenarioS_LD$Nt)-40]
CLAWRScenarioS_LD$Lambda[length(CLAWRScenarioS_LD$Lambda)-40]

# Table 3 ---
# is lambda correlated to either burn or industrial development? years 1940 through 2007
cor.test(CLAWR2$BURN_INC[(1940-1917+1):length(CLAWR2$SUM_CUM)-1], CLAWRScenarios$Lambda[102:170]) 
cor.test(CLAWR2$CUM_WELL[(1940-1917+1):length(CLAWR2$SUM_CUM)-1], CLAWRScenarios$Lambda[102:170])

# Create a file of all LS graphs ----
pdf("CL graphs.pdf", height = 4, width = 5, onefile = TRUE)
Fires(CLAWR2, "CL Annual Proportion Area Burned")
pHoof(CLAWR2, "CL Cummulative Intustrial Footprint")
pLambda(CLAWRruns, "CL_RS")
pLambda(CLAWRruns_LD, "CL_LD")
pLambda(CLAWRruns_HF, "CL_HF")
dev.off()

###################################################################################################################################
# RE ----
RE2 <- Burn_F(RE$all.data, lagYears = 50, colToUse = "PROP_BURN", startYear = 1940)
# ASSUMPTION - fires do not superimpose across the time lag window

# STEP 2: Recreating Table 1 - Summary data ----
RE2$AREA[1] # in ha. Divide by 100 to get km^2 (which is presented in Table 1)
RE_InitialPop = (RE2$AREA[1])/100*0.06 # assumes carry capacity is 0.06 caribou/km^2
RE_Fire = RE2$PROP_BURN[(1940-1917+1):length(RE2$PROP_BURN)-1] # fire events from 1940s onwards
RE_Fire_mean = mean(RE2$PROP_BURN[(1940-1917+1):length(RE2$PROP_BURN)-1]) # mean annual porportion burned from 1940 onwards
RE_Fire_sd = sd(RE2$PROP_BURN[(1940-1917+1):length(RE2$PROP_BURN)-1]) # sd annual porportion burned from 1940 onwards
RE_Beta<- BetaMomentEst(RE_Fire)
RE2$HOOF[RE2$YEAR == 1980]
RE2$HOOF[RE2$YEAR == 2007]

# summary plots
Fires(RE2, "RE Annual Proportion Area Burned")
pHoof(RE2, "RE Cummulative Intustrial Footprint")

# STEP 3: specify the population structure, based on the above information
# Set vital rates to average number from recorded from Alberta Caribou committee data: CaribouLambda.csv (2002-2008)
setwd("Z:/GitHub/Boo2019/data")
caribou<-read.csv("CaribouLambda.csv", header = T)
setwd("Z:/GitHub/Boo2019/outputs")
caribouRE<-subset(caribou, caribou$herd == "Red_Earth")
SadF_RE<-mean(caribouRE$Adult_Female_Survival)/100 #0.8458 Adult female survival
Rec_RE<-mean(caribouRE$Calf_Recruitment)/100 #0.1347 Juvenile recruitment - TODO should this number be 1/2?
# ASSUMPTION  - these rates are held consistent through time.
## Alternately, we could set SadF to 0.85, and Rec to 0.3 in accordance with Environment Canada assumptions

# population carrying capacity is 0.06 caribou/km^2
K = (RE2$AREA[1]/100)*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_RE) # adult females, and juvenile females
# ASSUMPITON: 50: sex ratio of calves at survey. Some publications use 60% male.


# STEP 4: Performing analyses ----
# FIRST FUNCTION: Calcualtes demographics without stochasticity, and only for the duration of time that we have data (69 years here)
# p50s = (RE2$PROP_BURN[(1940-1917+1):length(RE2$PROP_BURN)-1]) # I dont believe this is cummulative
# Annual proportion of area burned, all years from 1940s onwards, but not the last year
burn = (RE2$SUM_CUM[(1940-1917+1):length(RE2$SUM_CUM)-1]) # this should be cummulative. 
# Annual proportion of area burned, all year from 19402 onwards, but not the last year
hoof = RE2$HOOF[(1940-1917 +1):length(RE2$HOOF)-1]# all years from 1940s onwards, but not the last year

RECaribou<-Caribou_F(K, burn, hoof, Pop, adult = SadF, fecun = Rec) 

# SECOND FUNCTION: this function brings in the period of time before our data collection (older than 69 years ago), 
# and a projected period of time to 2050 (500 years total)
Area = RE2$AREA[1]/100# enter herd area size as one number in km^2
Regime = RE2$PROP_BURN[(1940-1917+1):length(RE2$PROP_BURN)-1] # enter the annual proportion of area burned. 
# TODO: I might need to change this to SUM_CUM?
IND = RE2$HOOF[(1940-1917 +1):length(RE2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year

REScenarios<- ScenarioS_F(Area, Regime, IND, Density = 0.06)


# THIRD FUNCTION: this function adds environmental stochasticity to the simulation by repeating it 300 times
Area = RE2$AREA[1]/100 # enter herd area size as one number in km^2
Regime = RE2$PROP_BURN[(1940-1917+1):length(RE2$PROP_BURN)-1] # enter the mean of the fire regime (from Table 1, or above area specific code (i.e. RE_Fire))
IND = RE2$HOOF[(1940-1917 +1):length(RE2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year

REruns<-MCRUNS_F(Area, Regime, IND, Density = 0.06)

#plot
pdf("RE_RS.pdf", height = 5, width = 4)
pLambda(REruns, "RE")
dev.off()


# Performing experiments ----
# EXPERIMENT 1: RE  - done above
# EXPERIMENT 2: LD - change carrying capacity from 0.03 to 0.02
K = (RE2$AREA[1]/100)*0.04
Pop <- c(K*0.5, K*0.5*Rec_RE)
RECaribou_LD<-Caribou_F(K, burn, hoof, Pop, adult = SadF, fecun = Rec) 
REScenarioS_LD<- ScenarioS_F(Area, Regime, IND, Density = 0.04)
REruns_LD<-MCRUNS_F(Area, Regime, IND, Density = 0.04)
pdf("RE_LD.pdf", height = 5, width  = 4)
pLambda(REruns_LD, "RE_LD")
dev.off()
# EXPERIMENT 3: HF - increase fire burn rate to 0.01 (ScenarioS and MCRUNS)
K = (RE2$AREA[1]/100)*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_RE) # adult females, and juvenile females
RECaribou_HF<-Caribou_F(K, burn, hoof, Pop, adult = SadF, fecun = Rec) 
Regime = rnorm(length(RE_Fire), mean = 0.01, sd = RE_Fire_sd)
REScenarioS_HF<- ScenarioS_F(Area, Regime, IND, Density = 0.06)
REruns_HF<-MCRUNS_F(Area, Regime, IND, Density = 0.06)
pdf("RE_HF.pdf", height = 5, width  = 4)
pLambda(REruns_HF, "RE_HF")
dev.off()

# Table 2 ----
# calculate extinctions: ----
# RS
# YCRIT date - date that lambda falls below 1.0
2057 - length(REScenarios$Lambda[REScenarios$Lambda < 1.0])
# year of extinction - where there are less than 10 females
2057 - length(REScenarios$Nt[REScenarios$Nt == "EXTINCT"]) #2012?
REScenarios$Nt[length(REScenarios$Nt)-40]
REScenarios$Lambda[length(REScenarios$Lambda)-40]
# LD
2057 - length(REScenarioS_LD$Lambda[REScenarioS_LD$Lambda < 1.0]) 
2057 - length(REScenarioS_LD$Nt[REScenarioS_LD$Nt == "EXTINCT"]) 
REScenarioS_HF$Nt[length(REScenarioS_HF$Nt)-41]
REScenarioS_HF$Lambda[length(REScenarioS_HF$Lambda)-40]
#HF
2057 - length(REScenarioS_HF$Lambda[REScenarioS_HF$Lambda < 1.0]) 
2057 - length(REScenarioS_HF$Nt[REScenarioS_HF$Nt == "EXTINCT"])
REScenarioS_LD$Nt[length(REScenarioS_LD$Nt)-40]
REScenarioS_LD$Lambda[length(REScenarioS_LD$Lambda)-40]

# Table 3 ---
# is lambda correlated to either burn or industrial development? years 1940 through 2007
cor.test(RE2$BURN_INC[(1940-1917+1):length(RE2$SUM_CUM)-1], REScenarios$Lambda[102:170]) 
cor.test(RE2$CUM_WELL[(1940-1917+1):length(RE2$SUM_CUM)-1], REScenarios$Lambda[102:170])

# Create a file of all LS graphs ----
pdf("RE graphs.pdf", height = 4, width = 5, onefile = TRUE)
Fires(RE2, "RE Annual Proportion Area Burned")
pHoof(RE2, "RE Cummulative Intustrial Footprint")
pLambda(REruns, "RE_RS")
pLambda(REruns_LD, "RE_LD")
pLambda(REruns_HF, "RE_HF")
dev.off()

##################################################################################################################################
# CM ----

CM2 <- Burn_F(CM$all.data, lagYears = 50, colToUse = "PROP_BURN", startYear = 1940)
# ASSUMPTION - fiCMs do not superimpose across the time lag window

# STEP 2: CMcCMating Table 1 - Summary data ----
CM2$AREA[1] # in ha. Divide by 100 to get km^2 (which is pCMsented in Table 1)
CM_InitialPop = (CM2$AREA[1])/100*0.06 # assumes carry capacity is 0.06 caribou/km^2
CM_Fire = CM2$PROP_BURN[(1940-1917+1):length(CM2$PROP_BURN)-1] # fiCM events from 1940s onwards
CM_Fire_mean = mean(CM2$PROP_BURN[(1940-1917+1):length(CM2$PROP_BURN)-1]) # mean annual porportion burned from 1940 onwards
CM_Fire_sd = sd(CM2$PROP_BURN[(1940-1917+1):length(CM2$PROP_BURN)-1]) # sd annual porportion burned from 1940 onwards
CM_Beta<- BetaMomentEst(CM_Fire)
CM2$HOOF[CM2$YEAR == 1980]
CM2$HOOF[CM2$YEAR == 2007]

# summary plots
Fires(CM2, "CM Annual Proportion Area Burned")
pHoof(CM2, "CM Cummulative Intustrial Footprint")

# STEP 3: specify the population structuCM, based on the above information
# Set vital rates to average number from CMcorded from Alberta Caribou committee data: CaribouLambda.csv (2002-2008)
setwd("Z:/GitHub/Boo2019/data")
caribou<-read.csv("CaribouLambda.csv", header = T)
setwd("Z:/GitHub/Boo2019/outputs")
caribouCM<-subset(caribou, caribou$herd == "Caribou_Mountains")
SadF_CM<-mean(caribouCM$Adult_Female_Survival)/100 #0.812 Adult female survival
Rec_CM<-mean(caribouCM$Calf_Recruitment)/100 #0.156 Juvenile CMcruitment - TODO should this number be 1/2?
# ASSUMPTION  - these rates aCM held consistent through time.
## Alternately, we could set SadF to 0.85, and CMc to 0.3 in accordance with Environment Canada assumptions

# population carrying capacity is 0.06 caribou/km^2
K = (CM2$AREA[1]/100)*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_CM) # adult females, and juvenile females
# ASSUMPITON: 50: sex ratio of calves at survey. Some publications use 60% male.

# STEP 4: Performing analyses ----
# FIRST FUNCTION: Calcualtes demographics without stochasticity, and only for the duration of time that we have data (69 years heCM)
# p50s = (CM2$PROP_BURN[(1940-1917+1):length(CM2$PROP_BURN)-1]) # I dont believe this is cummulative
# Annual proportion of aCMa burned, all years from 1940s onwards, but not the last year
burn = (CM2$SUM_CUM[(1940-1917+1):length(CM2$SUM_CUM)-1]) # this should be cummulative. 
# Annual proportion of aCMa burned, all year from 19402 onwards, but not the last year
hoof = CM2$HOOF[(1940-1917 +1):length(CM2$HOOF)-1]# all years from 1940s onwards, but not the last year

CMCaribou<-Caribou_F(K, burn, hoof, Pop, adult = SadF, fecun = Rec) 

# SECOND FUNCTION: this function brings in the period of time befoCM our data collection (older than 69 years ago), 
# and a projected period of time to 2050 (500 years total)
Area = CM2$AREA[1]/100# enter herd aCMa size as one number in km^2
Regime = CM2$PROP_BURN[(1940-1917+1):length(CM2$PROP_BURN)-1] # enter the annual proportion of aCMa burned. 
# TODO: I might need to change this to SUM_CUM?
IND = CM2$HOOF[(1940-1917 +1):length(CM2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year

CMScenarios<- ScenarioS_F(Area, Regime, IND, Density = 0.06)


# THIRD FUNCTION: this function adds environmental stochasticity to the simulation by CMpeating it 300 times
Area = CM2$AREA[1]/100 # enter herd aCMa size as one number in km^2
Regime = CM2$PROP_BURN[(1940-1917+1):length(CM2$PROP_BURN)-1] # enter the mean of the fiCM CMgime (from Table 1, or above aCMa specific code (i.e. CM_FiCM))
IND = CM2$HOOF[(1940-1917 +1):length(CM2$HOOF)-1] # enter the industrial disturbance on a yearly basis from 1940 onwards, but not the last year

CMruns<-MCRUNS_F(Area, Regime, IND)

#plot
pdf("CM_RS.pdf", width = 5, height = 4)
pLambda(CMruns, "CM")
dev.off()

# Performing experiments ----
# EXPERIMENT 1: RE  - done above
# EXPERIMENT 2: LD - change carrying capacity from 0.03 to 0.02
K = (CM2$AREA[1]/100)*0.04
Pop <- c(K*0.5, K*0.5*Rec_CM) # adult females, and juvenile females
CMCaribou_LD<-Caribou_F(K, burn, hoof, Pop, adult = SadF, fecun = Rec) 
CMScenarioS_LD<- ScenarioS_F(Area, Regime, IND, Density = 0.04)
CMruns_LD<-MCRUNS_F(Area, Regime, IND, Density = 0.04)
pdf("CM_LD.pdf", height = 5, width  = 4)
pLambda(CMruns_LD, "CM_LD")
dev.off()
# EXPERIMENT 3: HF - increase fire burn rate to 0.01 (ScenarioS and MCRUNS)
K = (CM2$AREA[1]/100)*0.06# carrying capacity is 0.03 feamles/km^2
Pop <- c(K*0.5, K*0.5*Rec_CM) # adult females, and juvenile females
CMCaribou_HF<-Caribou_F(K, burn, hoof, Pop, adult = SadF, fecun = Rec) 
Regime = rnorm(length(CM_Fire), mean = 0.01, sd = CM_Fire_sd)
CMScenarioS_HF<- ScenarioS_F(Area, Regime, IND, Density = 0.06)
CMruns_HF<-MCRUNS_F(Area, Regime, IND, Density = 0.06)
pdf("CM_HF.pdf", height = 5, width  = 4)
pLambda(CMruns_HF, "CM_HF")
dev.off()

# Table 2 ----
# calculate extinctions: ----
# RS
# YCRIT date - date that lambda falls below 1.0
2057 - length(CMScenarios$Lambda[CMScenarios$Lambda < 1.0])
# year of extinction - where there are less than 10 females
2057 - length(CMScenarios$Nt[CMScenarios$Nt == "EXTINCT"]) #2012?
CMScenarios$Nt[length(CMScenarios$Nt)-40]
CMScenarios$Lambda[length(CMScenarios$Lambda)-40]
# LD
2057 - length(CMScenarioS_LD$Lambda[CMScenarioS_LD$Lambda < 1.0]) 
2057 - length(CMScenarioS_LD$Nt[CMScenarioS_LD$Nt == "EXTINCT"]) 
CMScenarioS_LD$Nt[length(CMScenarioS_LD$Nt)-40]
CMScenarioS_LD$Lambda[length(CMScenarioS_LD$Lambda)-40]
#HF
2057 - length(CMScenarioS_HF$Lambda[CMScenarioS_HF$Lambda < 1.0]) 
2057 - length(CMScenarioS_HF$Nt[CMScenarioS_HF$Nt == "EXTINCT"])
CMScenarioS_HF$Nt[length(CMScenarioS_HF$Nt)-40]
CMScenarioS_HF$Lambda[length(CMScenarioS_HF$Lambda)-40]

# Table 3 ---
# is lambda correlated to either burn or industrial development? years 1940 through 2007
cor.test(CM2$BURN_INC[(1940-1917+1):length(CM2$SUM_CUM)-1], CMScenarios$Lambda[102:170]) # fire regime
cor.test(CM2$CUM_WELL[(1940-1917+1):length(CM2$SUM_CUM)-1], CMScenarios$Lambda[102:170]) # disturbance regime
cor.test(CM2$SUM_CUM[(1940-1917+1):length(CM2$SUM_CUM)-1], CMScenarios$Lambda[102:170]) # cummulative burns in 50 yr increments

# Create a file of all LS graphs ----
pdf("CM graphs.pdf", height = 4, width = 5, onefile = TRUE)
Fires(CM2, "CM Annual Proportion Area Burned")
pHoof(CM2, "CM Cummulative Intustrial Footprint")
pLambda(CMruns, "CM_RS")
pLambda(CMruns_LD, "CM_LD")
pLambda(CMruns_HF, "CM_HF")
dev.off()

#####################################################################################################################################
# FIGURES ----
# TODO: 
# Figure 2
# make a multi-pannel plot all the simulations as a central figure of this manuscript.
# "Lambda" on the y-axis
# "Year" on the x-axis
## tick marks on left most y-axies
## tick marcks on bottom most x-axies
### header: "RS", "LD", "HF"
# order of PDF figures
# WSA_RS  WSA_LD  WSA_HF
# LS_RS   LS_LD   LS_HF
# CL_RS   CL_LD   CL_HF
# RE_RS   RE_LD   RE_HF
# CM_RS   CM_LD   CM_HF

