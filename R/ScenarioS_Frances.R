ScenarioS_F<-function (Area, Regime, IND, CF = 1, Density = 0.06) 
{
  
  InitPop <- function(K) {
    aoy = round(K * 0.6769521) #
    yoy = K - aoy
    matrix(c(aoy, yoy), nrow = 2, ncol = 1, byrow = TRUE)
  }
  K <- round(Area * Density * 0.5)[1]
  P0 <- InitPop(K)
  print(P0)
  tmp <- FireModelS(Regime = Regime * CF, N = 400, T = 200, 
                    AgeStruct = NULL, SimFire = TRUE) 
  pYoung1 <- tmp$Young
  AS1 <- tmp$EndForest
  fp1 <- rep(0, length(pYoung1))
  Stage1 <- Caribou(K = K, p50s = pYoung1, hoof = fp1, Pop = P0)
  print(paste("EndPop1 = ", Stage1$EndPop, sep = ""))
  tmp2 <- FireModelS(Regime = Regime, N = 400, T = 200, AgeStruct = AS1, 
                     SimFire = FALSE)
  pYoung2 <- tmp2$Young
  AS2 <- tmp2$EndForest
  fp2 <- IND
  Stage2 <- Caribou(K = K, p50s = pYoung2, hoof = fp2, Pop = Stage1$EndPop)
  print(paste("EndPop2 = ", Stage2$EndPop, sep = ""))
  CurRegime <- rev(Regime)[1:length(Regime)] # TODO: take the last 50 years and use that. To revisit with Steve.
  tmp3 <- FireModelS(CurRegime, N = 50, T = 200, AgeStruct = AS2, 
                     SimFire = TRUE)
  pYoung3 <- tmp3$Young
  fp3 <- rep(tail(IND, 1), length(pYoung3))
  n <- length(pYoung2)
  Stage3 <- Caribou(K = K, p50s = pYoung3, hoof = fp3, Pop = Stage2$EndPop)
  print(paste("EndPop3 = ", Stage3$EndPop, sep = ""))
  foo <- c(Stage1$Lambda, Stage2$Lambda, Stage3$Lambda)
  plot(foo, type = "l", ylab = "Lambda", xlab = "Year")
  print(paste("K = ", K, sep = ""))
  NAFs <- c(Stage1$NAF[300:400], Stage2$NAF, Stage3$NAF)
  NYFs <- c(Stage1$NYF[300:400], Stage2$NYF, Stage3$NAF)
  Nt <- c(Stage1$Size[300:400], Stage2$Size, Stage3$Size)
  Juv_survival <- c(Stage1$JuvS[300:400], Stage2$JuvS, Stage3$JuvS)
  Fecundity <- c(Stage1$Fecundity[300:400], Stage2$Fecundity, 
                 Stage3$Fecundity)
  Lambdas <- c(Stage1$Lambda[300:400], Stage2$Lambda, Stage3$Lambda)
  pYoungs <- c(pYoung1[300:400], pYoung2, pYoung3)
  foots <- c(fp1[300:400], fp2, fp3)
  BioLambda <- c(Stage1$BioLambda[300:400], Stage2$Biolambda, 
                 Stage3$BioLambda)
  list(NAF = NAFs, Lambda = Lambdas, pYoung = pYoungs, foot = foots, 
       NYF = NYFs, Nt = Nt, Juv_survival = Juv_survival, Fecundity = Fecundity, 
       BioLambda = BioLambda)
}
