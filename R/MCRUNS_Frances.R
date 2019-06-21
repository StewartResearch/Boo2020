MCRUNS_F<-function (Area, Regime, IND, CF = 1, Density, #mmp = 5, 
                    Reps = 300) 
  # not sure what mmp was needed for - it is never called
{
  NF <- NULL
  Lambda <- NULL
  pYoung <- NULL
  foot <- NULL
  Nt <- NULL
  for (i in 1:Reps) {
    foo <- ScenarioS_F(Area, Regime, IND, CF, Density #mmp
                     )
    NF <- rbind(NF, foo$NF)
    Lambda <- rbind(Lambda, foo$Lambda)
    pYoung <- rbind(pYoung, foo$pYoung)
    foot <- rbind(foot, foo$foot)
    Nt <- rbind(Nt, foo$Nt)
  }
  list(NF = NF, Lambda = Lambda, pYoung = pYoung, foot = foot, Nt = Nt
       )
}
