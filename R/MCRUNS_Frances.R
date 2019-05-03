MCRUNS_F<-function (Area, Regime, IND, CF = 1, Density = 0.03, #mmp = 5, 
                    Reps = 300) 
  # not sure what mmp was needed for - it is never called
{
  NF <- NULL
  Lambda <- NULL
  pYoung <- NULL
  foot <- NULL
  for (i in 1:Reps) {
    foo <- ScenarioS(Area, Regime, IND, CF, Density #mmp
                     )
    NF <- rbind(NF, foo$NF)
    Lambda <- rbind(Lambda, foo$Lambda)
    pYoung <- rbind(pYoung, foo$pYoung)
    foot <- rbind(foot, foo$foot)
  }
  list(NF = NF, Lambda = Lambda, pYoung = pYoung, foot = foot)
}
