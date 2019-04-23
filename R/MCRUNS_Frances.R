MCRUNS_F<-function (Area, Regime, IND = 50, CF = 1, Density = 0.04, #mmp = 5, 
                    Reps = 300) 
  # TODO: Ask Steve: IS MMP a pseudo extinction threshold?
  # removed for now
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
