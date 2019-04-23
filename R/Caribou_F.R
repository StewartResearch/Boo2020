Caribou_F<-function (K, p50s, hoof, Pop) # TODO: why is K at 500? 
  # p50s is from AREA$all.data$PROP_BURN
  # hoof is from AREA$All.data$HOOF
  # Pop is a vector of length 2, indicating how many juveniles and how many adults there are in the population
  ## e.g. Pop <- Pop<-c(0, 902)
  # only run code from 1940s onwards, even thought we might have some data (mostly zeros) from before that point

{
  adult <- 0.83 # we could change this to be herd specific from the Caribou committee data
  fecun <- 0.354825 # same here.
  DensDep <- function(N = size, K = K, adult) {
    tmpf <- adult * exp(-1 * (N/K)^4)
  }
  sigj <- function(Lambda, fecun, adult) {
    jsurv <- ((2 * Lambda - adult)^2 - adult^2)/(4 * fecun)
    jsurv1 <- ifelse(jsurv > 1, 0.99, jsurv)
    jsurv2 <- ifelse(jsurv1 <= 0, 0, jsurv1)
  }
  Demog <- function(P, V = vital[[1]], S = TRUE) {
     if (S){
      tmp1 <- round(P[2] * as.numeric(V[1, 2]))
      tmp2 <- ifelse(P[1] == 0, 0, rbinom(1, P[1], ifelse(V[2,
                                                            1] > 1, 1, V[2, 1]))) + ifelse(P[2] == 0, 0,
                                                                                           rbinom(1, P[2], V[2, 2]))
      matrix(c(tmp1, tmp2), nrow = 2, ncol = 1, byrow = TRUE)
     }
    else { 
      V %*% P
      }
    
  }
  N <- length(p50s)
  stopifnot(N == length(hoof))
  NAF <- vector("numeric", N)
  NYF <- vector("numeric", N)
  Lambda <- vector("numeric", N)
  size <- vector("numeric", N)
  vital <- list()
  run <- list()
  juv <- vector("numeric", N)
  r <- list()
  AdultFecundity <- vector("numeric", N)
  eig <- list()
  dom.eig <- list()
  Lambda[1] <- 1.191 - (0.314 * hoof[1]) - (0.29 * p50s[1])
  size[1] <- sum(Pop)
  NAF[1] <- Pop[2]
  NYF[1] <- Pop[1]
  r[1] <- NYF[1]/NAF[1]
  juvs <- sigj(Lambda[1], fecun, adult)
  run[[1]] <- Pop
  juv[[1]] <- juvs
  vital[[1]] <- matrix(c(0, fecun, juv[[1]], adult), nrow = 2, 
                       ncol = 2, byrow = TRUE)
  eig[[1]] <- eigen(vital[[1]])
  dom.eig[[1]] <- eig[[1]]$values[1]
  AdultFecundity[[1]] <- fecun
  for (year in 2:N) {
    run[[year]] <- Demog(run[[year - 1]], vital[[year - 
                                                   1]], S = FALSE)
    size[year] <- ifelse(sum(run[[year]]) > 50, sum(run[[year]]), 
                         "EXTINCT")
    NAF[year] <- run[[year]][2]
    NYF[year] <- run[[year]][1]
    r[year] <- NYF[year]/NAF[year]
    Lambda[year] <- 1.191 - 0.314 * hoof[[year]] - 0.29 * 
      p50s[[year]]
    AdultFecundity[[year]] <- DensDep(N = sum(run[[year]]), 
                                      K = K, adult)
    juv[[year]] <- sigj(Lambda[year], AdultFecundity[[year]], 
                        adult)
    vital[[year]] <- matrix(c(0, AdultFecundity[[year]], 
                              juv[[year]], adult), nrow = 2, ncol = 2, byrow = TRUE)
    eig[[year]] <- eigen(vital[[year]])
    dom.eig[[year]] <- ifelse(Lambda[year] == eig[[year]]$values[1], 
                              eig[[year]]$values[1], "Pop at K or out of bounds (Lambda < 0.83)")
  }
  list(Lambda = Lambda, Size = size, BioLambda = dom.eig, 
       EndPop = run[[N]], NAF = NAF, NYF = NYF, JuvS = juv, 
       calf_cow_ratio = r, fecun = AdultFecundity)
}
