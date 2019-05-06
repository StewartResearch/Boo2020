function (Regime, N = 400, T = 200, AgeStruct = NULL, SimFire = TRUE) 
{
  AgeShift <- function(x) {
    n <- length(x)
    tmp <- c(0, x[1:(n - 1)])
    tmp[n] <- tmp[n] + x[n]
    tmp
  }
  ExpAgeStruct <- function(T, p) {
    foo <- pexp(0:(T - 1), p, lower.tail = F)
    bar <- c(foo[2:length(foo)], 0)
    foo - bar
  }
  ForestModel <- function(Forest, BurnProp) {
    b <- Forest * BurnProp
    a0 <- Forest - b
    NewForest <- AgeShift(a0)
    NewForest[1] <- sum(b)
    NewForest
  }
  p50 <- function(Forest) {
    sum(Forest[1:50])/sum(Forest)
  }
  if (is.null(AgeStruct)) {
    AgeStruct <- ExpAgeStruct(T, mean(Regime))
  }
  if (SimFire) {
    pars <- BetaMomentEst(Regime)
    Burns <- rbeta(N, pars$alpha, pars$beta)
  }
  else {
    Burns <- Regime
    N <- length(Regime)
  }
  Young <- rep(0, N)
  i <- 1
  while (i <= N) {
    AgeStruct <- ForestModel(AgeStruct, Burns[i])
    Young[i] <- p50(AgeStruct)
    i <- i + 1
  }
  list(Young = Young, EndForest = AgeStruct)
}
