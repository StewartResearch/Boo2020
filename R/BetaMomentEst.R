function (x) 
{
  xbar <- mean(x)
  v <- var(x)
  tmp <- (xbar * (1 - xbar)/v - 1)
  alpha <- xbar * tmp
  beta <- (1 - xbar) * tmp
  list(alpha = alpha, beta = beta)
}
