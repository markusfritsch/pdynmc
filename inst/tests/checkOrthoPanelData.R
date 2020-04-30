
rm(list = ls())

# Monte Carlo Simulation in OrthoPanels page 66
# Code copied from Appendix 2 page 72
rho <- .5
beta <- .5
sig2 <- 1
set.seed(321)
generate <- function(N, T, rho, beta, sig2) {
  LT <- T + 50
  f <- runif(N, -1, 1)
  x <- array(.75 * f, dim = c(N, LT)) + rnorm(N * LT, sd = 4)
  y <- matrix(0, N, LT)
  for (t in 1:LT) {
    yy <- if (t > 1)
      y[, t - 1]
    else
      ((f + beta * .75 * f)/(1 - rho))
    y[, t] <- rho * yy + f + x[, t] * beta +
      rnorm(N, sd = sqrt(sig2))
  }
  data.frame(i = rep(seq(N), LT - 50),
             t = rep(seq(LT - 50), each = N),
             x1 = c(x[(50 * N + 1):(LT * N)]),
             y = c(y[(50 * N + 1):(LT * N)]))
}
N <- 1000
T <- 4
reps <- 200
ds <- replicate(n = reps,
                generate(N = N, T = T, rho = rho, beta = beta, sig2 = sig2),
                simplify = FALSE)






m1 <- pdynmc(dat = ds[[1]], varname.i = "i", varname.t = "t",
       use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
       include.y = TRUE, varname.y = "y", lagTerms.y = 1,
       fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
       varname.reg.fur = c("x1"), lagTerms.reg.fur =0,
       include.dum = TRUE,  dum.diff = TRUE, dum.lev = FALSE, varname.dum = "t",
       w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
       opt.meth = "BFGS")















library(OrthoPanels)

data(BES_panel, package = "OrthoPanels")


BES.ab.model <- pdynmc(dat = BES_panel, varname.i = "n", varname.t = "t",
                       use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
                       include.y = TRUE, varname.y = "Approve", lagTerms.y = 1,
                       fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
                       varname.reg.fur = c("Econ", "Clegg", "Brown", "Cameron",  "NHS", "Terror", "PID", "Tax"), lagTerms.reg.fur =c(0,0,0,0,0,0,0,0),
                       include.dum = FALSE,  dum.diff = E, dum.lev = FALSE, varname.dum = "t",
                       w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
                       opt.meth = "BFGS")




