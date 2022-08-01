

rm(list = ls())


#	install.packages("pdynmc")
library(pdynmc)


data(ABdata, package = "pdynmc")
dat <- ABdata
dat[,c(4:7)] <- log(dat[,c(4:7)])
colnames(dat) <- c("id", "year", "sector", "n", "w", "k", "ys")


dat = dat
varname.i = "id"
varname.t = "year"

use.mc.diff = TRUE
use.mc.lev = TRUE
use.mc.nonlin = FALSE
use.mc.nonlinAS = NULL
inst.stata = FALSE

include.y = FALSE
varname.y = "n"
lagTerms.y = 0
maxLags.y = NULL

include.x = TRUE
varname.reg.end = NULL
lagTerms.reg.end = NULL
maxLags.reg.end = NULL
varname.reg.pre = "w"
lagTerms.reg.pre = 0
maxLags.reg.pre = 3
varname.reg.ex = NULL
lagTerms.reg.ex = NULL
maxLags.reg.ex = NULL
include.x.instr = FALSE
varname.reg.instr = NULL
inst.reg.ex.expand = TRUE
include.x.toInstr = FALSE
varname.reg.toInstr = NULL

fur.con = TRUE
fur.con.diff = TRUE
fur.con.lev = TRUE
varname.reg.fur = "k"
lagTerms.reg.fur = 0

include.dum = FALSE
dum.diff = FALSE
dum.lev = FALSE
varname.dum = NULL

col_tol = 0.65
w.mat = "iid.err"
w.mat.stata = FALSE

std.err = "corrected"
estimation = "onestep"
max.iter = 4
iter.tol = 0.01
inst.thresh = NULL
opt.meth = "none"
hessian = FALSE
optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
    .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
    .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = TRUE,
    follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
    all.methods = FALSE)
custom.start.val = FALSE
start.val = NULL
start.val.lo = -1
start.val.up = 1
seed.input = 2





