

rm(list = ls())


#	install.packages("pdynmc")
library(pdynmc)
#	install.packages("plm")
library(plm)


data(EmplUK, package = "plm")
dat <- EmplUK
dat[,c(4:7)] <- log(dat[,c(4:7)])



dat = dat
varname.i = "firm"
varname.t = "year"

use.mc.diff = TRUE
use.mc.lev = FALSE
use.mc.nonlin = TRUE
use.mc.nonlinAS = NULL
inst.stata = FALSE

include.y = TRUE
varname.y = "emp"
lagTerms.y = 2
maxLags.y = 4

include.x = FALSE
varname.reg.end = NULL
lagTerms.reg.end = NULL
maxLags.reg.end = NULL
varname.reg.pre = NULL
lagTerms.reg.pre = NULL
maxLags.reg.pre = NULL
varname.reg.ex = NULL
lagTerms.reg.ex = NULL
maxLags.reg.ex = NULL
include.x.instr = FALSE
varname.reg.instr = NULL
inst.reg.ex.expand = TRUE
include.x.toInstr = FALSE
varname.reg.toInstr = NULL

fur.con = TRUE
fur.con.diff = FALSE
fur.con.lev = TRUE
varname.reg.fur = c("wage", "capital", "output")
lagTerms.reg.fur = c(1,2,2)

include.dum = TRUE
dum.diff = TRUE
dum.lev = FALSE
varname.dum = "year"

col_tol = 0.65
w.mat = "iid.err"
w.mat.stata = FALSE

std.err = "corrected"
estimation = "twostep"
max.iter = 4
iter.tol = 0.01
inst.thresh = NULL
opt.meth = "BFGS"
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





