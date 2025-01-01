

rm(list = ls())


#	install.packages("pder")
library(pder)


data("DemocracyIncome", package = "pder")
data("DemocracyIncome25", package = "pder")


dat	<- DemocracyIncome
#	dat	<- DemocracyIncome25

rm(DemocracyIncome, DemocracyIncome25)







dat = dat
varname.i = "country"
varname.t = "year"

use.mc.diff = TRUE
use.mc.lev = FALSE
use.mc.nonlin = FALSE
use.mc.nonlinAS = NULL
inst.stata = FALSE

include.y = TRUE
varname.y = "democracy"
lagTerms.y = 1
maxLags.y = NULL

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

fur.con = FALSE
fur.con.diff = FALSE
fur.con.lev = FALSE
varname.reg.fur = NULL
lagTerms.reg.fur = NULL

include.dum = TRUE
dum.diff = FALSE
dum.lev = TRUE
varname.dum = "year"

col_tol = 0.65
w.mat = "iid.err"
w.mat.stata = FALSE

std.err = "corrected"
estimation = "twostep"
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





