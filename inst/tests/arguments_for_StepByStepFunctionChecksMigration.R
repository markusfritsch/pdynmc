

rm(list = ls())


setwd("D:/Work/20_Projekte/450_Migration/migFlowsAndDrivers/data")

dat	<- readRDS(file = "../data/migData.rds")
dat	<- dat[!is.na(dat$prcp), ]
#dat <- NULL

varname.i = "orig_dest"
#varname.i = NULL
varname.t = "year0"
#varname.t = NULL

use.mc.diff         = TRUE
use.mc.lev          = FALSE
use.mc.nonlin       = FALSE
use.mc.nonlinAS			= NULL
inst.collapse       = TRUE
inst.stata          = FALSE


include.y       = TRUE
varname.y				= "lflow"
lagTerms.y			= 1
maxLags.y				= NULL

include.x				= TRUE
varname.reg.end			= NULL
lagTerms.reg.end		= NULL
maxLags.reg.end			= NULL
varname.reg.pre			= NULL
lagTerms.reg.pre		= NULL
maxLags.reg.pre			= NULL
varname.reg.ex			= NULL
lagTerms.reg.ex			= NULL
maxLags.reg.ex			= NULL
inst.reg.ex.expand  = TRUE

include.x.instr			= FALSE
varname.reg.instr		= NULL
include.x.toInstr		= FALSE
varname.reg.toInstr	= NULL

fur.con				= FALSE
fur.con.diff			= NULL
fur.con.lev			  = NULL
varname.reg.fur		= NULL
lagTerms.reg.fur	= NULL

include.dum			= TRUE
dum.diff				= FALSE
dum.lev				  = TRUE
#varname.dum			= NULL
#varname.dum			= c("year0")
varname.dum			= c("dest_year", "year0")

col_tol				= 0.65

w.mat				= "iid.err"
w.mat.stata			= FALSE

std.err				= "corrected"

estimation = "iterative"
max.iter				= 100
iter.tol				= 0.01
inst.thresh			= NULL
opt.meth = "none"

hessian				= FALSE
optCtrl				= list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol = .Machine$double.eps^(1/3),
                   starttests = TRUE, dowarn = TRUE, badval = (0.25)*.Machine$double.xmax, usenumDeriv = FALSE,
                   reltol = 1e-12, maxit = 200, trace = TRUE,
                   follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e7, pgtol = 0, all.methods = FALSE)
# ,nmulti				= 1
custom.start.val		= FALSE
start.val				= NULL
start.val.lo			= -1
start.val.up			= 1
seed.input			= 42




