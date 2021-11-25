

rm(list = ls())


#	install.packages("pdynmc")
library(pdynmc)
#	install.packages("readstata13")
library(readstata13)



dat		<- read.dta13(
  file			= "D:/Work/20_Projekte/50_Linear-Dynamic-Panel-Models/50_Drafts/20_Paper/51_Educational/R/cig85_95.dta",
  convert.factors		= FALSE,	# changed from TRUE
  generate.factors	= FALSE,
  encoding			= "UTF-8",
  fromEncoding		= NULL,
  convert.underscore	= FALSE,
  missing.type		= FALSE,
  convert.dates		= TRUE,
  replace.strl		= TRUE,
  add.rownames		= FALSE,
  nonint.factors		= FALSE,
  select.rows		= NULL,
  select.cols		= NULL,
  strlexport		= FALSE,
  strlpath			= "."
)




dat$y			<- log(dat$packpc)				# response variable log(Q^Cigarettes)
dat$x			<- log(dat$avgprs/dat$cpi)			# endogenous variable log(P^Cigarettes)
dat$z1		<- (dat$taxs - dat$tax)/dat$cpi		# instrument 1 SalesTax
dat$z2		<- dat$tax/dat$cpi				# instrument 2 CigTax
dat$w			<- log((dat$income/dat$pop)/dat$cpi)	# exogenous variable log(rincome)






dat = dat
varname.i = "state"
varname.t = "year"

use.mc.diff = TRUE
use.mc.lev = FALSE
use.mc.nonlin = FALSE
use.mc.nonlinAS = NULL
inst.stata = FALSE

include.y = FALSE
varname.y = "y"
lagTerms.y = 1
maxLags.y = 4

include.x = TRUE
varname.reg.end = NULL
lagTerms.reg.end = NULL
maxLags.reg.end = NULL
varname.reg.pre = NULL
lagTerms.reg.pre = NULL
maxLags.reg.pre = NULL
varname.reg.ex = "z1"
lagTerms.reg.ex = 0
maxLags.reg.ex = 2
include.x.instr = TRUE
varname.reg.instr = "z1"
inst.reg.ex.expand = FALSE
include.x.toInstr = TRUE
varname.reg.toInstr = "x"

fur.con = TRUE
fur.con.diff = TRUE
fur.con.lev = FALSE
varname.reg.fur = "w"
lagTerms.reg.fur = 0

include.dum = FALSE
dum.diff = NULL
dum.lev = NULL
varname.dum = NULL

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





