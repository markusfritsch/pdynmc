

rm(list = ls())

#	install.packages("pdynmc")
require(pdynmc)
#require(haven)
#install.packages("data.table")
library(data.table)
#install.packages("foreign")
library(foreign)


#	setwd(dir = "E:/Work/20_Projekte/50_Linear-Dynamic-Panel-Models/50_Drafts/20_Paper/50_Revisiting-habits-and-heterogeneity-in-demands")
#	setwd(dir = "D:/Markus/Work/20_Projekte/50_Linear-Dynamic-Panel-Models/50_Drafts/20_Paper/50_Revisiting-habits-and-heterogeneity-in-demands")
setwd(dir = "D:/Work/20_Projekte/50_Linear-Dynamic-Panel-Models/50_Drafts/20_Paper/50_Revisiting-habits-and-heterogeneity-in-demands")



# data preparation Browning Collado (2007, JAE)







###	Load data

#	filename <- "bc2.dta"
filename <- "bc2small.dta"

dat <- read.dta(file = filename, convert.dates = TRUE, convert.factors = TRUE,
                missing.type = FALSE,
                convert.underscore = FALSE, warn.missing.labels = TRUE)

dat$yearquarter  <- as.character(dat$yearquarter)
dat$yearquarterA <- as.character(dat$yearquarterA)

#	dat2 <- dat[1:198,]
#	write.dta(dat2, file = "bc2small.dta")



#dat <- read_dta("Documents/BC-replication/bc2.dta")

#dat$rxtot		<- dat$xtot/dat$spi		# real (i.e., deflated) expenditures
#dat$lrxtot		<- log(dat$rxtot)			# log of real expenditures
#dat$lrxtot2		<- dat$lrxtot^2			# squared log real expenditures
#dat$rhearn		<- dat$hearn/dat$spi		# real (i.e., deflated) earnings
#dat$lrhearn		<- log(dat$rhearn)		# log of real earnings
#dat$lrhearn2	<- dat$lrhearn^2			# squared log of real earnings
#dat$hage2		<- dat$hage^2

#dat$yearquarter	<- paste(dat$year, dat$quarter, sep = "_")	# year and quarter (for dummies)
#dat$yearquarterA	<- dat$yearquarter	#	adjusted variable due to redundance
#dat$yearquarterA[dat$yearquarterA %in% c("1996_1", "1996_2", "1996_3", "1996_4")]	<- "1996_1234"
dat	<- dat[order(dat$i, dat$t, decreasing = FALSE), ]

dat$foodin1 	<- dat$foodin*100
dat$nds1 		<- dat$nds*100
dat$foodout1	<- dat$foodout*100
dat$alct1 		<- dat$alct*100
dat$clo1 		<- dat$clo*100
dat$sdur1 		<- dat$sdur*100






dat = dat
#dat = ds[[1]]
varname.i = "i"
varname.t = "t"
use.mc.diff = TRUE
use.mc.lev = FALSE
use.mc.nonlin = FALSE
use.mc.nonlinAS			= NULL
inst.stata			= FALSE
include.y = FALSE
varname.y = "foodin1"
lagTerms.y = 1
maxLags.y				= NULL

include.x				= TRUE
varname.reg.end			= "lrxtot"
lagTerms.reg.end		= 0
maxLags.reg.end			= 5
varname.reg.pre			= NULL
lagTerms.reg.pre		= NULL
maxLags.reg.pre			= NULL
varname.reg.ex			= "lrhearn"
lagTerms.reg.ex			= 0
maxLags.reg.ex			= 6

include.x.instr			= TRUE
varname.reg.instr		= "lrhearn"
inst.reg.ex.expand  = FALSE
include.x.toInstr		= FALSE
varname.reg.toInstr	= NULL

fur.con = FALSE
fur.con.diff = FALSE
fur.con.lev = FALSE
varname.reg.fur = NULL
lagTerms.reg.fur = NULL
include.dum = FALSE
dum.diff = FALSE
dum.lev = FALSE
varname.dum = NULL

col_tol				= 0.65
w.mat = "iid.err"
w.mat.stata			= FALSE
std.err = "corrected"
estimation = "twostep"
max.iter				= 100
iter.tol				= 0.01
inst.thresh			= NULL

opt.meth = "none"
hessian				= FALSE
optCtrl				= list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol = .Machine$double.eps^(1/3),
                   starttests = TRUE, dowarn = TRUE, badval = (0.25)*.Machine$double.xmax, usenumDeriv = FALSE,
                   reltol = 1e-12, maxit = 200, trace = TRUE,
                   follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e7, pgtol = 0, all.methods = FALSE)

custom.start.val		= FALSE
start.val				= NULL
start.val.lo			= -1
start.val.up			= 1
seed.input			= 42




