





rm(list = ls())



#	install.packages("plm")
library(plm)
#	install.packages("Matrix")
library(Matrix)
#	install.packages("optimx")
library(optimx)




#	setwd(dir = "E:/Work/20_Projekte/50_Linear-Dynamic-Panel-Models/50_Drafts/20_Paper/50_Revisiting-habits-and-heterogeneity-in-demands")
	setwd(dir = "D:/Work/20_Projekte/50_Linear-Dynamic-Panel-Models/50_Drafts/20_Paper/50_Revisiting-habits-and-heterogeneity-in-demands")








# data preparation Browning Collado (2007, JAE)




#install.packages("data.table")
library(data.table)
#install.packages("foreign")
library(foreign)


###	Load data


dat <- read.dta(file = "bc2.dta", convert.dates = TRUE, convert.factors = TRUE,
         missing.type = FALSE,
         convert.underscore = FALSE, warn.missing.labels = TRUE)

dat$yearquarter  <- as.character(dat$yearquarter)
dat$yearquarterA <- as.character(dat$yearquarterA)








dat         <- dat
varname.i		<- "i"
varname.t		<- "t"

use.mc.diff      <- TRUE
use.mc.lev       <- TRUE
use.mc.nonlin    <- FALSE
use.mc.nonlinAS	 <- NULL

inst.stata			= FALSE

include.y   <- FALSE
varname.y		<- "foodin"
lagTerms.y  <- 1
maxLags.y		<- NULL

include.x				  <- TRUE
varname.reg.end	  <-	"lrxtot"
lagTerms.reg.end	<- 0
maxLags.reg.end	  <- 5
varname.reg.pre	  <- NULL
lagTerms.reg.pre	<- NULL
maxLags.reg.pre	  <- NULL
varname.reg.ex		<- "lrhearn"
lagTerms.reg.ex		<- 0
maxLags.reg.ex		<- 4

include.x.instr			<- TRUE
varname.reg.instr		<- "lrhearn"
include.x.toInstr		<- FALSE
varname.reg.toInstr	<- NULL

fur.con				= TRUE
fur.con.diff			= TRUE
fur.con.lev			= TRUE
varname.reg.fur			= c("nch","nad","hage","hage2")
lagTerms.reg.fur		= c(0,0,0,0)

include.dum			= TRUE
dum.diff				= FALSE
dum.lev				= TRUE
varname.dum			<- c("week","yearquarter")
# ,custom.dum			= NULL
# ,partOut				= FALSE
# ,estimate.int			= FALSE

col_tol				= 0.65

w.mat				= "iid.err"
w.mat.stata			= FALSE

std.err				= "corrected"

estimation			= "twostep"
max.iter				= 100
iter.tol				= 0.01
inst.thresh			= NULL
opt.meth				= "none"
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




dat.part <- dat[1:72,]
dat.full <- dat


# dat	<- dat.part
 dat	<- dat.full



