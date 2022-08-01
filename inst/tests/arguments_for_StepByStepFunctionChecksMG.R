

rm(list = ls())

options(digits=3)
library(foreign)
library(readstata13)



# load dataset
data <- read.dta13("D:/Work/20_Projekte/50_Linear-Dynamic-Panel-Models/50_Drafts/20_Paper/55_metaScienceProject/data/compiled.dta")
epa <- read.dta13("D:/Work/20_Projekte/50_Linear-Dynamic-Panel-Models/50_Drafts/20_Paper/55_metaScienceProject/data/epa.dta")
## Merge epa data with compiled data
data  <- merge(data, epa, by=c("State", "year"))
dat2  <- data[which(data$year<14),]
# T = 7; N = 50


## Log transform continuous variables
data[c("epa", "wrkhrs")] <- log(data[c("epa", "wrkhrs")])




 dat 				= dat2
 varname.i 			= "State"
 varname.t 			= "year"
 use.mc.diff 		= TRUE
 use.mc.lev 		= FALSE
 use.mc.nonlin 		= FALSE
 use.mc.nonlinAS		= NULL
 inst.stata			= FALSE
 include.y 			= FALSE
 varname.y 			= "epa"
 lagTerms.y 		= 1
 maxLags.y			= NULL

 include.x			= TRUE
 varname.reg.end = "wrkhrs"
 lagTerms.reg.end		= 0
 maxLags.reg.end		= 2
 varname.reg.pre		= NULL
 lagTerms.reg.pre		= NULL
 maxLags.reg.pre		= NULL
 varname.reg.ex		= NULL
 lagTerms.reg.ex		= NULL
 maxLags.reg.ex		= NULL
 inst.reg.ex.expand	= TRUE

 include.x.instr		= FALSE
 varname.reg.instr	= NULL
 include.x.toInstr	= FALSE
 varname.reg.toInstr	= NULL

 fur.con 			= FALSE
 fur.con.diff 		= NULL
 fur.con.lev 		= NULL
 varname.reg.fur 		= NULL
 lagTerms.reg.fur 	= NULL
 include.dum 		= TRUE
 dum.diff 			= TRUE
 dum.lev 			= FALSE
 varname.dum 		= "year"

  col_tol			= 0.65
 w.mat 			= "iid.err"
 w.mat.stata		= FALSE
 std.err 			= "corrected"
 estimation 		= "twostep"
 max.iter			= 100
 iter.tol			= 0.01
 inst.thresh		= NULL

 opt.meth 			= "none"
 hessian			= FALSE
 optCtrl			= list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol = .Machine$double.eps^(1/3),
                    starttests = TRUE, dowarn = TRUE, badval = (0.25)*.Machine$double.xmax, usenumDeriv = FALSE,
                    reltol = 1e-12, maxit = 200, trace = TRUE,
                    follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e7, pgtol = 0, all.methods = FALSE)

 custom.start.val		= FALSE
 start.val			= NULL
 start.val.lo		= -1
 start.val.up		= 1
 seed.input			= 42




if(FALSE){

 model22 <- pdynmc(
   dat2,
   varname.i = "State",
   varname.t = "year",
   use.mc.diff = TRUE,
   use.mc.lev = FALSE,
   use.mc.nonlin = FALSE,
   include.y = FALSE,
   varname.y = "epa",
   lagTerms.y = 1,
   include.x = TRUE,
   varname.reg.end = "wrkhrs",
   lagTerms.reg.end = 0,
   maxLags.reg.end = 2,
   include.dum = TRUE,
   dum.diff = TRUE,
   dum.lev = FALSE,
   varname.dum = "year",
   col_tol = 0.65,
   w.mat = "iid.err",
   w.mat.stata = FALSE,
   max.iter = 10,
   std.err = "corrected",
   estimation = "iterative"
 )

}
