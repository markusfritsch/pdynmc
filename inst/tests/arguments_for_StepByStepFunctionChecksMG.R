

rm(list = ls())




data(ABdata, package = "pdynmc")

dati			<- ABdata
dati			<- cbind(dati[, 1:3], dati[, 4:7])
names(dati)		<- c("ireg", "t", "sector", "logy", "Sly", "mean_eta", "output")
data.info(dati, i.name = "ireg", t.name = "t")
# T = 9; N = 140




 dat 				= dati
 varname.i 			= "ireg"
 varname.t 			= "t"
 use.mc.diff 		= TRUE
 use.mc.lev 		= TRUE
 use.mc.nonlin 		= FALSE
 use.mc.nonlinAS		= NULL
 inst.stata			= FALSE
 include.y 			= TRUE
 varname.y 			= "logy"
 lagTerms.y 		= 1
 maxLags.y			= NULL

 include.x			= TRUE
 varname.reg.end		= NULL
 lagTerms.reg.end		= NULL
 maxLags.reg.end		= NULL
 varname.reg.pre		= NULL
 lagTerms.reg.pre		= NULL
 maxLags.reg.pre		= NULL
 varname.reg.ex		= c("Sly", "mean_eta")
 lagTerms.reg.ex		= c(0,0)
 maxLags.reg.ex		= c(9,9)
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
 include.dum 		= FALSE
 dum.diff 			= NULL
 dum.lev 			= NULL
 varname.dum 		= NULL

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







