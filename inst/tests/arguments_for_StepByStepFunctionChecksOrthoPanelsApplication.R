

rm(list = ls())

data(BES_panel)
#BES.opm.model<-opm(Approve ~ Econ + Clegg + Brown + Cameron + NHS + Terror + PID + Tax,
#                   data = BES_panel, index = c('n','t'), n.samp = 10000, add.time.indicators = TRUE)



 dat = BES_panel
 varname.i = "n"
 varname.t = "t"
 use.mc.diff = TRUE
 use.mc.lev = FALSE
 use.mc.nonlin = FALSE
 use.mc.nonlinAS			= NULL
 inst.stata			= FALSE
 include.y = TRUE
 varname.y = "Approve"
 lagTerms.y = 1
 maxLags.y				= NULL

 include.x				= FALSE
 varname.reg.end			= NULL
 lagTerms.reg.end		= NULL
 maxLags.reg.end			= NULL
 varname.reg.pre			= NULL
 lagTerms.reg.pre		= NULL
 maxLags.reg.pre			= NULL
 varname.reg.ex			= NULL
 lagTerms.reg.ex			= NULL
 maxLags.reg.ex			= NULL

 include.x.instr			= FALSE
 varname.reg.instr		= NULL
 include.x.toInstr		= FALSE
 varname.reg.toInstr		= NULL

 fur.con = TRUE
 fur.con.diff = TRUE
 fur.con.lev = FALSE
 varname.reg.fur = c("Econ", "Clegg", "Brown", "Cameron",  "NHS", "Terror", "PID", "Tax")
 lagTerms.reg.fur =c(0,0,0,0,0,0,0,0)
 include.dum = TRUE
 dum.diff = TRUE
 dum.lev = FALSE
 varname.dum = "t"

  col_tol				= 0.65
 w.mat = "iid.err"
 w.mat.stata			= FALSE
 std.err = "corrected"
 estimation = "twostep"
 max.iter				= 100
 iter.tol				= 0.01
 inst.thresh			= NULL

 opt.meth = "BFGS"
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



