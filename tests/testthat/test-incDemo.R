test_that("check estimation based on linear moment conditions from equations in differences for AB data", {
  skip_on_cran()

 library(pder)
 data("DemocracyIncome25", package = "pder")
 dat	<- DemocracyIncome25




 M10 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,opt.meth = "none"
 )


 M11 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "none"
 )


 M12 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M13 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M14 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M15 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "none"
 )


 M16 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M17 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M20 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,opt.meth = "BFGS"
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                  .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                  .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		                follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		                all.methods = FALSE)
 )


 M21 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                  .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                  .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		                follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		                all.methods = FALSE)
 )


 M22 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                  .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                  .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		                follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		                all.methods = FALSE)
 )


 M23 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                  .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                  .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		                follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		                all.methods = FALSE)
 )


 M24 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M25 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "none"
 )


 M26 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M27 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M28 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "none"
 )


 M29 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M30 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,opt.meth = "none"
 )


 M31 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,opt.meth = "none"
 )


 M32 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,opt.meth = "BFGS", max.iter = 4
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                  .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                  .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		                follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		                all.methods = FALSE)
 )


 M33 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,opt.meth = "none", estimation = "iterative", max.iter = 50
 )


 M34 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M35 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "none"
 )


 M36 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M37 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M38 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "none"
 )


 M39 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
 )


 M40 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,opt.meth = "BFGS"
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                  .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                  .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		                follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		                all.methods = FALSE)
 )


 M41 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,opt.meth = "BFGS"
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                  .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                  .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		                follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		                all.methods = FALSE)
 )


 M42 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,opt.meth = "BFGS", max.iter = 4
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                  .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                  .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		                follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		                all.methods = FALSE)
 )


 M44 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                  .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                  .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		                follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		                all.methods = FALSE)
 )


 M45 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                  .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                  .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		                follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		                all.methods = FALSE)
 )


 M46 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                  .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                  .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		                follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		                all.methods = FALSE)
 )


 M47 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                  .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                  .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		                follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		                all.methods = FALSE)
 )


 M48 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                 .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                 .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		               follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		               all.methods = FALSE)
 )


 M49 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
		,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
		                 .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
		                 .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
		               follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
		               all.methods = FALSE)
 )




 n.models <- length(ls()[grepl(ls(), pattern = "M")])		# 37 configurations are estimated
 expect_equal(n.models, 37)
})







