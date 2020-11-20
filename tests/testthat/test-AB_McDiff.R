test_that("check estimation based on linear moment conditions from equations in differences for AB data", {
 skip_on_cran()

 library(plm)
 data(EmplUK, package = "plm")
 dat <- EmplUK
 dat[,c(4:7)] <- log(dat[,c(4:7)])



 M1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
              include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
              fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
              varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
              include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
              w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
              opt.meth = "BFGS"
              ,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
                                      .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
                                      .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
                              follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
                              all.methods = FALSE))


 M2 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
             include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
             fur.con = FALSE,
             include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = "year",
             w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
             opt.meth = "none")


 M3 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
              include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
              fur.con = FALSE,
              include.dum = FALSE,
              w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
              opt.meth = "none")


 M4 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
              include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
              fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
              varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
              include.dum = FALSE,
              w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
              opt.meth = "none")


 M5 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
              include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
              fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = TRUE,
              varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
              include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
              w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
              opt.meth = "none")


 M6 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
              include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
              fur.con = TRUE, fur.con.diff = FALSE, fur.con.lev = TRUE,
              varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
              include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = "year",
              w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
              opt.meth = "none")


 M7 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
              include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
              fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = TRUE,
              varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
              include.dum = TRUE, dum.diff = TRUE, dum.lev = TRUE, varname.dum = "year",
              w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
              opt.meth = "none")


 M8 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
              include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
              fur.con = TRUE, fur.con.diff = FALSE, fur.con.lev = TRUE,
              varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
              include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
              w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
              opt.meth = "BFGS"
              ,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
                                      .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
                                      .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
                              follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
                              all.methods = FALSE))


 M9 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
              include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
              fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
              varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
              include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = "year",
              w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
              opt.meth = "BFGS"
              ,optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
                                      .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
                                      .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = FALSE,
                              follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
                              all.methods = FALSE))


 M10 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
               use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
               include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
               fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
               varname.reg.fur = "output", lagTerms.reg.fur = 2,
               include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
               w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
               opt.meth = "none")


 M11 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
               use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
               include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
               fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
               varname.reg.fur = "output", lagTerms.reg.fur = 2,
               include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
               w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
               opt.meth = "none")


 M12 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
               use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
               include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
               fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
               varname.reg.fur = "output", lagTerms.reg.fur = 1,
               include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
               w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
               opt.meth = "none")


 M13 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
               use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
               include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
               fur.con = TRUE, fur.con.diff = FALSE, fur.con.lev = TRUE,
               varname.reg.fur = "output", lagTerms.reg.fur = 1,
               include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
               w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
               opt.meth = "none")


 M14 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
               use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
               include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
               fur.con = TRUE, fur.con.diff = FALSE, fur.con.lev = TRUE,
               varname.reg.fur = c("wage", "output"), lagTerms.reg.fur = c(1,2),
               include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
               w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
               opt.meth = "none")


 M15 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
               use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
               include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
               fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = TRUE,
               varname.reg.fur = c("wage", "output"), lagTerms.reg.fur = c(1,2),
               include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
               w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
               opt.meth = "none")


 M16 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
               use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
               include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
               fur.con = FALSE, fur.con.diff = NULL, fur.con.lev = NULL,
               varname.reg.fur = NULL, lagTerms.reg.fur = NULL,
               include.dum = TRUE, dum.diff = TRUE, dum.lev = TRUE, varname.dum = "year",
               w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
               opt.meth = "none")


 M17 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
               use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
               include.y = TRUE, varname.y = "emp", lagTerms.y = 1,
               fur.con = FALSE, fur.con.diff = NULL, fur.con.lev = NULL,
               varname.reg.fur = NULL, lagTerms.reg.fur = NULL,
               include.dum = TRUE, dum.diff = TRUE, dum.lev = TRUE, varname.dum = "year",
               w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
               opt.meth = "none")


 M18 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
               use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
               include.y = TRUE, varname.y = "emp", lagTerms.y = 1,
               fur.con = FALSE, fur.con.diff = NULL, fur.con.lev = NULL,
               varname.reg.fur = NULL, lagTerms.reg.fur = NULL,
               include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = "year",
               w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
               opt.meth = "none")


 M19 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
               use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
               include.y = TRUE, varname.y = "emp", lagTerms.y = 1,
               fur.con = FALSE, fur.con.diff = NULL, fur.con.lev = NULL,
               varname.reg.fur = NULL, lagTerms.reg.fur = NULL,
               include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = "year",
               w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
               opt.meth = "none")


 n.models <- length(ls()[grepl(ls(), pattern = "M")])		# 19 configurations are estimated
 expect_equal(n.models, 19)
})
