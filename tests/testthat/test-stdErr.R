

 data(ABdata, package = "pdynmc")
 dat <- ABdata
 dat[,c(4:7)] <- log(dat[,c(4:7)])

## Arellano and Bond (1991) estimation in Table 4, column (a1)
 m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "unadjusted", estimation = "onestep",
         opt.meth = "none")
 summary(m1)


 m2 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
         opt.meth = "none")
 summary(m2)


 m3 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "dbl.corrected", estimation = "onestep",
         opt.meth = "none")
 summary(m3)



 m4 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "unadjusted", estimation = "twostep",
         opt.meth = "none")
 summary(m4)


 m5 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
         opt.meth = "none")
 summary(m5)


 m6 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "dbl.corrected", estimation = "twostep",
         opt.meth = "none")
 summary(m6)



 m7 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "unadjusted", estimation = "iterative",
         opt.meth = "none")
 summary(m7)


 m8 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "corrected", estimation = "iterative",
         opt.meth = "none")
 summary(m8)


 m9 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "dbl.corrected", estimation = "iterative",
         opt.meth = "none")
 summary(m9)



