

rm(list = ls())



 data(EmplUK, package = "plm")
 dat <- EmplUK
 rm(EmplUK)
 dat[,c(4:7)] <- log(dat[,c(4:7)])

 library(pdynmc)

## Arellano and Bond (1991) estimation in Table 4, column (a1)
 m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
         opt.meth = "none")
 summary(m1)

## Arellano and Bond (1991) estimation in Table 4, column (a2)
 m2 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
         opt.meth = "none")
 summary(m2)

## Arellano and Bond (1991) twostep estimation extended by nonlinear moment
## conditions
 m3 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
         opt.meth = "BFGS")
 summary(m3)

## Arellano and Bond (1991) iterative estimation extended by nonlinear moment
## conditions
 m4 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "corrected", estimation = "iterative",
         max.iter = 4, opt.meth = "BFGS")
 summary(m4)

## Arellano and Bond (1991) twostep estimation extended by linear moment
## conditions from equations in levels
 m5 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
         use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE,
         include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
         fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
         varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
         include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
         w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
         opt.meth = "none")
 summary(m5)






ls()[grepl(ls(), pattern = "m")]
length(ls()[grepl(ls(), pattern = "m")])		# 5 configurations are estimated
