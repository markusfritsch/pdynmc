


rm(list = ls())


#install.packages("data.table")
library(data.table)
#install.packages("foreign")
library(foreign)
library(pdynmc)



#	setwd(dir = "E:/Work/20_Projekte/50_Linear-Dynamic-Panel-Models/50_Drafts/20_Paper/50_Revisiting-habits-and-heterogeneity-in-demands")
setwd(dir = "D:/Work/20_Projekte/50_Linear-Dynamic-Panel-Models/50_Drafts/20_Paper/50_Revisiting-habits-and-heterogeneity-in-demands")





# data preparation Browning Collado (2007, JAE)


###	Load data


dat <- read.dta(file = "bc2.dta", convert.dates = TRUE, convert.factors = TRUE,
                missing.type = FALSE,
                convert.underscore = FALSE, warn.missing.labels = TRUE)

dat$yearquarter  <- as.character(dat$yearquarter)
dat$yearquarterA <- as.character(dat$yearquarterA)



sink(file="HNRandAS_log.txt")




m1 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
             include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
             varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
             include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
             fur.con = FALSE, fur.con.diff = NULL, fur.con.lev = NULL,
             varname.reg.fur = NULL, lagTerms.reg.fur = NULL,
             include.dum = TRUE, dum.diff = TRUE, dum.lev = TRUE, varname.dum = c("week", "yearquarter"),
             w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
             opt.meth = "BFGS")
summary(m1)

m2 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
             include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
             varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
             include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
             fur.con = FALSE, fur.con.diff = NULL, fur.con.lev = NULL,
             varname.reg.fur = NULL, lagTerms.reg.fur = NULL,
             include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = c("week", "yearquarter"),
             w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
             opt.meth = "BFGS")
summary(m2)

m3 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
             include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
             varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
             include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
             fur.con = FALSE, fur.con.diff = NULL, fur.con.lev = NULL,
             varname.reg.fur = NULL, lagTerms.reg.fur = NULL,
             include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = c("week", "yearquarter"),
             w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
             opt.meth = "BFGS")
summary(m3)

m4 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
             include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
             varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
             include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
             fur.con = FALSE, fur.con.diff = NULL, fur.con.lev = NULL,
             varname.reg.fur = NULL, lagTerms.reg.fur = NULL,
             include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
             w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
             opt.meth = "BFGS")
summary(m4)

m5 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
             include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
             varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
             include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
             fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = TRUE,
             varname.reg.fur = c("nch","nad","hage","hage2"), lagTerms.reg.fur = c(0,0,0,0),
             include.dum = TRUE, dum.diff = TRUE, dum.lev = TRUE, varname.dum = c("week", "yearquarter"),
             w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
             opt.meth = "BFGS")
summary(m5)

m6 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
             include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
             varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
             include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
             fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = TRUE,
             varname.reg.fur = c("nch","nad","hage","hage2"), lagTerms.reg.fur = c(0,0,0,0),
             include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = c("week", "yearquarter"),
             w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
             opt.meth = "BFGS")
summary(m6)

m7 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
             include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
             varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
             include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
             fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = TRUE,
             varname.reg.fur = c("nch","nad","hage","hage2"), lagTerms.reg.fur = c(0,0,0,0),
             include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = c("week", "yearquarter"),
             w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
             opt.meth = "BFGS")
summary(m7)

m8 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
             include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
             varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
             include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
             fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = TRUE,
             varname.reg.fur = c("nch","nad","hage","hage2"), lagTerms.reg.fur = c(0,0,0,0),
             include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
             w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
             opt.meth = "BFGS")
summary(m8)

m9 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
             include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
             varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
             include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
             fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
             varname.reg.fur = c("nch","nad","hage","hage2"), lagTerms.reg.fur = c(0,0,0,0),
             include.dum = TRUE, dum.diff = TRUE, dum.lev = TRUE, varname.dum = c("week", "yearquarter"),
             w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
             opt.meth = "BFGS")
summary(m9)

m10 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
             include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
             varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
             include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
             fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
             varname.reg.fur = c("nch","nad","hage","hage2"), lagTerms.reg.fur = c(0,0,0,0),
             include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = c("week", "yearquarter"),
             w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
             opt.meth = "BFGS")
summary(m10)

m11 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
              include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
              include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
              varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
              include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
              fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
              varname.reg.fur = c("nch","nad","hage","hage2"), lagTerms.reg.fur = c(0,0,0,0),
              include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = c("week", "yearquarter"),
              w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
              opt.meth = "BFGS")
summary(m11)

m12 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
              include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
              include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
              varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
              include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
              fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
              varname.reg.fur = c("nch","nad","hage","hage2"), lagTerms.reg.fur = c(0,0,0,0),
              include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
              w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
              opt.meth = "BFGS")
summary(m12)

m13 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
             include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
             varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
             include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
             fur.con = TRUE, fur.con.diff = FALSE, fur.con.lev = TRUE,
             varname.reg.fur = c("nch","nad","hage","hage2"), lagTerms.reg.fur = c(0,0,0,0),
             include.dum = TRUE, dum.diff = TRUE, dum.lev = TRUE, varname.dum = c("week", "yearquarter"),
             w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
             opt.meth = "BFGS")
summary(m13)

m14 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
              include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
              include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
              varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
              include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
              fur.con = TRUE, fur.con.diff = FALSE, fur.con.lev = TRUE,
              varname.reg.fur = c("nch","nad","hage","hage2"), lagTerms.reg.fur = c(0,0,0,0),
              include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = c("week", "yearquarter"),
              w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
              opt.meth = "BFGS")
summary(m14)

m15 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
              include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
              include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
              varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
              include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
              fur.con = TRUE, fur.con.diff = FALSE, fur.con.lev = TRUE,
              varname.reg.fur = c("nch","nad","hage","hage2"), lagTerms.reg.fur = c(0,0,0,0),
              include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = c("week", "yearquarter"),
              w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
              opt.meth = "BFGS")
summary(m15)

m16 <- pdynmc(dat = dat, varname.i = "i", varname.t = "t",
              use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
              include.y = FALSE, varname.y = "foodin", lagTerms.y = 1,
              include.x = TRUE, varname.reg.end = "lrxtot", lagTerms.reg.end = 0, maxLags.reg.end = 5,
              varname.reg.ex	= "lrhearn", lagTerms.reg.ex = 0, maxLags.reg.ex = 5,
              include.x.instr = TRUE, varname.reg.instr = "lrhearn", include.x.toInstr = FALSE,
              fur.con = TRUE, fur.con.diff = FALSE, fur.con.lev = TRUE,
              varname.reg.fur = c("nch","nad","hage","hage2"), lagTerms.reg.fur = c(0,0,0,0),
              include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
              w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
              opt.meth = "BFGS")
summary(m16)






ls()[grepl(ls(), pattern = "m")]
length(ls()[grepl(ls(), pattern = "m")])		# 16 configurations are estimated



