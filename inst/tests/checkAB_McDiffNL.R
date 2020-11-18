
rm(list = ls())


#	install.packages("pdynmc")
library(pdynmc)
#	install.packages("plm")
library(plm)


data(EmplUK, package = "plm")
dat <- EmplUK
dat[,c(4:7)] <- log(dat[,c(4:7)])
rm(EmplUK)

data.info(dat, i.name = "firm", t.name = "year")

strucUPD.plot(dat, i.name = "firm", t.name = "year")






m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
        use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
        include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
        fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
        varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
        include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
        w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
        opt.meth = "BFGS")
summary(m1)


m2 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
             fur.con = FALSE,
             include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = "year",
             w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
             opt.meth = "BFGS")
summary(m2)


m3 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
             fur.con = FALSE,
             include.dum = FALSE,
             w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
             opt.meth = "BFGS")
summary(m3)


m4 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
             fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
             varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
             include.dum = FALSE,
             w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
             opt.meth = "BFGS")
summary(m4)


m5 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
             fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = TRUE,
             varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
             include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
             w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
             opt.meth = "BFGS")
summary(m5)


m6 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
             fur.con = TRUE, fur.con.diff = FALSE, fur.con.lev = TRUE,
             varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
             include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = "year",
             w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
             opt.meth = "BFGS")
summary(m6)


m7 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
             fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = TRUE,
             varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
             include.dum = TRUE, dum.diff = TRUE, dum.lev = TRUE, varname.dum = "year",
             w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
             opt.meth = "BFGS")
summary(m7)


m8 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
             fur.con = TRUE, fur.con.diff = FALSE, fur.con.lev = TRUE,
             varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
             include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
             w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
             opt.meth = "BFGS")
summary(m8)


m9 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
             include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
             fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
             varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
             include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = "year",
             w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
             opt.meth = "BFGS")
summary(m9)








m10 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
        use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
        include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
        fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
        varname.reg.fur = "output", lagTerms.reg.fur = 2,
        include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
        w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
        opt.meth = "BFGS")
summary(m10)


m11 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
        use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
        include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
        fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
        varname.reg.fur = "output", lagTerms.reg.fur = 2,
        include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
        w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
        opt.meth = "BFGS")
summary(m11)


m12 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
        use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
        include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
        fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
        varname.reg.fur = "output", lagTerms.reg.fur = 1,
        include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
        w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
        opt.meth = "BFGS")
summary(m12)


m13 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
        use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
        include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
        fur.con = TRUE, fur.con.diff = FALSE, fur.con.lev = TRUE,
        varname.reg.fur = "output", lagTerms.reg.fur = 1,
        include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
        w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
        opt.meth = "BFGS")
summary(m13)


m14 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
        use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
        include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
        fur.con = TRUE, fur.con.diff = FALSE, fur.con.lev = TRUE,
        varname.reg.fur = c("wage", "output"), lagTerms.reg.fur = c(1,2),
        include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
        w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
        opt.meth = "BFGS")
summary(m14)


m15 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
        use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
        include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
        fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = TRUE,
        varname.reg.fur = c("wage", "output"), lagTerms.reg.fur = c(1,2),
        include.dum = FALSE, dum.diff = NULL, dum.lev = NULL, varname.dum = NULL,
        w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
        opt.meth = "BFGS")
summary(m15)


m16 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
        use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
        include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
        fur.con = FALSE, fur.con.diff = NULL, fur.con.lev = NULL,
        varname.reg.fur = NULL, lagTerms.reg.fur = NULL,
        include.dum = TRUE, dum.diff = TRUE, dum.lev = TRUE, varname.dum = "year",
        w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
        opt.meth = "BFGS")
summary(m16)


m17 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
        use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
        include.y = TRUE, varname.y = "emp", lagTerms.y = 1,
        fur.con = FALSE, fur.con.diff = NULL, fur.con.lev = NULL,
        varname.reg.fur = NULL, lagTerms.reg.fur = NULL,
        include.dum = TRUE, dum.diff = TRUE, dum.lev = TRUE, varname.dum = "year",
        w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
        opt.meth = "BFGS")
summary(m17)


m18 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
        use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
        include.y = TRUE, varname.y = "emp", lagTerms.y = 1,
        fur.con = FALSE, fur.con.diff = NULL, fur.con.lev = NULL,
        varname.reg.fur = NULL, lagTerms.reg.fur = NULL,
        include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = "year",
        w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
        opt.meth = "BFGS")
summary(m18)


m19 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
        use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
        include.y = TRUE, varname.y = "emp", lagTerms.y = 1,
        fur.con = FALSE, fur.con.diff = NULL, fur.con.lev = NULL,
        varname.reg.fur = NULL, lagTerms.reg.fur = NULL,
        include.dum = TRUE, dum.diff = FALSE, dum.lev = TRUE, varname.dum = "year",
        w.mat = "iid.err", std.err = "corrected", estimation = "iterative", max.iter = 4,
        opt.meth = "BFGS")
summary(m19)









ls()[grepl(ls(), pattern = "m")]
length(ls()[grepl(ls(), pattern = "m")])		# 19 configurations are estimated

