


data(EmplUK, package = "plm")
dat <- EmplUK
dat[,c(4:7)] <- log(dat[,c(4:7)])


m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
        use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
        include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
        fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
        varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
        include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
        w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
        opt.meth = "none")
summary(m1)

m2 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
             use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
             include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
             fur.con = FALSE, fur.con.diff = FALSE, fur.con.lev = FALSE,
             varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
             include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
             w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
             opt.meth = "none")
summary(m2)
