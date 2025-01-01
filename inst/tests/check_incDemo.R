

rm(list = ls())



#	install.packages("pdynmc")
library(pdynmc)
#	install.packages("pder")
library(pder)


data("DemocracyIncome", package = "pder")
data("DemocracyIncome25", package = "pder")


	dat	<- DemocracyIncome
#	dat	<- DemocracyIncome25

rm(DemocracyIncome, DemocracyIncome25)

head(dat)
tail(dat)
str(dat)


#dat <- dat[!is.na(dat[,"democracy"]), ]


#dat.full <- data.frame("i"	= rep(unique(as.numeric(dat$country)), each = length(unique(dat$year))),
#			"country"	= rep(unique(as.character(dat$country)), each = length(unique(dat$year))),
#			"t"		= rep(unique(as.numeric(dat$year)), each = length(unique(dat$country))),
#			"year"	= rep(unique(as.character(dat$year)), each = length(unique(dat$country))),
#			"democracy"	= NA,
#			"income"	= NA
#)

table(dat[, "year"])


data.info(dat, i.name = "country", t.name = "year")

strucUPD.plot(dat, i.name = "country", t.name = "year")








m10 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,opt.meth = "none"
)
summary(m10)


m11 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m11)


m12 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m12)


m13 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m13)


m14 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m14)


m15 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m15)


m16 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m16)


m17 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m17)










m20 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,opt.meth = "BFGS"
)
summary(m20)


m21 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
)
summary(m21)


m22 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
)
summary(m22)


m23 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
)
summary(m23)


m24 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m24)


m25 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m25)


m26 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m26)


m27 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m27)


m28 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m28)


m29 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m29)











m30 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,opt.meth = "none"
)
summary(m30)


m31 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,opt.meth = "none"
)
summary(m31)


m32 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,opt.meth = "BFGS", max.iter = 4
)
summary(m32)


m33 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,opt.meth = "none", estimation = "iterative", max.iter = 50
)
summary(m33)


m34 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m34)


m35 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m35)


m36 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m36)


m37 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m37)


m38 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m38)


m39 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "none"
)
summary(m39)















m40 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 1
		,opt.meth = "BFGS"
)
summary(m40)


m41 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,opt.meth = "BFGS"
)
summary(m41)


m42 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,opt.meth = "BFGS", max.iter = 4
)
summary(m42)


m44 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
)
summary(m44)


m45 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
)
summary(m45)


m46 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 2
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
)
summary(m46)


m47 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = FALSE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
)
summary(m47)


m48 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = FALSE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
)
summary(m48)


m49 <- pdynmc(dat = dat, varname.i = "country", varname.t = "year"
		,use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE
		,varname.y = "democracy", include.y = TRUE, lagTerms.y = 3
		,include.dum = TRUE, dum.lev = TRUE, dum.diff = TRUE, varname.dum = "year"
		,opt.meth = "BFGS", max.iter = 4
)
summary(m49)








ls()[grepl(ls(), pattern = "m")]
length(ls()[grepl(ls(), pattern = "m")])		# 37 configurations are estimated








