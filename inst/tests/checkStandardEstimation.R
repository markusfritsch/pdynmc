

rm(list = ls())





###
###	Simulation runs
###

R	<- 200

#	r	<- 1



###
###
###

n	<- 10		# Number of cross-section observations (maximal, if unbalanced panel).
T	<- 6		# Number of time periods (maximal, if unbalanced panel).

i.set	<- 1:n
t.set	<- 1:T


dat	<- expand.grid(i = i.set, t = t.set)
dat	<- dat[order(dat$i, dat$t), ]


set.seed(123)





###
###	Create error term
###

error.sigma	<- 1
epsilon	<- rnorm(n = nrow(dat), mean = 0, sd = error.sigma)






###
###	Create covariates x1 and x2
###

dat$x1	<- rnorm(n = nrow(dat), mean = 0, sd = 1)

beta0	<- 0
beta1	<- 0.5

#	put noise on last noise.perc% of cross-sectional units
x2.noise.perc	<- 20
x2.noise.sigma	<- 0.3
i.noise	<- (n - floor(n/100*x2.noise.perc) + 1):n
x2.noise	<- numeric(nrow(dat))
x2.noise[dat$i %in% i.noise]	<- rnorm(n = length(i.noise)*T, mean = 0, sd = x2.noise.sigma)


dat$x2	<- beta0 + beta1*dat$x1 + x2.noise







###
###	create response and lagged response (as covariate)
###

alpha0		<- 0
alpha1		<- 0.9
gamma1		<- 2
gamma2		<- 1.5

for(i in i.set){
  for(t in t.set){
    if(t == 1){
      dat$y[dat$i == i & dat$t == t]	<- alpha0 +
        alpha1*0 +
        gamma1*dat$x1[dat$i == i & dat$t == t] +
        gamma2*dat$x2[dat$i == i & dat$t == t] +
        epsilon[dat$i == i & dat$t == t]
    } else {
      dat$y[dat$i == i & dat$t == t]	<- alpha0 +
        alpha1*dat$y[dat$i == i & dat$t == t - 1] +
        gamma1*dat$x1[dat$i == i & dat$t == t] +
        gamma2*dat$x2[dat$i == i & dat$t == t] +
        epsilon[dat$i == i & dat$t == t]
    }
  }
}


###
###	Create unbalanced panel data set
###

delete.perc		<- 10		# percentage of rows that should be removed for creating unbalanced panel


if(delete.perc > 0){
  delete.rows		<- sample(x = 1:nrow(dat), size = delete.perc, replace = FALSE)
  dat		<- dat[-delete.rows, ]
}











###
### GMM-estimation using HNR & ABond moment conditions
###



dat = dat

varname.i = "i"
varname.t = "t"

use.mc.diff = TRUE
use.mc.lev = TRUE
use.mc.nonlin = FALSE
use.mc.nonlinAS			= NULL

inst.stata			= FALSE

include.y = TRUE
varname.y = "y"
lagTerms.y = 1
maxLags.y				= NULL

include.x = FALSE
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
inst.reg.ex.expand  = TRUE
include.x.toInstr		= FALSE
varname.reg.toInstr		= NULL

fur.con = TRUE
fur.con.diff = FALSE
fur.con.lev = TRUE
varname.reg.fur = c("x1", "x2")
lagTerms.reg.fur = c(0, 0)

include.dum			= FALSE
dum.diff				= NULL
dum.lev				  = NULL
varname.dum			= NULL

col_tol				= 0.65

w.mat = "iid.err"
w.mat.stata			= FALSE
std.err = "corrected"

estimation = "twostep"
max.iter				= 100
iter.tol				= 0.01
inst.thresh			= NULL
opt.meth = "none"
hessian				= FALSE
optCtrl				= list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol = .Machine$double.eps^(1/3),
                   starttests = TRUE, dowarn = TRUE, badval = (0.25)*.Machine$double.xmax, usenumDeriv = FALSE,
                   reltol = 1e-12, maxit = 200, trace = TRUE,
                   follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e7, pgtol = 0, all.methods = FALSE)
# ,nmulti				= 1
custom.start.val		= FALSE
start.val				= NULL
start.val.lo			= -1
start.val.up			= 1
seed.input			= 42
