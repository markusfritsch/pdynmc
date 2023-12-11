

rm(list = ls())


setwd("D:/Work/20_Projekte/450_Migration/migFlowsAndDrivers/data")

dat	<- readRDS(file = "../data/migData.rds")
dat	<- dat[!is.na(dat$prcp), ]

varname.i = "orig_dest"
varname.t = "year0"

use.mc.diff         = TRUE
use.mc.lev          = FALSE
use.mc.nonlin       = FALSE
use.mc.nonlinAS			= NULL


include.y       = TRUE
varname.y				= "lflow"
lagTerms.y			= 1
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
inst.reg.ex.expand  = TRUE

include.x.instr			= FALSE
varname.reg.instr		= NULL
include.x.toInstr		= FALSE
varname.reg.toInstr	= NULL

fur.con				= FALSE
fur.con.diff			= NULL
fur.con.lev			  = NULL
varname.reg.fur		= NULL
lagTerms.reg.fur	= NULL

include.dum			= TRUE
dum.diff				= FALSE
dum.lev				  = TRUE
varname.dum			= c("year_orig", "dest_year", "year0")

col_tol				= 0.65

w.mat				= "iid.err"
w.mat.stata			= FALSE

std.err				= "corrected"

estimation = "iterative"
max.iter				= 100
iter.tol				= 0.01
inst.thresh			= NULL
opt.meth = "none"





