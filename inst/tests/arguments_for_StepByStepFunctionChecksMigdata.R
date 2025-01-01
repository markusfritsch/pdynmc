

rm(list = ls())


#	install.packages("pdynmc")
library(pdynmc)


setwd("D:/Work/20_Projekte/450_Migration/migFlowsAndDrivers/data")


datInEU	<- readRDS(file = "datInEUwCov.rds")
datOutEU	<- readRDS(file = "datOutEUwCov.rds")





nrow(datInEU)
nrow(datOutEU)

table(datOutEU$orig)
table(datOutEU$year0)

summary(datOutEU$flow)


#number of observations if there is migration from country of origin i
# to destination j in any time period within Europe
length(unique(datInEU$dest)) * length(unique(datInEU$year0))
sum(table(datInEU$orig) == length(unique(datInEU$dest)) * length(unique(datInEU$year0)))
which(table(datInEU$orig) == length(unique(datInEU$dest)) * length(unique(datInEU$year0)))


#number of observations if there is migration from country of origin i
# to destination j in any time period from outside of Europe
length(unique(datOutEU$dest)) * length(unique(datOutEU$year0))
sum(table(datOutEU$orig) == length(unique(datOutEU$dest)) * length(unique(datOutEU$year0)))
which(table(datOutEU$orig) == length(unique(datOutEU$dest)) * length(unique(datOutEU$year0)))






#balance panel datasets (only sensible for migration from outside of Europe)

migPanel	<- datOutEU[datOutEU$orig %in% names(which(table(datOutEU$orig) == length(unique(datOutEU$dest)) * length(unique(datOutEU$year0)))), ]

head(migPanel, 20)

#relevant columns:
#year0: time horizon; for example 1960 means 1960-1969 (t)
#orig: country of origin (i)
#dest: destination (j)
#flow: migration flow estimate
#UnempRate_orig: Unemployment (in %) in country of origin
#GDPdomCurr_orig: GDP (domestic currency) in country of origin
#ExchRateDomCurrPerUSD: Exchange rate (domestic currency per USD)
#CPI_orig: Consumer price index in country of origin
#IndProdIdx_orig: Industrial production index in country of origin
#Pop_orig: Population (in persons) in country of origin
#UnempRate_orig: Unemployment rate (in %) in country of origin
#GDPdomCurr_dest: GDP (domestic currency) in country of destination
#CPI_dest: Consumer price index in country of destination
#IndProdIdx_dest: Industrial production index in country of destination
#Pop_dest: Population (in persons) in country of destination










###############################################
###	Extend data set
###############################################

dat	<- migPanel


###
###	Create contintent (of origin) variable
###
#	mostly for plotting purposes
#	we use a seven continent scheme (and omit continent antarctica)

dat$continent	<- NA

dat$continent[dat$orig %in% c("CMR", "EGY", "MAR", "MUS", "RWA")]	<- "AFR"	# Africa
dat$continent[dat$orig %in% c("CAN", "USA")]	<- "AMN"	# North America
dat$continent[dat$orig %in% c(
  "ARG", "BOL", "BRA", "CHL", "COL", "CUB", "DOM", "GTM",
  "HND", "JAM", "MEX", "NIC", "PAN", "PER", "URY", "VEN"
)]	<- "AMS"	# South and Central America
dat$continent[dat$orig %in% c("IND", "IRQ", "LBN", "PHL", "QAT", "SYR", "TUR")]	<- "ASI"	# Asia
dat$continent[dat$orig %in% "FJI"]	<- "AUS"	# Australia
dat$continent[dat$orig %in% c("CHE", "GBR", "SLV")]	<- "EUR"	# Europe

table(dat$continent, dat$orig)






###
###	Add log(flow) to data set
###

dat$lflow	<- log(dat$flow)








###############################################
###	Add Penn World Table information
###############################################

###
###	Load data
###

#	install.packages("pwt10")
library(pwt10)

#	?pwt10.01


data(pwt10.01)
pwt	<- pwt10.01



#	dim(pwt)	#	12810 = 183 countries * 70 years (1950 to 2019)
#	head(pwt)





###
###	Add decade information and isocode-decade-combination
###

pwt$decade		<- floor(pwt$year*0.1)*10

pwt$couDecade	<- paste(as.character(pwt$isocode), pwt$decade, sep = "-")






###
###	Missing values (general analysis)
###

count.NA	<- function(vec){sum(is.na(vec))}

apply(X = pwt, MARGIN = 2, FUN = count.NA)







###
###	Average variables over decades
###

###	Determine variables to be averaged

pwt.columns		<- data.frame(
  col.id	= 1:ncol(pwt),
  col.name	= colnames(pwt),
  col.class	= NA
)
for(co in 1:ncol(pwt)){
  pwt.columns$col.class[co]	<- class(pwt[, co])
}

vars.to.avg		<- pwt.columns$col.id[
  pwt.columns$col.class == "numeric" &
    pwt.columns$col.name != "decade"
]





###	Average per decade

pwt.avg	<- pwt[pwt$year == pwt$decade, c("country", "isocode", "year")]

for(co in vars.to.avg){
  pwt.avg[, colnames(pwt)[co]]	<- tapply(X = pwt[, co], INDEX = pwt$couDecade, FUN = mean, na.rm = TRUE)
}


apply(X = pwt.avg, MARGIN = 2, FUN = count.NA)
apply(X = pwt.avg[pwt.avg$year > 1950 & pwt.avg$year < 2010, ], MARGIN = 2, FUN = count.NA)







###	long to wide for analysis of single variables

library(tidyr)

var.temp	<- "rgdpna"
#	var.temp	<- "emp"

pwt.wide <- spread(pwt.avg[, c("country", "isocode", "year", var.temp)], year, var.temp)
pwt.wide






###
###	Merge data sets "dat" and "pwt.avg"
###

###	create isocode-year-combination

dat$orig_year	<- paste(dat$orig, dat$year0, sep = "-")
dat$dest_year	<- paste(dat$dest, dat$year0, sep = "-")

pwt.avg$iso_year	<- paste(pwt.avg$isocode, pwt.avg$year, sep = "-")



###	Merge variables (in var.set) from pwt.avg to dat

var.set	<- c("pop"			#population
             , "emp"		#employment
             , "rgdpna"		#real GDP (USD; 2017 prices)
             , "xr"		#exchange rate (national currency per USD)
             , "csh_c"		#household share of consumption (in percent)
             , "pl_c"		#price level of household consumption (price level of USA in 2017 is 1)
)	# !!! select variable names from pwt data
#	var.temp	<- "pop"


for(var.temp in var.set){
  dat	<- merge(
    x		= dat,
    y		= pwt.avg[, c("iso_year", var.temp)],
    by.x	= "orig_year",
    by.y	= "iso_year",
    all.x	= TRUE,
    all.y	= FALSE
  )

  dat	<- merge(
    x		= dat,
    y		= pwt.avg[, c("iso_year", var.temp)],
    by.x	= "dest_year",
    by.y	= "iso_year",
    all.x	= TRUE,
    all.y	= FALSE,
    suffixes	= c("_orig", "_dest")
  )
  dat		<- dat[order(dat$year0, dat$dest, dat$orig, decreasing = FALSE), ]
}



#real GDP per capita
dat$rgdpnaPC_orig	<- dat$rgdpna_orig/dat$pop_orig
dat$rgdpnaPC_dest	<- dat$rgdpna_dest/dat$pop_dest

#real GDP per capita that ends up with household
dat$rgdpnaHPC_orig	<- (dat$rgdpna_orig/dat$pop_orig)*dat$csh_c_orig
dat$rgdpnaHPC_dest	<- (dat$rgdpna_dest/dat$pop_dest)*dat$csh_c_dest
dat$rgdpnaHPC		<- as.numeric((dat$rgdpnaHPC_dest-dat$rgdpnaHPC_orig)/dat$rgdpnaHPC_orig)

#price level of household consumption
dat$pl_c			<- as.numeric((dat$pl_c_dest-dat$pl_c_orig)*dat$pl_c_orig)

#combine origin and destination information to variable s = (i,j),
# where i indicates the country of origin and j the destination
dat$orig_dest		<- paste(dat$orig, "_", dat$dest, sep = "")


head(dat)




###	Comparison of population variables

plot(dat$Pop_orig, dat$pop_orig)
abline(c(0, 1/10^6))

plot(dat$Pop_dest, dat$pop_dest)
abline(c(0, 1/10^6))


#countries with missing values in the variables
unique(dat[is.na(dat$rgdpnaHPC + dat$pl_c + dat$lflow), "orig"])

#remove countries with missing values in the variables
dat	<- dat[!(dat$orig %in% unique(dat[is.na(dat$rgdpnaHPC + dat$pl_c + dat$lflow), "orig"])), ]















dat = dat
varname.i = "orig_dest"
varname.t = "year0"

use.mc.diff = TRUE
use.mc.lev = FALSE
use.mc.nonlin = FALSE
use.mc.nonlinAS = NULL
inst.stata = FALSE

include.y = TRUE
varname.y = "lflow"
lagTerms.y = 1
maxLags.y = NULL

include.x = TRUE
varname.reg.end = c("rgdpnaHPC","pl_c")
lagTerms.reg.end = c(0,0)
maxLags.reg.end = NULL
varname.reg.pre = NULL
lagTerms.reg.pre = NULL
maxLags.reg.pre = NULL
varname.reg.ex = NULL
lagTerms.reg.ex = NULL
maxLags.reg.ex = NULL
include.x.instr = FALSE
varname.reg.instr = NULL
inst.reg.ex.expand = TRUE
include.x.toInstr = FALSE
varname.reg.toInstr = NULL

fur.con = FALSE
fur.con.diff = NULL
fur.con.lev = NULL
varname.reg.fur = NULL
lagTerms.reg.fur = NULL

include.dum = TRUE
#custom.dum = TRUE
dum.diff = FALSE
dum.lev = TRUE
varname.dum = "year0"

col_tol = 0.65
w.mat = "iid.err"
w.mat.stata = FALSE

std.err = "corrected"
estimation = "onestep"
max.iter = 4
iter.tol = 0.01
inst.thresh = NULL
opt.meth = "none"
hessian = FALSE
optCtrl = list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol =
    .Machine$double.eps^(1/3), starttests = TRUE, dowarn = TRUE, badval = (0.25) *
    .Machine$double.xmax, usenumDeriv = FALSE, reltol = 1e-12, maxit = 200, trace = TRUE,
    follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e+07, pgtol = 0,
    all.methods = FALSE)
custom.start.val = FALSE
start.val = NULL
start.val.lo = -1
start.val.up = 1
seed.input = 2





