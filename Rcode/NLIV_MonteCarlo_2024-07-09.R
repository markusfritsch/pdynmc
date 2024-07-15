
##############################################
###	Basics
##############################################

###
###	Work directory, packages, workspace, and helper functions
###

setwd("C:/Work/Research/Papers/Panel/Software_Proposal/R")



# install.packages("pdynmc")
library(pdynmc)

#	install.packages("mvtnorm")
library(mvtnorm)





rm(list = ls())





#source("NLIV_Functions_2024-07-09.R")






folder.name		<- "MonteCarlo_2024-07-09"
if(!file.exists(folder.name)){dir.create(folder.name)}











############################################################################################################
### Function for DGPs of Gorgens, Han, & Xue (AES, 2019)
############################################################################################################



dat.create.GHX	<- function(
    alpha_0,
    n,
    T,
    mean_y0_c,
    cova_y0_c,
    vari_v,
    weight_c = FALSE,
    seed
){
  set.seed(seed)

  dat.indices		<- data.frame(
    i		= rep(1:n, each = T),
    t		= rep(1:T, times = n)
  )

  if(cova_y0_c[1, 1] == 0 & cova_y0_c[2, 2] == 0){
    y_0	<- rep(x = mean_y0_c[1], times = n)
    c		<- rep(x = mean_y0_c[2], times = n)
  }
  if(cova_y0_c[1, 1] == 0 & cova_y0_c[2, 2] != 0){
    y_0	<- rep(x = mean_y0_c[1], times = n)
    c		<- rnorm(n = n, mean = mean_y0_c[2], sd = sqrt(cova_y0_c[2, 2]))
  }
  if(cova_y0_c[1, 1] != 0 & cova_y0_c[2, 2] == 0){
    y_0	<- rnorm(n = n, mean = mean_y0_c[1], sd = sqrt(cova_y0_c[1, 1]))
    c		<- rep(x = mean_y0_c[2], times = n)
  }
  if(cova_y0_c[1, 1] != 0 & cova_y0_c[2, 2] != 0){
    mat_y0_c	<- rmvnorm(
      n		= n,
      mean		= mean_y0_c,
      sigma		= cova_y0_c,
      method	= "chol"
    )
    y_0	<- mat_y0_c[, 1]
    c		<- mat_y0_c[, 2]
  }

  v		<- matrix(data = rnorm(n = n*T, mean = 0, sd = sqrt(vari_v)), nrow = T, ncol = n, byrow = FALSE)

  y		<- matrix(data = NA, nrow = T, ncol = n)

  if(weight_c){
    y[1, ]	<- alpha_0*y_0 + (1 - alpha_0)*c + v[1, ]
    for(t in 2:T){
      y[t, ]	<- alpha_0*y[t - 1, ] + (1 - alpha_0)*c + v[t, ]
    }
  } else {
    y[1, ]	<- alpha_0*y_0 + c + v[1, ]
    for(t in 2:T){
      y[t, ]	<- alpha_0*y[t - 1, ] + c + v[t, ]
    }
  }

  dat	<- data.frame(
    dat.indices,
    y	= as.numeric(y)
  )

  return(dat)
}

















##############################################
###	Gorgens, Han, & Xue (2010), DGP 1.1 & 1.2
##############################################

time.A	<- Sys.time()

file.name		<- "dgp_11_12"



###
###	Configuration
###

R		<- 5000
n.set		<- c(200, 5000)
T.set		<- 4
rho.set	<- 0.5



num.seeds	<- length(rho.set)*length(n.set)*length(T.set)*R


set.seed(1)
seed.set	<- array(
  data	= sample(x = num.seeds*100, size = num.seeds, replace = FALSE),
  dim		= c(length(rho.set), length(n.set), length(T.set), R)
) + sample(x = num.seeds*100, size = 1)




results.temp	<- as.data.frame(array(
  data	= NA,
  dim		= c(R, 2 + 5 + 5 + 1),
  dimnames	= list(NULL, c(
    "r", "seed",
    "A.T", "B.T", "C.T", "rho.hat.T.pos", "rho.hat.T.neg",
    "A.t", "B.t", "C.t", "rho.hat.t.pos", "rho.hat.t.neg",
    "rho.hat.HP"
  ))
))
results.temp[, "r"]	<- 1:R


for(rho.id in 1:length(rho.set)){
  rho.star	<- rho.set[rho.id]




  for(n.id in 1:length(n.set)){
    n		<- n.set[n.id]

    for(T.id in 1:length(T.set)){
      T	<- T.set[T.id]



	res	<- list(
		config	= c(n = n, T = T, rho = rho.star)
	)

	res.temp	<- results.temp

      for(r in 1:R){

seed		<- seed.set[rho.id, n.id, T.id, r]	# Seed for reproducible data creation
res.temp[r, "seed"]	<- seed





###
###	Create data
###

dat	<- dat.create.GHX(
  alpha_0	= rho.star,
  n		= n,
  T		= T,
  mean_y0_c	= c(0, 0),
  cova_y0_c	= matrix(c(0.04, 0.02, 0.02, 1), nrow = 2, byrow = FALSE),
  vari_v	= c(0.99, 0.495, rep(x = 1, times = T - 2)),
  weight_c	= FALSE,
  seed	= seed
)






###
###	Compute estimators
###

rho.hat.NLIV.T	<- NLIV(dat = dat, varname.i = "i", varname.t = "t", varname.y = "y")

rho.hat.NLIV.t	<- NLIV(dat = dat, varname.i = "i", varname.t = "t", varname.y = "y")

rho.hat.HP		<- FDLS(dat = dat, varname.i = "i", varname.t = "t", varname.y = "y")


res.temp[r, c("A.T", "B.T", "C.T", "rho.hat.T.pos", "rho.hat.T.neg")]	<- rho.hat.NLIV.T
res.temp[r, c("A.t", "B.t", "C.t", "rho.hat.t.pos", "rho.hat.t.neg")]	<- rho.hat.NLIV.t
res.temp[r, "rho.hat.HP"]	<- rho.hat.HP


print(paste("Computation done for rho=", rho.star, ", n=", n, ", T=", T, ", r=", r, ".", sep = ""))




###
###	Save results
###

      }	# end of r-loop

	res$res	<- res.temp

      save(list = "res", file = paste(folder.name, "/NLIV_MC_rho", c("m", "", "p")[c(rho.star < 0, rho.star == 0, rho.star > 0)], abs(rho.star), "_n", n, "_T", T, "_", file.name, ".RData", sep = ""))

    }		# end of T-loop
  }		# end of n-loop
}		# end of rho-loop

time.B	<- Sys.time()
difftime(time.B, time.A, unit = "mins")









##############################################
###	Gorgens, Han, & Xue (2010), DGP 1.1 & 1.2 adjusted to homoskedasticity
##############################################

file.name		<- "dgp_11_12_homo"



###
###	Configuration
###

R		<- 5000
n.set		<- c(200, 5000)
T.set		<- 4
rho.set	<- 0.5



num.seeds	<- length(rho.set)*length(n.set)*length(T.set)*R


set.seed(2)
seed.set	<- array(
  data	= sample(x = num.seeds*100, size = num.seeds, replace = FALSE),
  dim		= c(length(rho.set), length(n.set), length(T.set), R)
) + sample(x = num.seeds*100, size = 1)




results.temp	<- as.data.frame(array(
  data	= NA,
  dim		= c(R, 2 + 5 + 5 + 1),
  dimnames	= list(NULL, c(
    "r", "seed",
    "A.T", "B.T", "C.T", "rho.hat.T.pos", "rho.hat.T.neg",
    "A.t", "B.t", "C.t", "rho.hat.t.pos", "rho.hat.t.neg",
    "rho.hat.HP"
  ))
))
results.temp[, "r"]	<- 1:R

for(rho.id in 1:length(rho.set)){
  rho.star	<- rho.set[rho.id]




  for(n.id in 1:length(n.set)){
    n		<- n.set[n.id]

    for(T.id in 1:length(T.set)){
      T	<- T.set[T.id]



	res	<- list(
		config	= c(n = n, T = T, rho = rho.star)
	)

	res.temp	<- results.temp

      for(r in 1:R){

seed		<- seed.set[rho.id, n.id, T.id, r]	# Seed for reproducible data creation
res.temp[r, "seed"]	<- seed





###
###	Create data
###

dat	<- dat.create.GHX(
  alpha_0	= rho.star,
  n		= n,
  T		= T,
  mean_y0_c	= c(0, 0),
  cova_y0_c	= matrix(c(0.04, 0.02, 0.02, 1), nrow = 2, byrow = FALSE),
  vari_v	= rep(x = 1, times = T),
  weight_c	= FALSE,
  seed	= seed
)






###
###	Compute estimators
###

rho.hat.NLIV.T	<- NLIV(dat = dat, varname.i = "i", varname.t = "t", varname.y = "y")

rho.hat.NLIV.t	<- NLIV.alt(dat = dat, varname.i = "i", varname.t = "t", varname.y = "y")

rho.hat.HP		<- FDLS(dat = dat, varname.i = "i", varname.t = "t", varname.y = "y")


res.temp[r, c("A.T", "B.T", "C.T", "rho.hat.T.pos", "rho.hat.T.neg")]	<- rho.hat.NLIV.T
res.temp[r, c("A.t", "B.t", "C.t", "rho.hat.t.pos", "rho.hat.t.neg")]	<- rho.hat.NLIV.t
res.temp[r, "rho.hat.HP"]	<- rho.hat.HP


print(paste("Computation done for rho=", rho.star, ", n=", n, ", T=", T, ", r=", r, ".", sep = ""))




###
###	Save results
###

      }	# end of r-loop

	res$res	<- res.temp

      save(list = "res", file = paste(folder.name, "/NLIV_MC_rho", c("m", "", "p")[c(rho.star < 0, rho.star == 0, rho.star > 0)], abs(rho.star), "_n", n, "_T", T, "_", file.name, ".RData", sep = ""))

    }		# end of T-loop
  }		# end of n-loop
}		# end of rho-loop


time.C	<- Sys.time()
difftime(time.C, time.B, unit = "mins")












##############################################
###	Modified DGPs
##############################################

file.name		<- "dgp_1_adjusted_homo"



###
###	Configuration
###

R		<- 5000
n.set		<- c(40, 200, 1000)
T.set		<- c(8, 40, 200)
rho.set	<- c(0.5, 0.7, 0.9, 1)



num.seeds	<- length(rho.set)*length(n.set)*length(T.set)*R


set.seed(3)
seed.set	<- array(
  data	= sample(x = num.seeds*100, size = num.seeds, replace = FALSE),
  dim		= c(length(rho.set), length(n.set), length(T.set), R)
) + sample(x = num.seeds*100, size = 1)




results.temp	<- as.data.frame(array(
  data	= NA,
  dim		= c(R, 2 + 5 + 5 + 1),
  dimnames	= list(NULL, c(
    "r", "seed",
    "A.T", "B.T", "C.T", "rho.hat.T.pos", "rho.hat.T.neg",
    "A.t", "B.t", "C.t", "rho.hat.t.pos", "rho.hat.t.neg",
    "rho.hat.HP"
  ))
))
results.temp[, "r"]	<- 1:R

for(rho.id in 1:length(rho.set)){
  rho.star	<- rho.set[rho.id]




  for(n.id in 1:length(n.set)){
    n		<- n.set[n.id]

    for(T.id in 1:length(T.set)){
      T	<- T.set[T.id]



	res	<- list(
		config	= c(n = n, T = T, rho = rho.star)
	)

	res.temp	<- results.temp

      for(r in 1:R){

seed		<- seed.set[rho.id, n.id, T.id, r]	# Seed for reproducible data creation
res.temp[r, "seed"]	<- seed





###
###	Create data
###

dat	<- dat.create.GHX(
  alpha_0	= rho.star,
  n		= n,
  T		= T,
  mean_y0_c	= c(0, 0),
  cova_y0_c	= matrix(c(0.04, 0.02, 0.02, 1), nrow = 2, byrow = FALSE),
  vari_v	= rep(x = 1, times = T),
  weight_c	= TRUE,
  seed	= seed
)






###
###	Compute estimators
###

rho.hat.NLIV.T	<- NLIV(dat = dat, varname.i = "i", varname.t = "t", varname.y = "y")

rho.hat.NLIV.t	<- NLIV.alt(dat = dat, varname.i = "i", varname.t = "t", varname.y = "y")

rho.hat.HP		<- FDLS(dat = dat, varname.i = "i", varname.t = "t", varname.y = "y")


res.temp[r, c("A.T", "B.T", "C.T", "rho.hat.T.pos", "rho.hat.T.neg")]	<- rho.hat.NLIV.T
res.temp[r, c("A.t", "B.t", "C.t", "rho.hat.t.pos", "rho.hat.t.neg")]	<- rho.hat.NLIV.t
res.temp[r, "rho.hat.HP"]	<- rho.hat.HP




print(paste("Computation done for rho=", rho.star, ", n=", n, ", T=", T, ", r=", r, ".", sep = ""))




###
###	Save results
###

      }	# end of r-loop

res.temp[, "rho.hat.T"]		<- (res.temp[, "A.T"] < 0)*res.temp[, "rho.hat.T.pos"] + (res.temp[, "A.T"] >= 0)*res.temp[, "rho.hat.T.neg"]
res.temp[, "rho.hat.t"]		<- (res.temp[, "A.t"] < 0)*res.temp[, "rho.hat.t.pos"] + (res.temp[, "A.t"] >= 0)*res.temp[, "rho.hat.t.neg"]
res.temp[, "rho.hat.T.avg"]	<- rowMeans(res.temp[, c("rho.hat.T.pos", "rho.hat.T.neg")])
res.temp[, "rho.hat.t.avg"]	<- rowMeans(res.temp[, c("rho.hat.t.pos", "rho.hat.t.neg")])


	res$res	<- res.temp

      save(list = "res", file = paste(folder.name, "/NLIV_MC_rho", c("m", "", "p")[c(rho.star < 0, rho.star == 0, rho.star > 0)], abs(rho.star), "_n", n, "_T", T, "_", file.name, ".RData", sep = ""))

    }		# end of T-loop
  }		# end of n-loop
}		# end of rho-loop


time.D	<- Sys.time()
difftime(time.D, time.C, unit = "mins")















##############################################
###	Evaluation of results
##############################################

rm(list = ls())

folder.name		<- "MonteCarlo_2024-07-09"



###
###	Table 1
###

file.name		<- "dgp_11_12"

n.set		<- c(200, 5000)
T.set		<- 4
rho.set	<- 0.5


r	<- list()

est.set	<- c("rho.hat.T.neg", "rho.hat.HP")


for(est.temp in est.set){

for(rho.id in 1:length(rho.set)){
  rho.star	<- rho.set[rho.id]

  for(n.id in 1:length(n.set)){
    n		<- n.set[n.id]

    for(T.id in 1:length(T.set)){
      T	<- T.set[T.id]


      load(file = paste(folder.name, "/NLIV_MC_rho", c("m", "", "p")[c(rho.star < 0, rho.star == 0, rho.star > 0)], abs(rho.star), "_n", n, "_T", T, "_", file.name, ".RData", sep = ""))

re	<- data.frame(
  Estimator		= est.temp,
  t(res$config),
  Min		= min(res$res[, est.temp]),
  Q25		= quantile(res$res[, est.temp], probs = 0.25),
  Q50		= quantile(res$res[, est.temp], probs = 0.50),
  Q75		= quantile(res$res[, est.temp], probs = 0.75),
  Max		= max(res$res[, est.temp]),
  Mean	= mean(res$res[, est.temp]),
  SD		= sd(res$res[, est.temp])
)

rownames(re)	<- NULL
re["Bias"]	<- res$config["rho"] - re["Mean"]
re["RMSE"]	<- sqrt(re["SD"]^2 + re["Bias"]^2)

r	<- rbind(r, re)

}}}}

r
data.frame(r[, c(1:3)], round(r[, c("Bias", "SD", "RMSE")], 3))







###
###	Table 2
###

file.name		<- "dgp_11_12_homo"

n.set		<- c(200, 5000)
T.set		<- 4
rho.set	<- 0.5


r	<- list()

est.set	<- c("rho.hat.T.neg", "rho.hat.HP")


for(est.temp in est.set){

for(rho.id in 1:length(rho.set)){
  rho.star	<- rho.set[rho.id]

  for(n.id in 1:length(n.set)){
    n		<- n.set[n.id]

    for(T.id in 1:length(T.set)){
      T	<- T.set[T.id]


      load(file = paste(folder.name, "/NLIV_MC_rho", c("m", "", "p")[c(rho.star < 0, rho.star == 0, rho.star > 0)], abs(rho.star), "_n", n, "_T", T, "_", file.name, ".RData", sep = ""))

re	<- data.frame(
  Estimator		= est.temp,
  t(res$config),
  Min		= min(res$res[, est.temp]),
  Q25		= quantile(res$res[, est.temp], probs = 0.25),
  Q50		= quantile(res$res[, est.temp], probs = 0.50),
  Q75		= quantile(res$res[, est.temp], probs = 0.75),
  Max		= max(res$res[, est.temp]),
  Mean	= mean(res$res[, est.temp]),
  SD		= sd(res$res[, est.temp])
)

rownames(re)	<- NULL
re["Bias"]	<- res$config["rho"] - re["Mean"]
re["RMSE"]	<- sqrt(re["SD"]^2 + re["Bias"]^2)

r	<- rbind(r, re)

}}}}

r
data.frame(r[, c(1:3)], round(r[, c("Bias", "SD", "RMSE")], 3))






###
###	Tables 3 & 4
###

file.name		<- "dgp_1_adjusted_homo"


n.set		<- c(40, 200, 1000)
T.set		<- c(8, 40, 200)
rho.set	<- c(0.5, 0.7, 0.9)


r	<- list()

est.set	<- c("rho.hat.T.neg", "rho.hat.t.neg", "rho.hat.HP")


for(est.temp in est.set){

for(rho.id in 1:length(rho.set)){
  rho.star	<- rho.set[rho.id]

  for(n.id in 1:length(n.set)){
    n		<- n.set[n.id]

    for(T.id in 1:length(T.set)){
      T	<- T.set[T.id]


      load(file = paste(folder.name, "/NLIV_MC_rho", c("m", "", "p")[c(rho.star < 0, rho.star == 0, rho.star > 0)], abs(rho.star), "_n", n, "_T", T, "_", file.name, ".RData", sep = ""))

re	<- data.frame(
  Estimator		= est.temp,
  t(res$config),
  Min		= min(res$res[, est.temp]),
  Q25		= quantile(res$res[, est.temp], probs = 0.25),
  Q50		= quantile(res$res[, est.temp], probs = 0.50),
  Q75		= quantile(res$res[, est.temp], probs = 0.75),
  Max		= max(res$res[, est.temp]),
  Mean	= mean(res$res[, est.temp]),
  SD		= sd(res$res[, est.temp])
)

rownames(re)	<- NULL
re["Bias"]	<- res$config["rho"] - re["Mean"]
re["RMSE"]	<- sqrt(re["SD"]^2 + re["Bias"]^2)

r	<- rbind(r, re)

}}}}

r
data.frame(r[, c(1:4)], round(r[, c("Bias", "SD", "RMSE")], 3))








###
###	Table 5
###


file.name		<- "dgp_1_adjusted_homo"


n.set		<- c(40, 200, 1000)
T.set		<- c(8, 40, 200)
rho.set	<- 1


r	<- list()

est.set	<- c("rho.hat.T.avg", "rho.hat.t.avg", "rho.hat.HP")


for(est.temp in est.set){

for(rho.id in 1:length(rho.set)){
  rho.star	<- rho.set[rho.id]

  for(n.id in 1:length(n.set)){
    n		<- n.set[n.id]

    for(T.id in 1:length(T.set)){
      T	<- T.set[T.id]


      load(file = paste(folder.name, "/NLIV_MC_rho", c("m", "", "p")[c(rho.star < 0, rho.star == 0, rho.star > 0)], abs(rho.star), "_n", n, "_T", T, "_", file.name, ".RData", sep = ""))

re	<- data.frame(
  Estimator		= est.temp,
  t(res$config),
  Min		= min(res$res[, est.temp]),
  Q25		= quantile(res$res[, est.temp], probs = 0.25),
  Q50		= quantile(res$res[, est.temp], probs = 0.50),
  Q75		= quantile(res$res[, est.temp], probs = 0.75),
  Max		= max(res$res[, est.temp]),
  Mean	= mean(res$res[, est.temp]),
  SD		= sd(res$res[, est.temp])
)

rownames(re)	<- NULL
re["Bias"]	<- res$config["rho"] - re["Mean"]
re["RMSE"]	<- sqrt(re["SD"]^2 + re["Bias"]^2)

r	<- rbind(r, re)

}}}}

r
data.frame(r[, c(1:3)], round(r[, c("Bias", "SD", "RMSE")], 3))








