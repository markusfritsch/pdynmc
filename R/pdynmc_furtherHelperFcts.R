
#####################################################
###	Version information
#####################################################

###
###	Starting point
###

#	AhnSchmidt_Nonlinear_2017-11-07.R

#	split into different functions as of code version
#	AhnSchmidt_Nonlinear_2019-04-08.R



















###
### Functions for computing one-step and two-step weighting matrices
###







#' @keywords internal
#'
Wonestep.fct		<- function(
  w.mat
  ,w.mat.stata
  ,use.mc.diff
  ,use.mc.lev
  ,use.mc.nonlin
  ,use.mc.nonlinAS
  ,dum.diff
  ,dum.lev
  ,fur.con.diff
  ,fur.con.lev
  ,Z.temp
  ,n
  ,Time
  # ,mc.ref.t
  ,maxLags.y
  ,max.lagTerms
  ,end.reg
  ,ex.reg
  ,pre.reg
  ,n.inst
  ,inst.thresh
  ,env
){

  #
  # [M:] use different weight matrix, when collapsing the moment conditions to a single equation!?
  #




  # [M:] default Stata option for weighting matrix h(3) in xtabond2


  if(w.mat == "iid.err"){
    #   if(mc.ref.t){
    if(use.mc.diff | dum.diff | fur.con.diff){
      H_i.mcDiff	<- (diag(x = 2, nrow = Time - max.lagTerms - 1, ncol = Time - max.lagTerms - 1) -
                       rbind(rep(x = 0, times = Time - max.lagTerms - 1, ncol = Time - max.lagTerms - 2),
                             diag(x = 1, nrow = Time - max.lagTerms - 2, ncol = Time - max.lagTerms - 1)) -
                       cbind(rep(x = 0, times = Time - max.lagTerms - 1),
                             diag(x = 1, nrow = Time - max.lagTerms - 1, ncol = Time - max.lagTerms - 2)) )
    }

    if(use.mc.lev | end.reg){												# [M:] part of weighting matrix is identical for 'mc.ref.t' and 'mc.ref.T'
      H_i.mcLev		<- diag(Time - max(2,max.lagTerms))
    }

    if(dum.lev | fur.con.lev | ex.reg | pre.reg){
      if(pre.reg|ex.reg){
        if(max.lagTerms > 1){
          H_i.mcLev		<- diag(Time - (max.lagTerms-1))
        } else{
          H_i.mcLev		<- diag(Time - (max.lagTerms))
        }
      } else{
        H_i.mcLev		<- diag(Time - max.lagTerms)
      }

    }


    if(use.mc.nonlin){
      if(use.mc.nonlinAS){
        H_i.mcNL		<- diag(maxLags.y - max.lagTerms - 1)
      } else{
        H_i.mcNL		<- diag(Time - max.lagTerms - 2)
      }
    }

    if((use.mc.diff | dum.diff | fur.con.diff) & (use.mc.lev | dum.lev | fur.con.lev)){
      if((nrow(Z.temp[[1]]) - ncol(H_i.mcDiff) - if(use.mc.nonlin){ncol(H_i.mcNL)} else{0}) > Time - max.lagTerms - 1){
        H_i.off	<- (cbind(diag(x = -1, nrow = Time - max.lagTerms - 1, ncol = Time - max.lagTerms - 1), 0) +
                      cbind(rep(x = 0, times = Time - max.lagTerms - 1),
                               diag(x = 1, nrow = Time - max.lagTerms - 1, ncol = Time - max.lagTerms - 1)) )
        if((pre.reg|ex.reg) & max.lagTerms > 1){
          H_i.off <- cbind(H_i.off, 0)
        }
      } else{
        if((nrow(Z.temp[[1]]) - ncol(H_i.mcDiff) - if(use.mc.nonlin){ncol(H_i.mcNL)} else{0}) == Time - max.lagTerms - 1){
          H_i.off	<- (diag(x = -1, nrow = Time - max.lagTerms - 1, ncol = Time - max.lagTerms - 1) +
                      cbind(rep(x = 0, times = Time - max.lagTerms - 1),
                      diag(x = 1, nrow = Time - max.lagTerms - 1, ncol = Time - max.lagTerms - 2)) )
        } else{
          H_i.off	<- diag(x = -1, nrow = Time - max.lagTerms - 1, ncol = Time - max(2,max.lagTerms) - 1) +
                       cbind(rep(x = 0, times = Time - max.lagTerms - 1),
                             diag(x = 1, nrow = Time - max.lagTerms - 1, ncol = Time - max(2,max.lagTerms) - 1))
        }
      }
      if(use.mc.nonlin){
        H_i.temp		<- rbind(cbind(H_i.mcDiff, Matrix::Matrix(0, nrow = nrow(H_i.mcDiff), ncol = ncol(H_i.mcNL)), H_i.off),
                         cbind(Matrix::Matrix(0, nrow = nrow(H_i.mcNL), ncol = ncol(H_i.mcDiff)), H_i.mcNL, Matrix::Matrix(0, nrow = nrow(H_i.mcNL), ncol = ncol(H_i.off))),
                         cbind(t(H_i.off), Matrix::Matrix(0, nrow = nrow(H_i.mcLev), ncol = ncol(H_i.mcNL)), H_i.mcLev) )
      } else{
        H_i.temp		<- rbind(cbind(H_i.mcDiff, H_i.off), cbind( t(H_i.off), H_i.mcLev) )
      }
    }

    if((use.mc.diff | dum.diff | fur.con.diff) & !(use.mc.lev | dum.lev | fur.con.lev)){
      if(use.mc.nonlin){
        H_i.temp		<- Matrix::bdiag(H_i.mcDiff, H_i.mcNL)
      }
      if(!(use.mc.nonlin)){
        if(dum.lev | fur.con.lev){
          H_i.temp		<- Matrix::bdiag(H_i.mcDiff, H_i.mcLev)
        } else{
          H_i.temp		<- H_i.mcDiff
        }
      }
    }

    if(!(use.mc.diff | dum.diff | fur.con.diff) & (use.mc.lev | dum.lev | fur.con.lev)){
      if(use.mc.nonlin){
        H_i.temp		<- Matrix::bdiag(H_i.mcNL, H_i.mcLev)
      }
      if(!(use.mc.nonlin)){
        H_i.temp		<- H_i.mcLev
      }
    }

    if(!(use.mc.diff | dum.diff | fur.con.diff) & !(use.mc.lev | dum.lev | fur.con.lev) & use.mc.nonlin){
      H_i.temp		<- H_i.mcNL
    }

    #   } else{	#[M:] weighting matrix for reference period 'T' is still missing; also missing: consequences for weighting matrix when collapsing m.c. to a single equation

    #   }
  }






  # [M:] default Stata option for weighting matrix h(1) in xtabond2


  if(w.mat == "identity"){
    W1.inv.temp		<- lapply(Z.temp, function(x) crossprod(x, x))						# [M:] according to BBW, p.34 (Fn. 13); does not replicate the Stata xtdpdgmm results, though
    H_i.temp       <- diag(ncol(Z.temp))
  }






  # [M:] default Stata option for weighting matrix h(2) in xtabond2


  if(w.mat == "zero.cov"){					#[M:] similar to w.mat = "iid.err"; difference: covariances of linear m.c. are set to zero.
    #   if(mc.ref.t){
    if(use.mc.diff | dum.diff | fur.con.diff){
      H_i.mcDiff	<- (diag(x = 2, nrow = Time - max.lagTerms - 1, ncol = Time - max.lagTerms - 1) -
                       rbind(rep(x = 0, times = Time - max.lagTerms - 1, ncol = Time - max.lagTerms - 2),
                             diag(x = 1, nrow = Time - max.lagTerms - 2, ncol = Time - max.lagTerms - 1)) -
                       cbind(rep(x = 0, times = Time - max.lagTerms - 1),
                             diag(x = 1, nrow = Time - max.lagTerms - 1, ncol = Time - max.lagTerms - 2)) )
    }

    if(use.mc.lev | dum.lev | fur.con.lev){												# [M:] part of weighting matrix is identical for 'mc.ref.t' and 'mc.ref.T'
      if((pre.reg|ex.reg) & max.lagTerms > 1){
        H_i.mcLev		<- diag(Time - (max.lagTerms-1))
      } else{
        H_i.mcLev		<- diag(Time - max.lagTerms)
      }
    }

    if(use.mc.nonlin){
      H_i.mcNL		<- diag(Time - max.lagTerms - 2)
    }

    if((use.mc.diff | dum.diff | fur.con.diff) & (use.mc.lev | dum.lev | fur.con.lev)){
      if(use.mc.nonlin){
        H_i.temp		<- Matrix::bdiag(H_i.mcDiff, H_i.mcNL, H_i.mcLev)
      }
      if(!(use.mc.nonlin)){
        H_i.temp		<- Matrix::bdiag(H_i.mcDiff, H_i.mcLev)
      }
    }

    if(use.mc.diff & !(use.mc.lev | dum.lev | fur.con.lev)){
      if(use.mc.nonlin){
        H_i.temp		<- Matrix::bdiag(H_i.mcDiff, H_i.mcNL)
      }
      if(!(use.mc.nonlin)){
        H_i.temp		<- H_i.mcDiff
      }
    }

    if(!(use.mc.diff | dum.diff | fur.con.diff) & use.mc.lev){
      if(use.mc.nonlin){
        H_i.temp		<- Matrix::bdiag(H_i.mcNL, H_i.mcLev)
      }
      if(!(use.mc.nonlin)){
        H_i.temp		<- H_i.mcLev
      }
    }
    if(!(use.mc.diff | dum.diff | fur.con.diff) & !(use.mc.lev | dum.lev | fur.con.lev) & use.mc.nonlin){
      H_i.temp		<- H_i.mcNL
    }

    #   } else{	#[M:] weighting matrix for reference period 'T' is still missing; also missing: consequences for weighting matrix when collapsing m.c. to a single equation

    #   }
  }














  #[M:] the next three lines should correspond to the calculation of the 1step weighting matrix of the pgmm-function (but also yield a different outcome)
  #  calculation of the 1s weighting matrix according to BBW, p. 32, footnote 5: W_N = ((1/N)* (sum_{i=1}^N(Z' H Z) ))^{-1}
  #  according to BBW, the calculation below seems to be ok. This 'general weighting matrix' (W_N) is used as default.

  # if(use.mc.nonlin){
  # if(w.mat == "identity"){
  #   W1.inv.temp		<- lapply(Z.temp, function(x) crossprod(x, x))						# [M:] according to BBW, p.34 (Fn. 13); does not replicate the Stata xtdpdgmm results, though
  # } else{
  #   W1.inv.temp		<- lapply(Z.temp, function(x) crossprod(t(crossprod(x, H_i.temp)), x))
  # }

  if(w.mat != "identity"){
    W1.inv.temp		<- lapply(Z.temp, function(x) crossprod(t(crossprod(as.matrix(x), as.matrix(H_i.temp))), as.matrix(x)))
  }





  ## if(use.W_i){
  ##   N_i.temp		<- rapply(lapply(W1.inv.temp, FUN = as.matrix), f = function(x) ifelse(x!=0, 1, x), how = "replace")
  ##   N_i			<- Reduce("+", N_i.temp)
  ## } else{
  ##   N_i			<- n
  ## }
  ##
  ## W1.inv				<- (Reduce("+", W1.inv.temp))/N_i
  ## W1.inv[!is.finite(W1.inv)]	<- 0


  # W1.inv				<- (Reduce("+", W1.inv.temp))
  W1.inv				<- (1/n)*(Reduce("+", W1.inv.temp))
  W1.inv[!is.finite(W1.inv)]	<- 0


  if(use.mc.nonlin & w.mat.stata){
    if(use.mc.diff){
      diag(W1.inv[(n.inst[1,"inst.diff"] + 1):(n.inst[1,"inst.diff"] + n.inst[1,"inst.nl"]),
                  (n.inst[1,"inst.diff"] + 1):(n.inst[1,"inst.diff"] + n.inst[1,"inst.nl"])])		<- 1
    } else{
      diag(W1.inv[1:n.inst[1,"inst.nl"],1:n.inst[1,"inst.nl"]])		<- 1
    }
  }


  if(inst.thresh < sum(n.inst)){
    W1		<- MASS::ginv(as.matrix(W1.inv))
    #   W1		<- matlib::Ginv(as.matrix(W1.inv))
    #   W1		<- solve(qr(as.matrix(W1.inv)), LAPACK = TRUE)
    #   W1.alt2		<- solve(qr(as.matrix(W1.inv)), LAPACK = TRUE)		# [M:] ~ R-equivalent to the function used in Stata to obtain the pseudoinverse; calculations in R show that the matrix does not meet the requirements for a pseudoinverse!!
  } else{
    W1		<- MASS::ginv(as.matrix(W1.inv))
    #   W1		<- matlib::Ginv(as.matrix(W1.inv))
    #   W1		<- solve(as.matrix(W1.inv))
    #   W1.alt		<- solve(W1.inv)
  }
  ########################################
  # [M:] the 'pgmm' function uses the minimum eigenvalue to determine if a generalized inverse is to be used;
  #	the following code is taken from 'pgmm':
  #-------
  # minevA2 <- min(abs(Re(eigen(A2)$values)))
  #  eps <- 1E-9
  #  if (minevA2 < eps){
  #    SA2 <- ginv(A2)
  #    warning("a general inverse is used")
  #  }
  #  else SA2 <- solve(A2)
  ########################################


  #########################################
  # Calculations on pseudoinverse
  ##
  #
  # 1) A %*% p.invA %A% = A
  # sum(abs(W1.inv %*% ginv(as.matrix(W1.inv)) %*% W1.inv - W1.inv))				# ~fulfilled
  # sum(abs(W1.inv %*% Ginv(as.matrix(W1.inv)) %*% W1.inv - W1.inv))				# not fulfilled
  # sum(abs(W1.inv %*% solve(W1.inv) %*% W1.inv - W1.inv))						# ~fulfilled
  # sum(abs(W1.inv %*% solve(qr(as.matrix(W1.inv)), LAPACK = TRUE) %*% W1.inv - W1.inv))	# ~fulfilled
  #
  # --> not fulfilled for 'Ginv()'
  #
  #
  # 2) p.invA %*% A %*% p.invA = p.invA
  # sum(abs(ginv(as.matrix(W1.inv)) %*% W1.inv %*% ginv(as.matrix(W1.inv)) - ginv(as.matrix(W1.inv)) ))		# ~fulfilled
  # sum(abs(Ginv(as.matrix(W1.inv)) %*% W1.inv %*% Ginv(as.matrix(W1.inv)) - Ginv(as.matrix(W1.inv)) ))		# ~fulfilled
  # sum(abs(solve(W1.inv) %*% W1.inv %*% solve(W1.inv) - solve(W1.inv) ))							# ~fulfilled
  # sum(abs(solve(qr(as.matrix(W1.inv)), LAPACK = TRUE) %*% W1.inv %*% solve(qr(as.matrix(W1.inv)), LAPACK = TRUE) - solve(qr(as.matrix(W1.inv)), LAPACK = TRUE) ))		# ~fulfilled
  #
  # --> fulfilled for all functions
  #
  #
  # 3) (p.invA %*% A)' = p.invA %*% A
  # sum(abs( t(W1.inv %*% ginv(as.matrix(W1.inv))) - (W1.inv %*% ginv(as.matrix(W1.inv))) ))	# ~fulfilled
  # sum(abs( t(W1.inv %*% Ginv(as.matrix(W1.inv))) - (W1.inv %*% Ginv(as.matrix(W1.inv))) ))	# not fulfilled
  # sum(abs( t(W1.inv %*% solve(W1.inv)) - (W1.inv %*% solve(W1.inv)) ))					# ~fulfilled
  # sum(abs( t(W1.inv %*% solve(qr(as.matrix(W1.inv)), LAPACK = TRUE)) - (W1.inv %*% solve(qr(as.matrix(W1.inv)), LAPACK = TRUE)) ))		# ~fulfilled
  #
  # --> not fulfilled for 'Ginv()'
  #
  #
  # 4) (p.invA %*% A)' = p.invA %*% A
  # sum(abs( t(ginv(as.matrix(W1.inv)) %*% W1.inv) - (ginv(as.matrix(W1.inv)) %*% W1.inv) ))	# ~fulfilled
  # sum(abs( t(Ginv(as.matrix(W1.inv)) %*% W1.inv) - (Ginv(as.matrix(W1.inv)) %*% W1.inv) ))	# not fulfilled
  # sum(abs( t(solve(W1.inv) %*% W1.inv) - (solve(W1.inv) %*% W1.inv) ))					# ~fulfilled
  # sum(abs( t(solve(qr(as.matrix(W1.inv)), LAPACK = TRUE) %*% W1.inv) - (solve(qr(as.matrix(W1.inv)), LAPACK = TRUE) %*% W1.inv) ))		# ~fulfilled
  #
  # --> not fulfilled for 'Ginv()'
  #
  #########################################

  # assign("H_i", H_i.temp, pos = sys.frame(which = -1))
  assign("H_i", H_i.temp, envir = env)
  # assign("H_i", H_i.temp, pos = 1)
  # assign("H_i", H_i.temp, envir = env)

  return(W1)

}































#' @keywords internal
#'
Wtwostep.fct		<- function(
  Sj.0
  ,Z.temp
  ,n.inst
  ,inst.thresh
){

  t.SjSj	<- lapply(Sj.0, function(x) outer(x, x))

  Wj.inv	<- mapply(function(x, y) Matrix::crossprod(Matrix::t(Matrix::crossprod(x, y)), x), Z.temp, t.SjSj, SIMPLIFY = FALSE)



  Wj.inv	<- Reduce("+", Wj.inv)
  # Wj.inv				<- Reduce("+", Wj.inv)/n
  ##Wj.inv				<- (1/N_i)*(Reduce("+", Wj.inv))
  ##Wj.inv[!is.finite(Wj.inv)]	<- 0



  # if(inst.thresh > sum(n.inst)){									#[M:] use generalized inverse when instrument count is higher than cross-section dimension
  #   Wj		<- MASS::ginv(Wj.inv)
  #
  #   warning("Instrument count is equal to cross-section dimension (i.e., likely too high). A generalized inverse for therefore used to compute the j-step weighting matrix.")
  #
  # } else{
  Wj		<- MASS::ginv(as.matrix(Wj.inv))
  # }

  return(Wj)

}















































###
###	Function for computation of objective function for stepwise GMM-estimator (and computation of estimated part of moment conditions S1 before)
###



### Compute parameter independent part required for GMM objective function









#' @keywords internal
#'
gmmDat.fct		<- function(
  dat.na
  ,n
  ,Time
  ,varname.y
  ,varname.reg.estParam
){

  ###	Compute y_m1, y_tm1 (i.e., y_{t-1}) and X_t (for first version of nonlinear m.c.)
  #             y_T, y_Tm1 (i.e., y_{T-1}) and X_T (for second version of nonlinear m.c.)

  gmmDat		<- list()

  # gmmDat$n		<- n
  # gmmDat$T		<- Time
  gmmDat$dat.na	<- dat.na


  gmmDat$y_m1	<- dat.na[-( ((0:(n - 1))*Time) + rep(1, times = n) ), varname.y]
  gmmDat$y_tm1	<- dat.na[-( (1:n)*Time - rep(0, times = n) ), varname.y]

  gmmDat$X_m1	<- dat.na[-( ((0:(n - 1))*Time) + rep(1, times = n) ), varname.reg.estParam]
  gmmDat$X_tm1	<- dat.na[-((1:n)*Time), varname.reg.estParam]



  gmmDat$dy		<- gmmDat$y_m1 - gmmDat$y_tm1											# [M:] vector of length T-1


  gmmDat$dX		<- gmmDat$X_m1 - gmmDat$X_tm1


  #	Note:	(1:n)*(T - 1)	is index of all last (time period for each n) observations w.r.t. to the delta-vectors
  #	Note:	((0:(n - 1))*(T - 1)) + 1	is index of all first (time period for each n) observations w.r.t. to the delta-vectors
  # [M: one observation is lost due to differencing --> delta vector is 'N' elements shorter than dataset; 3rd to T-th period are required for each individual]


  return(gmmDat)

}















































###	Compute and return objective function value






#' @keywords internal
#'
gmmObj.fct		<- function(
  param
  ,j
  ,y_m1
  ,X_m1
  ,dy
  ,dX
  ,varname.reg.estParam
  ,n
  ,Time
  ,include.y
  ,varname.y
  ,use.mc.diff
  ,use.mc.nonlin
  ,use.mc.nonlinAS
  ,use.mc.lev
  ,dum.diff
  ,fur.con.diff
  ,max.lagTerms
  ,maxLags.y
  ,end.reg
  ,ex.reg
  ,pre.reg
  ,dum.lev
  ,fur.con.lev
  # ,mc.ref.t
  # ,mc.ref.T
  ,Z.temp
  # ,N_i
  ,W
  ,env
){

  ###	Compute y_m1, y_tm1 (i.e., y_{t-1}) and X_t (for first version of nonlinear m.c.)
  #             y_T, y_Tm1 (i.e., y_{T-1}) and X_T (for second version of nonlinear m.c.)

  gmmDat.parDep	<- list()


  ###	Compute u.hat_t

  gmmDat.parDep$fitted.lev	<- crossprod(t(X_m1), param)
  gmmDat.parDep$u.hat_t		<- y_m1 - gmmDat.parDep$fitted							# [M:] vector of length T-1






  ###	Compute du.hat

  #	Note:	(1:n)*(T - 1)	is index of all last (time period for each n) observations w.r.t. to the delta-vectors
  #	Note:	((0:(n - 1))*(T - 1)) + 1	is index of all first (time period for each n) observations w.r.t. to the delta-vectors
  # [M: one observation is lost due to differencing --> delta vector is 'N' elements shorter than dataset; 3rd to T-th period are required for each individual]


  gmmDat.parDep$fitted.diff	<- if(length(varname.reg.estParam) == 1){
    crossprod(t(dX[-(((0:(n - 1))*(Time - 1)) + 1)]), param)
  } else{
    crossprod(t(dX[-(((0:(n - 1))*(Time - 1)) + 1), ]), param)
  }

  gmmDat.parDep$du.hat		<- dy[-(((0:(n - 1))*(Time - 1)) + 1)] - gmmDat.parDep$fitted.diff						#[M:] vector of length T-2







  ###	Arrange parameter dependent part of moment conditions
  mc.arrange	<- function(	# rearrangement function
    i
    #    ,use.mc.diff
    #    ,use.mc.nonlin
    #    ,use.mc.nonlinAS
    #    ,use.mc.lev
    #    ,dum.diff
    #    ,fur.con.diff
    #    ,max.lagTerms
    #    ,T
    #    ,end.reg
    #    ,ex.reg
    #    ,pre.reg
    #    ,dum.lev
    #    ,fur.con.lev
  ){
    #    if(mc.ref.t){
    if(use.mc.diff | dum.diff | fur.con.diff){
      u.vec.1_diff			<- gmmDat.parDep$du.hat[(Time-2)*(i-1) + (max.lagTerms:(Time-2))]
      y.vec.1_diff			<- gmmDat.parDep$fitted.diff[(Time-2)*(i-1) + (max.lagTerms:(Time-2))]



    }

    if(use.mc.nonlin){
      if(use.mc.nonlinAS){
#        u.vec.2_lev.diff	<- rep(gmmDat.parDep$u.hat_t[(Time-1) + (i-1)*(Time-1)], times = length(max.lagTerms:(Time-3) + (i-1)*(Time-2)))*
        u.vec.2_lev.diff	<- rep(gmmDat.parDep$u.hat_t[(Time-1) + (i-1)*(Time-1)], times = maxLags.y - max.lagTerms -1)*
          gmmDat.parDep$du.hat[max.lagTerms:(Time-3) + (i-1)*(Time-2)][if(maxLags.y - (max.lagTerms+2) + 1 < Time - (max.lagTerms+2)){-(1:(Time - (max.lagTerms+2) - (maxLags.y - (max.lagTerms+2)+1)))}]
        y.vec.2_lev.diff	<- gmmDat.parDep$fitted.diff[max.lagTerms:(Time-3) + (i-1)*(Time-2)][if(maxLags.y - (max.lagTerms+2) + 1 < Time - (max.lagTerms+2)){-(1:(Time - (max.lagTerms+2) - (maxLags.y - (max.lagTerms+2)+1)))}]

      } else{
        u.vec.2_lev.diff	<- gmmDat.parDep$u.hat_t[(max.lagTerms + 2):(Time-1) + (i-1)*(Time-1)]*
          gmmDat.parDep$du.hat[max.lagTerms:(Time-3) + (i-1)*(Time-2)]
        y.vec.2_lev.diff	<- gmmDat.parDep$fitted.diff[max.lagTerms:(Time-3) + (i-1)*(Time-2)]
      }
    }

    if(use.mc.lev & (include.y | end.reg)){
      u.vec.3_lev		<- gmmDat.parDep$u.hat_t[(max(2,max.lagTerms)):(Time-1) + (i-1)*(Time-1)]
#      u.vec.3_lev		<- gmmDat.parDep$u.hat_t[(max.lagTerms):(Time-1) + (i-1)*(Time-1)]
      y.vec.3_lev		<- gmmDat.parDep$fitted.lev[(max(2,max.lagTerms)):(Time-1) + (i-1)*(Time-1)]
#      y.vec.3_lev		<- gmmDat.parDep$fitted.lev[(max.lagTerms):(Time-1) + (i-1)*(Time-1)]
    }

    if((use.mc.lev & (include.y | end.reg) & (ex.reg | pre.reg)) | dum.lev | fur.con.lev){
      u.vec.3_lev		<- gmmDat.parDep$u.hat_t[max.lagTerms:(Time-1) + (i-1)*(Time-1)]
      y.vec.3_lev		<- gmmDat.parDep$fitted.lev[max.lagTerms:(Time-1) + (i-1)*(Time-1)]
    }

    #    }

    u.vec		<- do.call(what = "c", args = mget(ls(pattern = "u.vec.")))
    y.vec		<- do.call(what = "c", args = mget(ls(pattern = "y.vec.")))
    names(u.vec)	<- NULL
    names(y.vec)	<- NULL

    return(list(u.vec = u.vec, y.vec = y.vec))

  }


  Sy.temp				<- sapply(X = 1:n, FUN = mc.arrange, simplify = FALSE)
  S.temp.zeros			<- lapply(lapply(Sy.temp, `[[`, 1), function(x) ifelse(is.na(x), yes = 0, no = x))

  #  assign("Szero.j", S.temp.zeros, pos = sys.frame(which = -2))
#  assign("Szero.j", S.temp.zeros, envir = env)
  #  assign("Szero.j", S.temp.zeros, pos = 1)
  #  assign("Szero.j", S.temp.zeros, envir = parent.frame())

  fitted.temp			<- lapply(Sy.temp, `[[`, 2)

  #  assign("fitted.j", fitted.temp, pos = sys.frame(which = 2))
#  assign("fitted.j", fitted.temp, envir = env)
  #  assign("fitted.j", fitted.temp, pos = 1)
  #  assign("fitted.j", fitted.temp, envir = parent.frame())


  M.mean				<- mapply(function(x, y) Matrix::crossprod(x, y), Z.temp, S.temp.zeros, SIMPLIFY = FALSE)
  ##   M.mean				<- Reduce("+", M.mean)/(diag(N_i))
  ##   M.mean[is.na(M1.mean)]	<- 0
  M.mean				<- Reduce("+", M.mean)
  #   M.mean				<- Reduce("+", M.mean)/n


  objective				<- Matrix::crossprod(Matrix::t(Matrix::crossprod(M.mean, W)), M.mean)

  return(objective[1, 1])

}



















































###
### Helper function for computing closed form estimates
###









#' @keywords internal
#'
sub.clForm.fct		<- function(
  i
  ,varname.i
  ,varname
  ,varname.y
  ,max.lagTerms
  ,maxLags.y
  ,Time
  ,data.temp
  ,use.mc.diff
  ,dum.diff
  ,fur.con.diff
  ,use.mc.lev
  ,dum.lev
  ,fur.con.lev
  ,use.mc.nonlin
  ,use.mc.nonlinAS
  ,include.x
  ,pre.reg
  ,ex.reg
){

  if(use.mc.diff | dum.diff | fur.con.diff){
    dat.temp_1diff				<- apply(X = data.temp[-c(1:(max.lagTerms)), ], MARGIN = 2, FUN = diff, args = list(differences=1)) *
      as.logical( ( (diff(data.temp[, varname.y], differences = max.lagTerms + 1)) * (diff(data.temp[, varname.y], differences = max.lagTerms + 1)) + 1) )
    colnames(dat.temp_1diff)			<- NULL
    rownames(dat.temp_1diff)			<- NULL
  }
  if(use.mc.nonlin){
    if(use.mc.nonlinAS){
      dat.temp_2nl			<- apply(X = data.temp[-c(1:(max.lagTerms), Time), ], MARGIN = 2, FUN = diff, args = list(differences=1)) *
        as.logical( ( (diff(data.temp[, varname.y], differences = max.lagTerms + 2)) * (diff(data.temp[, varname.y], differences = max.lagTerms + 2)) + 1) )
      dat.temp_2nl      <- dat.temp_2nl[if(maxLags.y - (max.lagTerms+2) + 1 < Time - (max.lagTerms+2)){-(1:(Time - (max.lagTerms+2) - (maxLags.y - (max.lagTerms+2)+1)))}, ]
    } else{
      dat.temp_2nl					<- apply(X = data.temp[-c(1:(max.lagTerms), Time), ], MARGIN = 2, FUN = diff, args = list(differences=1)) *
        as.logical( ( (diff(data.temp[, varname.y], differences = max.lagTerms + 2)) * (diff(data.temp[, varname.y], differences = max.lagTerms + 2)) + 1) )
    }
    colnames(dat.temp_2nl)			<- NULL
    rownames(dat.temp_2nl)			<- NULL
  }
  if(use.mc.lev | dum.lev | fur.con.lev){
    if(max.lagTerms == 1 & (dum.lev | fur.con.lev | (include.x & (pre.reg | ex.reg)))){
      dat.temp_3lev					<- as.matrix(data.temp[-c(1:(max.lagTerms)), ]) *
        as.logical( ( diff(data.temp[, varname.y], differences = max.lagTerms) * is.na(diff(data.temp[, varname.y], differences = max.lagTerms))) + 1 )
    } else{
      if(pre.reg|ex.reg){
        dat.temp_3lev					<- as.matrix(data.temp[-c(1:(max.lagTerms-1)), ]) *
          as.logical( ( diff(data.temp[, varname.y], differences = (max.lagTerms-1)) * is.na(diff(data.temp[, varname.y], differences = (max.lagTerms-1))) + 1 ) )

      } else{
        dat.temp_3lev					<- as.matrix(data.temp[-c(1:max(2,max.lagTerms)), ]) *
          as.logical( ( diff(data.temp[, varname.y], differences = max(2,max.lagTerms)) * is.na(diff(data.temp[, varname.y], differences = max(2,max.lagTerms))) + 1 ) )
      }
    }
    colnames(dat.temp_3lev)			<- NULL
    rownames(dat.temp_3lev)			<- NULL
  }
  dat.temp						<- do.call(what = rbind, args = mget(ls(pattern = "dat.temp_")))
  reg.temp						<- dat.temp[, !(varname %in% varname.y)]
  dep.temp						<- dat.temp[, varname %in% varname.y]

  return(list(reg.temp = Matrix::Matrix(reg.temp), dep.temp = Matrix::Matrix(dep.temp)))

}

























#' @keywords internal
#'
dat.closedFormExpand.fct		<- function(
  i
  ,dat.na
  ,varname.i
#  ,varname.reg.instr
#  ,varname.reg.toInstr
  ,varname.y
  ,varname.reg.estParam
  ,varname.reg
  ,use.mc.diff
  ,use.mc.lev
  ,use.mc.nonlin
  ,use.mc.nonlinAS
  ,dum.diff
  ,dum.lev
  ,fur.con.diff
  ,fur.con.lev
  ,max.lagTerms
  ,maxLags.y
  ,Time
  ,include.x
  ,pre.reg
  ,ex.reg
){

  varnames.temp	 <- c(varname.reg.estParam, varname.y)

#  data.temp		<- dat.na[dat.na[, varname.i] == as.numeric(as.factor(i)), varnames.temp]
  data.temp		<- dat.na[dat.na[, varname.i] == as.numeric(i), varnames.temp]

  dat.temp		<- do.call(what = sub.clForm.fct, args = list(i = i, varname.i = varname.i, varname = varnames.temp, varname.y = varname.y
                                                          ,max.lagTerms = max.lagTerms, maxLags.y = maxLags.y, Time = Time, data.temp = data.temp, use.mc.diff = use.mc.diff, dum.diff = dum.diff, fur.con.diff = fur.con.diff
                                                          ,use.mc.lev = use.mc.lev, dum.lev = dum.lev, fur.con.lev = fur.con.lev, use.mc.nonlin = use.mc.nonlin, use.mc.nonlinAS = use.mc.nonlinAS
                                                          ,include.x = include.x, pre.reg = pre.reg, ex.reg = ex.reg))

  return(dat.temp)
}













#' @keywords internal
#'
dat.expand.fct		<- function(
  i
  ,dat.na
  ,varname.i
#  ,varname.reg.instr
#  ,varname.reg.toInstr
  ,varname.y
  ,varname.reg.estParam
  ,max.lagTerms
  ,Time
){

  varnames.temp	<- c(varname.reg.estParam, varname.y)

#  data.temp		<- dat.na[dat.na[, varname.i] == as.numeric(as.factor(i)), varnames.temp]
  data.temp		<- dat.na[dat.na[, varname.i] == as.numeric(i), varnames.temp]

  dat.temp		<- data.temp[-c(1:max.lagTerms), ]

  reg.temp						<- as.matrix(dat.temp[, !(varnames.temp %in% varname.y)])
  colnames(reg.temp)  <- NULL
  rownames(reg.temp)  <- NULL
  dep.temp						<- as.matrix(dat.temp[, varnames.temp %in% varname.y, drop = FALSE])
  colnames(dep.temp)  <- NULL
  rownames(dep.temp)  <- NULL

  return(list(reg.temp = Matrix::Matrix(reg.temp), dep.temp = Matrix::Matrix(dep.temp)))
}











###
### Helper function to check arguments of estimation function
###

if(FALSE){

#' @keywords internal
#'
checkArgs		<- function(
    dat
    ,varname.t
    ,use.mc.diff
    ,use.mc.lev
    ,use.mc.nonlin
    ,include.x
    ,varname.reg.end
    ,varname.reg.pre
    ,varname.reg.ex
    ,fur.con
    ,varname.reg.fur
    ,fur.con.diff
    ,fur.con.lev
    ,include.x.instr
    ,include.x.toInstr
    ,instr.reg
    ,toInstr.reg
    ,varname.reg.instr
    ,varname.reg.toInstr
    ,instr.reg.ex.expand
    ,include.dum
    ,varname.dum
    ,dum.diff
    ,dum.lev
){

 if((use.mc.diff | use.mc.lev) && (length(unique(dat[, varname.t])) < 3)){
   stop("Insufficient number of time periods to derive linear moment conditions.")
 }
 if(use.mc.nonlin && (length(unique(dat[, varname.t])) < 4)){
   stop("Insufficient number of time periods to derive nonlinear moment conditions.")
 }



 if(include.x && (is.null(varname.reg.end) & is.null(varname.reg.pre) & is.null(varname.reg.ex))
 ){
 #  assign(x = include.x, value = "FALSE", envir = pdynmc::pdynmc)
   include.x		<- FALSE
   warning("Covariates (and types) from which additional instruments should be derived not given; 'include.x' was therefore set to FALSE.")
 }

 if(!(include.x) && !(is.null(varname.reg.end) | is.null(varname.reg.pre) | is.null(varname.reg.ex))
 ){
   suppressWarnings(rm(varname.reg.end, varname.reg.pre, varname.reg.ex))
   warning("Covariates (and types) specified, while no instruments are supposed to be derived from covariates; argument(s) specifying the name (and type) of covariates was therefore ignored.")
 }


 if(fur.con && is.null(varname.reg.fur)
 ){
   fur.con		  <- FALSE
   fur.con.diff <- FALSE
   fur.con.lev  <- FALSE
   warning("No further controls given; 'fur.con' was therefore set to FALSE.")
 }

 if(!fur.con){
   fur.con.diff <- FALSE
   fur.con.lev <- FALSE
 }

 if(!(fur.con) && !(is.null(varname.reg.fur))
 ){
   suppressWarnings(rm(varname.reg.fur))
   fur.con.diff <- FALSE
   fur.con.lev  <- FALSE
   warning("Further controls given, while further controls are not supposed to be included; argument specifying the further controls was therefore ignored.")
 }

 if(fur.con){
   if((is.null(fur.con.diff) & is.null(fur.con.lev)) | (!fur.con.diff & !fur.con.lev)){
     fur.con.diff		<- FALSE
     fur.con.lev		<- TRUE
     warning("Options 'fur.con.diff' and 'fur.con.lev' not specified; 'fur.con.lev' was therefore set to TRUE.")
   }
   if(fur.con.diff & is.null(fur.con.lev)){
     fur.con.lev		<- FALSE
     warning("Option 'fur.con.lev' not specified; option was therefore set to FALSE.")
   }
   if(fur.con.lev & is.null(fur.con.diff)){
     fur.con.diff	<- FALSE
     warning("Option 'fur.con.diff' not specified; option was therefore set to FALSE.")
   }
 }
 if(!fur.con && !((is.null(fur.con.diff) & is.null(fur.con.lev)) | (is.null(fur.con.diff) | is.null(fur.con.lev))) ){
   if(fur.con.diff){
     fur.con.diff <- FALSE
     warning("No further controls included; argument 'fur.con.diff' was therefore ignored.")
   }
   if(fur.con.lev){
     fur.con.lev <- FALSE
     warning("No further controls included; argument 'fur.con.lev' was therefore ignored.")
   }
 }


 if( (instr.reg == 0) & (toInstr.reg == 1) ){
   stop("No covariates given which should be used to instrument the endogenous covariate.")
 }

 if(include.x.instr & is.null(varname.reg.instr)
 ){
   include.x.instr	<- FALSE
   warning("No covariates given which should be used to derive instruments, while estimating no parameters for them; 'include.x.instr' was therefore set to FALSE.")
 }

 if(!(include.x.instr) & !(is.null(varname.reg.instr))
 ){
   suppressWarnings(rm(varname.reg.instr))
   warning("Covariates to be used as instruments specified, while these types of covariates are not supposed to be included; argument specifying these instruments was therefore ignored.")
 }

 if(include.x.toInstr & is.null(varname.reg.toInstr)
 ){
   include.x.toInstr	<- FALSE
   warning("No covariates given which should be instrumented; 'include.x.toInstr' was therefore set to FALSE.")
 }

 if(!(include.x.toInstr) & !(is.null(varname.reg.toInstr))
 ){
   suppressWarnings(rm(varname.reg.toInstr))
   warning("Further covariates to be instrumented specified, while these types of covariates are not supposed to be included; argument specifying these covariates was therefore ignored.")
 }

 if(inst.reg.ex.expand & !use.mc.diff & ( (!include.x.instr & is.null(varname.reg.ex)) | (is.null(varname.reg.ex)) ) ){
   inst.reg.ex.expand <- NULL
  #   warning("No exogenous covariates given; 'inst.reg.ex.expand' was therefore ignored.")
 }

 if(include.dum && is.null(varname.dum)
 ){
   include.dum		<- FALSE
   dum.diff       <- FALSE
   dum.lev        <- FALSE
   warning("No dummies given; 'include.dum' was therefore set to FALSE.")
 }

 if(!include.dum && !(is.null(varname.dum))
 ){
   suppressWarnings(rm(varname.dum))
   dum.diff       <- FALSE
   dum.lev        <- FALSE
   warning("Dummies given, while dummies are not supposed to be included; argument specifying the dummies was therefore ignored.")
 }

 if(include.dum){
   if((is.null(dum.diff) & is.null(dum.lev)) | (!dum.diff & !dum.lev)){
     dum.diff		<- FALSE
     dum.lev		<- TRUE
     warning("Options 'dum.diff' and 'dum.lev' not specified; 'dum.lev' was therefore set to TRUE.")
   }
   if(dum.diff & is.null(dum.lev)){
     dum.lev		<- FALSE
     warning("Option 'dum.lev' not specified; option was therefore set to FALSE.")
   }
   if(dum.lev & is.null(dum.diff)){
     dum.diff	<- FALSE
     warning("Option 'dum.diff' not specified; option was therefore set to FALSE.")
   }
 }
 if(!include.dum &  (!is.null(dum.diff) | !is.null(dum.lev)) ){
   if(!is.null(dum.diff)){
     dum.diff <- FALSE
     warning("No dummies included; argument 'dum.diff' was therefore ignored.")
   }
   if(!is.null(dum.lev)){
     dum.lev <- FALSE
     warning("No dummies included; argument 'dum.lev' was therefore ignored.")
   }
 }
 if(!include.dum &  (is.null(dum.diff) | is.null(dum.lev)) ){
   if(is.null(dum.diff)){
     dum.diff <- FALSE
   }
   if(is.null(dum.lev)){
     dum.lev <- FALSE
   }
 }

}

}





###
### Helper functions for expanding lag structure and dataset
###


if(FALSE){

#' @keywords internal
#'
varname.expand	<- function(
    varname
    ,lagTerms
){
  if(varname == varname.y){
    varname.reg.est.temp		<- paste("L", 1:lagTerms, ".", rep(varname, times = lagTerms), sep = "")
  } else{
    varname.reg.est.temp		<- paste("L", c(0:lagTerms), ".", rep(varname, times = lagTerms+1), sep = "")
  }
  return(varname.reg.est.temp)
}


#' @keywords internal
#'
#' @importFrom data.table shift
dat.na.lag		<- function(
    i
    ,varname
    ,lagTerms
){
  dat.na.lag.temp				<- data.table::shift(dat.na[dat.na[, varname.i] == i, varname], n = lagTerms, type = "lag")
  return(dat.na.lag.temp)
}


#' @keywords internal
#'
lag.expand		<- function(
    lagTerms
    ,varname
){
  if(varname == varname.y){
    lag.structure.temp			<- c(1:lagTerms)
  } else{
    lag.structure.temp			<- c(0:lagTerms)
  }
  return(lag.structure.temp)
}

}






###
### Helper function to check sparse matrix for collinearities
###


#' @keywords internal
#'
#' @importFrom Matrix colMeans
#' @importFrom Matrix colSums
#' @importFrom Matrix crossprod
#' @importFrom Matrix tcrossprod
#' @importFrom methods as
corSparse <- function(X, Y = NULL, cov = FALSE){

  X <- methods::as(X,"dgCMatrix")
  n <- nrow(X)
  muX <- Matrix::colMeans(X)

  if (!is.null(Y)) {
    stopifnot( nrow(X) == nrow(Y) )
    Y <- methods::as(Y,"dgCMatrix")
    muY <- Matrix::colMeans(Y)
    covmat <- ( as.matrix(Matrix::crossprod(X,Y)) - n*Matrix::tcrossprod(muX,muY) ) / (n-1)
    sdvecX <- sqrt( (Matrix::colSums(X^2) - n*muX^2) / (n-1) )
    sdvecY <- sqrt( (Matrix::colSums(Y^2) - n*muY^2) / (n-1) )
    cormat <- covmat/Matrix::tcrossprod(sdvecX,sdvecY)
  } else {
    covmat <- ( as.matrix(Matrix::crossprod(X)) - n*Matrix::tcrossprod(muX) ) / (n-1)
    sdvec <- sqrt(diag(covmat))
    cormat <- covmat/Matrix::tcrossprod(sdvec)
  }

  if (cov) {
    dimnames(covmat) <- NULL
    return(covmat)
  } else {
    dimnames(cormat) <- NULL
    return(cormat)
  }
}

# function 'corSparse' copied from R package 'qlcMatrix', which was scheduled to be archived 2023-11-29
#
# Pearson correlation matrix between columns of X, Y
# http://stackoverflow.com/questions/5888287/running-cor-or-any-variant-over-a-sparse-matrix-in-r
#
# covmat uses E[(X-muX)'(Y-muY)] = E[X'Y] - muX'muY
# with sample correction n/(n-1) this leads to cov = ( X'Y - n*muX'muY ) / (n-1)
#
# the sd in the case Y!=NULL uses E[X-mu]^2 = E[X^2]-mu^2
# with sample correction n/(n-1) this leads to sd^2 = ( X^2 - n*mu^2 ) / (n-1)
#
# Note that results larger than 1e4 x 1e4 will become very slow, because the resulting matrix is not sparse anymore.
#
#Further alternative:
#replace function 'corSparse()' from 'qlcMatrix'-package based on the following post (according to
#function documentation, the implementation is a slightly modified version of the post):
#https://stackoverflow.com/questions/5888287/running-cor-or-any-variant-over-a-sparse-matrix-in-r














