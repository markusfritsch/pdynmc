
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
###	Helper functions and side computations for creating Z (computation of (linear) HNR m.c. from equations in differences)
###





#' @keywords internal
#'
variable.fct	<- function(			# function that creates starting and end period when deriving instruments from data for endogenous variables
  varname
  ,i
  ,T.mcDiff
  ,lagTerms
  #   ,mc.ref.t
  ,Time
  ,varname.i
  ,dat
  ,dat.na
){
  #   if(mc.ref.t){

  ti.temp   <- rep(1, times = Time-lagTerms-1) + if(Time-lagTerms-1 - T.mcDiff > 0){c(rep(0, times = T.mcDiff - lagTerms), 1:(Time - T.mcDiff - 1))} else{rep(0, times = Time-lagTerms-1)}
  tend.temp <- lagTerms:(Time-2)
  Matrix::t(Matrix::bdiag(mapply(ti = ti.temp, t.end = tend.temp
                                , FUN = dat.fct, lagTerms = lagTerms, varname = varname
                                , MoreArgs = list(i = i
                                                 #			, mc.ref.t = mc.ref.t
                                                 , Time = Time, varname.i = varname.i, dat = dat, dat.na = dat.na)
                                , SIMPLIFY = FALSE)))
  #   } else{
  #     t(sapply(X = (Time - T.mcDiff - 1):(Time - 2), FUN = dat.fct, i = i, varname = varname))
  #   }
}















#' @keywords internal
#'
variable.pre.fct	<- function(			# function that creates starting and end period when deriving instruments from data for predetermined variables
  varname
  ,lagTerms
  ,T.mcDiff
  ,i
  #   ,mc.ref.t
  ,Time
  ,varname.i
  ,dat
  ,dat.na
){
  #   if(mc.ref.t){

  ti.temp   <- rep(1, times = Time-lagTerms-1) + if(Time-lagTerms-1 - T.mcDiff > 0){c(rep(0, times = T.mcDiff - lagTerms), 1:(Time - T.mcDiff - 1))} else{rep(0, times = Time-lagTerms-1)}
  tend.temp <- (lagTerms+1):(Time-1)

  Matrix::t(Matrix::bdiag(mapply(ti = ti.temp, t.end = tend.temp, FUN = dat.fct.pre, lagTerms = lagTerms, varname = varname
                                   , MoreArgs = list(i = i, Time = Time
                                                 #			, mc.ref.t = mc.ref.t
                                                 , varname.i = varname.i, dat = dat, dat.na = dat.na), SIMPLIFY = FALSE)))
  #   } else{
  #     t(sapply(X = (Time - T.mcDiff - 1):(Time - 1), FUN = dat.fct.pre, i = i, varname = varname))
  #   }
}

















#' @keywords internal
#'
variable.ex.fct	<- function(			# function that creates starting and end period when deriving instruments from data for exogenous variables
  varname
  ,lagTerms
  ,T.mcDiff
  ,i
  #   ,mc.ref.t
  ,Time
  ,varname.i
  ,inst.reg.ex.expand
  ,dat
  ,dat.na
){
  #   if(mc.ref.t){
#  t.start		<- if(Time > T.mcDiff){ c((Time-T.mcDiff):(Time-lagTerms-1)) } else{ rep(1, times = Time-lagTerms-1) }
#  t.start		<- if(Time > T.mcDiff){ c((Time-T.mcDiff):(Time)) } else{ rep(1, times = Time-lagTerms-1) }
#  t.start   <- rep(1, times = Time-lagTerms-1) + if(Time-T.mcDiff > 0){c(rep(0, times = Time - T.mcDiff), (1:(Time - T.mcDiff)))} else{0}
  if(inst.reg.ex.expand){
    ti        <- rep(1, times = Time-lagTerms-1) + if(Time-T.mcDiff > 0){c(rep(0, times = Time-lagTerms-1-(Time-T.mcDiff)), (1:(Time - T.mcDiff)))} else{0}
    t.end			<- ti + (T.mcDiff-1)
    t.req.i   <- 1:(Time-lagTerms-1)
    t.req.e   <- (1:(Time-lagTerms-1)) + (lagTerms+1)
  } else {
    ti   <- rep(1, times = Time-lagTerms-1) + if(Time-T.mcDiff > 0){c(rep(0, times = Time-lagTerms-1-(Time-T.mcDiff)), (1:(Time - T.mcDiff)))} else{0}
    t.end     <- (lagTerms+2):(Time)
    t.req.i   <- 1:(Time-lagTerms-1)
    t.req.e   <- (1:(Time-lagTerms-1)) + (lagTerms+1)
  }
#  t.end[t.end > Time]	<- Time
#  err.term.start	<- c((min(t.start) + lagTerms + 1):max(t.end))
  err.term.start	<- ti-1
  Matrix::t(Matrix::bdiag(mapply(ti = ti, t.end = t.end, err.term.start = err.term.start, t.req.i = t.req.i, t.req.e = t.req.e, FUN = dat.fct.ex, varname = varname
                                 , MoreArgs = list(i = i, Time = Time
                                                 #				, mc.ref.t = mc.ref.t
                                                 , varname.i = varname.i, dat = dat, dat.na = dat.na), SIMPLIFY = FALSE)))																		# [M:] use all m.c. in direction of T and cut at initial periods
  #   } else{
  #    t(sapply(X = (Time - T.mcDiff - 1):(Time), FUN = dat.fct.ex, i = i, varname = varname, ...))
  #   }
}























#' @keywords internal
#'
dat.fct		<- function(			# function that creates instruments based on
  ti
  ,t.end
  ,i												# renamed since 't()' is already a function
  ,lagTerms
  ,varname
  ,Time
  #   ,mc.ref.t
  ,varname.i
  ,dat
  ,dat.na
){
  #   if(mc.ref.t){
  dat[dat[, varname.i] == i, varname][ti:t.end]*					# if period t+1 and t+2 do not exist, t is not available as instrument
    (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][t.end-lagTerms+1] *
                         dat.na[dat.na[, varname.i] == i, varname][t.end] *
                         dat.na[dat.na[, varname.i] == i, varname][t.end+1] *
                         dat.na[dat.na[, varname.i] == i, varname][t.end+2])))
  #   } else{
  #     dat[dat[, varname.i] == i, varname][ti]*						# if period T, T-1 and T-2 do not exist, t is not available as instrument
  #     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][Time] *
  #                        dat.na[dat.na[, varname.i] == i, varname][Time-1] *
  #                        dat.na[dat.na[, varname.i] == i, varname][Time-2] *
  #                        dat.na[dat.na[, varname.i] == i, varname][Time-3])))
  #   }
}






















#' @keywords internal
#'
dat.fct.pre		<- function(
  ti
  ,t.end
  ,i												# renamed since 't' is already defined
  ,lagTerms
  ,varname
  ,Time
  #   ,mc.ref.t
  ,varname.i
  ,dat
  ,dat.na
){
  #   if(mc.ref.t){
  dat[dat[, varname.i] == i, varname][ti:t.end]*
    (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][t.end-lagTerms+1] *
                         dat.na[dat.na[, varname.i] == i, varname][t.end] *
                         dat.na[dat.na[, varname.i] == i, varname][t.end+1])))
  #   } else{
  #     dat[dat[, varname.i] == i, varname][ti]*
  #     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][Time] *
  #                        dat.na[dat.na[, varname.i] == i, varname][Time-1] *
  #                        dat.na[dat.na[, varname.i] == i, varname][Time-2])))
  #   }
}



















#' @keywords internal
#'
dat.fct.ex		<- function(
  ti
  ,t.end												# renamed since 't' is already defined
  ,t.req.i
  ,t.req.e
  ,err.term.start
  ,i
  ,varname
  ,Time
  #   ,mc.ref.t
  ,varname.i
  ,dat
  ,dat.na
){
  #   if(mc.ref.t){
  dat[dat[, varname.i] == i, varname][ti:t.end]*
    (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][ti:t.end] *
                         dat.na[dat.na[, varname.i] == i, varname][rep((err.term.start+2), times = length(ti:t.end))])))*
    as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][rep(t.req.i, times = length(ti:t.end))]))*
    as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][rep(t.req.e, times = length(ti:t.end))]))
#    (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][ti:t.end] *
#                        dat.na[dat.na[, varname.i] == i, varname][rep((err.term.start+2), times = length(ti:t.end))])))
  #                         dat.na[dat.na[, varname.i] == i, varname][rep((err.term.start-2), times = length(ti:t.end))])))
  #   } else{
  #     dat[dat[, varname.i] == i, varname][ti]*
  #     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][Time] *
  #                        dat.na[dat.na[, varname.i] == i, varname][Time-1] *
  #                        dat.na[dat.na[, varname.i] == i, varname][Time-2])))
  #   }
}



























###
###	Helper functions and side computations for creating Z (computation of (linear) Arellano-Bover m.c. from equations in levels)
###








#' @keywords internal
#'
LEV.fct	<- function(
  varname
  ,i
  ,T.mcLev
  ,lagTerms
  ,use.mc.diff
  ,inst.stata
  #   ,mc.ref.t
  ,Time
  ,varname.i
  ,dat
  ,dat.na
){
  ##   if(use.mc.diff){
  if(use.mc.diff & !(inst.stata)){
    #     if(mc.ref.t){
    ti.temp   <- max(2,lagTerms)
    tend.temp <- Time-1

    Matrix::Diagonal(do.call(what = datLEV.fct, args = list(ti = ti.temp, t.end = tend.temp, i = i, varname = varname, lagTerms = lagTerms, use.mc.diff = use.mc.diff, inst.stata = inst.stata
                                                            #				, mc.ref.t = mc.ref.t
                                                            , dat.na = dat.na, dat = dat, varname.i = varname.i, Time = Time)), n = Time-max(2,lagTerms))
    #     }
  } else{
    #     if(mc.ref.t){
    ti.temp   <- rep(max(2,lagTerms), times = Time-max(2,lagTerms)) + if(Time-max(2,lagTerms)-T.mcLev > 0){c(rep(0, times = T.mcLev-1), 1:(Time-max(2,lagTerms)-T.mcLev+1))} else{rep(0, times = Time-max(2,lagTerms))}
    tend.temp <- max(2,lagTerms):(Time-1)

    Matrix::t(Matrix::bdiag(mapply(ti = ti.temp, t.end = tend.temp, lagTerms = lagTerms, FUN = datLEV.fct, varname = varname,
                                   MoreArgs = list(i = i, use.mc.diff = use.mc.diff, inst.stata = inst.stata
                                                   #					, mc.ref.t = mc.ref.t
                                                   , dat.na = dat.na, dat = dat, varname.i = varname.i, Time = Time)) ))*
      as.vector(!is.na(diff(dat.na[dat.na[, varname.i] == i, varname][(max(2,lagTerms)-1):(Time-1)])))
    #     } else{
    #       t(mapply(ti = Time - T.mcLev, t.end = Time - 1, FUN = datLEV.fct, i = i, varname = varname,
    #		MoreArgs = list(use.mc.diff = use.mc.diff, inst.stata = inst.stata, mc.ref.t = mc.ref.t, dat.na = dat.na, dat = dat, varname.i = varname.i, Time = Time)))
    #     }
  }
}


































#' @keywords internal
#'
LEV.pre.fct	<- function(
  varname
  ,i
  ,T.mcLev
  ,lagTerms
  ,use.mc.diff
  ,inst.stata
  #   ,mc.ref.t
  ,Time
  ,varname.i
  ,dat
  ,dat.na
){
  if(use.mc.diff & !(inst.stata)){
    #     if(mc.ref.t){
    ti.temp   <- max(2,lagTerms)
    tend.temp <- Time

    Matrix::Diagonal(do.call(what = datLEV.pre.fct, args = list(ti = ti.temp, t.end = tend.temp, lagTerms = lagTerms, varname = varname, i = i, use.mc.diff = use.mc.diff, inst.stata = inst.stata
                                                                #			, mc.ref.t = mc.ref.t
                                                                , dat = dat, dat.na = dat.na, varname.i = varname.i, Time = Time)), n = Time-max(2,lagTerms)+1)
    #     }
  } else{
    #     if(mc.ref.t){
    ti.temp   <- rep(max(2,lagTerms), times = Time-max(2,lagTerms)+1) + if(Time-max(2,lagTerms)-T.mcLev > 0){c(rep(0, times = T.mcLev), 1:(Time-max(2,lagTerms)-T.mcLev+1))} else{rep(0, times = Time-max(2,lagTerms)+1)}
    tend.temp <- max(2,lagTerms):(Time)

    Matrix::t(Matrix::bdiag(mapply(ti = ti.temp, t.end = tend.temp, lagTerms = lagTerms, FUN = datLEV.pre.fct, varname = varname,
                                   MoreArgs = list(i = i, use.mc.diff = use.mc.diff, inst.stata = inst.stata
                                                   #					, mc.ref.t = mc.ref.t
                                                   , dat = dat, dat.na = dat.na, varname.i = varname.i, Time = Time))) )*
      as.vector(!is.na(diff(dat.na[dat.na[, varname.i] == i, varname][(lagTerms-1):Time])))
    #     } else{
    #       t(mapply(ti = Time - T.mcLev, t.end = Time, FUN = datLEV.pre.fct, varname = varname,
    #		MoreArgs = list(i = i, use.mc.diff = use.mc.diff, inst.stata = inst.stata
    #		, mc.ref.t = mc.ref.t
    #		, dat = dat, dat.na = dat.na, varname.i = varname.i, Time = Time)))
    #     }
  }
}



























#' @keywords internal
#'
datLEV.fct		<- function(
  ti
  ,t.end
  ,i
  ,lagTerms
  ,varname
  ,use.mc.diff
  ,inst.stata
  #   ,mc.ref.t
  ,Time
  ,varname.i
  ,dat
  ,dat.na
){

  if(use.mc.diff & !(inst.stata)){

    (dat[dat[, varname.i] == i, varname][ti:t.end]*
       as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][(ti - max(2,lagTerms) + 1):(t.end - max(2,lagTerms) + 1)]*
                           dat.na[dat[, varname.i] == i, varname][(ti):(t.end)]*
                           dat.na[dat[, varname.i] == i, varname][(ti + 1):(t.end + 1)] )) -
       dat[dat[, varname.i] == i, varname][(ti - 1):(t.end - 1)]*
       as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][(ti - max(2,lagTerms) + 1):(t.end - max(2,lagTerms) + 1)]*
                           dat.na[dat[, varname.i] == i, varname][(ti):(t.end)]*
                           dat.na[dat[, varname.i] == i, varname][(ti + 1):(t.end + 1)] )) ) *
      as.vector(!is.na(diff(dat.na[dat.na[, varname.i] == i, varname][(ti-1):(t.end)])))

  } else{

    (dat[dat[, varname.i] == i, varname][ti:t.end]*
       as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][t.end - max(2,lagTerms) + 1]*
                           dat.na[dat[, varname.i] == i, varname][t.end]*
                           dat.na[dat[, varname.i] == i, varname][t.end+1] )) -
       dat[dat[, varname.i] == i, varname][(ti - 1):(t.end - 1)]*
       as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][t.end - max(2,lagTerms) + 1]*
                           dat.na[dat[, varname.i] == i, varname][t.end]*
                           dat.na[dat[, varname.i] == i, varname][t.end+1] )) ) *
      as.vector(!is.na(diff(dat.na[dat.na[, varname.i] == i, varname][(ti-1):(t.end)])))
  }

  #   } else{
  #
  #     dat[dat[, varname.i] == i, varname][ti]*
  #     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][Time] *
  #                        dat.na[dat.na[, varname.i] == i, varname][Time-1] *
  #                        dat.na[dat.na[, varname.i] == i, varname][ti]))) -
  #     dat[dat[, varname.i] == i, varname][ti - 1]*
  #     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][Time] *
  #                        dat.na[dat.na[, varname.i] == i, varname][Time-1] *
  #                        dat.na[dat.na[, varname.i] == i, varname][ti - 1])))
  #   }
}






















#' @keywords internal
#'
datLEV.pre.fct		<- function(
  ti
  ,t.end
  ,i
  ,varname
  ,lagTerms
  ,use.mc.diff
  ,inst.stata
  #   ,mc.ref.t
  ,Time
  ,varname.i
  ,dat
  ,dat.na
){
  #   if(mc.ref.t){
  if(is.na(dat.na[dat.na[, varname.i] == i, varname][ti])){
    ti	= ti+1
    t.end	= t.end+1
  }

  if(use.mc.diff & !(inst.stata)){

    (dat[dat[, varname.i] == i, varname][(ti):t.end]*
       as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][(ti):(t.end)] )) -
     dat[dat[, varname.i] == i, varname][(ti-1):(t.end-1)]*
       as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][(ti-1):(t.end-1)] )) ) *
      as.vector(!is.na(diff(dat.na[dat.na[, varname.i] == i, varname][(ti-1):(t.end)])))

  } else{

    (dat[dat[, varname.i] == i, varname][(ti):t.end] *
       as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][t.end - 1]*
                           dat.na[dat[, varname.i] == i, varname][t.end] ))  -
       dat[dat[, varname.i] == i, varname][(ti-1):(t.end - 1)]*
       as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][t.end - 1]*
                           dat.na[dat[, varname.i] == i, varname][t.end] )) ) *
      as.vector(!is.na(diff(dat.na[dat.na[, varname.i] == i, varname][(ti-1):(t.end)])))
  }

  #   } else{
  #     dat[dat[, varname.i] == i, varname][ti]*
  #     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][Time] *
  #                        dat.na[dat.na[, varname.i] == i, varname][Time-1] *
  #                        dat.na[dat.na[, varname.i] == i, varname][ti]))) -
  #     dat[dat[, varname.i] == i, varname][ti - 1]*
  #     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][Time] *
  #                        dat.na[dat.na[, varname.i] == i, varname][Time-1] *
  #                        dat.na[dat.na[, varname.i] == i, varname][ti - 1])))
  #   }
}









































###
### Function for generating the instrument matrix
###







#' @keywords internal
#'
Z_i.fct	<- function(
  i
  ,Time
  ,varname.i
  #   ,mc.ref.t
  ,use.mc.diff
  ,use.mc.lev
  ,use.mc.nonlin
  ,use.mc.nonlinAS
  ,include.y
  ,varname.y
  ,inst.stata
  ,include.dum
  ,dum.diff
  ,dum.lev
  ,colnames.dum
  ,fur.con
  ,fur.con.diff
  ,fur.con.lev
  ,varname.reg.estParam.fur
  ,include.x
  ,end.reg
  ,varname.reg.end
  ,pre.reg
  ,varname.reg.pre
  ,ex.reg
  ,varname.reg.ex
  ,lagTerms.y
  ,maxLags.y
  ,max.lagTerms
  ,maxLags.reg.end
  ,maxLags.reg.pre
  ,maxLags.reg.ex
  ,inst.reg.ex.expand
  ,dat
  ,dat.na
){

  i <- as.numeric(as.factor(i))

  if(use.mc.diff){
    #     if(mc.ref.t){
    if(include.y){
      Z_i.mc.diff_end.y	<- do.call(what = "cbind", args = sapply(X = varname.y, FUN = variable.fct, i = i, T.mcDiff = maxLags.y,
                                                                 lagTerms = max.lagTerms
                                                                 #						, mc.ref.t = mc.ref.t
                                                                 , Time = Time, varname.i = varname.i, dat = dat, dat.na = dat.na) )
    }
    if(include.x){
      if(end.reg){
        if(length(varname.reg.end) == 1){
          Z_i.mc.diff_end.x	<- do.call(what = "cbind", args = sapply(FUN = variable.fct, varname.reg.end, i = i, T.mcDiff = maxLags.reg.end,
                                                                     lagTerms = max.lagTerms
                                                                     #						, mc.ref.t = mc.ref.t
                                                                     , Time = Time, varname.i = varname.i, dat = dat, dat.na = dat.na) )
        } else{
          Z_i.mc.diff_end.x	<- do.call(what = "cbind", args = mapply(FUN = variable.fct, varname.reg.end, T.mcDiff = maxLags.reg.end
                                                                   #						, mc.ref.t = mc.ref.t
                                                                   , MoreArgs = list(i = i, Time = Time, varname.i = varname.i, lagTerms = max.lagTerms
                                                                                     , dat = dat, dat.na = dat.na)) )
        }
      }
      if(pre.reg){
        if(length(varname.reg.pre) == 1){
          Z_i.mc.diff_pre	<- do.call(what = "cbind", args = sapply(FUN = variable.pre.fct, varname.reg.pre, i = i, T.mcDiff = maxLags.reg.pre,
                                                                   lagTerms = max.lagTerms
                                                                   #						, mc.ref.t = mc.ref.t
                                                                   , Time = Time, varname.i = varname.i, dat = dat, dat.na = dat.na) )
        } else{
          Z_i.mc.diff_pre	<- do.call(what = "cbind", args = mapply(FUN = variable.pre.fct, varname.reg.pre, T.mcDiff = maxLags.reg.pre
                                                                 #						, mc.ref.t = mc.ref.t
                                                                 , MoreArgs = list(i = i, Time = Time, varname.i = varname.i, lagTerms = max.lagTerms
                                                                                   , dat = dat, dat.na = dat.na)) )
        }
      }
      if(ex.reg){
        if(length(varname.reg.ex) == 1){
          Z_i.mc.diff_ex	<- do.call(what = "cbind", args = sapply(FUN = variable.ex.fct, varname.reg.ex, i = i, T.mcDiff = maxLags.reg.ex,
                                                                  lagTerms = max.lagTerms, inst.reg.ex.expand = inst.reg.ex.expand
                                                                  #						, mc.ref.t = mc.ref.t
                                                                  , Time = Time, varname.i = varname.i, dat = dat, dat.na = dat.na) )
        } else{
          Z_i.mc.diff_ex	<- do.call(what = "cbind", args = mapply(FUN = variable.ex.fct, varname.reg.ex, T.mcDiff = maxLags.reg.ex
                                                                #						, mc.ref.t = mc.ref.t
                                                                , MoreArgs = list(i = i, Time = Time, varname.i = varname.i, lagTerms = max.lagTerms
                                                                                  , inst.reg.ex.expand = inst.reg.ex.expand
                                                                                  , dat = dat, dat.na = dat.na)) )
        }
      }
    }
    #     }
    Z_i.mc.diff_temp	<- do.call(what = "cbind", args = mget(ls(pattern = "Z_i.mc.diff")))
    # Note that sequence of arrangement is in alphabetical order, i.e.:
    # 1. endogenous, 2. exogenous, 3. predetermined Variables.
    n.inst.diff	<- ncol(Z_i.mc.diff_temp)
    n.obs.diff	<- nrow(Z_i.mc.diff_temp)
  }

  if(use.mc.lev){
    #     if(mc.ref.t){
    if(include.y){
      Z_i.mc.lev_end.y	<- do.call(what = "cbind", args = sapply(X = varname.y, FUN = LEV.fct, i = i, T.mcLev = maxLags.y, lagTerms = max.lagTerms,
                                                                use.mc.diff = use.mc.diff, inst.stata = inst.stata
                                                                #						, mc.ref.t = mc.ref.t
                                                                , Time = Time, varname.i = varname.i, dat = dat, dat.na = dat.na) )
    }
    if(include.x){
      if(end.reg){
        if(length(varname.reg.end) == 1){
          Z_i.mc.lev_end.x	<- do.call(what = "cbind", args = sapply(FUN = LEV.fct, i = i, varname.reg.end, T.mcLev = maxLags.reg.end, lagTerms = max.lagTerms,
                                                                    use.mc.diff = use.mc.diff, inst.stata = inst.stata
                                                                    #						, mc.ref.t = mc.ref.t
                                                                    , Time = Time, varname.i = varname.i, dat = dat, dat.na = dat.na) )
        } else{
          Z_i.mc.lev_end.x	<- do.call(what = "cbind", args = mapply(FUN = LEV.fct, varname.reg.end, T.mcLev = maxLags.reg.end
                                                                  , MoreArgs = list(use.mc.diff = use.mc.diff, inst.stata = inst.stata
                                                                  #						, mc.ref.t = mc.ref.t
                                                                        , i = i, Time = Time, varname.i = varname.i, lagTerms = max.lagTerms
                                                                        , dat = dat, dat.na = dat.na)) )
        }
      }
      if(ex.reg | pre.reg){
        varname.ex.pre.temp <- c({if(!(is.null("varname.reg.ex"))){varname.reg.ex}}, {if(!(is.null("varname.reg.pre"))){varname.reg.pre}} )
        T.mcLev.temp    <- c({if(!(is.null("varname.reg.ex"))){maxLags.reg.ex - 1}}, {if(!(is.null("varname.reg.pre"))){maxLags.reg.pre}} )
        if(length(varname.ex.pre.temp) == 1){
          Z_i.mc.lev_ex.pre	<- do.call(what = "cbind", args = sapply(FUN = LEV.pre.fct, i = i, varname.ex.pre.temp, T.mcLev = T.mcLev.temp
                                                                    , use.mc.diff = use.mc.diff, inst.stata = inst.stata
                                                                     #						,mc.ref.t = mc.ref.t
                                                                    , Time = Time, varname.i = varname.i, lagTerms = max.lagTerms
                                                                    , dat = dat, dat.na = dat.na) )
        } else{
          Z_i.mc.lev_ex.pre	<- do.call(what = "cbind", args = mapply(FUN = LEV.pre.fct, varname.ex.pre.temp, T.mcLev = T.mcLev.temp
                                                                     ,MoreArgs = list(i = i, use.mc.diff = use.mc.diff, inst.stata = inst.stata
                                                                                      #						,mc.ref.t = mc.ref.t
                                                                        , Time = Time, varname.i = varname.i, lagTerms = max.lagTerms
                                                                        , dat = dat, dat.na = dat.na)) )
        }
      }
    }
    Z_i.mc.lev_end	<- do.call(what = "cbind", args = mget(ls(pattern = "Z_i.mc.lev_end")))
    if(include.x & (include.y | end.reg) & (ex.reg | pre.reg)){
      Z_i.mc.lev	<- cbind(rbind(0, Z_i.mc.lev_end), Z_i.mc.lev_ex.pre)
    } else{
      if((include.y | end.reg) & ((include.dum & dum.lev) | (fur.con & fur.con.lev))){
        if(max.lagTerms == 1){
          Z_i.mc.lev	<- rbind(0, Z_i.mc.lev_end)
        } else{
          Z_i.mc.lev	<- Z_i.mc.lev_end
        }
      } else{
        Z_i.mc.lev	<- Z_i.mc.lev_end
      }
    }
    #     }
    n.inst.lev	<- ncol(Z_i.mc.lev)
    n.obs.lev	<- nrow(Z_i.mc.lev)

    if(use.mc.diff){
      #       if(mc.ref.t){
      Z_i.temp		<- Matrix::bdiag(list(Z_i.mc.diff_temp, Z_i.mc.lev))
      #       }
    } else{
      #       if(mc.ref.t){
      Z_i.temp		<- Z_i.mc.lev
      #       }
    }
  }

  if(use.mc.nonlin){
    #     if(mc.ref.t){
    if(use.mc.nonlinAS){
      Z_i.mc.AS4	<- diag(as.numeric(!(is.na(diff(dat.na[dat[, varname.i] == i, varname.y], differences = max.lagTerms+2))) )[if(maxLags.y - (max.lagTerms+2) + 1 < Time - (max.lagTerms+2)){-(1:(Time - (max.lagTerms+2) - (maxLags.y - (max.lagTerms+2)+1)))}], nrow = Time - (max.lagTerms+2) - length((1:(Time - (max.lagTerms+2) - (maxLags.y - (max.lagTerms+2)+1)))))
    } else {
      Z_i.mc.AS4	<- diag(as.numeric(!(is.na(diff(dat.na[dat[, varname.i] == i, varname.y], differences = max.lagTerms+2))) ))
    }

    if(use.mc.diff & !(use.mc.lev)){
      Z_i.temp		<- Matrix::bdiag(list(Z_i.mc.diff_temp, Z_i.mc.AS4))
    }
    if(!(use.mc.diff) & use.mc.lev){
      Z_i.temp	<- Matrix::bdiag(Z_i.mc.AS4, Z_i.mc.lev)
    }
    if(!(use.mc.diff) & !(use.mc.lev)){
      Z_i.temp	<- Z_i.mc.AS4
    }

    if(use.mc.diff & use.mc.lev){
      Z_i.temp		<- Matrix::bdiag(Z_i.mc.diff_temp, Z_i.mc.AS4, Z_i.mc.lev)
    }
    #     }
    n.inst.nl		<- ncol(Z_i.mc.AS4)
    n.obs.nl		<- nrow(Z_i.mc.AS4)
  }
  if(!(use.mc.lev) & !(use.mc.nonlin)){
    Z_i.temp	<- Z_i.mc.diff_temp
  }


  if(include.dum){
    ind_vec.diff.row	<- is.na(diff(dat.na[dat[, varname.i] == i, varname.y][1:(Time)], differences = max.lagTerms+1) )
    ind_vec.lev.row	<- is.na(diff(dat.na[dat[, varname.i] == i, varname.y][1:(Time)], differences = max.lagTerms) )
    ind_vec.diff.col	<- is.na(diff(dat.na[dat[, varname.i] == i , varname.y][2:Time], differences = max.lagTerms) )
    ind_vec.lev.col	<- is.na(diff(dat.na[dat[, varname.i] == i , varname.y][1:Time], differences = max.lagTerms) )

    if(dum.lev){
      if(max.lagTerms > 1){
        Z_i.dum_4.lev				<- as.matrix(dat[dat[, varname.i] == i, colnames.dum[-c(1:(max.lagTerms-1))]][-c(1:max.lagTerms), ])
      } else{
        Z_i.dum_4.lev				<- as.matrix(dat[dat[, varname.i] == i, colnames.dum][-c(1:max.lagTerms), ])
      }
#      Z_i.dum_4.lev				<- as.matrix(dat[dat[, varname.i] == i, colnames.dum[if(max.lagTerms > 2){-c(1:max.lagTerms)} else{-1}]][-c(1:max.lagTerms), ])
      Z_i.dum_4.lev[ind_vec.lev.row, ]	<- 0
      Z_i.dum_4.lev[ ,ind_vec.lev.col]	<- 0
      colnames.dum_4.lev			<- colnames(Z_i.dum_4.lev)
      colnames(Z_i.dum_4.lev)		<- NULL
      rownames(Z_i.dum_4.lev)		<- NULL

      if(use.mc.nonlin){
        Z_i.dum_2.nl			<- matrix(0, ncol = ncol(Z_i.dum_4.lev), nrow = nrow(Z_i.mc.AS4))
        colnames.dum_2.nl		<- colnames.dum_4.lev
        colnames(Z_i.dum_2.nl)	<- NULL
      }
      if(dum.diff){
        Z_i.dum_1.diff			<- as.matrix(dat[dat[, varname.i] == i, colnames.dum[-c(1:max.lagTerms)]][(2+max.lagTerms):Time, ] -
                                        rbind(dat[dat[, varname.i] == i, colnames.dum[-c(1:max.lagTerms)]][(2+(max.lagTerms-1)):(Time-1), ]))
        colnames.dum_1.diff		<- paste("D.", colnames.dum_4.lev, sep = "")
        colnames(Z_i.dum_1.diff)	<- NULL
        rownames(Z_i.dum_1.diff)	<- NULL
      }
      if(dum.diff & dum.lev){
        if(use.mc.nonlin){
          Z_i.dum			<- Matrix::bdiag(Z_i.dum_1.diff, rbind(Z_i.dum_2.nl, Z_i.dum_4.lev))
        } else{
          Z_i.dum			<- do.call(what = Matrix::bdiag, mget(ls(pattern = "Z_i.dum_")))
        }
      } else{
        if((use.mc.diff | fur.con.diff) & !dum.diff){
          Z_i.dum_1.diff		<- matrix(0, ncol = ncol(Z_i.dum_4.lev), nrow = (Time-max.lagTerms-1))
          colnames.dum_1.diff	<- colnames(Z_i.dum_4.lev)
          if(use.mc.nonlin){
            Z_i.dum			<- rbind(Z_i.dum_1.diff, Z_i.dum_2.nl, Z_i.dum_4.lev)
          } else{
            Z_i.dum			<- rbind(Z_i.dum_1.diff, Z_i.dum_4.lev)
          }
        } else{
          if(length(ls(pattern = "Z_i.dum_")) == 1){
            Z_i.dum 			<- Z_i.dum_4.lev
          } else{
            Z_i.dum 			<- do.call(what = rbind, mget(ls(pattern = "Z_i.dum_")))
          }
        }
      }

    }

    if(dum.diff & !(dum.lev)){
      Z_i.dum_1.diff				<- as.matrix(dat[dat[, varname.i] == i, colnames.dum[-c(1:max.lagTerms)]][(2+max.lagTerms):Time, ] -
                                       rbind(dat[dat[, varname.i] == i, colnames.dum[-c(1:max.lagTerms)]][(2+(max.lagTerms-1)):(Time-1), ]))
      Z_i.dum_1.diff[ind_vec.diff.row, ]	<- 0
      Z_i.dum_1.diff[ ,ind_vec.diff.col]	<- 0
      colnames.dum_1.diff			    <- paste("D.", colnames(Z_i.dum_1.diff), sep = "")
      colnames(Z_i.dum_1.diff)		<- NULL
      rownames(Z_i.dum_1.diff)		<- NULL

      if(use.mc.nonlin){
        Z_i.dum_2.nl			<- matrix(0, ncol = ncol(Z_i.dum_1.diff), nrow = nrow(Z_i.mc.AS4))
        colnames.dum_2.nl		<- colnames(Z_i.dum_2.nl)
        colnames(Z_i.dum_2.nl)	<- NULL
      }
      if(use.mc.lev){
        if(fur.con.lev | ex.reg | pre.reg){
          Z_i.dum_4.lev		<- matrix(0, ncol = ncol(Z_i.dum_1.diff), nrow = (Time - max.lagTerms))
        } else{
          Z_i.dum_4.lev		<- matrix(0, ncol = ncol(Z_i.dum_1.diff), nrow = (Time - max(2,max.lagTerms)))
        }
        colnames.dum_4.lev		<- colnames(Z_i.dum_1.diff)
        colnames(Z_i.dum_4.lev)	<- NULL
      }

      Z_i.dum				<- do.call(what = "rbind", args = mget(ls(pattern = "Z_i.dum_")))
      rownames(Z_i.dum)		<- NULL

    }

#    if(use.mc.lev){
#      if((include.y | end.reg) & (ex.reg | pre.reg | dum.lev | fur.con.lev)){
#        Z_i.temp			<- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)),
#                            Z_i.temp )
#      }
#      if(!(include.y | end.reg) & (ex.reg | pre.reg | dum.lev | fur.con.lev)){
#        Z_i.temp			<- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)), Z_i.temp )
#      }
#      if((include.y | end.reg) & !(ex.reg | pre.reg | dum.lev | fur.con.lev) & (dum.diff | fur.con.diff)){
#        Z_i.temp			<- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)), Z_i.temp )
#      }
#    } else{
#      if((dum.diff | fur.con.diff) & !(dum.lev | fur.con.lev)){
#        Z_i.temp			<- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)), Z_i.temp)
#      }
#      if(!(dum.diff | fur.con.diff) & (dum.lev | fur.con.lev)){
#        Z_i.temp			<- rbind(Z_i.temp, matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)) )
#      }
#      if((dum.diff | fur.con.diff) & (dum.lev | fur.con.lev)){
#        if(use.mc.diff){
#          Z_i.temp		<- rbind(Z_i.temp, matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)))
#        }
#        if(!(use.mc.diff) & use.mc.nonlin){
#          Z_i.temp		<- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum_1.diff)),
#                             Z_i.temp,
#                             matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp) - nrow(Z_i.dum_1.diff)) )
#        }
#      }
#    }

    colnames_Z_i.dum		<- unique(as.vector(do.call(what = "c", mget(ls(pattern = "colnames.dum_")))))
    if(nrow(Z_i.temp) < nrow(Z_i.dum)){
      if(use.mc.lev){
        Z_i.temp    <- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)), Z_i.temp)
      } else{
        if(use.mc.diff){
          Z_i.temp    <- rbind(Z_i.temp, matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)))
        } else{
          if(use.mc.nonlin & !use.mc.diff & !use.mc.lev){
            if(dum.diff){
              Z_i.temp    <- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)), Z_i.temp)
            } else{
              Z_i.temp    <- rbind(Z_i.temp, matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)))
            }
          }
        }
      }
    }
    Z_i.temp			<- cbind(Z_i.temp, as.matrix(Z_i.dum))

    if(dum.diff & dum.lev){
      colnames_Z_i.dum <- colnames_Z_i.dum[-1]
      n.inst.dum			 <- c(length(get(ls(pattern = "colnames.dum_1"))) -1, length(get(ls(pattern = "colnames.dum_4"))))
    } else{
      if(dum.diff & !(dum.lev)){
        n.inst.dum		   <- length(get(ls(pattern = "colnames.dum_1")))
      }
      if(dum.lev & !(dum.diff)){
        n.inst.dum		 <- length(get(ls(pattern = "colnames.dum_4")))
      }
    }
  } else{
    colnames_Z_i.dum   <- NULL
  }



  if(fur.con){

    ind_vec.diff.row	<- is.na(diff(dat.na[dat[, varname.i] == i, varname.y][1:Time], differences = max.lagTerms+1) )
    ind_vec.lev.row	<- is.na(diff(dat.na[dat[, varname.i] == i, varname.y][1:(Time)], differences = max.lagTerms) )

    if(fur.con.diff){
      #       if(mc.ref.t){
      if(length(varname.reg.estParam.fur) == 1){
        Z_i.furCon.temp_diff				<- diff(as.matrix(dat.na[dat[, varname.i] == i, varname.reg.estParam.fur][-c(1:max.lagTerms)]), differences = 1)
        Z_i.furCon.temp_diff[ind_vec.diff.row]	<- 0

        colnames.fur.con.diff			        <- varname.reg.estParam.fur
        rownames(Z_i.furCon.temp_diff)		<- NULL
        colnames(Z_i.furCon.temp_diff)		<- NULL
      } else{
        Z_i.furCon.temp_diff				        <- diff(as.matrix(dat.na[dat[, varname.i] == i, varname.reg.estParam.fur][-c(1:max.lagTerms), ]), differences = 1)
        Z_i.furCon.temp_diff[ind_vec.diff.row, ]	<- 0

        colnames.fur.con.diff			        <- colnames(Z_i.furCon.temp_diff)
        rownames(Z_i.furCon.temp_diff)		<- NULL
        colnames(Z_i.furCon.temp_diff)		<- NULL
      }
    }

    if(fur.con.lev){
      #       if(mc.ref.t){
      if(length(varname.reg.estParam.fur) == 1){
        Z_i.furCon.temp_lev				            <- as.matrix(dat.na[dat[, varname.i] == i, varname.reg.estParam.fur][1:Time][-c(1:max.lagTerms)] )
        Z_i.furCon.temp_lev[ind_vec.lev.row]	<- 0

        colnames.fur.con.lev			      <- varname.reg.estParam.fur
        rownames(Z_i.furCon.temp_lev)		<- NULL
        colnames(Z_i.furCon.temp_lev)		<- NULL
      } else{
        Z_i.furCon.temp_lev				              <- as.matrix(dat.na[dat[, varname.i] == i, varname.reg.estParam.fur][1:Time, ][-c(1:max.lagTerms), ] )
        Z_i.furCon.temp_lev[ind_vec.lev.row, ]	<- 0

        colnames.fur.con.lev			      <- colnames(Z_i.furCon.temp_lev)
        rownames(Z_i.furCon.temp_lev)		<- NULL
        colnames(Z_i.furCon.temp_lev)		<- NULL
      }
    }

    if(fur.con.diff & fur.con.lev){
      if(length(varname.reg.estParam.fur) == 1){
        if(use.mc.nonlin){
          Z_i.furCon.diff  <- as.matrix(c(Z_i.furCon.temp_diff, rep(0, times = nrow(Z_i.furCon.temp_lev) + nrow(Z_i.mc.AS4))))
          Z_i.furCon.lev   <- as.matrix(c(rep(0, times = nrow(Z_i.furCon.temp_diff) + nrow(Z_i.mc.AS4)), Z_i.furCon.temp_lev))
        } else{
          Z_i.furCon.diff  <- as.matrix(c(Z_i.furCon.temp_diff, rep(0, times = nrow(Z_i.furCon.temp_lev))))
          Z_i.furCon.lev   <- as.matrix(c(rep(0, times = nrow(Z_i.furCon.temp_diff)), Z_i.furCon.temp_lev))
        }
      } else{
        if(use.mc.nonlin){
          Z_i.furCon.diff  <- rbind(Z_i.furCon.temp_diff, matrix(0, ncol = ncol(Z_i.furCon.temp_diff), nrow = nrow(Z_i.furCon.temp_lev) + nrow(Z_i.mc.AS4)))
          Z_i.furCon.lev   <- rbind(matrix(0, ncol = ncol(Z_i.furCon.temp_lev), nrow = nrow(Z_i.furCon.temp_diff) + nrow(Z_i.mc.AS4)), Z_i.furCon.temp_lev)
        } else{
          Z_i.furCon.diff  <- rbind(Z_i.furCon.temp_diff, matrix(0, ncol = ncol(Z_i.furCon.temp_diff), nrow = nrow(Z_i.furCon.temp_lev)))
          Z_i.furCon.lev   <- rbind(matrix(0, ncol = ncol(Z_i.furCon.temp_lev), nrow = nrow(Z_i.furCon.temp_diff)), Z_i.furCon.temp_lev)
        }
      }

      Z_i.furCon.temp		<- cbind(Z_i.furCon.diff, Z_i.furCon.lev)
      n.inst.furCon		<- c(length(get(ls(pattern = "colnames.fur.con.diff"))), length(get(ls(pattern = "colnames.fur.con.lev"))))
    } else{
      if(fur.con.diff){
        Z_i.furCon.temp	<- Z_i.furCon.temp_diff
        n.inst.furCon		<- length(get(ls(pattern = "colnames.fur.con.diff")))

#        if(dum.lev & !dum.diff){
#        if(dum.lev & !include.dum){
        if(nrow(Z_i.furCon.temp) != nrow(Z_i.temp)){
          if(!include.dum){
            if((use.mc.lev | use.mc.nonlin) & !use.mc.diff){
              Z_i.furCon.temp <- rbind(Z_i.furCon.temp, matrix(0, ncol = ncol(Z_i.furCon.temp), nrow = nrow(Z_i.temp)))
            }
            if(use.mc.diff & use.mc.lev){
              Z_i.furCon.temp <- rbind(Z_i.furCon.temp, matrix(0, ncol = ncol(Z_i.furCon.temp), nrow = nrow(Z_i.temp) - nrow(Z_i.furCon.temp)))
            }
            if(use.mc.diff & use.mc.nonlin){
              Z_i.furCon.temp <- rbind(Z_i.furCon.temp, matrix(0, ncol = ncol(Z_i.furCon.temp), nrow = nrow(Z_i.temp) - nrow(Z_i.furCon.temp)))
            }
          } else{
            if((use.mc.lev | use.mc.nonlin) & !use.mc.diff){
              Z_i.furCon.temp <- rbind(Z_i.furCon.temp, matrix(0, ncol = ncol(Z_i.furCon.temp), nrow = nrow(Z_i.temp) - nrow(Z_i.furCon.temp)))
            }
            if(use.mc.diff){
              Z_i.furCon.temp <- rbind(Z_i.furCon.temp, matrix(0, ncol = ncol(Z_i.furCon.temp), nrow = nrow(Z_i.temp) - nrow(Z_i.furCon.temp)))
            }
          }
        }

      } else{
        Z_i.furCon.temp	<- Z_i.furCon.temp_lev
        n.inst.furCon		<- length(get(ls(pattern = "colnames.fur.con.lev")))
        if(dum.diff & !dum.lev){
          if(use.mc.lev & !use.mc.diff){
            if(nrow(Z_i.temp) > nrow(Z_i.furCon.temp)){
              Z_i.furCon.temp <- rbind(matrix(0, ncol = ncol(Z_i.furCon.temp), nrow = nrow(Z_i.temp)-nrow(Z_i.furCon.temp)), Z_i.furCon.temp)
            }
          } else{
            if(use.mc.lev & use.mc.diff){
              Z_i.furCon.temp <- rbind(matrix(0, ncol = ncol(Z_i.furCon.temp), nrow = nrow(Z_i.temp)-nrow(Z_i.furCon.temp)), Z_i.furCon.temp)
            } else{
              Z_i.furCon.temp <- rbind(matrix(0, ncol = ncol(Z_i.furCon.temp), nrow = nrow(Z_i.temp)), Z_i.furCon.temp)
            }
          }
        } else{
          if(use.mc.diff & !use.mc.lev & !include.dum){
            Z_i.furCon.temp <- rbind(matrix(0, ncol = ncol(Z_i.furCon.temp), nrow = nrow(Z_i.temp)), Z_i.furCon.temp)
          }
          if(!use.mc.diff & !use.mc.lev & use.mc.nonlin){
            if(dum.lev){
              Z_i.furCon.temp <- rbind(matrix(0, ncol = ncol(Z_i.furCon.temp), nrow = nrow(Z_i.temp)-nrow(Z_i.furCon.temp)), Z_i.furCon.temp)
            } else{
              Z_i.furCon.temp <- rbind(matrix(0, ncol = ncol(Z_i.furCon.temp), nrow = nrow(Z_i.temp)), Z_i.furCon.temp)
            }
          }
          if(nrow(Z_i.furCon.temp) < nrow(Z_i.temp)){
            Z_i.furCon.temp <- rbind(matrix(0, ncol = ncol(Z_i.furCon.temp), nrow = nrow(Z_i.temp) - nrow(Z_i.furCon.temp)), Z_i.furCon.temp)
          }
        }
      }
    }
    if(nrow(Z_i.temp) < nrow(Z_i.furCon.temp)){
      if(use.mc.lev){
#        if(use.mc.nonlin){
#          Z_i.temp      <- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.furCon.temp) - nrow(Z_i.temp) + nrow(Z_i.mc.AS4)), Z_i.temp)
#        } else{
          Z_i.temp      <- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.furCon.temp) - nrow(Z_i.temp)), Z_i.temp)
#        }
      } else{
#        if(use.mc.nonlin){
#          Z_i.temp      <-
#        } else{
          Z_i.temp      <- rbind(Z_i.temp, matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.furCon.temp) - nrow(Z_i.temp)))
#        }
      }
    }

    Z_i.temp				          <- cbind(Z_i.temp, Z_i.furCon.temp)
    Z_i.temp[is.na(Z_i.temp)]	<- 0

  }

  n.inst				<- c(if(use.mc.diff){ n.inst.diff }, if(use.mc.lev){ n.inst.lev },
                 if(use.mc.nonlin){ n.inst.nl }, if(include.dum){ n.inst.dum }, if(fur.con){ n.inst.furCon } )
  names(n.inst)			<- c(if(use.mc.diff){ "inst.diff" }, if(use.mc.lev){ "inst.lev" },
                       if(use.mc.nonlin){ "inst.nl" },
                       if(include.dum & dum.diff){ "dum.diff" }, if(include.dum & dum.lev){ "dum.lev" },
                       if(fur.con & fur.con.diff){ "inst.furCon.diff" }, if(fur.con & fur.con.lev){ "inst.furCon.lev" } )

  n.obs				<- c(if(use.mc.diff){ n.obs.diff }, if(use.mc.lev){ n.obs.lev },
                if(use.mc.nonlin){ n.obs.nl } )
  names(n.obs)			<- c(if(use.mc.diff){ "n.obs.diff" }, if(use.mc.lev){ "n.obs.lev" },
                      if(use.mc.nonlin){ "n.obs.nl" })

  return(list(Z_i.temp = Z_i.temp, colnames.dum = colnames_Z_i.dum, n.inst = n.inst, n.obs = n.obs))
}














