
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
   ,T
   ,varname.i
   ,dat
   ,dat.na
 ){
#   if(mc.ref.t){
     Matrix::t(Matrix::bdiag(mapply(ti = c(rep(1, times = T-lagTerms-1), {if(T - T.mcDiff - 2 > 0){2:(T - T.mcDiff - 1)}} ), t.end = lagTerms:(T-2), FUN = dat.fct, lagTerms = lagTerms, varname = varname,
		MoreArgs = list(i = i
#			, mc.ref.t = mc.ref.t
			, T = T, varname.i = varname.i, dat = dat, dat.na = dat.na)
		, SIMPLIFY = FALSE)))
#   } else{
#     t(sapply(X = (T - T.mcDiff - 1):(T - 2), FUN = dat.fct, i = i, varname = varname))
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
   ,T
   ,varname.i
   ,dat
   ,dat.na
 ){
#   if(mc.ref.t){
     Matrix::t(Matrix::bdiag(mapply(ti = c(rep(1, times = T-lagTerms-1), {if(T - T.mcDiff - 2 > 0){2:(T - T.mcDiff - 1)}} ), t.end = (lagTerms+1):(T-1), FUN = dat.fct.pre, lagTerms = lagTerms, varname = varname,
		MoreArgs = list(i = i, T = T
#			, mc.ref.t = mc.ref.t
			, varname.i = varname.i, dat = dat, dat.na = dat.na), SIMPLIFY = FALSE)))
#   } else{
#     t(sapply(X = (T - T.mcDiff - 1):(T - 1), FUN = dat.fct.pre, i = i, varname = varname))
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
   ,T
   ,varname.i
   ,dat
   ,dat.na
 ){
#   if(mc.ref.t){
     t.start		<- if(T > T.mcDiff){ c((T-T.mcDiff):(T-lagTerms-1)) } else{ rep(1, times = T-lagTerms-1) }
     t.end			<- t.start + (T.mcDiff-1)
     t.end[t.end > T]	<- T
     err.term.start	<- c((min(t.start) + lagTerms + 1):max(t.end))
     Matrix::t(Matrix::bdiag(mapply(ti = t.start, t.end = t.end, err.term.start = err.term.start, FUN = dat.fct.ex, varname = varname,
		MoreArgs = list(i = i, T = T
#				, mc.ref.t = mc.ref.t
				, varname.i = varname.i, dat = dat, dat.na = dat.na), SIMPLIFY = FALSE)))																		# [M:] use all m.c. in direction of T and cut at initial periods
#   } else{
#    t(sapply(X = (T - T.mcDiff - 1):(T), FUN = dat.fct.ex, i = i, varname = varname, ...))
#   }
 }























#' @keywords internal
#'
 dat.fct		<- function(			# function that creates instruments based on
   ti
   ,t.end
   ,i												# [M:] renamed since 't()' is already a function
   ,lagTerms
   ,varname
   ,T
#   ,mc.ref.t
   ,varname.i
   ,dat
   ,dat.na
 ){
#   if(mc.ref.t){
     dat[dat[, varname.i] == i, varname][ti:t.end]*					# [M:] if period t+1 and t+2 do not exist, t is not available as instrument
     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][t.end-lagTerms+1] *
                        dat.na[dat.na[, varname.i] == i, varname][t.end] *
                        dat.na[dat.na[, varname.i] == i, varname][t.end+1] *
                        dat.na[dat.na[, varname.i] == i, varname][t.end+2])))
#   } else{
#     dat[dat[, varname.i] == i, varname][ti]*						# [M:] if period T, T-1 and T-2 do not exist, t is not available as instrument
#     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][T] *
#                        dat.na[dat.na[, varname.i] == i, varname][T-1] *
#                        dat.na[dat.na[, varname.i] == i, varname][T-2] *
#                        dat.na[dat.na[, varname.i] == i, varname][T-3])))
#   }
 }






















#' @keywords internal
#'
 dat.fct.pre		<- function(
   ti
   ,t.end
   ,i												# [M:] renamed since 't' is already defined
   ,lagTerms
   ,varname
   ,T
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
#     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][T] *
#                        dat.na[dat.na[, varname.i] == i, varname][T-1] *
#                        dat.na[dat.na[, varname.i] == i, varname][T-2])))
#   }
 }



















#' @keywords internal
#'
 dat.fct.ex		<- function(
   ti
   ,t.end												# [M:] renamed since 't' is already defined
   ,err.term.start
   ,i
   ,varname
   ,T
#   ,mc.ref.t
   ,varname.i
   ,dat
   ,dat.na
 ){
#   if(mc.ref.t){
     dat[dat[, varname.i] == i, varname][ti:t.end]*
     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][ti:t.end] *
                        dat.na[dat.na[, varname.i] == i, varname][rep((err.term.start-2), times = length(ti:t.end))])))
#   } else{
#     dat[dat[, varname.i] == i, varname][ti]*
#     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][T] *
#                        dat.na[dat.na[, varname.i] == i, varname][T-1] *
#                        dat.na[dat.na[, varname.i] == i, varname][T-2])))
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
   ,T
   ,varname.i
   ,dat
   ,dat.na
 ){
##   if(use.mc.diff){
   if(use.mc.diff & !(inst.stata)){
#     if(mc.ref.t){
       Matrix::Diagonal(do.call(what = datLEV.fct, args = list(ti = max(2,lagTerms), t.end = T-1, i = i, varname = varname, lagTerms = lagTerms, use.mc.diff = use.mc.diff, inst.stata = inst.stata
#				, mc.ref.t = mc.ref.t
				, dat.na = dat.na, dat = dat, varname.i = varname.i, T = T)), n = T-max(2,lagTerms))
#     }
   } else{
#     if(mc.ref.t){
         Matrix::t(Matrix::bdiag(mapply(ti = rep(max(2,lagTerms), times = (T - max(2,lagTerms))), t.end = c(max(2,lagTerms):(T-1)), lagTerms = lagTerms, FUN = datLEV.fct, varname = varname,
			MoreArgs = list(i = i, use.mc.diff = use.mc.diff, inst.stata = inst.stata
#					, mc.ref.t = mc.ref.t
					, dat.na = dat.na, dat = dat, varname.i = varname.i, T = T)) ))*
		as.vector(!is.na(diff(dat.na[dat.na[, varname.i] == i, varname][(max(2,lagTerms)-1):(T-1)])))
#     } else{
#       t(mapply(ti = T - T.mcLev, t.end = T - 1, FUN = datLEV.fct, i = i, varname = varname,
#		MoreArgs = list(use.mc.diff = use.mc.diff, inst.stata = inst.stata, mc.ref.t = mc.ref.t, dat.na = dat.na, dat = dat, varname.i = varname.i, T = T)))
#     }
   }
 }




# [M:] allows for exogenous/predetermined x





























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
   ,T
   ,varname.i
   ,dat
   ,dat.na
 ){
   if(use.mc.diff & !(inst.stata)){
#     if(mc.ref.t){
       Matrix::Diagonal(do.call(what = datLEV.pre.fct, args = list(ti = max(2,lagTerms), t.end = T-1, lagTerms = lagTerms, varname = varname, i = i, use.mc.diff = use.mc.diff, inst.stata = inst.stata
#			, mc.ref.t = mc.ref.t
			, dat = dat, dat.na = dat.na, varname.i = varname.i, T = T)), n = T-max(2,lagTerms))
#     }
   } else{
#     if(mc.ref.t){
         Matrix::t(Matrix::bdiag(mapply(ti = rep(max(2,lagTerms), times = T-max(2,lagTerms)), t.end = (max(2,lagTerms)+1):T, lagTerms = lagTerms, FUN = datLEV.pre.fct, varname = varname,
			MoreArgs = list(i = i, use.mc.diff = use.mc.diff, inst.stata = inst.stata
#					, mc.ref.t = mc.ref.t
					, dat = dat, dat.na = dat.na, varname.i = varname.i, T = T))) )*
		as.vector(!is.na(diff(dat.na[dat.na[, varname.i] == i, varname][(max(2,lagTerms)-1):T])))
#     } else{
#       t(mapply(ti = T - T.mcLev, t.end = T, FUN = datLEV.pre.fct, varname = varname,
#		MoreArgs = list(i = i, use.mc.diff = use.mc.diff, inst.stata = inst.stata
#		, mc.ref.t = mc.ref.t
#		, dat = dat, dat.na = dat.na, varname.i = varname.i, T = T)))
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
   ,T
   ,varname.i
   ,dat
   ,dat.na
 ){
#   if(mc.ref.t){
#     if(is.na(dat.na[dat.na[, varname.i] == i, varname][ti])){
#       ti	= ti+1
#       t.end	= t.end+1
#     }

     if(use.mc.diff & !(inst.stata)){

       (dat[dat[, varname.i] == i, varname][ti:t.end]*
         as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][(ti - max(2,lagTerms) + 1):(t.end - max(2,lagTerms) + 1)]*
					dat.na[dat[, varname.i] == i, varname][(ti):(t.end)]*
					dat.na[dat[, varname.i] == i, varname][(ti + 1):(t.end + 1)] )) -
       dat[dat[, varname.i] == i, varname][(ti - 1):(t.end - 1)]*
         as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][(ti - max(2,lagTerms) + 1):(t.end - max(2,lagTerms) + 1)]*
					dat.na[dat[, varname.i] == i, varname][(ti):(t.end)]*
					dat.na[dat[, varname.i] == i, varname][(ti + 1):(t.end + 1)] )) ) *
		as.vector(!is.na(diff(dat.na[dat.na[, varname.i] == i, varname][(max(2,lagTerms)-1):(t.end)])))

     } else{

       (dat[dat[, varname.i] == i, varname][ti:t.end]*
         as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][t.end - max(2,lagTerms) + 1]*
					dat.na[dat[, varname.i] == i, varname][t.end]*
					dat.na[dat[, varname.i] == i, varname][t.end+1] )) -
       dat[dat[, varname.i] == i, varname][(ti - 1):(t.end - 1)]*
         as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][t.end - max(2,lagTerms) + 1]*
					dat.na[dat[, varname.i] == i, varname][t.end]*
					dat.na[dat[, varname.i] == i, varname][t.end+1] )) ) *
		as.vector(!is.na(diff(dat.na[dat.na[, varname.i] == i, varname][(max(2,lagTerms)-1):(t.end)])))
     }

#   } else{
#
#     dat[dat[, varname.i] == i, varname][ti]*
#     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][T] *
#                        dat.na[dat.na[, varname.i] == i, varname][T-1] *
#                        dat.na[dat.na[, varname.i] == i, varname][ti]))) -
#     dat[dat[, varname.i] == i, varname][ti - 1]*
#     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][T] *
#                        dat.na[dat.na[, varname.i] == i, varname][T-1] *
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
   ,T
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

       (dat[dat[, varname.i] == i, varname][ti:t.end]*
         as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][(ti - max(2,lagTerms) + 1):(t.end - max(2,lagTerms) + 1)]*
					dat.na[dat[, varname.i] == i, varname][(ti):(t.end)] )) -
       dat[dat[, varname.i] == i, varname][(ti - 1):(t.end - 1)]*
         as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][(ti - max(2,lagTerms) + 1):(t.end - max(2,lagTerms) + 1)]*
					dat.na[dat[, varname.i] == i, varname][(ti):(t.end)] )) ) *
		as.vector(!is.na(diff(dat.na[dat.na[, varname.i] == i, varname][(max(2,lagTerms)-1):(t.end)])))

     } else{

       (dat[dat[, varname.i] == i, varname][ti:t.end] *
                   as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][t.end - max(2,lagTerms)+1]*
						dat.na[dat[, varname.i] == i, varname][t.end] ))  -
       dat[dat[, varname.i] == i, varname][(ti - 1):(t.end - 1)]*
                   as.numeric(!is.na(dat.na[dat[, varname.i] == i, varname][t.end - max(2,lagTerms)+1]*
						dat.na[dat[, varname.i] == i, varname][t.end] )) ) *
		as.vector(!is.na(diff(dat.na[dat.na[, varname.i] == i, varname][(max(2,lagTerms)-1):(t.end)])))
     }

#   } else{
#     dat[dat[, varname.i] == i, varname][ti]*
#     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][T] *
#                        dat.na[dat.na[, varname.i] == i, varname][T-1] *
#                        dat.na[dat.na[, varname.i] == i, varname][ti]))) -
#     dat[dat[, varname.i] == i, varname][ti - 1]*
#     (as.numeric(!is.na(dat.na[dat.na[, varname.i] == i, varname][T] *
#                        dat.na[dat.na[, varname.i] == i, varname][T-1] *
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
   ,T
   ,varname.i
#   ,mc.ref.t
   ,use.mc.diff
   ,use.mc.lev
   ,use.mc.nonlin
   ,include.y
   ,varname.y
   ,inst.stata = inst.stata
   ,include.dum
   ,dum.diff
   ,dum.diff.stata
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
   ,maxLags.y
   ,max.lagTerms
   ,maxLags.reg.end
   ,maxLags.reg.pre
   ,maxLags.reg.ex
   ,dat
   ,dat.na
 ){

   if(use.mc.diff){
#     if(mc.ref.t){
       if(include.y){
         Z_i.mc.diff_end.y	<- do.call(what = "cbind", args = sapply(X = varname.y, FUN = variable.fct, i = i, T.mcDiff = maxLags.y,
						lagTerms = max.lagTerms
#						, mc.ref.t = mc.ref.t
						, T = T, varname.i = varname.i, dat = dat, dat.na = dat.na) )
       }
       if(include.x){
         if(end.reg){
           Z_i.mc.diff_end.x	<- do.call(what = "cbind", args = mapply(FUN = variable.fct, varname.reg.end, i = i, T.mcDiff = maxLags.reg.end,
						lagTerms = max.lagTerms
#						, mc.ref.t = mc.ref.t
						, T = T, varname.i = varname.i, dat = dat, dat.na = dat.na) )
         }
         if(pre.reg){
           Z_i.mc.diff_pre	<- do.call(what = "cbind", args = mapply(FUN = variable.pre.fct, varname.reg.pre, i = i, T.mcDiff = maxLags.reg.pre,
						lagTerms = max.lagTerms
#						, mc.ref.t = mc.ref.t
						, T = T, varname.i = varname.i, dat = dat, dat.na = dat.na) )
         }
         if(ex.reg){
           Z_i.mc.diff_ex	<- do.call(what = "cbind", args = mapply(FUN = variable.ex.fct, varname.reg.ex, i = i, T.mcDiff = maxLags.reg.ex,
						lagTerms = max.lagTerms
#						, mc.ref.t = mc.ref.t
						, T = T, varname.i = varname.i, dat = dat, dat.na = dat.na) )
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
						, T = T, varname.i = varname.i, dat = dat, dat.na = dat.na) )
       }
       if(include.x){
         if(end.reg){
           Z_i.mc.lev_end.x	<- do.call(what = "cbind", args = mapply(FUN = LEV.fct, i = i, varname.reg.end, T.mcLev = maxLags.reg.end, lagTerms = max.lagTerms,
						use.mc.diff = use.mc.diff, inst.stata = inst.stata
#						, mc.ref.t = mc.ref.t
						, T = T, varname.i = varname.i, dat = dat, dat.na = dat.na) )
         }
         if(ex.reg | pre.reg){
           Z_i.mc.lev_ex.pre	<- do.call(what = "cbind", args = mapply(FUN = LEV.pre.fct, c({if(!(is.null("varname.reg.ex"))){varname.reg.ex}}, {if(!(is.null("varname.reg.pre"))){varname.reg.pre}} ),
						i = i, T.mcLev = c({if(!(is.null("varname.reg.ex"))){maxLags.reg.ex - 1}}, {if(!(is.null("varname.reg.pre"))){maxLags.reg.pre}} ),
						lagTerms = max.lagTerms, use.mc.diff = use.mc.diff, inst.stata = inst.stata
#						,mc.ref.t = mc.ref.t
						, T = T, varname.i = varname.i, dat = dat, dat.na = dat.na) )
         }
       }
       Z_i.mc.lev_end	<- do.call(what = "cbind", args = mget(ls(pattern = "Z_i.mc.lev_end")))
       if(include.x & (ex.reg | pre.reg)){
         Z_i.mc.lev	<- Matrix::bdiag(Z_i.mc.lev_end, Z_i.mc.lev_ex.pre)
       } else{
         Z_i.mc.lev	<- Z_i.mc.lev_end
       }
#     }
     n.inst.lev	<- ncol(Z_i.mc.lev)
     n.obs.lev	<- nrow(Z_i.mc.lev)

     if(use.mc.diff){
#       if(mc.ref.t){
         if(!(use.mc.diff) & ((use.mc.lev & (include.y | end.reg)) & (use.mc.lev & (ex.reg | pre.reg)) | dum.lev | fur.con.lev)){
           Z_i.temp		<- Matrix::bdiag(list(Z_i.mc.diff_temp, rbind(rep(0, times = ncol(Z_i.mc.lev)), Z_i.mc.lev)) )
         } else{
           Z_i.temp		<- Matrix::bdiag(list(Z_i.mc.diff_temp, Z_i.mc.lev))
         }
#       }
     } else{
#       if(mc.ref.t){
         Z_i.temp		<- Z_i.mc.lev
#       }
     }
   }

   if(use.mc.nonlin){
#     if(mc.ref.t){
       Z_i.mc.AS4	<- diag(as.numeric(!(is.na(diff(dat.na[dat[, varname.i] == i, varname.y], differences = max.lagTerms+2))) ))	# [M:] indexing adjusted

       if(use.mc.diff & !(use.mc.lev)){																# [M:] changed to allow for nonlinear mc only
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
     ind_vec.diff.row	<- is.na(diff(dat.na[dat[, varname.i] == i, varname.y][1:(T)], differences = max.lagTerms+1) )
     ind_vec.lev.row	<- is.na(diff(dat.na[dat[, varname.i] == i, varname.y][1:(T)], differences = max.lagTerms) )
     ind_vec.diff.col	<- is.na(diff(dat.na[dat[, varname.i] == i , varname.y][1:T], differences = max(2, max.lagTerms)) )
     ind_vec.lev.col	<- is.na(diff(dat.na[dat[, varname.i] == i , varname.y][1:T], differences = max.lagTerms) )

     if(dum.lev){
       Z_i.dum_4.lev				<- as.matrix(dat[dat[, varname.i] == i, colnames.dum[-c(1:max.lagTerms)]][-c(1:max.lagTerms), ])
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
         Z_i.dum_1.diff			<- as.matrix(dat[dat[, varname.i] == i, colnames.dum[-c(1:max.lagTerms)]][(max(2, max.lagTerms)+max.lagTerms):T, ] -
                                    		rbind(dat[dat[, varname.i] == i, colnames.dum[-c(1:max.lagTerms)]][(max(2, max.lagTerms)+(max.lagTerms-1)):(T-1), ]))
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
         if((use.mc.diff | fur.con.diff) & !(dum.diff)){
           Z_i.dum_1.diff		<- matrix(0, ncol = ncol(Z_i.dum_4.lev), nrow = (T-max.lagTerms-1))
           colnames.dum_1.diff	<- colnames(Z_i.dum_4.lev)
           if(use.mc.nonlin){
             Z_i.dum			<- rbind(Z_i.dum_1.diff, Z_i.dum_2.nl, Z_i.dum_4.lev)
           } else{
             Z_i.dum			<- rbind(Z_i.dum_1.diff, Z_i.dum_4.lev)
           }
         } else{
           Z_i.dum 			<- Z_i.dum_4.lev
         }
       }

     }

     if(dum.diff & !(dum.lev)){
       Z_i.dum_1.diff				<- as.matrix(dat[dat[, varname.i] == i, colnames.dum[-c(1:max.lagTerms)]][(max(2, max.lagTerms)+max.lagTerms):T, ] -
                                    		rbind(dat[dat[, varname.i] == i, colnames.dum[-c(1:max.lagTerms)]][(max(2, max.lagTerms)+(max.lagTerms-1)):(T-1), ]))
       Z_i.dum_1.diff[ind_vec.diff.row, ]	<- 0
       Z_i.dum_1.diff[ ,ind_vec.diff.col]	<- 0
       colnames.dum_1.diff			<- paste("D.", colnames(Z_i.dum_1.diff), sep = "")
       colnames(Z_i.dum_1.diff)		<- NULL
       rownames(Z_i.dum_1.diff)		<- NULL

       if(use.mc.nonlin){
         Z_i.dum_2.nl			<- matrix(0, ncol = ncol(Z_i.dum_1.diff), nrow = nrow(Z_i.mc.AS4))
         colnames.dum_2.nl		<- colnames(Z_i.dum_2.nl)
         colnames(Z_i.dum_2.nl)	<- NULL
       }
       if(use.mc.lev & (!include.y | end.reg)){
         Z_i.dum_3.end			<- matrix(0, ncol = ncol(Z_i.dum_1.diff), nrow = nrow(Z_i.mc.lev_end))
         colnames.dum_3.end		<- colnames(Z_i.dum_3.end)
         rownames(Z_i.dum_3.end)	<- NULL

       }
       if((use.mc.lev | (!include.y & (ex.reg | pre.reg))) | fur.con.lev){
         Z_i.dum_4.expre		<- matrix(0, ncol = ncol(Z_i.dum_1.diff), nrow = (T - max.lagTerms))
         colnames.dum_4.expre		<- colnames(Z_i.dum_1.diff)
         colnames(Z_i.dum_4.expre)	<- NULL
       }

       Z_i.dum				<- do.call(what = "rbind", args = mget(ls(pattern = "Z_i.dum_")))
       rownames(Z_i.dum)		<- NULL

     }

     if(use.mc.lev){
       if((include.y | end.reg) & (ex.reg | pre.reg | dum.lev | fur.con.lev)){
         Z_i.temp			<- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)),
						Z_i.temp )
       }
       if(!(include.y | end.reg) & (ex.reg | pre.reg | dum.lev | fur.con.lev)){
         Z_i.temp			<- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)), Z_i.temp )
       }
       if((include.y | end.reg) & !(ex.reg | pre.reg | dum.lev | fur.con.lev) & (dum.diff | fur.con.diff)){
         Z_i.temp			<- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)), Z_i.temp )
       }
     } else{
       if((dum.diff | fur.con.diff) & !(dum.lev | fur.con.lev)){
         Z_i.temp			<- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)), Z_i.temp)
       }
       if(!(dum.diff | fur.con.diff) & (dum.lev | fur.con.lev)){
         Z_i.temp			<- rbind(Z_i.temp, matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)) )
       }
       if((dum.diff | fur.con.diff) & (dum.lev | fur.con.lev)){
         if(use.mc.diff){
           Z_i.temp		<- rbind(Z_i.temp, matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp)))
         }
         if(!(use.mc.diff) & use.mc.nonlin){
           Z_i.temp		<- rbind(matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum_1.diff)),
						Z_i.temp,
						matrix(0, ncol = ncol(Z_i.temp), nrow = nrow(Z_i.dum) - nrow(Z_i.temp) - nrow(Z_i.dum_1.diff)) )
         }
       }
     }

     colnames_Z_i.dum		<- unique(as.vector(do.call(what = "c", mget(ls(pattern = "colnames.dum_")))))
     Z_i.temp			<- cbind(Z_i.temp, as.matrix(Z_i.dum))
     if(dum.diff & dum.lev){
       n.inst.dum			<- c(length(get(ls(pattern = "colnames.dum_1"))), length(get(ls(pattern = "colnames.dum_4"))))
     } else{
       if(dum.diff & !(dum.lev)){
         n.inst.dum		<- length(get(ls(pattern = "colnames.dum_1")))-1
       }
       if(dum.lev & !(dum.diff)){
         n.inst.dum		<- length(get(ls(pattern = "colnames.dum_4")))
       }
     }
   }



   if(fur.con){

     ind_vec.diff.row	<- is.na(diff(dat.na[dat[, varname.i] == i, varname.y][1:(T)], differences = max.lagTerms+1) )

     if(fur.con.diff){
#       if(mc.ref.t){
         if(length(varname.reg.estParam.fur) == 1){
           Z_i.furCon.diff				<- diff(as.matrix(dat.na[dat[, varname.i] == i, varname.reg.estParam.fur][-c(1:max.lagTerms)]), differences = 1)
           Z_i.furCon.diff[ind_vec.diff.row]	<- 0
           Z_i.furCon.temp_diff			<- as.matrix(c(Z_i.furCon.diff, rep(0, times = nrow(Z_i.temp) - length(Z_i.furCon.diff)) ) )
           colnames.fur.con.diff			<- colnames(Z_i.furCon.temp_diff)
           rownames(Z_i.furCon.temp_diff)		<- NULL
           colnames(Z_i.furCon.temp_diff)		<- NULL
         } else{
           Z_i.furCon.diff				<- diff(as.matrix(dat.na[dat[, varname.i] == i, varname.reg.estParam.fur][-c(1:max.lagTerms), ]), differences = 1)
           Z_i.furCon.diff[ind_vec.diff.row, ]	<- 0
           Z_i.furCon.temp_diff			<- rbind(Z_i.furCon.diff, matrix(0, ncol = ncol(Z_i.furCon.diff), nrow = nrow(Z_i.temp) - nrow(Z_i.furCon.diff)) )
           colnames.fur.con.diff			<- colnames(Z_i.furCon.temp_diff)
           rownames(Z_i.furCon.temp_diff)		<- NULL
           colnames(Z_i.furCon.temp_diff)		<- NULL
         }
#       }
     }

     if(fur.con.lev){
#       if(mc.ref.t){
         if(length(varname.reg.estParam.fur) == 1){
           Z_i.furCon.lev				<- as.matrix(dat.na[dat[, varname.i] == i, varname.reg.estParam.fur][1:T][-c(1:max.lagTerms)] )
           Z_i.furCon.lev[ind_vec.lev.row]	<- 0
           Z_i.furCon.temp_lev			<- as.matrix(c(rep(0, times = nrow(Z_i.temp) - length(Z_i.furCon.lev)), Z_i.furCon.lev) )
           colnames.fur.con.lev			<- colnames(Z_i.furCon.temp_lev)
           rownames(Z_i.furCon.temp_lev)		<- NULL
           colnames(Z_i.furCon.temp_lev)		<- NULL
         } else{
           Z_i.furCon.lev				<- as.matrix(dat.na[dat[, varname.i] == i, varname.reg.estParam.fur][1:T, ][-c(1:max.lagTerms), ] )
           Z_i.furCon.lev[ind_vec.lev.row, ]	<- 0
           Z_i.furCon.temp_lev			<- rbind(matrix(0, ncol = ncol(Z_i.furCon.lev), nrow = nrow(Z_i.temp) - nrow(Z_i.furCon.lev)), Z_i.furCon.lev)
           colnames.fur.con.lev			<- colnames(Z_i.furCon.temp_lev)
           rownames(Z_i.furCon.temp_lev)		<- NULL
           colnames(Z_i.furCon.temp_lev)		<- NULL
         }
#       }
     }

     if(fur.con.diff & fur.con.lev){
       Z_i.furCon.temp		<- cbind(Z_i.furCon.temp_diff, Z_i.furCon.temp_lev)
       n.inst.furCon		<- c(length(get(ls(pattern = "colnames.fur.con.diff"))), length(get(ls(pattern = "colnames.fur.con.lev"))))
     } else{
       if(fur.con.diff){
         Z_i.furCon.temp	<- Z_i.furCon.temp_diff
         n.inst.furCon		<- length(get(ls(pattern = "colnames.fur.con.diff")))
       } else{
         Z_i.furCon.temp	<- Z_i.furCon.temp_lev
         n.inst.furCon		<- length(get(ls(pattern = "colnames.fur.con.lev")))
       }
     }

     Z_i.temp				<- cbind(Z_i.temp, Z_i.furCon.temp)
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
 ,dum.diff
 ,dum.lev
 ,fur.con.diff
 ,fur.con.lev
 ,Z.temp
 ,n
 ,T
# ,mc.ref.t
 ,max.lagTerms
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
       H_i.mcDiff	<- (diag(x = 2, nrow = T - max.lagTerms - 1, ncol = T - max.lagTerms - 1) -
       			rbind(rep(x = 0, times = T - max.lagTerms - 1, ncol = T - max.lagTerms - 2),
	 				diag(x = 1, nrow = T - max.lagTerms - 2, ncol = T - max.lagTerms - 1)) -
       			cbind(rep(x = 0, times = T - max.lagTerms - 1),
	 				diag(x = 1, nrow = T - max.lagTerms - 1, ncol = T - max.lagTerms - 2)) )
       H_i.off		<- (cbind(diag(x = -1, nrow = T - max.lagTerms - 1, ncol = T - max.lagTerms - 1), 0) +
     					cbind(rep(x = 0, times = T - max.lagTerms - 1),
 						diag(x = 1, nrow = T - max.lagTerms - 1, ncol = T - max.lagTerms - 1)) )
     }

     if(use.mc.lev){												# [M:] part of weighting matrix is identical for 'mc.ref.t' and 'mc.ref.T'
       H_i.mcLev		<- diag(T - max.lagTerms)
     }

     if(dum.lev | fur.con.lev | ex.reg | pre.reg){
       H_i.mcLev		<- diag(T - max.lagTerms)
     }

     if(use.mc.lev & !(dum.lev | fur.con.lev)){
       H_i.off		<- H_i.off
     }

     if(use.mc.nonlin){
       H_i.mcNL		<- diag(T - max.lagTerms - 2)
     }

     if((use.mc.diff | dum.diff | fur.con.diff) & (use.mc.lev | dum.lev | fur.con.lev)){
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
       H_i.mcDiff	<- (diag(x = 2, nrow = T - max.lagTerms - 1, ncol = T - max.lagTerms - 1) -
      			rbind(rep(x = 0, times = T - max.lagTerms - 1, ncol = T - max.lagTerms - 2),
					diag(x = 1, nrow = T - max.lagTerms - 2, ncol = T - max.lagTerms - 1)) -
      			cbind(rep(x = 0, times = T - max.lagTerms - 1),
					diag(x = 1, nrow = T - max.lagTerms - 1, ncol = T - max.lagTerms - 2)) )
     }

     if(use.mc.lev | dum.lev | fur.con.lev){												# [M:] part of weighting matrix is identical for 'mc.ref.t' and 'mc.ref.T'
       H_i.mcLev		<- diag(T - max.lagTerms)
     }

     if(use.mc.nonlin){
       H_i.mcNL		<- diag(T - max.lagTerms - 2)
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
#   W1		<- Ginv(as.matrix(W1.inv))
#   W1		<- solve(qr(as.matrix(W1.inv)), LAPACK = TRUE)
#   W1.alt		<- Ginv(as.matrix(W1.inv))
#   W1.alt2		<- solve(qr(as.matrix(W1.inv)), LAPACK = TRUE)		# [M:] ~ R-equivalent to the function used in Stata to obtain the pseudoinverse; calculations in R show that the matrix does not meet the requirements for a pseudoinverse!!
 } else{
   W1		<- MASS::ginv(as.matrix(W1.inv))
#   W1		<- Ginv(as.matrix(W1.inv))
#   W1		<- solve(as.matrix(W1.inv))
#   W1.alt		<- solve(W1.inv)
#   W1.alt2		<- Ginv(as.matrix(W1.inv))
 }
########################################
# [M:] the 'pgmm' function uses the minimum eigenvalue to dtermine if a generalized inverse is to be used;
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
 ,T
 ,varname.y
 ,varname.reg.estParam
){

###	Compute y_m1, y_tm1 (i.e., y_{t-1}) and X_t (for first version of nonlinear m.c.)
#             y_T, y_Tm1 (i.e., y_{T-1}) and X_T (for second version of nonlinear m.c.)

 gmmDat		<- list()

# gmmDat$n		<- n
# gmmDat$T		<- T
 gmmDat$dat.na	<- dat.na


 gmmDat$y_m1	<- dat.na[-( ((0:(n - 1))*T) + rep(1, times = n) ), varname.y]
 gmmDat$y_tm1	<- dat.na[-( (1:n)*T - rep(0, times = n) ), varname.y]

 gmmDat$X_m1	<- dat.na[-( ((0:(n - 1))*T) + rep(1, times = n) ), varname.reg.estParam]
 gmmDat$X_tm1	<- dat.na[-((1:n)*T), varname.reg.estParam]



 gmmDat$dy		<- gmmDat$y_m1 - gmmDat$y_tm1											# [M:] vector of length T-1


 gmmDat$dX		<- gmmDat$X_m1 - gmmDat$X_tm1


	#	Note:	(1:n)*(T - 1)	is index of all last (time period for each n) observations w.r.t. to the delta-vectors
	#	Note:	((0:(n - 1))*(T - 1)) + 1	is index of all first (time period for each n) observations w.r.t. to the delta-vectors
	# [M: one observation is lost due to differencing --> delta vector is 'N' elements shorter than data set; 3rd to T-th period are required for each individual]


  return(gmmDat)

}















































###	Compute and return objective function value






#' @keywords internal
#'
gmmObj.fct		<- function(
 j
 ,param
 ,y_m1
 ,X_m1
 ,dy
 ,dX
 ,varname.reg.estParam
 ,n
 ,T
 ,include.y
 ,varname.y
 ,use.mc.diff
 ,use.mc.nonlin
 ,use.mc.nonlinAS
 ,use.mc.lev
 ,dum.diff
 ,fur.con.diff
 ,max.lagTerms
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
	# [M: one observation is lost due to differencing --> delta vector is 'N' elements shorter than data set; 3rd to T-th period are required for each individual]


 gmmDat.parDep$fitted.diff	<- if(length(varname.reg.estParam) == 1){
				  		  crossprod(t(dX[-(((0:(n - 1))*(T - 1)) + 1)]), param)
						} else{
						  crossprod(t(dX[-(((0:(n - 1))*(T - 1)) + 1), ]), param)
						}

 gmmDat.parDep$du.hat		<- dy[-(((0:(n - 1))*(T - 1)) + 1)] - gmmDat.parDep$fitted.diff						#[M:] vector of length T-2







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
        u.vec.1_diff			<- gmmDat.parDep$du.hat[(T-2)*(i-1) + (max.lagTerms:(T-2))]
        y.vec.1_diff			<- gmmDat.parDep$fitted.diff[(T-2)*(i-1) + (max.lagTerms:(T-2))]



      }

      if(use.mc.nonlin){
        if(use.mc.nonlinAS){
          u.vec.2_lev.diff	<- rep(gmmDat.parDep$u.hat_t[(T-1) + (i-1)*(T-1)], times = length(max.lagTerms:(T-3) + (i-1)*(T-2)))*
							gmmDat.parDep$du.hat[max.lagTerms:(T-3) + (i-1)*(T-2)]
          y.vec.2_lev.diff	<- gmmDat.parDep$fitted.diff[max.lagTerms:(T-3) + (i-1)*(T-2)]

        } else{
          u.vec.2_lev.diff	<- gmmDat.parDep$u.hat_t[(max.lagTerms + 2):(T-1) + (i-1)*(T-1)]*
							gmmDat.parDep$du.hat[max.lagTerms:(T-3) + (i-1)*(T-2)]
          y.vec.2_lev.diff	<- gmmDat.parDep$fitted.diff[max.lagTerms:(T-3) + (i-1)*(T-2)]
        }
      }

      if(use.mc.lev & (include.y | end.reg)){
        u.vec.3_lev		<- gmmDat.parDep$u.hat_t[(max.lagTerms):(T-1) + (i-1)*(T-1)]
        y.vec.3_lev		<- gmmDat.parDep$fitted.lev[(max.lagTerms):(T-1) + (i-1)*(T-1)]
      }

      if((use.mc.lev & (include.y | end.reg) & (ex.reg | pre.reg)) | dum.lev | fur.con.lev){
        u.vec.3_lev		<- gmmDat.parDep$u.hat_t[max.lagTerms:(T-1) + (i-1)*(T-1)]
        y.vec.3_lev		<- gmmDat.parDep$fitted.lev[max.lagTerms:(T-1) + (i-1)*(T-1)]
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
  assign("Szero.j", S.temp.zeros, envir = env)
#  assign("Szero.j", S.temp.zeros, pos = 1)
#  assign("Szero.j", S.temp.zeros, envir = parent.frame())

  fitted.temp			<- lapply(Sy.temp, `[[`, 2)

#  assign("fitted.j", fitted.temp, pos = sys.frame(which = 2))
  assign("fitted.j", fitted.temp, envir = env)
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
   ,max.lagTerms												# [M:] renamed since 't' is already defined
   ,T
   ,data.temp
   ,use.mc.diff
   ,dum.diff
   ,fur.con.diff
   ,use.mc.lev
   ,dum.lev
   ,fur.con.lev
   ,use.mc.nonlin
 ){

  if(use.mc.diff | dum.diff | fur.con.diff){
    dat.temp_1diff				<- apply(X = data.temp[-c(1:(max.lagTerms)), ], MARGIN = 2, FUN = diff, args = list(differences=1)) *
								( (diff(data.temp[, varname.y], differences = max.lagTerms + 1)) * is.na(diff(data.temp[, varname.y], differences = max.lagTerms + 1)) + 1 )
    colnames(dat.temp_1diff)			<- NULL
    rownames(dat.temp_1diff)			<- NULL
  }
  if(use.mc.nonlin){
    dat.temp_2nl					<- apply(X = data.temp[-c(1:(max.lagTerms), T), ], MARGIN = 2, FUN = diff, args = list(differences=1)) *
								( (diff(data.temp[, varname.y], differences = max.lagTerms + 2)) * is.na(diff(data.temp[, varname.y], differences = max.lagTerms + 2)) + 1 )
    colnames(dat.temp_2nl)			<- NULL
    rownames(dat.temp_2nl)			<- NULL
  }
  if(use.mc.lev | dum.lev | fur.con.lev){
    dat.temp_3lev					<- as.matrix(data.temp[-c(1:(max.lagTerms)), ]) *
								( diff(data.temp[, varname.y], differences = max(2,max.lagTerms)) * is.na(diff(data.temp[, varname.y], differences = max(2,max.lagTerms))) + 1 )
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
 ,varname.reg.instr
 ,varname.reg.toInstr
 ,varname.y
 ,varname.reg.estParam
 ,use.mc.diff
 ,use.mc.lev
 ,use.mc.nonlin
 ,dum.diff
 ,dum.lev
 ,fur.con.diff
 ,fur.con.lev
 ,max.lagTerms
 ,T
){

  varnames.temp	<- if( !(is.null(varname.reg.instr)) | !(is.null(varname.reg.toInstr)) ){
								c(if(varname.reg.instr){ varname.reg.estParam[!(varname.reg.estParam %in% varname.reg.instr)] }
								,if(varname.reg.toInstr){ varname.reg.toInstr }, varname.y )
								} else{ c(varname.reg.estParam, varname.y) }

  data.temp		<- dat.na[dat.na[, varname.i] == i, varnames.temp]

  dat.temp		<- do.call(what = sub.clForm.fct, args = list(i = i, varname.i = varname.i, varname = varnames.temp, varname.y = varname.y
				,max.lagTerms = max.lagTerms, T = T, data.temp = data.temp, use.mc.diff = use.mc.diff, dum.diff = dum.diff, fur.con.diff = fur.con.diff
				,use.mc.lev = use.mc.lev, dum.lev = dum.lev, fur.con.lev = fur.con.lev, use.mc.nonlin = use.mc.nonlin))

  return(dat.temp)
}











































############################################################################################################
### Standard specification tests (Wald, Hansen J-Test, Arellano and Bond serial correlation test)
############################################################################################################












#' Wald test.
#'
#' \code{wald.fct} computes F test statistics and corresponding p-values for
#'    `pdynmc` objects.
#'
#' The three available null hypothesis are: All time dummies are zero jointly,
#'    all slope coefficients are zero jointly, all times dummies and slope
#'    coefficients are zero jointly.
#'
#' @param param A character string that denotes the null hypothesis. Choices are
#'    time.dum (i.e., all time dummies are jointly zero), slope (i.e., all slope
#'    coefficients are jointly zero), and all (i.e., all dummies and slope
#'    coefficients are jointly zero).
#' @param object An object of class `pdynmc`.
#' @return An object of class `htest` which contains the F test statistic and
#'    corresponding p-value for the tested null hypothesis.
#'
#' @export
#' @importFrom MASS ginv
#' @importFrom Matrix crossprod
#' @importFrom Matrix tcrossprod
#' @importFrom Matrix t
#' @importFrom stats pchisq
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#'
#' @examples
#' \donttest{
#' data(EmplUK, package = "plm")
#' dat <- EmplUK
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'    opt.meth = "none")
#' wald.fct(param = "all", m1)
#' }
#'
#'
wald.fct 		<- function(
 param
 ,object
){

  if(all(class(object) != "pdynmc")) stop("Object needs to be of class 'pdynmc'")

  coef.est				<- ifelse((sapply(get(paste("step", object$iter, sep = ""), object$par.optim), FUN = is.na)),
						yes = get(paste("step", object$iter, sep = ""), object$par.clForm),
						no = get(paste("step", object$iter, sep = ""), object$par.optim) )
  dat.na				<- object$data$dat.na
  varname.y				<- object$data$varname.y
  varname.reg.estParam		<- object$data$varnames.reg
  varname.dum			<- object$data$varnames.dum
  vcov.est				<- get(paste("step", object$iter, sep = ""), object$vcov)
  estimation			<- object$data$estimation
  n					<- object$data$n
  T					<- object$data$T
  n.inst				<- object$data$n.inst
  Szero.j				<- get(paste("step", object$iter, sep = ""), object$residuals)



  K.tot		<- length(coef.est)
  K.t			<- length(varname.dum)

  if(param == "time.dum"){
    start		<- K.tot - K.t + 1
    end		<- K.tot
  }
  if(param == "slope"){
    start		<- 1
    end		<- K.tot - K.t
  }
  if(param == "all"){
    start		<- 1
    end		<- K.tot
  }

  coef.hat		<- coef.est[start:end]
  vcov.hat		<- vcov.est[start:end, start:end]

  if(estimation == "onestep"){
#    w.stat		<- n*crossprod(coef.hat, tcrossprod(MASS::ginv(vcov.hat), t(coef.hat)) ) *
#				(as.vector(crossprod(do.call(res.1s_temp, what = "c"), do.call(Szero.j, what = "c"), na.rm = TRUE) /(n*T - sum(n.inst)+7)))						#[M:] Stata results with different dof-correction
    w.stat		<- n * Matrix::crossprod(coef.hat, Matrix::tcrossprod(solve(vcov.hat), Matrix::t(coef.hat)) ) *
				(as.vector(Matrix::crossprod(do.call(Szero.j, what = "c"), do.call(Szero.j, what = "c"), na.rm = TRUE)) /(sum(!is.na(dat.na[, varname.y])) - sum(n.inst)))		#[M:] Adjusted dof-correction (for missing observations)
  } else{
    w.stat		<- crossprod(coef.hat, tcrossprod(solve(vcov.hat), t(coef.hat)) )
  }

  names(w.stat)	<- "chisq"
  dof			<- length(coef.hat)
  names(dof)	<- "df"
  pval		<- stats::pchisq(w.stat, df = dof, lower.tail = FALSE)
  wald		<- list(statistic = w.stat, p.value = pval, parameter = dof, method = "Wald test"
				,data.name = paste(object$iter, "step GMM Estimation; H0: ", param, " parameters are zero jointly", sep = "")
				)
  class(wald) <- "htest"
  return(wald)
}




















































#' Hansen J test.
#'
#' \code{jtest.fct} tests the validity of the overidentifying restrictions.
#'
#' The null hypothesis is that the overidentifying restrictions are valid.
#'    The test statistic is computed as proposed by
#'    \insertCite{Han1982large;textual}{pdynmc}. As noted by
#'    \insertCite{Bow2002testing;textual}{pdynmc} and
#'    \insertCite{Win2005;textual}{pdynmc}
#'    the test statistic is weakened by many instruments.
#'
#' @param object An object of class `pdynmc`.
#' @return An object of class `htest` which contains the Hansen J test statistic
#'    and corresponding p-value for the null hypothesis that the overidentifying
#'    restrictions are valid.
#'
#' @export
#' @importFrom Matrix crossprod
#' @importFrom stats pchisq
#' @importFrom Rdpack reprompt
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @references
#'
#' \insertAllCited{}
#'
#'
#' @examples
#' \donttest{
#' data(EmplUK, package = "plm")
#' dat <- EmplUK
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'    opt.meth = "none")
#' jtest.fct(m1)
#' }
#'
#'
jtest.fct		<- function(
 object
){

  if(all(class(object) != "pdynmc")) stop("Object needs to be of class 'pdynmc'")

  coef.est		<- ifelse((sapply(get(paste("step", object$iter, sep = ""), object$par.optim), FUN = is.na)), yes = get(paste("step", object$iter, sep = ""), object$par.clForm), no = get(paste("step", object$iter, sep = ""), object$par.optim) )
  Szero.j		<- get(paste("step", object$iter, sep = ""), object$residuals)
  Z.temp		<- object$data$Z
  W.j			<- get(paste("step", object$iter, sep = ""), object$w.mat)
  n.inst		<- object$data$n.inst



  K.tot			<- length(coef.est)
  N				<- length(do.call(what = "c", Szero.j))
  tzu				<- as.numeric(Reduce("+", mapply(function(x,y) Matrix::crossprod(x,y), Z.temp, Szero.j)))
  stat			<- as.numeric(crossprod(tzu, t(crossprod(tzu, W.j))))
  names(stat)		<- "chisq"
  p				<- sum(n.inst)
  param			<- p - K.tot
  names(param)		<- "df"
  method			<- "J-Test of Hansen"
  pval			<- stats::pchisq(stat, df = param, lower.tail = FALSE)
  jtest			<- list(statistic = stat, p.value = pval, parameter = param, method = method
					,data.name = paste(object$iter, "step GMM Estimation; H0: overidentifying restrictions valid", sep = "")
					)
  class(jtest)		<- "htest"
  return(jtest)
}





























#' Arellano and Bond serial correlation test.
#'
#' \code{mtest.fct} tests for serial correlation in the error terms.
#'
#' The null hypothesis is that there is no serial correlation of a
#'    particular order. The test statistic is computed as proposed by
#'    \insertCite{AreBon1991;textual}{pdynmc}.
#'
#' @param object An object of class `pdynmc`.
#' @param t.order A number denoting the order of serial correlation to test for.
#' @return An object of class `htest` which contains the Arellano and Bond m test
#'    statistic and corresponding p-value for the null hypothesis that there is no
#'    serial correlation of the given order.
#'
#' @export
#' @importFrom Matrix crossprod
#' @importFrom Matrix tcrossprod
#' @importFrom Matrix t
#' @importFrom stats pnorm
#' @importFrom Rdpack reprompt
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @references
#'
#' \insertAllCited{}
#'
#'
#' @examples
#' \donttest{
#' data(EmplUK, package = "plm")
#' dat <- EmplUK
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'    opt.meth = "none")
#' mtest.fct(m1, t.order = 2)
#' }
#'
#'
mtest.fct 		<- function(
 object
 ,t.order
){

  if(all(class(object) != "pdynmc")) stop("Object needs to be of class 'pdynmc'")

  estimation	<- object$data$estimation
  Szero.j			<- get(paste("step", object$iter, sep = ""), object$residuals)
  Z.temp			<- object$data$Z
  vcov.est		<- get(paste("step", object$iter, sep = ""), object$vcov)
  W.j				  <- get(paste("step", object$iter, sep = ""), object$w.mat)

  stderr.type	<- object$data$stderr.type
  std.err			<- get(paste("step", object$iter, sep = ""), object$stderr)
  n.inst			<- object$data$n.inst
  n				    <- object$data$n
  T				    <- object$data$T
  varname.y			<- object$data$varname.y
  varname.reg		<- object$data$varnames.reg
  varname.dum		<- object$data$varnames.dum
  dat.clF.temp	<- rapply(lapply(object$dat.clF, FUN = as.matrix), function(x) ifelse(is.na(x), 0, x), how = "replace")
  dat.na			  <- object$data$dat.na



  K.t			<- length(varname.dum) - length(varname.dum[!(varname.dum %in% varname.reg)])
  u.hat.m_o		<- lapply(Szero.j, function(x) c(rep(0, times = t.order), x[1:(length(x)-t.order)]) )

  if(estimation == "onestep" & stderr.type == "unadjusted"){
    #    uHtu			<- lapply(lapply(Szero.j, function(x) crossprod(x,x)), function(x) as.numeric(x) * 0.2* H_i.temp * (1/ (n*T - sum(n.inst)+3) ))
    uHtu			<- lapply(lapply(Szero.j, function(x) crossprod(x,x)), function(x) as.numeric(x) * H_i * (1/ (sum(!is.na(dat.na[, varname.y])) - sum(n.inst)) ))
    tu_m_outuu.m_o	<- Reduce("+", mapply(function(x,y) Matrix::crossprod(x, Matrix::tcrossprod(y, Matrix::t(x))), u.hat.m_o, uHtu, SIMPLIFY = FALSE ))
    tZutuu.m_o		<- Reduce("+", mapply(function(x,y,z) Matrix::tcrossprod(Matrix::crossprod(x,y), Matrix::t(z)), Z.temp, uHtu, u.hat.m_o, SIMPLIFY = FALSE))
  } else{
    tu_m_outuu.m_o	<- Reduce("+", mapply(function(x,y) tcrossprod(crossprod(y,x), crossprod(y,x)), Szero.j, u.hat.m_o, SIMPLIFY = FALSE))
    tZutuu.m_o		<- Reduce("+", mapply(function(x,y,z) Matrix::tcrossprod(Matrix::crossprod(x,y), Matrix::crossprod(z,y)), Z.temp, Szero.j, u.hat.m_o, SIMPLIFY = FALSE))
  }

  tu.m_oX		<- Reduce("+", mapply(function(x,y) Matrix::crossprod(x,y), u.hat.m_o, dat.clF.temp, SIMPLIFY = FALSE))
  tZX			<- Reduce("+", mapply(function(x,y) Matrix::crossprod(x,y), Z.temp, dat.clF.temp, SIMPLIFY = FALSE))

  frac.num		<- Reduce("+", mapply(function(x,y) crossprod(x,y), Szero.j, u.hat.m_o, SIMPLIFY = FALSE))
  frac.denom.sq	<- (as.numeric(tu_m_outuu.m_o - 2* Matrix::crossprod(Matrix::t(tu.m_oX), Matrix::crossprod(Matrix::t(vcov.est), Matrix::crossprod(tZX, Matrix::tcrossprod(W.j, Matrix::t(tZutuu.m_o))))) + Matrix::crossprod(Matrix::t(tu.m_oX), Matrix::tcrossprod(vcov.est, tu.m_oX))))

  if(frac.denom.sq < 0){
    frac.denom	<- sqrt(abs(as.numeric(tu_m_outuu.m_o - 2* Matrix::crossprod(Matrix::t(tu.m_oX), Matrix::crossprod(Matrix::t(vcov.est), Matrix::crossprod(tZX, Matrix::tcrossprod(W.j, Matrix::t(tZutuu.m_o))))) + Matrix::crossprod(Matrix::t(tu.m_oX), Matrix::tcrossprod(vcov.est, tu.m_oX)))))
    warning("Absolute value of denominator of test statistic was used in the computation.")
  } else{
    frac.denom	<- sqrt(as.numeric(tu_m_outuu.m_o - 2* Matrix::crossprod(Matrix::t(tu.m_oX), Matrix::crossprod(Matrix::t(vcov.est), Matrix::crossprod(tZX, Matrix::tcrossprod(W.j, Matrix::t(tZutuu.m_o))))) + Matrix::crossprod(Matrix::t(tu.m_oX), Matrix::tcrossprod(vcov.est, tu.m_oX))))
  }

  stat		<- frac.num/frac.denom
  names(stat)	<- "normal"
  pval		<- 2*stats::pnorm(abs(stat), lower.tail = FALSE)
  mtest		<- list(statistic = stat, p.value = pval, method = paste("Arrelano and Bond (1991) serial correlation test of degree", t.order)
                 ,data.name = paste(object$iter, "step GMM Estimation; H0: no serial correlation of order ", t.order, " in the error terms", sep = "")
  )
  class(mtest)	<- "htest"
  return(mtest)
}











































































############################################################
### Estimation function
############################################################











#' Generalized Method of Moments (GMM) Estimation of Linear Dynamic Panel Data
#'    Models.
#'
#' \code{pdynmc} fits a linear dynamic panel data model based on moment
#'    conditions with the Generalized Method of Moments (GMM).
#'
#' The function estimates a linear dynamic panel data model of the form
#'    \deqn{y_{i,t} = y_{i,t-1} \rho_1 + \boldsymbol{x}_{i,t}' \boldsymbol{\beta} + a_i + \varepsilon_{i,t}}
#'    where \eqn{y_{i,t-1}} is the lagged dependent variable, \eqn{\rho_1} is
#'    the lag parameter, \eqn{\boldsymbol{x}_{i,t}} are further covariates,
#'    \eqn{\boldsymbol{\beta}} are the corresponding parameters, \eqn{a_i}
#'    is an unobserved individual specific effect, and
#'    \eqn{\varepsilon_{i,t}} is an idiosyncratic remainder component. The
#'    model structure accounts for unobserved individual specific heterogeneity
#'    and dynamics. Note that the specification given above is simplified for
#'    illustatory purposes and more general lag structures are allowed in
#'    \code{pdynmc}.
#'
#'    Estimation of the model parameters in \code{pdynmc} is based on
#'    moment conditions with the generalized method of moments (GMM). Linear
#'    dynamic panel data models  The moment conditions employed in estimation can be linear and
#'    nonlinear in parameters and estimation is carried out iteratively. In case
#'    only linear moment conditions are used in estimation, closed form solutions
#'    can be for computing parameter estimates -- while when nonlinear moment
#'    conditions are employed, parameter estimation relies on numerical
#'    optimization of the objective function.
#'
#'    `pdynmc` provides an implementation of some of the functionality available
#'    in the Stata library xtdpdgmm \insertCite{Kri2019;textual}{pdynmc} and allows
#'    for `"onestep"`, `"twostep"`, and `"iterative"` GMM estimation based on the
#'    moment conditions of \insertCite{HolNewRos1988;textual}{pdynmc},
#'    \insertCite{AreBov1995;textual}{pdynmc}, and
#'    \insertCite{AhnSch1995;textual}{pdynmc}.
#'
#' @aliases pdynmc
#' @param dat A data set.
#' @param varname.i The name of the cross-section identifier.
#' @param varname.t The name of the time-series identifier.
#' @param use.mc.diff A logical variable indicating whether moment conditions from
#'    equations in differences (i.e. instruments in levels) should be used.
#' @param use.mc.lev A logical variable indicating whether moment conditions from
#'    equations in levels (i.e. instruments in differences) should be used.
#' @param use.mc.nonlin A logical variable indicating whether nonlinear (quadratic)
#'    moment conditions should be used.
#' @param use.mc.nonlinAS A logical variable indicating whether only the nonlinear
#'    (quadratic) moment conditions in the form proposed by
#'    \insertCite{AhnSch1995;textual}{pdynmc} should be used (defaults to `TRUE`).
#' @param inst.stata A logical variable indicating whether to use the moment
#'    conditions from equations in levels as in Stata implementations xtabond2
#'    \insertCite{Roo2018xtabond2;textual}{pdynmc} and xtdpdgmm
#'    \insertCite{Kri2019;textual}{pdynmc}.
#' @param include.y A logical variable indicating whether instruments should be
#'    derived from the lags of the response variable.
#' @param varname.y A character string denoting the name of the response variable
#'    in the data set.
#' @param lagTerms.y An integer indicating the number of lags of the dependent
#'    variable used as explanatory variables.
#' @param maxLags.y An integer indicating the maximum number of lags of the
#'    dependent variable from which instruments should be derived.
#' @param include.x A logical variable indicating whether instruments should be
#'    derived from the covariates. Setting the argument to `TRUE` requires
#'    specifying whether the covariates are endogenous, predetermined, or
#'    (strictly) exogenous (defaults to `FALSE`).
#' @param varname.reg.end One or more character strings denoting the covariate(s)
#'    in the data set to be treated as endogenous (defaults to `NULL`).
#' @param lagTerms.reg.end One or more integers indicating the number of lags of
#'    the endogenous covariate(s) used as explanatory variables. One integer per
#'    covariate needs to be given in the same order as the covariate names
#'    (defaults to `NULL`).
#' @param maxLags.reg.end One or more integers indicating the maximum number of
#'    lags of the endogenous covariate(s) used for deriving instruments. One
#'    integer per covariate needs to be given in the same order as the covariate
#'    names (defaults to `NULL`).
#' @param varname.reg.pre One or more character strings denoting the covariate(s)
#'    in the data set to be treated as predetermined (defaults to `NULL`).
#' @param lagTerms.reg.pre One or more integers indicating the number of lags of
#'    the predetermined covariate(s) used as explanatory variables. One integer per
#'    covariate needs to be given in the same order as the covariate name (defaults
#'    to `NULL`).
#' @param maxLags.reg.pre One or more integers indicating the maximum number of
#'    lags of the predetermined covariate(s) used for deriving instruments. One
#'    integer per covariate needs to be given in the same order as the covariate
#'    names (defaults to `NULL`).
#' @param varname.reg.ex One or more character strings denoting the covariate(s)
#'    in the data set to be treated as (strictly) exogenous (defaults to `NULL`).
#' @param lagTerms.reg.ex One or more integers indicating the number of lags of
#'    the (strictly) exogenous covariate(s) used as explanatory variables. One
#'    integer per covariate needs to be given in the same order as the covariate
#'    name (defaults to `NULL`).
#' @param maxLags.reg.ex One or more integers indicating the maximum number of
#'    lags of the (strictly) exogenous covariate(s) used for deriving instruments.
#'    One integer per covariate needs to be given in the same order as the
#'    covariate names (defaults to `NULL`).
#' @param include.x.instr A logical variable that allows to include additionl
#'    IV-type instruments (i.e., include covariates which are used as instruments
#'    but for which no parameters are estimated; defaults to `FALSE`).
#' @param varname.reg.instr One or more character strings denoting the covariate(s)
#'    in the data set treated as instruments in IV-estimation (defaults to `NULL`).
#' @param include.x.toInstr A logical variable that allows to instrument covariates
#'    (i.e., include covariates for which parameters are estimated but which are
#'    not employed in estimation; defaults to `FALSE`).
#' @param varname.reg.toInstr One or more character strings denoting the covariates
#'    in the data set to be instrumented (i.e., covariates which are used as
#'    instruments but for which no parameters are estimated; defaults to `FALSE`).
#' @param fur.con A logical variable indicating whether further control variables
#'    (covariates) are included (defaults to `FALSE`).
#' @param fur.con.diff A logical variable indicating whether to include further
#'    control variables in equations from differences (defaults to `NULL`).
#' @param fur.con.lev A logical variable indicating whether to include further
#'    control variables in equations from level (defaults to `NULL`).
#' @param varname.reg.fur One or more character strings denoting covariate(s) in
#'    the data set to treat as further controls (defaults to `NULL`).
#' @param lagTerms.reg.fur One or more integers indicating the number of lags of
#'    the further controls to be used as explanatory variables. One integer per
#'    further control needs to be given in the same order as the corresponding
#'    variable names (defaults to `NULL`).
#' @param include.dum A logical variable indicating whether dummy variables for
#'    the time periods are included (defaults to `FALSE`).
#' @param dum.diff A logical variable indicating whether dummy variables are
#'    included in the equations in first differences (defaults to `FALSE`).
#' @param dum.lev A logical variable indicating whether dummy variables are
#'    included in the equations in levels (defaults to `TRUE`).
#' @param varname.dum One or more character strings from which time dummies should
#'    be derived (can be different from varname.t; defaults to `NULL`).
#' @param col_tol A numeric variable in [0,1] indicating the absolute correlation
#'    threshold for collinearity checks (columns are omitted when pairwise
#'    correlations are above the threshold; defaults to 0.65).
#' @param w.mat One of the character strings c(`"iid.err"`, `"identity"`,
#'    `"zero.cov"`) indicating the type of weighting matrix to use (defaults to
#'    `"iid.err"`).
#' @param w.mat.stata A logical variable that slightly adjusts the weighting
#'    matrix according to the Stata function xtdpdgmm (defaults to `FALSE`).
#' @param std.err One of the character strings c(`"corrected"`, `"unadjusted"`).
#'    The former option computes
#'	\insertCite{Win2005;textual}{pdynmc}
#'	corrected standard errors (defaults to `"corrected"`).
#' @param estimation One of the character strings c(`"onestep"`, `"twostep"`,
#'    `"iterative"`). Denotes the number of iterations of the parameter procedure
#'    (defaults to `"twostep"`).
#' @param max.iter An integer indicating the maximum number of iterations
#'    (defaults to `NULL`; if estimation is set to `"iterative"`, `max.iter`
#'    defaults to 100).
#' @param iter.tol A numeric variable in [0,1] indicating the tolerance for
#'    determining convergence of the iterative approach (defaults to `NULL`;
#'    if estimation is set to `"iterative"`, iter.tol defaults to 0.01).
#' @param inst.thresh An integer denoting wether to limit the total number of
#'    instruments to be used in estimation (defaults to `NULL`).
#' @param opt.meth A character string denoting the numerical optimization procedure.
#'    When no nonlinear moment conditions are employed in estimation, closed form
#'    estimates can be computed by setting the argument to `"none"` (defaults to
#'    `"BFGS"`; for details on the further available optimizers see the
#'    documentation of package \pkg{optimx}).
#' @param hessian A logical variable indicating if the hessian matrix should be
#'    approximated in optimization (defaults to `FALSE`).
#' @param optCtrl A list of arguments that are passed to \pkg{optimx}.
#'    For details on the arguments and the available options see the package
#'    documentation.
#' @param custom.start.val A logical variable indicating whether prespecified
#'    starting values for the parameters are provided by the user (defaults to
#'    `FALSE`; if set to `TRUE`, starting values need to be provided via argument
#'    `start.val`).
#' @param start.val A vector of numeric variables denoting the starting values
#'    for the parameter vector for numeric optimization (defaults to `NULL`).
#' @param start.val.lo A numeric variable denoting the lower limit for drawing
#'    starting values with uniform density (defaults to -1; ignored if
#'    `custom.start.val` is set to `TRUE`).
#' @param start.val.up A numeric variable denoting the lower limit for drawing
#'    starting values with uniform density (defaults to 1; ignored if
#'    `custom.start.val` is set to `TRUE`).
#' @param seed.input An integer used as seed for drawing starting values (defaults
#'    to 42; required if custom.start.val is set to `FALSE`).
#' @return An object of class `c("list","pdynmc)` with the following elements:
#'
#' \item{data}{a list of elements on which computation of the model fit is based}
#' \item{dep.clF}{a list of vectors containing the dependent variable for the
#'    cross-sectional observations}
#' \item{dat.clF}{a list of matrices containing the explanatory variables for the
#'    cross-sectional observations}
#' \item{w.mat}{a list of weighting matrices for the different estimation steps}
#' \item{H_i}{a matrix used to create the weighting matrix for the first estimation
#'    step}
#' \item{par.optim}{a list of vectors containing the parameter estimates obtained
#'    from numerical optimization for the estimation steps}
#' \item{ctrl.optim}{a list of control parameters used in numerical optimization for
#'    the estimation steps}
#' \item{par.clForm}{a list of vectors containing the parameter estimates obtained
#'    from the closed form for the estimation steps}
#' \item{iter}{a scalar denoting the number of iteration steps carried out to
#'    obtain parameter estimates}
#' \item{fitted}{a list for each estimation step that contains a list of vectors
#'    of fitted values for each cross-sectional observation}
#' \item{resid}{a list for each estimation step that contains a list of vectors of
#'    residuals for each cross-sectional observation}
#' \item{vcov}{a list of matrices containing the variance covariance matrix of the
#'    parameter estimates for each estimation step}
#' \item{stderr}{a list of vectors containing the standard errors of the parameter
#'    estimates for each estimation step}
#' \item{zvalue}{a list of vectors containing the z scores for the parameter
#'    estimates for each estimation step}
#' \item{pvalue}{a list of vectors containing the p-values for the parameter
#'    estimates for each estimation step}
#'
#' It has `fitted`, `residuals`, `wmat`, `vcov`, `summary`, and `print.summary`
#'    methods.
#'
#' @author Markus Fritsch
#' @export
#' @importFrom data.table shift
#' @importFrom dplyr left_join
#' @importFrom MASS ginv
#' @importFrom Matrix crossprod
#' @importFrom Matrix Diagonal
#' @importFrom Matrix Matrix
#' @importFrom Matrix t
#' @importFrom Matrix tcrossprod
#' @importFrom optimx optimx
#' @importFrom qlcMatrix corSparse
#' @importFrom Rdpack reprompt
#' @importFrom stats as.formula
#' @importFrom stats model.matrix
#' @importFrom stats runif
#'
#' @seealso
#'
#' \code{\link{wald.fct}} for Wald tests,
#' \code{\link{jtest.fct}} for the Hansen J test, and
#' \code{\link{mtest.fct}} for serial correlation tests.
#' \code{\link[optimx]{optimx}} for details on alternative routines and options
#'    for numerical optimization
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @examples
#' \donttest{
#' ## Load data from plm package
#' data(EmplUK, package = "plm")
#' dat <- EmplUK
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' ## Arellano and Bond (1991) estimation in Table 4, column (a1)
#' pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'    opt.meth = "none")
#'
#' ## Arellano and Bond (1991) estimation in Table 4, column (a2)
#' pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
#'    opt.meth = "none")
#'
#' ## Arellano and Bond (1991) twostep estimation extended by nonlinear moment
#' ## conditions
#' pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
#'    opt.meth = "BFGS")
#'
#' ## Arellano and Bond (1991) iterative estimation extended by nonlinear moment
#' ## conditions
#' pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "iterative",
#'    max.iter = 4, opt.meth = "BFGS")
#'
#' ## Arellano and Bond (1991) twostep estimation extended by linear moment
#' ## conditions from equations in levels
#' pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
#'    opt.meth = "none")
#' }
#'
#'
pdynmc		<- function(
 dat
 ,varname.i
 ,varname.t

 ,use.mc.diff
 ,use.mc.lev
 ,use.mc.nonlin
 ,use.mc.nonlinAS			= NULL

# ,mc.ref.t				= TRUE
# ,mc.ref.T				= FALSE

 ,inst.stata			= FALSE

 ,include.y
 ,varname.y				= NULL
 ,lagTerms.y			= NULL
 ,maxLags.y				= NULL

 ,include.x				= FALSE
 ,varname.reg.end			= NULL
 ,lagTerms.reg.end		= NULL
 ,maxLags.reg.end			= NULL
 ,varname.reg.pre			= NULL
 ,lagTerms.reg.pre		= NULL
 ,maxLags.reg.pre			= NULL
 ,varname.reg.ex			= NULL
 ,lagTerms.reg.ex			= NULL
 ,maxLags.reg.ex			= NULL

 ,include.x.instr			= FALSE
 ,varname.reg.instr		= NULL
 ,include.x.toInstr		= FALSE
 ,varname.reg.toInstr		= NULL

 ,fur.con				= FALSE
 ,fur.con.diff			= NULL
 ,fur.con.lev			= NULL
 ,varname.reg.fur			= NULL
 ,lagTerms.reg.fur		= NULL

 ,include.dum			= TRUE
 ,dum.diff				= NULL
 ,dum.lev				= TRUE
 ,varname.dum			= NULL
# ,custom.dum			= NULL
# ,partOut				= FALSE
# ,estimate.int			= FALSE

 ,col_tol				= 0.65

 ,w.mat				= "iid.err"
 ,w.mat.stata			= FALSE

 ,std.err				= "corrected"

 ,estimation			= "twostep"
 ,max.iter				= NULL
 ,iter.tol				= NULL
 ,inst.thresh			= NULL
 ,opt.meth				= "BFGS"
 ,hessian				= FALSE
 ,optCtrl				= list(kkt = FALSE, kkttol = .Machine$double.eps^(1/3), kkt2tol = .Machine$double.eps^(1/3),
						starttests = TRUE, dowarn = TRUE, badval = (0.25)*.Machine$double.xmax, usenumDeriv = FALSE,
						reltol = 1e-12, maxit = 200, trace = TRUE,
						follow.on = FALSE, save.failures = TRUE, maximize = FALSE, factr = 1e7, pgtol = 0, all.methods = FALSE)
# ,nmulti				= 1
 ,custom.start.val		= FALSE
 ,start.val				= NULL
 ,start.val.lo			= -1
 ,start.val.up			= 1
 ,seed.input			= 42
){


 if(estimation == "onestep"){
   j.max			<- 1
 }
 if(estimation == "twostep"){
   j.max			<- 2
   max.iter			<- j.max
 }
 if(estimation == "iterative"){
   if(!(is.null(max.iter))){
     j.max			<- max.iter
   } else{
     j.max			<- 100
   }
   if(is.null(iter.tol)){
     iter.tol		<- 0.01
   }
 }
# if(estimation == "cue"){
#   if(!(is.null(max.iter))){
#     j.max			<- max.iter
#   } else{
#     j.max			<- 100
#   }
# }

 if(custom.start.val == TRUE & is.null(custom.start.val)){
   start.val		<- rep(0, times = 17)
 }

 if(use.mc.nonlin & opt.meth == "none"){
   opt.met			<- "BFGS"
 }

 if(use.mc.nonlin == TRUE & is.null(use.mc.nonlinAS)){
   use.mc.nonlinAS	<- TRUE
 }




 resGMM			<- list()

 end.reg			<- !(is.null(varname.reg.end))
 pre.reg			<- !(is.null(varname.reg.pre))
 ex.reg			<- !(is.null(varname.reg.ex))

 instr.reg			<- !(is.null(varname.reg.instr))
 toInstr.reg		<- !(is.null(varname.reg.toInstr))


 max.lagTerms		<- max(if(!(is.null(varname.y))){ lagTerms.y }, if(!(is.null(varname.reg.end))){ lagTerms.reg.end },
				if(!(is.null(varname.reg.pre))){ lagTerms.reg.pre }, if(!(is.null(varname.reg.ex))){ lagTerms.reg.ex },
				if(!(is.null(varname.reg.fur))){ lagTerms.reg.fur } )











 if(include.x && (is.null(varname.reg.end) & is.null(varname.reg.pre) & is.null(varname.reg.ex))
 ){
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
   fur.con		<- FALSE
   warning("No further controls given; 'fur.con' was therefore set to FALSE.")
 }

 if(!(fur.con) && !(is.null(varname.reg.fur))
 ){
   suppressWarnings(rm(varname.reg.fur))
   warning("Further controls given, while further controls are not supposed to be included; argument specifying the further controls was therefore ignored.")
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


 if(include.dum && is.null(varname.dum)
 ){
   include.dum		<- FALSE
   warning("No dummies given; 'include.dum' was therefore set to FALSE.")
 }

 if(!(include.dum) && (is.null(varname.dum))
 ){
   suppressWarnings(rm(varname.dum))
   warning("Dummies given, while dummies are not supposed to be included; argument specifying the dummies was therefore ignored.")
 }

 if(include.dum){
   if((is.null(dum.diff) & is.null(dum.lev)) | (!(dum.diff) & !(dum.lev))){
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



# if((mc.ref.t && mc.ref.T) | (is.null(mc.ref.t) && is.null(mc.ref.T))		# [M:] check that only one reference period is set; else choose 'mc.ref.t'
# ){
#   mc.ref.t		<- TRUE
#   mc.ref.T		<- FALSE
#   warning("Only one of 'mc.ref.t' and 'mc.ref.T' is allowed to be TRUE; 'mc.ref.T' was therefore set to FALSE.")
# }
























###
###	Expand data set and set number of cross-section-/time-series-observations
###



 i_cases			<- sort(unique(dat[, varname.i]))
 i_temp			<- 1:length(i_cases)				# [M:] reflects data structures where i does not start at i = 1
 t_cases			<- sort(unique(dat[, varname.t]))
 t_temp			<- 1:length(unique(t_cases))			# [M:] reflects data structures where t does not start at t = 1


 dat_b			<- as.data.frame(array(data = NA, dim = c(length(i_cases)*length(t_cases), 2),dimnames = list(NULL, c(varname.i, varname.t))))
 dat_b[, varname.i]	<- rep(x = i_cases, each = length(t_cases))
 dat_b[, varname.t]	<- rep(x = t_cases, times = length(i_cases))


 dat				<- dplyr::left_join(x = dat_b, y = dat, by = c(varname.i, varname.t), all.x = TRUE)
 dat				<- dat[order(dat[, varname.i], dat[, varname.t], decreasing = FALSE), ]

 dat.na			<- dat
 dat[is.na(dat.na)]	<- 0






 n				<- length(unique(dat[, varname.i]))		# number of cross-section units
 T				<- length(unique(dat[, varname.t]))		# number of time-series units



 if(is.null(inst.thresh)){			# number of instruments above which a generalized inverse is used to invert the weighting matrix
   inst.thresh	<- n				# if not specified, the cross-section dimension is used as instrument threshold [Stata-default]
 }
# [M:] reasoning is that the weighting matrix equals the expected variance covariance structure of the moment conditions;
#       for the first step weighting matrix, reasonable specifications are derived from the underlying model assumptions.
#       the second step weighting matrix (and also the one of steps beyond the second one) is estimated based on the coefficient
#       estimates of previous steps; if less cross-section observations are available (each m.c. is a cross-section average)
#       than there are m.c., the variance covariance matrix of the moment conditions can not be computed from the data (not
#       even when a diagonal structure of the vcov matrix is assumed) without imposing further restrictions.
#	side note: Why is a generalized inverse (Moore-Penrose Inverse) used in these situations? Only for computational reasons
#			or is there more to it?













###
###	Create dummy matrix for time dummies and matrices for partialling out the time effects
###


#a) Create dummy matrix for time dummies


 if(include.dum){

   if(length(varname.dum) == 1){
     dat[, varname.dum]		<- as.character(dat[, varname.dum])
     form.dum.temp		<- stats::as.formula(paste(varname.y, paste(varname.dum, " -1", collapse = "+"), sep = " ~ "))
   } else{
     dat	<- cbind(dat[, !(colnames(dat) %in% varname.dum)], as.data.frame(lapply(dat, as.character), stringsAsFactors = FALSE)[, varname.dum])
     dat	<- dat[, colnames(dat)]
     form.dum.temp		<- stats::as.formula(paste(varname.y, paste(paste(varname.dum, collapse = "+"), "-1", sep = ""), sep = " ~ "))
   }


   D.add	<- stats::model.matrix(form.dum.temp, data = dat)

   adjust.colnames.fct	<- function(
   j
   ){
     cols.dum.temp		<- gsub(pattern = varname.dum[j], replacement = "", x = colnames(D.add)[grepl(pattern = varname.dum[j], x = colnames(D.add))])
   }

   colnames.dum			<- Reduce(c, lapply(do.call(what = "c", args = list(sapply(1:length(varname.dum), FUN = adjust.colnames.fct))), FUN = c))


   colnames(D.add)		<- colnames.dum

   dat_add				<- matrix(NA, ncol = ncol(D.add), nrow = nrow(dat))
   colnames(dat_add)		<- colnames.dum
   dat				<- cbind(dat, dat_add)
   dat[, colnames.dum]		<- D.add
   dat.na[, colnames.dum]	<- D.add

   dat[is.na(dat.na[, varname.y]), !(colnames(dat) %in% c(varname.i, varname.t))]		<- 0
   dat.na[is.na(dat.na[, varname.y]), !(colnames(dat) %in% c(varname.i, varname.t))]		<- NA








#b) Matrices P_D and M_D for partialling out time effects and actual partialling out


     D.temp					<- as.matrix(dat[ , colnames.dum])
     var.cor				<- vector()
     det_tol.low				<- 10e-12
     det_tol.up				<- 10e+12



     for(k in 1:ncol(D.temp)){													#[M:] not helpful in obtaining results analoguous to Stata and pdgmm
       eigen.temp		<- eigen(crossprod(D.temp, D.temp))$values
       if(max(eigen.temp)/min(eigen.temp) < det_tol.low | max(eigen.temp)/min(eigen.temp) > det_tol.up){
         var.cor		<- c(var.cor, colnames(D.temp)[which.max(colSums(abs(qlcMatrix::corSparse(D.temp))))])
         D.temp		<- D.temp[, -which.max(colSums(abs(qlcMatrix::corSparse(D.temp))))]
       }
     }
#     paste(var.cor)




#   if(partOut){
#
#     P_D		<- crossprod(t(D.temp), tcrossprod(solve(crossprod(D.temp, D.temp)), D.temp))
##     P_D		<- D.temp %*% solve(t(D.temp) %*% D.temp) %*% t(D.temp)
#     I_nT		<- Matrix::Diagonal(n*T)
#     M_D		<- I_nT - P_D
#
#
#
#
#     varname.reg	<- c( if(!(is.null(varname.reg.end))) varname.reg.end				# [M:] covariates (besides the lagged dependent variable) to include in estimation
#				,if(!(is.null(varname.reg.pre))) varname.reg.pre
#				,if(!(is.null(varname.reg.ex))) varname.reg.ex
#				,if(!(is.null(varname.reg.fur))) as.vector(varname.reg.fur) )
#
#
#     if(report.dum){
#       dum.est		<- unique(as.vector(crossprod(P_D, Matrix::Matrix(dat.na[, varname.y]))))
#       names(dum.est)	<- dat.na[rownames(unique(Matrix::crossprod(P_D, Matrix::Matrix(dat.na[, varname.y])))), "year"]
#       dum.est		<- dum.est[dum.est > 0][sort(names(dum.est[dum.est > 0]))]
#     }
#
#
#     dat[, varname.y]		<- Matrix::crossprod(M_D, Matrix::Matrix(dat[, varname.y]))
#     dat[, varname.reg]		<- Matrix::crossprod(M_D, Matrix::Matrix(as.matrix(dat[, varname.reg])))
#     dat.na[, varname.y]	<- Matrix::crossprod(M_D, Matrix::Matrix(dat.na[, varname.y]))
#     dat.na[, varname.reg]	<- Matrix::crossprod(M_D, Matrix::Matrix(dat.na[, varname.reg]))
#
#   }



 }


















###
###	Specifying the number of lags avaialble to derive instruments and further expanding the data set
###


#a) maximum number of lags available as instruments

 if(include.y & !(is.null(maxLags.y))){
   if(maxLags.y + 2 > T){				# [M:] maximum number of time periods of y_{it}- and x_{it}-process employed in estimation
     maxLags.y		<- T-2
     warning(cat(paste(c("Longitudinal dimension too low. Maximum number of instruments from dependent variable to be employed in estimation",
				"was therefore reduced to ", T-2, " (= T-2)."), sep = "\n")) )
   }
   if(maxLags.y < 2 & use.mc.nonlin){
     use.mc.nonlin		<- FALSE
     warning(paste("Number of lags of dependent variable too low to obtain nonlinear moment conditions; 'use.mc.nonlin' was therefore set to 'FALSE'."))
   }
 } else{
   maxLags.y			<- T-2
 }

 if(include.x){
   if(is.null(maxLags.reg.end)){
     try(if(length(maxLags.reg.end) != length(varname.reg.end)) stop("maximum number of lags of non-lagged-dependent endogenous covariates from which instruments should be derived needs to be specified completely"))
     if(any(maxLags.reg.end + 2 > T)){
       maxLags.reg.end[maxLags.reg.end > T-2]		<- T - 2
       warning(cat(paste(c("Longitudinal dimension too low. Maximum number of lags to obtain instruments from non-lagged-dependent endogenous covariates",
				"was reduced to ", T-2, " (= T-2)."), sep = "\n")) )
     }
   }
   if(!is.null(varname.reg.end) & is.null(maxLags.reg.end)){
     maxLags.reg.end						<- rep(T-2, times = length(varname.reg.end))
     warning(paste("Number of lags of the non-lagged dependent endogenous covariates from which instruments should be derived not specified. Number was set to ", T-2, " (= T-2) for the ", length(varname.reg.end), " endogenous covariates.", sep = ""))
   }
   if(!(is.null(maxLags.reg.pre))){
     try(if(length(maxLags.reg.pre) != length(varname.reg.pre)) stop("maximum number of lags of non-lagged-dependent predetermined covariates from which instruments should be derived needs to be specified completely."))
     if(any(maxLags.reg.pre + 1 > T)){
       maxLags.reg.pre[maxLags.reg.pre > T-1]		<- T - 1
       warning(cat(paste(c("Longitudinal dimension too low. Maximum number of lags to obtain instruments from non-lagged-dependent predetermined covariates",
				"was reduced to ", T-2, " (= T-2)."), sep = "\n")) )
     }
   }
   if(!(is.null(varname.reg.pre)) & is.null(maxLags.reg.pre)){
     maxLags.reg.pre						<- rep(T-1, times = length(varname.reg.pre))
     warning(cat(paste("Number of lags of non-lagged dependent predetermined covariates from which instruments should be derived not specified.",
			"Number was set to ", T-1, " (= T-1) for the ", length(varname.reg.pre), " predetermined covariates.", sep = "\n")) )
   }
   if(!(is.null(maxLags.reg.ex))){
     try(if(length(maxLags.reg.ex) != length(varname.reg.ex)) stop("maximum number of lags of non-lagged-dependent exogenous covariates from which instruments should be derived needs to be specified completely"))
     if(any(maxLags.reg.ex > T)){
       maxLags.reg.ex[maxLags.reg.ex > T]		<- T					# [M:] only required for HNR m.c. (from equ. in differences)
       warning(cat(paste(c("Longitudinal dimension too low. Maximum number of lags to obtain instruments from non-lagged-dependent exogenous covariates",
				"was reduced to ", T-2, " (= T-2)."), sep = "\n")) )
     }
   }
   if(!(is.null(varname.reg.ex)) & is.null(maxLags.reg.ex)){
     maxLags.reg.ex						<- rep(T, times = length(varname.reg.ex))
     warning(cat(paste("Number of lags of non-lagged dependent exogenous covariates from which instruments should be derived not specified.",
			"Number was set to ", T, " (= T) for the ", length(varname.reg.ex), " exogenous covariates.", sep = "\n")) )
   }
 }






#b) lags of lagged dependent variable and non-lagged dependent variable included in the model

 if(include.y & is.null(lagTerms.y)){
   lagTerms.y		<- 1
   warning(paste(c("Number of lags of lagged dependent variables on rhs of model equation not specified; 1 lag was therefore used.")))
 }

 if(include.x){
   if(!(is.null(lagTerms.reg.end))){
     try(if(length(lagTerms.reg.end) != length(varname.reg.end)) stop("number of lags of non-lagged dependent endogenous covariates needs to be specified completely."))
   }
   if(!(is.null(varname.reg.end)) & is.null(lagTerms.reg.end)){
     lagTerms.reg.end						<- rep(0, times = length(varname.reg.end))
     warning(paste("Number of lags of the non-lagged dependent endogenous covariates not specified. Number was set to 0 for all covariates.", sep = ""))
   }
   if(!(is.null(lagTerms.reg.pre))){
     try(if(length(lagTerms.reg.pre) != length(varname.reg.pre)) stop("number of AR-terms of non-lagged dependent predetermined covariates needs to be specified completely."))
   }
   if(!(is.null(varname.reg.pre)) & is.null(lagTerms.reg.pre)){
     lagTerms.reg.pre						<- rep(0, times = length(varname.reg.pre))
     warning(paste("Number of lags of the non-lagged dependent predetermined covariates not specified. Number was set to 0 for all covariates.", sep = ""))
   }
   if(!(is.null(lagTerms.reg.ex))){
     try(if(length(lagTerms.reg.ex) != length(varname.reg.ex)) stop("number of lags of non-lagged dependent exogenous covariates needs to be specified completely."))
   }
   if(!(is.null(varname.reg.ex)) & is.null(lagTerms.reg.ex)){
     lagTerms.reg.ex						<- rep(0, times = length(varname.reg.ex))
     warning(paste("Number of lags of the non-lagged dependent exogenous covariates not specified. Number was set to 0 for all covariates.", sep = ""))
   }
 }

 if(fur.con){
   if(!(is.null(lagTerms.reg.fur))){
     try(if(length(lagTerms.reg.fur) != length(varname.reg.fur)) stop("number of lags of further controls needs to be specified completely."))
   }
 }







#c) Expanding the lag structure and expanding the data set


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


 dat.na.lag		<- function(
  i
  ,varname
  ,lagTerms
 ){
  dat.na.lag.temp				<- data.table::shift(dat.na[dat.na[, varname.i] == i, varname], n = lagTerms, type = "lag")
  return(dat.na.lag.temp)
 }


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



 if(include.y){
   if(lagTerms.y > 0){
     varname.reg.estParam.y				<- do.call(what = "varname.expand", args = list(varname = varname.y, lagTerms = lagTerms.y) )
     if(length(varname.reg.estParam.y) == 1){
       dat.na[, varname.reg.estParam.y]		<- as.vector(mapply(lagTerms = rep(c(1:lagTerms.y), each = length(i_cases)), i = i_cases, varname = varname.y, FUN = dat.na.lag))
     } else{
       dat.na[, varname.reg.estParam.y]		<- mapply(lagTerms = rep(c(1:lagTerms.y), each = length(i_cases)), i = i_cases, varname = varname.y, FUN = dat.na.lag)
     }
   }
 }

 if(include.x){
   if(!(is.null(varname.reg.end))){
     varname.temp					<- if(!(is.null(varname.reg.instr))){ varname.reg.end[!(varname.reg.end %in% varname.reg.instr)]} else{ varname.reg.end }
     lagTerms.temp					<- if(!(is.null(varname.reg.instr))){ lagTerms.reg.end[!(varname.reg.end %in% varname.reg.instr)]} else{ lagTerms.reg.end }
     if(length(varname.reg.end) == 1){
       varname.reg.estParam.x.end			<- as.vector(mapply(varname = varname.temp, lagTerms = lagTerms.temp, FUN = varname.expand) )
       dat.na[, varname.reg.estParam.x.end]	<- as.vector(mapply(lagTerms = rep(mapply(lagTerms.temp, varname = varname.temp, FUN = lag.expand), each = length(i_cases)),
											i = rep(i_cases, times = length(varname.reg.estParam.x.end)),
											varname = mapply(varname.temp, FUN = rep, each = (lagTerms.reg.end+1)*length(i_cases) ),
										FUN = dat.na.lag))
     } else{
       varname.reg.estParam.x.end			<- do.call(what = "c", args = mapply(varname = varname.temp, lagTerms = lagTerms.temp, FUN = varname.expand) )
       dat.na[, varname.reg.estParam.x.end]	<- mapply(lagTerms = rep(do.call(what = "c", args = mapply(lagTerms.temp, varname = varname.temp, FUN = lag.expand)), each = length(i_cases)),
										i = rep(i_cases, times = length(varname.reg.estParam.x.end)),
										varname = do.call(what = "c", args = mapply(varname.temp, FUN = rep, each = (lagTerms.reg.end+1)*length(i_cases)) ),
									FUN = dat.na.lag)
     }
   }
   if(!(is.null(varname.reg.pre))){
     if(any(lagTerms.reg.pre > 0)){
       varname.temp					<- if(!(is.null(varname.reg.instr))){ varname.reg.pre[!(varname.reg.pre %in% varname.reg.instr)] } else{ varname.reg.pre }
       lagTerms.temp					<- if(!(is.null(varname.reg.instr))){ lagTerms.reg.pre[!(varname.reg.pre %in% varname.reg.instr)] } else{ lagTerms.reg.pre }
       if(length(varname.reg.pre) == 1){
         varname.reg.estParam.x.pre			<- as.vector(mapply(varname = varname.temp, lagTerms = lagTerms.temp, FUN = varname.expand) )
         dat.na[, varname.reg.estParam.x.pre]	<- as.vector(mapply(lagTerms = rep(mapply(lagTerms.temp, varname = varname.temp, FUN = lag.expand), each = length(i_cases)),
											i = rep(i_cases, times = length(varname.reg.estParam.x.pre)),
											varname = mapply(varname.temp, FUN = rep, each = (lagTerms.reg.pre+1)*length(i_cases) ),
										FUN = dat.na.lag))
       } else{
         varname.reg.estParam.x.pre			<- do.call(what = "c", args = mapply(varname = varname.temp, lagTerms = lagTerms.temp, FUN = varname.expand) )
         dat.na[, varname.reg.estParam.x.pre]	<- mapply(lagTerms = rep(do.call(what = "c", args = mapply(lagTerms.temp, varname = varname.temp, FUN = lag.expand)), each = length(i_cases)),
										i = rep(i_cases, times = length(varname.reg.estParam.x.pre)),
										varname = do.call(what = "c", args = mapply(varname.temp, FUN = rep, each = (lagTerms.reg.pre+1)*length(i_cases)) ),
									FUN = dat.na.lag)
       }
     }
   }
   if(!(is.null(varname.reg.ex))){
     if(any(lagTerms.reg.ex > 0)){
       varname.temp					<- if(!(is.null(varname.reg.instr))){ varname.reg.ex[!(varname.reg.ex %in% varname.reg.instr)] } else{ varname.reg.ex }
       lagTerms.temp					<- if(!(is.null(varname.reg.instr))){ lagTerms.reg.ex[!(varname.reg.ex %in% varname.reg.instr)] } else{ lagTerms.reg.ex }
       if(length(varname.reg.ex) == 1){
         varname.reg.estParam.x.ex			<- as.vector(mapply(varname = varname.temp, lagTerms = lagTerms.temp, FUN = varname.expand) )
         dat.na[, varname.reg.estParam.x.ex]	<- as.vector(mapply(lagTerms = rep(mapply(lagTerms.temp, varname = varname.temp, FUN = lag.expand), each = length(i_cases)),
											i = rep(i_cases, times = length(varname.reg.estParam.x.ex)),
											varname = mapply(varname.temp, FUN = rep, each = (lagTerms.reg.ex+1)*length(i_cases) ),
										FUN = dat.na.lag))
       } else{
         varname.reg.estParam.x.ex			<- do.call(what = "c", args = mapply(varname = varname.temp, lagTerms = lagTerms.temp, FUN = varname.expand) )
         dat.na[, varname.reg.estParam.x.ex]	<- mapply(lagTerms = rep(do.call(what = "c", args = mapply(lagTerms.temp, varname = varname.temp, FUN = lag.expand)), each = length(i_cases)),
										i = rep(i_cases, times = length(varname.reg.estParam.x.ex)),
										varname = do.call(what = "c", args = mapply(varname.temp, FUN = rep, each = (lagTerms.reg.ex+1)*length(i_cases)) ),
									FUN = dat.na.lag)
       }
     }
   }
 }

 if(fur.con){
   if(length(varname.reg.fur) ==1){
     varname.reg.estParam.fur			<- as.vector(mapply(varname = varname.reg.fur, lagTerms = lagTerms.reg.fur, FUN = varname.expand) )
     dat.na[, varname.reg.estParam.fur]	<- as.vector(mapply(lagTerms = rep(do.call(what = "c", args = mapply(lagTerms.reg.fur, FUN = lag.expand, varname = varname.reg.fur, SIMPLIFY = FALSE)), each = length(i_cases)),
								i = rep(i_cases, times = length(varname.reg.estParam.fur)),
								varname = do.call(what = "c", args = mapply(varname.reg.fur, FUN = rep, each = (lagTerms.reg.fur+1)*length(i_cases), SIMPLIFY = FALSE) ),
								FUN = dat.na.lag))
   } else{
     varname.reg.estParam.fur			<- do.call(what = "c", args = mapply(varname = varname.reg.fur, lagTerms = lagTerms.reg.fur, FUN = varname.expand, SIMPLIFY = FALSE) )
     dat.na[, varname.reg.estParam.fur]	<- mapply(lagTerms = rep(do.call(what = "c", args = mapply(lagTerms.reg.fur, FUN = lag.expand, varname = varname.reg.fur, SIMPLIFY = FALSE)), each = length(i_cases)),
								i = rep(i_cases, times = length(varname.reg.estParam.fur)),
								varname = do.call(what = "c", args = list(do.call(what = "c", args = mapply(varname.reg.fur, FUN = rep, each = (lagTerms.reg.fur+1)*length(i_cases), SIMPLIFY = FALSE))) ),
								FUN = dat.na.lag)
   }
 }

 varname.reg.estParam		 <- c(if(exists("varname.reg.estParam.y")) as.vector(varname.reg.estParam.y)			# [M:] covariates (besides the lagged dependent variable) for which to estimate parameters
						,if(exists("varname.reg.estParam.x.end")) as.vector(varname.reg.estParam.x.end)
						,if(exists("varname.reg.estParam.x.pre")) as.vector(varname.reg.estParam.x.pre)
						,if(exists("varname.reg.estParam.x.ex")) as.vector(varname.reg.estParam.x.ex)
						,if(exists("varname.reg.estParam.fur")) as.vector(varname.reg.estParam.fur) )




# varname.reg.estParam		<- do.call(what = "c", args = list(varname.reg.estParam))





 varname.reg			<- 	c( if(!(is.null(varname.reg.end))) varname.reg.end							# [M:] covariates (besides the lagged dependent variable) to include in estimation
							,if(!(is.null(varname.reg.pre))) varname.reg.pre
							,if(!(is.null(varname.reg.ex))) varname.reg.ex
							,if(!(is.null(varname.reg.estParam.fur))) as.vector(varname.reg.estParam.fur) )






 if(!(is.null(varname.reg.toInstr))){
   if(varname.reg.toInstr != varname.y){
     varname.reg.estParam	<- c(varname.reg.estParam, varname.reg.toInstr)				# [M:] include further (endogenous) covariates for which to estimate parameters, but from which no instruments should be derived
   }
   varname.reg			<- varname.reg[!(varname.reg %in% varname.reg.toInstr)]		# [M:] exclude the (endogenous) covariates for which to estimate parameters, but from which no instruments should be derived
 }
 #else{
 #  varname.reg.estParam		<- c(if(!(is.null(varname.reg.estParam.y))){ varname.reg.estParam.y }, varname.reg)
 #}









 dat					<- dat.na
 dat[is.na(dat.na)]		<- 0








###
###	Combination of Z-part of Equations (3) and (4) of AS (requires helper functions)
###



 Z.obj		<- lapply(X = i_cases, FUN = Z_i.fct, T = T, varname.i = varname.i
#					, mc.ref.t = mc.ref.t
					,use.mc.diff = use.mc.diff, use.mc.lev = use.mc.lev, use.mc.nonlin = use.mc.nonlin
					,include.y = include.y, varname.y = varname.y, inst.stata = inst.stata
					,include.dum = include.dum, dum.diff = dum.diff, dum.lev = dum.lev, colnames.dum = colnames.dum
					,fur.con = fur.con, fur.con.diff = fur.con.diff, fur.con.lev = fur.con.lev, varname.reg.estParam.fur = varname.reg.estParam.fur
   					,include.x = include.x, end.reg = end.reg, varname.reg.end = varname.reg.end, pre.reg = pre.reg, varname.reg.pre = varname.reg.pre, ex.reg = ex.reg, varname.reg.ex = varname.reg.ex
					,maxLags.y = maxLags.y, max.lagTerms = max.lagTerms, maxLags.reg.end = maxLags.reg.end, maxLags.reg.pre = maxLags.reg.pre, maxLags.reg.ex = maxLags.reg.ex, dat = dat, dat.na = dat.na)


 resGMM$n.inst		<- apply(Reduce(f = rbind, x = lapply(Z.obj, `[[`, 3)), FUN = max, MARGIN = 2)

 colnames.dum.Z		<- as.vector(unique(Reduce(f = rbind, x = lapply(Z.obj, `[[`, 2) ) ))

 resGMM$Z.temp		<- lapply(Z.obj, `[[`, 1)

 resGMM$diffMC		<- use.mc.diff
 resGMM$levMC		<- use.mc.lev
 resGMM$nlMC		<- use.mc.nonlin




 if(include.dum){
   if((dum.lev & !(dum.diff)) | (dum.lev & dum.diff)){
     varname.reg.estParam	<- c(varname.reg.estParam, colnames.dum[colnames.dum %in% colnames.dum.Z])
   } else{
     varname.reg.estParam	<- c(varname.reg.estParam, unlist(lapply(strsplit(x = colnames.dum.Z, split = "D."), FUN = `[[`, 2))[-1])
   }
 }


 resGMM$dat.na			<- dat.na
 resGMM$n				<- n
 resGMM$T				<- T

 resGMM$varname.y			<- varname.y
 resGMM$varnames.reg		<- varname.reg.estParam
 resGMM$varnames.dum		<- colnames.dum[colnames.dum %in% varname.reg.estParam]

 resGMM$estimation		<- estimation
 resGMM$opt.method		<- opt.meth
 resGMM$stderr.type		<- std.err


 resGMM$seed			<- seed.input

 set.seed(seed.input)

 if(custom.start.val){
   resGMM$param.ini			<- start.val
 } else{
   resGMM$param.ini			<- stats::runif(n = length(varname.reg.estParam), min = start.val.lo, max = start.val.up)
 }


# resGMM$param.ini			<- runif(n = nmulti*(length(varname.reg.estParam)), min = start.val.lo, max = start.val.up)
## resGMM$param.ini			<- runif(n = nmulti*(length(varname.reg.estParam)), min = -1, max = 1)
### resGMM$param.ini			<- matrix(data = runif(n = nmulti*(estimate.int + 1 + length(varname.reg.estParam)), min = -1, max = 1), nrow = nmulti)	#INT#
## resGMM$param.ini			<- matrix(data = runif(n = nmulti*(length(varname.reg.estParam)), min = -1, max = 1), nrow = nmulti)
### resGMM$param.ini			<- matrix(data = rep(0, times = length(varname.reg.estParam)) )		# [M:] Stata default




 resGMM.Dat			<- gmmDat.fct(dat.na = dat.na, n = n, T = T, varname.y = varname.y, varname.reg.estParam)




# resGMM.Dat			<- list()
#
# resGMM.Dat$y_m1		<- lapply(X = i_cases, FUN = gmmDat_m1.fct, dat.na = dat.na, T = T, varname = varname.y, varname.i = varname.i)
# resGMM.Dat$y_tm1		<- lapply(X = i_cases, FUN = gmmDat_tm1.fct, dat.na = dat.na, T = T, varname = varname.y, varname.i = varname.i)
# resGMM.Dat$dy		<- mapply(function(x,y) x - y, resGMM.Dat$y_m1,resGMM.Dat$y_tm1, SIMPLIFY = FALSE)
#
# resGMM.Dat$X_m1		<- lapply(lapply(X = i_cases, FUN = gmmDat_m1.fct, dat.na = dat.na, T = T, varname = varname.reg.estParam, varname.i = varname.i), FUN = as.matrix)
# resGMM.Dat$X_tm1		<- lapply(lapply(X = i_cases, FUN = gmmDat_tm1.fct, dat.na = dat.na, T = T, varname = varname.reg.estParam, varname.i = varname.i), FUN = as.matrix)
# resGMM.Dat$dX		<- mapply(function(x,y) x - y, resGMM.Dat$X_m1, resGMM.Dat$X_tm1, SIMPLIFY = FALSE)







###
###	Computation of weighting matrix and optimization
###

 env				<- as.numeric()
 par.opt.j			<- as.numeric()
 W.j				<- as.numeric()
 resGMM.W.j			<- list()
 resGMM.H.i			<- as.numeric()
 resGMM.opt.j		<- list()
 resGMM.par.opt.j		<- list()
 resGMM.ctrl.opt.j	<- list()
 resGMM.clF.j		<- list()
 resGMM.Szero.j		<- list()
 resGMM.fitted.j		<- list()
 resGMM.resid		<- list()
 resGMM.vcov.j		<- list()
 resGMM.stderr.j		<- list()
 resGMM.zvalue.j		<- list()
 resGMM.pvalue.j		<- list()



 j 				<- 1

 env				<- environment()


# if(estimation != "cue"){

   W.j				<- Wonestep.fct(w.mat = w.mat, w.mat.stata = w.mat.stata, use.mc.diff = use.mc.diff, use.mc.lev = use.mc.lev, use.mc.nonlin = use.mc.nonlin
						,dum.diff = dum.diff, dum.lev = dum.lev, fur.con.diff = fur.con.diff, fur.con.lev = fur.con.lev
						,Z.temp = resGMM$Z.temp, n = n, T = T, env = env
#						,mc.ref.t = mc.ref.t
						, max.lagTerms = max.lagTerms, ex.reg = ex.reg, pre.reg = pre.reg, n.inst = resGMM$n.inst, inst.thresh = inst.thresh)
   resGMM.W.j[[j]]		<- W.j
   names(resGMM.W.j)[j]		<- paste("step", j, sep = "")

   resGMM.H.i			<- H_i


   if(opt.meth == "none"){
     resGMM.opt.j[[j]]		<- rep(NA, times = length(varname.reg.estParam))
   }

   if(opt.meth != "none"){


#gmmObj.fct(j = j, param = resGMM$param.ini, y_m1 = resGMM.Dat$y_m1, X_m1 = resGMM.Dat$X_m1, dy = resGMM.Dat$dy, dX = resGMM.Dat$dX, varname.reg.estParam = varname.reg.estParam, n = n, T = T, include.y = include.y, varname.y = varname.y, use.mc.diff = use.mc.diff, use.mc.nonlin = use.mc.nonlin, use.mc.nonlinAS = use.mc.nonlinAS, use.mc.lev = use.mc.lev, dum.diff = dum.diff, fur.con.diff = fur.con.diff, max.lagTerms = max.lagTerms, end.reg = end.reg, ex.reg = ex.reg, pre.reg = pre.reg, dum.lev = dum.lev, fur.con.lev = fur.con.lev, Z.temp = resGMM$Z.temp, W = get(paste("step", j, sep = ""), resGMM.W.j))



     par.opt.j		 		<- optimx::optimx(
#         results.GMM1s			<- optimx::optimx(
##         results.GMM1s[[ro]]		<- optimx(			#[M:] in case of multi starts; multistarting is addressed more easily outside the function!
##         results.GMM1s[[ro]]		<- optimr(
##          par				= param.ini[ro]
##          par				= param.ini[ro, ]
       par = resGMM$param.ini, fn = gmmObj.fct, method = opt.meth, control = optCtrl
       ,j = j, y_m1 = resGMM.Dat$y_m1, X_m1 = resGMM.Dat$X_m1, dy = resGMM.Dat$dy, dX = resGMM.Dat$dX
       ,varname.reg.estParam = resGMM$varnames, n = n, T = T, include.y = include.y, varname.y = varname.y
       ,use.mc.diff = use.mc.diff, use.mc.nonlin = use.mc.nonlin, use.mc.nonlinAS = use.mc.nonlinAS , use.mc.lev = use.mc.lev
       ,dum.diff = dum.diff, fur.con.diff = fur.con.diff, max.lagTerms = max.lagTerms, end.reg = end.reg, ex.reg = ex.reg, pre.reg = pre.reg
       ,dum.lev = dum.lev, fur.con.lev = fur.con.lev
       ,Z.temp = resGMM$Z.temp, W = W.j, env = env
#       ,mc.ref.t = mc.ref.t, mc.ref.T = mc.ref.T, N_i = N_i
     )

     resGMM.fitted.j[[j]]		<- fitted.j
     resGMM.Szero.j[[j]]		<- Szero.j
   }
   resGMM.opt.j[[j]]			<- par.opt.j
   resGMM.par.opt.j[[j]]		<- as.numeric(resGMM.opt.j[[j]][1:length(varname.reg.estParam)])
   names(resGMM.par.opt.j)[j]		<- paste("step", j, sep = "")
   resGMM.ctrl.opt.j[[j]]		<- par.opt.j[-c(1:length(varname.reg.estParam))]
   names(resGMM.ctrl.opt.j)[j]	<- paste("step", j, sep = "")

   dat.temp			<- lapply(X = i_cases, FUN = dat.closedFormExpand.fct
					,dat.na = dat.na, varname.i = varname.i, varname.reg.instr = varname.reg.instr
					,varname.reg.toInstr = varname.reg.toInstr, varname.y = varname.y, varname.reg.estParam = varname.reg.estParam
					,use.mc.diff = use.mc.diff, use.mc.lev = use.mc.lev, use.mc.nonlin = use.mc.nonlin
					,dum.diff = dum.diff, dum.lev = dum.lev, fur.con.diff = fur.con.diff, fur.con.lev = fur.con.lev, max.lagTerms = max.lagTerms, T = T)


   dat.clF.temp		<- lapply(dat.temp, `[[`, 1)
   dat.clF.temp.0		<- rapply(lapply(dat.clF.temp, FUN = as.matrix), function(x) ifelse(is.na(x), 0, x), how = "replace")

   dep.temp			<- lapply(dat.temp, `[[`, 2)
   dep.temp.0		<- rapply(lapply(dep.temp, FUN = as.matrix), function(x) ifelse(is.na(x), 0, x), how = "replace")


#   if(use.mc.diff + use.mc.lev + use.mc.nonlin > 1){
##   if(ncol(dep.temp[[1]]) > 1){
##     n.obs.diff	<- sum(Reduce("+", lapply(dep.temp, function(x) x[,paste("D.", varname.y, sep = "")])) != 0)*n
##     n.obs.lev		<- sum(Reduce("+", lapply(dep.temp, function(x) x[,paste(varname.y, sep = "")])) != 0)*n
#     dep.temp		<- lapply(dep.temp, function(x) rowSums(x))
#
#       dat.clF.temp.diff	<- if(use.mc.diff | dum.diff | fur.con.diff) lapply(dat.clF.temp, function(x) x[, colnames(x) %in% paste("D.", varname.reg.estParam, sep = "")])
#       dat.clF.temp.nl		<- if(use.mc.nonlin) lapply(dat.clF.temp, function(x) x[, colnames(x) %in% paste("NL.D.", varname.reg.estParam, sep = "")])
#       dat.clF.temp.lev		<- if(use.mc.lev | dum.lev | fur.con.lev) lapply(dat.clF.temp, function(x) x[, colnames(x) %in% varname.reg.estParam])
#       list.temp			<- list()
#       for(i in 1:n){
#         if((use.mc.diff | dum.diff | fur.con.diff) & use.mc.nonlin & (use.mc.lev | dum.lev | fur.con.lev)){
#           list.temp[[i]]		<- dat.clF.temp.diff[[i]] + dat.clF.temp.nl[[i]] + dat.clF.temp.lev[[i]]
#         } else{
#           if((use.mc.diff | dum.diff | fur.con.diff) & use.mc.nonlin){
#             list.temp[[i]]		<- dat.clF.temp.diff[[i]] + dat.clF.temp.nl[[i]]
#           }
#           if((use.mc.diff | dum.diff | fur.con.diff) & (use.mc.lev | dum.lev | fur.con.lev)){
#             list.temp[[i]]		<- dat.clF.temp.diff[[i]] + dat.clF.temp.lev[[i]]
#           }
#           if(use.mc.nonlin & (use.mc.lev | dum.lev | fur.con.lev)){
#             list.temp[[i]]		<- dat.clF.temp.nonlin[[i]] + dat.clF.temp.lev[[i]]
#           }
#         }
#       }
#
#     dat.clF.temp		<- list.temp
#     rm(dat.clF.temp.diff, dat.clF.temp.nl, dat.clF.temp.lev, list.temp)
#   }
#
#   dat.clF.temp.0			<- rapply(lapply(dat.clF.temp, FUN = as.matrix), f = function(x) ifelse(is.na(x), 0, x), how = "replace")
#   resGMM$dat.clF.temp.0	<- dat.clF.temp.0

   tZX				<- Reduce("+", mapply(function(x,y) Matrix::crossprod(x,y), resGMM$Z.temp, dat.clF.temp.0))
   tZY				<- Reduce("+", mapply(function(x,y) Matrix::crossprod(x,y), resGMM$Z.temp, dep.temp.0))

#   tXZW1tZX.inv			<- solve(tcrossprod(crossprod(as.matrix(tZX), get(paste("step", j, sep = ""), resGMM.W.j)), t(as.matrix(tZX))))
   tXZW1tZX.inv			<- MASS::ginv(as.matrix(Matrix::tcrossprod(Matrix::crossprod(tZX, get(paste("step", j, sep = ""), resGMM.W.j)), Matrix::t(tZX))) )
   tXZW1tZY				<- Matrix::tcrossprod(Matrix::crossprod(tZX, get(paste("step", j, sep = ""), resGMM.W.j)), Matrix::t(tZY))

   if(!(use.mc.nonlin)){
     resGMM.clF.j[[j]]		<- as.numeric(Matrix::crossprod(Matrix::t(tXZW1tZX.inv), tXZW1tZY))
   } else{
     resGMM.clF.j[[j]]		<- rep(NA, times = length(varname.reg.estParam))
   }
   names(resGMM.clF.j)[j]		<- paste("step", j, sep = "")

   if(opt.meth == "none"){
     resGMM.fitted.j[[j]]		<- lapply(mapply(function(x) Matrix::tcrossprod(x, Matrix::t(get(paste("step", j, sep = ""), resGMM.clF.j))), dat.clF.temp, SIMPLIFY=FALSE), FUN = as.numeric)
     resGMM.Szero.j[[j]]		<- mapply(function(y,x) y - Matrix::tcrossprod(x, Matrix::t(get(paste("step", j, sep = ""), resGMM.clF.j))), dep.temp, dat.clF.temp, SIMPLIFY=FALSE)
     resGMM.Szero.j[[j]]		<- lapply(rapply(lapply(resGMM.Szero.j[[j]], FUN = as.matrix), f = function(x) ifelse(is.na(x), 0, x), how = "replace"), FUN = as.vector)
   }
   names(resGMM.fitted.j)[j]		<- paste("step", j, sep = "")
   names(resGMM.Szero.j)[j]		<- paste("step", j, sep = "")


   resGMM.W.j[[j+1]]		<- Wtwostep.fct(Sj.0 = get(paste("step", j, sep = "") , resGMM.Szero.j), Z.temp = resGMM$Z.temp, n.inst = sum(resGMM$n.inst), inst.thresh = inst.thresh)
   names(resGMM.W.j)[j+1]	<- paste("step", j+1, sep = "")


   n.obs				<- nrow(dat.na) - sum(is.na(dat.na[, varname.y]))
   resGMM.n.obs			<- n.obs

   if(std.err == "unadjusted"){
     resGMM.vcov.j[[j]]		<- tXZW1tZX.inv * (as.vector(crossprod(do.call(get(paste("step", j, sep = "") , resGMM.Szero.j), what = "c"), do.call(get(paste("step", j, sep = "") , resGMM.Szero.j), what = "c"), na.rm = TRUE) /(n.obs - length(varname.reg.estParam))))		# [M:] calculation acc. to description in Doornik, Arellano, and Bond (2012), p.30-31
   }
   if(std.err == "corrected"){
     resGMM.vcov.j[[j]]		<- Matrix::tcrossprod(Matrix::crossprod(tXZW1tZX.inv, Matrix::tcrossprod(Matrix::tcrossprod(Matrix::crossprod(tZX, get(paste("step", j, sep = "") , resGMM.W.j)), MASS::ginv(get(paste("step", j+1, sep = "") , resGMM.W.j))), Matrix::tcrossprod(Matrix::t(tZX), get(paste("step", j, sep = "") , resGMM.W.j)))), tXZW1tZX.inv)
   }
   names(resGMM.vcov.j)[j]	<- paste("step", j, sep = "")

   resGMM.stderr.j[[j]]		<- sqrt(diag(as.matrix(get(paste("step", j, sep = "") , resGMM.vcov.j))))
   names(resGMM.stderr.j)[j]	<- paste("step", j, sep = "")


   if(opt.meth != "none"){
     resGMM.zvalue.j[[j]]	<- as.numeric(round(get(paste("step", j, sep = ""), resGMM.par.opt.j)/get(paste("step", j, sep = "") , resGMM.stderr.j), digits = 3))
   } else{
     resGMM.zvalue.j[[j]]	<- as.numeric(round(get(paste("step", j, sep = ""), resGMM.clF.j)/get(paste("step", j, sep = "") , resGMM.stderr.j), digits = 3))
   }
   names(resGMM.zvalue.j)[j]	<- paste("step", j, sep = "")

   resGMM.pvalue.j[[j]]		<- round(2*(1 - pnorm(abs(unlist(get(paste("step", j, sep = ""), resGMM.zvalue.j))))), digits = 5)
   names(resGMM.pvalue.j)[j]	<- paste("step", j, sep = "")


   resGMM.iter 	<- j


   if(estimation != "onestep"){

     j		<- j + 1


     for(j in 2:j.max){

       if(j > 2){
         W.j				<- Wtwostep.fct(Sj.0 = get(paste("step", j-1, sep = "") , resGMM.Szero.j), Z.temp = resGMM$Z.temp, n.inst = sum(resGMM$n.inst), inst.thresh = inst.thresh)
         resGMM.W.j[[j]]		<- W.j
         names(resGMM.W.j)[j]		<- paste("step", j, sep = "")
       }

       if(opt.meth == "none"){
         resGMM.opt.j[[j]]		<- rep(NA, times = length(varname.reg.estParam))
       }

       if(opt.meth != "none"){

         par.opt.j		<- optimx::optimx(
#          results.GMM1s		<- optimx::optimx(
          par = as.numeric(par.opt.j[c(1:length(varname.reg.estParam))]), fn = gmmObj.fct, method = opt.meth, hessian = hessian, control = optCtrl
          ,j = j, Z.temp = resGMM$Z.temp, y_m1 = resGMM.Dat$y_m1, X_m1 = resGMM.Dat$X_m1, dy = resGMM.Dat$dy, dX = resGMM.Dat$dX
          ,varname.reg.estParam = resGMM$varnames, n = n, T = T, include.y = include.y, varname.y = varname.y
          ,use.mc.diff = use.mc.diff, use.mc.nonlin = use.mc.nonlin, use.mc.nonlinAS = use.mc.nonlinAS , use.mc.lev = use.mc.lev
          ,dum.diff = dum.diff, fur.con.diff = fur.con.diff, max.lagTerms = max.lagTerms, end.reg = end.reg, ex.reg = ex.reg, pre.reg = pre.reg
          ,dum.lev = dum.lev, fur.con.lev = fur.con.lev
          ,W = W.j, env = env
#         ,mc.ref.t = mc.ref.t, mc.ref.T = mc.ref.T, N_i = N_i
         )

         resGMM.fitted.j[[j]]		<- fitted.j
         resGMM.Szero.j[[j]]		<- Szero.j
       }
       resGMM.par.opt.j[[j]]		<- as.numeric(par.opt.j[1:length(varname.reg.estParam)])
       names(resGMM.par.opt.j)[j]	<- paste("step", j, sep = "")
       resGMM.ctrl.opt.j[[j]]		<- par.opt.j[-c(1:length(varname.reg.estParam))]
       names(resGMM.ctrl.opt.j)[j]	<- paste("step", j, sep = "")

       tXZW2tZX.inv			<- MASS::ginv(as.matrix(Matrix::tcrossprod(Matrix::crossprod(tZX, get(paste("step", j, sep = ""), resGMM.W.j)), Matrix::t(tZX))) )
       tXZW2tZY				<- Matrix::tcrossprod(Matrix::crossprod(tZX, get(paste("step", j, sep = ""), resGMM.W.j)), Matrix::t(tZY))

       if(!(use.mc.nonlin)){
         resGMM.clF.j[[j]]		<- as.numeric(Matrix::crossprod(Matrix::t(tXZW2tZX.inv), tXZW2tZY))
       } else{
         resGMM.clF.j[[j]]		<- rep(NA, times = length(varname.reg.estParam))
       }
       names(resGMM.clF.j)[j]		<- paste("step", j, sep = "")

       if(opt.meth == "none"){
         resGMM.fitted.j[[j]]		<- lapply(mapply(function(x) Matrix::tcrossprod(x, Matrix::t(get(paste("step", j, sep = ""), resGMM.clF.j))), dat.clF.temp, SIMPLIFY=FALSE), FUN = as.numeric)
         resGMM.Szero.j[[j]]		<- mapply(function(y,x) y - Matrix::tcrossprod(x, Matrix::t(get(paste("step", j, sep = ""), resGMM.clF.j))), dep.temp, dat.clF.temp, SIMPLIFY=FALSE)
         resGMM.Szero.j[[j]]		<- lapply(rapply(lapply(resGMM.Szero.j[[j]], FUN = as.matrix), f = function(x) ifelse(is.na(x), 0, x), how = "replace"), FUN = as.vector)
       }
       names(resGMM.fitted.j)[j]	<- paste("step", j, sep = "")
       names(resGMM.Szero.j)[j]	<- paste("step", j, sep = "")


       resGMM.vcov.j[[j]]		<- MASS::ginv(as.matrix(Matrix::tcrossprod(Matrix::crossprod(tZX, get(paste("step", j, sep = ""), resGMM.W.j)), Matrix::t(tZX) )))
       names(resGMM.vcov.j)[j]	<- paste("step", j, sep = "")
       resGMM.stderr.j[[j]]		<- sqrt(diag(as.matrix(get(paste("step", j, sep = ""), resGMM.vcov.j)) ))
       names(resGMM.stderr.j)[j]	<- paste("step", j, sep = "")


       tZ.res2s				<- Reduce("+", mapply(function(x,y) Matrix::crossprod(x,y), resGMM$Z.temp, get(paste("step", j, sep = ""), resGMM.Szero.j)))

       D					<- c()

       for(k in 1:length(varname.reg.estParam)){
         x_ktu	<- mapply(function(x,y){
				  z		<- Matrix::tcrossprod(x[, k], y)
							 - z - t(z)						#[M:] Code line from R-code of 'vcovHC.pgmm'; '-z' multiplies all elements with (-1); '-t(z)' adds up the off-diagonal elements
				}, dat.clF.temp.0, get(paste("step", j-1, sep = ""), resGMM.Szero.j), SIMPLIFY = FALSE)
         tZtux_kZ	<- Reduce("+", mapply(function(x,y) Matrix::crossprod(x, Matrix::crossprod(y,x)), resGMM$Z.temp, x_ktu, SIMPLIFY = FALSE))
         D_k	<- Matrix::crossprod((-1)*get(paste("step", j, sep = ""), resGMM.vcov.j), Matrix::crossprod(Matrix::crossprod(get(paste("step", j, sep = ""), resGMM.W.j), tZX), Matrix::tcrossprod(tZtux_kZ, Matrix::tcrossprod(Matrix::t(tZ.res2s), get(paste("step", j, sep = ""), resGMM.W.j)))))
         D		<- cbind(D, D_k)
       }


       resGMM.vcov.j[[j]]		<- get(paste("step", j, sep = ""), resGMM.vcov.j) + Matrix::crossprod(Matrix::t(D), get(paste("step", j, sep = ""), resGMM.vcov.j)) + Matrix::t(Matrix::crossprod(Matrix::t(D), get(paste("step", j, sep = ""), resGMM.vcov.j))) + Matrix::tcrossprod(Matrix::tcrossprod(D, get(paste("step", 1, sep = ""), resGMM.vcov.j)), D)
       resGMM.stderr.j[[j]]		<- sqrt(diag(as.matrix(get(paste("step", j, sep = ""), resGMM.vcov.j))))



       if(opt.meth != "none"){
         resGMM.zvalue.j[[j]]		<- as.numeric(round(get(paste("step", j, sep = ""), resGMM.par.opt.j)/get(paste("step", j, sep = "") , resGMM.stderr.j), digits = 3))
       } else{
         resGMM.zvalue.j[[j]]		<- as.numeric(round(get(paste("step", j, sep = ""), resGMM.clF.j)/get(paste("step", j, sep = "") , resGMM.stderr.j), digits = 3))
       }
       names(resGMM.zvalue.j)[j]	<- paste("step", j, sep = "")

       resGMM.pvalue.j[[j]]		<- round(2*(1 - pnorm(abs(unlist(get(paste("step", j, sep = ""), resGMM.zvalue.j))))), digits = 5)
       names(resGMM.pvalue.j)[j]	<- paste("step", j, sep = "")



### CHECK
#
# gmmObj.fct(
#   j = j, param = rep(x = 0.5, times = length(varname.reg.estParam)), Z.temp = Z.temp
#   , y_m1 = resGMM.Dat$y_m1, X_m1 = resGMM.Dat$X_m1, dy = resGMM.Dat$dy, dX = resGMM.Dat$dX ,varname.reg.estParam = resGMM$varnames, n = n, T = T
#   , include.y = include.y, varname.y = varname.y, use.mc.diff = use.mc.diff, use.mc.nonlin = use.mc.nonlin, use.mc.nonlinAS = use.mc.nonlinAS, use.mc.lev = use.mc.lev
#   , dum.diff = dum.diff, fur.con.diff = fur.con.diff, max.lagTerms = max.lagTerms, end.reg = end.reg, ex.reg = ex.reg, pre.reg = pre.reg
#   , dum.lev = dum.lev, fur.con.lev = fur.con.lev, mc.ref.t	= mc.ref.t, W = resGMM$W1
##   ,mc.ref.T = mc.ref.T, N_i = N_i
# )
###


     resGMM.iter 	<- j

     if(opt.meth != "none"){
       if((j > 2) && ((j > j.max) | (sum(abs(as.numeric(get(paste("step", j, sep = "") , resGMM.par.opt.j)) - as.numeric(get(paste("step", j-1, sep = "") , resGMM.par.opt.j))))) < iter.tol) ) break
     } else{
       if((j > 2) && ((j > j.max) | (sum(abs(as.numeric(get(paste("step", j, sep = "") , resGMM.clF.j)) - as.numeric(get(paste("step", j-1, sep = "") , resGMM.clF.j))))) < iter.tol) ) break
     }
   }

 }


# if(estimation == "cue"){
#
#   res.GMM.cue		<- list()
#
#   resGMM.cue 		<- optimx(
#    j = j, par = resGMM$param.ini, fn = gmm_cueObj.fct, method = opt.meth, hessian = FALSE, control = optCtrl
#    ,Z.temp = Z.temp, y_m1 = resGMM.Dat$y_m1, X_m1 = resGMM.Dat$X_m1, dy = resGMM.Dat$dy, dX = resGMM.Dat$dX
#    ,varname.reg.estParam = resGMM$varnames, n = n, T = T, include.y = include.y, varname.y = varname.y
#    ,use.mc.diff = use.mc.diff, use.mc.nonlin = use.mc.nonlin, use.mc.nonlinAS = use.mc.nonlinAS , use.mc.lev = use.mc.lev
#    ,dum.diff = dum.diff, fur.con.diff = fur.con.diff, max.lagTerms = max.lagTerms, end.reg = end.reg, ex.reg = ex.reg, pre.reg = pre.reg
#    ,dum.lev = dum.lev, fur.con.lev = fur.con.lev, mc.ref.t = mc.ref.t
##    ,mc.ref.T = mc.ref.T, N_i = N_i
#   )
#
#
#
# }


 fit 		<-  list(data = resGMM, dep.clF = dep.temp, dat.clF = dat.clF.temp, w.mat = resGMM.W.j, H_i = resGMM.H.i, par.optim = resGMM.par.opt.j, ctrl.optim = resGMM.ctrl.opt.j, par.clForm = resGMM.clF.j, iter = resGMM.iter,
				fitted.values = resGMM.fitted.j, residuals = resGMM.Szero.j, vcov = resGMM.vcov.j, stderr = resGMM.stderr.j, zvalue = resGMM.zvalue.j, pvalue = resGMM.pvalue.j)
 class(fit)	<- "pdynmc"

 return(fit)


}








































##################################################################################
###	Define residuals, fitted, predict, and summary methods for class 'pdynmc'
##################################################################################












#' Extract fitted values.
#'
#' \code{fitted.pdynmc} extracts fitted values of an object of class
#'    `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param step An integer denoting the iteration step for which fitted values
#'    are extracted (defaults to last iteration step used for obtaining parameter
#'    estimates).
#' @param na.rm A logical variable indicating whether missing values should be
#'    removed from the vector of fitted values (defaults to `FALSE`).
#' @param ... further arguments.
#'
#' @return Extract fitted values from object of class `pdynmc`.
#'
#' @export
#' @importFrom stats na.omit
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' \donttest{
#' data(EmplUK, package = "plm")
#' dat <- EmplUK
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'    opt.meth = "none")
#' fitted(m1, na.rm = TRUE)
#' }
#'
#'
fitted.pdynmc		<- function(object, step = object$iter, na.rm = FALSE, ...){
  if(na.rm == TRUE){
    fit.pd	<- stats::na.omit(get(paste("step", step, sep = "") , object$fitted.values))
  } else{
    fit.pd	<- get(paste("step", step, sep = "") , object$fitted.values)
  }
  return(fit.pd)
}





















#' Extract residuals.
#'
#' \code{residual.pdynmc} extracts residuals of an object of class
#'    `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param step An integer denoting the iteration step for which fitted values
#'    are extracted (defaults to last iteration step used for obtaining parameter
#'    estimates).
#' @param na.rm A logical variable indicating whether missing values should be
#'    removed from the vector of fitted values (defaults to `FALSE`).
#' @param ... further arguments.
#'
#' @return Extract residuals from object of class `pdynmc`.
#'
#' @export
#' @importFrom stats na.omit
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' \donttest{
#' data(EmplUK, package = "plm")
#' dat <- EmplUK
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'    opt.meth = "none")
#' residuals.pdynmc(m1, na.rm = TRUE)
#' }
#'
#'
residuals.pdynmc		<- function(object, step = object$iter, na.rm = FALSE, ...){
  if(na.rm == TRUE){
    res.pd	<- stats::na.omit(get(paste("step", step, sep = "") , object$residuals))
  } else{
    res.pd	<- get(paste("step", step, sep = "") , object$residuals)
  }
  return(res.pd)
}




















#' Extract variance covariance matrix.
#'
#' \code{vcov.pdynmc} extracts variance covariance matrix of the paramter
#'    estimates of an object of class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param step An integer denoting the iteration step for which fitted values
#'    are extracted (defaults to last iteration step used for obtaining parameter
#'    estimates).
#' @param ... further arguments.
#'
#' @return Extract variance covariance matrix of the paramter estimates from
#'    object of class `pdynmc`.
#'
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' \donttest{
#' data(EmplUK, package = "plm")
#' dat <- EmplUK
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'    opt.meth = "none")
#' vcov.pdynmc(m1)
#' }
#'
#'
vcov.pdynmc		<- function(object, step = object$iter, ...){
  vcov	<- get(paste("step", step, sep = "") , object$vcov)
  return(vcov)
}



























#' Extract weighting matrix.
#'
#' \code{wmat.pdynmc} extracts weighting matrix of an object of class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param step An integer denoting the iteration step for which fitted values
#'    are extracted (defaults to last iteration step used for obtaining parameter
#'    estimates).
#' @param ... further arguments.
#'
#' @return Extract weighting matrix from an object of class `pdynmc`.
#'
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' \donttest{
#' data(EmplUK, package = "plm")
#' dat <- EmplUK
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'    opt.meth = "none")
#' wmat.pdynmc(m1)
#' }
#'
#'
wmat.pdynmc		<- function(object, step = object$iter, ...){
  wmat	<- get(paste("step", step, sep = "") , object$w.mat)
  return(wmat)
}








































#' Summary for objects of class `pdynmc`.
#'
#' \code{summary.pdynmc} generates the summary for objects of class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param ... further arguments.
#'
#' @return Object of class `summary.pdynmc`.
#'
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' \donttest{
#' data(EmplUK, package = "plm")
#' dat <- EmplUK
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'    opt.meth = "none")
#' summary.pdynmc(m1)
#' }
#'
#'
summary.pdynmc	<- function(object, ...){

  step		<- object$iter
  est			<- object$data$estimation
  object$n.obs	<- object$data$n * object$data$n - length(object$data$dat.na[is.na(object$data$dat.na[, object$data$varname.y]), ])
  object$unbal	<- length(object$data$dat.na[is.na(object$data$dat.na[, object$data$varname.y]), ]) > 0

  coef.est		<- as.numeric(if(object$data$opt.method == "none"){ get(paste("step", step, sep = ""), object$par.clForm)} else{get(paste("step", step, sep = ""), object$par.optim)})
  varnames.reg	<- object$data$varnames.reg

  stderr		<- get(paste("step", step, sep = ""), object$stderr)
  zvalue		<- get(paste("step", step, sep = ""), object$zvalue)
  pvalue		<- get(paste("step", step, sep = ""), object$pvalue)

  object$coefficients			<- cbind(coef.est, stderr, zvalue, pvalue)
  colnames(object$coefficients)	<- if(object$data$stderr.type != "corrected") {c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")} else{c("Estimate", "Std.Err.rob", "z-value.rob", "Pr(>|z.rob|)")}
  rownames(object$coefficients)	<- object$data$varnames.reg

  object$hansenj		<- jtest.fct(object)

  object$slopef		<- wald.fct(param = "slope", object = object)
  object$time.dumf	<- wald.fct(param = "time.dum", object = object)

  class(object)		<- append(class(object), "summary.pdynmc")
  return(object)
}


























#' Print summary for objects of class `pdynmc`.
#'
#' \code{print.summary.pdynmc} prints the summary for objects of class
#'    `pdynmc`.
#'
#' @param x An object of class `summary.pdynmc`.
#' @param digits An integer indicating the maximum number of digits to display in the object.
#' @param width Argument is defined as in \code{\link{options}}.
#' @param ... further arguments.
#'
#' @return Print information on objcets of class `summary.pdynmc`.
#'
#' @export
#' @importFrom stats coef
#' @importFrom stats printCoefmat
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' \donttest{
#' data(EmplUK, package = "plm")
#' dat <- EmplUK
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'    opt.meth = "none")
#' summary.pdynmc(m1)
#' }
#'
#'
print.summary.pdynmc	<- function(x, digits = max(3, getOption("digits") - 3), width = getOption("width"), ...){

#  cat(formula(paste(x$data$varname.y, paste(x$data$varnames.reg, collapse = "+"), sep = " ~ ")))
#  cat("\n")
  cat(paste("Dynamic linear panel estimation (", x$data$estimation, ")", "\n", sep = ""))
  cat(paste("Moment conditions: ", if(x$data$diffMC){ "linear (DIF)" }, if(x$data$levMC){ " linear (LEV)" }, if(x$data$nlMC){ " nonlinear" }, "\n", sep = ""))
  cat(paste("Estimation steps: ", x$iter, "\n", sep = ""))
  cat("\n")
  stats::printCoefmat(stats::coef(x), digits = digits)
  cat("\n")
#  cat(paste("Total Sum of Squares ", round(x$tss, digits = digits), "\n", sep = ""))
#  cat(paste("Residual Sum of Squares ", round(x$rss, digits = digits), "\n", sep = ""))
  cat(paste(sum(x$data$n.inst), " total instruments are employed to estimate ", length(x$data$varnames.reg), " parameters", "\n", sep = ""))
  cat(paste("J-Test (overid restrictions): ", round(x$hansenj$statistic, digits = 2), " with ", x$hansenj$parameter, " DF, pvalue: ", if(x$hansenj$p.value < 0.001){paste("<0.001")} else{round(x$hansenj$p.value, digits = digits)}, "\n", sep = ""))
  cat(paste("F-Statistic (slope coeff): ", round(x$slopef$statistic, digits = 2), " with ", x$slopef$parameter, " DF, pvalue: ", if(x$slopef$p.value < 0.001){paste("<0.001")} else{round(x$slopef$p.value, digits = digits)}, "\n", sep = ""))
  cat(paste("F-Statistic (time dummies): ", round(x$time.dumf$statistic, digits = 2), " with ", x$time.dumf$parameter, " DF, pvalue: ", if(x$time.dumf$p.value < 0.001){paste("<0.001")} else{round(x$slopef$p.value, digits = digits)} ) )
}
















