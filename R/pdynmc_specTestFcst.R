
#####################################################
###	Version information
#####################################################

###
###	Starting point
###

#	AhnSchmidt_Nonlinear_2017-11-07.R

#	split into different functions as of code version
#	AhnSchmidt_Nonlinear_2019-04-08.R


















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
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'  dat <- dat[c(140:0), ]
#'
#' ## Code example
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'     use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'     include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'     fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'     varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'     include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'     w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'     opt.meth = "none")
#'  wald.fct(param = "all", m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' ## Further code example
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'     use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'     include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'     fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'     varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'     include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'     w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'     opt.meth = "none")
#'  wald.fct(param = "all", m1)
#' }
#' }
#'
#'
wald.fct 		<- function(
 param
 ,object
){

  if(!(all(inherits(object, "pdynmc")))) stop("Object needs to be of class 'pdynmc'")

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
  Time					<- object$data$Time
  n.inst				<- object$data$n.inst
  Szero.j				<- get(paste("step", object$iter, sep = ""), object$residuals)



  K.tot		<- length(coef.est)
  if(!is.character(varname.dum)){
    K.t			<- length(varname.dum)
  } else{
    K.t     <- 0
  }

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
#				(as.vector(crossprod(do.call(res.1s_temp, what = "c"), do.call(Szero.j, what = "c"), na.rm = TRUE) /(n*Time - sum(n.inst)+7)))						#[M:] Stata results with different dof-correction
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
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'  dat <- dat[c(140:0), ]
#'
#' ## Code example
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'     use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'     include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'     fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'     varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'     include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'     w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'     opt.meth = "none")
#'  jtest.fct(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' ## Further code example
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'     use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'     include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'     fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'     varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'     include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'     w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'     opt.meth = "none")
#'  jtest.fct(m1)
#' }
#' }
#'
#'
jtest.fct		<- function(
 object
){

  if(!(all(inherits(object, "pdynmc")))) stop("Object needs to be of class 'pdynmc'")

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
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'  dat <- dat[c(140:0), ]
#'
#' ## Code example
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'     use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'     include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'     fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'     varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'     include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'     w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'     opt.meth = "none")
#'  mtest.fct(m1, t.order = 2)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' ## Further code example
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'     use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'     include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'     fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'     varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'     include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'     w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'     opt.meth = "none")
#'  mtest.fct(m1, t.order = 2)
#' }
#' }
#'
#'
mtest.fct 		<- function(
 object
 ,t.order
){

  if(!(all(inherits(object, "pdynmc")))) stop("Object needs to be of class 'pdynmc'")

  estimation	<- object$data$estimation
  Szero.j			<- get(paste("step", object$iter, sep = ""), object$residuals)
  Z.temp			<- object$data$Z
  vcov.est		<- get(paste("step", object$iter, sep = ""), object$vcov)
  W.j				  <- get(paste("step", object$iter, sep = ""), object$w.mat)

  stderr.type	<- object$data$stderr.type
  std.err			<- get(paste("step", object$iter, sep = ""), object$stderr)
  n.inst			<- object$data$n.inst
  n				    <- object$data$n
  Time				    <- object$data$Time
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















