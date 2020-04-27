
#####################################################
###	Version information
#####################################################

###
###	Starting point
###

#	AhnSchmidt_Nonlinear_2017-11-07.R

#	split into different functions as of code version
#	AhnSchmidt_Nonlinear_2019-04-08.R





















##################################################################################
###	Define residuals, fitted, predict, print, and summary methods for class 'pdynmc'
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
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'  dat <- dat[c(1:140), ]
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
#'  fitted(m1, na.rm = TRUE)
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
#'  fitted(m1, na.rm = TRUE)
#' }
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
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'  dat <- dat[c(1:140), ]
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
#'  residuals(m1, na.rm = TRUE)
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
#'  residuals(m1, na.rm = TRUE)
#' }
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
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'  dat <- dat[c(1:140), ]
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
#'  vcov(m1)
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
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'     use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'     include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'     fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'     varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'     include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'     w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'     opt.meth = "none")
#'  vcov(m1)
#' }
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
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'  dat <- dat[c(1:140), ]
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
#'  wmat.pdynmc(m1)
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
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'     use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'     include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'     fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'     varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'     include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'     w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'     opt.meth = "none")
#'  wmat.pdynmc(m1)
#' }
#' }
#'
#'
wmat.pdynmc		<- function(object, step = object$iter, ...){
  wmat	<- get(paste("step", step, sep = "") , object$w.mat)
  return(wmat)
}

































#' Print objects of class `pdynmc`.
#'
#' \code{print.pdynmc} prints objects of class `pdynmc`.
#'
#' @param x An object of class `pdynmc`.
#' @param digits An integer indicating the maximum number of digits to display in the object.
#' @param ... further arguments.
#'
#' @return Print objects of class `pdynmc`.
#'
#' @export
#' @importFrom stats coef
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'  dat <- dat[c(1:140), ]
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
#'  m1
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
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'     use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'     include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'     fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'     varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'     include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'     w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'     opt.meth = "none")
#'  m1
#' }
#' }
#'
#'
print.pdynmc	<- function(x, digits = max(3, getOption("digits") - 3), ...){

  cat("\nDynamic linear panel estimation", paste(" (", x$data$estimation, ")", sep = ""), sep = "")
  cat("\nMoment conditions: ", paste(if(x$data$diffMC){ "linear (DIF)" }, if(x$data$levMC){ " linear (LEV)" }, if(x$data$nlMC){ " nonlinear" }), sep = "")
  cat("\nEstimation steps: ", paste(x$iter), sep = "")
  cat("\n")
  cat("\nCoefficients:\n")
  print.default(format(coef(x), digits = digits), print.gap = 2L, quote = FALSE)
  cat("\n")
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
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'  dat <- dat[c(1:140), ]
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
#'  summary(m1, na.rm = TRUE)
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
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'     use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'     include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'     fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'     varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'     include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'     w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'     opt.meth = "none")
#'  summary(m1)
#' }
#' }
#'
#'
summary.pdynmc	<- function(object, ...){

  step		<- object$iter
  est			<- object$data$estimation
  object$n.obs	<- object$data$n * object$data$n - length(object$data$dat.na[is.na(object$data$dat.na[, object$data$varname.y]), ])
  object$unbal	<- length(object$data$dat.na[is.na(object$data$dat.na[, object$data$varname.y]), ]) > 0

  coef.est		  <- as.numeric(if(object$data$opt.method == "none"){ get(paste("step", step, sep = ""), object$par.clForm)} else{get(paste("step", step, sep = ""), object$par.optim)})
  varnames.reg	<- object$data$varnames.reg

  stderr		<- get(paste("step", step, sep = ""), object$stderr)
  zvalue		<- get(paste("step", step, sep = ""), object$zvalue)
  pvalue		<- get(paste("step", step, sep = ""), object$pvalue)

  object$coefficients			<- cbind(coef.est, stderr, zvalue, pvalue)
  colnames(object$coefficients)	<- if(object$data$stderr.type != "corrected") {c("Estimate", "Std.Err", "z-value", "Pr(>|z|)")} else{c("Estimate", "Std.Err.rob", "z-value.rob", "Pr(>|z.rob|)")}
  rownames(object$coefficients)	<- object$data$varnames.reg

  object$hansenj		<- jtest.fct(object)

  object$slopef		  <- wald.fct(param = "slope", object = object)
  if(length(object$data$varnames.dum) > 1){
    object$time.dumf	<- wald.fct(param = "time.dum", object = object)
  } else{
    if(object$data$varnames.dum == "no time dummies"){
      object$time.dumf	<- "no times dummies included in estimation"
    } else{
      object$time.dumf	<- wald.fct(param = "time.dum", object = object)
    }
  }

  class(object)		<- "summary.pdynmc"
  return(object)
}


























#' Print summary for objects of class `pdynmc`.
#'
#' \code{print.summary.pdynmc} prints the summary for objects of class
#'    `pdynmc`.
#'
#' @param x An object of class `summary.pdynmc`.
#' @param digits An integer indicating the maximum number of digits to display in the object.
#' @param signif.stars Argument is defined as in \code{\link{options}}.
#' @param ... further arguments.
#'
#' @return Print information on objects of class `summary.pdynmc`.
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
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example. Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'  dat <- dat[c(1:140), ]
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
#'  summary(m1)
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
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'     use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'     include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'     fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'     varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'     include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'     w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'     opt.meth = "none")
#'  summary(m1)
#' }
#' }
#'
#'
print.summary.pdynmc	<- function(x, digits = max(3, getOption("digits") - 3), signif.stars = getOption("show.signif.stars"), ...){

#  cat(formula(paste(x$data$varname.y, paste(x$data$varnames.reg, collapse = "+"), sep = " ~ ")))
#  cat("\n")
  cat("\nDynamic linear panel estimation", paste(" (", x$data$estimation, ")", sep = ""), sep = "")
  cat("\nMoment conditions: ", paste(if(x$data$diffMC){ "linear (DIF)" }, if(x$data$levMC){ " linear (LEV)" }, if(x$data$nlMC){ " nonlinear" }), sep = "")
  cat("\nEstimation steps: ", paste(x$iter), "\n", sep = "")
  cat("\nCoefficients:\n")
  printCoefmat(coef(x), digits = digits, signif.stars = signif.stars, na.print = "NA", ...)
#  cat(paste("Total Sum of Squares ", round(x$tss, digits = digits), "\n", sep = ""))
#  cat(paste("Residual Sum of Squares ", round(x$rss, digits = digits), "\n", sep = ""))
  cat("\n", paste(sum(x$data$n.inst), " total instruments are employed to estimate ", length(x$data$varnames.reg), " parameters", "\n", sep = ""))
  cat("\nJ-Test (overid restrictions): ", paste(round(x$hansenj$statistic, digits = 2), " with ", x$hansenj$parameter, " DF, pvalue: ", if(x$hansenj$p.value < 0.001){paste("<0.001")} else{round(x$hansenj$p.value, digits = digits)}, sep = ""))
  cat("\nF-Statistic (slope coeff): ", paste(round(x$slopef$statistic, digits = 2), " with ", x$slopef$parameter, " DF, pvalue: ", if(x$slopef$p.value < 0.001){paste("<0.001")} else{round(x$slopef$p.value, digits = digits)}, sep = ""))
  cat("\nF-Statistic (time dummies): ", if(length(x$time.dumf) == 1){ "no time dummies included in estimation" } else{ paste(round(x$time.dumf$statistic, digits = 2), " with ", x$time.dumf$parameter, " DF, pvalue: ", if(x$time.dumf$p.value < 0.001){paste("<0.001")} else{round(x$time.dumf$p.value, digits = digits)}, sep = "")} )
}



































