
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











#' Case and Variable Names of Fitted Model.
#'
#' \code{case.names} extracts variable names of cross-sectional and
#'    longitudinal identifiers of an object of class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param ... further arguments.
#'
#' @return A list containing tow character vectors with the variable
#'    names of the cross-sectional and the longitudinal identifiers
#'    from object of class `pdynmc`.
#'
#' @author Markus Fritsch
#' @export
#' @importFrom stats case.names
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  case.names(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  case.names(m1)
#' }
#' }
#'
#'
case.names.pdynmc <- function(object, ...){

  cn.pd <- list("cross-section id" = as.character(unique(object$data$dat.na[ , object$data$varname.i])),
                "longitudinal id" = as.character(unique(object$data$dat.na[ , object$data$varname.t])))

  return(cn.pd)
}













#' Extract Coefficient Estimates of Fitted Model.
#'
#' \code{coef.pdynmc} extracts coefficient estimates of an object
#'    of class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param ... further arguments.
#'
#' @return Extract coefficient estimates from object of class
#'    `pdynmc`.
#'
#' @author Markus Fritsch
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  coef(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  coef(m1)
#' }
#' }
#'
#'
coef.pdynmc <- function(object, ...){

  coef.pd <- object$coefficients

  return(coef.pd)
}















#' Extract Coefficient Estimates of Time Dummies of Fitted Model.
#'
#' \code{dummy.coef.pdynmc} extracts coefficient estimates of
#'    time dummies of an object of class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param ... further arguments.
#'
#' @return Extract coefficient estimates of time dummies from
#'    object of class `pdynmc`.
#'
#' @author Markus Fritsch
#' @export
#' @importFrom stats dummy.coef
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  dummy.coef(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  dummy.coef(m1)
#' }
#' }
#'
#'
dummy.coef.pdynmc		<- function(object, ...){

  if(!inherits(object, what = "pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }
  if(length(object$data$varnames.dum) == 1 &&  object$data$varnames.dum == "no time dummies"){
    stop("No time dummies included in estimation.")
  }

  dum.pd <- object$coefficients[object$data$varnames.reg %in% object$data$varnames.dum]

  return(dum.pd)
}
















#' Extract Fitted Values of Fitted Model.
#'
#' \code{fitted.pdynmc} extracts fitted values of an object of class
#'    `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param step An integer denoting the iteration step for which
#'    fitted values are extracted (defaults to last iteration step
#'    used for obtaining parameter estimates).
#' @param na.rm A logical variable indicating whether missing values
#'    should be removed from the vector of fitted values (defaults
#'    to `FALSE`).
#' @param ... further arguments.
#'
#' @return Extract fitted values from object of class `pdynmc`.
#'
#' @author Markus Fritsch
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
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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

  if(!inherits(object, what = "pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }

  if(na.rm == TRUE){
    fit.pd	<- stats::na.omit(get(paste("step", step, sep = "") , object$fitted.values))
  } else{
    fit.pd	<- get(paste("step", step, sep = "") , object$fitted.values)
  }
  return(fit.pd)
}































#' Extract Instrument Matrix of Fitted Model.
#'
#' \code{model.matrix.pdynmc} extracts explanatory variables of an
#'    object of class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param sparse Whether to return a sparse matrix (if set to 'TRUE')
#'    or a regular matrix (if set to 'FALSE').
#' @param ... further arguments.
#'
#' @return Extracts instrument matrix from an object of class `pdynmc`.
#'
#' @author Markus Fritsch
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  model.matrix(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  model.matrix(m1)
#' }
#' }
#'
#'
model.matrix.pdynmc		<- function(object, sparse = TRUE, ...){

  if(!inherits(object, what = "pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }

  if(sparse == TRUE){
    modmat <- object$data$Z.temp
  } else{
    modmat <- as.matrix(do.call(what = rbind, object$data$Z.temp, ...))
  }
  return(modmat)
}














#' Extract Instrument Count of Fitted Model.
#'
#' \code{ninst} is a generic function fo extracting the instrument
#'    count of an object.
#'
#' @param object An object for which the instrument count is desired.
#' @param ... further arguments.
#'
#' @return Extracts instrument count from an object.
#'
#' @author Markus Fritsch
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  ninst(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  ninst(m1)
#' }
#' }
#'
#'
ninst <- function(object, ...){
  UseMethod("ninst", object)
}


#' Extract Instrument Count of Fitted Model.
#'
#' \code{ninst.pdynmc} extracts instrument count of an object of
#'    class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param ... further arguments.
#'
#' @return Extracts instrument count from an object of class `pdynmc`.
#'
#' @author Markus Fritsch
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  ninst(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  ninst(m1)
#' }
#' }
#'
#'
ninst.pdynmc		<- function(object, ...){

  if(!inherits(object, what = "pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }

  ninsts <- object$data$n.inst

  return(ninsts)
}

















#' Extract Number of Observations of Fitted Model.
#'
#' \code{nobs.pdynmc} extracts number of observations in cross-section
#'    dimension and longitudinal dimension of an object of class
#'    `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param ... further arguments.
#'
#' @return Extracts number of observations in cross-section dimension
#'    and longitudinal dimension of an object of class `pdynmc`.
#'
#' @author Markus Fritsch
#' @export
#' @importFrom stats nobs
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  nobs(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  nobs(m1)
#' }
#' }
#'
#'
nobs.pdynmc		<- function(object, ...){

  if(!inherits(object, what = "pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }

  nob.i <- object$data$n
  nob.t <- object$data$Time

  cat("Cross-section dimension ", paste("n = ", nob.i, sep = ""), "\n", sep = "")
  cat("longitudinal dimension ", paste("n = ", nob.t, sep = ""), "\n", sep = "")
}


















#' Extract Input Parameters of Numeric Optimization of Fitted Model.
#'
#' \code{optimIn} is a generic function for extracting input parameters
#'    of numeric optimization for an object.
#'
#' @param object An object for which input parameters of numeric
#'    optimization are desired.
#' @param ... further arguments.
#'
#' @return \code{optimIn} extracts input parameters used in numeric
#'    optimization from object.
#'
#' @author Markus Fritsch
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
#' } else{
#'  data(EmplUK, package = "plm")
#'  dat <- EmplUK
#'  dat[,c(4:7)] <- log(dat[,c(4:7)])
#'  dat <- dat[c(1:35), ]
#'  dat <- subset(dat, year < 1981)
#'
#' ## Code example
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'     use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'     include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'     fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'     varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'     include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'     w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'     opt.meth = "BFGS")
#'  optimIn(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'     opt.meth = "BFGS")
#'  optimIn(m1)
#' }
#' }
#'
#'
optimIn <- function(object, ...){
  UseMethod("optimIn", object)
}


#' Extract Input Parameters of Numeric Optimization of Fitted Model.
#'
#' \code{optimIn.pdynmc} extracts input parameters of numeric
#'    optimization for an object of class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param step An integer denoting the iteration step for which input
#'    parameters are extracted (defaults to last iteration step used
#'    for obtaining parameter estimates).
#' @param ... further arguments.
#'
#' @return Extracts input parameters of numeric optimization from
#'    object of class `pdynmc`.
#'
#' @author Markus Fritsch
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  optimIn(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'     opt.meth = "BFGS")
#'  optimIn(m1)
#' }
#' }
#'
#'
optimIn.pdynmc		<- function(object, step = object$iter, ...){

  if(!inherits(object, what = "pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }

  if(object$data$opt.method == "none"){
    ret <- paste("No parameter inputs can be extracted (no numerical optimization carried out).")
  } else {
    ret	<- get(paste("step", step, sep = "") , object$ctrl.optim, ...)
    return(ret)
  }
}
















#' Plot Coefficient Estimates and Corresponding Ranges of Fitted Model.
#'
#' \code{plot.pdynmc} Plot coefficient estimates and corresponding
#'    coefficient estimate ranges for objects of class `pdynmc`.
#'
#' @param x An object of class `pdynmc`. The function requires
#'    twostep or iterative GMM estimates.
#' @param type Wether to plot fitted values against residuals (argument
#'    'fire'; default) or coefficient ranges (argument 'coef.range';
#'    this requires twostep or iterative GMM estimates.
#' @param include.dum Include estimates of parameters corresponding to time
#'    dummies (defaults to 'FALSE'; requires 'type = coef.range').
#' @param include.fur.con Include estimates of parameters corresponding to
#'    further controls (defaults to 'FALSE'; requires 'type = coef.range').
#' @param col.coefRange Specify color for plotting range of coefficient
#'    estimates (defaults to 'black'; requires 'type = coef.range').
#' @param col.coefInitial Specify color for plotting initial coefficient
#'    estimates (defaults to 'darkgrey'; requires 'type = coef.range').
#' @param col.coefEst Specify color for plotting coefficient estimate
#'    (defaults to 'royalblue'; requires 'type = coef.range').
#' @param boxplot.coef Wether to draw boxplots for coefficient estimates
#'    (defaults to 'FALSE'); requires iterative GMM with at least 10
#'    iterations and argument 'type = coef.range'. Proceed with caution
#'    as this argument is experimental.
#' @param ... further arguments.
#'
#' @return Plot fitted values against residuals ('type = fire') or
#'    coefficient estimates and coefficient estimate ranges
#'    ('type = coef.range') for object of class `pdynmc`. The latter
#'    plot requires twostep or iterative GMM estimates.
#'
#' @author Markus Fritsch and Joachim Schnurbus
#' @export
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics boxplot
#' @importFrom graphics legend
#' @importFrom graphics lines
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'     w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
#'     opt.meth = "none")
#'  plot(m1)
#'  plot(m1, type = "coef.range")
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'     w.mat = "iid.err", std.err = "corrected", estimation = "iterative",
#'     opt.meth = "none")
#'  plot(m1)
#'  plot(m1, type = "coef.range")
#' }
#' }
#'
#'
plot.pdynmc		<- function(
  x,
  type = "fire",
  include.dum = FALSE,
  include.fur.con = FALSE,
  col.coefRange = 1,
  col.coefInitial = "darkgrey",
  col.coefEst = "royalblue",
  boxplot.coef = FALSE,
  ...
){

  if(type == "fire"){

    if(!inherits(x, what = "pdynmc")){
      stop("Use only with \"pdynmc\" objects.")
    }

    fitteds <- unlist(x$fitted.values)
    resids  <- unlist(x$residuals)

    y.range	<- c(-1, 1)*max(abs(resids))
    plot(x = fitteds, y = resids, ylim = y.range, xlab = "Fitted Values", ylab = "Residuals",
         main	= paste("Fitted Residual Plot of", substitute(x)), col = "grey60", ...)
    abline(h = 0)
  }


  if(type == "coef.range"){
    if(!inherits(x, what = "pdynmc")){
      stop("Use only with \"pdynmc\" objects.")
    }
    if(x$iter == 1){
      stop("Only onestep estimates available; plot requires twostep or iterated GMM results.")
    }
    if(boxplot.coef == TRUE && x$iter < 10){
      boxplot.coef <- FALSE
      warning("Argument 'boxplot.coef' was ignored as coefficient boxplots are only displayed for a minimum of 10 iterations.")
    }

    if(!include.dum | !include.fur.con){
      if(!include.dum && !include.fur.con){
        varnames.ind <- !(x$data$varnames.reg %in% x$data$varnames.dum) & !(x$data$varnames.reg %in% x$data$varnames.reg.fur)
      } else{
        if(!include.dum){
          varnames.ind <- !(x$data$varnames.reg %in% x$data$varnames.dum)
        } else{
          varnames.ind <- !(x$data$varnames.reg %in% x$data$varnames.reg.fur)
        }
      }
    } else{
      varnames.ind <- rep(TRUE, times = length(x$data$varnames.reg))
    }
    var.names <- x$data$varnames.reg[varnames.ind]

    n.iter    <- x$iter
    if(x$data$opt.method == "none"){
      coef.list <- lapply(x$par.clForm, FUN = '[', varnames.ind)
    } else{
      coef.list <- lapply(x$par.optim, FUN = '[', varnames.ind)
    }
    coef.est  <- x$coefficients[varnames.ind]
    n.coef    <- length(coef.est)

    coef.mat  <- do.call(what = cbind, coef.list)

    if(nrow(coef.mat) == 1){
      if(boxplot.coef){
        plot(x = rep(n.coef, times = 2), y = c(coef.mat.min, coef.mat.max), type = "n", xaxt = "n", xlab = "", ylab = "", ...)
        graphics::points(x = n.coef, y = coef.mat[,1], col = col.coefInitial, pch = 1, ...)
        graphics::boxplot(t(coef.mat), xaxt = "n", xlabel = "", ylabel = "", ...)

      } else{
        coef.mat.min <- min(coef.mat)
        coef.mat.max <- max(coef.mat)
        plot(x = rep(n.coef, times = 2), y = c(coef.mat.min, coef.mat.max), type = "n", xaxt = "n", xaxt = "n", xlab = "", ylab = "", ...)
        graphics::lines(x = rep(n.coef, times = 2), y = c(coef.mat.min, coef.mat.max), col = col.coefRange, lwd = 1, lty = 2, ...)
        graphics::points(x = n.coef, y = coef.mat[,1], col = col.coefInitial, pch = 1, ...)
        graphics::points(x = x.vec[i], y = coef.est[i], col = col.coefEst, pch = 18, ...)
        graphics::axis(side = 1, at = c(1:n.coef), labels = paste(var.names))
      }
    } else{
      if(boxplot.coef){
        graphics::boxplot(t(coef.mat), xaxt = "n", xlabel = "", ylabel = "", ...)
        for(i in 1:n.coef){
          graphics::points(x = x.vec[i], y = coef.mat[i,1], col = col.coefInitial, pch = 1, ...)
          graphics::lines(x = c(i-0.2, i+0.2), y = rep(coef.est[i], times = 2), col = col.coefEst, lwd = 2, ...)
        }
      } else{
        coef.mat.min.max <- cbind(apply(X = coef.mat, MARGIN = 1, FUN = min), apply(X = coef.mat, MARGIN = 1, FUN = max))
        x.vec        <- 1:n.coef
        plot(x = rep(x.vec, each = 2), y = t(coef.mat.min.max), type = "n", xlim = c(0.7, n.coef+0.3), xaxt = "n", xaxt = "n", xlab = "", ylab = "", ...)
        for(i in 1:n.coef){
          graphics::lines(x = rep(x.vec[i], times = 2), y = coef.mat.min.max[i,], col = col.coefRange, lwd = 1, lty = 2, ...)
          graphics::points(x = x.vec[i], y = coef.mat[i,1], col = col.coefInitial, pch = 1, ...)
          graphics::points(x = x.vec[i], y = coef.est[i], col = col.coefEst, pch = 18, ...)
        }
        graphics::axis(side = 1, at = c(1:n.coef), labels = paste(var.names))
      }
    }
    abline(h = 0)
    graphics::legend("bottomleft", col = c(col.coefEst, col.coefInitial, col.coefRange), lwd = c(NA,NA,1), pch = c(18,1,NA), lty = c(NA,NA,2), legend = c("coeff. est.", "coeff. initial", "coeff. range"), bty = "n")
  }

}





















#' Print Fitted Model Object.
#'
#' \code{print.pdynmc} prints objects of class `pdynmc`.
#'
#' @param x An object of class `pdynmc`.
#' @param digits An integer indicating the maximum number of digits to
#'    display in the object.
#' @param ... further arguments.
#'
#' @return Print objects of class `pdynmc`.
#'
#' @author Markus Fritsch
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
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'
print.pdynmc	<- function(x, digits = max(3, getOption("digits") - 3), ...){

  if(!inherits(x, what = "pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }

  cat("\nDynamic linear panel estimation", paste(" (", x$data$estimation, ")", sep = ""), sep = "")
  cat("\nMoment conditions: ", paste(if(x$data$diffMC){ "linear (DIF)" }, if(x$data$levMC){ " linear (LEV)" }, if(x$data$nlMC){ " nonlinear" }), sep = "")
  cat("\nEstimation steps: ", paste(x$iter), sep = "")
  cat("\n")
  cat("\nCoefficients:\n")
  print.default(format(coef(x), digits = digits), print.gap = 2L, quote = FALSE)
  cat("\n")
}
















#' Print Summary of Fitted Model Object.
#'
#' \code{print.summary.pdynmc} prints the summary for objects of class
#'    `pdynmc`.
#'
#' @param x An object of class `summary.pdynmc`.
#' @param digits An integer indicating the maximum number of digits to
#'    display in the object.
#' @param signif.stars Argument is defined as in \code{\link{options}}.
#' @param ... further arguments.
#'
#' @return Print information on objects of class `summary.pdynmc`.
#'
#' @author Markus Fritsch
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
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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

  if(!inherits(x, what = "summary.pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }

  #  cat(formula(paste(x$data$varname.y, paste(x$data$varnames.reg, collapse = "+"), sep = " ~ ")))
  #  cat("\n")
  cat("\nDynamic linear panel estimation", paste(" (", x$data$estimation, ")", sep = ""), sep = "")
  cat("\nMoment conditions: ", paste(if(x$data$diffMC){paste(x$data$n.inst["inst.diff"], " linear (DIF)", sep = "") }, if(x$data$levMC){paste(" ", x$data$n.inst["inst.lev"], " linear (LEV)", sep = "") }, if(x$data$nlMC){paste(" ", x$data$n.inst["inst.nl"], " nonlinear", sep = "") }), sep = "")
  cat("\nEstimation steps: ", paste(x$iter), "\n", sep = "")
  cat("\nCoefficients:\n")
  printCoefmat(coef(x), digits = digits, signif.stars = signif.stars, na.print = "NA", ...)
  #  cat(paste("Total Sum of Squares ", round(x$tss, digits = digits), "\n", sep = ""))
  #  cat(paste("Residual Sum of Squares ", round(x$rss, digits = digits), "\n", sep = ""))
  cat("\n", paste(sum(x$data$n.inst), " total instruments are employed to estimate ", length(x$data$varnames.reg), " parameters", sep = ""))
  cat("\n", paste(if(x$data$diffMC){paste(x$data$n.inst["inst.diff"], " linear (DIF) ", sep = "") }, if(x$data$levMC){paste(x$data$n.inst["inst.lev"], " linear (LEV) ", sep = "") }, if(x$data$nlMC){paste(x$data$n.inst["inst.nl"],  " nonlinear", sep = "") }, sep = ""))
  cat("\n", paste(if(x$data$varnames.fur.con[1] == "no further controls"){"no further controls "} else {paste(if("inst.furCon.diff" %in% names(x$data$n.inst)){paste(x$data$n.inst["inst.furCon.diff"], " further controls (DIF) ", sep = "")}, if("inst.furCon.lev" %in% names(x$data$n.inst)){paste(x$data$n.inst["inst.furCon.lev"], " further controls (LEV) ", sep = "")}, sep = "")}, sep = ""))
  cat("\n", paste(if(x$data$varnames.dum[1] == "no time dummies"){"no time dummies "} else {paste(if("dum.diff" %in% names(x$data$n.inst)){paste(x$data$n.inst["dum.diff"], " time dummies (DIF) ", sep = "")}, if("dum.lev" %in% names(x$data$n.inst)){paste(x$data$n.inst["dum.lev"], " time dummies (LEV) ", sep = "")}, sep = "")}, sep = ""))

  cat("\n", "\nJ-Test (overid restrictions): ", paste(round(x$hansenj$statistic, digits = 2), " with ", x$hansenj$parameter, " DF, pvalue: ", if(x$hansenj$p.value < 0.001){paste("<0.001")} else{round(x$hansenj$p.value, digits = digits)}, sep = ""))
  cat("\nF-Statistic (slope coeff): ", paste(round(x$slopef$statistic, digits = 2), " with ", x$slopef$parameter, " DF, pvalue: ", if(x$slopef$p.value < 0.001){paste("<0.001")} else{round(x$slopef$p.value, digits = digits)}, sep = ""))
  cat("\nF-Statistic (time dummies): ", if(length(x$time.dumf) == 1){ "no time dummies included in estimation" } else{ paste(round(x$time.dumf$statistic, digits = 2), " with ", x$time.dumf$parameter, " DF, pvalue: ", if(x$time.dumf$p.value < 0.001){paste("<0.001")} else{round(x$time.dumf$p.value, digits = digits)}, sep = "")} )
  cat("\n")
}
















#' Extract Residuals of Fitted Model.
#'
#' \code{residuals.pdynmc} extracts residuals from an object of class
#'    `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param step An integer denoting the iteration step for which fitted
#'    values are extracted (defaults to last iteration step used for
#'    obtaining parameter estimates).
#' @param na.rm A logical variable indicating whether missing values
#'    should be removed from the vector of fitted values (defaults to
#'    `FALSE`).
#' @param ... further arguments.
#'
#' @return Extract residuals from object of class `pdynmc`.
#'
#' @author Markus Fritsch
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
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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

  if(!inherits(object, what = "pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }

  if(na.rm == TRUE){
    res.pd	<- stats::na.omit(get(paste("step", step, sep = "") , object$residuals))
  } else{
    res.pd	<- get(paste("step", step, sep = "") , object$residuals)
  }
  return(res.pd)
}




















#' Summary for Fitted Model Object.
#'
#' \code{summary.pdynmc} generates the summary for objects of class
#'    `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param ... further arguments.
#'
#' @return Object of class `summary.pdynmc`.
#'
#' @author Markus Fritsch
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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

  if(!inherits(object, what = "pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }

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






















#' Extract Names of Explanatory Variables of Fitted Model.
#'
#' \code{variable.names.pdynmc} extracts explanatory variables
#'    from an object of class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param ... further arguments.
#'
#' @return Extract explanatory variables from an object of class
#'    `pdynmc`.
#'
#' @author Markus Fritsch
#' @export
#' @importFrom stats variable.names
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  variable.names(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  variable.names(m1)
#' }
#' }
#'
#'
variable.names.pdynmc		<- function(object, ...){

  if(!inherits(object, what = "pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }

  vnames	<- object$data$varnames.reg
  return(vnames)
}













#' Extract Variance Covariance Matrix of Fitted Model.
#'
#' \code{vcov.pdynmc} extracts variance covariance matrix of the
#'    paramter estimates from an object of class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param step An integer denoting the iteration step for which
#'    fitted values are extracted (defaults to last iteration step
#'    used for obtaining parameter estimates).
#' @param ... further arguments.
#'
#' @return Extract variance covariance matrix of the paramter estimates
#'    from an object of class `pdynmc`.
#'
#' @author Markus Fritsch
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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

  if(!inherits(object, what = "pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }

  vcov	<- get(paste("step", step, sep = "") , object$vcov)
  return(vcov)
}


















#' Extract Weighting Matrix of Fitted Model.
#'
#' \code{wmat} is a generic function for extracting the
#'    weighting matrix of an object.
#'
#' @param object An object for which the weighting matrix is
#'    desired.
#' @param ... further arguments.
#'
#' @return Extract weighting matrix from an object.
#'
#' @author Markus Fritsch
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  wmat(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  wmat(m1)
#' }
#' }
#'
#'
wmat <- function(object, ...){
  UseMethod("wmat", object)
}


#' Extract Weighting Matrix of Fitted Model.
#'
#' \code{wmat.pdynmc} extracts weighting matrix from an object of
#'    class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param step An integer denoting the iteration step for which
#'    fitted values are extracted (defaults to last iteration step
#'    used for obtaining parameter estimates).
#' @param ... further arguments.
#'
#' @return Extract weighting matrix from an object of class
#'    `pdynmc`.
#'
#' @author Markus Fritsch
#' @export
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#' @examples
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  wmat(m1)
#' }
#'
#' \donttest{
#' ## Load data from plm package
#' if(!requireNamespace("plm", quietly = TRUE)){
#'  stop("Dataset from package \"plm\" needed for this example.
#'  Please install the package.", call. = FALSE)
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
#'  wmat(m1)
#' }
#' }
#'
#'
wmat.pdynmc		<- function(object, step = object$iter, ...){

  if(!inherits(object, what = "pdynmc")){
    stop("Use only with \"pdynmc\" objects.")
  }

  wmat	<- get(paste("step", step, sep = "") , object$w.mat, ...)
  return(wmat)
}











