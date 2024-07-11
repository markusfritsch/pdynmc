
############################################################################################################
### Closed form estimation functions for linear dynamic panel data models
############################################################################################################




#' Nonlinear instrumental variables estimator - T-version (NLIV.T).
#'
#' \code{NLIV.T} Computes closed form solution for lag parameter of linear
#'    dynamic panel data model based on instrumental variables (IV) estimator
#'    employing nonlinear moment conditions.
#'
#' The function estimates a linear dynamic panel data model of the form
#'    \deqn{y_{i,t} = y_{i,t-1} \rho_1 + a_i + \varepsilon_{i,t}}
#'    where \eqn{y_{i,t-1}} is the lagged dependent variable, \eqn{\rho_1} is
#'    the lag parameter, \eqn{a_i} is an unobserved individual specific effect,
#'    and \eqn{\varepsilon_{i,t}} is an idiosyncratic remainder component. The
#'    model structure accounts for unobserved individual specific heterogeneity
#'    and dynamics. Note that more general lag structures and further covariates
#'    are beyond the scope of the current implementation in \code{pdynmc}.
#'
#' The nonlinear IV estimator employs the original version of the nonlinear
#'    moment conditions of \insertCite{AhnSch1995;textual}{pdynmc}.
#'    More details on the implementation and the properties of the estimator
#'    are provided in \insertCite{FriPuaSch2024;textual}{pdynmc}.
#'
#'
#' @param dat A dataset.
#' @param varname.i The name of the cross-section identifier.
#' @param varname.t The name of the time-series identifier.
#' @param varname.y A character string denoting the name of the dependent
#'    variable in the dataset.
#' @return An object of class `numeric` that contains the coefficient estimate for
#'    the lag parameter according to the two roots of the quadratic equation.
#'
#' @export
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table setDT
#' @importFrom data.table setorderv
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' ## Load data
#' data(cigDemand, package = "pdynmc")
#' dat <- cigDemand
#'
#' ## Code example
#' m1 <- NLIV.T(dat = dat, varname.i = "state", varname.t = "year", varname.y = "packpc")
#'
#'
NLIV.T	<- function(
  dat,
  varname.i,
  varname.t,
  varname.y
){

  tPer			<- unique(as.numeric(dat[[varname.t]]))
  dat.tmp			<- data.table::data.table(cbind(dat[[varname.i]],
                                rep(1:length(tPer), times = length(unique(dat[[varname.i]]))),
                                dat[[varname.y]]))
  colnames(dat.tmp)	<- c(varname.i, varname.t, varname.y)
  dat.tmp[[varname.t]]	<- as.numeric(dat.tmp[[varname.t]])
  dat.tmp[[varname.y]]	<- as.numeric(dat.tmp[[varname.y]])

  T				<- max(dat.tmp[[varname.t]])

  if(!data.table::is.data.table(dat.tmp)){data.table::setDT(x = dat.tmp)}

  data.table::setorderv(dat.tmp, cols = c(varname.i, varname.t))

  y.T			<- dat.tmp[i = dat.tmp[[varname.t]] == T, j = get(varname.y)]
  y.Tm1		<- dat.tmp[i = dat.tmp[[varname.t]] == T - 1, j = get(varname.y)]
  y.Tm2		<- dat.tmp[i = dat.tmp[[varname.t]] == T - 2, j = get(varname.y)]
  y.2			<- dat.tmp[i = dat.tmp[[varname.t]] == 2, j = get(varname.y)]
  y.1			<- dat.tmp[i = dat.tmp[[varname.t]] == 1, j = get(varname.y)]

  A	<- sum(unlist(y.Tm1*(y.Tm2 - y.1)))
  B	<- -sum(unlist( y.Tm1*(y.Tm1 - y.2) + y.T*(y.Tm2 - y.1) ))
  C	<- sum(unlist( y.T*(y.Tm1 - y.2) ))

  rho.sqrtterm	<- ((-B)^2 - 4*A*C)

  if(rho.sqrtterm < 0){rho.sqrtterm <- 0}

  rho.sqrt		<- sqrt(rho.sqrtterm)

  rho.hat.1		<- -B/(2*A) + rho.sqrt/(2*A)
  rho.hat.2		<- -B/(2*A) - rho.sqrt/(2*A)

  to.return		<- c(A = A, B = B, C = C, rho.hat.pos = rho.hat.1, rho.hat.neg = rho.hat.2)

  return(to.return)
}








#' Nonlinear instrumental variables estimator - t-version (NLIV.t).
#'
#' \code{NLIV.t} Computes closed form solution for lag parameter of linear
#'    dynamic panel data model based on instrumental variables (IV) estimator
#'    employing alternative formulation of nonlinear moment conditions.
#'
#' The function estimates a linear dynamic panel data model of the form
#'    \deqn{y_{i,t} = y_{i,t-1} \rho_1 + a_i + \varepsilon_{i,t}}
#'    where \eqn{y_{i,t-1}} is the lagged dependent variable, \eqn{\rho_1} is
#'    the lag parameter, \eqn{a_i} is an unobserved individual specific effect,
#'    and \eqn{\varepsilon_{i,t}} is an idiosyncratic remainder component. The
#'    model structure accounts for unobserved individual specific heterogeneity
#'    and dynamics. Note that more general lag structures and further covariates
#'    are beyond the scope of the current implementation in \code{pdynmc}.
#'
#' The nonlinear IV estimator employs an alternative formulation of the
#'    nonlinear moment conditions of \insertCite{AhnSch1995;textual}{pdynmc}.
#'    More details on the implementation and the properties of the estimator
#'    are provided in \insertCite{FriPuaSch2024;textual}{pdynmc}.
#'
#' @param dat A dataset.
#' @param varname.i The name of the cross-section identifier.
#' @param varname.t The name of the time-series identifier.
#' @param varname.y A character string denoting the name of the dependent
#'    variable in the dataset.
#' @return An object of class `numeric` that contains the coefficient estimate for
#'    the lag parameter according to the two roots of the quadratic equation.
#'
#' @export
#' @importFrom data.table is.data.table
#' @importFrom data.table setDT
#' @importFrom data.table setorderv
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' ## Load data
#' data(cigDemand, package = "pdynmc")
#' dat <- cigDemand
#'
#' ## Code example
#' m1 <- NLIV.t(dat = dat, varname.i = "state", varname.t = "year", varname.y = "packpc")
#'
#'
NLIV.t	<- function(
  dat,
  varname.i,
  varname.t,
  varname.y
){

  if(!data.table::is.data.table(dat)){data.table::setDT(x = dat)}

  data.table::setorderv(dat, cols = c(varname.i, varname.t))

  dat[, "y"]		<- dat[[varname.y]]
  dat[, "y.lag1"]	<- shift(x = dat[[varname.y]], n = 1L, type = "lag")
  dat[, "y.lag2"]	<- shift(x = dat[[varname.y]], n = 2L, type = "lag")
  dat[, "y.lag3"]	<- shift(x = dat[[varname.y]], n = 3L, type = "lag")

  dat[i = dat[[varname.t]] == 1, j = c("y.lag1", "y.lag2", "y.lag3")]	<- NA
  dat[i = dat[[varname.t]] == 2, j = c("y.lag2", "y.lag3")]	<- NA
  dat[i = dat[[varname.t]] == 3, j = "y.lag3"]	<- NA


  A	<- sum(dat$y.lag1*(dat$y.lag2 - dat$y.lag3), na.rm = TRUE)
  B	<- -sum(dat$y.lag1*(dat$y.lag1 - dat$y.lag2) + dat$y*(dat$y.lag2 - dat$y.lag3), na.rm = TRUE)
  C	<- sum((dat$y*(dat$y.lag1 - dat$y.lag2))[dat$t != 3], na.rm = TRUE)

  rho.sqrtterm	<- ((-B)^2 - 4*A*C)

  if(rho.sqrtterm < 0){rho.sqrtterm <- 0}

  rho.sqrt		<- sqrt(rho.sqrtterm)

  rho.hat.1		<- -B/(2*A) + rho.sqrt/(2*A)
  rho.hat.2		<- -B/(2*A) - rho.sqrt/(2*A)

  to.return		<- c(A = A, B = B, C = C, rho.hat.pos = rho.hat.1, rho.hat.neg = rho.hat.2)

  return(to.return)
}





###
###	Estimator of Han & Phillips (2010, ET)
###


#' First difference least squares (FDLS) estimator of Han and Phillips (2010).
#'
#' \code{HP2010} computes closed form estimator for lag parameter of linear
#'    dynamic panel data model based on first difference least squares (FDLS)
#'    estimator.
#'
#' The function estimates a linear dynamic panel data model of the form
#'    \deqn{y_{i,t} = y_{i,t-1} \rho_1 + a_i + \varepsilon_{i,t}}
#'    where \eqn{y_{i,t-1}} is the lagged dependent variable, \eqn{\rho_1} is
#'    the lag parameter, \eqn{a_i} is an unobserved individual specific effect,
#'    and \eqn{\varepsilon_{i,t}} is an idiosyncratic remainder component. The
#'    model structure accounts for unobserved individual specific heterogeneity
#'    and dynamics. Note that more general lag structures and further covariates
#'    are beyond the scope of the current implementation in \code{pdynmc}.
#'
#' More details on the FDLS estimator and its properties are provided
#'    in \insertCite{HanPhi2010;textual}{pdynmc}.
#'
#' @param dat A dataset.
#' @param varname.i The name of the cross-section identifier.
#' @param varname.t The name of the time-series identifier.
#' @param varname.y A character string denoting the name of the dependent variable
#'    in the dataset.
#' @return An object of class `numeric` that contains the coefficient estimate for
#'    the lag parameter according to the two roots of the quadratic equation.
#'
#' @export
#' @importFrom data.table is.data.table
#' @importFrom data.table setDT
#' @importFrom data.table setorderv
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' ## Load data
#' data(cigDemand, package = "pdynmc")
#' dat <- cigDemand
#'
#' ## Code example
#' m1 <- HP2010(dat = dat, varname.i = "state", varname.t = "year", varname.y = "packpc")
#'
#'
HP2010	<- function(
  dat,
  varname.i,
  varname.t,
  varname.y
){

  if(!data.table::is.data.table(dat)){data.table::setDT(x = dat)}

  data.table::setorderv(dat, cols = c(varname.i, varname.t))

  dat[, "y"]		<- dat[[varname.y]]
  dat[, "y.lag1"]	<- shift(x = dat[[varname.y]], n = 1L, type = "lag")
  dat[, "y.lag2"]	<- shift(x = dat[[varname.y]], n = 2L, type = "lag")

  dat[i = dat[[varname.t]] == 1, j = c("y.lag1", "y.lag2")]	<- NA
  dat[i = dat[[varname.t]] == 2, j = "y.lag2"]	<- NA

  dat[, "Dy"]		<- dat$y - dat$y.lag1
  dat[, "Dy.lag1"]	<- dat$y.lag1 - dat$y.lag2

  rho.hat	<- sum(dat$Dy.lag1*(2*dat$Dy + dat$Dy.lag1), na.rm = TRUE) / sum(dat$Dy.lag1^2, na.rm = TRUE)

  return(rho.hat)
}









