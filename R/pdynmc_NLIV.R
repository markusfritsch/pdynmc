
############################################################################################################
### Closed form estimation functions for AR(1) linear dynamic panel data models
############################################################################################################




#' Nonlinear Instrumental Variables Estimator - T-Version (NLIV).
#'
#' \code{NLIV} Computes closed form solution for lag parameter of linear
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
#'    The nonlinear IV estimator employs the original version of the nonlinear
#'    moment conditions of \insertCite{AhnSch1995;textual}{pdynmc}.
#'    More details on the implementation and the properties of the estimator
#'    are provided in \insertCite{FriPuaSch2024;textual}{pdynmc}.
#'
#' @param dat A dataset.
#' @param varname.i The name of the cross-section identifier.
#' @param varname.t The name of the time-series identifier.
#' @param varname.y A character string denoting the name of the dependent
#'    variable in the dataset.
#' @param trueAR A logical variable indicating whether the true autoregressive
#'    parameter is known (defaults to `NULL`). The parameter is used to
#'    compute the terms `A`, `B`, and `C` that can be employed to rewrite
#'    the estimating equation. For details, see
#'    \insertCite{FriPuaSch2024;textual}{pdynmc}.
#' @return An object of class `numeric` that contains the coefficient estimate for
#'    the lag parameter according to the two roots of the quadratic equation.
#'
#' @author Joachim Schnurbus, Markus Fritsch
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
#' m1 <- NLIV(dat = dat, varname.i = "state", varname.t = "year", varname.y = "packpc")
#'
#'
NLIV	<- function(
  dat,
  varname.i,
  varname.t,
  varname.y,
  trueAR = NULL
){


  if(!data.table::is.data.table(dat)){data.table::setDT(x = dat)}

  data.table::setorderv(dat, cols = c(varname.i, varname.t))

  dat$i.label        <- as.character(dat[[varname.i]])
  dat[[varname.i]]   <- as.numeric(as.factor(dat[[varname.i]]))

  dat$t.label        <- as.character(dat[[varname.t]])
  dat[[varname.t]]   <- as.numeric(as.factor(dat[[varname.t]]))

  i_cases			<- sort(unique(dat[[varname.i]]))
  i_temp			<- 1:length(i_cases)				      # reflects data structures where i does not start at i = 1
  t_cases			<- sort(unique(dat[[varname.t]]))
  t_temp			<- 1:length(unique(t_cases))			# reflects data structures where t does not start at t = 1

  n				    <- length(i_temp)		# number of cross-section units
  Time				<- length(t_temp)		# number of time-series units

  if(Time < 4){
    stop(paste("At least 4 time periods required to compute estimator. Dataset only has ", Time, " time periods.", sep = ""))
  }

  dat_b			  <- as.data.frame(array(data = NA, dim = c(length(i_cases)*length(t_cases), 2),
                                   dimnames = list(NULL, c(varname.i, varname.t))))
  dat_b[, varname.i]	<- rep(x = i_cases, each = length(t_cases))
  dat_b[, varname.t]	<- rep(x = t_cases, times = length(i_cases))

  dat_b			  <- data.table::setDT(x = dat_b)
  data.table::setorderv(dat_b, cols = c(varname.i, varname.t))

  dat			    <- merge(x = dat_b, y = dat, by = c(varname.i, varname.t), all.x = TRUE)
  dat			    <- dat[order(dat[[varname.i]], dat[[varname.t]], decreasing = FALSE), ]

  dat.na			<- dat

  dat.tmp   <- lapply(i_cases, FUN = abcIV.compute_T, Time = Time, dat.na = dat.na, varname.i = varname.i, varname = varname.y)

  A	<- Reduce("+", lapply(dat.tmp, "[[", 1))
  B	<- -Reduce("+", lapply(dat.tmp, "[[", 2))
  C	<- Reduce("+", lapply(dat.tmp, "[[", 3))

  rho.sqrtterm	<- (((-1)*B/(2*A))^2 - C/A)

  if(rho.sqrtterm < 0){rho.sqrtterm		<- abs(rho.sqrtterm)}

  rho.sqrt		<- sqrt(rho.sqrtterm)

  rho.hat.1		<- -B/(2*A) + rho.sqrt
  rho.hat.2		<- -B/(2*A) - rho.sqrt


  if(!is.null(trueAR)){
    papB	<- -sum(unlist( y.Tm1*((y.Tm1 - y.2) - trueAR*(y.Tm2 - y.1)) + (y.T - trueAR*y.Tm1)*(y.Tm2 - y.1) ))
    papC	<- sum(unlist( (y.T - trueAR*y.Tm1)*((y.Tm1 - y.2) - trueAR*(y.Tm2 - y.1)) ))
    to.return		<- c(A = A, B = B, C = C, rho.hat.pos = rho.hat.1, rho.hat.neg = rho.hat.2, papB = papB, papC = papC)
  } else {
    to.return		<- c(A = A, B = B, C = C, rho.hat.pos = rho.hat.1, rho.hat.neg = rho.hat.2)
  }

  return(to.return)


}








#' Nonlinear Instrumental Variables Estimator - t-Version (NLIV_t).
#'
#' \code{NLIV_t} Computes closed form solution for lag parameter of linear
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
#'    The nonlinear IV estimator employs an alternative formulation of the
#'    nonlinear moment conditions of \insertCite{AhnSch1995;textual}{pdynmc}.
#'    More details on the implementation and the properties of the estimator
#'    are provided in \insertCite{FriPuaSch2024;textual}{pdynmc}.
#'
#' @param dat A dataset.
#' @param varname.i The name of the cross-section identifier.
#' @param varname.t The name of the time-series identifier.
#' @param varname.y A character string denoting the name of the dependent
#'    variable in the dataset.
#' @param trueAR A logical variable indicating whether the true autoregressive
#'    parameter is known (defaults to `NULL`). The parameter is used to
#'    compute the terms `A`, `B`, and `C` that can be employed to rewrite
#'    the estimating equation. For details, see
#'    \insertCite{FriPuaSch2024;textual}{pdynmc}.
#' @return An object of class `numeric` that contains the coefficient estimate for
#'    the lag parameter according to the two roots of the quadratic equation.
#'
#' @author Joachim Schnurbus, Markus Fritsch
#' @export
#' @importFrom data.table is.data.table
#' @importFrom data.table setDT
#' @importFrom data.table setorderv
#' @importFrom data.table shift
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
#' m1 <- NLIV_t(dat = dat, varname.i = "state", varname.t = "year", varname.y = "packpc")
#'
#'
NLIV_t	<- function(
  dat,
  varname.i,
  varname.t,
  varname.y,
  trueAR = NULL
){


  if(!data.table::is.data.table(dat)){data.table::setDT(x = dat)}

  data.table::setorderv(dat, cols = c(varname.i, varname.t))

  dat$i.label        <- as.character(dat[[varname.i]])
  dat[[varname.i]]   <- as.numeric(as.factor(dat[[varname.i]]))

  dat$t.label        <- as.character(dat[[varname.t]])
  dat[[varname.t]]   <- as.numeric(as.factor(dat[[varname.t]]))

  i_cases			<- sort(unique(dat[[varname.i]]))
  i_temp			<- 1:length(i_cases)				      # reflects data structures where i does not start at i = 1
  t_cases			<- sort(unique(dat[[varname.t]]))
  t_temp			<- 1:length(unique(t_cases))			# reflects data structures where t does not start at t = 1

  n				    <- length(i_temp)		# number of cross-section units
  Time				<- length(t_temp)		# number of time-series units

  if(Time < 4){
    stop(paste("At least 4 time periods required to compute estimator. Dataset only has ", Time, " time periods.", sep = ""))
  }

  dat_b			  <- as.data.frame(array(data = NA, dim = c(length(i_cases)*length(t_cases), 2),
                                   dimnames = list(NULL, c(varname.i, varname.t))))
  dat_b[, varname.i]	<- rep(x = i_cases, each = length(t_cases))
  dat_b[, varname.t]	<- rep(x = t_cases, times = length(i_cases))

  dat_b			<- data.table::setDT(x = dat_b)
  data.table::setorderv(dat_b, cols = c(varname.i, varname.t))

  dat			  <- merge(x = dat_b, y = dat, by = c(varname.i, varname.t), all.x = TRUE)
  dat			  <- dat[order(dat[[varname.i]], dat[[varname.t]], decreasing = FALSE), ]

  dat.na		<- dat

  dat.tmp   <- lapply(i_cases, FUN = abcIV.compute_t, Time = Time, dat.na = dat.na, varname.i = varname.i, varname = varname.y)

  dat.tmp2	<- lapply(dat.tmp, FUN = colSums)

  A		<- Reduce("+", lapply(dat.tmp2, "[[", 1))
  B		<- -Reduce("+", lapply(dat.tmp2, "[[", 2))
  C		<- Reduce("+", lapply(dat.tmp2, "[[", 3))

  rho.sqrtterm	<- (((-1)*B/(2*A))^2 - C/A)

  if(rho.sqrtterm < 0){rho.sqrtterm   <- abs(rho.sqrtterm)}

  rho.sqrt		<- sqrt(rho.sqrtterm)

  rho.hat.1		<- -B/(2*A) + rho.sqrt
  rho.hat.2		<- -B/(2*A) - rho.sqrt


  if(!is.null(trueAR)){
    papB	<- -sum( dat$y.lag1*((dat$y.lag1 - dat$y.lag2) - trueAR*(dat$y.lag2 - dat$y.lag3)) + (dat$y - trueAR*dat$y.lag1)*(dat$y.lag2 - dat$y.lag3))
    papC	<- sum( (dat$y - trueAR*dat$y.lag1)*((dat$y.lag1 - dat$y.lag2) - trueAR*(dat$y.lag2 - dat$y.lag3)))
    to.return		<- c(A = A, B = B, C = C, rho.hat.pos = rho.hat.1, rho.hat.neg = rho.hat.2, papB = papB, papC = papC)
  } else {
    to.return		<- c(A = A, B = B, C = C, rho.hat.pos = rho.hat.1, rho.hat.neg = rho.hat.2)
  }

  return(to.return)


}





###
###	Estimator of Han & Phillips (2010, ET)
###


#' First Difference Least Squares (FDLS) Estimator of Han and Phillips (2010).
#'
#' \code{FDLS} computes closed form estimator for lag parameter of linear
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
#'    More details on the FDLS estimator and its properties are provided
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
#' @author Joachim Schnurbus, Markus Fritsch
#' @export
#' @importFrom data.table is.data.table
#' @importFrom data.table setDT
#' @importFrom data.table setorderv
#' @importFrom data.table shift
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
#' m1 <- FDLS(dat = dat, varname.i = "state", varname.t = "year", varname.y = "packpc")
#'
#'
FDLS	<- function(
  dat,
  varname.i,
  varname.t,
  varname.y
){


  if(!data.table::is.data.table(dat)){data.table::setDT(x = dat)}

  data.table::setorderv(dat, cols = c(varname.i, varname.t))

  dat$i.label        <- as.character(dat[[varname.i]])
  dat[[varname.i]]   <- as.numeric(as.factor(dat[[varname.i]]))

  dat$t.label        <- as.character(dat[[varname.t]])
  dat[[varname.t]]   <- as.numeric(as.factor(dat[[varname.t]]))

  i_cases			<- sort(unique(dat[[varname.i]]))
  i_temp			<- 1:length(i_cases)				      # reflects data structures where i does not start at i = 1
  t_cases			<- sort(unique(dat[[varname.t]]))
  t_temp			<- 1:length(unique(t_cases))			# reflects data structures where t does not start at t = 1

  n				    <- length(i_temp)		# number of cross-section units
  Time				<- length(t_temp)		# number of time-series units

  if(Time < 3){
    stop(paste("At least 3 time periods required to compute estimator. Dataset only has ", Time, " time periods.", sep = ""))
  }

  dat_b			  <- as.data.frame(array(data = NA, dim = c(length(i_cases)*length(t_cases), 2),
                                   dimnames = list(NULL, c(varname.i, varname.t))))
  dat_b[, varname.i]	<- rep(x = i_cases, each = length(t_cases))
  dat_b[, varname.t]	<- rep(x = t_cases, times = length(i_cases))

  dat_b			  <- data.table::setDT(x = dat_b)
  data.table::setorderv(dat_b, cols = c(varname.i, varname.t))

  dat			    <- merge(x = dat_b, y = dat, by = c(varname.i, varname.t), all.x = TRUE)
  dat			    <- dat[order(dat[[varname.i]], dat[[varname.t]], decreasing = FALSE), ]

  dat.na			<- dat

  dat.tmp   <- lapply(i_cases, FUN = lagsDiffFDLS.compute, dat.na = dat.na, varname.i = varname.i, varname = varname.y)

  fdls_num   <- Reduce("+", lapply(dat.tmp, "[[", 1))
  fdls_denom   <- Reduce("+", lapply(dat.tmp, "[[", 2))

  rho.hat	<- fdls_num/fdls_denom

  return(rho.hat)


}





###
###	Estimator of Anderson & Hsiao (1981, JASA)
###


#' Estimator of Anderson and Hsiao (1981).
#'
#' \code{AH81} computes closed form estimator for lag parameter of linear
#'    dynamic panel data model based on Anderson & Hsiao (1981) (AH81)
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
#'    More details on the AH81 estimator and its properties are provided
#'    in \insertCite{AndHsi1981;textual}{pdynmc} and
#'    \insertCite{AndHsi1982;textual}{pdynmc}.
#'
#' @param dat A dataset.
#' @param varname.i The name of the cross-section identifier.
#' @param varname.t The name of the time-series identifier.
#' @param varname.y A character string denoting the name of the dependent variable
#'    in the dataset.
#' @param eq8.2 A logical variable indicating whether the estimation function is
#'    based on Equation (8.2) of Anderson and Hsiao (1981);
#'    otherwise Equation (8.1) is employed (defaults to `TRUE`).
#' @return An object of class `numeric` that contains the coefficient estimate for
#'    the lag parameter according to the two roots of the quadratic equation.
#'
#' @author Joachim Schnurbus, Markus Fritsch
#' @export
#' @importFrom data.table is.data.table
#' @importFrom data.table setDT
#' @importFrom data.table setorderv
#' @importFrom data.table shift
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
#' m1 <- AH81(dat = dat, varname.i = "state", varname.t = "year", varname.y = "packpc")
#'
#'
AH81	<- function (
    dat,
    varname.i,
    varname.t,
    varname.y,
    eq8.2	= TRUE	# this uses the estimator of AH1981, equation (8.2), if FALSE, the estimator of equation (8.1) is computed.
){


  if(!data.table::is.data.table(dat)){data.table::setDT(x = dat)}

  data.table::setorderv(dat, cols = c(varname.i, varname.t))

  dat$i.label        <- as.character(dat[[varname.i]])
  dat[[varname.i]]   <- as.numeric(as.factor(dat[[varname.i]]))

  dat$t.label        <- as.character(dat[[varname.t]])
  dat[[varname.t]]   <- as.numeric(as.factor(dat[[varname.t]]))

  i_cases			<- sort(unique(dat[[varname.i]]))
  i_temp			<- 1:length(i_cases)				      # reflects data structures where i does not start at i = 1
  t_cases			<- sort(unique(dat[[varname.t]]))
  t_temp			<- 1:length(unique(t_cases))			# reflects data structures where t does not start at t = 1

  n				    <- length(i_temp)		# number of cross-section units
  Time				<- length(t_temp)		# number of time-series units

  if(Time < 4){
    stop(paste("At least 4 time periods required to compute estimator. Dataset only has ", Time, " time periods.", sep = ""))
  }

  dat_b			  <- as.data.frame(array(data = NA, dim = c(length(i_cases)*length(t_cases), 2),
                                   dimnames = list(NULL, c(varname.i, varname.t))))
  dat_b[, varname.i]	<- rep(x = i_cases, each = length(t_cases))
  dat_b[, varname.t]	<- rep(x = t_cases, times = length(i_cases))

  dat_b			  <- data.table::setDT(x = dat_b)
  data.table::setorderv(dat_b, cols = c(varname.i, varname.t))

  dat			    <- merge(x = dat_b, y = dat, by = c(varname.i, varname.t), all.x = TRUE)
  dat			    <- dat[order(dat[[varname.i]], dat[[varname.t]], decreasing = FALSE), ]

  dat.na			        <- dat

  dat.tmp   <- lapply(i_cases, FUN = lagsDiffAH81.compute, dat.na = dat.na, varname.i = varname.i, varname = varname.y, eq8.2 = eq8.2)

  ah81_num   <- Reduce("+", lapply(dat.tmp, "[[", 1))
  ah81_denom   <- Reduce("+", lapply(dat.tmp, "[[", 2))

  rho.hat	<- ah81_num/ah81_denom

  return(rho.hat)


}



