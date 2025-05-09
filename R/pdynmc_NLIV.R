
############################################################################################################
### Closed form estimation functions for AR(1) linear dynamic panel data models
############################################################################################################




#' Nonlinear Instrumental Variables Estimator - T-Version (NLIV.T).
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
#'    The nonlinear IV estimator employs the original version of the nonlinear
#'    moment conditions of \insertCite{AhnSch1995;textual}{pdynmc}.
#'    More details on the implementation and the properties of the estimator
#'    are provided in \insertCite{FriPuaSch2024;textual}{pdynmc}.
#'
#' @param data A dataset.
#' @param varname.i The name of the cross-section identifier.
#' @param varname.t The name of the time-series identifier.
#' @param varname.y A character string denoting the name of the dependent
#'    variable in the dataset.
#' @param trueAR A logical variable indicating whether the true autoregressive
#'    parameter is known (defaults to `NULL`). The parameter is used to
#'    compute the terms `A`, `B`, and `C` that can be employed to rewrite
#'    the estimating equation (for details, see
#'    \insertCitep{FriPuaSch2024;textual}{pdynmc}).
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
#' m1 <- NLIV.T(dat = dat, varname.i = "state", varname.t = "year", varname.y = "packpc")
#'
#'
NLIV.T	<- function(
  data,
  varname.i,
  varname.t,
  varname.y,
  trueAR = NULL
){


  if(!data.table::is.data.table(data)){data.table::setDT(x = data)}

  data.table::setorderv(data, cols = c(varname.i, varname.t))

  data$i.label        <- as.character(data[[varname.i]])
  data[[varname.i]]   <- as.numeric(as.factor(data[[varname.i]]))

  data$t.label        <- as.character(data[[varname.t]])
  data[[varname.t]]   <- as.numeric(as.factor(data[[varname.t]]))

  i_cases			<- sort(unique(data[[varname.i]]))
  i_temp			<- 1:length(i_cases)				      # reflects data structures where i does not start at i = 1
  t_cases			<- sort(unique(data[[varname.t]]))
  t_temp			<- 1:length(unique(t_cases))			# reflects data structures where t does not start at t = 1

  data_b			<- as.data.frame(array(data = NA, dim = c(length(i_cases)*length(t_cases), 2),
                                  dimnames = list(NULL, c(varname.i, varname.t))))
  data_b[, varname.i]	<- rep(x = i_cases, each = length(t_cases))
  data_b[, varname.t]	<- rep(x = t_cases, times = length(i_cases))

  data_b			<- data.table::setDT(x = data_b)
  data.table::setorderv(data_b, cols = c(varname.i, varname.t))

  data			<- merge(x = data_b, y = data, by = c(varname.i, varname.t), all.x = TRUE)
  data			<- data[order(data[[varname.i]], data[[varname.t]], decreasing = FALSE), ]

  data.na			<- data
  data[is.na(data.na)]	<- 0

  data[, "y"]	<- data[[varname.y]]
  y.T			<- data[i = data[[varname.t]] == T, j = y]
  y.Tm1		<- data[i = data[[varname.t]] == sort(unique(data[[varname.t]]))[length(unique(as.numeric(data[[varname.t]]))) - 1], j = y]
  y.Tm2		<- data[i = data[[varname.t]] == sort(unique(data[[varname.t]]))[length(unique(as.numeric(data[[varname.t]]))) - 2], j = y]
  y.2			<- data[i = data[[varname.t]] == sort(unique(data[[varname.t]]))[2], j = y]
  y.1			<- data[i = data[[varname.t]] == sort(unique(data[[varname.t]]))[1], j = y]

  A	<- sum(unlist(y.Tm1*(y.Tm2 - y.1)))
  B	<- -sum(unlist( y.Tm1*(y.Tm1 - y.2) + y.T*(y.Tm2 - y.1) ))
  C	<- sum(unlist( y.T*(y.Tm1 - y.2) ))

  rho.sqrtterm	<- ((-B)^2 - 4*A*C)

  if(rho.sqrtterm < 0){rho.sqrtterm <- 0}

  rho.sqrt		<- sqrt(rho.sqrtterm)

  rho.hat.1		<- -B/(2*A) + rho.sqrt/(2*A)
  rho.hat.2		<- -B/(2*A) - rho.sqrt/(2*A)


  if(!is.null(trueAR)){
    papB	<- -sum(unlist( y.Tm1*((y.Tm1 - y.2) - trueAR*(y.Tm2 - y.1)) + (y.T - trueAR*y.Tm1)*(y.Tm2 - y.1) ))
    papC	<- sum(unlist( (y.T - trueAR*y.Tm1)*((y.Tm1 - y.2) - trueAR*(y.Tm2 - y.1)) ))
    to.return		<- c(A = A, B = B, C = C, rho.hat.pos = rho.hat.1, rho.hat.neg = rho.hat.2, papB = papB, papC = papC)
  } else {
    to.return		<- c(A = A, B = B, C = C, rho.hat.pos = rho.hat.1, rho.hat.neg = rho.hat.2)
  }

  return(to.return)


}








#' Nonlinear Instrumental Variables Estimator - t-Version (NLIV.t).
#'
#' \code{NLIV.alt} Computes closed form solution for lag parameter of linear
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
#' @param data A dataset.
#' @param varname.i The name of the cross-section identifier.
#' @param varname.t The name of the time-series identifier.
#' @param varname.y A character string denoting the name of the dependent
#'    variable in the dataset.
#' @param trueAR A logical variable indicating whether the true autoregressive
#'    parameter is known (defaults to `NULL`). The parameter is used to
#'    compute the terms `A`, `B`, and `C` that can be employed to rewrite
#'    the estimating equation (for details, see
#'    \insertCitep{FriPuaSch2024;textual}{pdynmc}).
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
#' m1 <- NLIV.t(dat = dat, varname.i = "state", varname.t = "year", varname.y = "packpc")
#'
#'
NLIV.t	<- function(
  data,
  varname.i,
  varname.t,
  varname.y,
  trueAR = NULL
){

  if(!data.table::is.data.table(data)){data.table::setDT(x = data)}

  data.table::setorderv(data, cols = c(varname.i, varname.t))

  data[, "y"]		<- data[[varname.y]]
  data[, "y.lag1"]	<- data.table::shift(x = data[[varname.y]], n = 1L, type = "lag")
  data[, "y.lag2"]	<- data.table::shift(x = data[[varname.y]], n = 2L, type = "lag")
  data[, "y.lag3"]	<- data.table::shift(x = data[[varname.y]], n = 3L, type = "lag")

  data[i = data[[varname.t]] == 1, j = c("y.lag1", "y.lag2", "y.lag3")]	<- NA
  data[i = data[[varname.t]] == 2, j = c("y.lag2", "y.lag3")]	<- NA
  data[i = data[[varname.t]] == 3, j = "y.lag3"]	<- NA


  A	<- sum(data$y.lag1*(data$y.lag2 - data$y.lag3), na.rm = TRUE)
  B	<- -sum(data$y.lag1*(data$y.lag1 - data$y.lag2) + data$y*(data$y.lag2 - data$y.lag3), na.rm = TRUE)
  C	<- sum((data$y*(data$y.lag1 - data$y.lag2))[dat$t != 3], na.rm = TRUE)

  rho.sqrtterm	<- ((-B)^2 - 4*A*C)

  if(rho.sqrtterm < 0){rho.sqrtterm <- 0}

  rho.sqrt		<- sqrt(rho.sqrtterm)

  rho.hat.1		<- -B/(2*A) + rho.sqrt/(2*A)
  rho.hat.2		<- -B/(2*A) - rho.sqrt/(2*A)


  if(!is.null(trueAR)){
    papB	<- -sum( data$y.lag1*((data$y.lag1 - data$y.lag2) - trueAR*(data$y.lag2 - data$y.lag3)) + (data$y - trueAR*data$y.lag1)*(data$y.lag2 - data$y.lag3) , na.rm = TRUE)
    papC	<- sum( (data$y - trueAR*data$y.lag1)*((data$y.lag1 - data$y.lag2) - trueAR*(data$y.lag2 - data$y.lag3)) , na.rm = TRUE)
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
#' @param data A dataset.
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
  data,
  varname.i,
  varname.t,
  varname.y
){

  if(!data.table::is.data.table(data)){data.table::setDT(x = data)}

  data.table::setorderv(data, cols = c(varname.i, varname.t))

  data[, "y"]		<- data[[varname.y]]
  data[, "y.lag1"]	<- data.table::shift(x = data[[varname.y]], n = 1L, type = "lag")
  data[, "y.lag2"]	<- data.table::shift(x = data[[varname.y]], n = 2L, type = "lag")

  data[i = data[[varname.t]] == 1, j = c("y.lag1", "y.lag2")]	<- NA
  data[i = data[[varname.t]] == 2, j = "y.lag2"]	<- NA

  data[, "Dy"]		<- data$y - data$y.lag1
  data[, "Dy.lag1"]	<- data$y.lag1 - data$y.lag2

  rho.hat	<- sum(data$Dy.lag1*(2*data$Dy + data$Dy.lag1), na.rm = TRUE) / sum(data$Dy.lag1^2, na.rm = TRUE)

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
#' @param data A dataset.
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
    data,
    varname.i,
    varname.t,
    varname.y,
    eq8.2	= TRUE	# this uses the estimator of AH1981, equation (8.2), if FALSE, the estimator of equation (8.1) is computed.
){

  if(!data.table::is.data.table(data)){data.table::setDT(x = data)}

  data.table::setorderv(data, cols = c(varname.i, varname.t))

  data[, "y"]		<- data[[varname.y]]
  data[, "y.lag1"]	<- data.table::shift(x = data[[varname.y]], n = 1L, type = "lag")
  data[, "y.lag2"]	<- data.table::shift(x = data[[varname.y]], n = 2L, type = "lag")
  data[, "y.lag3"]	<- data.table::shift(x = data[[varname.y]], n = 3L, type = "lag")

  data[i = dat[[varname.t]] == 1, j = c("y.lag1", "y.lag2", "y.lag3")]	<- NA
  data[i = dat[[varname.t]] == 2, j = c("y.lag2", "y.lag3")]	<- NA
  data[i = dat[[varname.t]] == 3, j = "y.lag3"]	<- NA


  if(eq8.2){
    rho.hat	<- sum( (data$y - data$y.lag1) * data$y.lag2 , na.rm = TRUE ) / sum( (data$y.lag1 - data$y.lag2) * data$y.lag2 , na.rm = TRUE )
  } else {
    rho.hat	<- sum( (data$y - data$y.lag1) * (data$y.lag2 - data$y.lag3) , na.rm = TRUE ) / sum( (data$y.lag1 - data$y.lag2) * (data$y.lag2 - data$y.lag3) , na.rm = TRUE )
  }

  return(rho.hat)

}



