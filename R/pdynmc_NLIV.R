
############################################################################################################
### Closed form estimation functions for AR(1) panel data models
############################################################################################################




#' Function for closed form nonlinear IV-estimator - T-version.
#'
#' \code{NLIV.T} computes closed form solution for lag parameter of linear
#'    dynamic AR(1) panel data model based on original \insertCite{AhnSch1995;textual}{pdynmc}
#'    moment conditions.
#'
#' @param dat A dataset.
#' @param varname.i The name of the cross-section identifier.
#' @param varname.t The name of the time-series identifier.
#' @param varname.y A character string denoting the name of the dependent variable
#'    in the dataset.
#'
#' @export
#' @importFrom data.table is.data.table
#' @importFrom data.table setDT
#' @importFrom data.table setorderv
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#'
#' @examples
#' ## Load data
#' data(ABdata, package = "pdynmc")
#' dat <- ABdata
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' ## Code example
#' m1 <- NLIV.T(dat = dat, varname.i = "firm", varname.t = "year", varname.y = "emp")
#'
#'
NLIV.T	<- function (
  dat,
  varname.i,
  varname.t,
  varname.y
){

  if(!data.table::is.data.table(dat)){data.table::setDT(x = dat)}

  data.table::setorderv(dat, cols = c(varname.i, varname.t))

T	<- max(as.numeric(dat[[varname.t]]))

  dat[, "y"]	<- dat[[varname.y]]
  y.T			<- dat[i = dat[[varname.t]] == T, j = y]
  y.Tm1		<- dat[i = dat[[varname.t]] == T - 1, j = y]
  y.Tm2		<- dat[i = dat[[varname.t]] == T - 2, j = y]
  y.2			<- dat[i = dat[[varname.t]] == 2, j = y]
  y.1			<- dat[i = dat[[varname.t]] == 1, j = y]

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








#' Function for closed form nonlinear IV-estimator - t-version.
#'
#' \code{NLIV.t} computes closed form solution for lag parameter of linear
#'    dynamic AR(1) panel data model based on an alternative version of the
#'    \insertCite{AhnSch1995;textual}{pdynmc} moment conditions.
#'
#' @param dat A dataset.
#' @param varname.i The name of the cross-section identifier.
#' @param varname.t The name of the time-series identifier.
#' @param varname.y A character string denoting the name of the dependent variable
#'    in the dataset.
#'
#' @export
#' @importFrom data.table is.data.table
#' @importFrom data.table setDT
#' @importFrom data.table setorderv
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#'
#' @examples
#' ## Load data
#' data(ABdata, package = "pdynmc")
#' dat <- ABdata
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' ## Code example
#' m1 <- NLIV.t(dat = dat, varname.i = "firm", varname.t = "year", varname.y = "emp")
#'
#'
NLIV.t	<- function (
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


#' Function for closed form estimator of Han and Phillips (2010).
#'
#' \code{HP2010} computes closed form estimator for lag parameter of linear
#'    dynamic AR(1) panel data model as described in
#'    \insertCite{HanPhi2010;textual}{pdynmc}.
#'
#' @param dat A dataset.
#' @param varname.i The name of the cross-section identifier.
#' @param varname.t The name of the time-series identifier.
#' @param varname.y A character string denoting the name of the dependent variable
#'    in the dataset.
#'
#' @export
#' @importFrom data.table is.data.table
#' @importFrom data.table setDT
#' @importFrom data.table setorderv
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
#'
#'
#' @examples
#' ## Load data
#' data(ABdata, package = "pdynmc")
#' dat <- ABdata
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' ## Code example
#' m1 <- NLIV.t(dat = dat, varname.i = "firm", varname.t = "year", varname.y = "emp")
#'
#'
HP2010	<- function (
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
