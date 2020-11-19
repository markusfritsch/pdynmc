
#####################################################
###	Version information
#####################################################

###
###	Starting point
###

#	AhnSchmidt_Nonlinear_2017-11-07.R

#	split into different functions as of code version
#	AhnSchmidt_Nonlinear_2019-04-08.R




















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
#'    in the data set treated as instruments in estimation (defaults to `NULL`).
#' @param inst.reg.ex.expand A logical variable that allows for using all past,
#'    present, and future observations of `varname.reg.ex` to derive instruments
#'    (defaults to `TRUE`).
#' @param include.x.toInstr A logical variable that allows to instrument covariates
#'    (i.e., covariates which are not used as instruments but for which parameters
#'    are estimated; defaults to `FALSE`).
#' @param varname.reg.toInstr One or more character strings denoting the covariates
#'    in the data set to be instrumented (defaults to `NULL`).
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
#'    included in the equations in first differences (defaults to `NULL`).
#' @param dum.lev A logical variable indicating whether dummy variables are
#'    included in the equations in levels (defaults to `NULL`).
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
#' @return An object of class `pdynmc` with the following elements:
#'
#' \item{coefficients}{a vector containing the coefficient estimates}
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
#' \item{fitted.values}{a list for each estimation step that contains a list of
#'    vectors of fitted values for each cross-sectional observation}
#' \item{residuals}{a list for each estimation step that contains a list of vectors
#'    of residuals for each cross-sectional observation}
#' \item{vcov}{a list of matrices containing the variance covariance matrix of the
#'    parameter estimates for each estimation step}
#' \item{stderr}{a list of vectors containing the standard errors of the parameter
#'    estimates for each estimation step}
#' \item{zvalue}{a list of vectors containing the z scores for the parameter
#'    estimates for each estimation step}
#' \item{pvalue}{a list of vectors containing the p-values for the parameter
#'    estimates for each estimation step}
#'
#' It has `case.names`, `coef`, `dum.coef`, `fitted`, `model.matrix`, `ninst`,
#'    `nobs`, `optimIn`, `plot`, `print`,`residuals`, `summary`, `variable.names`,
#'    `vcov`, and `wmat` methods.
#'
#' @author Markus Fritsch
#' @export
#' @importFrom data.table shift
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
#'          use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'          include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'          fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'          varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'          include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'          w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'          opt.meth = "none")
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
#' ## Arellano and Bond (1991) estimation in Table 4, column (a1)
#'  m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'          use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'          include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'          fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'          varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'          include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'          w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
#'          opt.meth = "none")
#'  summary(m1)
#'
#' ## Arellano and Bond (1991) estimation in Table 4, column (a2)
#'  m2 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'          use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
#'          include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'          fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'          varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'          include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'          w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
#'          opt.meth = "none")
#'  summary(m2)
#'
#' ## Arellano and Bond (1991) twostep estimation extended by nonlinear moment
#' ## conditions
#'  m3 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'          use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
#'          include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'          fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'          varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'          include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'          w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
#'          opt.meth = "BFGS")
#'  summary(m3)
#'
#' ## Arellano and Bond (1991) iterative estimation extended by nonlinear moment
#' ## conditions
#'  m4 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'          use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = TRUE,
#'          include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'          fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'          varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'          include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'          w.mat = "iid.err", std.err = "corrected", estimation = "iterative",
#'          max.iter = 4, opt.meth = "BFGS")
#'  summary(m4)
#'
#' ## Arellano and Bond (1991) twostep estimation extended by linear moment
#' ## conditions from equations in levels
#'  m5 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
#'          use.mc.diff = TRUE, use.mc.lev = TRUE, use.mc.nonlin = FALSE,
#'          include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
#'          fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
#'          varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
#'          include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
#'          w.mat = "iid.err", std.err = "corrected", estimation = "twostep",
#'          opt.meth = "none")
#'  summary(m5)
#' }
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
 ,inst.reg.ex.expand  = TRUE

 ,include.x.instr			= FALSE
 ,varname.reg.instr		= NULL
 ,include.x.toInstr		= FALSE
 ,varname.reg.toInstr	= NULL

 ,fur.con				= FALSE
 ,fur.con.diff			= NULL
 ,fur.con.lev			= NULL
 ,varname.reg.fur			= NULL
 ,lagTerms.reg.fur		= NULL

 ,include.dum			= FALSE
 ,dum.diff				= NULL
 ,dum.lev				  = NULL
 ,varname.dum			= NULL
# ,custom.dum			= NULL
# ,partOut				= FALSE
# ,estimate.int			= FALSE

 ,col_tol				= 0.65

 ,w.mat				= "iid.err"
 ,w.mat.stata			= FALSE

 ,std.err				= "corrected"

 ,estimation			= "iterative"
 ,max.iter				= 100
 ,iter.tol				= 0.01
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
   j.max			<- max.iter
 }
# if(estimation == "cue"){
#   if(!(is.null(max.iter))){
#     j.max			<- max.iter
#   } else{
#     j.max			<- 100
#   }
# }


 if(use.mc.nonlin & opt.meth == "none"){
   opt.meth			<- "BFGS"
   warning("Nonlinear optimization required to use nonlinear moment conditions; 'opt.meth' was set to 'BFGS'.")
 }

 if(use.mc.nonlin == TRUE & is.null(use.mc.nonlinAS)){
   use.mc.nonlinAS	<- TRUE
 }




 resGMM			<- list()

 end.reg			<- !(is.null(varname.reg.end))
 pre.reg			<- !(is.null(varname.reg.pre))
 ex.reg			  <- !(is.null(varname.reg.ex))

 instr.reg			<- !(is.null(varname.reg.instr))
 toInstr.reg		<- !(is.null(varname.reg.toInstr))


 max.lagTerms		<- max(if(!(is.null(varname.y)) | !(is.null(lagTerms.y))){ lagTerms.y }, if(!(is.null(varname.reg.end))){ lagTerms.reg.end },
				if(!(is.null(varname.reg.pre))){ lagTerms.reg.pre }, if(!(is.null(varname.reg.ex))){ lagTerms.reg.ex },
				if(!(is.null(varname.reg.fur))){ lagTerms.reg.fur } )








 if((use.mc.diff | use.mc.lev) && (length(unique(dat[, varname.t])) < 3)){
   stop("Insufficient number of time periods to derive linear moment conditions.")
 }
 if(use.mc.nonlin && (length(unique(dat[, varname.t])) < 4)){
   stop("Insufficient number of time periods to derive nonlinear moment conditions.")
 }



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
     warning("No further controls included; argument 'fur.con.diff' was therefore ignored")
   }
   if(fur.con.lev){
     fur.con.lev <- FALSE
     warning("No further controls included; argument 'fur.con.lev' was therefore ignored")
   }
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
     warning("No dummies included; argument 'dum.diff' was therefore ignored")
   }
   if(!is.null(dum.lev)){
     dum.lev <- FALSE
     warning("No dummies included; argument 'dum.lev' was therefore ignored")
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




# if((mc.ref.t && mc.ref.T) | (is.null(mc.ref.t) && is.null(mc.ref.T))		# [M:] check that only one reference period is set; else choose 'mc.ref.t'
# ){
#   mc.ref.t		<- TRUE
#   mc.ref.T		<- FALSE
#   warning("Only one of 'mc.ref.t' and 'mc.ref.T' is allowed to be TRUE; 'mc.ref.T' was therefore set to FALSE.")
# }
























###
###	Expand data set and set number of cross-section-/time-series-observations
###



 i_cases		<- sort(unique(dat[, varname.i]))
 i_temp			<- 1:length(i_cases)				      # reflects data structures where i does not start at i = 1
 t_cases		<- sort(unique(dat[, varname.t]))
 t_temp			<- 1:length(unique(t_cases))			# reflects data structures where t does not start at t = 1


 dat_b			<- as.data.frame(array(data = NA, dim = c(length(i_cases)*length(t_cases), 2),dimnames = list(NULL, c(varname.i, varname.t))))
 dat_b[, varname.i]	<- rep(x = i_cases, each = length(t_cases))
 dat_b[, varname.t]	<- rep(x = t_cases, times = length(i_cases))


 dat				<- merge(x = dat_b, y = dat, by = c(varname.i, varname.t), all.x = TRUE)
 dat				<- dat[order(dat[, varname.i], dat[, varname.t], decreasing = FALSE), ]

 dat.na			<- dat
 dat[is.na(dat.na)]	<- 0

# i_cases		<- as.numeric(i_cases)




 n				<- length(unique(dat[, varname.i]))		# number of cross-section units
 Time				<- length(unique(dat[, varname.t]))		# number of time-series units



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


   D.add	<- stats::model.matrix(form.dum.temp, data = dat)[,-1]

   adjust.colnames.fct	<- function(
   j
   ){
     cols.dum.temp		<- gsub(pattern = varname.dum[j], replacement = "", x = colnames(D.add)[grepl(pattern = varname.dum[j], x = colnames(D.add))])
   }

   colnames.dum			<- Reduce(c, lapply(do.call(what = "c", args = list(sapply(1:length(varname.dum), FUN = adjust.colnames.fct))), FUN = c))


   colnames(D.add)		<- colnames.dum

#   colnames.dum   <- colnames(D.add)

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
#     I_nT		<- Matrix::Diagonal(n*Time)
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



 }else{
   colnames.dum   <- NULL
 }


















###
###	Specifying the number of lags avaialble to derive instruments and further expanding the data set
###


#a) maximum number of lags available as instruments

 if((include.y | !(is.null(lagTerms.y))) & !(is.null(maxLags.y))){
   if(maxLags.y + 2 > Time){				# [M:] maximum number of time periods of y_{it}- and x_{it}-process employed in estimation
     maxLags.y		<- Time-2
     warning(cat(paste(c("Longitudinal dimension too low. Maximum number of instruments from dependent variable to be employed in estimation",
				"was therefore reduced to ", Time-2, " (= Time-2)."), sep = "\n")) )
   }
   if(maxLags.y < 2 & use.mc.nonlin){
     use.mc.nonlin		<- FALSE
     warning(paste("Number of lags of dependent variable too low to obtain nonlinear moment conditions; 'use.mc.nonlin' was therefore set to 'FALSE'."))
   }
 } else{
   maxLags.y			<- Time-2
 }

 if(include.x){
   if(is.null(maxLags.reg.end)){
     try(if(length(maxLags.reg.end) != length(varname.reg.end)) stop("maximum number of lags of non-lagged-dependent endogenous covariates from which instruments should be derived needs to be specified completely"))
     if(any(maxLags.reg.end + 2 > Time)){
       maxLags.reg.end[maxLags.reg.end > Time-2]		<- Time - 2
       warning(cat(paste(c("Longitudinal dimension too low. Maximum number of lags to obtain instruments from non-lagged-dependent endogenous covariates",
				"was reduced to ", Time-2, " (= Time-2)."), sep = "\n")) )
     }
   }
   if(!is.null(varname.reg.end) & is.null(maxLags.reg.end)){
     maxLags.reg.end						<- rep(Time-2, times = length(varname.reg.end))
     warning(paste("Number of lags of the non-lagged dependent endogenous covariates from which instruments should be derived not specified. Number was set to ", Time-2, " (= Time-2) for the ", length(varname.reg.end), " endogenous covariates.", sep = ""))
   }
   if(!(is.null(maxLags.reg.pre))){
     try(if(length(maxLags.reg.pre) != length(varname.reg.pre)) stop("maximum number of lags of non-lagged-dependent predetermined covariates from which instruments should be derived needs to be specified completely."))
     if(any(maxLags.reg.pre + 1 > Time)){
       maxLags.reg.pre[maxLags.reg.pre > Time-1]		<- Time - 1
       warning(cat(paste(c("Longitudinal dimension too low. Maximum number of lags to obtain instruments from non-lagged-dependent predetermined covariates",
				"was reduced to ", Time-2, " (= Time-2)."), sep = "\n")) )
     }
   }
   if(!(is.null(varname.reg.pre)) & is.null(maxLags.reg.pre)){
     maxLags.reg.pre						<- rep(Time-1, times = length(varname.reg.pre))
     warning(cat(paste("Number of lags of non-lagged dependent predetermined covariates from which instruments should be derived not specified.",
			"Number was set to ", Time-1, " (= Time-1) for the ", length(varname.reg.pre), " predetermined covariates.", sep = "\n")) )
   }
   if(!(is.null(maxLags.reg.ex))){
     try(if(length(maxLags.reg.ex) != length(varname.reg.ex)) stop("maximum number of lags of non-lagged-dependent exogenous covariates from which instruments should be derived needs to be specified completely"))
     if(any(maxLags.reg.ex > Time)){
       maxLags.reg.ex[maxLags.reg.ex > Time]		<- Time					# [M:] only required for HNR m.c. (from equ. in differences)
       warning(cat(paste(c("Longitudinal dimension too low. Maximum number of lags to obtain instruments from non-lagged-dependent exogenous covariates",
				"was reduced to ", Time-2, " (= Time-2)."), sep = "\n")) )
     }
   }
   if(!(is.null(varname.reg.ex)) & is.null(maxLags.reg.ex)){
     maxLags.reg.ex						<- rep(Time, times = length(varname.reg.ex))
     warning(cat(paste("Number of lags of non-lagged dependent exogenous covariates from which instruments should be derived not specified.",
			"Number was set to ", Time, " (= Time) for the ", length(varname.reg.ex), " exogenous covariates.", sep = "\n")) )
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



# if(include.y){
   if(lagTerms.y > 0){
     varname.reg.estParam.y				<- do.call(what = "varname.expand", args = list(varname = varname.y, lagTerms = lagTerms.y) )
     if(length(varname.reg.estParam.y) == 1){
       dat.na[, varname.reg.estParam.y]		<- as.vector(mapply(lagTerms = rep(c(1:lagTerms.y), each = length(i_cases)), i = i_cases, varname = varname.y, FUN = dat.na.lag))
     } else{
       dat.na[, varname.reg.estParam.y]		<- mapply(lagTerms = rep(c(1:lagTerms.y), each = length(i_cases)), i = i_cases, varname = varname.y, FUN = dat.na.lag)
     }
   }
# }

 if(include.x){
   if(!(is.null(varname.reg.end)) & sum(!(varname.reg.end %in% varname.reg.instr)) > 0){
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
   if(!(is.null(varname.reg.pre)) & sum(!(varname.reg.pre %in% varname.reg.instr)) > 0){
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
   if(!(is.null(varname.reg.ex)) & sum(!(varname.reg.ex %in% varname.reg.instr)) > 0){
     varname.temp					<- if(!(is.null(varname.reg.instr))){ varname.reg.ex[!(varname.reg.ex %in% varname.reg.instr)] } else{ varname.reg.ex }
     lagTerms.temp					<- if(!(is.null(varname.reg.instr))){ lagTerms.reg.ex[!(varname.reg.ex %in% varname.reg.instr)] } else{ lagTerms.reg.ex }
     if(length(varname.reg.ex) == 1){
       varname.reg.estParam.x.ex			<- as.vector(mapply(varname = varname.temp, lagTerms = lagTerms.temp, FUN = varname.expand) )
       dat.na[, varname.reg.estParam.x.ex]	<- as.vector(mapply(lagTerms = rep(mapply(lagTerms.temp, varname = varname.temp, FUN = lag.expand), each = length(i_cases)),
											i = rep(i_cases, times = length(varname.reg.estParam.x.ex)),
											varname = mapply(varname.temp, FUN = rep, each = (lagTerms.reg.ex+1)*length(i_cases) ),
										FUN = dat.na.lag))
     } else{
       varname.reg.estParam.x.ex			<- do.call(what = "c", args = list(mapply(varname = varname.temp, lagTerms = lagTerms.temp, FUN = varname.expand)) )
       dat.na[, varname.reg.estParam.x.ex]	<- mapply(lagTerms = rep(do.call(what = "c", args = list(mapply(lagTerms.temp, varname = varname.temp, FUN = lag.expand))), each = length(i_cases)),
										i = rep(i_cases, times = length(varname.reg.estParam.x.ex)),
										varname = do.call(what = "c", args = list(mapply(varname.temp, FUN = rep, each = (lagTerms.reg.ex+1)*length(i_cases)) )),
									FUN = dat.na.lag)
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
 } else{
   varname.reg.estParam.fur <- NULL
 }

 varname.reg.estParam		 <- c(if(exists("varname.reg.estParam.y")) as.vector(varname.reg.estParam.y)			# [M:] covariates (besides the lagged dependent variable) for which to estimate parameters
						,if(exists("varname.reg.estParam.x.end")) as.vector(varname.reg.estParam.x.end)
						,if(exists("varname.reg.estParam.x.pre")) as.vector(varname.reg.estParam.x.pre)
						,if(exists("varname.reg.estParam.x.ex")) as.vector(varname.reg.estParam.x.ex)
						,if(exists("varname.reg.estParam.fur") & !(is.null(varname.reg.estParam.fur))) as.vector(varname.reg.estParam.fur) )



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







 dat.na$i.label      <- dat.na[, varname.i]
 dat.na[, varname.i] <- as.numeric(dat.na[, varname.i])

 dat					        <- dat.na
 dat[is.na(dat.na)]		<- 0







###
###	Combination of Z-part of Equations (3) and (4) of AS (requires helper functions)
###



 Z.obj		<- lapply(X = i_cases, FUN = Z_i.fct, Time = Time, varname.i = varname.i
#					, mc.ref.t = mc.ref.t
					,use.mc.diff = use.mc.diff, use.mc.lev = use.mc.lev, use.mc.nonlin = use.mc.nonlin
					,include.y = include.y, varname.y = varname.y, inst.stata = inst.stata
					,include.dum = include.dum, dum.diff = dum.diff, dum.lev = dum.lev, colnames.dum = colnames.dum
					,fur.con = fur.con, fur.con.diff = fur.con.diff, fur.con.lev = fur.con.lev, varname.reg.estParam.fur = varname.reg.estParam.fur
   				,include.x = include.x, end.reg = end.reg, varname.reg.end = varname.reg.end, pre.reg = pre.reg, varname.reg.pre = varname.reg.pre, ex.reg = ex.reg, varname.reg.ex = varname.reg.ex
					,maxLags.y = maxLags.y, lagTerms.y = lagTerms.y, max.lagTerms = max.lagTerms, maxLags.reg.end = maxLags.reg.end, maxLags.reg.pre = maxLags.reg.pre, maxLags.reg.ex = maxLags.reg.ex, inst.reg.ex.expand = inst.reg.ex.expand, dat = dat, dat.na = dat.na)


 resGMM$n.inst		<- apply(Reduce(f = rbind, x = lapply(Z.obj, `[[`, 3)), FUN = max, MARGIN = 2)

 colnames.dum.Z		<- as.vector(unique(Reduce(f = rbind, x = lapply(Z.obj, `[[`, 2) ) ))

 resGMM$Z.temp		<- lapply(Z.obj, `[[`, 1)

 resGMM$diffMC		<- use.mc.diff
 resGMM$levMC		  <- use.mc.lev
 resGMM$nlMC		  <- use.mc.nonlin
 resGMM$varname.i <- varname.i
 resGMM$varname.t <- varname.t




 if(include.dum){
   if((dum.lev & !(dum.diff)) | (dum.lev & dum.diff)){
     varname.reg.estParam	<- c(varname.reg.estParam, colnames.dum[colnames.dum %in% colnames.dum.Z])
   } else{
     varname.reg.estParam	<- c(varname.reg.estParam, unlist(lapply(strsplit(x = colnames.dum.Z, split = "D."), FUN = `[[`, 2)))
   }
 }


 resGMM$dat.na			<- dat.na
 resGMM$n				    <- n
 resGMM$Time				<- Time

 resGMM$varname.y			<- varname.y
 resGMM$varnames.reg		<- varname.reg.estParam
 resGMM$varnames.fur.con <- if(fur.con){varname.reg.fur} else{ "no further controls"}
 if(include.dum){
   resGMM$varnames.dum		<- colnames.dum[colnames.dum %in% varname.reg.estParam]
 } else{
   resGMM$varnames.dum    <- "no time dummies"
 }

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




 resGMM.Dat			<- gmmDat.fct(dat.na = dat.na, n = n, Time = Time, varname.y = varname.y, varname.reg.estParam)




# resGMM.Dat			<- list()
#
# resGMM.Dat$y_m1		<- lapply(X = i_cases, FUN = gmmDat_m1.fct, dat.na = dat.na, Time = Time, varname = varname.y, varname.i = varname.i)
# resGMM.Dat$y_tm1		<- lapply(X = i_cases, FUN = gmmDat_tm1.fct, dat.na = dat.na, Time = Time, varname = varname.y, varname.i = varname.i)
# resGMM.Dat$dy		<- mapply(function(x,y) x - y, resGMM.Dat$y_m1,resGMM.Dat$y_tm1, SIMPLIFY = FALSE)
#
# resGMM.Dat$X_m1		<- lapply(lapply(X = i_cases, FUN = gmmDat_m1.fct, dat.na = dat.na, Time = Time, varname = varname.reg.estParam, varname.i = varname.i), FUN = as.matrix)
# resGMM.Dat$X_tm1		<- lapply(lapply(X = i_cases, FUN = gmmDat_tm1.fct, dat.na = dat.na, Time = Time, varname = varname.reg.estParam, varname.i = varname.i), FUN = as.matrix)
# resGMM.Dat$dX		<- mapply(function(x,y) x - y, resGMM.Dat$X_m1, resGMM.Dat$X_tm1, SIMPLIFY = FALSE)







###
###	Computation of weighting matrix and optimization
###

 env				<- as.numeric()
 par.opt.j			<- as.numeric()
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

   resGMM.W.j[[j]]				<- Wonestep.fct(w.mat = w.mat, w.mat.stata = w.mat.stata, use.mc.diff = use.mc.diff, use.mc.lev = use.mc.lev, use.mc.nonlin = use.mc.nonlin
						,dum.diff = dum.diff, dum.lev = dum.lev, fur.con.diff = fur.con.diff, fur.con.lev = fur.con.lev
						,Z.temp = resGMM$Z.temp, n = n, Time = Time, env = env
#						,mc.ref.t = mc.ref.t
						, max.lagTerms = max.lagTerms, end.reg = end.reg, ex.reg = ex.reg, pre.reg = pre.reg, n.inst = resGMM$n.inst, inst.thresh = inst.thresh)
   names(resGMM.W.j)[j]		<- paste("step", j, sep = "")

   resGMM.H.i			<- H_i


   if(opt.meth == "none"){
     resGMM.opt.j[[j]]		<- rep(NA, times = length(varname.reg.estParam))
   }

   if(opt.meth != "none"){




     par.opt.j		 		<- optimx::optimx(
       par = resGMM$param.ini, fn = gmmObj.fct, method = opt.meth, control = optCtrl
       ,j = j, y_m1 = resGMM.Dat$y_m1, X_m1 = resGMM.Dat$X_m1, dy = resGMM.Dat$dy, dX = resGMM.Dat$dX
       ,varname.reg.estParam = resGMM$varnames.reg, n = n, Time = Time, include.y = include.y, varname.y = varname.y
       ,use.mc.diff = use.mc.diff, use.mc.nonlin = use.mc.nonlin, use.mc.nonlinAS = use.mc.nonlinAS , use.mc.lev = use.mc.lev
       ,dum.diff = dum.diff, fur.con.diff = fur.con.diff, max.lagTerms = max.lagTerms, end.reg = end.reg, ex.reg = ex.reg, pre.reg = pre.reg
       ,dum.lev = dum.lev, fur.con.lev = fur.con.lev
       ,Z.temp = resGMM$Z.temp, W = resGMM.W.j[[1]], env = env
#       ,mc.ref.t = mc.ref.t, mc.ref.T = mc.ref.T, N_i = N_i
     )

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
					,dum.diff = dum.diff, dum.lev = dum.lev, fur.con.diff = fur.con.diff, fur.con.lev = fur.con.lev, max.lagTerms = max.lagTerms, Time = Time
					,include.x = include.x, pre.reg = pre.reg, ex.reg = ex.reg)

   if(nrow(resGMM$Z.temp[[1]]) == 1){
     dat.clF.temp		<- lapply(lapply(dat.temp, `[[`, 1), function(x) Matrix::t(x))
     dep.temp			<- lapply(dat.temp, `[[`, 2)
   } else{
     dat.clF.temp		<- lapply(dat.temp, `[[`, 1)
     dep.temp			<- lapply(dat.temp, `[[`, 2)
   }

   dat.clF.temp.0		<- rapply(lapply(dat.clF.temp, FUN = as.matrix), function(x) ifelse(is.na(x), 0, x), how = "replace")
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


   tZX				<- Reduce("+", mapply(function(x,y) Matrix::crossprod(x,y), resGMM$Z.temp, dat.clF.temp.0, SIMPLIFY = FALSE))
   tZY				<- Reduce("+", mapply(function(x,y) Matrix::crossprod(x,y), resGMM$Z.temp, dep.temp.0, SIMPLIFY = FALSE))

#   tXZW1tZX.inv			<- solve(tcrossprod(crossprod(as.matrix(tZX), get(paste("step", j, sep = ""), resGMM.W.j)), t(as.matrix(tZX))))
   tXZW1tZX.inv			<- MASS::ginv(as.matrix(Matrix::crossprod(tZX, Matrix::crossprod(get(paste("step", j, sep = ""), resGMM.W.j), tZX)) ) )
#   tXZW1tZY				<- Matrix::crossprod(tZX, Matrix::crossprod(get(paste("step", j, sep = ""), resGMM.W.j), tZY))
   tYZW1tZX				<- Matrix::crossprod(tZY, Matrix::crossprod(get(paste("step", j, sep = ""), resGMM.W.j), tZX))

   if(!use.mc.nonlin){
     resGMM.clF.j[[j]]		<- as.numeric(Matrix::tcrossprod(tXZW1tZX.inv, tYZW1tZX))
   } else{
     resGMM.clF.j[[j]]		<- rep(NA, times = length(varname.reg.estParam))
   }
   names(resGMM.clF.j)[j]		<- paste("step", j, sep = "")

   if(opt.meth != "none"){
     resGMM.fitted.j[[j]]		<- lapply(mapply(function(x) Matrix::tcrossprod(x, Matrix::t(get(paste("step", j, sep = ""), resGMM.par.opt.j))), dat.clF.temp, SIMPLIFY=FALSE), FUN = as.numeric)
     resGMM.Szero.j[[j]]		<- mapply(function(y,x) y - Matrix::tcrossprod(x, Matrix::t(get(paste("step", j, sep = ""), resGMM.par.opt.j))), dep.temp, dat.clF.temp, SIMPLIFY=FALSE)
     resGMM.Szero.j[[j]]		<- lapply(rapply(lapply(resGMM.Szero.j[[j]], FUN = as.matrix), f = function(x) ifelse(is.na(x), 0, x), how = "replace"), FUN = as.vector)

   } else{
     resGMM.fitted.j[[j]]		<- lapply(mapply(function(x) Matrix::tcrossprod(x, Matrix::t(get(paste("step", j, sep = ""), resGMM.clF.j))), dat.clF.temp, SIMPLIFY=FALSE), FUN = as.numeric)
     resGMM.Szero.j[[j]]		<- mapply(function(y,x) y - Matrix::tcrossprod(x, Matrix::t(get(paste("step", j, sep = ""), resGMM.clF.j))), dep.temp, dat.clF.temp, SIMPLIFY=FALSE)
     resGMM.Szero.j[[j]]		<- lapply(rapply(lapply(resGMM.Szero.j[[j]], FUN = as.matrix), f = function(x) ifelse(is.na(x), 0, x), how = "replace"), FUN = as.vector)
   }

   names(resGMM.fitted.j)[j]	<- paste("step", j, sep = "")
   names(resGMM.Szero.j)[j]		<- paste("step", j, sep = "")


   resGMM.W.j[[j + 1]]  <- Wtwostep.fct(Sj.0 = get(paste("step", j, sep = "") , resGMM.Szero.j), Z.temp = resGMM$Z.temp, n.inst = sum(resGMM$n.inst), inst.thresh = inst.thresh)
   names(resGMM.W.j)[j + 1] <- paste("step", j+1, sep = "")

   n.obs				<- nrow(dat.na) - sum(is.na(dat.na[, varname.y]))
   resGMM.n.obs			<- n.obs

   if(std.err == "unadjusted"){
     resGMM.vcov.j[[j]]		<- tXZW1tZX.inv * (as.vector(Matrix::crossprod(do.call(get(paste("step", j, sep = "") , resGMM.Szero.j), what = "c"), do.call(get(paste("step", j, sep = "") , resGMM.Szero.j), what = "c"), na.rm = TRUE) /(n.obs - length(varname.reg.estParam))))		# [M:] calculation acc. to description in Doornik, Arellano, and Bond (2012), p.30-31
   }
   if(std.err == "corrected"){
     resGMM.vcov.j[[j]]		<- Matrix::tcrossprod(Matrix::crossprod(tXZW1tZX.inv, Matrix::tcrossprod(Matrix::tcrossprod(Matrix::crossprod(tZX, get(paste("step", j, sep = "") , resGMM.W.j)), MASS::ginv(get(paste("step", j+1, sep = "") , resGMM.W.j))), Matrix::tcrossprod(Matrix::t(tZX), get(paste("step", j, sep = "") , resGMM.W.j)))), tXZW1tZX.inv)
   }
   names(resGMM.vcov.j)[j]	<- paste("step", j, sep = "")

   if(sum(diag(as.matrix(get(paste("step", j, sep = "") , resGMM.vcov.j))) < 0) > 0){
     neg.ent                <- which(diag(as.matrix(get(paste("step", j, sep = "") , resGMM.vcov.j))) < 0)
     resGMM.stderr.j[[j]]		<- sqrt(abs(diag(as.matrix(get(paste("step", j, sep = "") , resGMM.vcov.j)))))
     warning(paste("Covariance function contains ", length(neg.ent), " negative value(s); observation index(es): \n", paste(neg.ent, collapse = ", "), sep = ""))
     rm(neg.ent)
   } else{
     resGMM.stderr.j[[j]]		<- sqrt(diag(as.matrix(get(paste("step", j, sep = "") , resGMM.vcov.j))))
   }
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
         resGMM.W.j[[j]]		<- Wtwostep.fct(Sj.0 = get(paste("step", j-1, sep = "") , resGMM.Szero.j), Z.temp = resGMM$Z.temp, n.inst = sum(resGMM$n.inst), inst.thresh = inst.thresh)
         names(resGMM.W.j)[j]		<- paste("step", j, sep = "")
       }

       if(opt.meth == "none"){
         resGMM.opt.j[[j]]		<- rep(NA, times = length(varname.reg.estParam))
       }

       if(opt.meth != "none"){

         par.opt.j		<- optimx::optimx(
#          results.GMM1s		<- optimx::optimx(
           par = resGMM.par.opt.j[[j-1]], fn = gmmObj.fct, method = opt.meth, hessian = hessian, control = optCtrl
#          par = as.numeric(par.opt.j[1:length(varname.reg.estParam)]), fn = gmmObj.fct, method = opt.meth, hessian = hessian, control = optCtrl
          ,j = j, Z.temp = resGMM$Z.temp, y_m1 = resGMM.Dat$y_m1, X_m1 = resGMM.Dat$X_m1, dy = resGMM.Dat$dy, dX = resGMM.Dat$dX
          ,varname.reg.estParam = resGMM$varnames.reg, n = n, Time = Time, include.y = include.y, varname.y = varname.y
          ,use.mc.diff = use.mc.diff, use.mc.nonlin = use.mc.nonlin, use.mc.nonlinAS = use.mc.nonlinAS , use.mc.lev = use.mc.lev
          ,dum.diff = dum.diff, fur.con.diff = fur.con.diff, max.lagTerms = max.lagTerms, end.reg = end.reg, ex.reg = ex.reg, pre.reg = pre.reg
          ,dum.lev = dum.lev, fur.con.lev = fur.con.lev
          ,W = resGMM.W.j[[j]], env = env
#         ,mc.ref.t = mc.ref.t, mc.ref.T = mc.ref.T, N_i = N_i
         )

#         resGMM.fitted.j[[j]]		<- fitted.j
#         resGMM.Szero.j[[j]]		<- Szero.j
       }
       resGMM.par.opt.j[[j]]		<- as.numeric(par.opt.j[1:length(varname.reg.estParam)])
       names(resGMM.par.opt.j)[j]	<- paste("step", j, sep = "")
       resGMM.ctrl.opt.j[[j]]		<- par.opt.j[-c(1:length(varname.reg.estParam))]
       names(resGMM.ctrl.opt.j)[j]	<- paste("step", j, sep = "")

       tXZW2tZX.inv			<- MASS::ginv(as.matrix(Matrix::crossprod(tZX, Matrix::crossprod(get(paste("step", j, sep = ""), resGMM.W.j), tZX))) )
#       tXZW2tZY				<- Matrix::tcrossprod(Matrix::crossprod(tZX, get(paste("step", j, sep = ""), resGMM.W.j)), Matrix::t(tZY))
       tYZW2tZX				<- Matrix::crossprod(tZY, Matrix::crossprod(get(paste("step", j, sep = ""), resGMM.W.j), tZX))

       if(!use.mc.nonlin){
         resGMM.clF.j[[j]]		<- as.numeric(Matrix::tcrossprod(tXZW2tZX.inv, tYZW2tZX))
       } else{
         resGMM.clF.j[[j]]		<- rep(NA, times = length(varname.reg.estParam))
       }
       names(resGMM.clF.j)[j]		<- paste("step", j, sep = "")

       if(opt.meth != "none"){
         resGMM.fitted.j[[j]]		<- lapply(mapply(function(x) Matrix::tcrossprod(x, Matrix::t(get(paste("step", j, sep = ""), resGMM.par.opt.j))), dat.clF.temp, SIMPLIFY=FALSE), FUN = as.numeric)
         resGMM.Szero.j[[j]]		<- mapply(function(y,x) y - Matrix::tcrossprod(x, Matrix::t(get(paste("step", j, sep = ""), resGMM.par.opt.j))), dep.temp, dat.clF.temp, SIMPLIFY=FALSE)
         resGMM.Szero.j[[j]]		<- lapply(rapply(lapply(resGMM.Szero.j[[j]], FUN = as.matrix), f = function(x) ifelse(is.na(x), 0, x), how = "replace"), FUN = as.vector)

       } else{
         resGMM.fitted.j[[j]]		<- lapply(mapply(function(x) Matrix::tcrossprod(x, Matrix::t(get(paste("step", j, sep = ""), resGMM.clF.j))), dat.clF.temp, SIMPLIFY=FALSE), FUN = as.numeric)
         resGMM.Szero.j[[j]]		<- mapply(function(y,x) y - Matrix::tcrossprod(x, Matrix::t(get(paste("step", j, sep = ""), resGMM.clF.j))), dep.temp, dat.clF.temp, SIMPLIFY=FALSE)
         resGMM.Szero.j[[j]]		<- lapply(rapply(lapply(resGMM.Szero.j[[j]], FUN = as.matrix), f = function(x) ifelse(is.na(x), 0, x), how = "replace"), FUN = as.vector)
       }

       names(resGMM.fitted.j)[j]	<- paste("step", j, sep = "")
       names(resGMM.Szero.j)[j]	<- paste("step", j, sep = "")


       resGMM.vcov.j[[j]]		<- MASS::ginv(as.matrix(Matrix::crossprod(tZX, Matrix::crossprod(get(paste("step", j, sep = ""), resGMM.W.j), tZX) ) ) )
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
         D_k	<- Matrix::crossprod((-1)*get(paste("step", j, sep = ""), resGMM.vcov.j), Matrix::crossprod(Matrix::crossprod(get(paste("step", j, sep = ""), resGMM.W.j), tZX), Matrix::tcrossprod(tZtux_kZ, Matrix::tcrossprod(Matrix::t(tZ.res2s), get(paste("step", j, sep = ""), resGMM.W.j) ) ) ) )
         D		<- cbind(D, D_k)
       }


       resGMM.vcov.j[[j]]		<- get(paste("step", j, sep = ""), resGMM.vcov.j) + Matrix::tcrossprod(D, get(paste("step", j, sep = ""), resGMM.vcov.j)) + Matrix::tcrossprod(D, get(paste("step", j, sep = ""), resGMM.vcov.j)) + Matrix::tcrossprod(Matrix::tcrossprod(D, get(paste("step", 1, sep = ""), resGMM.vcov.j)), D)
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
#   , y_m1 = resGMM.Dat$y_m1, X_m1 = resGMM.Dat$X_m1, dy = resGMM.Dat$dy, dX = resGMM.Dat$dX ,varname.reg.estParam = resGMM$varnames, n = n, Time = Time
#   , include.y = include.y, varname.y = varname.y, use.mc.diff = use.mc.diff, use.mc.nonlin = use.mc.nonlin, use.mc.nonlinAS = use.mc.nonlinAS, use.mc.lev = use.mc.lev
#   , dum.diff = dum.diff, fur.con.diff = fur.con.diff, max.lagTerms = max.lagTerms, end.reg = end.reg, ex.reg = ex.reg, pre.reg = pre.reg
#   , dum.lev = dum.lev, fur.con.lev = fur.con.lev, mc.ref.t	= mc.ref.t, W = resGMM$W1
##   ,mc.ref.T = mc.ref.T, N_i = N_i
# )
###


#     resGMM.iter 	<- j

     if(opt.meth != "none"){
       if((j > 2) && ((j == j.max) | (sum(abs(as.numeric(get(paste("step", j, sep = "") , resGMM.par.opt.j)) - as.numeric(get(paste("step", j-1, sep = "") , resGMM.par.opt.j))))) < iter.tol) ) break
     } else{
       if((j > 2) && ((j == j.max) | (sum(abs(as.numeric(get(paste("step", j, sep = "") , resGMM.clF.j)) - as.numeric(get(paste("step", j-1, sep = "") , resGMM.clF.j))))) < iter.tol) ) break
     }
   }

 }

 resGMM.iter    <- j
 coefGMM        <- if(resGMM$opt.method == "none"){ get(paste("step", resGMM.iter, sep = ""), resGMM.clF.j)} else{get(paste("step", resGMM.iter, sep = ""), resGMM.par.opt.j)}
 names(coefGMM) <- resGMM$varnames.reg

# if(estimation == "cue"){
#
#   res.GMM.cue		<- list()
#
#   resGMM.cue 		<- optimx(
#    j = j, par = resGMM$param.ini, fn = gmm_cueObj.fct, method = opt.meth, hessian = FALSE, control = optCtrl
#    ,Z.temp = Z.temp, y_m1 = resGMM.Dat$y_m1, X_m1 = resGMM.Dat$X_m1, dy = resGMM.Dat$dy, dX = resGMM.Dat$dX
#    ,varname.reg.estParam = resGMM$varnames, n = n, Time = Time, include.y = include.y, varname.y = varname.y
#    ,use.mc.diff = use.mc.diff, use.mc.nonlin = use.mc.nonlin, use.mc.nonlinAS = use.mc.nonlinAS , use.mc.lev = use.mc.lev
#    ,dum.diff = dum.diff, fur.con.diff = fur.con.diff, max.lagTerms = max.lagTerms, end.reg = end.reg, ex.reg = ex.reg, pre.reg = pre.reg
#    ,dum.lev = dum.lev, fur.con.lev = fur.con.lev, mc.ref.t = mc.ref.t
##    ,mc.ref.T = mc.ref.T, N_i = N_i
#   )
#
#
#
# }


 fit 		<-  list(coefficients = coefGMM, residuals = resGMM.Szero.j, fitted.values = resGMM.fitted.j,
   par.optim = resGMM.par.opt.j, ctrl.optim = resGMM.ctrl.opt.j, par.clForm = resGMM.clF.j, iter = resGMM.iter,
   w.mat = resGMM.W.j, H_i = resGMM.H.i, vcov = resGMM.vcov.j, stderr = resGMM.stderr.j,
   zvalue = resGMM.zvalue.j, pvalue = resGMM.pvalue.j,
   data = resGMM, dep.clF = dep.temp, dat.clF = dat.clF.temp)
 attr(fit, "class")  <- "pdynmc"

 return(fit)


}









