% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdynmc_fitMethods.R
\name{vcov.pdynmc}
\alias{vcov.pdynmc}
\title{Extract Variance Covariance Matrix of Fitted Model.}
\usage{
\method{vcov}{pdynmc}(object, step = object$iter, ...)
}
\arguments{
\item{object}{An object of class `pdynmc`.}

\item{step}{An integer denoting the iteration step for which
fitted values are extracted (defaults to last iteration step
used for obtaining parameter estimates).}

\item{...}{further arguments.}
}
\value{
Extract variance covariance matrix of the parameter estimates
   from an object of class `pdynmc`.
}
\description{
\code{vcov.pdynmc} extracts variance covariance matrix of the
   paramter estimates from an object of class `pdynmc`.
}
\examples{
## Load data
data(ABdata, package = "pdynmc")
dat <- ABdata
dat[,c(4:7)] <- log(dat[,c(4:7)])
dat <- dat[c(1:140), ]

## Code example
m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
    opt.meth = "none")
vcov(m1)

\donttest{
## Load data
 data(ABdata, package = "pdynmc")
 dat <- ABdata
 dat[,c(4:7)] <- log(dat[,c(4:7)])

 m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
    opt.meth = "none")
 vcov(m1)
}


}
\seealso{
\code{\link{pdynmc}} for fitting a linear dynamic panel data model.
}
\author{
Markus Fritsch
}
