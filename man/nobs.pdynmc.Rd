% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdynmc_fitMethods.R
\name{nobs.pdynmc}
\alias{nobs.pdynmc}
\title{Extract Number of Observations of Fitted Model.}
\usage{
\method{nobs}{pdynmc}(object, ...)
}
\arguments{
\item{object}{An object of class `pdynmc`.}

\item{...}{further arguments.}
}
\value{
Extracts number of observations in cross-section dimension
   and longitudinal dimension of an object of class `pdynmc`.
}
\description{
\code{nobs.pdynmc} extracts number of observations in cross-section
   dimension and longitudinal dimension of an object of class
   `pdynmc`.
}
\examples{
## Load data from plm package
if(!requireNamespace("plm", quietly = TRUE)){
 stop("Dataset from package \"plm\" needed for this example.
 Please install the package.", call. = FALSE)
} else{
 data(EmplUK, package = "plm")
 dat <- EmplUK
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
 nobs(m1)
}

\donttest{
## Load data from plm package
if(!requireNamespace("plm", quietly = TRUE)){
 stop("Dataset from package \"plm\" needed for this example.
 Please install the package.", call. = FALSE)
} else{
 data(EmplUK, package = "plm")
 dat <- EmplUK
 dat[,c(4:7)] <- log(dat[,c(4:7)])

 m1 <- pdynmc(dat = dat, varname.i = "firm", varname.t = "year",
    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
    w.mat = "iid.err", std.err = "corrected", estimation = "onestep",
    opt.meth = "none")
 nobs(m1)
}
}


}
\seealso{
\code{\link{pdynmc}} for fitting a linear dynamic panel data model.
}
\author{
Markus Fritsch
}
