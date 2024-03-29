% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdynmc_fitMethods.R
\name{print.summary.pdynmc}
\alias{print.summary.pdynmc}
\title{Print Summary of Fitted Model Object.}
\usage{
\method{print}{summary.pdynmc}(
  x,
  digits = max(3, getOption("digits") - 3),
  signif.stars = getOption("show.signif.stars"),
  ...
)
}
\arguments{
\item{x}{An object of class `summary.pdynmc`.}

\item{digits}{An integer indicating the maximum number of digits to
display in the object.}

\item{signif.stars}{Argument is defined as in \code{\link{options}}.}

\item{...}{further arguments.}
}
\value{
Print information on objects of class `summary.pdynmc`.
}
\description{
\code{print.summary.pdynmc} prints the summary for objects of class
   `pdynmc`.
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
summary(m1)

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
 summary(m1)
}


}
\seealso{
\code{\link{pdynmc}} for fitting a linear dynamic panel data model.
}
\author{
Markus Fritsch
}
