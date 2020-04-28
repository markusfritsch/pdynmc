






#' Investigate if panel data set is balanced or unbalanced.
#'
#' \code{data.info} Plot fitted values against residuals for an
#'    object of class `pdynmc`.
#'
#' @param object An object of class `pdynmc`.
#' @param i.name Column name of cross-section identifier.
#' @param t.name Column name of time-series identifier.
#' @param ... further arguments.
#'
#' @return Returns information if object of class `data.frame`
#'    is a balanced or unbalanced panel data set.
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
#'  data.info(dat, i.name = "firm", t.name = "year")
#'
#'  data.info(dat[dat$year %in% 1979:1981, ], i.name = "firm", t.name = "year")
#' }
#'
#'
data.info	<- function(object, i.name, t.name, ...){
  if (!is.data.frame(object))
    stop("'data.info' applied to non data frame")

  i.set		<- sort(unique(object[, i.name]))
  t.set		<- sort(unique(object[, t.name]))

  cs.obs.per.period		<- tapply(X = object[, i.name], INDEX = object[, t.name], FUN = length)

  balanced	<- var(cs.obs.per.period) == 0	# or < 2*10^(-14)

  if(balanced){
    cat(
      paste("Balanced panel data set with ", nrow(object), " rows, ", sep = ""),
      "\n",
      paste("i.e., ", length(i.set), " cross-sectional units, and ", length(t.set), " time periods (", paste(t.set, collapse = ", "), ").", sep = ""),
      "\n"
    )
  } else {
    cat(
      paste("Unbalanced panel data set with ", nrow(object), " rows and the following time period frequencies:", sep = ""),
      "\n"
    )
    cs.obs.per.period
  }
}




























