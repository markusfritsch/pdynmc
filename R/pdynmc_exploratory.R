






#' Show basic structure of panel data set.
#'
#' \code{data.info} shows basic structure of a balanced/unbalanced
#'    panel data set contained in a `data.frame`.
#'
#' @param object An object of class `data.frame`.
#' @param i.name Column name of cross-section identifier.
#' @param t.name Column name of time-series identifier.
#' @param ... further arguments.
#'
#' @return Returns information if panel data set contained
#'    in an object of class `data.frame` is a balanced or
#'    unbalanced panel data set.
#'
#' @author Markus Fritsch, Joachim Schnurbus
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
data.info	<- function(object, i.name = NULL, t.name = NULL, ...){
  if (!is.data.frame(object))
    stop("Function applied to non `data.frame`.")

  if(is.null(i.name) || is.null(t.name)){
    stop("Cross-section dimension and longitudinal dimension need to be specified.")
  }

  i.set		<- sort(unique(object[, i.name]))
  t.set		<- sort(unique(object[, t.name]))

  cs.obs.per.period		<- tapply(X = object[, i.name], INDEX = object[, t.name], FUN = length)

  balanced	<- var(cs.obs.per.period) == 0	# or < 2*10^(-14)

  if(balanced){
    cat(
      paste("Balanced panel data set with ", nrow(object), " rows:", sep = ""),
      "\n",
      paste(length(i.set), " cross-sectional units; ", length(t.set), " time periods (", paste(t.set, collapse = ", "), ").", sep = ""),
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


















#' Plot on structure of unbalanced panel data set.
#'
#' \code{strucPD.plot} Plot on cross-section and longtudinal
#'    structure of an object of class `data.frame` containing
#'    an unbalanced panel data set.
#'
#' @param object An object of class `data.frame`.
#' @param i.name Column name of cross-section identifier.
#' @param t.name Column name of time-series identifier.
#' @param col.range A vector of at least two colors used to
#'    visualize the structure of the unbalanced panel data
#'    set (defaults to 'gold' and 'darkblue'); must be a
#'    valid argument to \link{col2rgb}.
#' @param ... further arguments.
#'
#' @return Returns a plot for an unbalanced panel data set
#'    contained in an object of class `data.frame` that
#'    visualizes the structure of the data. Cross-section
#'    dimension is plotted on the ordinate, longitudinal
#'    dimension on the abscissa. Each cross-sectional
#'    observation is represented by a bar. Breaks in the
#'    bars represent missing longitudinal observations.
#'
#' @author Markus Fritsch, Joachim Schnurbus
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
#'
#' ## Code example
#'  strucUPD.plot(dat, i.name = "firm", t.name = "year")
#'
#'  set.seed(42)
#'  strucUPD.plot(dat[sample(x = 1:nrow(dat), size = floor(0.5*nrow(dat)), replace = FALSE), ], i.name = "firm", t.name = "year")
#' }
#'
#'
strucUPD.plot	<- function(object, i.name = NULL,	t.name = NULL, col.range = c("gold", "darkblue"), ...){
  if (!is.data.frame(object))
    stop("Function 'strucPD.plot' applied to non `data.frame`.")

  if(is.null(i.name) || is.null(t.name)){
    stop("Cross-section dimension and longitudinal dimension need to be specified.")
  }

  i.set		<- sort(unique(object[, i.name]))
  t.set		<- sort(unique(object[, t.name]))

  periods.per.cs.obs	<- tapply(X = object[, t.name], INDEX = object[, i.name], FUN = length)

  balanced	<- var(periods.per.cs.obs) == 0	# or < 2*10^(-14)
  if (balanced)
    stop("Plot is only suitable for unbalanced panel data.")

  par.mar.def	<- par()$mar	# save plot window default configuration
  par.xpd.def	<- par()$xpd	# save plot window default configuration
  par(mar = c(5.1, 4.1, 4.1, 6.1), xpd = TRUE)		# adjust plot window configuration

  plot(x = c(min(t.set) - 0.5, max(t.set) + 0.5), y = c(min(i.set) - 0.5, max(i.set) + 0.5),
    type = "n", xlab = t.name, ylab = i.name, main = "Unbalanced panel structure",
    xaxs = "i", yaxs = "i", xaxt = "n", ...)
  axis(side = 1, at = seq(from = min(t.set) - 0.5, to = max(t.set) + 0.5, by = 1), labels = FALSE)
  axis(side = 1, at = t.set, labels = t.set, tick = FALSE)

  col.set	<- colorRampPalette(col.range)(length(table(periods.per.cs.obs)))

  for(i in i.set){
    t.i	<- object[object[, i.name] == i, t.name]

    rect(
      xleft		= t.i - 0.5,
      ybottom	= i - 0.5,
      xright	= t.i + 0.5,
      ytop		= i + 0.5,
      col		= col.set[which(names(table(periods.per.cs.obs)) == length(t.i))],
      border	= col.set[which(names(table(periods.per.cs.obs)) == length(t.i))]
    )
  }

  legend(
    title = expression(T[i]), x.intersp = 0.2,
    x = max(t.set) + 0.5, y = max(i.set),
    legend = rev(names(table(periods.per.cs.obs))),
    fill = rev(col.set), border = rev(col.set), bg = "white", bty = "n"
  )
  box()

  par(mar = par.mar.def, xpd = par.xpd.def)	# return plot window default configuration
}











