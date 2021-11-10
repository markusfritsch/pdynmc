






#' Show Basic Structure of Panel Dataset.
#'
#' \code{data.info} shows basic structure of a balanced/unbalanced
#'    panel dataset contained in a `data.frame`.
#'
#' @param object An object of class `data.frame`.
#' @param i.name Column name of cross-section identifier.
#' @param t.name Column name of time-series identifier.
#' @param ... further arguments.
#'
#' @return Returns information if panel dataset contained
#'    in an object of class `data.frame` is a balanced or
#'    unbalanced panel dataset.
#'
#' @author Markus Fritsch, Joachim Schnurbus
#' @export
#' @importFrom stats var
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
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
#'  data.info(dat, i.name = "firm", t.name = "year")
#'
#'  data.info(dat[dat$year %in% 1979:1981, ], i.name = "firm", t.name = "year")
#' }
#'
#'
data.info	<- function(object, i.name = NULL, t.name = NULL, ...){
  if (!is.data.frame(object))
    stop("Function applied to non `data.frame`.")
  if (is.null(i.name) || is.null(t.name)) {
    stop("Cross-section dimension and longitudinal dimension need to be specified.")
  }


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


















#' Plot on Structure of Unbalanced Panel Dataset.
#'
#' \code{strucUPD.plot} Plot on cross-section and longtudinal
#'    structure of an object of class `data.frame` containing
#'    an unbalanced panel dataset.
#'
#' @param object An object of class `data.frame`.
#' @param i.name Column name of cross-section identifier.
#' @param t.name Column name of time-series identifier.
#' @param col.range A vector of at least two colors used to
#'    visualize the structure of the unbalanced panel data
#'    set (defaults to 'gold' and 'darkblue'); must be a
#'    valid argument to \link{col2rgb}.
#' @param plot.name A vector indicating the title of the plot
#'    (defaults to 'Unbalanced panel structure').
#' @param ... further arguments.
#'
#' @return Returns a plot for an unbalanced panel dataset
#'    contained in an object of class `data.frame` that
#'    visualizes the structure of the data. Cross-section
#'    dimension is plotted on the ordinate, longitudinal
#'    dimension on the abscissa. Each cross-sectional
#'    observation is represented by a bar. Breaks in the
#'    bars represent missing longitudinal observations.
#'
#' @author Markus Fritsch, Joachim Schnurbus
#' @export
#' @importFrom graphics axis
#' @importFrom graphics box
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics rect
#' @importFrom grDevices colorRampPalette
#' @importFrom stats var
#'
#' @seealso
#'
#' \code{\link{pdynmc}} for fitting a linear dynamic panel data model.
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
#'
#' ## Code example
#'  strucUPD.plot(dat, i.name = "firm", t.name = "year")
#'
#' }
#'
#'
strucUPD.plot	<- function(
  object, i.name = NULL,	t.name = NULL,
  col.range = c("gold", "darkblue"), plot.name = "Unbalanced panel structure", ...
){
  if (!is.data.frame(object))
    stop("Function 'strucPD.plot' applied to non `data.frame`.")
  if (is.null(i.name) || is.null(t.name)) {
    stop("Cross-section dimension and longitudinal dimension need to be specified.")
  }

  i.set		<- sort(unique(object[, i.name]))
  i.set.ind		<- 1:length(i.set)
  t.set		<- sort(unique(object[, t.name]))

  cs.obs.per.period		<- tapply(X = object[, i.name], INDEX = object[, t.name], FUN = length)
  periods.per.cs.obs		<- tapply(X = object[, t.name], INDEX = object[, i.name], FUN = length)
  balanced	<- var(cs.obs.per.period) == 0	# or < 2*10^(-14)

  if (balanced)
    stop("Plot is only suitable for unbalanced panel data.")
  par.mar.def <- graphics::par()$mar
  par.xpd.def <- graphics::par()$xpd
  graphics::par(mar = c(5.1, 4.1, 4.1, 6.1), xpd = TRUE)
  plot(x = c(min(t.set) - 0.5, max(t.set) + 0.5), y = c(min(i.set.ind) -
                                                          0.5, max(i.set.ind) + 0.5), type = "n", xlab = t.name,
       ylab = i.name, main = plot.name, xaxs = "i", yaxs = "i",
       xaxt = "n", ...)
  graphics::axis(side = 1, at = seq(from = min(t.set) - 0.5,
                                    to = max(t.set) + 0.5, by = 1), labels = FALSE)
  graphics::axis(side = 1, at = t.set, labels = t.set,
                 tick = FALSE)
  col.set <- (grDevices::colorRampPalette(col.range))(length(if (sum(periods.per.cs.obs ==
                                                                     0) > 0) {
    table(periods.per.cs.obs)[-1]
  } else {
    table(periods.per.cs.obs)
  }))
  for (i in i.set.ind) {
    t.i <- as.numeric(periods.per.cs.obs[i])
    if (t.i > 0) {

      t.i.set <- object[object[, i.name] == i.set[i], t.name]

      graphics::rect(xleft = t.i.set - 0.5, ybottom = i -
                       0.5, xright = t.i.set + 0.5, ytop = i + 0.5,
                     col = col.set[which(names(table(periods.per.cs.obs)) ==
                                           t.i)], border = col.set[which(names(table(periods.per.cs.obs)) ==
                                                                           t.i)])
    }
  }
  legend(title = expression(T[i]), x.intersp = 0.2, x = max(t.set) +
           0.5, y = max(i.set.ind), legend = rev(names(if (sum(periods.per.cs.obs ==
                                                               0) > 0) {
             table(periods.per.cs.obs)[-1]
           } else {
             table(periods.per.cs.obs)
           })), fill = rev(col.set), border = rev(col.set), bg = "white",
         bty = "n")
  graphics::box()
  graphics::par(mar = par.mar.def, xpd = par.xpd.def)
}











