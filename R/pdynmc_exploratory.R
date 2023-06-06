






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
#' ## Load data
#' data(ABdata, package = "pdynmc")
#' dat <- ABdata
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#' dat <- dat[c(1:140), ]
#'
#' ## Code example
#' data.info(dat, i.name = "firm", t.name = "year")
#'
#' data.info(dat[dat$year %in% 1979:1981, ], i.name = "firm", t.name = "year")
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
      paste("Balanced panel data set with ", nrow(object), " rows: ", sep = ""),
      "\n",
      paste(length(i.set), " cross-sectional units, and ", length(t.set), " time periods (", paste(t.set, collapse = ", "), ").", sep = ""),
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
#' ## Load data
#' data(ABdata, package = "pdynmc")
#' dat <- ABdata
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' ## Code example
#' strucUPD.plot(dat, i.name = "firm", t.name = "year")
#'
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


















#' Plot empirical density of a column of a Panel Dataset.
#'
#' \code{pDensTime.plot} Plot the empirical density
#'    of a column of an object of class `data.frame`
#'    containing a panel dataset across time
#'    periods/aggregates of time periods.
#'
#' @param object An object of class `data.frame`.
#' @param i.name Column name of cross-section identifier.
#' @param t.name Column name of time-series identifier.
#' @param var.name Column name of the variable that is
#'    plotted (see `Details').
#' @param aggregate.t Argument of data type `numeric'.
#'    If argument is specified, the corresponding number
#'    of time periods is merged (approximately); (defaults
#'    to 'NULL').
#' @param plot.quantiles Argument of data type `logical',
#'    indicating whether the 5\%- and 95\%-quantiles and
#'    the quartiles should be plotted (as specified by
#'    `col.set[2]'; defaults to 'TRUE').
#' @param plot.mean_ci Argument of data type `logical',
#'    indicating whether the mean and the approximate
#'    confidence intervals (mean plus/minus 2 standard
#'    deviations) should be plotted (as specified by
#'    `col.set[3]'; defaults to 'TRUE').
#' @param plot.extrema Argument of data type `logical',
#'    indicating whether the minimal and maximal observed
#'    value (per time period/group) should be plotted
#'    (as specified by `col.set[4]'; defaults to 'TRUE').
#' @param col.set Vector of length 4 with entries of
#'    data type `character' used to visualize the entities
#'    of `pDensTime.plot' (see `Details'); must be a
#'    valid argument to `col2rgb'; defaults to
#'    `c("gray", "navy", "darkorange1", "red")'.
#'
#' @return Returns a plot that visualizes the empirical
#'    density for a column of a panel dataset
#'    contained in an object of class `data.frame`. The
#'    variable of interest is plotted on the ordinate,
#'    the longitudinal dimension on the abscissa. For each
#'    time period or aggregate of time periods, one
#'    empirical density is computed and plotted.
#'    Corresponding summary statistics on empirical
#'    quantiles and the sample size per longitudinal
#'    dimension are included in the plot.
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
#' ## Load data
#' data(ABdata, package = "pdynmc")
#' dat <- ABdata
#' dat[,c(4:7)] <- log(dat[,c(4:7)])
#'
#' ## Minimal set of arguments
#' pDensTime.plot(object = ABdata, var.name = "emp", i.name = "firm", t.name = "year")
#'
#' ## All arguments explicitly stated
#' pDensTime.plot(object	= ABdata, var.name	= "emp", i.name	= "firm", t.name	= "year",
#'  aggregate.t	= NULL,	plot.quantiles	= TRUE, plot.mean_ci	= TRUE, plot.extrema	= TRUE,
#'  col.set		= c("gray", "navy", "darkorange1", "red"))
#'
#' ## Aggregation over time periods (3 time periods per group)
#' pDensTime.plot(object = ABdata, var.name = "emp", i.name = "firm", t.name = "year",
#'  aggregate.t = 3)
#'
#' ## Employ alternative colouring scheme
#' pDensTime.plot(object = ABdata, var.name = "emp", i.name = "firm", t.name = "year",
#'  col.set		= c("pink", "blue", "purple", "black"))
#'
#' ## Plot only density, mean, and asymptotic confidence interval
#' pDensTime.plot(object = ABdata, var.name = "emp", i.name = "firm", t.name = "year",
#'  plot.quantiles	= FALSE, plot.extrema	= FALSE)
#'
pDensTime.plot	<- function(
    object,
    var.name,
    i.name,
    t.name,
    aggregate.t	= NULL,	# if numeric, the corresponding number of time periods is merged (approximately)
    plot.quantiles	= TRUE,
    plot.mean_ci	= TRUE,
    plot.extrema	= TRUE,
    col.set		= c("gray", "navy", "darkorange1", "red")
){

  if (!is.data.frame(object)) {
    stop("Function 'pDensTime.plot' applied to non `data.frame`.")
  }
  if (is.null(i.name) || is.null(t.name)) {
    stop("Cross-section dimension and longitudinal dimension need to be specified.")
  }

  t.set		<- sort(unique(object[, t.name]))
  i.set		<- sort(unique(object[, i.name]))
  da		<- object[, var.name]




  if(is.numeric(aggregate.t)){
    ###
    ###	aggregated version (to approximately aggregate.t number of time periods per group)
    ###

    t.cat			<- floor((t.set - min(t.set))/aggregate.t)
    t.cat.values	<- floor((object[, t.name] - min(object[, t.name]))/aggregate.t)
    if(t.cat[length(t.cat)] != t.cat[length(t.cat) - 1]){
      t.cat.values[t.cat.values == t.cat[length(t.cat)]] <- t.cat[length(t.cat) - 1]
      t.cat[length(t.cat)] <- t.cat[length(t.cat) - 1]
    }
    t.cat.set		<- unique(t.cat)

    t.cat.names		<- rep(NA, length(t.cat.set))
    for(i in 1:length(t.cat.names)){
      t.cat.temp	<- t.cat.set[i]
      t.cat.names[i]	<- paste(range(t.set[t.cat == t.cat.temp]), collapse = "-")
    }



    dat.t.dens	<- list()
    for(el in t.cat.set){
      dat.t.dens[[paste("t.", el, sep = "")]]	<- density(da[object[, t.name] %in% t.set[t.cat == el]])
    }
    maxy		<- function(da){max(da$y)}
    dens.max	<- max(sapply(X = dat.t.dens, FUN = maxy))





    plot(
      x		= t.cat.set,
      xlim	= range(t.cat.set) + c(-0.5, 1),
      ylim	= range(da),
      type	= "n",
      xlab	= "time",
      ylab	= var.name,
      xaxs	= "i",
      yaxs	= "i",
      xaxt	= "n"
    )
    axis(side = 1, at = t.cat.set, labels = t.cat.names)

    abline(v = t.cat.set, col = col.set[1])
    for(el in t.cat.set){
      polygon(
        x	= el + (dat.t.dens[[paste("t.", el, sep = "")]]$y/dens.max),
        y	= dat.t.dens[[paste("t.", el, sep = "")]]$x,
        col = col.set[1], border = col.set[1]
      )
    }

    if(plot.quantiles){
      lines(x = t.cat.set, y = tapply(X = da, INDEX = t.cat.values, FUN = median), col = col.set[2], type = "b", pch = 19)
      lines(x = t.cat.set, y = tapply(X = da, INDEX = t.cat.values, FUN = quantile, probs = 0.25), col = col.set[2], type = "b", pch = 19, lty = 2)
      lines(x = t.cat.set, y = tapply(X = da, INDEX = t.cat.values, FUN = quantile, probs = 0.75), col = col.set[2], type = "b", pch = 19, lty = 2)
      lines(x = t.cat.set, y = tapply(X = da, INDEX = t.cat.values, FUN = quantile, probs = 0.05), col = col.set[2], type = "b", pch = 19, lty = 3)
      lines(x = t.cat.set, y = tapply(X = da, INDEX = t.cat.values, FUN = quantile, probs = 0.95), col = col.set[2], type = "b", pch = 19, lty = 3)
      quant.temp	<- quantile(da[t.cat.values == min(t.cat.set)], probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
      text(x = min(t.cat.set) - 0.2, y = quant.temp, labels = paste("q", c("05", "25", "50", "75", "95"), sep = ""), col = rep(col.set[2], 5))
    }

    if(plot.mean_ci){
      mp2s	<- function(da){mean(da) + 2*sd(da)}
      mm2s	<- function(da){mean(da) - 2*sd(da)}
      lines(x = t.cat.set, y = tapply(X = da, INDEX = t.cat.values, FUN = mean), col = col.set[3], type = "b", pch = 19)
      lines(x = t.cat.set, y = tapply(X = da, INDEX = t.cat.values, FUN = mp2s), col = col.set[3], type = "b", pch = 19, lty = 3)
      lines(x = t.cat.set, y = tapply(X = da, INDEX = t.cat.values, FUN = mm2s), col = col.set[3], type = "b", pch = 19, lty = 3)
      text(x = max(t.cat.set) + 0.35, y = mean(da[t.cat.values == max(t.cat.set)]), labels = "Mean", col = col.set[3])
      text(x = max(t.cat.set) + 0.4, y = mean(da[t.cat.values == max(t.cat.set)]) - 2*sd(da[t.cat.values == max(t.cat.set)]), labels = "M-2SD", col = col.set[3])
      text(x = max(t.cat.set) + 0.4, y = mean(da[t.cat.values == max(t.cat.set)]) + 2*sd(da[t.cat.values == max(t.cat.set)]), labels = "M+2SD", col = col.set[3])
    }

    if(plot.extrema){
      lines(x = t.cat.set, y = tapply(X = da, INDEX = t.cat.values, FUN = quantile, probs = 0), col = col.set[4], type = "l", pch = 19, lty = 1)
      lines(x = t.cat.set, y = tapply(X = da, INDEX = t.cat.values, FUN = quantile, probs = 1), col = col.set[4], type = "l", pch = 19, lty = 1)
      quant.temp	<- quantile(da[t.cat.values == min(t.cat.set)], probs = 0:1)
      text(x = min(t.cat.set) - 0.2, y = quant.temp, labels = c("Min", "Max"), col = rep(col.set[4], 2))
    }

    axis(side = 3, at = t.cat.set, labels = paste("N=", tapply(X = da, INDEX = t.cat.values, FUN = length), sep = ""))
    box()

  } else {		# i.e., aggregate.t is not a number
    ###
    ###	disaggregated version, i.e., one density per period
    ###


    dat.t.dens	<- list()
    for(el in t.set){
      dat.t.dens[[paste("t.", el, sep = "")]]	<- density(da[object[, t.name] == el])
    }
    maxy		<- function(da){max(da$y)}
    dens.max	<- max(sapply(X = dat.t.dens, FUN = maxy))



    plot(
      x	= t.set,
      xlim	= range(t.set) + c(-0.5, 1),
      ylim	= range(da),
      type	= "n",
      xlab	= "time",
      ylab	= var.name,
      xaxs	= "i",
      yaxs	= "i",
      xaxt	= "n"
    )
    axis(side = 1, at = t.set, labels = t.set)

    abline(v = t.set, col = col.set[1])
    for(el in t.set){
      polygon(
        x	= el + (dat.t.dens[[paste("t.", el, sep = "")]]$y/dens.max),
        y	= dat.t.dens[[paste("t.", el, sep = "")]]$x,
        col = col.set[1], border = col.set[1]
      )
    }

    if(plot.quantiles){
      lines(x = t.set, y = tapply(X = da, INDEX = object[, t.name], FUN = median), col = col.set[2], type = "b", pch = 19)
      lines(x = t.set, y = tapply(X = da, INDEX = object[, t.name], FUN = quantile, probs = 0.25), col = col.set[2], type = "b", pch = 19, lty = 2)
      lines(x = t.set, y = tapply(X = da, INDEX = object[, t.name], FUN = quantile, probs = 0.75), col = col.set[2], type = "b", pch = 19, lty = 2)
      lines(x = t.set, y = tapply(X = da, INDEX = object[, t.name], FUN = quantile, probs = 0.05), col = col.set[2], type = "b", pch = 19, lty = 3)
      lines(x = t.set, y = tapply(X = da, INDEX = object[, t.name], FUN = quantile, probs = 0.95), col = col.set[2], type = "b", pch = 19, lty = 3)
      quant.temp	<- quantile(da[object[, t.name] == min(t.set)], probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
      text(x = min(t.set) - 0.2, y = quant.temp, labels = paste("q", c("05", "25", "50", "75", "95"), sep = ""), col = rep(col.set[2], 5))
    }

    if(plot.mean_ci){
      mp2s	<- function(da){mean(da) + 2*sd(da)}
      mm2s	<- function(da){mean(da) - 2*sd(da)}
      lines(x = t.set, y = tapply(X = da, INDEX = object[, t.name], FUN = mean), col = col.set[3], type = "b", pch = 19)
      lines(x = t.set, y = tapply(X = da, INDEX = object[, t.name], FUN = mp2s), col = col.set[3], type = "b", pch = 19, lty = 3)
      lines(x = t.set, y = tapply(X = da, INDEX = object[, t.name], FUN = mm2s), col = col.set[3], type = "b", pch = 19, lty = 3)
      text(x = max(t.set) + 0.35, y = mean(da[object[, t.name] == max(t.set)]), labels = "Mean", col = col.set[3])
      text(x = max(t.set) + 0.4, y = mean(da[object[, t.name] == max(t.set)]) - 2*sd(da[object[, t.name] == max(t.set)]), labels = "M-2SD", col = col.set[3])
      text(x = max(t.set) + 0.4, y = mean(da[object[, t.name] == max(t.set)]) + 2*sd(da[object[, t.name] == max(t.set)]), labels = "M+2SD", col = col.set[3])
    }

    if(plot.extrema){
      lines(x = t.set, y = tapply(X = da, INDEX = object[, t.name], FUN = quantile, probs = 0), col = col.set[4], type = "l", pch = 19, lty = 1)
      lines(x = t.set, y = tapply(X = da, INDEX = object[, t.name], FUN = quantile, probs = 1), col = col.set[4], type = "l", pch = 19, lty = 1)
      quant.temp	<- quantile(da[object[, t.name] == min(t.set)], probs = 0:1)
      text(x = min(t.set) - 0.2, y = quant.temp, labels = c("Min", "Max"), col = rep(col.set[4], 2))
    }

    axis(side = 3, at = t.set, labels = paste("N=", tapply(X = da, INDEX = object[, t.name], FUN = length), sep = ""))
    box()

  }

}







