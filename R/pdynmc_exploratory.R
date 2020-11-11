






#' Show Basic Structure of Panel Data Set.
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

  if(is.null(i.name) || is.null(t.name)){
    stop("Cross-section dimension and longitudinal dimension need to be specified.")
  }

  nCol <- ncol(object)

  object[, "i.label"] <- as.character(object[, i.name])
  object[, "t.label"] <- as.character(object[, t.name])

  i.set		<- 1:length(sort(as.numeric(unique(object[, i.name]))))
  t.set		<- 1:length(sort(as.numeric(unique(object[, t.name]))))

  i_cases			<- sort(as.numeric(unique(object[, i.name])))
  i_temp			<- 1:length(i_cases)				      # reflects data structures where i does not start at i = 1
  t_cases			<- sort(as.numeric(unique(object[, t.name])))
  t_temp			<- 1:length(unique(t_cases))			# reflects data structures where t does not start at t = 1

  object[, i.name] <- as.numeric(object[, i.name])
  object[, t.name] <- as.numeric(object[, t.name])

  object_b			  <- as.data.frame(array(data = NA, dim = c(length(i_cases)*length(t_cases), 2),dimnames = list(NULL, c(i.name, t.name))))
  object_b[, i.name]	<- rep(x = i_cases, each = length(t_cases))
  object_b[, t.name]	<- rep(x = t_cases, times = length(i_cases))


  object				<- merge(x = object_b, y = object, by = c(i.name, t.name), all.x = TRUE)
  object				<- object[order(object[, i.name], object[, t.name], decreasing = FALSE), ]

  object_b <- object[!is.na(apply(X = as.matrix(object[, 1:nCol]), FUN = sum, MARGIN = 1) ), ]

  periods.per.cs <- table(object_b[, "t.label"])

  balanced	<- sum(is.na(apply(X = as.matrix(object[, 1:nCol]), FUN = sum, MARGIN = 1) )) == 0	# or < 2*10^(-14)

  if(balanced){
    cat(
      paste("Balanced panel data set with ", nrow(object), " rows:", sep = ""),
      "\n",
      paste(length(i.set), " cross-sectional units; ", length(t.set), " time periods (", paste(t.set, collapse = ", "), ").", sep = ""),
      "\n"
    )
  } else {
    cat(
      paste("Unbalanced panel data set with ", nrow(object_b), " rows and the following time period frequencies:", sep = ""),
      "\n"
    )
    periods.per.cs
  }
}


















#' Plot on Structure of Unbalanced Panel Data Set.
#'
#' \code{strucUPD.plot} Plot on cross-section and longtudinal
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
#' @param plot.name A vector indicating the title of the plot
#'    (defaults to 'Unbalanced panel structure').
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
#'  set.seed(42)
#'  strucUPD.plot(dat[sample(x = 1:nrow(dat),
#'   size = floor(0.5*nrow(dat)), replace = FALSE), ],
#'  i.name = "firm", t.name = "year")
#' }
#'
#'
strucUPD.plot	<- function(
  object, i.name = NULL,	t.name = NULL,
  col.range = c("gold", "darkblue"), plot.name = "Unbalanced panel structure", ...
){
  if (!is.data.frame(object))
    stop("Function 'strucPD.plot' applied to non `data.frame`.")

  if(is.null(i.name) || is.null(t.name)){
    stop("Cross-section dimension and longitudinal dimension need to be specified.")
  }

  nCol <- ncol(object)

  object[, "i.label"] <- as.character(object[, i.name])
  object[, "t.label"] <- as.character(object[, t.name])

  i.set		<- 1:length(sort(as.numeric(unique(object[, i.name]))))
  t.set		<- 1:length(sort(as.numeric(unique(object[, t.name]))))

  i_cases			<- sort(as.numeric(unique(object[, i.name])))
  i_temp			<- 1:length(i_cases)				      # reflects data structures where i does not start at i = 1
  t_cases			<- sort(as.numeric(unique(object[, t.name])))
  t_temp			<- 1:length(unique(t_cases))			# reflects data structures where t does not start at t = 1

  object[, i.name] <- as.numeric(object[, i.name])
  object[, t.name] <- as.numeric(object[, t.name])

  object_b			  <- as.data.frame(array(data = NA, dim = c(length(i_cases)*length(t_cases), 2),dimnames = list(NULL, c(i.name, t.name))))
  object_b[, i.name]	<- rep(x = i_cases, each = length(t_cases))
  object_b[, t.name]	<- rep(x = t_cases, times = length(i_cases))


  object				<- merge(x = object_b, y = object, by = c(i.name, t.name), all.x = TRUE)
  object				<- object[order(object[, i.name], object[, t.name], decreasing = FALSE), ]

  object_b <- object[!is.na(apply(X = as.matrix(object[, 1:nCol]), FUN = sum, MARGIN = 1) ), ]

  periods.per.cs.obs        <- rep(0, times = length(i_cases))
  names(periods.per.cs.obs) <- i_cases

  ppcobs <- tapply(X = object_b[, t.name], INDEX = object_b[, i.name], FUN = length)
  periods.per.cs.obs[names(ppcobs)] <- ppcobs

  balanced	<- sum(is.na(apply(X = as.matrix(object[, 1:nCol]), FUN = sum, MARGIN = 1) )) == 0	# or < 2*10^(-14)
  if (balanced)
    stop("Plot is only suitable for unbalanced panel data.")

  par.mar.def	<- graphics::par()$mar	# save plot window default configuration
  par.xpd.def	<- graphics::par()$xpd	# save plot window default configuration
  graphics::par(mar = c(5.1, 4.1, 4.1, 6.1), xpd = TRUE)		# adjust plot window configuration

  plot(x = c(min(t.set) - 0.5, max(t.set) + 0.5), y = c(min(i.set) - 0.5, max(i.set) + 0.5),
    type = "n", xlab = t.name, ylab = i.name, main = plot.name,
    xaxs = "i", yaxs = "i", xaxt = "n", ...)
  graphics::axis(side = 1, at = seq(from = min(t.set) - 0.5, to = max(t.set) + 0.5, by = 1), labels = FALSE)
  graphics::axis(side = 1, at = t.set, labels = unique(object[,"t.label"]), tick = FALSE)

  col.set	<- grDevices::colorRampPalette(col.range)(length(if(sum(periods.per.cs.obs == 0) > 0){table(periods.per.cs.obs)[-1]} else{table(periods.per.cs.obs)} ))

  for(i in i.set){
    t.i	    <- as.numeric(periods.per.cs.obs[i])

    if(t.i > 0){

      t.i.start <- min(object_b[object_b[ ,i.name] == i, t.name])
      t.i.end   <- max(object_b[object_b[ ,i.name] == i, t.name])

      graphics::rect(
        xleft		= t.i.start - 0.5,
        ybottom	= i - 0.5,
        xright	= t.i.end + 0.5,
        ytop		= i + 0.5,
        col		= col.set[which(names(table(periods.per.cs.obs)) == t.i)],
        border	= col.set[which(names(table(periods.per.cs.obs)) == length(t.i))]
      )
    }
  }

  legend(
    title = expression(T[i]), x.intersp = 0.2,
    x = max(t.set) + 0.5, y = max(i.set),
    legend = rev(names(if(sum(periods.per.cs.obs == 0) > 0){table(periods.per.cs.obs)[-1]} else{table(periods.per.cs.obs)})),
    fill = rev(col.set), border = rev(col.set), bg = "white", bty = "n"
  )
  graphics::box()

  graphics::par(mar = par.mar.def, xpd = par.xpd.def)	# return plot window default configuration
}











