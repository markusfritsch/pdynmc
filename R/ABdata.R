#' Employment, wages, capital, and output for companies based in the UK
#'
#' Unbalanced panel dataset of 140 firms from different sectors
#' located in the UK which were observed over the years 1976
#' until 1984. The dataset contains the variables firm, year,
#' sector, employment, wages, capital, and output and was used
#' in \insertCite{AreBon1991;textual}{pdynmc} which also
#' provides more details on the different variables.
#'
#' @name ABdata
#'
#' @docType data
#'
#' @usage data(ABdata)
#'
#' @format A dataset with 1031 rows and 7 variables containing:
#' \describe{
#' \item{firm}{firm identifier}
#' \item{year}{year}
#' \item{sector}{sector}
#' \item{emp}{number of firm employees in the UK}
#' \item{wage}{real wage}
#' \item{capital}{gross capital stock}
#' \item{output}{industry output}
#' }
#'
#' @keywords datasets
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' \dontrun{
#'   data(ABdata, package = "pdynmc")
#'   n <- ABdata$emp
#'   w <- ABdata$wage
#'   \donttest{plot(y = n, x = w)}
#' }
#'
NULL
