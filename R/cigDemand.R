#' Cigarette consumption in the US
#'
#' Balanced panel dataset on annual cigarette consumption in the
#' US for the 48 continental states in the years 1985 until 1995.
#' The dataset is available from \insertCite{Stock2003;textual}{pdynmc}
#' and used in \insertCite{SW2019;textual}{pdynmc} and
#' \insertCite{FriPuaSch2022;textual}{pdynmc}.
#' Gratitude is owed to Jonathan Gruber of MIT for
#' providing the data.
#'
#' @name cigDemand
#'
#' @docType data
#'
#' @usage data(cigDemand)
#'
#' @format A dataset with 528 rows and 9 variables containing:
#' \describe{
#' \item{state}{state}
#' \item{year}{year}
#' \item{cpi}{consumer price index (US)}
#' \item{pop}{state population}
#' \item{packpc}{number of cigarette packs sold per capita and year}
#' \item{income}{state personal income (total, nominal)}
#' \item{tax}{average federal, state, and local excise taxes on cigarettes for fiscal year in cents per pack}
#' \item{avgprs}{average price during fiscal year in cents per pack (including sales taxes)}
#' \item{taxs}{average excise tax for fiscal year in cents per pack (including sales taxes)}
#' }
#'
#' @keywords datasets
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' \dontrun{
#'   data(cigDemand, package = "pdynmc")
#'   packs <- cigDemand$packpc
#'   tax <- cigDemand$tax
#'   \donttest{plot(y = packs, x = tax)}
#' }
#'
NULL
