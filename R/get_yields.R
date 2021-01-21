#' get_yields
#'
#' @param series The series code as found on https://fred.stlouisfed.org/
#' @param ... Additional parameters handed to quantmod::getSymbols
#'
#' @return The yields data as xts
#'
#' @details
#' 1-Month Treasury Constant Maturity Rate: DGS1MO
#' 10-Year Treasury Constant Maturity Rate: DGS10
#'
#'
#' @export
#'
#' @examples
get_yields <- function(series = "DGS10", na_locf = T, percent_adjust = T, ...) {
  yields <- quantmod::getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = F, ...)

  if (na_locf) {
    yields <- na.locf0(yields)
  }

  if (percent_adjust) {
    yields <- yields/100
  }

  return(yields)
}
