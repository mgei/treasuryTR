#' Title
#'
#' @param yields
#' @param maturity
#'
#' @return
#' @export
#'
#' @examples
convexity <- function(yields, maturity, format_out = "xts") {
  if (!(format_out %in% c("xts", "tibble"))) {
    stop("format_out has to be one of xts or tibble")
  }

  out <- (2/(yields)^2)*(1-(1/(1+yields/2)^(2*maturity)))-(2*maturity)/((yields)*(1+yields/2)^(2*maturity+1))

  if (format_out == "tibble") {
    out <- xts_to_tibble(out)
  }

  return(out)
}
