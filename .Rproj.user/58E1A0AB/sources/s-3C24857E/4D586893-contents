#' Title
#'
#' @param yields
#' @param maturity
#' @param format_out
#'
#' @return
#' @export
#'
#' @examples
mod_duration <- function(yields, maturity, format_out = "xts") {
  if (!(format_out %in% c("xts", "tibble"))) {
    stop("format_out has to be one of xts or tibble")
  }

  out <- (1-(1/(1+0.5*yields)^(2*maturity)))/(yields)

  if (format_out == "tibble") {
    out <- xts_to_tibble(out)
  }

  return(out)
}
