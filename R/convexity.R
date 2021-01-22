#' Calculate the convexity of a bond
#'
#' @param yields a series of yields
#' @param maturity constant bond maturity in years
#' @param format_out xts or tibble
#'
#' @description
#' Calculates the convexity of a bond.
#'
#' This function is normally used in combination with total_return() to compute bond total returns.
#'
#' @details
#' This function is normally used in combination with total_return() to compute bond total returns.
#'
#' The convexity is the interest rate sensitivity of the modified duration.
#'
#' \usepackage{lmodern}
#'
#' \deqn{convexity = C_1 - C_2}{hello}
#'
#' where
#'
#' \deqn{C_1 = \frac{2}{y_t^2} (1-{z_t}^{-2M})}{hello}
#' \deqn{C_2 = \frac{2M}{y_t}{z_t}^{-2M-1}}{hello}
#' \deqn{z_t = 1+\frac{y_t}{2}}{hello}
#'
#' \eqn{M}{M} is the maturity in years (e.g. 10), \eqn{y_t}{t_y} is the yield at time \eqn{t}{t}.
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
