#' Title
#'
#' @param yields
#' @param maturity
#' @param mdur
#' @param convex
#' @param scale
#' @param format_out
#'
#' @return
#' @export
#'
#' @examples
total_return <- function(yields, maturity,
                         mdur = mod_duration(yields, maturity),
                         convex = convexity(yields, maturity),
                         scale = 261,
                         format_out = "xts") {
  if (!(format_out %in% c("xts", "tibble"))) {
    stop("format_out has to be one of xts or tibble")
  }

  yields_lag <- lag.xts(yields)[2:nrow(yields)]
  yields <- yields[2:nrow(y)]

  out <- (-mdur*(yields-yields_lag) + 0.5*convex*(yields-yields_lag)^2 + ((1+yields_lag)^(1/scale)-1))

  if (format_out == "tibble") {
    out <- xts_to_tibble(out)
  }

  return(out)
}

