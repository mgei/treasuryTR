#' Calculate bond total returns from constant-maturity yield data
#'
#' @param yields xts series or vector or yields
#' @param maturity bond constant-maturity in years
#' @param mdur modified duration, by default calculated using mod_duration()
#' @param convex convexity, by default calculated using convexity()
#' @param scale number of periods in a year (for US treasury data daily scale = 261, weekly scale = 52, monthly scale = 12, quarterly scale = 4)
#' @param format_out xts or tibble
#'
#' @details
#' Calculate bond total returns from constant-maturity yield data.
#'
#' \usepackage{lmodern}
#'
#' \deqn{R_t = yield income - duration\cdot \Delta y + \frac{1}{2} \cdot convexity \cdot (\Delta y)^2}{hello}
#'
#' where
#'
#' \deqn{yield income = (1+y_t)^{\Delta t}-1 \approx y_t {\Delta t}}{hello}
#'
#' \deqn{duration = \frac{1}{y_t} {z_t}^{2 M}}{hello}
#'
#' \deqn{convexity = C_1 - C_2}{hello}
#'
#' and
#'
#' \deqn{z_t = 1+\frac{y_t}{2}}{hello}
#'
#' \deqn{C_1 = \frac{2}{y_t^2}(1-{z_t}^{-2M})}{hello}
#'
#' \deqn{C_2 = \frac{2 M}{y_t} {z_t}^{-2 M - 1}}{hello}
#'
#' \eqn{M}{M} is the maturity in years (e.g. 10), \eqn{y_t}{y_t} is the yield at time \eqn{t}{t}
#'
#'
#' @return
#' Bond total returns
#'
#' @export
#'
#' @examples
#' # download US treasury 10Y constant-maturity
#' #   yield data and compute a total return series
#' t10_yield <- get_yields("DGS10")
#' t10_tr <- total_return(yields = t10_yield, maturity = 10)
#' head(t10_tr)
#'
#' # step-by-step calculation
#' t10_yield <- get_yields("DGS10", format_out = "tibble")
#' t10_tr <- t10_yield %>%
#'   mutate(convexity = convexity(DGS10, 10),
#'          mod_duration = mod_duration(DGS10, 10),
#'          TR = total_return(DGS10, 10, mod_duration, convexity))
#' head(t10_r)
total_return <- function(yields, maturity,
                         mdur = mod_duration(yields, maturity),
                         convex = convexity(yields, maturity),
                         scale = 261,
                         format_out = "xts") {
  if (!(format_out %in% c("xts", "tibble"))) {
    stop("format_out has to be one of xts or tibble")
  }

  yields_lag <- lag.xts(yields)[2:length(yields)]
  yields <- yields[2:length(yields)]

  out <- (-mdur*(yields-yields_lag) + 0.5*convex*(yields-yields_lag)^2 + ((1+yields_lag)^(1/scale)-1))

  if (format_out == "tibble") {
    out <- xts_to_tibble(out)
  }

  return(out)
}

