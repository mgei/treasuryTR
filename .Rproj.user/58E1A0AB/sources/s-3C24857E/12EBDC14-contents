#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
tibble_to_xts <- function(x) {
  out <- xts(x = x %>% select(-date),
             order.by = x$date)
  return(out)
}
