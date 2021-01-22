#' Convert an xts object to a tibble data frame
#'
#' @param x an xts object
#'
#' @return
#' A tibble data frame with first column 'date'
#'
#' @export
xts_to_tibble <- function(x) {
  if (!is.xts(x)) {
    stop("x has to be an xts object")
  }

  out <- as_tibble(x) %>%
    mutate(date = index(x)) %>%
    select(date, everything())

  return(out)
}
