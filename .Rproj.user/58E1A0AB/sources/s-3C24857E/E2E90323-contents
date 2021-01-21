#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
xts_to_tibble <- function(x) {
  out <- as_tibble(x) %>%
    mutate(date = index(x)) %>%
    select(date, everything())

  return(out)
}
