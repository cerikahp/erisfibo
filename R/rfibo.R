
#' Recursive nth element
#'
#'
#' @param n nth index
#'
#' @return nth fibonacci
#' @export
#'
#'
#' @examples
#' rfibo()
#'
rfibo <- function(n = 0) {
  if (n == 0) {
    return(0)
  }
  if (n == 1) {
    return(1)
  }
  return(rfibo(n - 1) + rfibo(n - 2))
}
