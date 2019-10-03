

#' Until nth fibonacci sequence
#'
#'@importFrom magrittr %>%
#' @param fend the index of the last fibonacci, starting from 0
#'
#' @return a numeric arrary that represents a fibbonacci sequence from 0 to fend
#' @export
#'
#' @examples
#' nfibos(4)
#'
nfibos <- function(fend) {
  if (!is.numeric(fend)) {
    stop("nth is expected to be numeric")
    }
  if (fend < 0) {
    stop("nth is expected to be positive")
    }
  fiboseq <- c(0)

  if (fend == 0) return(fiboseq)
  for (i in 1:fend) {
    if (i <= 1) {
        fiboseq <- c(fiboseq, 1)
    } else {
      fiboseq <- c(fiboseq, fiboseq[i - 1] + fiboseq[i])
    }

  }
  return(fiboseq)

}

#' subset fibonacci
#'
#' @inheritParams nfibos
#' @param f0n starting index
#'
#' @return  a numeric arrary that represents
#' a fibbonacci sequence from f0n to fend
#' @export
#'
#' @examples
#' fibosfrom()
#'
fibosfrom <- function(f0n = 0, fend = 15) {
  if (!is.numeric(fend) || !is.numeric(f0n)) {
     stop("parameters are expected to be numeric")
    }
  if (fend < f0n) {
    stop("fend should be greater than f0n")
    }

  golden_ratio <- (1 + sqrt(5)) / 2
  if (f0n == 0) {
    return(nfibos(fend))
  }
  f0 <- as.integer(
    ((golden_ratio ^ f0n) - (-golden_ratio) ^ -f0n) / sqrt(5)
    )
  f1 <- as.integer(
    ((golden_ratio ^ (f0n + 1)) - (-golden_ratio) ^ -(f0n + 1)) / sqrt(5)
    )
  fiboseq <- c(f0, f1)
  if (fend == f0n) {
    return(f0n)
  }
  if (fend == f0n + 1) {
    return(fiboseq)
  }
  for (i in (f0n + 2):fend) {
    size <- length(fiboseq)
    fiboseq <- c(fiboseq, fiboseq[size] + fiboseq[size - 1])
  }
  return(fiboseq)
}
