

#' Until nth fibonacci sequence
#'
#' @param fend the index of the last fibonacci, starting from 0
#'
#' @return a numeric arrary that represents a fibbonacci sequence from 0 to fend
#' @export
#'
#' @examples
#' nfibos(4)
#'
nfibos<-function(fend){
  if(!is.numeric(fend)){stop("nth is expected to be numeric")}
  if((fend)<0){stop("nth is expected to be positive")}
  fiboseq<-c(0)

  if(fend==0)return(fiboseq)
  for(i in 1:fend){
    if(i<=1){
        fiboseq<-c(fiboseq,1)

    }else{
      fiboseq<-c(fiboseq,fiboseq[i-1]+fiboseq[i])
    }

  }
  return(fiboseq)

}

#' subset fibonacci
#'
#' @inheritParams nfibos
#' @param f0n starting index
#'
#' @return  a numeric arrary that represents a fibbonacci sequence from f0n to fend
#' @export
#'
#' @examples
#' fibosfrom()
#'
fibosfrom<-function(f0n=0,fend=15){
  GoldenRatio=(1+sqrt(5))/2
  if(f0n==0){
    return(nfibos(fend))
  }
  f0<-((GoldenRatio^f0n)- (-GoldenRatio)^-f0n)
  f1<-((GoldenRatio^(f0n+1))- (-GoldenRatio)^-(f0n+1))
  fiboseq=c(f0,f1)
  for (i in f0n+2:fend) {
    size<-length(fiboseq)
    fiboseq<-c(fiboseq,fiboseq[size]+fiboseq[size-1])
  }
  return(fiboseq)
}