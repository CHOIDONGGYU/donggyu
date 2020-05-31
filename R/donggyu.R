#' my inverse matrix
#'
#' this fuction can inverse matrix(2x2)
#'
#' @examples
#'
#' inverse.m(1,2,3,4)
inverse.m <- function(m1, m2, m3, m4) {
  if(m1*m4-m2*m3 == 0) {
    inverse <- NA
  } else {
    if(m1*m4-m2*m3 != 0) {
      inverse <- 1/(m1*m4-m2*m3)*c(m4, -m2, -m3, m1)
    }
  }
  matrix(inverse,2,2)
}



