

#' @title Pipe Function To Paste Two Characters
#' @param x character
#' @param y character
#' @return paste0(x,y)
`%&%` <- function(x, y){
  base::paste0(x, y)
}

