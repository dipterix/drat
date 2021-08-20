#' @importFrom grDevices col2rgb rgb
#' @importFrom graphics arrows axis barplot lines plot points polygon
#' @importFrom stats lm mad median runif sd t.test
#' @importFrom methods is
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#' @importFrom graphics par
#' @importFrom stats aggregate
#' @importFrom magrittr %>%
#' @importFrom stringr str_split
#' @importFrom utils tail
#' @import dipsaus
NULL

.missing_arg <- alist(x = )

as_call <- function(..., .list=list(), .drop_nulls = FALSE){
  call <- c(list(...), .list)
  if('...' %in% names(call)){
    call[['...']] <- NULL
    call[[length(call) + 1]] <- quote(...)
  }
  if (.drop_nulls) {
    call <- call[!vapply(call, is.null, FUN.VALUE = FALSE)]
  }
  as.call(call)
}
