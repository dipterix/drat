# # # Formatters for statistics


#' helper function to build value labels
#'
#' (questioning)
#'
#' @param nm statistics
#' @param stats names of statistics
#'
#' @export
format_stat <- function(nm, stats=c('b', 't', 'p')) {
  sapply(stats, function(stat) sprintf('%s(%s)', stat, nm), USE.NAMES = FALSE)
}

set_names <- .Primitive("names<-")
set_class <- .Primitive("class<-")

#' Get statistics from linear model
#'
#' (questioning)
#'
#' @rdname lm-statistics
#'
#' @param formula,... passed to \link{lm}
#' @export
get_f <- function(formula, ...) {
  format_f(lm(formula, ...))
}

#' @rdname lm-statistics
#'
#' @param lm.mod linear model returned by lm()
#' @param test_name name
#' @export
format_f <-  function(lm.mod, test_name='All') {
  nms <- sapply(c('Rsq(%s)', 'F(%s)', 'p(%s)'), sprintf, test_name)

  re = with(summary(lm.mod), {
    c(r.squared, fstatistic[1],
      pf(fstatistic[1], fstatistic[2], fstatistic[3], lower.tail=FALSE))
  })
  re = set_names(re, nms)
  set_class(re, 'fres')
  re
}

#' @noRd
#' @param precision precisions for numbers
#' @export
pretty.fres <- function(x, precision = 4, ...) {
  if(is.character(x)){
    return(x)
  }
  # don't save intermediate results back into fres or else it changes the type into character,
  # messing up following lines
  fmt = sprintf('%%.%dg', precision)
  re = sprintf(fmt, x)
  set_class(re, c('fres', 'character'))
  re
}

#' helper function for t-tests that returns the values wanted by format_stat
#'
#' (questioning)
#'
#' @param ... passed to \link{t.test}
#' @rdname t-test-statistics
#' @export
get_t <- function(...){
  re = with(t.test(...), c(estimate, statistic, p.value))
  set_class(re, 'tres')
}

#' @noRd
#' @param ... ignored
#'
#' @export
pretty.tres <- function(x, ...) {
  if(is.character(x)){
    return(x)
  }
  re = mapply(format, x, digits=c(2,2,1))
  set_names(re, c('m', 't', 'p'))
  set_class(re, c('tres', 'character'))
  re
}

#' Make nice plot titles
#'
#' @param x title content
#' @param ... pass to other methods
#'
#' @examples
#' \dontrun{
#' # Display normal title
#' plot_clean(1,1, main = as_title('this is title'))
#'
#' # Only capitalize the first character
#' plot_clean(1,1, main = as_title('this is title', capitalize_all = F))
#'
#' # What if title is a formula
#' plot_clean(1,1, main = as_title(p[value](beta[1]) < ~.(0.01) ~ ' is significant'))
#'
#' # Display t-statistics
#' plot_clean(1,1)
#' t_stat = get_t(rnorm(10))
#' title(main=as_title(t_stat))
#' }
#' @export
as_title <- function(x, ...) {
  UseMethod('as_title')
}


#' @rdname as_title
#' @export
as_title.formula <- function(x, ...){
  expr = eval(substitute(x))

  do.call(bquote, list(expr = expr))
}

#' @rdname as_title
#' @param capitalize_all make the first letter cap
#' @param excluded exclude making first letter cap, numeric vector
#' @export
as_title.default <- function(
  x, capitalize_all = TRUE, excluded = c(
  'is', 'are', 'vs', 'v.s.', 'from', 'of', 'be', 'for', 'over'
  ), ...){
  if(!capitalize_all){
    if(grepl('^[a-z]', x)){
      y = stringr::str_sub(x, end = 1)
      stringr::str_sub(x, end = 1) = toupper(y)
    }
    return(x)
  }

  x = stringr::str_split(x, ' ')
  x = unlist(x)
  x = stringr::str_trim(x)
  x = x[!x == '']
  sel = !x %in% excluded
  if(any(sel)){
    x[sel] = stringr::str_to_title(x[sel])
  }
  paste(x, collapse = ' ')
}

#' @rdname as_title
#' @export
as_title.fres <- function(x, ...) {
  res = pretty.fres(x, ...)
  bquote(H[0] ~ mu[i] == mu[j] * ';' ~ R^2 == .(res[1]) ~ ',' ~ F == .(res[2]) * ','~ p==.(res[3]))
}

#' @rdname as_title
#' @export
as_title.tres <- function(x,...) {
  res = pretty.tres(x, ...)
  bquote(H[0] * ':' ~ mu == 0 * ';' ~ bar(x)==.(res[1]) * ',' ~ t == .(res[2]) * ',' ~ p==.(res[3]))
}

#' @title Convert vector into comma-separated string
#' @param x the vector to collapse
#' @param by the separating token (default is ', ')
#' @param ... further arguments for paste0
#' @return a character vector as produced by paste0
#' @export
str_collapse <- function(x, by=', ', ...) {
  paste0(x, collapse=by, ...)
}
