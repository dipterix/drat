# Plot helpers

# This file defines util functions for plotting figures

# ------------------------------------------------------------------------
#
# I'm thinking to put some constants here so that we get a general look/feel on our plots
rave_cex.main <- 1.5
rave_cex.axis <- 1.3
# putting this to 1.4 because 1.5 causes some clipping of the axis(2) label, we could also try to increase
# the left margin to compensate
rave_cex.lab <- 1.4
# ------------------------------------------------------------------------










#' @title Create A Blank Plot With Given X And Y Range
#'
#' (stable)
#'
#' @param xlim numeric vector
#' @param ylim numeric vector
#' @param x x
#' @param y y
#' @param type default is 'n', i.e. plot nothing. See \code{\link{plot.default}}
#' @param xlab x label
#' @param ylab y label
#' @param cex.main title font size
#' @param cex.axis axis ticks font size
#' @param cex.lab axis label font size
#' @param ... other params passed to \code{plot}
#'
#' @examples
#' \dontrun{
#' # create a blank plot with x from 0 to 10 and y from 1 to 5
#' plot_clean(0:10, 1:5, xlab = 'X')
#' }
#' @export
plot_clean <- function(
  xlim, ylim, x = 1, y = 1, type = "n", xlab="", ylab="",
  cex.main=rave_cex.main, cex.axis=rave_cex.axis, cex.lab=rave_cex.lab,...
) {

  plot(x, y, type = type, axes = F, ylab = ylab, xlab = xlab,
       xlim = range(xlim), ylim = range(ylim),
       cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab, ...)
}


















#' @title Show A Blank Plot With Messages
#'
#' (maturing)
#'
#' @param main the title/msg to show
#'
#' @export
#' @examples
#' \dontrun{
#' plot_msg("Let's Say Something")
#' }
#' @export
plot_msg <- function(main = 'No Conditions Specified') {
  plot_clean(1, 1, type='n', main=main)
}
















#' @title A Neat Way To Show Axis
#'
#' (stable)
#'
#' @param side 1 to 4: bottom, left, up, right. See \code{\link{axis}}
#' @param at,tcl,labels,las,mgpy,mgpx,... passed to \code{\link{axis}}
#' @param cex.axis tick size
#' @param cex.lab label size
#'
#' @examples
#' \dontrun{
#' # create a blank plot with x from 0 to 10 and y from 1 to 5
#' plot_clean(0:10, 1:5, xlab = 'X')
#' ruta_axis(side = 1, at = 1:8)
#' }
#' @export
ruta_axis <- function(
  side, at, tcl=-0.3, labels=at, las=1, cex.axis=rave_cex.axis,
  cex.lab=rave_cex.lab, mgpy=c(3, .6, 0), mgpx=c(3, .75, 0), ...) {
  if(length(side) > 1) {
    return (invisible(sapply(
      side, ruta_axis, at=at, tcl=tcl, labels=labels,
      cex.axis=cex.axis, las=las, cex.lab=cex.lab, ...)
    ))
  }
  mgp <- mgpy
  if(side %% 2) mgp <- mgpx

  invisible(as.matrix(axis(side, at=at, labels=labels, tcl=tcl, mgp=mgp,
                           cex.axis=cex.axis, las=las, cex.lab=cex.lab, ...)))
}


#' @title Draw Symmetric Error Bars
#'
#' @param x,y data to plot
#' @param sem error range (half)
#' @param length line length
#' @param type type of xy data see \link{points}
#' @param col default color for points and error areas
#' @param pt.col point colors
#' @param code arrow code
#' @param ... other params passed to ebars.y
#' @examples
#' \dontrun{
#' plot_clean(0:10, -1:5, xlab = 'X')
#' ruta_axis(side = 2, at = -1:8)
#' ebars(x = c(2, 4), y = c(0, 3), sem = c(1, 0.5), col = c(2, 3))
#' }
#' @export
ebars <- function(x, y=NULL, sem=NULL, length = 0.05, type='n', col='black', pt.col=col, code=2, ...) {
  if(is.null(y)) {
    if(is.matrix(x)) {
      y <- x[,1]
      sem <- x[,2]
    } else {
      y <- x
    }
    x <- seq_along(y)
  }

  if(is.matrix(y)) {
    sem <- y[,2]
    y <- y[,1]
  }

  if(is.null(sem)) {
    sem <- y
    y <- x
    x <- seq_along(y)
  }

  ebars.y(x, y, sem, length, code=code, col=col, ...)
  points(x, y, type=type, col=pt.col, ...)
}


ebars.x <- function(x, y, sem, length = 0.05, ...) {
  arrows(x - sem, y, x + sem, y, angle = 90, code = 3, length = length, ...)
}

ebars.y <- function(x, y, sem, length = 0.05, up = T, down = T, code = 2, ...) {
  if (up) {
    arrows(x0 = x, y0 = as.numeric(y), y1 = as.numeric(y + sem), angle = 90, code = code, length = length, ...)
  }
  if (down) {
    arrows(x0 = x, y0 = as.numeric(y), y1 = as.numeric(y - sem), angle = 90, code = code, length = length, ...)
  }
}

#' Polygon plot
#' @param x,y x and y
#' @param col color
#' @param alpha 0-255 transparency
#' @param border border of polygon
#' @param ... passed to polygon
#' @export
do_poly <- function(x, y, col, alpha=50, border=NA, ...) {
  polygon(c(x,rev(x)), rep(y, each=2), col=getAlphaRGB(col, alpha), border=border, ...)
}

#' @title Draw Symmetric Error Bars
#'
#' (stable)
#'
#' @param x,y plot data
#' @param sem error bar half width
#' @param alpha transparency
#' @param col color
#' @param fill error bar color
#' @param stroke if add lines, line color
#' @param border errorbar border width
#' @param add_line add line(x,y) or not
#' @param lwd line weight
#' @param ... passed to \code{\link{lines}}
#'
#'
#' @examples
#' \dontrun{
#' plot_clean(0:10, -1:5, xlab = 'X')
#' ruta_axis(side = 2, at = -1:8)
#' ebar_polygon(1:10, (1:10)/2, rnorm(10))
#' }
#' @export
ebar_polygon <- function(x, y, sem, alpha=100, col='black', fill=col,
                        stroke=col, border = NA, add_line=TRUE, lwd=1, ...) {
  is_finite = is.finite(y) & is.finite(sem)

  # if all the SEMs are non finite, then we should still just draw a line, be helpful!
  if(all(!is.finite(sem))) {
    is_finite <- is.finite(y)
    sem <- 0*y
  }
  x = x[is_finite]
  y = y[is_finite]
  sem = sem[is_finite]

  sem = abs(sem)
  polygon(c(x, rev(x)), c(y + sem, rev(y - sem)), border = border, col = getAlphaRGB(fill, alpha))

  if(add_line) lines(x,y, col=stroke, lwd=lwd, ...)
}







#' @title Get Hex Color With Transparency
#'
#' @param colname name or number of color
#' @param alpha transparency
#'
#' @examples
#' \dontrun{
#' getAlphaRGB('red', 0.5)
#' }
#' @export
getAlphaRGB <- function(colname, alpha) {
  c = col2rgb(colname)
  rgb(t(c), alpha = alpha, maxColorValue = 255)
}





#' @title Get Elements/Slot/Attributes From List
#'
#' (stable)
#'
#' @param ll list of elements
#' @param name attribute/name/slot to extract
#' @param drop_nulls drop NULL results
#' @param is_attr are we extracting attributes?
#' @param use_sapply try to return a matrix/vector if possible?
#' @param ... if is_attr, additional params to \code{\link{attr}}
#'
#' @export
get_list_elements <- function(ll, name, drop_nulls = TRUE, is_attr = FALSE, use_sapply = TRUE, ...) {
  if(use_sapply){
    lapply = sapply
  }
  if(is_attr){
    l = lapply(ll, attr, which = name, ...)
  }else{
    l = lapply(ll, getElement, name)
  }

  # remove NULLs
  if(drop_nulls){
    is_v = !vapply(l, is.null, FUN.VALUE = FALSE)
    idx = which(is_v)
    l = l[idx]
    attr(l, 'original_index') = idx
  }
  if(!length(l)){
    l = NULL
  }
  l
}


abs_cdiff <- function(m) {
  if(!is.matrix(m))
    m = t(as.matrix(m))

  abs(apply(m, 1, diff))
}

#' @title Get A Integer Interval That Contains X
#'
#' (maturing)
#'
#' @param x vector/matrix, numeric
#'
#' @examples
#' \dontrun{
#' # 0 - 11
#' round_range(0.5:10.5)
#' }
#'
#' @export
round_range <- function(x) {
  c(floor(min(x)), ceiling(max(x)))
}






#' @title Get Data Range From A Collection Of Named Lists
#'
#' (questioning)
#'
#' @param ll list
#' @param name element name
#' @param ... additional params for \link{get_list_elements}
#' @export
get_data_range <- function(ll, name='range', ...) {
  re = get_list_elements(ll, name = name, ...)
  range(
    unlist(re),
    na.rm=TRUE
  )
}

#' @title Barplot Function That Uses All The Rave Sizes And Colors
#'
#' (stable)
#'
#' @param height,cex.axis,cex.lab,cex.names,... passed to \link{barplot} but the default values are changed
#'
#' @export
rave_barplot <- function(height, cex.axis=rave_cex.axis, cex.lab=rave_cex.lab, cex.names=rave_cex.lab, ...) {
  barplot(height, cex.axis=cex.axis, cex.lab=cex.lab, cex.names=cex.names, las=1, ...)
}






#' @title Return Jittered X
#'
#' (experimental)
#'
#' @param x data
#' @param len length of x
#' @param r jitter size
#'
#' @export
jitr <- function(x, len=length(x), r) {
  if(missing(r)){
    r = (1/3)*min(abs_cdiff(sort(unique(x))))
  }
  x + runif(len, -r, r)
}










#' @title Same As Points, But Can Be Jittered
#'
#' (maturing)
#'
#' @param x,y plot data
#' @param jitr_x jitter shift size for x
#' @param pch data point type, default is 19, See \code{\link{points}}
#' @param ... other params to \code{\link{jitr}}
#' @export
add_points <- function(x, y, jitr_x=0, pch=19, ...) {
  points(jitr(x, length(y), r=jitr_x), y, pch=pch, ...)
}












#' @title Ensure Data Are Within Some Bounds
#'
#' (stable)
#'
#' @param x data
#' @param lim clip range, length of 1 or more. If length(lim) is 1, then the clip is symmetric
#' @export
clip_x <- function(x, lim) {
  if(length(lim) == 1){
    lim = c(lim, -lim)
  }
  x[x<min(lim)] <- min(lim)
  x[x>max(lim)] <- max(lim)

  x
}

#' @title Useful For Plotting When You Want To Go A Bit Beyond The Data
#'
#' (experimental)
#'
#' @param x data
#' @param pct stretch percentage
#' @export
stretch <- function(x, pct) {
  d <- pct * diff(range(x))
  c(min(x)-d, max(x)+d)
}


#' @title Function for repeatedly writing plots to PDFs
#' @param PLOT a function that produces a plot
#' @param w the requested width of the PDF
#' @param h the requested height of the PDF
#' @param mar the margins of the PDF, set by a call to par(mar=mar). Defaults to c(1,1,1,1)
#' @return A function that takens optional arguments to the PLOT function and fname, the name of the PDF. One-off changes to w, h, and mar, are specified with width, height, and margin, respectively
#' @export
to_pdf = function(PLOT, w, h, mar=rep(1,4)) {
  return(function(..., fname, width = w, height = h, margin=mar) {
    on.exit(dev.off())

    fname <- fix_pdf_name(fname)
    pdf(fname, width = width, height = height, useDingbats=FALSE)
    par(mar=margin)
    PLOT(...)
  })
}

#' @title Ensure that the file names ends in ".pdf"
#' @param fname potential file name
#' @export
fix_pdf_name <- function(fname) {
  if(!grepl("\\.pdf$", fname)) {
    fname = paste0(fname, ".pdf")
  }
  return(fname)
}

#' @title pdf wrapper that evaluates an arbitrary expression.
#' @param fname file path
#' @param w the requested width of the PDF
#' @param h the requested height of the PDF
#' @param expr the expression to evaluate to produce the plot
#' @param bg set the background color of the PDF, defaults to 'white'
#' @param TEST if FALSE (the default) print to PDF, otherwise print to the current graphics device
#' @return the output of the resulting expression (the plot is likely produced by side effect but may additionally 'return' a value)
#'
#' @export
as_pdf <- function(fname, w, h, expr, TEST=FALSE, bg='white') {
  if(! TEST) {
    on.exit(dev.off())

    fname <- fix_pdf_name(fname)
    pdf(fname, width=w, height=h, useDingbats=FALSE, bg=bg)
    res = eval(expr)
  } else {
    res = eval(expr)
  }
  return (invisible(res))
}


call_or_function <- function(expr){
  if(!length(expr)){ return(TRUE) }
  tmp <- as.list(expr)
  if(length(tmp) > 1){
    expr <- tmp
  } else {
    expr <- list(expr)
  }
  while (length(expr) >= 2 && (expr[[1]] == "{" || expr[[1]] == "(")) {
    expr <- expr[-1]
    if(length(expr) == 1){
      tmp <- as.list(expr[[1]])
      if(length(tmp) > 1){
        expr <- tmp
      }
    }
  }
  if( expr[[1]] == "function" ){ return(FALSE) }
  if(length(expr) > 1){
    return(TRUE)
  }
  expr <- expr[[1]]
  if( is.symbol(expr) ){
    return(FALSE)
  }
  return(TRUE)
}


#' @title add decoration (frames) to plots based on their layout location
#' @export
create_frames <- function(
  layout, common = NULL, bottom = NULL, left = NULL, top = NULL,
  right = NULL, env = parent.frame(), exclude = NULL) {

  if(!is.matrix(layout) && length(layout) == 2){
    layout <- matrix(seq_len(prod(layout)),
      nrow = layout[[1]], byrow = TRUE)
  }
  if(length(exclude)){
    layout[layout %in% exclude] <- NA
  }

  expr <- substitute(common)
  if(!call_or_function(expr)){ expr <- eval(expr) }
  common <- expr

  expr <- substitute(bottom)
  if(!call_or_function(expr)){ expr <- eval(expr) }
  bottom <- expr

  expr <- substitute(left)
  if(!call_or_function(expr)){ expr <- eval(expr) }
  left <- expr

  expr <- substitute(top)
  if(!call_or_function(expr)){ expr <- eval(expr) }
  top <- expr

  expr <- substitute(right)
  if(!call_or_function(expr)){ expr <- eval(expr) }
  right <- expr

  force(env)

  # is_bottom <-  is_left <-  is_top <-  is_right <- FALSE
  suppressWarnings({
    bottom_counts <- apply(layout, 2, max, na.rm = TRUE)
    left_counts <- apply(layout, 1, min, na.rm = TRUE)
    top_counts <- apply(layout, 2, min, na.rm = TRUE)
    right_counts <- apply(layout, 1, max, na.rm = TRUE)
  })


  count <- 1

  list(
    cur_count = function(){ count },

    #the ... is passed to functions
    add_frame = function(skip = FALSE, force = NULL, ...){
      eval_or_call <- function(.x, envir) {
        if(is.function(.x)) {
          do.call(match.fun(.x), list(...), envir =envir)
        } else {
          eval(.x, envir = envir)
        }
      }

      if(skip || !count %in% layout){
        count <<- count + 1
        # if we've gone past the max frame, then recycle
        if(count > max(layout)) {
          count <<- 1
        }
        return(invisible())
      }
      eval_or_call(common, envir=env)

      # check if count (current figure is the bottom)
      is_bottom <- count %in% bottom_counts || 1 %in% force
      if(is_bottom){
        eval_or_call(bottom, envir = env)
      }

      is_left <- count %in% left_counts || 2 %in% force
      if(is_left){
        eval_or_call(left, envir = env)
      }

      is_top <- count %in% top_counts || 3 %in% force
      if(is_top){
        eval_or_call(top, envir = env)
      }

      is_right <- count %in% right_counts || 4 %in% force
      if(is_right){
        eval_or_call(right, envir = env)
      }

      count <<- count + 1

      # if we've gone past the max frame, then recycle
      if(count > max(layout)) {
        count <<- 1
      }

      invisible(c(is_bottom, is_left, is_top, is_right))
    }
  )
}
