
get_xlim <- function(..., default = c(0,1)){
  re = get_dots('xlim', ...)
  re %?<-% get_dots('x', ...)
  re %?<-% default
  range(re, na.rm = TRUE)
}
get_ylim <- function(..., default = c(0,1)){
  re = get_dots('ylim', ...)
  re %?<-% get_dots('y', ...)
  re %?<-% default
  range(re, na.rm = TRUE)
}
new_plotlayer <- function(re = NULL){
  if(!inherits(re, 'fastmap2')){
    re <- fastmap2()
  }
  re$layers <- fastmap2()
  re$total_layers = 0
  re
}


decor_debug <- function(...){
  if(getOption('rutabaga.debug', FALSE)){
    cat2(paste(..., sep = '', collapse = ''), end = '\n', sep = '')
  }
}


extract_formula2 <- function(x){
  if(inherits(x, 'formula')){
    x[[2]]
  }else{
    substitute(x)
  }
}

decor_plot_clean <- function(
  xlab = '', ylab = '', main = '', axes = FALSE, type = 'n'
){
  warning('decor_plot_clean is soft-depricated')
  # static settings
  layer_name = 'decor_plot_clean'
  settings = list(type = type, axes = axes, xlab = xlab, ylab = ylab,
                  main = main)
  function(f = graphics::plot){
    decor_debug('Compile ', layer_name)
    function(...,
             cex.main = rave_cex.main, cex.axis = rave_cex.axis,
             cex.lab = rave_cex.lab){
      decor_debug('Running ', layer_name)
      # Dynamic settings
      call <- match.call()
      call[[1]] <- quote(f)
      call$cex.main = cex.main
      call$cex.axis = cex.axis
      call$cex.lab = cex.lab
      call$xlim <- get_xlim(...)
      call$ylim <- get_ylim(...)
      for(nm in names(settings)){
        call[[nm]] <- settings[[nm]]
      }
      decor_debug(deparse(call))
      res <- eval(call)
      res <- new_plotlayer(res)
      res$xlim <- call$xlim
      res$ylim <- call$ylim
      res$main <- settings$main
      res$xlab <- settings$xlab
      res$ylab <- settings$ylab
      res$cex.main <- cex.main
      res$cex.axis <- cex.axis
      res$cex.lab <- cex.lab
      res$total_layers <- res$total_layers + 1
      res$layers$names <- c(res$layers$names, layer_name)
      res$layers[[res$total_layers]] = list(
        name = layer_name,
        xlim = call$xlim,
        ylim = call$ylim,
        xlab = res$xlab, ylab = res$ylab, main = res$main,
        cex.main = cex.main, cex.axis = cex.axis,
        cex.lab = cex.lab,
        call = deparse(call)
      )
      decor_debug('Finished ', layer_name)
      invisible(res)
    }
  }
}


decor_ruta_axis <- function(
  side = 1, at, labels, las = 1, mgpy = c(3, 0.6, 0),
  mgpx = c(3, 0.75, 0), tcl = -0.3, ...
){
  warning('decor_ruta_axis is soft-depricated')
  layer_name = 'decor_ruta_axis'
  missing_at <- missing(at)
  missing_label <- missing(labels)
  more_args <- list(...)
  if (side%%2) {
    mgp <- mgpx
    get_lim <- get_xlim
  }else{
    mgp <- mgpy
    get_lim <- get_ylim
  }
  # fixed settings (global)
  function(f){
    decor_debug('Compile ', layer_name)
    function(..., cex.axis = rave_cex.axis){
      res <- eval(as_call(quote(f), quote(...), cex.axis = cex.axis))
      if( missing_at ){
        at <- get_lim(...)
      }
      if(missing_label){ labels <- at }

      call <- as_call(quote(graphics::axis),
                      side = side,
                      at = at, labels = labels,
                      tcl = tcl, mgp = mgp, cex.axis = cex.axis,
                      las = las,
                      .list = more_args)
      decor_debug(deparse(call))

      re <- eval(call)

      res <- new_plotlayer(res)
      res[[sprintf('ruta_axis_%d', side)]] = re
      res$cex.axis = cex.axis
      res$layers$names <- c(res$layers$names, layer_name)
      res$total_layers <- res$total_layers + 1
      res$layers[[res$total_layers]] = list(
        name = layer_name,
        side = side, at = at, labels = labels,
        tcl = tcl, mgp = mgp, cex.axis = cex.axis, las = las,
        call = deparse(call)
      )
      decor_debug('Finished ', layer_name)
      invisible(res)
    }
  }
}


decor_points <- function(pch = 16, type = 'p', jitter_x = 0){
  warning('decor_points is soft-depricated')
  layer_name = 'decor_points'
  function(f){
    decor_debug('Compile ', layer_name)
    function(x, y = x, ...){
      decor_debug('Running ', layer_name)
      res <- eval(as_call(quote(f), x = x, y = y, .list = list(...)))
      call <- as_call(
        quote(graphics::points),
        x = jitr(x, length(y), r=jitter_x),
        y = y,
        pch = pch, type = type,
        quote(...)
      )
      decor_debug(deparse(call))
      eval(call)
      res <- new_plotlayer(res)
      res$total_layers <- res$total_layers + 1
      res$layers$names <- c(res$layers$names, layer_name)
      res$layers[[res$total_layers]] = list(
        name = layer_name,
        pch = pch, type = type,
        call = call
      )
      decor_debug('Finished ', layer_name)
      invisible(res)
    }
  }
}

decor_formals <- function(kwargs = alist(), args = NULL, dots = TRUE){
  warning('decor_formals is soft-depricated')
  formals <- list()
  if(length(args)){
    for(nm in args){
      formals[[nm]] <- .missing_arg[[1]]
    }
  }
  if(length(kwargs)){
    for(nm in names(kwargs)){
      formals[[nm]] = kwargs[[nm]]
    }
  }
  if(dots){
    formals[['...']] <- .missing_arg[[1]]
  }
  function(f){
    re <- function(...){
      call <- match.call(definition = f)
      call[[1]] <- quote(f)
      decor_debug(deparse(call))
      eval(call)
    }

    formals(re) <- formals
    re
  }
}

decor_ebar <- function(sem, alpha=100, col='black', fill=col,
                       stroke=col, border = NA, add_line=TRUE, lwd=1, ...){
  layer_name <- 'decor_ebar'
  warning('decor_ebar is soft-depricated')
  force(sem)
  more_args <- list(...)
  force(more_args)
  function(f){
    decor_debug('Compile ', layer_name)
    function(...){
      decor_debug('Running ', layer_name)
      res <- f(...)

      x <- get_dots('x', ...)
      y <- get_dots('y', ...)
      if(!any(is.finite(sem))){
        sem <- get_dots('decor_ebar.sem', y * 0, ...)
      }
      is_finite = is.finite(y) & is.finite(sem)

      x = x[is_finite]
      y = y[is_finite]
      sem = sem[is_finite]

      sem = abs(sem)
      call1 <- quote(graphics::polygon(
        c(x, rev(x)), c(y + sem, rev(y - sem)),
        border = border, col = getAlphaRGB(fill, alpha)))
      decor_debug(deparse(call1))
      eval(call1)

      call2 <- ''
      if(add_line) {
        call2 <- as_call(quote(graphics::lines), x,y, col=stroke, lwd=lwd, .list = more_args)
        decor_debug(deparse(call2))
        eval(call2)
      }
      res <- new_plotlayer(res)
      res$layers$names <- c(res$layers$names, layer_name)
      res$total_layers <- res$total_layers + 1
      res$layers[[res$total_layers]] = list(
        name = layer_name,
        sem = sem,
        alpha=alpha, col=col, fill=fill,
        stroke=stroke, border = border, add_line=add_line, lwd=lwd,
        call = paste0(
          paste(deparse(call1), collapse = '\n'), '; ',
          paste(deparse(call2), collapse = '\n'))
      )
      decor_debug('Finished ', layer_name)
      invisible(res)
    }
  }
}



# plot_with_axis <- plot %D%
#   decor_plot_clean(xlab = 'Time(s)', ylab = 'Value', main = 'Title') %D%
#   decor_ruta_axis(side = 1) %D%
#   decor_ruta_axis(side = 2) %D%
#   # decor_points() %D%
#   decor_ebar(sem = rnorm(10)) %D%
#   decor_formals(args = c('x', 'y', 'decor_ebar.sem'))
# plot_with_axis
#
# x <- 1:10; y <- x + rnorm(length(x))
# res = plot_with_axis(x = x, y = y, ylim = c(0,10))
#
