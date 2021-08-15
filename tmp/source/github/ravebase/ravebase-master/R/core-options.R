RAVE_OPT_NUMBER <- c('delay_input', 'image_width', 'image_height', 'drive_speed', 'max_worker', 'max_mem')
RAVE_OPT_LOGIC <- c('test_mode', 'fast_cache')

rave_options_gui <- function(...){ }

rave_options <- function(..., .save = TRUE, launch_gui = TRUE,
         host = '127.0.0.1', port = NULL){
  args = list(...)
  if(length(args) && length(names(args))){
    # set options
    for(nm in names(args)){
      raveio::raveio_setopt(nm, args[[nm]], .save = .save)
    }
  }else if (length(args)){
    # get options
    args = c(...)
    re <- sapply(args, function(nm){
      val <- raveio::raveio_getopt(nm, default = NULL)
      if(nm %in% RAVE_OPT_NUMBER){
        val <- as.numeric(val)
      } else if (nm %in% RAVE_OPT_LOGIC){
        val <- as.logical(val)
      }
      val
    }, simplify = FALSE, USE.NAMES = TRUE)
    if(length(re) == 1){
      re <- unlist(re)
    }
    return(re)
  } else if(launch_gui){
    return(rave_options_gui(host = host, port = port))
  }

  return(invisible())
}
