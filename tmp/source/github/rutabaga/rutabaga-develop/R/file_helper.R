#' Create directory signature
#'
#' @param path directory path to generate signatures
#' @param file.info,md5sum,recursive,... passed to \code{\link{fileSnapshot}}
#' @export
dir_signature = function(path = '.', file.info = FALSE, md5sum = TRUE, recursive = TRUE, ...){
  path = normalizePath(path)
  info = utils::fileSnapshot(path = path, file.info = file.info, md5sum = md5sum, recursive = recursive, ...)
  digest = digest::digest(info$info)
  digest
}


#' Get the file name from a full file path
#' @param full_path path to file
#' @param keep_extension whether the file extension should be retained (defaults to FALSE)
#' @export
get_filename = function(full_path, keep_extension = FALSE) {
  name <- stringr::str_split(full_path, .Platform$file.sep) %>%
    unlist %>%
    tail(1)

  if (keep_extension)
    return(name)

 stringr::str_split(name, "\\.") %>%
    unlist %>%
    remove_tail %>%
    # because we split on '.' above, collapse back if needed
    paste(collapse='.')
}


#' Remove the last k elements from a vector (list)
#' Returns x (with a warning) if k < 1
#' @param x the vector
#' @param k the number of items to remove
#' @export
remove_tail = function(x, k=1) {

  if(k<1) {
    warning('Tried removing less than 1 element, k = ', k)
    return(x)
  }

  stopifnot(k>=1 & k<length(x))

  #seems like it's faster to rewrite as selecting from the beginning
  #rather than using negative indexing
  # x[-(length(x):(length(x) - (k-1)))]
  x[1:(length(x)-k)]
}
