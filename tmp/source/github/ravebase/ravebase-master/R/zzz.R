.onLoad <- function(libname, pkgname){
  ns <- asNamespace(pkgname)
  # Assign global variables dynamically
  assign('rave_repos', dipsaus::fastmap2(), envir = ns)
}


.onUnload <- function(libpath){
  rave_repos <- get0('rave_repos', ifnotfound = NULL)
  if(inherits(rave_repos, 'fastmap2')){
    .subset2(rave_repos, 'reset')()
  }
}
