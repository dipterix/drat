# nocov start


.onLoad <- function(libname, pkgname){
  pkg_env = asNamespace(pkgname)

  # S4 method is hard and so far this is the best way to do it
  # We detect if pkg is installed,
  # if so, import generic from pkg, add methods and export
  # if not, define generics, add methods and export
  register_generics <- function(pkg, generic, signature, fun = NULL){

    stopifnot(is.character(pkg), length(pkg) == 1)
    stopifnot(is.character(generic), length(generic) == 1)
    if (is.null(fun)) {
      fun <- get(generic, envir = parent.frame())
    } else if(is.character(fun)){
      fun <- get(fun, envir = parent.frame())
    } else {
      stopifnot(is.function(fun))
    }


    if(!is.list(signature)){
      signature = list(signature)
    }

    # Check if pkg has been installed
    if(system.file('', package = pkg) != ''){
      # installed, extend the definition
      register <- function(...){
        tmp_env = new.env()
        base::namespaceImportFrom(self = tmp_env, ns = pkg, generic)
        for(sig in signature){
          methods::setMethod(f = generic, signature = sig, definition = fun, where = tmp_env)
        }
        # Need to override the definition
        pkg_env[[generic]] = tmp_env[[generic]]
        base::namespaceExport(pkg_env, generic)
      }
    }else{
      register <- function(...){
        assign(generic, methods::makeGeneric(generic, fun), envir = pkg_env)

        for(sig in signature){
          methods::setMethod(f = generic, signature = sig, definition = fun, where = pkg_env)
        }
        base::namespaceExport(pkg_env, generic)
      }

    }

    register()
  }






  register_generics('Matrix', '%&%', signature = list(
    methods::signature(x = "character", y = 'ANY'),
    methods::signature(x = "ANY", y = 'character'),
    methods::signature(x = "character", y = 'character')
  ))



  invisible()
}


# S3 method is easy because I can dynamically update S3 table
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else if(is.character(fun)){
    fun <- get(fun, envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}



# nocov end
