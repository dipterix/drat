# This script installs packages that will be cached (e.g. package imports suggests depends...)
# The goal is to install and cache (automatically) dependent libraries that do not update frequently

# available packages: remotes, drat (from dipterix/drat-1) and BiocManager


if(!dipsaus::package_installed('rhdf5')){
  BiocManager::install('rhdf5', update = FALSE, type = 'source')
}

install_if_not_exists <- function(pkgs){
  sel <- sapply(pkgs, function(p){
    system.file(package = p) == ""
  })
  pkgs <- pkgs[sel]
  if(length(pkgs)){
    remotes::install_cran(pkgs, dependencies = TRUE, upgrade = 'never')
  }
}

install_if_not_exists(c("roxygen2", "devtools", "dipsaus", "raveio", "threeBrain"))

if(!dipsaus::package_installed('ravebuiltins')){
  remotes::install_github(c(
    "beauchamplab/ravebuiltins@migrate2"
  ), dependencies = TRUE, upgrade = 'never')
}

