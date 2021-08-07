# This script installs packages that will be cached (e.g. package imports suggests depends...)
# The goal is to install and cache (automatically) dependent libraries that do not update frequently

# available packages: remotes, drat (from dipterix/drat-1) and BiocManager

# IMPORTANT: this script only run once (results will be cached)

BiocManager::install('rhdf5', update = FALSE, type = 'source')
remotes::install_cran(c("roxygen2", "devtools", "dipsaus", "raveio", "threeBrain"), dependencies = TRUE, upgrade = 'never')

remotes::install_github(c(
  "beauchamplab/ravebuiltins@migrate2"
), dependencies = TRUE, upgrade = 'never')

