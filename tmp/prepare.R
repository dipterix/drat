# Script to add packages
os <- 'macosx/el-capitan'
rinfo <- R.Version()
major <- rinfo$major
minor <- strsplit(rinfo$minor, '\\.')[[1]][[1]]
rver <- sprintf('%s.%s', major, minor)
# install.packages('drat')

proj_dir <- normalizePath('.')
options(dratRepo = proj_dir)
drat::addRepo('dipterix', paste0('file:', proj_dir))

source_path <- 'tmp/source/'
github_path <- 'tmp/source/github'
download_path <- 'tmp/downloads'
binary_path <- file.path(proj_dir, 'tmp', os)
unlink(source_path, recursive = TRUE)
unlink(binary_path, recursive = TRUE)
dir.create(binary_path, recursive = TRUE, showWarnings = FALSE)
dir.create(github_path, recursive = TRUE, showWarnings = FALSE)
unlink(download_path, recursive = TRUE)
dir.create(download_path, recursive = TRUE, showWarnings = FALSE)

# Remove all source packages
# unlink('src/contrib/', recursive = TRUE)
dir.create('src/contrib/', recursive = TRUE, showWarnings = FALSE)
binary_target <- file.path(proj_dir, 'bin', os, 'contrib', rver)
# unlink(binary_target, recursive = TRUE)
dir.create(binary_target, recursive = TRUE, showWarnings = FALSE)


dependencies <- list(
  'Rcpp' = list(
    url = 'https://github.com/RcppCore/drat/raw/gh-pages/src/contrib/',
    name = 'Rcpp_1.0.4.8.tar.gz',
    type = 'source'
  ),
  'dipsaus' = list(
    url = 'https://github.com/dipterix/dipsaus/archive/master.zip',
    type = 'github'
  ),
  'threeBrain' = list(
    url = 'https://github.com/dipterix/threeBrain/archive/master.zip',
    type = 'github'
  ),
  'rutabaga' = list(
    url = 'https://github.com/dipterix/rutabaga/archive/develop.zip',
    type = 'github'
  ),
  'ravebuiltins' = list(
    url = 'https://github.com/beauchamplab/ravebuiltins/archive/migrate2.zip',
    type = 'github'
  ),
  'rave' = list(
    url = 'https://github.com/beauchamplab/rave/archive/dev-1.0.zip',
    type = 'github'
  ),
  'fstcore' = list(
    url = 'https://github.com/fstpackage/fstcore/archive/master.zip',
    type = 'github'
  )
)


# Online dependencies

cache_source <- function(pkg){
  info <- dependencies[[pkg]]
  destfile <- paste0(source_path, info$name)
  download.file(paste0(info$url, info$name), destfile)
  
}

cache_github <- function(pkg){
  info <- dependencies[[pkg]]
  tdf <- file.path(github_path, paste0(pkg, '.zip'))
  junkdir <- file.path(github_path, pkg)
  utils::download.file(info$url, tdf)
  unzip(tdf, exdir = junkdir)
  
  pkgdir <- list.dirs(junkdir, full.names = FALSE, recursive = FALSE)
  if(length(pkgdir) > 1){
    pkgdir = pkgdir[grepl(sprintf('^%s', pkg), pkgdir)][[1]]
  }
  pkgdir = file.path(junkdir, pkgdir)
  
  # if configure exists, make it executable
  configure <- file.path(pkgdir, 'configure')
  if(file.exists(configure)){
    system(sprintf('chmod a+x "%s"', configure))
  }
  
  devtools::build(pkg = pkgdir, path = source_path, binary = FALSE, vignettes = TRUE, manual = TRUE)
}

for(pkg in names(dependencies)){
  cat('Caching -', pkg, '\n')
  
  info <- dependencies[[pkg]]
  f <- paste0('cache_', info$type)
  do.call(f, list(pkg))
  
}

source_packages <- list.files(source_path, pattern = 'gz$', full.names = TRUE)
for(destfile in source_packages){
  devtools::build(destfile, path = binary_path, binary = TRUE)
  drat::insertPackage(destfile)
}

binary_packages <- list.files(binary_path, pattern = 'gz$', full.names = TRUE)
for(destfile in binary_packages){
  print(destfile)
  drat::insertPackage(destfile)
}


# WARNING: Will download all dependent packages


# CRAN packages
cran_packages <- c("abind", "askpass", "assertthat", "backports", "base64enc", "base64url", "BH", "bigmemory.sri", "bit", "bit64", "bitops", "boot", "brew", "callr", "car", "carData", "cellranger", "circular", "class", "cli", "clipr", "clisymbols", "cluster", "codetools", "colorspace", "commonmark", "covr", "crayon", "crosstalk", "curl", "data.table", "desc", "devtools", "digest", "downloader", "DT", "ellipsis", "emmeans", "estimability",  "evaluate", "fansi", "farver", "fastmap", "fftwtools", "filelock", "forcats", "foreign", "freesurferformats", "fs", "fst", "future", "future.apply", "future.callr", "ggplot2", "gh", "gifti", "git2r", "globals", "glue", "grid", "gtable", "haven", "hdf5r", "highr", "hms", "htmltools", "htmlwidgets", "httpuv", "httr", "ini", "isoband", "jsonlite", "KernSmooth", "knitr", "labeling", "later", "lattice", "lazyeval", "lifecycle", "listenv", "lme4", "lmerTest", "lsmeans", "magrittr",  "maptools", "markdown", "Matrix", "MatrixModels", "memoise", "mgcv", "microbenchmark", "mime", "minqa", "munsell", "nlme", "nloptr", "nnet", "numDeriv", "openssl", "openxlsx", "oro.nifti", "pbkrtest", "pillar", "pkgbuild", "pkgconfig", "pkgfilecache", "pkgload", "plyr", "praise", "prettyunits", "processx", "progress", "progressr", "promises", "pryr", "ps", "purrr", "qs", "R.matlab", "R.methodsS3", "R.oo", "R.utils", "R6", "RApiSerialize", "rappdirs", "rcmdcheck",  "RColorBrewer", "RcppEigen", "RcppParallel", "RcppRedis", "readr", "readxl", "rematch", "remotes", "reshape2", "reticulate", "rex", "rio", "rlang", "rmarkdown", "RNifti", "roxygen2", "rpart", "rprojroot", "rstudioapi", "rversions", "scales", "servr", "sessioninfo", "shiny", "shinydashboard", "shinyFiles", "shinyjs", "signal", "sourcetools", "sp", "SparseM", "spatial", "startup", "stringi", "stringr", "survival", "synchronicity", "sys", "testthat", "tibble", "tidyselect", "tinytex", "txtq", "usethis", "utf8", "uuid", "vctrs", "viridisLite", "whisker", "withr", "xfun", "xml2", "xopen", "xtable", "yaml", "zip")



download.packages(cran_packages, repos = "https://cran.rstudio.com/", 
                  type = 'source', destdir = download_path)

# Add to repo
cran_extras <- list.files(download_path, pattern = 'gz$', full.names = TRUE)
for(destfile in cran_extras){
  drat::insertPackage(destfile)
}

# Remove temporary download folder
unlink(download_path, recursive = TRUE)
dir.create(download_path, recursive = TRUE, showWarnings = FALSE)

# Download compiled code (work for macosx and windows)
download.packages(cran_packages, repos = "https://cran.rstudio.com/", 
                  type = 'binary', destdir = download_path)
cran_extras <- list.files(download_path, pattern = 'gz$', full.names = TRUE)
for(destfile in cran_extras){
  drat::insertPackage(destfile)
}

unlink(download_path, recursive = TRUE)
