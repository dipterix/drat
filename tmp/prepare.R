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
binary_path <- file.path(proj_dir, 'tmp', os)
dir.create(binary_path, recursive = TRUE, showWarnings = FALSE)
dir.create(github_path, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(proj_dir, 'bin', os, 'contrib', rver), recursive = TRUE, showWarnings = FALSE)

dependencies <- list(
  'Rcpp' = list(
    url = 'https://github.com/RcppCore/drat/raw/gh-pages/src/contrib/',
    name = 'Rcpp_1.0.4.5.tar.gz',
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


