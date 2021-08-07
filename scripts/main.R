# Script to add packages
proj_dir <- normalizePath('.')
repo_dir <- file.path(proj_dir, get0("repo_dname", ifnotfound = ""))

# https://cran.rstudio.com/src/contrib
rinfo <- R.Version()
major <- rinfo$major
minor <- strsplit(rinfo$minor, '\\.')[[1]][[1]]
rver <- sprintf('%s.%s', major, minor)
# install.packages('drat')

dir.create(repo_dir, showWarnings = FALSE, recursive = TRUE)
options(dratRepo = repo_dir)
drat::addRepo('dipterix', paste0('file:', repo_dir))

if(!file.exists(file.path(repo_dir, 'index.html'))){
  writeLines("<!doctype html><title>empty</title>", file.path(repo_dir, 'index.html'))
}

source_path <- './tmp/source/'
github_path <- './tmp/source/github'
download_path <- './tmp/downloads'
unlink(source_path, recursive = TRUE)
unlink(download_path, recursive = TRUE)
# dir.create(binary_path, recursive = TRUE, showWarnings = FALSE)
dir.create(github_path, recursive = TRUE, showWarnings = FALSE)
dir.create(download_path, recursive = TRUE, showWarnings = FALSE)



source("https://raw.githubusercontent.com/dipterix/drat/master/dependencies.R")


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

# Build source packages, and prune old packages
source_packages <- list.files(source_path, pattern = 'gz$', full.names = TRUE)
for(destfile in source_packages){
  drat::insertPackage(destfile, action = "prune")
}

# Get source path
source_contrib <- drat:::contrib.url2(repo_dir, 'source')
source_contrib <- normalizePath(source_contrib, mustWork = TRUE)
source_packages <- list.files(source_contrib, pattern = "tar\\.gz$", full.names = TRUE)



for(destfile in source_packages){
  f <- tempfile()
  if(dir.exists(f)){ unlink(f, recursive = TRUE) }
  dir.create(f, recursive = TRUE, showWarnings = FALSE)
  devtools::build(destfile, path = f, binary = TRUE)
  bin_file <- list.files(f, full.names = TRUE)
  
  pkginfo <- drat:::getPackageInfo(bin_file)
  pkgtype <- drat:::identifyPackageType(bin_file, pkginfo)
  pkgdir <- normalizePath(drat:::contrib.url2(repo_dir, pkgtype, pkginfo["Rmajor"]),
                          mustWork = FALSE)
  if(!dir.exists(pkgdir)){
    dir.create(pkgdir, recursive = TRUE, showWarnings = FALSE)
  }
  
  drat::insertPackage(bin_file, action = 'prune')
}

# clean
unlink(source_path, recursive = TRUE)
unlink(download_path, recursive = TRUE)

