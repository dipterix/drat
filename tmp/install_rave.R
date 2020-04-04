# install.packages('drat')

remove.packages(c('startup', 'ravebuiltins', 'rave', 'dipsaus', 'threeBrain', 'rutabaga'))

cmd <- c(
  "drat::addRepo('dipterix', alturl = sprintf('file:%s', normalizePath('.')))",
  "install.packages(c('Rcpp', 'rave', 'rutabaga', 'ravebuiltins'))"
)

.rs.restartR(paste(cmd, collapse = ';'))


