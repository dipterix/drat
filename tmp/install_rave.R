# install.packages('drat')

drat::addRepo('dipterix', alturl = sprintf('file:%s', normalizePath('.')))
remove.packages(c('startup', 'ravebuiltins', 'rave', 'dipsaus', 'threeBrain', 'rutabaga'))
.rs.restartR("install.packages(c('Rcpp', 'rave', 'rutabaga', 'ravebuiltins'))")


