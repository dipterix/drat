# Dipterix’s R Package Repository

This is my personal [R package repository](https://dipterix.github.io/drat/). It contain the following R packages that are compiled locally.

* [dipsaus](https://github.com/dipterix/dipsaus)
* [threeBrain](https://github.com/dipterix/threeBrain)
* [rutabaga](https://github.com/dipterix/rutabaga/tree/develop)
* [ravebase](https://github.com/dipterix/ravebase)
* [rave](https://github.com/beauchamplab/rave)
* [raveio](https://github.com/beauchamplab/raveio)
* [ravebuiltins](https://github.com/beauchamplab/ravebuiltins/tree/migrate2)

To install any of these packages, use

```r
install.packages("pkg-name", repos = "https://dipterix.github.io/drat")
```

The compiled packages are automatically generated from Github Action (see [configurations](https://github.com/dipterix/drat/blob/master/.github/workflows/drat--insert-package.yaml) or [workflows](https://github.com/dipterix/drat/actions))
