
# ogbox

[![Build
Status](https://travis-ci.org/oganm/ogbox.svg?branch=master)](https://travis-ci.org/oganm/ogbox)[![codecov.io](https://codecov.io/github/oganm/ogbox/coverage.svg?branch=master)](https://codecov.io/github/oganm/ogbox?branch=master)

A multi-purpose R package primarily for personal use

## Installation

    devtools::install_github('oganm/ogbox')

## What’s in the box?

There are many poorly documented functions here. Below are some examples
separated by category.

### Developer tools

`getVersion`, `setVersion` and `setDate` functions allow manipulation of
DESCRIPTION files. I use them for auto updating packages.

`forkCRAN` allows you to create a github repository for a current or
older version of a cran package which can be followed up by
`install_github` or `Remotes` in your DESCRIPTION file if you really
need an older version of a package or want to make some changes yourself

### List unpacking

Taken from
[this](http://stackoverflow.com/questions/1826519/function-returning-more-than-one-value)
github answer ages ago, this syntax

``` r
list[a,b] = list(c(1,2,3),c(4,5,6))
a
```

    ## [1] 1 2 3

``` r
b
```

    ## [1] 4 5 6

allows unpacking of lists. These days `zeallot` package does that with
%\<-% operator.

### Remote code sourcing and data loading

  - `sourceGithub` runs R scripts or specific lines of R scripts from
    github.
  - `loadURL` and `loadGithub` loads remote rda files
  - `readRDSGithub` reads an RDS file from github
  - `getGithubFile`, also used by the functions above gets files from
    github.

### Gene Expression Omnibus tools

Didn’t want to deal with S4 classes from bioconductor packages so there
are a bunch of functions that deal with GEO datasets.

  - `gsmFind` picks samples that match a regex from a dataset
  - `whichGSE` finds the datasets that a sample belongs to
  - `gsmDown` downloads cel files for microarray samples
  - `gseDown` downloads all cel files for a microarray dataset
  - `softDown` downloads softfile for an experiment
  - `softParser` parses said soft file.
