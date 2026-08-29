# Reads GRASS metadata from the current LOCATION

GRASS LOCATION metadata are read into a list in R; helper function
getLocationProj returns a WKT2 string of projection information. The
helper function `gmeta2grd` creates a GridTopology object from the
current GRASS mapset region definitions.

## Usage

``` r
gmeta(ignore.stderr = FALSE, g.proj_WKT = NULL)

getLocationProj(ignore.stderr = FALSE, g.proj_WKT = NULL)

gmeta2grd(ignore.stderr = FALSE)

# S3 method for class 'gmeta'
print(x, ...)

get.ignore.stderrOption()

get.stop_on_no_flags_parasOption()

get.echoCmdOption()

get.useInternOption()

get.legacyExecOption()

get.defaultFlagsOption()

get.suppressEchoCmdInFuncOption()

set.ignore.stderrOption(value)

set.stop_on_no_flags_parasOption(value)

set.echoCmdOption(value)

set.useInternOption(value)

set.legacyExecOption(value)

set.defaultFlagsOption(value)

set.suppressEchoCmdInFuncOption(value)
```

## Arguments

- ignore.stderr:

  default FALSE, can be set to TRUE to silence
  [`system()`](https://rdrr.io/r/base/system.html) output to standard
  error; does not apply on Windows platforms.

- g.proj_WKT:

  default NULL: return WKT2 representation in GRASS \>= 7.6 and Proj4 in
  GRASS \< 7.6; may be set to FALSE to return Proj4 for GRASS \>= 7.6.

- x:

  S3 object returned by gmeta

- ...:

  arguments passed through print method

- value:

  logical value for setting options on `ignore.stderr` set by default on
  package load to FALSE, `stop_on_no_flags_params` set by default on
  package load to TRUE, `echoCmd` set by default on package load to
  FALSE. `useIntern` sets the intern argument globally; `legacyExec`
  sets the legacyExec option globally, but is initialized to FALSE on
  unix systems (all but Windows) and TRUE on Windows; `defaultFlags` is
  initialized to NULL, but may be a character vector with values from
  c("quiet", "verbose") `suppressEchoCmdInFunc` default TRUE suppresses
  the effect of echoCmd within package functions, maybe set FALSE for
  debugging.

## Value

Returns list of g.gisenv, g.region -g3, and g.proj values.

## Author

Roger S. Bivand, e-mail: <Roger.Bivand@nhh.no>

## Examples

``` r
run <- FALSE
if (nchar(Sys.getenv("GISRC")) > 0 &&
    read.dcf(Sys.getenv("GISRC"))[1, "LOCATION_NAME"] == "nc_basic_spm_grass7") {
  run <- TRUE
}

if (run) {
  G <- gmeta()
  print(G)
}
#> Warning: cannot open file '/file1f7257de6bae': No such file or directory
#> Error in file(con, "r"): cannot open the connection

if (run) {
  cat(getLocationProj(), "\n")
  cat(getLocationProj(g.proj_WKT = FALSE), "\n")
}
#> Warning: cannot open file '/file1f7240b46691': No such file or directory
#> Warning: cannot open file '/file1f726a03ad7f': No such file or directory
#> Error in file(con, "r"): cannot open the connection

if (run) {
  grd <- gmeta2grd()
  print(grd)
}
#> Warning: cannot open file '/file1f726698597d': No such file or directory
#> Error in file(con, "r"): cannot open the connection

if (run) {
  ncells <- prod(slot(grd, "cells.dim"))
  df <- data.frame(k = rep(1, ncells))
  mask_SG <- sp::SpatialGridDataFrame(grd, data = df)
  print(summary(mask_SG))
}
#> Error: object 'grd' not found
```
