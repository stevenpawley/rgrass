# Read and write GRASS vector object files

`read_VECT` moves one GRASS vector object file with attribute data
through a temporary GeoPackage file to a
[`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
object; `write_VECT` moves a
[`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
object through a temporary GeoPackage file to a GRASS vector object
file. `vect2neigh` returns neighbour pairs with shared boundary length
as described by Markus Neteler, in
<https://stat.ethz.ch/pipermail/r-sig-geo/2005-October/000616.html>.
`cygwin_clean_temp` can be called to try to clean the GRASS
mapset-specific temporary directory under cygwin.

## Usage

``` r
read_VECT(
  vname,
  layer = "",
  proxy = FALSE,
  use_gdal_grass_driver = TRUE,
  type = NULL,
  flags = "overwrite",
  Sys_ignore.stdout = FALSE,
  ignore.stderr = get.ignore.stderrOption()
)

write_VECT(
  x,
  vname,
  flags = "overwrite",
  ignore.stderr = get.ignore.stderrOption()
)

vInfo(vname, layer, ignore.stderr = NULL)

vColumns(vname, layer, ignore.stderr = NULL)

vDataCount(vname, layer, ignore.stderr = NULL)

vect2neigh(
  vname,
  ID = NULL,
  ignore.stderr = NULL,
  remove = TRUE,
  vname2 = NULL,
  units = "k"
)
```

## Arguments

- vname:

  A GRASS vector file name.

- layer:

  a layer name (string); if missing the first layer will be used.

- proxy:

  Default is `FALSE`. Set as `TRUE` if you need a `SpatVectorProxy`
  object.

- use_gdal_grass_driver:

  Default `TRUE`. The [standalone GDAL-GRASS
  driver](https://github.com/OSGeo/gdal-grass) for the vector format
  will be used if it is installed. The advantage is that no intermediate
  file needs to be written from GRASS GIS and subsequently read into R;
  instead the vector layer is read directly from the GRASS GIS database.
  Please read the **Note** further below!.

- type:

  override type detection when multiple types are non-zero, passed to
  v.out.ogr.

- flags:

  Character vector containing additional optional flags and/or options
  for v.in.ogr, particularly "o" and "overwrite".

- Sys_ignore.stdout:

  Passed to `system`.

- ignore.stderr:

  default the value set by `set.ignore.stderrOption`; NULL, taking the
  value set by `set.ignore.stderrOption`, can be set to TRUE to silence
  [`system()`](https://rdrr.io/r/base/system.html) output to standard
  error; does not apply on Windows platforms.

- x:

  A `SpatVector` object moved to GRASS.

- ID:

  A valid DB column name for unique identifiers (optional).

- remove:

  default TRUE, remove copied vectors created in `vect2neigh`.

- vname2:

  If on a previous run, remove was FALSE, the name of the temporary
  vector may be given to circumvent its generation.

- units:

  default "k"; see GRASS 'v.to.db' manual page for alternatives.

## Value

`read_VECT` imports a GRASS vector layer into a `SpatVector` or
`SpatVectorProxy` object. `vect2neigh` returns a data frame object with
left and right neighbours and boundary lengths, also given class
GRASSneigh and spatial.neighbour (as used in spdep). The incantation to
retrieve the neighbours list is `sn2listw(vect2neigh())$neighbours`, and
to retrieve the boundary lengths: `sn2listw(vect2neigh())$weights`. The
GRASSneigh object has two other useful attributes: external is a vector
giving the length of shared boundary between each polygon and the
external area, and total giving each polygon's total boundary length.

## Note

Be aware that the GDAL-GRASS driver may have some
[issues](https://github.com/OSGeo/gdal-grass/issues) for vector data. In
our experience, the error and warning messages for vector data can be
ignored. Further, the returned metadata about the coordinate reference
system may currently be incomplete, e.g. it may miss the EPSG code.

## Author

Roger S. Bivand, e-mail: <Roger.Bivand@nhh.no>

## Examples

``` r
# Run example if in active GRASS nc_basic_spm_grass7 location
run <- FALSE
if (nchar(Sys.getenv("GISRC")) > 0 &&
    read.dcf(Sys.getenv("GISRC"))[1, "LOCATION_NAME"] == "nc_basic_spm_grass7") {
  run <- require(terra, quietly = TRUE)
}

# Store original environment variable settings
GV <- Sys.getenv("GRASS_VERBOSE")
Sys.setenv("GRASS_VERBOSE" = 0)
ois <- get.ignore.stderrOption()
set.ignore.stderrOption(TRUE)
#> [1] FALSE

if (run) {
  # Create a new mapset
  meta <- gmeta()
  location_path <- file.path(meta$GISDBASE, meta$LOCATION_NAME)
  previous_mapset <- meta$MAPSET
  example_mapset <- "RGRASS_EXAMPLES"
  execGRASS("g.mapset", "c", mapset = example_mapset)
 }

if (run) {
  # Report basic metadata about the schools dataset
  execGRASS("v.info", map = "schools", layer = "1")
  print(vInfo("schools"))
 }

if (run) {
  # Read/write as a SpatVector
  schs <- read_VECT("schools")
  print(summary(schs))
 }

if (run) {
  try({
    write_VECT(schs, "newsch", flags = c("o", "overwrite"))
  })
  schs <- read_VECT("schools", use_gdal_grass_driver = FALSE)
 }

if (run) {
  write_VECT(schs, "newsch", flags = c("o", "overwrite"))
  execGRASS("v.info", map = "newsch", layer = "1")
 }

if (run) {
  nschs <- read_VECT("newsch")
  print(summary(nschs))
 }

if (run) {
  print(all.equal(names(nschs), as.character(vColumns("newsch")[, 2])))
 }

if (run) {
  # Show metadata for the roadsmajor dataset and read as spatVector
  print(vInfo("roadsmajor"))
 }

if (run) {
  roads <- read_VECT("roadsmajor")
  print(summary(roads))
}

# not run: vect2neigh() currently writes 3 new data sources in the PERMANENT
# mapset, despite this mapset not being the active one.
if (FALSE) {
  cen_neig <- vect2neigh("census")
  str(cen_neig)
}

# Cleanup the previously created datasets
if (run) {
  execGRASS(
    "g.remove",
    flags = "f",
    name = c("newsch", "newsch1"),
    type = "vector"
  )
  execGRASS("g.mapset", mapset = previous_mapset)
  if (example_mapset != previous_mapset) {
    unlink(file.path(location_path, example_mapset), recursive = TRUE)
  }
}

# Restore environment variable settings
Sys.setenv("GRASS_VERBOSE" = GV)
set.ignore.stderrOption(ois)
#> [1] TRUE
```
