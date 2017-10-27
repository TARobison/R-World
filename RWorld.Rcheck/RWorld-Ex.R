pkgname <- "RWorld"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('RWorld')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("make.plants")
### * make.plants

flush(stderr()); flush(stdout())

### Name: make.plants
### Title: Generate plants to exist, reproduce, and compete within the
###   terrain
### Aliases: make.plants

### ** Examples

plants <- make.plants(make.terrain(6,15), c(.7,.85), repro=c(.95,.55), names=NULL, 50)



cleanEx()
nameEx("make.terrain")
### * make.terrain

flush(stderr()); flush(stdout())

### Name: make.terrain
### Title: Terrain with elivations represented by numerics in each cell
### Aliases: make.terrain

### ** Examples

terry <- make.terrain(4, 15)
image(terry)




### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
