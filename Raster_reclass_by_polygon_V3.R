# First, here is a list of libraries that are useful for geospatial analysis in R
packages <- c("tidyverse","sp", "raster", "sf", "maps", "rasterVis")

# Then, check to see if each is installed, and install if not.
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {    
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# now, use the lapply function to load the installed libraries in the packages list 
lapply(packages,library,character.only=TRUE)

# Get test raster
# r <- raster(system.file("external/test.grd", package="raster"))
list.files(pattern = "*.tif")
barc256 <- raster("wy4102310619920180610_20180602_20180618_dnbr_barc256_utm.tif")
barc4 <- raster("wy4102310619920180610_20180602_20180618_dnbr_barc4_utm.tif")
barcdnbr <- raster("wy4102310619920180610_20180602_20180618_dnbr_utm.tif")
refl_20180602 <- raster("wy4102310619920180610_20180602_l8_refl_utm.tif")
refl_20180618 <- raster("wy4102310619920180610_20180618_l8_refl_utm.tif")

# Using the barc4 raster because barc256 does not have NA values
# for outer cells.
r <- barc4
plot(r)
# click(r,id = T)

# convert those cells that are equal to 0 to polygons.
# this assumes that cells equal to 0 are UNCLASSIFIED 
# and need to be assigned values.
rTpol <- rasterToPolygons(r,fun = function(x) x == 0, dissolve = T)
plot(rTpol,col = "blue",border = "blue")
# click(r,xy = T)

poly1 <- rTpol

plot(r)
plot(poly1, add = T,col = "blue",border = "blue")


# zoom in to coords
xlim1 = c(407638.9,412948.9)
ylim1 = c(4542446,4546316)

plot(r, xlim = xlim1, ylim = ylim1)
plot(poly1, add = T,col = "blue",border = "blue")

jpeg(filename = "barc_wHole.jpeg",width = 900, height = 700)
plot(r,xlim = xlim1,ylim = ylim1)
dev.off()


# Get the cell numbers from the raster that are covered by the polygon
cellns <- cellFromPolygon(r,poly1)[[1]]

# Randomly sample cells into n groups. The size of each group needs 
# to be defined as a percentage of the total.
# number of values/catagories
n <- 4

# percentage of total cells allocated to each value/catagory
prc_v <- c(.1,.3,.2,.4)

# updated values/catagories
vals = c(1,2,3,4)

#######################################################
## Get cell numbers randomly allocated to each value

# set cell numbers to object celli
celli <- cellns

# create an empty list to store random cell allocations
sample_list <- list()

# For each value, i, in the number of values, n-1:
# Note that I'm using n-1 because percentages produce fractional numbers
# so by the time i = n, there can be left over cells. 
# i = n is handled after the for loop.
for(i in 1:(n-1)){
  # get a random sample of cell numbers from celli, where the size
  # of the sample is equal to the defined percentage of the total
  # for the given value.
  samplei <- sample(x = celli,
                    size = prc_v[i]*length(cellns),
                    replace = F)
  
  # add random sample i to the result list
  sample_list[[paste0("Value_",i,"_",prc_v[i]*100,"%")]] <- samplei
  
  # remove those cells sampled for this value from the list so that
  # a cell cannot be sampled twice.
  celli <- celli[!celli %in% samplei]
}

# finally, set i equal to n (the final value/catagory)
i = n

# set samplei equal to celli (i.e. those cells not yet allocated)
samplei <- celli

# add the final sample to the result list
sample_list[[paste0("Value_",i,"_",prc_v[i]*100,"%")]] <- samplei

# sum(sapply(1:length(sample_list),function(x) length(sample_list[[x]])))

#####################################
# Assign values to the cell numbers according to 
# their repective allocations.

# For each value, i, in the number of values, n"
for(i in 1:n) {
  # set celli equal to the cell numbers stored in element i within
  # the sample result list
  celli <- sample_list[[i]]
  
  # set the raster cells identified in celli equal to element i in
  # the vector of values
  r[cellns[cellns %in% celli]] <- vals[i]
}

# Plot entire raster
jpeg(filename = "barc_wUpdate.jpeg",width = 900, height = 700)
plot(r,xlim = xlim1,ylim = ylim1)
dev.off()
# Plot a portion of raster zoomed in to an update area
jpeg(filename = "barc_wUpdate.jpeg",width = 900, height = 700)
plot(r,xlim = xlim1,ylim = ylim1)
dev.off()
writeRaster(r)



