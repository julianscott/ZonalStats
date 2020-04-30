# Julian A Scott 04/22/20
# jascott@usda.gov
# Zonal Stats Webinar

# First, here is a list of the required packages
packages <- c("tmap","sf","tidyverse","sp","nhdplusTools","raster","FedData","prism","RColorBrewer","data.table","rgdal","httr")

packages

# Then, check to see if each is installed, and install if not.
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {    
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# now, use the lapply function to load the installed libraries in the packages list 
lapply(packages,library,character.only=TRUE)

################################################################################
## Package details
################################################################################
# sp: A package for spatial data: points, lines, polygons and grids
?sp

# sf: Simple Features; More advanced and intuitive than sp, but less ubiquitous, for now. 
help(package="sf")

# raster: The raster package provides classes and functions to manipulate geographic (spatial) data in grid format.
?raster

# tidyverse: provides raft of useful packages including:
#             -ggplot2: Create Elegant Data Visualizations Using the 
#                     Grammar of Graphics (gg)
#             -dplyr: a flexible grammar of data manipulation. 
#             - lots of other useful packages
?tidyverse

# tmap: A new mapping package that is flexible and easy to use to create maps. It can access an ESRI world basemap, plus neat cartographic templates.  It is based on the grammar of graphics, and resembles the syntax of ggplot2.
?tmap

# nhdplusTools: Tools for traversing and working with National Hydrography Dataset Plus (NHDPlus) data. 

# FedData: Functions to Automate Downloading Geospatial Data Available from Several Federated Data Sources. NED, NHD, SSURGO, DAYMET, historic climate, tree ring data, national land cover dataset, and more. 
help(package="FedData")

##########################################
# Zonal Statistics in R
# 1. RStudio bonus tip: use 'Projects'
# 2. Accessing watershed polygons in R
# 3. Writing/reading shapefiles
# 4. Calculating polygon area
# 5. Accessing elevation and land cover datasets
#    and plotting with the tmap package
# 6. Zonal Stats Task 1: mean elevation and slope
# 7. Zonal Stats Task 2: percent of catchment above an elevation threshold
# 8. Zonal Stats Task 3: Percent of catchment covered by NLCD cover classes

##########################################
# 1. RStudio bonus tip: use 'Projects'
# 
# Best practice is to create an RStudio Project in the folder that
# contains your data. A Project automatically has the working 
# directory that matches the location of the project file (*.RProj).
##########################################
# getwd()

##########################################
# 2. Accessing watershed polygons in R
# 
# To carry out zonal statistics, we need polygons and rasters. 
# Now I will show you how to get NHDPlus watershed polygons in R. 
# Then later, we will calculate some zonal statistics for these
# watersheds.
##########################################

# Get NHDPlus data using the nhdplusTools package.

# Identify coordinate for which the upstream NHDPlus data will be retrieved
# start_point <- st_sfc(st_point(c(-105.8079073, 40.6662174)), crs = 4326)
start_point <- st_sfc(st_point(c(-105.7546915, 40.5354662)), crs = "+proj=longlat +datum=WGS84")

# Get the ID number of the NHD plus flowline that is nearest to the start point
start_comid <- discover_nhdplus_id(start_point)

# Get upstream mainstem and tributary flowlines for this point
flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries")

plot(flowline$geometry)
plot(start_point,add = T,col = "red")

# Get all of the NHDPlus data that corresponds to the identfied flowlines
NHDplus_data <- subset_nhdplus(comids = flowline$nhdplus_comid,
                               output_file = tempfile(fileext = ".gpkg"),
                               nhdplus_data = "download", 
                               return_data = TRUE)

# NHDplus_data is a list of simple features (analogous to a list of 
# shapefiles)
class(NHDplus_data)
summary(NHDplus_data)

# The NHD data is downloaded in a unprojected coordinate system, 
# so, I want to project it in order to calculate areas and do other 
# zonal statistics.
proj_crs <- "+proj=utm +zone=13 +datum=NAD83 +units=m +no_defs"
# browseURL("https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf")
# EPSG <- make_EPSG()
# View(EPSG[grep("zone 13", EPSG$note),])

# Transform the projection of the shapefiles in the list
NHDplus_data <- lapply(NHDplus_data,function(x) st_transform(x,proj_crs))

# From within the NHDplus_data list, get the simple features (i.e. shapefiles)
flowline <- NHDplus_data$NHDFlowline_Network
catchment <- NHDplus_data$CatchmentSP
waterbody <- NHDplus_data$NHDWaterbody

# Plot catchment and flowlines
par(pin = c(2,2))
plot(catchment$geometry,col = "green")
plot(flowline$geometry,add = T,col = "dark blue")
plot(waterbody$geometry,add = T,col = "light blue")

# Add names for plotting
catchment$legend <- "Watersheds"
waterbody$legend <- "Waterbodies"
flowline$legend <- "Streams"
start_point <- st_as_sf(start_point)
start_point$legend <- "Point of Interest"

##########################################
#3.  Writing/reading shapefiles
##########################################

# Write catchment to working directory as a shapefile.
st_write(catchment,"StudyArea_catchments.shp",delete_layer = T)

# Write catchment to a folder within your working directory.
st_write(catchment,"./junkfolder/StudyArea_catchments.shp",delete_layer = T)

# Remove the catchment object from your R environment.
rm(catchment)

# Read the catchment shapefile into your R environment
catchment <- st_read("StudyArea_catchments.shp")

# And plot 
plot(catchment$geometry,col = "green")
plot(flowline$geometry,add = T,col = "blue")

# Simple as that!

##########################################
# 4. Calculate polygon area
##########################################

# Area of the entire collection of polygons 
# raster::area uses units of coordinate projection system (meters)
# 1609.34 meters = 1 mile
raster::area(as_Spatial(st_union(catchment)))/(1609.34^2)

# Area of each individual catchment
raster::area(as_Spatial(catchment))/(1609.34^2)

##########################################
# 5. Accessing elevation and land cover datasets
#     and plotting with the tmap package
##########################################

# Because I want to perform zonal statistics on the 
# polygons in the catchment shapefile, its good practice to 
# download the raster data with a slightly larger footprint 
# than the polygon data, so there aren't issues of NoData cells. 

# I will buffer the catchment object by 250 meters. 
# Note I dissolve the boundaries of the multipart polygon first.
buf_catchment <- st_buffer(st_union(catchment),
                           units::set_units(250,m))

par(pin = c(2.5,2.5))
plot(buf_catchment,col = "green",)
plot(catchment$geometry,add = T,border = "red")

# Get DEM using the FedData package

# Because the FedData::get_ned function can be finicky, I provide
# the tif file via a link to my github site as a failsafe way to run this code.
# I encourage you to uncomment out the FedData::get_ned function below and see
# if you can get it to run (email me or call for assistance). Its a nice tool, 
# just touchy for unknown reasons.
# 
NEDurl <- "https://github.com/julianscott/ZonalStats/blob/master/30meter_NED_1.zip?raw=true"
GET(NEDurl, write_disk("30meter_NED_1.zip", overwrite = TRUE))
unzip(zipfile = "30meter_NED_1.zip",
      overwrite = TRUE)
area_NED <- raster("30meter_NED_1.tif")
# 
# 

# Uncomment get_ned() function below to test drive. If it doesn't work,
# Don't despair, get in touch with me to troubleshoot.

# area_NED <- get_ned(template = as_Spatial(buf_catchment),
#                     extraction.dir = ".\\EXTRACTIONS\\NED\\",
#                     label = "30meter",
#                     res = "1",
#                     force.redo = F)

# Make the variable name of the raster more intuitive.
names(area_NED) <- "elev"

# Transform elevation raster to match our projected coordinate system
area_NED <- raster::projectRaster(from = area_NED,
                                  res = c(30,30),
                                  crs = proj_crs)

# Convert elevations into feet. This is analogous to raster calculator in ArcGIS.
area_NED <- area_NED*3.2808

# dev.off()
plot(area_NED)
plot(catchment$geometry,border = "black",add = T)
plot(flowline$geometry,add = T,col = "blue")

# Plot using tmap. Note that running ttm() switches between static and 
# interactive plotting.
# 
# The interactive plotting allows you to easilu see basemaps, like earth imagery
# and to move around like google earth or ArcGIS. In interactive mode, layers 
# can be turned on and off by clicking the hamburger icon in the upper right.

# Activate switch. Note the console readout to see if it is set to interactive
# or normal plotting. Re-run ttm() to swich until set to interactive.
ttm()

# To use the interactive plotting function of tmap, the rasters must be 
# in the following projection.
merc_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0
+x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
area_NED_merc <- projectRaster(area_NED,crs = merc_crs)

elev_map <- tm_shape(area_NED_merc)+
  tm_raster("elev",
            palette = terrain.colors(10),
            # palette = viridisLite::viridis(20, begin = 0.37, end = 1),
            title = "NED 30 meter",n=4,style = "cont",
            alpha = 0.9)+
  tm_layout(frame = F,
            inner.margins = c(0.02,0.3,0.06,0.02),
            legend.position = c("left","top"),
            attr.position = c("left","bottom"))

elev_map


# Note the layers that are available by default. 
# Does not include satellite imagery.
# Add world imagery like this:
tmap_options(basemaps = c(Canvas = "Esri.WorldGrayCanvas",
                          Imagery = "Esri.WorldImagery",
                          StreetMap = "OpenStreetMap",
                          Topo = "Esri.WorldTopoMap"))

# Remap to see results with world imagery layer available.
elev_map

# Get National Land Cover Dataset raster for catchments

# Because the FedData::get_nlcd function can be finicky, I provide
# the tif file via a link to my github site as a failsafe way to run this code.
# I encourage you to uncomment out the FedData::get_nlcd function below and see
# if you can get it to run (email me or call for assistance). Its a nice tool, 
# just touchy for unknown reasons.

NLCDurl <- "https://github.com/julianscott/ZonalStats/blob/master/NLCD_2016_Land_Cover_L48.zip?raw=true"
GET(NLCDurl, write_disk("NLCD_2016_Land_Cover_L48.zip", overwrite = TRUE))
unzip(zipfile = "NLCD_2016_Land_Cover_L48.zip",
      overwrite = TRUE)
area_NLCD <- raster("NLCD_2016_Land_Cover_L48.tif")

# Uncomment get_ned() function below to test drive. If it doesn't work,
# Don't despair, get in touch with me to troubleshoot.

# Get National Land Cover Dataset
# area_NLCD <- get_nlcd(template = buf_catchment,
#                       extraction.dir = ".\\EXTRACTIONS\\NLCD\\",
#                       label = "NLCD",
#                       landmass = "L48",force.redo = T)

# If get_nlcd doesn't work (it can be buggy), uncomment the 
# code below to get the same data from my github site.

# Transform NED raster to match our working coordinate system
area_NLCD <- raster::projectRaster(from = area_NLCD,
                                   res = res(area_NLCD),
                                   crs = proj_crs,
                                   method = "ngb")

# Awesome color palette tool!
# tmaptools::palette_explorer()

# Another way to set the palette using brewer.pal...
# mycolors <- colorRampPalette(brewer.pal(9,'Spectral'))(16)

# To use the interactive plotting function of tmap, the NLCD raster must be 
# in the merc_crs projectio (defined above).
area_NLCD_merc <- projectRaster(area_NLCD,crs = merc_crs,method = "ngb")

# Get more descriptive class names to match the integer key for the cover classes.
unique(area_NLCD)
nlcdclass <- fread("https://raw.githubusercontent.com/julianscott/ZonalStats/master/NLCD_classes.csv")

nlcdclass

# Map NLCD cover for study area using tmap.
cover_map <- tm_shape(area_NLCD_merc)+
  tm_raster("NLCD_2016_Land_Cover_L48",title = "NLCD 2016",
            # palette = mycolors,
            palette = tmaptools::get_brewer_pal("Accent", n = 8,plot = F),
            style = "cat",
            labels = nlcdclass$Class,
            colorNA = "black",
            alpha = 0.6)+
  # legend.hist = TRUE, 
  # legend.hist.title = "Frequency of land cover classes")+
  tm_layout(frame = F,
            inner.margins = c(0.02,0.3,0.06,0.02),
            legend.position = c("left","top"),
            attr.position = c("left","bottom"),
            legend.show = T)

cover_map

# Map vector data (catchments, flowlines...)
vector_map <-
  tm_shape(catchment)+
  tm_polygons(col = "legend",alpha = 0,border.col = "#7570B3",title = "",
              lwd = 2)+
  tm_shape(waterbody)+
  tm_polygons(col = "legend",palette = "#6baed6",border.col ="black",
              title = "",lwd = 0.5,
              popup.vars = c("gnis_name","meandepth"))+
  tm_shape(start_point)+
  tm_dots(col = "legend",size = 0.5,shape = 16,palette = "red",title = "")+
  tm_shape(flowline)+
  tm_lines(col = "legend",palette = "blue",title.col = "",
           popup.vars = c("gnis_name","slope","streamorde"))+
  # tm_format("NLD_wide")+
  tm_layout(frame = F,
            inner.margins = c(0.02,0.3,0.06,0.02),
            legend.position = c("left","top"),
            attr.position = c("left","bottom"),
            legend.show = T)

vector_map

# OVerlay shapefiles on raster data

# dev.new(width=5, height=4, unit="in") # this code will create a
 # new plotting device that is big enough to provide good detail.
 
# Plot all three maps together.
elev_map + cover_map + vector_map 

##########################################
#6. Zonal Stats Task 1: mean elevation
##########################################

# Get Mean Elevation for each catchment
mean_elevations_ft <- raster::extract(area_NED,
                                      catchment,
                                      fun = mean) 

# NOTE: With raster::extract(), you could use fun = min, max, sd 
# or custom function. 

# Get slope raster
slope_raster <- raster::terrain(area_NED,"slope","degrees")

# NOTE: With raster::terrain(), you can also return aspect and 
# other terrain measures. 

# Get Mean slope for each catchment
mean_slope_deg <- raster::extract(slope_raster,
                                  catchment,
                                  fun = mean) 

# Attach mean elevation and slope to the catchment object
catchment$elev_ft <- mean_elevations_ft[,1]
catchment$slope_deg <- mean_slope_deg[,1]
View(catchment)

# Plot results
# 
# Switch tmap to static plotting mode.
ttm()

tm_shape(catchment)+
  tm_polygons(col = c("elev_ft","slope_deg"),palette = terrain.colors(10),n=10)+
  tm_layout(frame = F,
            inner.margins = c(0.02,0.3,0.06,0.02),
            legend.position = c("left","top"),
            attr.position = c("left","bottom"),
            legend.show = T)

# Present results as table
names(catchment)
catchment %>% 
  as.data.frame() %>% 
  dplyr::select(featurd,elev_ft,slope_deg) %>% 
  head()

# Classify catchments by an elevation threshold
catchment$elevClass <- if_else(catchment$elev_ft >= 11000,"elev >= 11000","elev < 11000")

# Plot results
tm_shape(catchment)+
  tm_polygons(col = "elevClass",palette = terrain.colors(2),n=10)+
  tm_layout(frame = F,
            inner.margins = c(0.02,0.3,0.06,0.02),
            legend.position = c("left","top"),
            attr.position = c("left","bottom"),
            legend.show = T)

# Present results as table, sorted  by elevClass
catchment %>% 
  as.data.frame() %>% 
  dplyr::select(featurd,elevClass,elev_ft) %>% 
  arrange(elev_ft) %>% 
  head()

##########################################
# 7. Zonal Stats Task 2: For each catchment, 
#                       determine the percentage of catchment above 
#                       a certain elevation threshold. 
###########################################

# Let’s work with a single catchment first.
catch15 <- catchment[15,] #[row, column]

# Extract elevation for each cell covered by catchment15
catch15_elevs <- raster::extract(area_NED,catch15)[[1]]
catch15_elevs

# Make a histogram of the elevation data for the catchment
ggplot()+
  aes(catch15_elevs)+
  geom_histogram()+
  geom_vline(aes(xintercept = mean(catch15_elevs)),color = "red")

# Now, let’s find the proportion of the catchment above 10000 feet 
length(catch15_elevs[catch15_elevs >= 10000])/length(catch15_elevs)
# [1] 0.7929027

# Great! But what about all the catchments? And what if we wanted 
# to choose a different elevation threshold? 

# First define an elevation threshold variable
threshold = 10500

# Get elevation for each cell for each catchment.
# Note that extract will return results as a list by default.
cell_elevations_ft <- raster::extract(area_NED,catchment)
str(cell_elevations_ft)

# And use sapply to calculate the proportion of each catchment 
# above the threshold
frac_high_elev <- sapply(cell_elevations_ft,
                         function(x) length(x[x >= threshold])/length(x))
frac_high_elev

# Add results to the catchment object
catchment$frac_high_elev <- frac_high_elev

# Plot results
tm_shape(catchment)+
  tm_polygons(col = "frac_high_elev",palette = terrain.colors(10),n=10)+
  tm_layout(frame = F,
            inner.margins = c(0.02,0.3,0.06,0.02),
            legend.position = c("left","top"),
            attr.position = c("left","bottom"),
            legend.show = T)

# Present results as table, sorted  by elevClass
catchment %>% 
  as.data.frame() %>%
  dplyr::select(featurd,frac_high_elev,elev_ft) %>% 
  arrange(frac_high_elev) %>% 
  head()

#############################################
#8. Zonal Stats Task 3: Get the percent of each 
#                       catchment in each category
#                       of land cover
#############################################

# First, let’s look at the area_NLCD raster
area_NLCD
# Note the raster data name is NLCD_2016_Land_Cover_L48

# Now we use extract to get land cover class for each pixel in
# each catchment.

# Note that this time we are asking it to return a dataframe, not a list.
NLCD_classes <- extract(area_NLCD,as_Spatial(catchment),df = T)

# The result has two columns, ID and the name of the raster data,
# NLCD_2016_Land_Cover_L48. 
head(NLCD_classes)
# Each row is a pixel from the area_NLCD raster.

# ID corresponds to the catchments (ID = 1 refers to the catchment 
# in the first row of the 'catchment' object, etc.).

# The NLCD_2016_Land_Cover_L48 column contains the cover classes. 

str(NLCD_classes)
unique(NLCD_classes$ID)
unique(NLCD_classes$NLCD_2016_Land_Cover_L48)
summary(NLCD_classes)

# Lets rename ID to be more descriptive
NLCD_classes <- rename(NLCD_classes,"catchment" = "ID")
head(NLCD_classes)

# The packages dplyr and tidyr are excellent for summarizing data.  
browseURL("https://tidyr.tidyverse.org/")

# The task is to get the percent of each catchment in each category
# of land cover. I'll break this down step by step, then show you 
# how to do it all at once.

# Group the data in NLCD_classes by catchment ID and cover class 
NLCD_grouped <- group_by(NLCD_classes,catchment, NLCD_2016_Land_Cover_L48)
head(NLCD_grouped)

# Count the number of observations in each group 
NCLD_tallied <- tally(NLCD_grouped)
View(NCLD_tallied)

# Put the cell counts for each cover type its own column
NCLD_tallied_wider <- pivot_wider(NCLD_tallied,
                                  names_from = NLCD_2016_Land_Cover_L48,
                                  values_from = n, 
                                  values_fill = list(n = 0))
# For each catchment we have the number of cells covered by each land cover type.
View(NCLD_tallied_wider)

# So that was step-by-step, but it can be done much more succinctly.
# Here is the power of tidyverse and the piping operator, %>% 

# Start the piping chain with the NLCD_classes dataframe
NCLD_tallied_wider2 <- NLCD_classes %>% 
  # Group the dataframe by catchment ID and cover class variables
  group_by(catchment,NLCD_2016_Land_Cover_L48) %>%
  # Count the number of observations in each group 
  tally() %>% 
  # Pivot the table so that each cover class has its own column
  pivot_wider(names_from = NLCD_2016_Land_Cover_L48,
              values_from = n, 
              values_fill = list(n = 0))
View(NCLD_tallied_wider2)

all.equal(NCLD_tallied_wider,NCLD_tallied_wider2)

# Our task was to get the percent of each catchment in each category 
# of land cover, so we need to simply divide by the total number of 
# cells in each catchment

# Start the piping chain with the NLCD_classes dataframe
NLCD_classes_results <- NLCD_classes %>% 
  # Group the dataframe by catchment ID and cover class variables
  group_by(catchment,NLCD_2016_Land_Cover_L48) %>%
  # Count the number of observations in each group.
  # Tally drops the cover class grouping variable because 
  # its nonsensical after the summary aggregates the data.
  tally() %>% 
  # Because the data is still grouped by catchment,
  #  we can sum the number of cells in each catchment 
  #  in a new column.
  mutate(total_cells_per_catchment = sum(n),
         perc_of_catchment = n/total_cells_per_catchment*100,
         perc_of_catchment = round(perc_of_catchment,1)) %>% 
  # Drop the count columns
  dplyr::select(-n,-total_cells_per_catchment) %>% 
  # Pivot the table so that each cover class has its own column
  pivot_wider(names_from = NLCD_2016_Land_Cover_L48,
              values_from = perc_of_catchment, 
              values_fill = list(perc_of_catchment = 0))

View(NLCD_classes_results)  

# Lets add more descriptive names to the land cover classes 
# in our results table
NLCD_classes_results <- NLCD_classes_results %>% 
  # Pivot the dataframe to be longer (so that I can lookup class 
  # names in a single column)
  pivot_longer(-catchment,names_to = "class_num",values_to = "perc_catchm") %>% 
  # change the name of the cover class and make numeric 
  mutate(class_num = as.numeric(class_num)) %>% 
  # join (like vlookup in excel) with the table of NLCD land cover 
  # class names and numbers
  left_join(nlcdclass,by= c("class_num" = "NLCDnumber")) %>% 
  # use join to add catchment id, instead of the simple order id
  left_join(data.frame("catchmentID" = catchment$id,"order" = 1:nrow(catchment)),
            by = c("catchment" = "order")) %>% 
  # Drop uneeded columns
  dplyr::select(catchment,catchmentID,Class,perc_catchm) %>% 
  # pivot back to the wide table, 1 row per catchment, 1 column per cover class
  pivot_wider(names_from = Class,
              values_from = perc_catchm, 
              values_fill = list(perc_catchm = 0))
View(NLCD_classes_results)

# Add NLCD data to the catchment shapefile.
catchment_new <- catchment %>% 
  left_join(NLCD_classes_results,by = c("id" = "catchmentID"))
names(catchment_new)

# Plot catchments colored by % Evergreen Forest cover
ggplot(catchment_new) +
  geom_sf(aes(fill = `Evergreen Forest`))

# Select catchments that meet multiple conditions
sub_catchments <- catchment_new %>% 
  filter(slope_deg > 30) %>% 
  filter(elev_ft >=10500) %>% 
  filter(`Bare Rock/Sand/Clay` > 1)

# Plot
ggplot() +
  geom_sf(data = catchment_new)+
  geom_sf(data = sub_catchments,fill = "red")+
  ggtitle("Watersheds with slope > 30 degrees, \n mean elevation above 10500, \n and with more than 1% bare ground")

# export shapefile as csv to bring into excel, etc
st_write(catchment_new,"catchment_new.csv")

# export as shapefile for arcgis, etc
st_write(catchment_new,"catchment_new.shp")

