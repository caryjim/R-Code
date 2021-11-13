#' Presentation by Martijn Tennekes of tmap in R
#' Source from Youtube video: https://www.youtube.com/watch?v=DtZYJBv5ZBY 
# I followed the tutorials and added my notes based on my config and process. 

#' Initiate set-up, don't need to repeat 
# library(devtools)
# install_github("mtennekes/tmap")

# Ensure libraries are loaded 
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)

data("World", "metro", "land")

qtm(World) # A function for quick plot
# Use these settings and then re-visit the qtm() function to replot 

tmap_mode("plot") # set to plotting (static view), display under plots
tmap_mode("view") # set to interactive viewing, display under Viewer or set to browser

options(viewer = NULL) # This setting will always send the map to browser 

qtm(metro) # all major metros marked on map with tooltip information
qtm(land) # 4 quadrants of biomes on earth

tm_shape(World) + 
  tm_polygons(col = "purple") # col= can be a variable or a color 

ttm()# simply call this to change view options between plotting or interactive 

# Set back to plot for now 
tm_shape(World) + 
  tm_polygons(col = "footprint", n = 6) # assign the number of category by n= 

# pretty(1:10, n = 5) # a function for well rounded levels within a given range

tm_shape(World) + 
  tm_polygons("footprint", style = "cont") # style can be change to continuous range

tm_shape(World) + 
  tm_polygons("footprint", style = "cont",
              breaks = c(0, 10)) # breaks put a bound to the range in footprint

tm_shape(World) + 
  tm_polygons("footprint", style = "cont",
              breaks = c(0, 10))

# tmap_options() # this call the list of settings in terms of what you can do 

#' There are three default palettes depend on the variable's type
#' categorical, sequential, gradient, divergent, etc. 

# library(shiny)
# library(shinyjs)
# tmaptools::palette_explorer() # This will show the palette through shiny 
# You can adjust the Brewer and Sequential option to obtain a palette customization


tm_shape(World) + 
  tm_polygons("footprint", palette = "PuRd")

tm_shape(World) + 
  tm_polygons("footprint", palette = "Blues", n = 14)

tm_shape(World) + 
  tm_polygons("HPI", palette = "-RdYlGn") # use "-" sign to make it diverged 

tm_shape(World) + 
  tm_polygons("footprint", palette = "Blues", n = 14) # for color deficiency 
# Visit colorbrewer2.org for color blind palette 

World$x <- paste0("x", 1:nrow(World))
# Add a column to the sf with x1,x2...

tm_shape(World) + tm_polygons(col = "x")
# a warning is given due to categories > 30 and the color is repeated

tmap_options(max.categories = 200) # then, re-run these to see the adjustment

# Stagging of layers 

# Second layer can be any sf, SP, or raster spatial data 
tm_shape(World) + 
  tm_polygons("pink") +
  tm_shape(metro) + 
  tm_dots()

# You can specify projection here
tm_shape(World, projection = 4326) + 
  tm_polygons("pink") +
  tm_shape(metro) 
  tm_dots()
# warning message of projection of long/lat in 4326, to recreate object with sf::st_crs() 
  
# You can specify projection here
tm_shape(World, projection = st_crs(World)) + 
  tm_polygons("pink") + 
  tm_shape(metro, projection = st_crs(metro)) + 
  tm_dots()

metro$growth <- (metro$pop2020 - metro$pop2010)/ (metro$pop2010 * 10)* 100
  
tm_shape(World) + 
  tm_polygons(col = "HPI") +
  tm_shape(metro) + 
  tm_bubbles(size = "pop2020", col = "growth") # base layer and bubbles on the map with legends


tm_shape(World) + 
  tm_polygons(col = "HPI") +
  tm_shape(metro) + 
  tm_bubbles(size = "pop2020", col = "growth", shape = " ") # shape argument for categorical data 

tm_shape(World) + 
  tm_polygons(col = "HPI") +
  tm_shape(metro) + 
  tm_bubbles(size = "pop2020", col = "growth") +
  tm_style("classic") # There are few pre-defined map, classic, beaver, cobalt

# tmap_style("bw") # black and white map, output shows other options

# Shape, layer, compass
# this add a compass to the map 

tm_shape(World) + 
  tm_polygons(col = "HPI") +
  tm_compass() + 
  tm_shape(metro) + 
  tm_bubbles(size = "pop2020", col = "growth") 

tm_shape(World) + 
  tm_polygons(col = "HPI") +
  tm_shape(metro) + 
  tm_bubbles(size = "pop2020", col = "growth") +
  tm_style("classic") 

# raster can be layered the same way 

tm_shape(land) + tm_raster("cover_cls") +
  tm_shape(metro) +
  tm_dots()

# tmap_style("natural")

# This add the country borders on the map 
tm_shape(land) + 
  tm_raster("cover_cls") + # colver class is the biomes
tm_shape(World) +
  tm_borders() + 
tm_shape(metro) +
  tm_dots()

# thicken the broader lines
# cover class is the biomes
# This add the country borders on the map 
tm_shape(land) + 
  tm_raster("cover_cls") + 
  tm_shape(World) +
  tm_borders(lwd = 3) + 
  tm_shape(metro) +
  tm_dots()

# Shape, Layers, Facets are the groups in the thematic map
# tm functions vs. tmap functions 

# Facets (display multiple maps side-by-side)

# specify multiple variables to show groups of different organization
tm_shape(World) + tm_polygons(c("HPI", "gdp_cap_est"))
