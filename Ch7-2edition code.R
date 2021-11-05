# Spatial Autocorrlation with the 2nd Edition of the Spatial Analysis Book
# https://study.sagepub.com/brunsdon2e 


# Make sure the necessary packages have been loaded
library(tmap)
library(tmaptools)
library(SpatialEpi)
library(sf) # for the st_crs function 
sf_extSoftVersion()[1:3] #Check the versions of GEOS, GDAL, and PROJ from the sf package 



# Read in the Pennsylvania lung cancer data
data(pennLC)

#View data type
str(pennLC)

# This step assign the geometry to a variable from the pennLC dataset 
penn.state.latlong <- pennLC$spatial.polygon # This object is the spatial.polygon, class "sp"
# with Proj4 coordinate in proj4string

#st_crs in sf is to extract the coordinate system information
st_crs(penn.state.latlong) #The CRS is unknown in this dataset 

st_as_sf(penn.state.latlong) #Convert a foreign object to an sf object 

# Convert to UTM zone 17N
#penn.state.utm <- set_projection(penn.state.latlong, 3724) #set_projection has been depreciated
#if ("sf" %in% class(penn.state.utm)) 
 # penn.state.utm <- as(penn.state.utm,"Spatial")
# Problem with this original code is the sp object cannot be align to crs until
# it has been tranformed to sf, before processing to the next step. 


# Modified code 
penn.state.utm <- st_as_sf(penn.state.latlong, crs = st_crs(3724))

# Obtain the smoking rates
penn.state.utm$smk <- pennLC$smoking$smoking * 100

# Draw a choropleth map of the smoking rates
tm_shape(penn.state.utm) + tm_polygons(col='smk',title='% of Popn.')

##------Spatial Autocorrelation Visual Inspection --------

# Set up a set of five 'fake' smoking update rates as well as the real one
# Create new columns in penn.state.utm for randomised data
# Here the seed 4676 is used.  Use a different one to get an unknown outcome.

set.seed(4676)
penn.state.utm$smk_rand1 <- sample(penn.state.utm$smk)
penn.state.utm$smk_rand2 <- sample(penn.state.utm$smk)
penn.state.utm$smk_rand3 <- sample(penn.state.utm$smk)
penn.state.utm$smk_rand4 <- sample(penn.state.utm$smk)
penn.state.utm$smk_rand5 <- sample(penn.state.utm$smk)

# Scramble the variables used in terms of plotting order
vars <- sample(c('smk','smk_rand1','smk_rand2','smk_rand3','smk_rand4','smk_rand5'))

# Which one will be the real data?
# Don't look at this variable before you see the maps!
real.data.i <- which(vars == 'smk')

# Draw the scrambled map grid
tm_shape(penn.state.utm) + tm_polygons(col=vars,legend.show=FALSE) +
  tm_layout(title=1:6,title.position=c("right","top"))

real.data.i #Which map is the real map 

## ---- Neighbours and Lagged Mean Plots -------
require(spdep)
penn.state.nb <- poly2nb(penn.state.utm)
penn.state.nb

# Create a SpatialLines DataFrame showing the Queen's case contiguities
# penn.state.net <- nb2lines(penn.state.nb, coords = coordinates(penn.state.utm))

# The code above is giving an error in which the coordinates () is not found 

str(penn.state.utm) # It is a sf and dataframe, where the geometry is sfc_POLYGON
class(penn.state.utm) #coordinates will only work on class Spatial 
if ("sf" %in% class(penn.state.utm)) 
  penn.state.utm <- as(penn.state.utm,"Spatial") #The previous code can now be used here for conversion

penn.state.net <- nb2lines(penn.state.nb, coords = coordinates(penn.state.utm))
# A warning was generated in which CRS(proj4string) is null and has been set to NA

# Default projection is NA,  can change this as below
# penn.state.net <- set_projection(penn.state.net,current.projection = 3724)
# same error as first part where the set_projection function no longer works. 

#modified code
penn.state.net <- st_as_sf(penn.state.net,crs = st_crs(3724))

# Draw the projections
tm_shape(penn.state.utm) + tm_borders(col='lightgrey') + 
  tm_shape(penn.state.net) + tm_lines(col='red')

# Calculate the Rook's case neighbours
penn.state.nb2 <- poly2nb(penn.state.utm,queen=FALSE)

# Convert this to a SpatialLinesDataFrame
penn.state.net2 <- nb2lines(penn.state.nb2,coords=coordinates(penn.state.utm))

# Update projection
# penn.state.net2 <- set_projection(penn.state.net2,current.projection = 3724)

#modified code
penn.state.net2 <- st_as_sf(penn.state.net2, crs = st_crs(3724))

# Plot the counties in background,  then the two networks to compare: 
tm_shape(penn.state.utm) + tm_borders(col='lightgrey') + 
  tm_shape(penn.state.net) + tm_lines(col='blue',lwd = 2) + 
  tm_shape(penn.state.net2) + tm_lines(col='yellow')

# There are three warnings in this code due to the proj4string package 

# Convert the neighbour list to a listw object - use Rook's case...
penn.state.lw <- nb2listw(penn.state.nb2)
penn.state.lw

penn.state.utm$smk.lagged.means <- lag.listw(penn.state.lw,penn.state.utm$smk)
tm_shape(penn.state.utm) + tm_polygons(col='smk.lagged.means',title='% of Popn.')  +
  tm_layout(legend.bg.color = "white") 
# A warning message for CRS object 

# Plot the data points of Lagged Mean Plot for Smoking Uptake 
with(data.frame(penn.state.utm), {
  plot(smk,smk.lagged.means,asp=1,xlim=range(smk),ylim=range(smk))
  abline(a=0,b=1)
  abline(v=mean(smk),lty=2)
  abline(h=mean(smk.lagged.means),lty=2)
})

# Plot the data points for Lagged Mean Plot for Smoking Uptake - Alternative Method 
moran.plot(penn.state.utm$smk,penn.state.lw)

##----------- Morans I in R -------------------
moran.test(penn.state.utm$smk,penn.state.lw)

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(penn.state.lw)

moran.test(penn.state.utm$smk,penn.state.lw,randomisation=FALSE)

# A simulation based approach 
moran.mc(penn.state.utm$smk,penn.state.lw,10000)

## ------------- Spatial Autoregression -------------------
sar.res <- spautolm(smk~1,listw=penn.state.lw,data=penn.state.utm)
sar.res

sar.res$lambda.se

sar.res$lambda + c(-2,2)*sar.res$lambda.se

# Model with predictors - Bivariate Example 
head(pennLC$data)

require(plyr)
totcases <- ddply(pennLC$data,c("county"),numcolwise(sum))

head(totcases)

totcases <- transform(totcases,rate=10000*cases/population)

head(totcases)

# Check the distribution of rates
boxplot(totcases$rate,horizontal=TRUE,
        xlab='Cancer Rate (Cases per 10,000 Popn.)')

# A warning was given about the following functions in which spatialreg in required
library(spatialreg)

sar.mod <- spautolm(rate~sqrt(penn.state.utm$smk),listw=penn.state.lw,
                    weight=population,data=totcases)
summary(sar.mod)

# Resources:
# https://mgimond.github.io/Spatial/coordinate-systems-in-r.html
# https://r-spatial.github.io/sf/reference/st_as_sf.html