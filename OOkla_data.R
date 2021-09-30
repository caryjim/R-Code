#OOkla Open Data 
# https://github.com/teamookla/ooklaOpenDataR

#install.packages("remotes")
#remotes:: install_github("teamookla/ooklaOpenDataR")
library(ooklaOpenDataR)

mobile_q3_2020 <- get_performance_tiles(service = "mobile", quarter = 2, year = 2020)
#install.packages("sf")
fixed_q3_sf_2020 <- get_performance_tiles(service = "fixed", quarter = 2, year = 2020, sf = TRUE) #sf dataframe

#write files 
write.csv(mobile_q3_2020,"D:/Research Data/2021 Dissertation Collection/OOKLA/mobile_q3_2020.csv", row.names = FALSE)
write.csv(fixed_q3_sf_2020,"D:/Research Data/2021 Dissertation Collection/OOKLA/fixed_q3_sf_2020.csv", row.names = FALSE)

#------------------------------------------------------------------------------------------------------------------------------------
#Analyzing download speeds in Kentucky counties
#https://github.com/teamookla/ookla-open-data/blob/master/tutorials/aggregate_by_county.md

#In this tutorial I will talk about how to:

#Download the Ookla open dataset
#Geocode the tiles to Kentucky counties
#Make a table of the top and bottom 20 counties by download speed
#Map the counties

#There are two main ways to join these tiles to another geographic
#dataset: quadkeys and spatial joins. This tutorial will use the spatial
#join approach.


library(tigris) # county boundaries tigris:counties is the function to use 
library(tidyverse) # data cleaning
library(sf) # spatial functions
library(knitr)
library(kableExtra) # county statistics table
library(RColorBrewer) # colors
library(here) # file management
library(usethis) # download data


## Download data

##First, download the data to a local directory by uncommenting that line below

#download the zip folder from s3 and save to working directory

#use_zip("https://ookla-open-data.s3-us-west-2.amazonaws.com/shapefiles/performance/type%3Dfixed/year%3D2020/quarter%3D2/2020-04-01_performance_fixed_tiles.zip")

#read the shapefile. 
tiles <- read_sf(here("2020-04-01_performance_fixed_tiles/gps_fixed_tiles.shp")) %>%
  mutate(avg_d_kbps = as.numeric(avg_d_kbps),
         avg_u_kbps = as.numeric(avg_u_kbps),
         avg_lat_ms = as.numeric(avg_lat_ms))

## Get county boundaries
# Then, I'll load the Kentucky county boundaries from the U.S. Census Bureau via `tigris`.

ky_counties <- tigris::counties(state = "Kentucky") %>%
  select(state_code = STATEFP, geoid = GEOID, name = NAME) %>% # only keep useful variables 
  st_transform(4326) # transform to the same CRS as the tiles

## Load US counties to the shapefile
us_counties <- tigris::counties() %>% # all states
  select(state_code = STATEFP, geoid = GEOID, name = NAME) %>% # only keep useful variables 
  st_transform(4326) # transform to the same CRS as the tiles


## Join tiles to counties

#Now I'll join the tiles to the counties. I use `left = FALSE` because I only want to include counties that have at least 1 tile.

tiles_in_ky_counties <- st_join(ky_counties, tiles, left = FALSE)


## Calculate statistics

#Once the datasets are joined, we are interested in summary statistics at
#the county level. Since we know the average download speed per tile, we
#could just do a simple average. To make it more accurate, I'll use a
#weighted mean, weighted by test count. This gives us the overall mean in
#the county if the data hadn't been aggregated to tiles first. I've also
#included weighted means for upload speed and latency here as well.

county_stats <- tiles_in_ky_counties %>%
  st_set_geometry(NULL) %>%
  group_by(state_code, geoid, name) %>%
  summarise(mean_dl_mbps_wt = weighted.mean(avg_d_kbps, tests) / 1000,
            mean_ul_mbps_wt = weighted.mean(avg_u_kbps, tests) / 1000,
            mean_lat_ms_wt = weighted.mean(avg_lat_ms, tests),
            tests = sum(tests)) %>% ungroup() %>% left_join(fips_codes %>%
                                                              mutate(geoid = paste0(state_code, county_code)) %>%
                                                              # get nicer county and state names
                                                              select(state, geoid, long_name = county, county), by = c("geoid")) 

## Make a table of the top 20 and bottom 20 counties

#Next we can make a summary table of just the best and worst counties.
#We'll require that counties have at least 50 tests so that the
#averages are more reliable. I use `kable()` here for simplicity, but you
#could use any of the R packages that help with tables.


table_data <- county_stats %>%
  filter(tests >= 50) %>%
  mutate(rank = min_rank(-mean_dl_mbps_wt)) %>% # rank in descending order
  dplyr::filter(rank <= 20 | rank >= n() - 19) %>%
  mutate(`County` = paste0(long_name, ", ", state),
         mean_dl_mbps_wt = round(mean_dl_mbps_wt, 2)) %>%
  arrange(rank) %>%
  select(`County`, `Average download speed (Mbps)` = mean_dl_mbps_wt, `Tests` = tests, `Rank` = rank)

kable(table_data, format.args = list(big.mark = ","))

## Map the counties

#The table is good for a quick glance at overall patterns (what are the
#overall maxima and minima? where is the fastest speed?) but unless
#you're already familiar with these areas it can be hard to picture
#where they are on a map. To go along with the table we can produce a
#quick choropleth map that will help give a more visual representation.

#We can join our county statistics table to the basemap (remember, we
#already got rid of the geometry from that county statistics table). I'm
#also creating a categorical variable from the continuous download speed
#because people aren't great at reading continuous color schemes. People
#can read discrete legends much more easily, with 7 categories maximum
#(this can depend on the situation, though).

#One thing that helps people orient themselves on a map is including
#major place names. The `{tigris}` package makes it fairly easy to get a
#quick list


set.seed(1) # get the same random sample each time
ky_places <- places(state = "Kentucky") %>%
  filter(PCICBSA == "Y") %>% # principal cities only
  st_centroid() %>%
  mutate(NAME = if_else(NAME == "Louisville/Jefferson County metro government (balance)", "Louisville", NAME)) %>% # shorten the name for Louisville
  sample_n(15) # just get a random 10 places


county_stats_sf <- ky_counties %>%
  select(geoid) %>%
  left_join(county_stats %>% mutate(geoid = as.character(geoid)), by = c("geoid")) %>%
  mutate(mean_dl_mbps_wt = case_when(tests < 50 ~ NA_real_,
                                     TRUE ~ mean_dl_mbps_wt)) %>% # at least 50 tests
  mutate(dl_cat = cut(mean_dl_mbps_wt, c(0, 25, 50, 100, 150, 200), ordered_result = TRUE))

ggplot() +
  geom_sf(data = county_stats_sf, aes(fill = dl_cat), color = "gray20", lwd = 0.1) +
  geom_sf_text(data = ky_places, aes(label = NAME), color = "black", size = 3) +
  theme_void() +
  scale_fill_manual(values = brewer.pal(n = 6, name = "BuPu"),  
                    na.value = "gray80", 
                    labels = c("0 to 25", "25.1 to 50", "50.1 to 100", "100.1 to 150",
                               "150.1 to 200", "Insufficient data"), 
                    name = "Mean download speed (Mbps)", 
                    guide = guide_legend(direction = "horizontal",
                                         title.position = "top", nrow = 1, 
                                         label.position = "bottom", 
                                         keyheight = 0.5, keywidth = 5)) +
  theme(text = element_text(color = "gray25"),
        legend.position = "top")


sessionInfo()

# Texas
tx_counties <- tigris::counties(state = "Texas") %>%
  select(state_code = STATEFP, geoid = GEOID, name = NAME) %>% # only keep useful variables 
  st_transform(4326) # transform to the same CRS as the tiles


