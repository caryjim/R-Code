
#' Reference: https://arilamstein.com/packages/ 
#' https://www.census.gov/data/academy/courses/choroplethr.html 
#' https://github.com/RConsortium/censusguide
fa_score <- read.csv("./Factor_Analysis/county-level-fa-regression-barlett.csv", header = T)

library(dplyr)
library(choroplethr)
library(choroplethrMaps)


# region is the county.id 
# value is the score to code 
names(fa_score)

score <- fa_score[, c(1, 28)]
colnames(score) <- c("region", "value")
score <- distinct(score, region, .keep_all = TRUE)

county_choropleth(score, title = "Digital Opportunity for Students across County",
                  legend = "Score",
                  num_colors = 0)


data(county.regions)
data(df_pop_county)



# show the population of the 5 counties (boroughs) that make up New York City
nyc_county_names = c("kings", "bronx", "new york", "queens", "richmond")
nyc_county_fips = county.regions %>%
  filter(state.name == "new york" & county.name %in% nyc_county_names) %>%
  select(region)
county_choropleth(df_pop_county, 
                  title        = "Population of Counties in New York City",
                  legend       = "Population",
                  num_colors   = 1,
                  county_zoom = nyc_county_fips$region)