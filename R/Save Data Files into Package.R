
state_map <- read.csv("/Users/blakenorthrop/Desktop/QAC356/USevolve/data/state_map.csv")
county_map <- read.csv("/Users/blakenorthrop/Desktop/QAC356/finalpackageprep/Data/county_map.csv")

# Load data used to fill map
statepres <- read.csv("/Users/blakenorthrop/Desktop/QAC356/USevolve/data/3 state president.csv")
countypres <- read.csv("/Users/blakenorthrop/Desktop/QAC356/USevolve/data/2 county president.csv")

covid <- read.csv("/Users/blakenorthrop/Desktop/QAC356/USevolve/data/US Covid (to Nov8).csv")

library(geojsonio)

json_states <- geojson_read("/Users/blakenorthrop/Desktop/QAC356/USevolve/data/geojson_states.json",  what = "sp")
json_counties <- geojson_read("/Users/blakenorthrop/Desktop/QAC356/USevolve/data/geojson_counties.json",  what = "sp")

##################

# save(state_map, file= "data/state_map.rda", compress = "xyz")
