library(geojsonio)

json_states <- geojson_read("/Users/blakenorthrop/Desktop/QAC356/finalpackageprep/Data/geojson_states.json",  what = "sp")
json_counties <- geojson_read("/Users/blakenorthrop/Desktop/QAC356/finalpackageprep/Data/geojson_counties.json",  what = "sp")

library(broom)
json_states <- tidy(json_states)

library(ggplot2)

ggplot() +
  geom_polygon(data = json_states, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() +
  coord_map()

