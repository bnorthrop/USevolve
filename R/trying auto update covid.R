install.packages("covid19.analytics")
library(covid19.analytics)
library(reshape2)
library(dplyr)
library(ggplot2)
library(maps)
library(plotly)

data <- covid19.US.data()
data <- data %>% select(-c(Country_Region, Lat, Long_))

data_long <- data %>% melt(id.vars="Province_State")

data_long <- rename(data_long,
       region = Province_State,
       date = variable,
       day_count = value)
data_long$region <- tolower(data_long$region)
data_long$date <- as.Date(data_long$date, format="%Y-%m-%d")

data_long <- data_long %>%
  group_by(region, date) %>%
  mutate(state_total = sum(day_count))

data_long <- data_long %>%
  group_by(region, state_total) %>%
  distinct(date)

data_long <- data_long %>%
  group_by(date) %>%
  mutate(US_total = sum(state_total))

# TOTAL US CASES
ggplot(data_long)+
  geom_line(aes(x=date, y=US_total))

# TOTAL CALIFORNIA CASES
ca_covid <- data_long %>% filter(region == "california")
ggplot(ca_covid)+
  geom_line(aes(x=date, y=state_total))

##############################

state_map <- read.csv("data/state_map.csv")

date_filt <- data_long %>% filter(date=="2020-12-02")

# map_plot <-
ggplot()+
  geom_map(data=state_map, aes(map_id= region), map = state_map) +
  geom_map(data=date_filt, aes(map_id= region, fill=state_total), map = state_map) +
  expand_limits(x = state_map$long, y = state_map$lat) +
  scale_fill_distiller("Case Count", palette="YlOrRd", direction=1) +
  coord_map("albers", lat0=30, lat1=40) +
  ggtitle("Covid Count Map")

# Way to improve plotly option?
# ggplotly(map_plot)





