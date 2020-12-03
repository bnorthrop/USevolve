# install.packages("covid19.analytics")
# library(covid19.analytics)
# library(reshape2)
# library(dplyr)

covid_update <- function(){
  covid <- read.csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD")

  names(covid)[names(covid) == "submission_date"] <- "date"
  names(covid)[names(covid) == "state"] <- "region"

  covid$region <- revalue(covid$region, c("AL" = "alabama",
                                          "AZ" = "arizona",
                                          "AR"="arkansas",
                                          "CA"="california",
                                          "CO"="colorado",
                                          "CT"="connecticut",
                                          "DE"="delaware",
                                          "FL"="florida",
                                          "GA"="georgia",
                                          "ID"="idaho",
                                          "IL"="illinois",
                                          "IN"="indiana",
                                          "IA"="iowa",
                                          "KS"="kansas",
                                          "KY"="kentucky",
                                          "LA"="louisiana",
                                          "ME"="maine",
                                          "MD"="maryland",
                                          "MA"="massachusetts",
                                          "MI"="michigan",
                                          "MN"="minnesota",
                                          "MS"="mississippi",
                                          "MO"="missouri",
                                          "MT"="montana",
                                          "NE"="nebraska",
                                          "NV"="nevada",
                                          "NH"="new hampshire",
                                          "NJ"="new jersey",
                                          "NM"="new mexico",
                                          "NY"="new york",
                                          "NC"="north carolina",
                                          "ND"="north dakota",
                                          "OH"="ohio",
                                          "OK"="oklahoma",
                                          "OR"="oregon",
                                          "PA"="pennsylvania",
                                          "RI"="rhode island",
                                          "SC"="south carolina",
                                          "SD"="south dakota",
                                          "TN"="tennessee",
                                          "TX"="texas",
                                          "UT"="utah",
                                          "VT"="vermont",
                                          "VA"="virginia",
                                          "WA"="washington",
                                          "WV"="west virginia",
                                          "WI"="wisconsin",
                                          "WY"="wyoming"))

  `%notin%` <- Negate(`%in%`)

  covid <- subset(covid, region %notin% c("RMI", "AS", "NYC", "PR", "DC", "HI", "AK", "GU", "VI", "MP", "FSM", "PW"))

}



covid <- covid_update()

library(ggplot2)
# TOTAL US CASES
ggplot(covid)+
  geom_line(aes(x=date, y=tot_cases))

# TOTAL CALIFORNIA CASES
ca_covid <- covid %>% filter(region == "california")
ggplot(ca_covid)+
  geom_line(aes(x=date, y=state_total))

##############################

library(maps)
library(plotly)

state_map <- read.csv("data/state_map.csv")

date_filt <- covid %>% filter(date=="2020-12-02")

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




# covid_update <- function(){
#   require(covid19.analytics)
#   require(reshape2)
#   require(dplyr)
#
#   data <- covid19.US.data()
#   data <- data %>% select(-c(Country_Region, Lat, Long_))
#
#   data_long <- data %>% melt(id.vars="Province_State")
#
#   data_long <- rename(data_long,
#                       region = Province_State,
#                       date = variable,
#                       day_count = value)
#   data_long$region <- tolower(data_long$region)
#   data_long$date <- as.Date(data_long$date, format="%Y-%m-%d")
#
#   data_long <- data_long %>%
#     group_by(region, date) %>%
#     mutate(state_total = sum(day_count))
#
#   data_long <- data_long %>%
#     group_by(region, state_total) %>%
#     distinct(date)
#
#   data_long <- data_long %>%
#     group_by(date) %>%
#     mutate(US_total = sum(state_total))
#
#   covid_updated <- data_long
#   return(covid_updated)
# }


