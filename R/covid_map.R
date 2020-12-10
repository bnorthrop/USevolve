#' Covid Map
#'
#' covid_map automatically loads the latest COVID-19 data from the CDC and plots a chloropleth
#' map of the United States based on the fill value the user specifies. Fill options include
#' total cases ("tot_cases"), new cases ("new_cases"), total deaths ("tot_death"), and
#' new deaths ("new_death").
#'
#' @param value covid statistic to analyze.
#' @param Date a date in form "YYYY-MM-DD".  --> CHOOSE DEFAULT
#' @param pal a color palette.
#' @param ... pass other arguments
#'
#' @import dplyr ggplot2
#' @export
#'
#' @return a ggplot map object.
#' @examples
#' covid_map(Value="new_death", Date="2020-12-01")

covid_map <- function(value, Date="2020-12-02", pal="YlOrRd", ...){
  require(dplyr)
  require(ggplot2)

  Date <- as.Date(as.character(Date), format = "%Y-%m-%d")
  # state_map <- read.csv("data/state_map.csv")
  state_map <- USevolve:::state_map

  covid <- covid_update()
  if(!missing(Date)){
    date_filt <- covid %>% filter(date==Date)
  }else{
    date_filt <- covid
  }

  if(missing(value)){
    Value <- date_filt$tot_cases }
  else if(value=="tot_cases"){
    Value <- date_filt$tot_cases }
  else if(value=="new_cases"){
    Value <- date_filt$new_case }
  else if(value=="tot_death"){
    Value <- date_filt$tot_death }
  else if(value=="new_death"){
    Value <- date_filt$new_death }

  ggplot(...)+
    geom_map(data=state_map, aes(map_id= region), map = state_map) +
    geom_map(data=date_filt, aes(map_id= region, fill=Value), map = state_map) +
    expand_limits(x = state_map$long, y = state_map$lat) +
    scale_fill_distiller("Case Count", palette=pal, direction=1) +
    coord_map("albers", lat0=30, lat1=40) +
    ggtitle("Covid Count Map - ", Date) + xlab("Longitude") + ylab("Latitude")

}

#covid_map()
#
# covid_map(Value="new_cases")
# covid_map(Value="new_death", Date="2020-07-04")

