#' covid_map
#'
#' FUNCTION_DESCRIPTION
#'
#' @param Value DESCRIPTION.
#' @param Date DESCRIPTION.
#'
#' @import dplyr ggplot2
#' @export
#'
#' @return a ggplot object. CHECK
#' @examples
#' library(USevolve)
#' covid_map(Value="new_death", Date="2020-07-04")

covid_map <- function(Value, Date="2020-12-02"){
  require(dplyr)
  require(ggplot2)

  Date <- as.Date(as.character(Date), format = "%Y-%m-%d")
  state_map <- read.csv("data/state_map.csv")

  if(missing(Value)){
    Value <- date_filt$tot_cases }
  else if(Value=="tot_cases"){
    Value <- date_filt$tot_cases }
  else if(Value=="new_cases"){
    Value <- date_filt$new_case }
  else if(Value=="tot_death"){
    Value <- date_filt$tot_death }
  else if(Value=="new_death"){
    Value <- date_filt$new_death }

  covid <- covid_update()
  if(!missing(Date)){
    date_filt <- covid %>% filter(date==Date)
  }else{
    date_filt <- covid
  }

  ggplot()+
    geom_map(data=state_map, aes(map_id= region), map = state_map) +
    geom_map(data=date_filt, aes(map_id= region, fill=Value), map = state_map) +
    expand_limits(x = state_map$long, y = state_map$lat) +
    scale_fill_distiller("Case Count", palette="YlOrRd", direction=1) +
    coord_map("albers", lat0=30, lat1=40) +
    ggtitle("Covid Count Map - ", Date)

}

covid_map()

covid_map(Value="new_cases")
covid_map(Value="new_death", Date="2020-07-04")

