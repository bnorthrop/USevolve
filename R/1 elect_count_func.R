#' Election Count
#'
#' elect_count creates a map of the United States with fill color based on the specified parameter.
#'
#' @param level what region size to visualize (state or county).
#' @param Year year. For built in election data, year must align with US election years.
#' @param states list of states user wants to visualize.
#' @param log fill based on log of count.
#'
#' @import dplyr ggplot2 maps
#' @export
#'
#' @return a ggplot map
#' @examples
#' /dontrun{
#' elect_count()
#' elect_count(data="county", states = c("connecticut", "rhode island"), log=F)
#' }

# Create restrictions on year based on what data set user calls
## if statement with error message if year not in specific list of election years for that state/county

# Note: Does not work with longitudinal data (e.g. Covid changes over months)

elect_count <- function(level="state", Year, states=c(), log=TRUE){
  require(dplyr)
  require(ggplot2)
  require(maps)
  # Load map data
  state_map <- read.csv("data/state_map.csv")
  county_map <- read.csv("data/county_map.csv")
  # Load data used to fill map
  statepres <- read.csv("data/3 state president.csv")
  countrypres <- read.csv("data/2 county president.csv")

  if (level=="state" & missing(Year)){
    Year=2016 }
  if (level=="state"){
    election_years <- c(1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016) }
  if(level=="county"){
    Year=2016
    election_years <- c(2000, 2004, 2008, 2012, 2016) }
  if(Year %in% election_years){
    if(level=="county"){
      if (length(states)==0){
        filtered_map <- county_map

        filtered_results <- countypres %>%
          filter(year==Year) %>%
          group_by(region) }
      else {
        filtered_map <- county_map %>%
          filter(state %in% states)

        filtered_results <- countypres %>%
          filter(state %in% states, year==Year) %>%
          group_by(region) }
    }
    if(level=="state"){
      if (length(states)==0){
        filtered_map <- state_map

        filtered_results <- statepres %>%
          filter(year==Year) %>%
          group_by(region)
      } else{
        filtered_map <- state_map %>%
          filter(state %in% states)

        filtered_results <- statepres %>%
          filter(state %in% states, year==Year) %>%
          group_by(region) }
    }

  }
  else if (Year == 2020){
    stop("Data not yet available") }
  else if (Year %in% 2020 == F){
    stop("Must enter a valid election year for region") }
  if(log==FALSE){
    ggplot()+
      geom_map(data=filtered_map, aes(map_id= region), map = filtered_map) +
      geom_map(data=filtered_results, aes(map_id= region, fill=totalvotes), map = filtered_map) +
      expand_limits(x = filtered_map$long, y = filtered_map$lat) +
      scale_fill_distiller("Voter Count", palette="YlOrRd", direction=1) +
      coord_map("albers", lat0=30, lat1=40) +
      ggtitle("Count Map") }
  else{
    ggplot()+
      geom_map(data=filtered_map, aes(map_id= region), map = filtered_map) +
      geom_map(data=filtered_results, aes(map_id= region, fill=totalvotes), map = filtered_map) +
      expand_limits(x = filtered_map$long, y = filtered_map$lat) +
      scale_fill_distiller("Voter Count (Log)", palette="YlOrRd", direction=1, trans="log") +
      coord_map("albers", lat0=30, lat1=40) +
      ggtitle("Count Map") }
}

elect_count()
elect_count(Year=2000, log=F)
# elect_count(Year=2020)
# elect_count(Year=2019)
#
# elect_count(data="county", states = c("connecticut", "rhode island"))
# elect_count(data="county", states = c("connecticut", "rhode island"), log=F)
# elect_count(data="county", Year= 1996, states = c("connecticut", "rhode island"))



