#' County Winner
#'
#' county_winner displays a map of presidential election results by party on the county level
#'
#' @param Year United States Presidential Election year starting in 2000.
#' @param states region to plot.
#'
#' @import dplyr ggplot2 maps devtools
#' @export
#'
#' @return a ggplot map object.
#' @examples
#' county_winner(Year = 2008)
#' county_winner(Year = 2000, states=c("texas", "oklahoma"))

# Create Function
county_winner <- function(Year=2016, states=c()){
  require(maps)
  require(dplyr)
  require(ggplot2)
  require(devtools)
  # load data
  county_map <- USevolve:::county_map
  county_pres <- USevolve:::county_pres

  county_election_years <- c(2000, 2004, 2008, 2012, 2016)
  if(Year %in% county_election_years){

    county_pres <- na.omit(county_pres)
    county_pres$party[county_pres$party != "democrat" & county_pres$party != "republican"] <- "other"
    partycolor <- c("blue2", "red1", "yellow")
    names(partycolor) <- unique(county_pres$party)
    ###########
    if(length(states)==0){
      county_majority <- county_pres %>%
        filter(year==Year) %>%
        group_by(region) %>%
        slice_max(county_percent)     ## ISSUE IN SLICE_MAX
    } else{
      county_map <- county_map %>%
        filter(state %in% states)
      county_majority <- county_pres %>%
        filter(state %in% states, year==Year) %>%
        group_by(region) %>%
        slice_max(county_percent)
    }

    ggplot()+
      geom_map(data=county_map, aes(map_id= region), map = county_map) +
      geom_map(data=county_majority, aes(map_id= region, fill=party), map = county_map) +
      expand_limits(x = county_map$long, y = county_map$lat) +
      scale_fill_manual(name = "party", values = partycolor) +
      coord_map("albers", lat0=30, lat1=40) +
      ggtitle("County Winner Map", Year)
  }
  else if(Year == 2020){
    stop("Data not yet available")
  }
  else{
    stop("Must enter valid election year starting in 2000")
  }
}

# FIX DATA READING ERROR
#### Error in file(file, "rt") : cannot open the connection

# county_winner()
#
# county_winner(Year = 2008)
# county_winner(Year = 1996)
#
# county_winner(Year = 2000, states=c("texas", "oklahoma"))







