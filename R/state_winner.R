#' State Winner
#'
#' state_winner displays a chloropleth map of United States presidential election
#' results by party on the state level for a given year.
#'
#' @param Year United States Presidential Election year starting in 1976.
#' @param states region to plot.
#' @param label legend labels.
#'
#' @import dplyr ggplot2 maps
#' @export
#'
#' @seealso [USevolve::county_winner()]
#'
#' @return a ggplot map object.
#' @examples
#' state_winner(Year = 1988)
#' state_winner(states=c("california", "nevada", "arizona", "utah"))

# Create function
state_winner <- function(Year=2016, states=c(), label="candidate"){

  state_map <- USevolve:::state_map
  state_pres <- USevolve:::state_pres
  state_pres$party[state_pres$party=="democratic-farmer-labor"] <- "democrat"

  state_election_years <- c(1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)
  if(Year %in% state_election_years){

    ############
    if(length(states)==0){
      state_majority <- state_pres %>%
        filter(year==Year) %>%
        group_by(region) %>%
        slice_max(state_percent)
    } else{
      state_map <- state_map %>%
        filter(region %in% states)
      state_majority <- state_pres %>%
        filter(region %in% states, year==Year) %>%
        group_by(region) %>%
        slice_max(state_percent)
    }

    state_majority$party[state_majority$party != "democrat" & state_majority$party != "republican"] <- "other"
    state_majority$candidate[state_majority$party != "democrat" & state_majority$party != "republican"] <- "other"


    state_majority_alphabet <- state_majority[order(state_majority$candidate), ]

    if(state_majority_alphabet$party[[1]]=="democrat"){
      partycolor <- c("blue2", "red1")
    }
    else if(state_majority_alphabet$party[[1]]=="republican"){
      partycolor <- c("red1", "blue2")
    }

    if(label=="party"){
      names(partycolor) <- unique(state_majority_alphabet$party)

      ggplot()+
        geom_map(data=state_map, aes(map_id= region), map = state_map) +
        geom_map(data=state_majority, aes(map_id= region, fill=party), map = state_map) +
        expand_limits(x = state_map$long, y = state_map$lat) +
        scale_fill_manual(name = "Party", values = partycolor) +
        coord_map("albers", lat0=30, lat1=40) +
        ggtitle("State Winner Map", Year) + xlab("Longitude") + ylab("Latitude")
    }
    else{
      names(partycolor) <- unique(state_majority_alphabet$candidate)

      ggplot()+
        geom_map(data=state_map, aes(map_id= region), map = state_map) +
        geom_map(data=state_majority, aes(map_id= region, fill=candidate), map = state_map) +
        expand_limits(x = state_map$long, y = state_map$lat) +
        scale_fill_manual("Candidate",
                          values = partycolor) +
        coord_map("albers", lat0=30, lat1=40) +
        ggtitle("State Winner Map", Year) + xlab("Longitude") + ylab("Latitude")
    }

  }
  else if(Year == 2020){
    stop("Data not yet available")
  }
  else{
    stop("Must enter valid election year starting in 1976")
  }
}

# state_winner()
# state_winner(label="party")
#
# state_winner(Year = 1988)
# state_winner(Year = 2008)
# state_winner(Year = 2004)
#
# state_winner(Year = 2000, states=c("texas", "oklahoma"))
# state_winner(Year = 2000, states=c("texas", "oklahoma"), label="party")

