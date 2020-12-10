#' County Winner
#'
#' county_winner displays a chloropleth map of United States presidential election
#' results by party on the county level for a given year.
#'
#' @param Year United States Presidential Election year starting in 2000.
#' @param states region to plot.
#' @param label legend labels.
#'
#' @import dplyr ggplot2 maps devtools
#' @export
#'
#' @seealso [USevolve::state_winner()]
#'
#' @return a ggplot map object.
#' @examples
#' county_winner(Year = 2008)
#' county_winner(Year = 2000, states=c("texas", "oklahoma"))

# Create Function
county_winner <- function(Year=2016, states=c(), label="candidate"){

  # load data
  county_map <- USevolve:::county_map
  county_pres <- USevolve:::county_pres

  county_election_years <- c(2000, 2004, 2008, 2012, 2016)
  if(Year %in% county_election_years){

    county_pres <- na.omit(county_pres)
    county_pres$party[county_pres$party != "democrat" & county_pres$party != "republican"] <- "other"
    county_pres$candidate[county_pres$party != "democrat" & county_pres$party != "republican"] <- "other"



    partycolor <- c("blue2", "red1", "yellow")
    names(partycolor) <- unique(county_pres$party)
    ###########
    if(length(states)==0){
      county_majority <- county_pres %>%
        filter(year==Year) %>%
        group_by(region) %>%
        slice_max(county_percent)
    } else{
      county_map <- county_map %>%
        filter(state %in% states)
      county_majority <- county_pres %>%
        filter(state %in% states, year==Year) %>%
        group_by(region) %>%
        slice_max(county_percent)
    }

    county_majority_alphabet <- county_majority[order(county_majority$candidate), ]

    if(county_majority_alphabet$party[[1]]=="democrat"){
      partycolor <- c("blue2", "red1")
    }
    else if(county_majority_alphabet$party[[1]]=="republican"){
      partycolor <- c("red1", "blue2")
    }

    if(label=="party"){
      names(partycolor) <- unique(county_majority_alphabet$party)


      ggplot()+
        geom_map(data=county_map, aes(map_id= region), map = county_map) +
        geom_map(data=county_majority, aes(map_id= region, fill=party), map = county_map) +
        expand_limits(x = county_map$long, y = county_map$lat) +
        scale_fill_manual(name = "Party", values = partycolor) +
        coord_map("albers", lat0=30, lat1=40) +
        ggtitle("County Winner Map", Year) + xlab("Longitude") + ylab("Latitude")
    }
    else{
      names(partycolor) <- unique(county_majority_alphabet$candidate)

      ggplot()+
        geom_map(data=county_map, aes(map_id= region), map = county_map) +
        geom_map(data=county_majority, aes(map_id= region, fill=candidate), map = county_map) +
        expand_limits(x = county_map$long, y = county_map$lat) +
        scale_fill_manual(name = "Candidate", values = partycolor) +
        coord_map("albers", lat0=30, lat1=40) +
        ggtitle("County Winner Map", Year) + xlab("Longitude") + ylab("Latitude")
    }

  }
  else if(Year == 2020){
    stop("Data not yet available")
  }
  else{
    stop("Must enter valid election year starting in 2000")
  }
}

# county_winner()
# county_winner(label="party")
#
# county_winner(Year = 2008)
# county_winner(Year = 2008, states="california")
#
# county_winner(Year = 2000, states=c("texas", "oklahoma"))







