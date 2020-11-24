require(dplyr)
require(ggplot2)
require(maps)

state_map <- read.csv("/Users/blakenorthrop/Desktop/QAC356/Final Project Prep/Map Data/state_map.csv")

statepres <- read.csv("/Users/blakenorthrop/Desktop/QAC356/Final Project Prep/Election Data/3 state president.csv")

########################

# Create function
state_winner <- function(Year=2016, states=c()){
  state_election_years <- c(1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)
  if(Year %in% state_election_years){
    
    statepres$party[statepres$party != "democrat" & statepres$party != "republican"] <- "other"
    partycolor <- c("blue2", "red1", "yellow")
    names(partycolor) <- unique(statepres$party)
    ############
    if(length(states)==0){
      state_majority <- statepres %>% 
        filter(year==Year) %>% 
        group_by(region) %>% 
        slice_max(state_percent)
    } else{
      state_map <- state_map %>% 
        filter(region %in% states)
      state_majority <- statepres %>% 
        filter(region %in% states, year==Year) %>% 
        group_by(region) %>% 
        slice_max(state_percent)
    }
    
    ggplot()+
      geom_map(data=state_map, aes(map_id= region), map = state_map) +
      geom_map(data=state_majority, aes(map_id= region, fill=party), map = state_map) +
      expand_limits(x = state_map$long, y = state_map$lat) +
      scale_fill_manual(name = "party", values = partycolor) +
      coord_map("albers", lat0=30, lat1=40) +
      ggtitle("State Winner Map")
  } 
  else if(Year == 2020){
    stop("Data not yet available")
  }
  else{
    stop("Must enter valid election year starting in 1976")
  }
}

state_winner()

state_winner(Year = 1988)
state_winner(Year = 1990)
state_winner(Year = 2020)

state_winner(Year = 2000, states=c("texas", "oklahoma"))

