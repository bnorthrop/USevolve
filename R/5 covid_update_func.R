#' covid_update
#'
#' covid_update downloads and cleans the latest COVID-19 data from the CDC website.
#'
#' @param
#'
#' @import dplyr plyr
#'
#' @return a data frame
#' @examples
#' library(USevolve)
#' covid_data <- covid_update()

covid_update <- function(){
  require(dplyr)
  require(plyr)

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

  covid <- subset(covid, region %notin% c("RMI", "AS", "NYC", "PR", "DC",
                                          "HI", "AK", "GU", "VI", "MP", "FSM", "PW"))

  covid <- covid %>% select(-c(conf_cases, prob_cases, pnew_case,
                               conf_death, prob_death, pnew_death,
                               created_at, consent_cases, consent_deaths))

  covid$date <- as.Date(covid$date, format="%m/%d/%Y")

  # Find total US cases from state sums
  df <- data.frame(matrix(vector(), 0, 7,
                          dimnames=list(c(), c("date", "region", "tot_cases",
                                               "new_case", "tot_death", "new_death", "us_total"))))
  for (i in 1:length(unique(covid$date))){
    filtered_data <- covid %>%
      filter(date==date[[i]]) %>%
      mutate(us_total = sum(tot_cases))

    df <- rbind(df, filtered_data)

    filtered_data <- data.frame(matrix(vector(), 0, 7))
  }
  return(df)
}

covid <- covid_update()


####################

# library(ggplot2)
# library(maps)
# library(plotly)
#
# state_map <- read.csv("data/state_map.csv")
#
# date_filt <- covid %>% filter(date=="2020-12-02")
#
# ggplot()+
#   geom_map(data=state_map, aes(map_id= region), map = state_map) +
#   geom_map(data=date_filt, aes(map_id= region, fill=state_total), map = state_map) +
#   expand_limits(x = state_map$long, y = state_map$lat) +
#   scale_fill_distiller("Case Count", palette="YlOrRd", direction=1) +
#   coord_map("albers", lat0=30, lat1=40) +
#   ggtitle("Covid Count Map")

# Way to improve plotly option?
# ggplotly(map_plot)




