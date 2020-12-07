#' covid_update
#'
#' covid_update downloads and cleans the latest COVID-19 data from the CDC website.
#'
#' @import dplyr plyr
#' @keywords internal
###############################
#
# @return a data frame
# @examples
# library(USevolve)
# covid_data <- covid_update()

covid_update <- function(){
  require(dplyr)
  require(plyr)

  covid <- read.csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD")

  names(covid)[names(covid) == "submission_date"] <- "date"
  covid$date <- as.Date(covid$date, format="%m/%d/%Y")

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

  # Find total US cases from state sums
  df <- data.frame(matrix(vector(), 0, 10,
                          dimnames=list(c(), c("date", "region", "tot_cases",
                                               "new_cases", "tot_death", "new_death", "us_tot_cases",
                                               "us_new_cases", "us_tot_death", "us_new_death"))))

  for (i in 1:length(unique(covid$date))){
    filtered_data <- covid %>%
      filter(date==date[[i]]) %>%
      mutate(us_tot_cases = sum(tot_cases),
             us_new_cases = sum(new_case),
             us_tot_death = sum(tot_death),
             us_new_death = sum(new_death))

    df <- rbind(df, filtered_data)

    filtered_data <- data.frame(matrix(vector(), 0, 10))
  }
  covid <- df
}

# covid <- covid_update()
#
# covid_update()


