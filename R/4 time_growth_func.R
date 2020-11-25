#' Growth Over Time
#'
#' time_growth streamlines the process for plotting longitudinal changes in
#' a variable over time.
#'
#' @param data a data frame. Default is Covid data.
#' @param states region to analyze.
#' @param sum take sum of all cases over states or treat states individually.
#' @param df_print print a data frame or not.
#'
#' @import dplyr ggplot2 maps
#' @export
#'
#' @return a data frame and ggplot graph
#' @examples
#' /dontrun{
#' time_growth(states="colorado", df_print=F)
#' time_growth(states=c("colorado", "nevada"), sum=T)
#' time_growth(states=c("colorado", "nevada", "florida", "california"), df_print=F) }

# Make function
time_growth <- function(data=covid, states=c(), sum=FALSE, df_print=TRUE){
  require(dplyr)
  require(ggplot2)

  #load data from data folder
  covid <- read.csv("data/US Covid (to Nov8).csv")
  covid$date <- as.Date(covid$date, format = "%Y-%m-%d")

  if(sum==TRUE){
    if(length(states)==0){
      filtered_data <- data %>%
        group_by(date) %>%
        mutate(combined_total = sum(tot_cases))
    } else{
      filtered_data <- data %>%
        filter(region %in% states) %>%
        group_by(date) %>%
        mutate(combined_total = sum(tot_cases))
    }

    returned_data <- filtered_data %>%
      select(date, combined_total)
    if(df_print==TRUE){
      print(as.data.frame(returned_data))}

    ggplot(filtered_data) +
      geom_line(aes(x=date, y=combined_total)) +
      ggtitle("Growth over Time (combined by state)") + xlab("Date") + ylab("Count")
  }
  else{
    if (length(states)==0){
      filtered_data <- data
    } else{
      filtered_data <- data %>%
        filter(region %in% states)
    }

    returned_data <- filtered_data %>%
      select(date, region, tot_cases, new_case, tot_death, new_death)
    if(df_print==TRUE){
      print(as.data.frame(returned_data))}

    filtered_data %>%
      group_by(region) %>%
      ggplot() +
      geom_line(aes(x=date, y=tot_cases, col=region)) +
      ggtitle("Growth over Time (by state)") + xlab("Date") + ylab("Count")
  }
}

time_growth(df_print=F, sum=T)
time_growth(sum=T)

time_growth(states="colorado")
time_growth(states="colorado", df_print=F)

time_growth(states=c("colorado", "nevada"), sum=T)
time_growth(states=c("colorado", "nevada"))

time_growth(states=c("colorado", "nevada", "florida", "california"), df_print=F)
