#' Covid Growth Over Time
#'
#' covid_growth streamlines the process for plotting longitudinal changes in
#' a variable over time.
#'
#' @param data a data frame. Default is Covid data.
#' @param states region to analyze.
#' @param sum take sum of all cases over states or treat states individually.
#' @param df_print print a data frame or not.
#'
#' @import dplyr ggplot2 maps plotly
#' @export
#'
#' @return a list (plotly object and htmlwidget) CHECK
#' @examples
#' covid_growth(states="colorado", df_print=F, sum=F)
#' covid_growth(states=c("colorado", "nevada"))
#' covid_growth(states=c("colorado", "nevada", "florida", "california"), df_print=F, sum=F)


# GET TO PROPERLY USE covid_update FUNCTION
covid_growth <- function(states=c(), Value, combined=T, df_print=FALSE, ...){
  require(dplyr)
  require(ggplot2)
  require(plotly)

  #load latest covid data
  covid <- USevolve:::covid_update()

  if(missing(Value)){
    Value <- "tot_cases"
  }

  if(combined==TRUE){
    if(length(states)==0){
      if(Value=="tot_cases"){
        filtered_data <- covid %>%
          group_by(date) %>%
          mutate(combined_total = sum(tot_cases)) }
      else if(Value=="new_cases"){
        filtered_data <- covid %>%
          group_by(date) %>%
          mutate(combined_total = sum(new_case)) }
      else if(Value=="tot_death"){
        filtered_data <- covid %>%
          group_by(date) %>%
          mutate(combined_total = sum(tot_death)) }
      else if(Value=="new_death"){
        filtered_data <- covid %>%
          group_by(date) %>%
          mutate(combined_total = sum(new_death)) }
    }
    else if(Value=="tot_cases"){
      filtered_data <- covid %>%
        filter(region %in% states) %>%
        group_by(date) %>%
        mutate(combined_total = sum(tot_cases)) }
    else if(Value=="new_cases"){
      filtered_data <- covid %>%
        filter(region %in% states) %>%
        group_by(date) %>%
        mutate(combined_total = sum(new_case)) }
    else if(Value=="tot_death"){
      filtered_data <- covid %>%
        filter(region %in% states) %>%
        group_by(date) %>%
        mutate(combined_total = sum(tot_death)) }
    else if(Value=="new_death"){
      filtered_data <- covid %>%
        filter(region %in% states) %>%
        group_by(date) %>%
        mutate(combined_total = sum(new_death)) }

    returned_data <- filtered_data %>%
      select(date, combined_total)
    if(df_print==TRUE){
      print(as.data.frame(returned_data))}

    plot <- ggplot(filtered_data) +
      geom_line(aes(x=date, y=combined_total), ...) +
      ggtitle("Total Covid Cases - United States") + xlab("Date") + ylab("Count")
    ggplotly(plot)
  }
  else{
    if (length(states)==0){
      filtered_data <- covid
    } else{
      filtered_data <- covid %>%
        filter(region %in% states)
    }

    returned_data <- filtered_data %>%
      select(date, region,
             # tot_cases,
             new_case)
             # tot_death, new_death)
    if(df_print==TRUE){
      print(as.data.frame(returned_data))}

    if(Value=="tot_cases"){
      plot <- filtered_data %>%
        group_by(region) %>%
        ggplot(...) +
        geom_line(aes(x=date, y=tot_cases, col=region)) +
        ggtitle("Total Covid Cases by State") + xlab("Date") + ylab("Count")
      ggplotly(plot) }
    else if(Value=="new_cases"){
      plot <- filtered_data %>%
        group_by(region) %>%
        ggplot(...) +
        geom_line(aes(x=date, y=new_case, col=region)) +
        ggtitle("New Covid Cases by State") + xlab("Date") + ylab("Count")
      ggplotly(plot) }
    else if(Value=="tot_death"){
      plot <- filtered_data %>%
        group_by(region) %>%
        ggplot(...) +
        geom_line(aes(x=date, y=tot_death, col=region)) +
        ggtitle("Total Covid Deaths by State") + xlab("Date") + ylab("Count")
      ggplotly(plot) }
    else if(Value=="new_death"){
      plot <- filtered_data %>%
        group_by(region) %>%
        ggplot(...) +
        geom_line(aes(x=date, y=new_death, col=region)) +
        ggtitle("New Covid Deaths by State") + xlab("Date") + ylab("Count")
      ggplotly(plot) }

  }
}


# covid_growth(df_print=T, combined=T)
# covid_growth(Value="tot_cases", df_print=F, combined=T)
# covid_growth(Value="new_cases", df_print=F, combined=T)
# covid_growth(Value="new_death", df_print=F, combined=T)
# covid_growth(Value="tot_death", df_print=F, combined=T)
# covid_growth(combined=T)
# #
# # covid_growth(states="colorado")
# # covid_growth(states=c("colorado", "wyoming", "virginia"), df_print=F)
# #
# covid_growth(states=c("colorado", "nevada"), combined=T)
# covid_growth(states=c("colorado", "nevada"))
# #
# covid_growth(states=c("colorado", "nevada", "florida", "california"), combined=T)
# covid_growth(Value="new_cases", states=c("colorado", "nevada", "florida", "california"), df_print=F)
# covid_growth(Value="tot_death", states=c("colorado", "nevada", "florida", "california"), df_print=F)





