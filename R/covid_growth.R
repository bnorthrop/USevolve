#' Covid Growth Over Time
#'
#' covid_growth streamlines the process for plotting a line graph to analyze
#' longitudinal changes in a user-selected variable over time. Variable options include
#' total cases ("tot_cases"), new cases ("new_cases"), total deaths ("tot_death"), and
#' new deaths ("new_death").
#'
#' @param states region to analyze.
#' @param value covid statistic to analyze.
#' @param sum take sum of all cases over states or treat states individually.
#' @param df_print print a data frame or not.
#' @param interact whether additional information will display when user hovers over image (use ggplot or plotly)
#' @param ... pass other arguments
#'
#' @import plyr dplyr ggplot2 maps plotly
#' @export
#'
#' @seealso [ggplot2::ggplot()]
#'
#' @return a plotly or ggplot object.
#' @examples
#' covid_growth(states="colorado", df_print=FALSE, sum=FALSE)
#' covid_growth(states=c("colorado", "nevada"))
#' covid_growth(states=c("colorado", "nevada", "florida", "california"), df_print=FALSE, sum=FALSE)
#' covid_growth(states=c("colorado", "nevada", "florida", "california"), interact=FALSE)

covid_growth <- function(states=c(), value, sum=FALSE, df_print=FALSE, interact=TRUE, ...) {
  require(plyr)
  require(dplyr)
  require(ggplot2)
  require(plotly)
  #load latest covid data
  covid <<- covid_update()

  if(missing(value)){
    value <- "tot_cases" }
  `%notin%` <- Negate(`%in%`)
  if(value %notin% c("tot_cases", "new_cases", "tot_death", "new_death")){
    stop("Please print valid value (statistic) to measure")
  }

  # SUM IS TRUE
  if(sum==TRUE) {
    # Combined for all states
    if(length(states)==0 | missing(states)){
      if(value=="tot_cases"){
        Value <- covid$us_tot_cases }
      else if(value=="new_cases"){
        Value <- covid$us_new_cases }
      else if(value=="tot_death"){
        Value <- covid$us_tot_death }
      else if(value=="new_death"){
        Value <- covid$us_new_death }
      else{
        stop("Please print valid value (statistic) to measure.") }

      if(df_print==TRUE){
        returned_data <- covid %>%
          select(date, value)
        print(as.data.frame(returned_data)) }

      if(interact==TRUE){
        plotgraph <- ggplot(covid) +
          geom_line(aes(x=date, y=Value)) +
          ggtitle("Total Count (United States) - ", value) + xlab("Date") + ylab("Count")
        ggplotly(plotgraph)
      }
      else if(interact==FALSE){
        ggplot(covid) +
          geom_line(aes(x=date, y=Value)) +
          ggtitle("Total Count (United States) - ", value) + xlab("Date") + ylab("Count")
      }
    }
    # Combined for select states
    else {
      filtered_data <- covid %>%
        filter(region %in% states)

      # Find state sums
      df <- data.frame(matrix(vector(), 0, 14,
                              dimnames=list(c(), c("date", "region", "tot_cases",
                                                   "new_case", "tot_death", "new_death", "us_total_cases",
                                                   "us_new_cases", "us_total_death", "us_new_death",
                                                   "subset_tot_cases", "subset_new_cases",
                                                   "subset_tot_death", "subset_new_death"))))

      n <- length(states)*length(unique(filtered_data$date))
      for (i in 1:n){
        subset_data <- filtered_data %>%
          filter(date==date[[i]]) %>%
          mutate(subset_tot_cases = sum(tot_cases),
                 subset_new_cases = sum(new_case),
                 subset_tot_death = sum(tot_death),
                 subset_new_death = sum(new_death))

        df <- rbind(df, subset_data)

        subset_data <- data.frame(matrix(vector(), 0, 14))
      }
      filtered_data <- df

      if(value=="tot_cases"){
        Value <- filtered_data$subset_tot_cases }
      else if(value=="new_cases"){
        Value <- filtered_data$subset_new_cases }
      else if(value=="tot_death"){
        Value <- filtered_data$subset_tot_death }
      else if(value=="new_death"){
        Value <- filtered_data$subset_new_death }
      else{
        stop("Please print valid value (statistic) to measure.") }

      if(df_print==TRUE){
        returned_data <- filtered_data %>%
          select(date, value)
        print(as.data.frame(returned_data)) }

      if(interact==TRUE){
        plotgraph <- ggplot(filtered_data) +
          geom_line(aes(x=date, y=Value)) +
          ggtitle("Select States Combined - ", value) + xlab("Date") + ylab("Count")
        ggplotly(plotgraph)
      }
      else if(interact==FALSE){
        ggplot(filtered_data) +
          geom_line(aes(x=date, y=Value)) +
          ggtitle("Select States Combined - ", value) + xlab("Date") + ylab("Count")
      }
    }
  }
  ############################

  # SUM IS FALSE --> Separate lines for each state
  else if(sum==FALSE) {
    if (length(states)==0 | missing(states)) {
      filtered_data <- covid
    }
    else{
      filtered_data <- covid %>%
        filter(region %in% states)
    }

    if(value=="tot_cases"){
      Value <- filtered_data$tot_cases }
    else if(value=="new_cases"){
      Value <- filtered_data$new_case }
    else if(value=="tot_death"){
      Value <- filtered_data$tot_death }
    else if(value=="new_death"){
      Value <- filtered_data$new_death }
    else{
      stop("Please print valid value (statistic) to measure.") }

    if(df_print==TRUE){
      returned_data <- filtered_data %>%
        select(date, region, value)
      print(as.data.frame(returned_data))}

    if(interact==TRUE){
      plotgraph <- filtered_data %>%
        group_by(region) %>%
        ggplot() +
        geom_line(aes(x=date, y=Value, col=region)) +
        ggtitle("Occurance by State", value) + xlab("Date") + ylab("Count")
      ggplotly(plotgraph)
    }
    else if(interact==FALSE){
      filtered_data %>%
        group_by(region) %>%
        ggplot() +
        geom_line(aes(x=date, y=Value, col=region)) +
        ggtitle("Occurance by State", value) + xlab("Date") + ylab("Count")
    }

  }
}

# covid_growth(states=c("california", "virginia", "new york", "arizona"), sum=T)
#
# covid_growth(sum=T)
# covid_growth(states=c("colorado", "nevada", "florida", "california"), sum=F)
# covid_growth(states=c("colorado", "nevada", "florida", "california"), interact=F)
#
#
# covid_growth(value="tot_cases", df_print=F)
#
# covid_growth(df_print=T, sum=T)
# covid_growth(value="tot_cases", df_print=F, sum=T)
# covid_growth(value="new_cases", df_print=F, sum=T)
# covid_growth(value="new_death", df_print=F, sum=T)
# covid_growth(value="tot_death", df_print=F, sum=T)
# covid_growth(sum=T)
#
# covid_growth(states="colorado")
# covid_growth(states=c("colorado", "wyoming", "virginia"), df_print=F)
#
# covid_growth(states=c("colorado", "nevada"), sum=T)
# covid_growth(states=c("colorado", "nevada"))
# covid_growth(states=c("colorado", "nevada", "florida", "california"), sum=T)
# covid_growth(value="new_cases", states=c("colorado", "nevada", "florida", "california"))
#
# covid_growth(value="new_cases", states=c("colorado", "nevada", "florida", "california"), sum=F)
# covid_growth(value="tot_death", states=c("colorado", "nevada", "florida", "california"), sum=F)
#


