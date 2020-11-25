library(dplyr)
library(ggplot2)

covid <- read.csv("/Users/blakenorthrop/Desktop/QAC356/finalpackageprep/Data/US Covid (to Nov8).csv")

covid$date <- as.Date(covid$date, format = "%Y-%m-%d")

##################

# Make function
time_growth <- function(data=covid, state=c(), sum=FALSE, df_print=TRUE){
  if(sum==TRUE){
    filtered_data <- data %>% 
      filter(region %in% state) %>% 
      group_by(date) %>% 
      mutate(combined_total = sum(tot_cases))
    
    returned_data <- filtered_data %>%
      select(date, combined_total)
    if(df_print==TRUE){
      print(as.data.frame(returned_data))}
    
    ggplot(filtered_data) +
      geom_line(aes(x=date, y=combined_total)) +
      ggtitle("Growth over Time (combined by state)") + xlab("Date") + ylab("Count")
  }
  else{
    filtered_data <- data %>% 
      filter(region %in% state)
    
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

time_growth(state="colorado")
time_growth(state="colorado", df_print=F)

time_growth(state=c("colorado", "nevada"), sum=T)
time_growth(state=c("colorado", "nevada"))

time_growth(state=c("colorado", "nevada", "florida", "california"), df_print=F)
