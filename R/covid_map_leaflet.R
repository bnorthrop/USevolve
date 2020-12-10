#' Covid Map
#'
#' covid_map automatically loads the latest COVID-19 data from the CDC and plots a chloropleth
#' map of the United States based on the fill value the user specifies. Fill options include
#' total cases ("tot_cases"), new cases ("new_cases"), total deaths ("tot_death"), and
#' new deaths ("new_death").
#'
#' @param value covid statistic to analyze.
#' @param Date a date in form "YYYY-MM-DD".  --> CHOOSE DEFAULT
#' @param pal a color palette.
#' @param ... pass other arguments
#'
#' @import dplyr ggplot2 rgdal leaflet readr data.table RColorBrewer stringr
#' @export
#'
#' @return a ggplot map object.
#' @examples
#' covid_map(Value="new_death", Date="2020-12-01")

covid_map2 <- function(value, Date="2020-12-02", pal="YlOrRd"){
  # require(dplyr)
  # require(ggplot2)
  # require(rgdal)
  # require(leaflet)
  # require(readr)
  # require(data.table)
  # require(RColorBrewer)
  # require(geojsonio)
  # require(stringr)

  Date <- as.Date(as.character(Date), format = "%Y-%m-%d")

  state_shapes <- geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json",
                               what="sp")
  slotNames(state_shapes)
  state_shapes@data$rn <- row.names(state_shapes)
  temp.states <- data.table(state_shapes@data)
  temp.states$name <- tolower(temp.states$name)

  covid <- covid_update()
  covid %>%
    mutate(name=region) -> covid

  if(!missing(Date)){
    date_filt <- covid %>% filter(date==Date)
  }else{
    date_filt <- covid
  }


  out.states <- merge(temp.states, date_filt, by="name", all.x=TRUE)
  out.states <- data.table(out.states)
  setkey(out.states, rn)
  state_shapes@data <- out.states[row.names(state_shapes)]

  if(missing(value)){
    Value <- state_shapes$tot_cases
    Label <<- "Total Cases:"}
  else if(value=="tot_cases"){
    Value <- state_shapes$tot_cases
    Label <<- "Total Cases:"}
  else if(value=="new_cases"){
    Value <- state_shapes$new_case
    Label <<- "New Cases:"}
  else if(value=="tot_death"){
    Value <- state_shapes$tot_death
    Label <<- "Total Deaths:"}
  else if(value=="new_death"){
    Value <- state_shapes$new_death
    Label <<- "New Deaths:"}

  palette <- colorBin(pal,domain=NULL, n=5)
  leaflet(data= state_shapes) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor= ~palette(Value),
                fillOpacity= 0.8,
                color="black",
                weight= 1,
                popup=~paste(str_to_title(name),
                             "<br>", Date, "<br>", Label,
                             Value)) %>%
    addLegend("bottomleft",
              colors=brewer.pal(5, pal),
              labels=c("low","","","","high"),
              title=~paste("Covid-19 Data in the United States <br>", Label))

}

#covid_map2("tot_death",pal="Blues")

#covid_map2("tot_death","2020-11-01")

# ###trying but failing to do a shiny ####
#
# require(dplyr)
# require(ggplot2)
# require(rgdal)
# require(leaflet)
# require(geojsonio)
# require(readr)
# require(data.table)
# require(RColorBrewer)
# require(shiny)
# require(stringr)
#
# state_shapes <- geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json",
#                              what="sp")
# slotNames(state_shapes)
# state_shapes@data$rn <- row.names(state_shapes)
# temp.states <- data.table(state_shapes@data)
# temp.states$name <- tolower(temp.states$name)
#
# covid <- covid_update()
#
# covid %>%
#   mutate(name=region) -> covid
#
# Date="2020-12-02"
# date_filt <- covid
# date_filt <- covid %>% filter(date==Date)
# Value <- date_filt$tot_cases
#
# out.states <- merge(temp.states, date_filt, by="name", all.x=TRUE)
# out.states <- data.table(out.states)
# setkey(out.states, rn)
# state_shapes@data <- out.states[row.names(state_shapes)]
#
# pal <- colorBin("Reds",domain=NULL, n=5)
# leaflet(data= state_shapes) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(fillColor= ~pal(date_filt$tot_cases),
#               fillOpacity= 0.8,
#               color="black",
#               weight= 1,
#               popup=~paste(str_to_title(name), "<br>Total Covid Cases:", date_filt$tot_cases))
#
#
# # covid_map()
# #
# covid_map2(Value="new_cases")
# covid_map2(Value="new_death", Date="2020-07-04")
#
# covid_map_shiny <- function(){
#
#
#
#
#
# # ui <- fluidPage(
# #   dateInput(inputId="Date",
# #             label="Enter a Date between 2020-01-22 and Today",
# #             format="yyyy-mm-dd"),
# #   checkboxGroupInput("value","Pick a COVID-19 Variable",
# #                      choices=c("tot_cases","new_cases","tot_death","new_death")),
# #   leafletOutput("mymap")
# # )
# #
# # server <- function(input,output){
# #   state_shapes <- geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json",
# #                                what="sp")
# #   state_shapes@data$rn <- row.names(state_shapes)
# #   temp.states <- data.table(state_shapes@data)
# #   temp.states$name <- tolower(temp.states$name)
# #   covid <- covid_update()
# #   covid %>%
# #     mutate(name=region) -> covid
# #   date_filt <- reactive({covid %>% filter(date==input$Date)})
# #   out.states <- merge(temp.states, date_filt, by="name", all.x=TRUE)
# #   out.states <- data.table(out.states)
# #   setkey(out.states, rn)
# #   state_shapes@data <- out.states[row.names(state_shapes)]
# #   pal <- colorQuantile("YlOrRd",domain=NULL, n =5)
# #   output$mymap <- renderLeaflet({
# #     leaflet(data = state_shapes) %>%
# #       addProviderTiles("CartoDB.Positron") %>%
# #       addPolygons(fillColor = ~pal(input$value),
# #                   fillOpacity = 0.8,
# #                   color = "black",
# #                   weight = 1,
# #                   popup=~paste(name,"<br>", input$value,":"))
# #   })
# # }
# #
# #
# # shinyApp(ui=ui,server=server)
