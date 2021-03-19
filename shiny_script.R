####packages####
library(shiny)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(TTR)
library(plotly)
library(shinythemes)
library(broom)
require(scales)

####data####
dudedata <- read_csv("../data/dude_modeldf.csv")

####inputs####
ui <- fluidPage(
  
  tabsetPanel(#Create tabs
    tabPanel("Find a room", #Tab one details
             selectInput("event", "Event Type", choices = dudedata$Event_Category),
             numericInput("attend", "Planned attendence", 0, min = 0),
             dateInput("date", "Event date (dd/mm/yyy)"),
             br(),
             br(),
             br(),
             br(),
             dataTableOutput("room.text")
    ),#close tabPanel 1
    tabPanel("Pricing details", #Tab two details
             textInput("etype", "Event type", ""),
             numericInput("attend", "Planned attendence", 0, min = 0),
             dateInput("date", "Event date (dd/mm/yyy)"),
             br(),
             br(),
             br(),
             br(),
             textOutput("price.text")
    )#close tabPanel 2
  )#close tabset
)#close fluidPage

####outputs####
server <- function(input, output) {
  
  #just removing nas from RoomCapacity before getting the standard diviation
  dudedata %>% 
    filter(!is.na(RoomCapacity)) -> modelnas
  
  #calculate standard diviation of RoomCapacity and store in modelsd object
  modelsd <- sd(modelnas$RoomCapacity)
  
  #reactive datatable that responds to selected inputs
  output$room.text <- renderDataTable({
    dudedata %>% 
      mutate(Room = row_number()) %>% #add unique id to return
      mutate(EventStartDate = mdy(EventStartDate)) %>% #format as date
      filter(EventStartDate != !!input$date) %>% #made sure there is not an event already scheduled for the room on that day
      filter(Event_Category == !!input$event) %>% #filter based on the selected room category
      filter(RoomCapacity >= !!input$attend - (.25 * modelsd) & RoomCapacity <= !!input$attend + (.25 * modelsd)) %>% #filter to within .25 standard diviations on either side of imputed attendence
      filter(RoomCapacity >= !!input$attend) %>% #filter to greater than or equal to planned attendence
      select(Room, RoomCapacity) #just show room and its capacity
  })#Close renderDateTable
  
}#Close Server

# Run the application 
shinyApp(ui = ui, server = server)
