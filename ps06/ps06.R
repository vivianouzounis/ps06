library(shiny)
library(tidyverse)

data <- read_delim("Use_Of_Force.csv")

ui <- fluidPage(
  titlePanel("Use of Force"),
    tabsetPanel(
      tabPanel(
        "General Information",
        mainPanel(textOutput("gen_info"))
    ),
      tabPanel("Precinct Histogram",
          sidebarLayout(
            sidebarPanel(
              selectInput("genderInput", "Choose Genders to Display:",
                  choices = c("Male", "Female", "Unknown"),
                  selected = "Male",
                  multiple = TRUE),
              selectInput("colorInput", "Choose Bar Color:",
                          choices = c("Blue", "Red", "Green"),
                          selected = "Blue")),
            mainPanel(plotOutput("precinct"),verbatimTextOutput("summaryText")),
      )
    ),
    tabPanel("Incident Types and Subject Races",
             sidebarLayout(
               sidebarPanel(
                 selectInput("incidentType", "Select Incident Type",
                      choices = c("Level 1 - Use of Force", "Level 2 - Use of Force", "Level 3 - OIS"))
               ),
               mainPanel(dataTableOutput("incidentTable"))
               )
             )
            )
)


server <- function(input, output) {
  filteredDataOne <- reactive({
    data %>%
      filter(Subject_Gender %in% input$genderInput)
  })
  
  barColor <- reactive({
    switch(input$colorInput,
           "Blue" = "cornflowerblue",
           "Red" = "indianred",
           "Green" = "forestgreen")
  })
  
  output$precinct <- renderPlot({
    ggplot(filteredDataOne(), aes(x = Subject_Gender)) +
      geom_bar(fill = barColor()) +
      labs(x = "Gender", y = "Count", title = "Gender Distribution") +
      theme_minimal()
  })
  
  filteredDataTwo <- reactive({
    data %>%
      filter(Incident_Type == input$incidentType)
  })
  
  summaryText <- reactive({
    data <- filteredDataTwo()
    n_incidents <- nrow(data)
    n_males <- sum(data$Subject_Gender == "Male")
    n_females <- sum(data$Subject_Gender == "Female")
    n_unknown <- sum(data$Subject_Gender == "Unknown")
    paste("Total incidents:", n_incidents, "\n",
          "Number of males:", n_males, "\n",
          "Number of females:", n_females, "\n",
          "Number of unknown gender:", n_unknown)
  })
  
  output$gen_info <- renderText({
    "This app shows information from the data set titled Use of Force from 
    Seattle Open Data. "
  })
  
  filteredData <- reactive({
    data %>%
      filter(Incident_Type == input$incidentType)
  })
  
  output$incidentTable <- renderDataTable({
    filteredData() %>%
      select(Incident_Type, Subject_Race)
  })
  
  output$summaryText <- renderPrint({
    summaryText()
  })
}

shinyApp(ui = ui, server = server)



