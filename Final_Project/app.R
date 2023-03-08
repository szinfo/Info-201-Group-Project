library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)

yearlydata <- read_delim("allyearlydata.csv")

bigdata <- gather(yearlydata, key = "Substance", value = "Usage rate", 2:16)

ui <- fluidPage(
  titlePanel("Stats for Drug Use"),
  headerPanel("By Nev Fenelon and Sam Zinbarg"),
  tabsetPanel(type = "tab",
              tabPanel("Welcome!",
                       mainPanel(
                         h3("Put description here")
                       )),
              
              tabPanel("Data Table",
                       sidebarLayout(
                         sidebarPanel(
                           p("Which year's data would you like to look at?"),
                           selectInput("year_choice", "Choose year to view",
                                       choices = unique(yearlydata$Year))),
                         mainPanel(
                           dataTableOutput("table")))),
              
              tabPanel("Bar Graph",
                       sidebarLayout(
                         sidebarPanel(
                           p("Which year's data would you like to look at?"),
                           selectInput("year_choice2", "Choose year to view",
                                       choices = unique(yearlydata$Year)),
                           p("You can observe usage rates among different demographics"),
                           selectInput("Substance_selection","Select substance:",
                                       choices=unique(bigdata$Substance))),
                         mainPanel(
                           plotOutput("barGraph"),
                         )
                       )
              )
  )
)


server <- function(input, output) {
  
  output$table <- renderDataTable({
    bigdata %>%
      filter(Year %in% input$year_choice) %>%
      group_by(Year)
  })
  graphdata <- reactive({
    bigdata %>%
      filter(Year %in% input$year_choice2)
  })
  output$barGraph <- renderPlot ({
    ggplot(data = graphdata(), aes(x = Substance, y = ('Usage rate'), fill = Substance)) + geom_bar(stat = "identity") +
      ggtitle("Drug usage rates by substance in 2015 per age group") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Substances",
           y = "Percent of usage")
    
  })
}

shinyApp(ui = ui, server = server)
