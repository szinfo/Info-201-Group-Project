library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)
library(maps)

yearlydata <- read_delim("allyearlydata.csv")
statedata <- read_delim("data-table.csv")

bigdata <- gather(yearlydata, key = "Substance", value = "Usage rate", 2:16)

statedata <- read_delim("data-table-7.csv") 
stateMaps <- map_data("state")
states <- merge(stateMaps, statedata, sort = FALSE, by = "region")
states <- states[order(states$order), ]


ui <- fluidPage(
  titlePanel("By Nev Fenelon and Sam Zinbarg"),
  headerPanel("Statistics for Drug Use"),
  tabsetPanel(type = "tab",
              #WELCOME PAGE
              tabPanel("Welcome!",
                       mainPanel(
                         h1("Introduction"),
                         p("Our project focuses on two data sets that explore the substance usage rates and overdose deaths across the US.
                           While researching this topic, we wanted to see if there were patterns between different demographics and their usage of substences."),
                         h2("About the dataset"),
                         p("We gathered the usage rates by substance dataset from Substance Abuse and Mental Health Services Administration.
                           The drug overdose deaths by state dataset came from the Centers for Disease Control and Prevention. There should not be any reliability issues with the data, as the SAMHSA data came from records of admissions
                           to substance abuse treatment centers, so the data is compiled at client-level, and the CDC data was government sponsered. However, the SAMHSA data is only complied from those who check themselves into a treatment center,
                            so the data disregards those who do not. This leaves out a group who people who may still be drug users but are unable to get treatment."),
                         h2("Conclusion"),
                         p("This data shows a comparison between multiple demographics' substance use and the prevelence of overdose deaths in each state.
                           By breaking down the demographics into subcategories, such as race, age, and sex, we are able to observe trends between specific groups that gives us a better picture of who is using what substance.
                           Knowing this will give us a better sense of where to allocate resources like narcane or hospitalization support to drug users. It also gives us a sense of what groups are the most at-risk, which means we can focus on
                           prevention before the substance abuse cycle starts. Some notable conclusions we drew from our usage rate by substance bar graph data include men being the primary users of all substances except for sedatives or cocaine (smoked) usage in black groups went down
                           by 6% between 2015 and 2019. Our overdose death by state bar graph showed us California was the leading state in overdose deaths in most of the years between 1999 and 2020. Knowing this information gives us a better picture of where and how to help drug users.
                            In the future, researching certain demographics in specific area can help to narrow the search for at-risk groups. While these two datasets are useful on their own, combining location and sub-demographics when compiling data will yield more applicable results."),
                         img(alt = "drug usage",
                             src = "https://ccbhc.org/wp-content/uploads/2020/12/drug-abuse-701x467-1-1.jpg")
                         
                           )
                       ),
              #PAGE 2: DATA TABLE FOR SAMHSA DATA
              tabPanel("Data Table",
                       sidebarLayout(
                         sidebarPanel(
                           p("Which year's data would you like to look at?"),
                           selectInput("year_choice", "Choose year to view",
                                       choices = unique(yearlydata$Year))),
                         mainPanel(
                           p("This table shows the range of substance abuse between different drugs across three demographics and their sub-categories. The data spans from 2015-2019."),
                           dataTableOutput("table"))
                         )
                      ),
              #PAGE 3: BAR GRAPH FOR SAMHSA DATA
              tabPanel("Bar Graph",
                       sidebarLayout(
                         sidebarPanel(
                           selectizeInput("substance_select",
                                          "Choose two or more substances to compare usage rates",
                                          choices = unique(bigdata$Substance),multiple = TRUE),
                           p("Which year's data would you like to look at?"),
                           selectInput("year_choice2", "Choose year to view",
                                       choices = unique(bigdata$Year)),
                           p("You can observe usage rates among different demographics"),
                           selectInput("demo_choice", "Choose a demographic",
                                       choices = unique(bigdata$Demographic)),
                           selectInput("Qualities_selection","Select demographic quality",
                                       choices=NULL)),
                         mainPanel(p("This bar graph allows the user to input the specific substance, year, demographic, and subdemographic they want to compare.
                                     The usage rates are displayed above each bar. The data specifies the % of the demographic that uses the selected substance."),
                                   (plotOutput("barGraph")),
                                   (em("The total usage rates might not add up to 100%
                                     as there were also instances of multiple substance usage in a single patient."))),
                         )
                    ),
              #PAGE 4: BAR GRAPH FOR CDC DATA
              tabPanel("Overdose mortality by state",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("year_select3",
                                       "Choose a year to compare drug morality rates between states",
                                       choices = unique(statedata$YEAR))),
                         mainPanel(
                           p("This bar graph shows the overdose deaths in each US state in 1999, 2005, and from 2014-2020. Input allows the user to choose what year they wish to observe."),
                           plotOutput("stateGraph")
                          )
                         
                       )
                ),
      )   
)


server <- function(input, output, session) {
  #RENDER DATA TABLE FOR PAGE 2
  output$table <- renderDataTable({
    yearlydata %>%
      filter(Year %in% input$year_choice) %>%
      group_by(Year)
  })
  #FILTERING CHOICES FROM WIDGETS IN PAGE 3
  graphdata <- reactive({
    bigdata %>%
      filter(Year %in% input$year_choice2) %>%
      filter(Substance %in% input$substance_select) %>%
      filter(Demographic %in% input$demo_choice) %>%
      filter(Qualities %in% input$Qualities_selection)
  })
  #RENDERING BAR GRAPH FOR PAGE 3 USING SAMHSA
  output$barGraph <- renderPlot ({
    ggplot(graphdata(), aes(x = Substance, y = (`Usage rate`), fill = Substance)) + geom_bar(stat = "identity") +
      ggtitle("Drug usage rates by substance in ", input$year_choice2) +
      geom_text(aes(label = scales::percent(((`Usage rate`)/100), accuracy = 0.1)), position = position_dodge(width = 1),
                vjust = -0.5) +
      labs(x = "Substances",
           y = "Percent of usage")
  })
  #THIS CHANGES THE QUALITY SELECTION WIDGET BASED ON THE CHOICE MADE IN THE PREVIOUS DEMOGRAPHIC WIDGET
  observeEvent(input$demo_choice,{
    updateSelectInput(session,"Qualities_selection",
                      choices = bigdata[bigdata$Demographic %in% input$demo_choice,
                                        "Qualities", drop = TRUE])
  })
  #FILTERS DATA FOR BAR GRAPH BASED ON SELECTION FROM WIDGET
  graphstatedata <- reactive({
    statedata %>%
      filter(YEAR %in% input$year_select3)
  })
  #RENDERS BAR GRAPH FOR PAGE 4 USING CDC DATA
  output$stateGraph <- renderPlot({
    ggplot(graphstatedata(), aes(x = DEATHS, y = STATE, fill = STATE)) + geom_bar(stat = "identity", color = "black") +
      ggtitle("Total Deaths per Year by State") + theme(legend.position="none") +
      labs(x = "Total Deaths",
           y = "State")
  })
}


shinyApp(ui = ui, server = server)
