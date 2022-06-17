library(shiny)
library (igraph)
library (tidygraph)
library (ggraph)
library (plotly)
library (tmap)
library (tidyverse)
library (sf)
library (patchwork)
library (ggstatsplot)
library (gapminder)


interaction <- readRDS("data/participant_interaction.rds")
total_data <- read_rds('data/total_data.rds')
buildings <- read_sf("data/Buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")
resto_month2 <- read_rds('data/resto_month2.rds')
total_data_sf <- st_as_sf(total_data)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    tabsetPanel(
      
      tabPanel( "Social Interaction",
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    helpText(" Visualise the Social Network Interaction of the Population
          in Ohio"),
                    
                    selectInput(inputId = "Category",
                                label = "Choose a Category",
                                choices = c( "Household Size" = "Household_Size",
                                             "Have Kids" = "Have_Kids",
                                             "Education Level" = "Education_Level",
                                             "Interest Group" = "Interest_Group",
                                             "Age Group" = "Age_Group"
                                ),
                                selected = "Household_Size")
                  ),
                  mainPanel(
                    plotOutput("heatmapPlot")
                  )
                )         
        
        
      ),
      
      tabPanel( "Business",
                
                plotOutput("statsPlot"),
                
                hr(),
                
                fluidRow(
                  
                  column(3, 
                         h4("User Selection"),
                         selectInput(inputId = "Weekday",
                                     label = "Choose a Weekday Type",
                                     choices = c( "Weekday Earnings" = "weekday_earn",
                                                  "Weekend Earnings" = "weekend_earn",
                                                  "Total Earnings" = "total_earn"
                                     ),
                                     selected = "total_earn"),
                         br(),
                         
                         selectInput(inputId = "plotType",
                                     label = "Choose Plot Type for Statistical Plot",
                                     choices = c( "Box & Violin Plot" = "boxviolin",
                                                  "Box Plot" = "box",
                                                  "Violin Plot" = "violin"
                                     ),
                                     selected = "boxviolin"),
                         
                         br(),
                         selectInput(inputId = "testType",
                                     label = "Choose the type of Statistical Test",
                                     choices = c( "Parametric" = "p",
                                                  "Non-Parametric" = "np",
                                                  "Robust" = "r",
                                                  "Bayes Factor" = "bf"
                                     ),
                                     selected = "p"),
                         br(),
                         selectInput(inputId = "pvalueType",
                                     label = "Choose the P Value Type for Test",
                                     choices = c( "Holm" = "holm",
                                                  "Hochberg" = "hochberg",
                                                  "Hommel" = "hommel",
                                                  "Bonferroni" = "bonferroni",
                                                  "Benjamini & Hochberg" = "BH",
                                                  "Benjamini & Yekutieli" = "BY",
                                                  "None" = "none"
                                     ),
                                     selected = "holm")
                         
                  ),
                  
                  column(4,offset = 1,
                         plotlyOutput("plotlyEarnings")
                         ),
                  column(4, 
                         plotOutput("tMapEarnings"))       
                         ),
                  
                ),
                
    )

  )   


server <- function(input, output, session) {
  
  dataset <- reactive({
    interaction %>%
      group_by(Month, .data[[input$Category]])%>%
      summarise(InteractionCount = n()) %>%
      ungroup
  })

    output$heatmapPlot <- renderPlot({
      ggplot(dataset(),aes(x = .data[[input$Category]], y = Month, fill = InteractionCount)) +
        geom_tile()
    })
    
    output$plotlyEarnings <- renderPlotly({
      plot_ly(total_data, 
              x = ~type, 
              y = ~.data[[input$Weekday]],
              split = ~type,
              type = 'violin',
              box = list(visible = T),
              meanline = list(visible = T)) %>%
        layout(xaxis = list(title = 'Business Type'),
               yaxis = list(title = 'Revenue'))
    })
    
    output$tMapEarnings <- renderPlot({
      tm_shape(buildings)+
        tm_polygons(col = "grey60",
                    size = 1,
                    border.col = "black",
                    border.lwd = 1) +
        tm_shape(total_data_sf) +
        tm_symbols(size = 0.5, shape = "type", shapes.labels = c("Restaurant", "Pub"), title.shape = "Venue Type",
                   col = input$Weekday, style = "cont", title.col = "Revenue")  +
        tm_layout(main.title.size = 1)
    })
    
    output$statsPlot <- renderPlot({
      
      ggbetweenstats(
        data = resto_month2,
        x = month,
        y = earn,
        type = input$testType,
        xlab = "Month",
        ylab = "Revenue",
        p.adjust.method = input$pvalueType,
        plot.type = input$plotType,
        ggtheme = ggplot2::theme_gray(),
        title = "Revenue of Restaurant for different Months"
      )
      
      

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
