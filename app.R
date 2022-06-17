#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library (igraph)
library (tidygraph)
library (ggraph)
library (plotly)

interaction <- readRDS("participant_interaction.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Social Network Interaction"),

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

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("heatmapPlot")
        )
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
}

# Run the application 
shinyApp(ui = ui, server = server)
