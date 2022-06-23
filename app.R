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
library(treemap)
#renv::install("d3treeR/d3treeR")
library(d3treeR)
library (igraph)
library(hrbrthemes)


Participant_Details <- readRDS("data/Participant_Details.rds")
interaction <- readRDS("data/participant_interaction.rds")
interaction_all <- readRDS("data/participant_interaction_all.rds")
total_data <- read_rds('data/total_data.rds')
buildings <- read_sf("data/Buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")
resto_month2 <- read_rds('data/resto_month2.rds')
total_data_sf <- st_as_sf(total_data)
social_graph_workingday <- readRDS("data/social_graph_workingday.rds")
social_graph_non_workingday <- readRDS("data/social_graph_non_working.rds")
total_month_data <- read_rds('data/total_month_data_new.rds')



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    tabsetPanel(
      
      tabPanel( "Demographics",
                
                plotOutput("barPlot"),
                
                hr(),
                
                fluidRow(
                  
                  column(3, 
                         h4("User Selection"),
                         selectInput(inputId = "User_Category",
                                     label = "Choose a Category Type for Bar Plot",
                                     choices = c( "Household Size" = "Household_Size",
                                                  "Have Kids" = "Have_Kids",
                                                  "Education Level" = "Education_Level",
                                                  "Interest Group" = "Interest_Group",
                                                  "Age Group" = "Age_Group",
                                                  "Income Level" = "Income_Level",
                                                  "Joviality" = "Joviality_Level"
                                     ),
                                     selected = "Household_Size"),
                         br(),
                         
                         uiOutput("filter1")
                         
                  ),
                  
                ),
                
      ),
      
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
                                selected = "Household_Size"),
                    
                    selectInput(inputId = "workday",
                                label = "Choose a Workday Type",
                                choices = c( "Working Days",
                                             "Non-Working Days"
                                ),
                                selected = "Working Days"),
                    
                    
                    selectInput(inputId = "Network",
                                label = "Choose a Network Centrality Measure",
                                choices = c( "Degree Centrality" = "degree",
                                             "Eigenvector Centrality" = "eig",
                                             "Hub centrality" = "hubs",
                                             "Authority centrality" = "authorities",
                                             "Closeness centrality" = "closeness"
                                ),
                                selected = "degree")
                    
                    
                  ),
                  mainPanel(
                    d3treeOutput("treemapPlot"),
                    plotOutput ("socialPlot")
                  )
                )         
        
        
      ),
      
      tabPanel( "Business",
                
                selectInput(inputId = "ggstatfilter",
                            label = "Choose Venue Type for Statistical Plot",
                            choices = c( "Restaurant",
                                         "Pub"
                            ),
                            selected = "Restaurant"),
                
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
    interaction_all %>%
      group_by(Participant_ID,MonthYear, .data[[input$Category]])%>%
      summarise(InteractionCount = n()) %>%
      filter(InteractionCount > 1) %>%
      ungroup
  })
  
  ggstatsplot <- reactive({
    total_month_data %>%
      filter(type == input$ggstatfilter)
  })
  
  
  
  vards1 <- reactive ({
    switch(input$User_Category,
           "Household_Size" = unique(Participant_Details$Household_Size),
           "Have_Kids"= unique(Participant_Details$Have_Kids) ,
           "Education_Level" = unique(Participant_Details$Education_Level),
           "Interest_Group" = unique(Participant_Details$Interest_Group),
           "Age_Group" = unique(Participant_Details$Age_Group),
           "Income_Level" = unique(Participant_Details$Income_Level),
           "Joviality_Level" = unique(Participant_Details$Joviality_Level))
    
  })
  
  output$filter1 <- renderUI({
    radioButtons("fil1","", choices=vards1())
  })
  

   social_data <- reactive ({
      
     V(social_graph_workingday)$color <- ifelse (V(social_graph_workingday)$.data[[input$Network]] > quantile(V(social_graph_workingday)$.data[[input$Network]],0.9), "darkgoldenrod3", "azure3")
     V(social_graph_workingday)$size <- ifelse (V(social_graph_workingday)$.data[[input$Network]] > quantile(V(social_graph_workingday)$.data[[input$Network]],0.9), 2, 0.05)
     V(social_graph_workingday)$label <- ifelse (V(social_graph_workingday)$.data[[input$Network]] > quantile(V(social_graph_workingday)$.data[[input$Network]],0.9),V(social_graph_workingday)$name,NA)
   })
   
   output$treemapPlot <- renderD3tree3({
     d3tree3(
       treemap(dataset(),
               index = c(input$Category,"Participant_ID"),
               vSize = "InteractionCount",
               type = "index",
               palette="Set2",
               title="Interaction Count of Participant",
               title.legend = "Interaction Count"
       ), 
       rootname = "Tree Map of Interaction Count by Participant"
     )
   })

    output$socialPlot <- renderPlot({
      plot.igraph(social_data(),layout=layout.mds, edge.arrow.size=0.1,edge.arrow.mode = "-", vertex.label.cex = 0.65, vertex.label.font = 2)
    })


    
    output$barPlot <- renderPlot({
      ggplot(Participant_Details,
             aes(fill = factor(.data[[input$User_Category]]), x = Region)) + 
        geom_bar(position = "dodge") + 
        scale_fill_viridis(discrete = T, option = "E") +
        facet_wrap(~.data[[input$User_Category]]) +
        labs(y= 'No. of\nResidents', x= 'Region',
             title = "Distribution of Residents' Across Regions") +
        theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
              panel.background= element_blank(), axis.line= element_line(color= 'grey'), legend.position="none") 
        
    })
    
    
    #output of inputs for UI
    
    
    
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
        data = ggstatsplot(),
        x = time,
        y = earn,
        type = input$testType,
        xlab = "Mon/Year",
        ylab = "Revenue",
        p.adjust.method = input$pvalueType,
        palette = "Set3",
        plot.type = input$plotType,
        ggtheme = ggplot2::theme_gray(),
        title = "Revenue of Restaurant for different Months"
      )
      
      

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
