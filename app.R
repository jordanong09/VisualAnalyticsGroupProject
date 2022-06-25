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
library(ggsci)


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
grid_data <- readRDS('data/grid_data.rds')
base_data <- readRDS('data/base_data.rds')
label_data <- readRDS('data/label_data.rds')
social_circular_barplot <- readRDS('data/social_circular_barplot.rds')


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
                    #plotOutput ("socialPlot"),
                    plotOutput ("circularPlot")
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
                         plotOutput("typePlot")
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
  
  ggstatsplot2 <- reactive({
    switch(input$Weekday,
           "total_earn" = total_earn,
           "weekday_earn"= weekday_earn,
           "weekend_earn" = weekday_earn)
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

   output$circularPlot <- renderPlot ({
     ggplot(social_circular_barplot) +      
       
       # Add the stacked bar
       geom_bar(aes(x=as.factor(id), y=value, fill=workday), stat="identity", alpha=0.5) +
       scale_fill_tron()+
       
       # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
       geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
       geom_segment(data=grid_data, aes(x = end, y = 50000, xend = start, yend = 50000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
       geom_segment(data=grid_data, aes(x = end, y = 100000, xend = start, yend = 100000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
       geom_segment(data=grid_data, aes(x = end, y = 150000, xend = start, yend = 150000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
       geom_segment(data=grid_data, aes(x = end, y = 200000, xend = start, yend = 200000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
       
       # Add text showing the value of each 100/75/50/25 lines
       annotate("text", x = rep(max(social_circular_barplot$id),5), y = c(0, 50000, 100000, 150000, 200000), label = c("0", "50000", "100000", "150000", "200000") , color="grey", size=2 , angle=0, fontface="bold", hjust=0.8) +
       
       ylim(-200000,max(label_data$tot + 1000, na.rm=T)) +
       labs(title = "Social Interaction of Participants by Month and Week", caption = "From Mar 22 - Feb 23", fill = "Workday Type") +
       theme_minimal() +
       theme(
         legend.position="right",
         axis.text = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         panel.grid = element_blank()
       ) +
       coord_polar(start = 0) +
       # Add labels on top of each bar
       geom_text(data=label_data, aes(x=id, y=tot+1000, label=weeknum, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
       # Add base line information
       geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size= 0.4 , inherit.aes = FALSE ) +
       geom_text(data=base_data, aes(x = title, y = -25000, label=as.factor(month)), hjust=c(1,1,1,1,1,1,0,0,0,0,0,0), colour = "black", alpha=0.7, size=3, fontface="bold", inherit.aes = FALSE)
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
    
    
    
    output$typePlot <- renderPlot({

      ggbetweenstats(
        data = total_data,
        x = type,
        y = input$Weekday,
        type = input$testType,
        xlab = "Business Type",
        ylab = "Revenue",
        p.adjust.method = input$pvalueType,
        palette = "Set3",
        plot.type = input$plotType,
        ggtheme = ggplot2::theme_gray()
      )
      
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
        ggtheme = ggplot2::theme_gray() + theme(axis.title.y= element_text(angle=0)),
        title = "Revenue of Restaurant for different Months"
      )
      
      

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
