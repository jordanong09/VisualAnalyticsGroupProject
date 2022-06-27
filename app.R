library(shiny)
library (igraph)
library (tidygraph)
library (ggraph)
library (plotly)
library (tmap)
library (tidyverse)
library (sf)
library (ggstatsplot)
library(treemap)
#renv::install("d3treeR/d3treeR")
library(d3treeR)
library (igraph)
library(ggsci)
library (DT)
library(scales)


Participant_Details <- readRDS("data/Participant_Details(880).rds")
interaction <- readRDS("data/participant_interaction.rds")
total_data <- read_rds('data/business_plot.rds')
buildings <- read_sf("data/Buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")

resto_month2 <- read_rds('data/resto_month2.rds')
total_month_data <- read_rds('data/total_month_data_new.rds')

# grid_data <- readRDS('data/grid_data.rds')
# base_data <- readRDS('data/base_data.rds')
# label_data <- readRDS('data/label_data.rds')
# social_circular_barplot <- readRDS('data/social_circular_barplot.rds')

social_interaction <- readRDS("data/social_interaction.rds")
participant_interaction <- readRDS("data/participant_interaction.rds")
residential_data <- readRDS("data/Residential_Details.rds")
residential_data_sf <- st_as_sf(residential_data)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    tabsetPanel(
      
      tabPanel( "Demographics",
                
                
                fluidRow(
                  
                  column(3, 
                         h4("Demographics of Population in Ohio"),
                         
                         radioButtons(
                           inputId = "Demo_Resident",
                           label = "Choose Resident Type",
                           choices = c("Resident",
                                       "Non-Resident"),
                           selected = "Resident"
                         ),
                         selectInput(inputId = "Demo_Category",
                                     label = "Choose a Category Type for Bar Plot",
                                     choices = c( "Household Size" = "Household Size",
                                                  "Have Kids" = "Have Kids",
                                                  "Education Level" = "Education Level",
                                                  "Interest Group" = "Interest Group",
                                                  "Age Group" = "Age Group",
                                                  "Income Level" = "Income Level",
                                                  "Joviality" = "Joviality Level",
                                                  "Residence" = "Residence Region",
                                                  "Workplace" = "Workplace Region"
                                     ),
                                     selected = "Household Size"),
                         br(),
                         
                         selectInput(inputId = "Demo_Category1",
                                     label = "Choose a 2nd Category Type for Statistical Plot",
                                     choices = c( "Household Size" = "Household Size",
                                                  "Have Kids" = "Have Kids",
                                                  "Education Level" = "Education Level",
                                                  "Interest Group" = "Interest Group",
                                                  "Age Group" = "Age Group",
                                                  "Income Level" = "Income Level",
                                                  "Joviality" = "Joviality Level",
                                                  "Residence" = "Residence Region",
                                                  "Workplace" = "Workplace Region"
                                     ),
                                     selected = "Have Kids"),
                         
                  ),
                  column (4,
                          plotOutput("barPlot")),
                  column (4,
                          plotOutput("demostatsPlot")),
                  
                ),
                
                hr(),
                
                fluidRow(
                  
                  column(3, 
                         h4("Demographics of Buildings in Ohio"),
                         radioButtons(
                           inputId = "Demo_Buildings",
                           label = "Choose Building Type",
                           choices = c("Residential",
                                       "Commercial"),
                           selected = "Residential"
                         ),
                         selectInput(inputId = "Demo_Buildings1",
                                     label = "Choose a Category Type for Buildings Visualisation",
                                     choices = c( "Vacancy",
                                                  "Shared Apartment"
                                     ),
                                     selected = "Vacancy"),
                         br(),
                         
                         uiOutput("filter1"),
                         
                         br (),
                         
                         sliderInput("RentCost", "Rental Cost:",
                                     min = 0, max = 2000, value = 100
                         ),
                         
                  ),
                  
                  column(4,
                         plotOutput("buildingBarPlot")),
                  
                  column(4,
                         plotOutput("buildingPlot"))
                  
                ),
              
                
      ),
      
      tabPanel( "Social Interaction",
                fluidRow(
                  column (3,
                              helpText(" Visualise the Social Network Interaction of the Population in Ohio"),
                              selectInput(inputId = "social_category",
                                          label = "Choose a Category",
                                          choices = c( "Household Size",
                                                       "Have Kids",
                                                       "Education Level",
                                                       "Interest Group",
                                                       "Age Group"
                                          ),
                                          selected = "Household Size"),
                              
                              selectInput(inputId = "social_category1",
                                          label = "Choose 2nd Category",
                                          choices = c( "Household Size",
                                                       "Have Kids",
                                                       "Education Level",
                                                       "Interest Group",
                                                       "Age Group"
                                          ),
                                          selected = "Age Group")
                              
                  
                ),
                
                column (6,
                        d3treeOutput("treemapPlot"),
                        )
        
        
      ),
      
      hr(),
      
      fluidRow(
        column (3,                    
                helpText(" Visualise the top 1% influential people in Ohio based on Month and Day"),
                selectInput(inputId = "month",
                            label = "Choose a Month",
                            choices = c( "Jan",
                                         "Feb",
                                         "Mar",
                                         "Apr",
                                         "May",
                                         "Jun",
                                         "Jul",
                                         "Aug",
                                         "Sept",
                                         "Oct",
                                         "Nov",
                                         "Dec"
                                         
                            ),
                            selected = "Jan"),
                selectInput(inputId = "workday",
                                label = "Choose a Workday Type",
                                choices = c( "Working Day",
                                             "Non-Working Day"
                                ),
                                selected = "Working Days"),
                    
                    
                    selectInput(inputId = "network",
                                label = "Choose a Network Centrality Measure",
                                choices = c( "Degree Centrality" = "degree",
                                             "Eigenvector Centrality" = "eig",
                                             "Hub Centrality" = "hubs",
                                             "Authority Centrality" = "authorities",
                                             "Closeness Centrality" = "closeness",
                                             "PageRank Centrality" = "pagerank"
                                ),
                                selected = "degree")
                    
                ),
        
        column (5,
                plotOutput("socialPlot")),
        
        column (4,
                DT::dataTableOutput("mytable"))
      ),
      ),
      
      tabPanel( "Business",
                
                selectInput(inputId = "ggstatfilter",
                            label = "Choose Venue Type for Statistical Plot",
                            choices = c( "Restaurant",
                                         "Pubs"
                            ),
                            selected = "Restaurant"),
                
                plotOutput("statsPlot"),
                
                hr(),
                
                fluidRow(
                  
                  column(3, 
                         h4("User Selection"),
                         
                         selectInput(inputId = "Weekday",
                                     label = "Choose a Weekday Type",
                                     choices = c( "Total Earnings",
                                                  "Non-Working Day",
                                                  "Working Day"
                                     ),
                                     selected = "Total Earnings"),
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
                  
                  column(4,
                         plotOutput("typePlot")
                         ),
                  column(4, 
                         plotOutput("tMapEarnings"))       
                         ),
                  
                ),
                
    ),
)


server <- function(input, output, session) {
  
  ##Reactive Values for Demographics Plot
  
  demo_dataset <- reactive({
    Participant_Details %>%
      filter (Type == input$Demo_Resident)
  })
  
  
  vards1 <- reactive ({
    
    if(input$Demo_Buildings == "Residential") {
      switch(input$Demo_Buildings1,
             "Vacancy" = unique(residential_data$Vacancy),
             "Shared Apartment"= unique(residential_data$`Shared Apartment`)
      )
    }
    
    
  })
  
  output$filter1 <- renderUI({
    radioButtons("fil1","Filter", choices=vards1())
  })
  
  buildingData <- reactive (
    {
      residential_data_sf %>%
          filter(.data[[input$Demo_Buildings1]] == as.character(input$fil1))
    }
  )
  
   
   ##Reactive Values for Social Network Plot
  
  social_data <- reactive ({
    social_interaction %>%
      filter(Month == input$month & workday == input$workday) %>%
      group_by(participantIdFrom,participantIdTo) %>%
      summarise(Weight = n()) %>%
      filter (participantIdFrom != participantIdTo) %>%
      filter (Weight > 1) %>%
      ungroup
  })
   
   social_graph <- reactive ({
     
     new_graph <- graph_from_data_frame (social_data(),
                                         vertices = participant_interaction) %>%
       as_tbl_graph()
     
     if (input$network == "degree") {
       V(new_graph)$degree <- degree(new_graph)
       new_graph <- delete_vertices(new_graph, V(new_graph)[degree < quantile (V(new_graph)$degree,0.9)])
       filter <- quantile (V(new_graph)$degree,0.9)
       V(new_graph)$size <- ifelse (V(new_graph)$degree > filter, 10, 0.01)
       V(new_graph)$color <- ifelse (V(new_graph)$degree > filter, "darkgoldenrod3", "azure3")
       V(new_graph)$label <- ifelse (V(new_graph)$degree > filter,V(new_graph)$name,NA)
       
       new_graph
       
     } else if(input$network == "eig") {
       V(new_graph)$eig <- evcent(new_graph)$vector
       new_graph <- delete_vertices(new_graph, V(new_graph)[eig < quantile (V(new_graph)$eig,0.9)])
       filter <- quantile (V(new_graph)$eig,0.9)
       V(new_graph)$size <- ifelse (V(new_graph)$eig > filter, 10, 0.01)
       V(new_graph)$color <- ifelse (V(new_graph)$eig > filter, "darkgoldenrod3", "azure3")
       V(new_graph)$label <- ifelse (V(new_graph)$eig > filter,V(new_graph)$name,NA)
       
       new_graph
     } else if(input$network == "pagerank") {
       V(new_graph)$pagerank <- page_rank(new_graph)$vector
       new_graph <- delete_vertices(new_graph, V(new_graph)[pagerank < quantile (V(new_graph)$pagerank,0.9)])
       filter <- quantile (V(new_graph)$pagerank,0.9)
       V(new_graph)$size <- ifelse (V(new_graph)$pagerank > filter, 10, 0.01)
       V(new_graph)$color <- ifelse (V(new_graph)$pagerank > filter, "darkgoldenrod3", "azure3")
       V(new_graph)$label <- ifelse (V(new_graph)$pagerank > filter,V(new_graph)$name,NA)
       
       new_graph
     }
     
     
     
   })
   
   newtable <- reactive ({
     Participant_Details %>%
       filter (as.character(`Participant ID`) %in% V(social_graph())$label)
     
   })
   
   ###Reactive Values for Business Plot
   
   ggstatsplot <- reactive({
     total_data %>%
       filter(Type == input$ggstatfilter) %>%
       group_by(Id,DateMonth) %>%
       summarise (Expenses = sum(Expenses))
   })
   
   ggstatsplot1 <- reactive({
     total_data %>%
       filter(
         if (input$Weekday == "Total Earnings") {
           workday == workday
         } else {
           workday == input$Weekday
         }
       ) %>%
       group_by(Id, Type, long, lat) %>%
       summarise (Expenses = sum(Expenses)) 
   })
   
   #####################################################Plotting of Graph####################################################
   
   #Plot for Demographics Tabs
   
   output$barPlot <- renderPlot({
     ggplot(demo_dataset(),
            aes(x = .data[[input$Demo_Category]])) + 
       geom_bar(fill= '#E15E8E') +
       geom_text(stat = 'count',
                 aes(label= paste0(stat(count), ', ', 
                                   round(stat(count)/sum(stat(count))*100, 
                                         1), '%')), vjust= -0.5, size= 2.5) +
       labs(y= 'No. of\nResidents', x= input$Demo_Category,
            title = paste0("Distribution of Residents by ",input$Demo_Category)) +
       theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
             panel.background= element_blank(), axis.line= element_line(color= 'grey'), legend.position="none",
             plot.title = element_text(size = 14, face = "bold")) 
     
   })
   
   output$demostatsPlot <- renderPlot({
     ggbarstats(
       data     = demo_dataset(),
       x = !!rlang::sym(input$Demo_Category),
       y = !!rlang::sym(input$Demo_Category1),
       title            = paste0("Correlation of ",input$Demo_Category," and ", input$Demo_Category1),
       xlab             = input$Demo_Category,
       legend.title     = input$Demo_Category1,
       ggplot.component = list(ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))),
       palette          = "Set2"
     )
     
   })
   
   output$buildingBarPlot <- renderPlot({
     ggplot(residential_data,
            aes(x = Region, fill = .data [[input$Demo_Buildings1]])) + 
       geom_bar(position="dodge", stat="count") +
       scale_fill_manual(values=c("#E6286E",
                                  "#539CF0")) +
       geom_text(stat = 'count',
                 aes(label= paste0(stat(count), ', ', 
                                   round(stat(count)/sum(stat(count))*100, 
                                         1), '%')), position=position_dodge(width=0.9), vjust=-0.35, size = 2.5) +
       labs(y= 'No. of\nBuildings', x= "Region",
            title = paste0("Distribution of Buildings by ", input$Demo_Buildings1)) +
       theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
             panel.background= element_blank(), axis.line= element_line(color= 'grey'),
             plot.title = element_text(size = 14, face = "bold")) 
     
   })
   
   output$buildingPlot <- renderPlot ({
     tm_shape(buildings)+
       tm_polygons(col = "grey60",
                   size = 1,
                   border.col = "black",
                   border.lwd = 1) +
       tm_shape(buildingData()) +
       tm_bubbles (
         col = "Shared Apartment",
         size = "Rental Cost",
         border.col = "black",
         border.lwd = 1
       )
     
   })
   
   
   ##Plot for Social Network Tabs
   
   
   output$treemapPlot <- renderD3tree3({
     d3tree3(
       treemap(Participant_Details,
               index = c(input$social_category, input$social_category1),
               vSize = "InteractionCount",
               type = "value",
               vColor = "InteractionCount",
               palette="Set2",
               title="Interaction Count of Participant",
               title.legend = "Interaction Count"
       ), 
       rootname = "Tree Map of Interaction Count by Participant"
     )
   })
   
   
   output$socialPlot <- renderPlot ({
     
     plot(social_graph(),layout=layout.fruchterman.reingold, edge.arrow.size=0.1,edge.arrow.mode = "-", vertex.label.cex = 1, vertex.label.font = 2)
     
   })
   
   output$mytable <- DT::renderDataTable(newtable(),
                                         options = list(scrollX = TRUE),
                                         rownames = FALSE)
   
   
   
   
   

   # output$circularPlot <- renderPlot ({
   #   ggplot(social_circular_barplot) +      
   #     
   #     # Add the stacked bar
   #     geom_bar(aes(x=as.factor(id), y=value, fill=workday), stat="identity", alpha=0.5) +
   #     scale_fill_tron()+
   #     
   #     # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
   #     geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
   #     geom_segment(data=grid_data, aes(x = end, y = 50000, xend = start, yend = 50000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
   #     geom_segment(data=grid_data, aes(x = end, y = 100000, xend = start, yend = 100000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
   #     geom_segment(data=grid_data, aes(x = end, y = 150000, xend = start, yend = 150000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
   #     geom_segment(data=grid_data, aes(x = end, y = 200000, xend = start, yend = 200000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
   #     
   #     # Add text showing the value of each 100/75/50/25 lines
   #     annotate("text", x = rep(max(social_circular_barplot$id),5), y = c(0, 50000, 100000, 150000, 200000), label = c("0", "50000", "100000", "150000", "200000") , color="grey", size=2 , angle=0, fontface="bold", hjust=0.8) +
   #     
   #     ylim(-200000,max(label_data$tot + 1000, na.rm=T)) +
   #     labs(title = "Social Interaction of Participants by Month and Week", caption = "From Mar 22 - Feb 23", fill = "Workday Type") +
   #     theme_minimal() +
   #     theme(
   #       legend.position="right",
   #       axis.text = element_blank(),
   #       axis.title.x = element_blank(),
   #       axis.title.y = element_blank(),
   #       panel.grid = element_blank()
   #     ) +
   #     coord_polar(start = 0) +
   #     # Add labels on top of each bar
   #     geom_text(data=label_data, aes(x=id, y=tot+1000, label=weeknum, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
   #     # Add base line information
   #     geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size= 0.4 , inherit.aes = FALSE ) +
   #     geom_text(data=base_data, aes(x = title, y = -25000, label=as.factor(month)), hjust=c(1,1,1,1,1,1,0,0,0,0,0,0), colour = "black", alpha=0.7, size=3, fontface="bold", inherit.aes = FALSE)
   # })


    
    
    
    
    
    
    
    
    
    ### Plot for BusinessPlot
   
   output$statsPlot <- renderPlot({
     
     ggbetweenstats(
       data = ggstatsplot(),
       x = DateMonth,
       y = Expenses,
       type = input$testType,
       xlab = "Mon/Year",
       ylab = "Revenue",
       p.adjust.method = input$pvalueType,
       palette = "Set3",
       plot.type = input$plotType,
       ggtheme = ggplot2::theme_classic() + theme(axis.title.y= element_text(angle=0),
                                               plot.title = element_text(size = 14, face = "bold", hjust=0.5)),
       title = paste0("Revenue of ", input$ggstatfilter, "s for different Months")
     )
     
   })
    
    
    output$typePlot <- renderPlot({

      ggbetweenstats(
        data = ggstatsplot1(),
        x = Type,
        y = Expenses,
        type = input$testType,
        xlab = "Business Type",
        ylab = "Revenue",
        p.adjust.method = input$pvalueType,
        palette = "Set3",
        plot.type = input$plotType,
        ggtheme = ggplot2::theme_classic()+ theme(axis.title.y= element_text(angle=0),
                                               plot.title = element_text(size = 14, face = "bold", hjust=0.5)),
        ggplot.component = ggplot2::scale_y_continuous (labels = comma),
        title = paste0(input$Weekday," Revenue of Pubs and Restaurants ")
      ) 
      
    })
    
    output$tMapEarnings <- renderPlot({
      
      ggplot (data = buildings) +
        geom_sf() +
        geom_point(data = ggstatsplot1(), aes(x = long, y = lat, shape = Type, color = Expenses, size = 1)) +
        scale_color_continuous(breaks = c(200000, 400000, 600000, 800000), label=comma) +
        theme_graph() + 
        labs(color = "Revenue", shape = 'Venue Type') +
        guides(size = "none") +
        ggtitle("Observation Sites", subtitle = "(20 Restaurants and 12 Pubs)") +
        theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                              size = 0.5), panel.background = element_rect(fill = "aliceblue"))
    })
    
    output$info <- renderPrint({
      nearPoints(ggstatsplot1(), input$plot_click, threshold = 10, maxpoints = 1,
                 addDist = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
