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

recreation_visit <- readRDS("data/recreation_visit.rds")
social_interaction <- readRDS("data/social_interaction_all.rds")
participant_interaction <- readRDS("data/participant_interaction.rds")
residential_data <- readRDS("data/Residential_Details.rds")
residential_data_sf <- st_as_sf(residential_data)

color_palettes <- c("#e485a4",
  "#58bc51",
  "#a55bd1",
  "#a1b534",
  "#596dd1",
  "#d69c35",
  "#5d8bc9",
  "#de5d2f",
  "#44bcd1",
  "#d3414c",
  "#5dc497",
  "#d353ad",
  "#4d8a39",
  "#d03d74",
  "#368660",
  "#89529a",
  "#aab267",
  "#bf8ed7",
  "#697329",
  "#9c4970",
  "#936a2e",
  "#b05b5c",
  "#e1966b",
  "#ae502c")


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
                              checkboxGroupInput( inputId = "social_checkbox",
                                                  label = "Select variables to group:",
                                                  choices = c(
                                                    "Participant Id" = "Participand Id",
                                                    "Pub Id",
                                                    "Month Year" = "Dates",
                                                    "Time of Day (hours)" = "Time Period",
                                                    "Weekday type (Mon-Sun)" = "Weekday",
                                                    "Workday type (Working/Non-Working)" = "Workday Type",
                                                    "Period of Day (Morning - Midnight)" = "Session",
                                                    "Region of Pub" = "Region"
                                                  )),
                              
                          uiOutput("socialFilter"),
                          uiOutput("socialFilterfill")
                          
                              
                  
                ),
                
                column (3,
                        plotOutput("treemapPlot")
                        ),
                
                column (6,
                        plotOutput("socialstatsPlot")
                )
        
        
      ),
      
      hr(),
      
      fluidRow(
        column (3,                    
                helpText(" Visualise the top 1% influential people in Ohio based on Month and Day"),
                selectInput(inputId = "month",
                            label = "Choose a Month",
                            choices = unique(social_interaction$MonYear),
                            selected = "Mar 2022"),
                selectInput(inputId = "workday",
                                label = "Choose a Workday Type",
                                choices = c( "Working Day",
                                             "Non-Working Day"
                                ),
                                selected = "Working Days"),
                    
                    
                selectInput(inputId = "network",
                                label = "Choose a Network Centrality Measure",
                                choices = c( "Degree Centrality",
                                             "Eigenvector Centrality",
                                             "Hub Centrality" = "hubs",
                                             "Authority Centrality" = "authorities",
                                             "Closeness Centrality" = "closeness",
                                             "PageRank Centrality"
                                ),
                                selected = "Degree Centrality"),
                
                uiOutput("networkstatsFilter")
                    
                ),
        
        column (5,
                plotOutput("socialPlot")),
        column (4,
                plotOutput("networkstatsPlot"))
        
        
      ),
      
      fluidRow(
        column (12,
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
  
  recreation_data <- reactive ({
    req(input$social_checkbox)
  
    new <- recreation_visit %>%
      group_by(!!! rlang::syms(input$social_checkbox)) %>%
      summarise(Visitcount = n())
      
    new
  })
  
  output$socialFilter <- renderUI({
    selectInput("socialfilter","Choose X Axis Variable for Bar Plot:", choices=colnames(recreation_data())[names(recreation_data()) !="Visitcount"])
  })
  
  output$socialFilterfill <- renderUI({
    selectInput("socialfilterfill","Choose Fill Variable for Bar Plot:", choices=colnames(recreation_data())[names(recreation_data())  %in% c("Visitcount") == FALSE])
  })
  
  
  output$networkstatsFilter <- renderUI({
    req(input$month)
    selectInput("networkstatsfilter","Choose Variable for Statsitical Plot:", choices=colnames(statstable())[names(statstable()) %in% c("label", input$network, "Participant Id", "Type") == FALSE])
  })
  
  social_data <- reactive ({
    social_interaction %>%
      filter(MonYear == input$month & workday == input$workday) %>%
      group_by(participantIdFrom,participantIdTo) %>%
      summarise(Weight = n()) %>%
      filter (participantIdFrom != participantIdTo) %>%
      filter (Weight > 1) %>%
      ungroup
  })
   
   social_graph <- reactive ({
     
     new_graph <- graph_from_data_frame (social_data(),
                                         vertices = Participant_Details) %>%
       as_tbl_graph()
     
     if (input$network == "Degree Centrality") {
       V(new_graph)$value <- degree(new_graph)
       V(new_graph)$label <- ifelse (V(new_graph)$value > quantile (V(new_graph)$value,0.99),V(new_graph)$name,NA)
       new_graph
       
     } else if(input$network == "Eigenvector Centrality") {
       V(new_graph)$value <- evcent(new_graph)$vector
       V(new_graph)$label <- ifelse (V(new_graph)$value > quantile (V(new_graph)$value,0.99),V(new_graph)$name,NA)
       new_graph
       
     } else if(input$network == "PageRank Centrality") {
       V(new_graph)$value <- page_rank(new_graph)$vector
       V(new_graph)$label <- ifelse (V(new_graph)$value > quantile (V(new_graph)$value,0.99),V(new_graph)$name,NA)
       new_graph
     }
     
     
     
   })
   
   recreation_visit_count <- reactive ({
     
     statsplot1 <- recreation_visit %>%
       filter (Dates == input$month) %>%
       group_by(`Participant Id`) %>%
       summarise(`Pub Visit Count` = n()) %>%
       select (`Participant Id`, `Pub Visit Count`)
     
     statsplot1
   })
   
  statstable <- reactive ({
     
     sdt <- as_tibble(social_graph(), what="vertices")
     
     stats_dt <- sdt %>%
       rename ("Participant Id" = "name") %>%
       select (-label) %>%
       left_join(recreation_visit_count(), by = "Participant Id")
     
     names(stats_dt)[names(stats_dt) == 'value'] <- input$network
     
     stats_dt
     
   })
   
   
   newtable <- reactive ({
     
     dt <- as_tibble(social_graph(), what="vertices")
     
     new_dt <- dt %>%
       filter (name %in% V(social_graph())$label) %>%
       rename ("Participant Id" = "name") %>%
       select (-label) %>%
       left_join(recreation_visit_count(), by = "Participant Id")
     
     names(new_dt)[names(new_dt) == 'value'] <- input$network
     
     new_dt
     
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
   
   
   # output$treemapPlot <- renderD3tree3({
   #   d3tree3(
   #     treemap(Participant_Details,
   #             index = c(input$social_category, input$social_category1),
   #             vSize = "InteractionCount",
   #             type = "value",
   #             vColor = "InteractionCount",
   #             palette="Set2",
   #             title="Interaction Count of Participant",
   #             title.legend = "Interaction Count"
   #     ), 
   #     rootname = "Tree Map of Interaction Count by Participant"
   #   )
   # })
   
   output$treemapPlot <- renderPlot({
     ggplot(recreation_data(), aes(x = .data[[input$socialfilter]], y = Visitcount, fill = .data[[input$socialfilterfill]])) +
       geom_bar(stat = "identity", position = "dodge") + 
       scale_fill_manual(values = color_palettes) +
       theme_classic() +
       labs(y= 'No. of\n Visits', x= input$socialfilter,
            title = paste0("Number of Visits to Pubs by ", input$socialfilter)) +
       theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
             panel.background= element_blank(), axis.line= element_line(color= 'grey'),
             plot.title = element_text(size = 14, face = "bold")) 
       
   })
   
   
   output$socialstatsPlot <- renderPlot({
     req(input$social_checkbox)
     
     ggbetweenstats(
       data = recreation_data(),
       x = !!rlang::sym(input$socialfilter),
       y = Visitcount,
       xlab = input$socialfilter,
       ylab = "VisitCount",
       pairwise.comparisons = FALSE,
       ggtheme = ggplot2::theme_classic() + theme(axis.title.y= element_text(angle=0),
                                                  plot.title = element_text(size = 14, face = "bold", hjust=0.5)),
       ggplot.component = ggplot2::scale_color_manual(values = color_palettes),
       title = paste0("Visit Count of Pubs by ", input$socialfilter)
     )
     
   })
   
   
   output$socialPlot <- renderPlot ({
     
     
     new_graph1 <- delete_vertices(social_graph(), V(social_graph())[value < quantile (V(social_graph())$value,0.9)])
     filter <- quantile (V(new_graph1)$value,0.9)
     
     ggraph(new_graph1, layout = "graphopt") +
       geom_edge_link(edge_colour = "grey", edge_width = 0.05) + 
       geom_node_point(aes(size = ifelse (V(new_graph1)$value > filter, 4, 0.001)),color = ifelse (V(new_graph1)$value > filter, "#98984d", "#b3669e")) +
       geom_node_label(aes(label = ifelse (V(new_graph1)$value > filter, V(new_graph1)$name, NA)), repel = TRUE) +
       theme_graph() +
       ggtitle(paste0("Top 1% influential participant based on ",input$network)) +
       theme(legend.position = "none", plot.title=element_text( size = 10, hjust=0.5, vjust=0.5, face='bold'))
     

   })
   
   output$mytable <- DT::renderDataTable(newtable(),
                                         options = list(scrollX = TRUE),
                                         rownames = FALSE)
   
   
   output$networkstatsPlot <- renderPlot({
     
     
     req(input$networkstatsfilter)
     
     if( is.numeric(statstable()[[input$networkstatsfilter]])  || is.integer(statstable()[[input$networkstatsfilter]])) {
       ggscatterstats(
         data  = statstable(),
         x = !!rlang::sym(input$networkstatsfilter),
         y = !!rlang::sym(input$network),
         xlab  = input$networkstatsfilter,
         ylab  = input$network,
         ggtheme = ggplot2::theme_classic() + theme(axis.title.y= element_text(angle=0),
                                                    plot.title = element_text(size = 14, face = "bold", hjust=0.5)),
         ggplot.component = ggplot2::scale_color_manual(values = color_palettes),
         title = paste0(input$network, " comparison with ", input$networkstatsfilter)
       )
       
     } else {
       ggbetweenstats(
         data = statstable(),
         x = !!rlang::sym(input$networkstatsfilter),
         y = !!rlang::sym(input$network),
         xlab = input$networkstatsfilter,
         ylab = input$network,
         pairwise.comparisons = FALSE,
         ggtheme = ggplot2::theme_classic() + theme(axis.title.y= element_text(angle=0),
                                                    plot.title = element_text(size = 14, face = "bold", hjust=0.5)),
         ggplot.component = ggplot2::scale_color_manual(values = color_palettes),
         title = paste0(input$network, " comparison with ", input$networkstatsfilter)
       )
       
     }
     
     
   })
   
  
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
       plot.type = input$plotType,
       ggtheme = ggplot2::theme_classic() + theme(axis.title.y= element_text(angle=0),
                                               plot.title = element_text(size = 14, face = "bold", hjust=0.5)),
       ggplot.component = ggplot2::scale_color_manual(values = color_palettes),
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
        ggtitle(paste0(input$Weekday," Revenue of Pubs and Restaurants "), subtitle = "(20 Restaurants and 12 Pubs)") +
        theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                              size = 0.5), panel.background = element_rect(fill = "aliceblue"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
