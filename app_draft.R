library(shiny)
library(shinydashboard)
library (tidygraph)
library (ggraph)
library (tmap)
library (tidyverse)
library (sf)
library (ggstatsplot)
library (igraph)
library(ggsci)
library (DT)
library(scales)


Participant_Details <- readRDS("data/archive/Participant_Details(880).rds")
nonParticipant_Details <- readRDS("data/archive/Participant_Details(131).rds")
total_data <- read_rds('data/business_plot.rds')
buildings <- read_sf("data/Buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")


buildings_details <- readRDS("data/Building_Details.rds")
recreation_visit <- readRDS("data/recreation_visit.rds")
social_interaction <- readRDS("data/social_interaction_all.rds")
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


# Define UI for application 

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Demographics", tabName = "point1", icon = icon("globe")),
    menuItem("Social Interaction", tabName = "point2", icon = icon("user", lib = "glyphicon")),
    menuItem("Business", tabName = "point3", icon = icon("usd", lib = "glyphicon"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
           h2("Dashboard tab content")),
    tabItem(tabName = "point1",
            h2("Demographics in Ohio"),
            fluidRow(
              valueBoxOutput("point1_info1"),
              valueBoxOutput("point1_info2"),
              valueBoxOutput("point1_info3")
            ),
            fluidRow(
              
              column(2, 
                     h4("Demographics of Population in Ohio"),
                     
                     radioButtons(
                       inputId = "Demo_Resident",
                       label = "Choose Resident Type",
                       choices = c("Resident",
                                   "Non-Resident"),
                       selected = "Resident"
                     ),
                     uiOutput("Demo_Category"),
                     br(),
                     uiOutput("Demo_Category1"),
                     
              ),
              column (5,
                      plotOutput("barPlot")),
              column (5,
                      plotOutput("demostatsPlot")),
              
            ),
            
            hr(),
            
            fluidRow(
              
              column(2, 
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
              
              column(5,
                     plotOutput("buildingBarPlot")),
              
              column(5,
                     plotOutput("buildingPlot"))
              )
    ),
    tabItem(tabName = "point2",
            h2("Social Network Interaction in Ohio"),
            fluidRow(
              valueBoxOutput("point2_info1"),
              valueBoxOutput("point2_info2"),
              valueBoxOutput("point2_info3")
            ),
            fluidRow(
              column (2,
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
              
              column (4,
                      plotOutput("treemapPlot")
              ),
              
              column (6,
                      plotOutput("socialstatsPlot")
              )
              
              
            ),
            
            hr(),
            
            fluidRow(
              column (2,                    
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
              column (5,
                      plotOutput("networkstatsPlot"))
              
              
            ),
            
            fluidRow(
              column (12,
                      DT::dataTableOutput("mytable"))
            )
    ),
    tabItem(tabName = "point3",
            h2("Predominant Business in Ohio"),
            fluidRow(
              valueBoxOutput("point3_info1"),
              valueBoxOutput("point3_info2"),
              valueBoxOutput("point3_info3")
            ),
            fluidRow(
              
              column(2,
                     selectInput(inputId = "ggstatfilter",
                                 label = "Choose Venue Type for Statistical Plot",
                                 choices = c( "Restaurant",
                                              "Pubs"),
                                 selected = "Restaurant")),
              column(10,
                     plotOutput("statsPlot"))
            ),
            
            hr(),
            
            fluidRow(
              
              column(2, 
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
              
              column(5,
                     plotOutput("typePlot")
              ),
              column(5, 
                     plotOutput("tMapEarnings", click = "plot_click"),
                     h4("Clicked Points"),
                     verbatimTextOutput("plot_clickedpoints"))       
            )
      
    )
  )
)  



ui <- dashboardPage(
  dashboardHeader(title = "Group1 Project"),
  sidebar,
  body,
  skin = "black")


############################################################SERVER###########################################################################
server <- function(input, output, session) {
  
  ##Reactive Values for Demographics Plot
  
  demo_dataset <- reactive({
    if (input$Demo_Resident == "Resident") {
      Participant_Details <- Participant_Details[, sapply(Participant_Details, class) %in% c('character', 'factor')]
      Participant_Details
    } else {
      nonParticipant_Details <- nonParticipant_Details[, sapply(nonParticipant_Details, class) %in% c('character', 'factor')]
      nonParticipant_Details
    }
  })
  
  output$Demo_Category<- renderUI({
    
    selectInput("demo_category","Choose X Axis Variable for Bar Plot:", choices=colnames(demo_dataset())[names(demo_dataset()) !="Participant ID"])
    
  })
  output$Demo_Category1<- renderUI({
    
    req(input$demo_category)
    selectInput("demo_category1","Choose Variable for Statsitical Plot:", choices=colnames(demo_dataset())[names(demo_dataset()) %in% c("Participant ID", input$demo_category) == FALSE])
    
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
   
   output$point1_info1 <- renderValueBox({
     valueBox(
       "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "yellow")
   })
   
   output$point1_info2 <- renderValueBox({
     valueBox(
       "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "green")
   })
   
   output$point1_info3 <- renderValueBox({
     valueBox(
       "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "red")
   })
   
   output$barPlot <- renderPlot({
     ggplot(demo_dataset(),
            aes(x = .data[[input$demo_category]])) + 
       geom_bar(fill= '#E15E8E') +
       geom_text(stat = 'count',
                 aes(label= paste0(stat(count), ', ', 
                                   round(stat(count)/sum(stat(count))*100, 
                                         1), '%')), vjust= -0.5, size= 2.5) +
       labs(y= 'No. of\nResidents', x= input$demo_category,
            title = paste0("Distribution of Residents by ",input$demo_category)) +
       theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
             panel.background= element_blank(), axis.line= element_line(color= 'grey'), legend.position="none",
             plot.title = element_text(size = 14, face = "bold")) 
     
   })
   
   output$demostatsPlot <- renderPlot({
     req(input$demo_category1)
     
     ggbarstats(
       data     = demo_dataset(),
       x = !!rlang::sym(input$demo_category),
       y = !!rlang::sym(input$demo_category1),
       title = paste0("Correlation of ",input$demo_category," and ", input$demo_category1),
       xlab = input$demo_category,
       ylab = input$demo_category1,
       ggplot.component = list(ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))),
       ggtheme = ggplot2::theme_classic() + theme(axis.title.y= element_text(angle=0),
                                                  plot.title = element_text(size = 12, face = "bold", hjust=0.5)),
       palette  = "Set2"
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
   
   output$buildingPlot <- renderPlot({
     
     buildings_details <- buildings_details %>%
       filter(time == "Mar 22") 
     
     buildings_details$long <- sapply(buildings_details$geom_points, "[", 1)
     buildings_details$lat <- sapply(buildings_details$geom_points, "[", 2)
     
     ggplot(data = buildings) +
       geom_sf() +
       geom_point(data = buildings_details, aes(x = long, y = lat, size = count)) +
       scale_size_continuous(breaks = c(2, 4, 6, 8, 10)) +
       theme_graph() + 
       labs(size = 'count') +
       ggtitle(paste0(input$time," @@ ")) +
       theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                             size = 0.5), panel.background = element_rect(fill = "aliceblue"))
   })
   
   

   ##Plot for Social Network Tabs
   
   output$point2_info1 <- renderValueBox({
     valueBox(
       "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "yellow")
   })
   
   output$point2_info2 <- renderValueBox({
     valueBox(
       "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "green")
   })
   
   output$point2_info3 <- renderValueBox({
     valueBox(
       "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "red")
   })
      
   
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
   
   output$point3_info1 <- renderValueBox({
     valueBox(
       "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "yellow")
   })
   
   output$point3_info2 <- renderValueBox({
     valueBox(
       "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "green")
   })
   
   output$point3_info3 <- renderValueBox({
     valueBox(
       "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "red")
   })
   
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
    
    output$plot_clickedpoints <- renderTable({
      # For base graphics, we need to specify columns, though for ggplot2,
      # it's usually not necessary.
      res <- nearPoints(ggstatsplot1(), input$plot_click, addDist = TRUE)
      if (nrow(res) == 0)
        return()
      res
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
