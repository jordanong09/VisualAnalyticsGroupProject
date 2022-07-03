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

Resident_Details <- readRDS("data/Resident_Details.rds")
nonResident_Details <- readRDS("data/NonResident_Details.rds")
total_data <- readRDS('data/business_plot.rds')
buildings <- read_sf("data/Buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")
total_data <- st_as_sf (total_data)
Region <- st_read('data/buildings.shp') 

demo_buildings <- readRDS ("data/demo_buildings.rds")
demo_buildings <- st_as_sf(demo_buildings)

buildings_details <- readRDS("data/Building_Details.rds")

recreation_visit <- readRDS("data/recreation_visit.rds")
recreation_visit_social <- readRDS("data/recreation_visit_social.rds")
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
    menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
    menuItem("Demographics", tabName = "point1", icon = icon("globe"), startExpanded = TRUE,
             menuSubItem("Population", tabName = "point1-1"),
             menuSubItem("Buildings", tabName = "point1-2")),
    menuItem("Social Interaction", tabName = "point2", icon = icon("user", lib = "glyphicon"), startExpanded = TRUE,
             menuSubItem(HTML("Social Interaction among<br/>&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbspPopulation"), tabName = "point2-1"),
             menuSubItem("Top 1% Influential People", tabName = "point2-2")),
    menuItem("Business", tabName = "point3", icon = icon("usd", lib = "glyphicon"), startExpanded = TRUE,
             menuSubItem("Revenue by Month", tabName = "point3-1"),
             menuSubItem(HTML("Revenue of Venues by<br/>&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbspWorkday"), tabName = "point3-2"),
             menuSubItem("Funnel Plot", tabName = "point3-3")),
    selectInput(inputId = "demo_quantile",
                label = "Choose Quantile for Income and Joviality Level:",
                choices = c( "10%" = "0.1",
                             "15%" = "0.15",
                             "20%" = "0.2",
                             "25%" = "0.25",
                             "30%" = "0.3",
                             "35%" = "0.35",
                             "40%" = "0.4"
                ),
                selected = "0.2")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
           h2("Our Application"),
           hr(),
           h5("Our application was created as part of VAST Challenge 2022. Our application will analyse the Demographics and Relationships to provide insights about the city’s demographics, its neighbourhoods, and its business base."),
           hr(),
           h2("Application Features"),
           hr(),
           h5("As seen in the sidebar menu on the left, this application has 3 key modules:"),
           h4("1) Demographics:"),
           h5("This module allow user to understand the demographics of the population (Residents/Non-Residents) and Buildings. User can use the interactive buttons to toggle the selection and derive statistical conclusion from the statistical plot."),
           h4("2) Social Activities:"),
           h5("This module provides insights on factors that affects recreation activities (Pubs Visit) and social influence. Users can select the variable to visual the social network of the city and derive statistical conclusion from the statistical plot."),
           h4("3) Business Base:"),
           h5("This module allow user to explore the revenues and visit counts of all the Pubs and Restaurants. User will be allowed to use the variables to choose different statistic variables and derive statistical conclusion from the statistical plot.")
           ),
    tabItem(tabName = "point1-1",
            fluidRow(
              infoBoxOutput(width = 6, "point1_info1"),
              infoBoxOutput(width = 6, "point1_info2")
            ),
            fluidRow(
              column(width = 4,
                     box(
                       width = NULL, status = "info",
                       radioButtons(
                         inputId = "Demo_Resident",
                         label = "Choose Resident Type",
                         choices = c("Resident",
                                     "Non-Resident"),
                         selected = "Resident")
                       )
                     ),
              column(width = 4,
                     box(
                       width = NULL, status = "info",
                       uiOutput("Demo_Category")
                     )
                    ),
              column(width = 4,
                     box(
                       width = NULL, status = "info",
                       uiOutput("Demo_Category1")
                     )
              )
            ),
            fluidRow(
              
              column(width = 6,
                      box(width = NULL, color = "teal", plotOutput("barPlot"))),
              column(width = 6,
                      box(width = NULL, color = "teal", plotOutput("demostatsPlot")))
              
            )
    ),
    tabItem(tabName = "point1-2",
            fluidRow(
              infoBoxOutput("point1_info3"),
              infoBoxOutput("point1_info4"),
              infoBoxOutput("point1_info5")
            ),
            fluidRow(
              column(width = 2,
                     box(
                       width = NULL, status = "info",
                       radioButtons(
                         inputId = "Demo_Buildings",
                         label = "Choose Building Type",
                         choices = c("Buildings",
                                     "Residential"),
                         selected = "Buildings"
                       )
                     ),   
              ),
              column(width = 2,
                     box(
                       width = NULL, status = "info",
                       uiOutput("Demo_Buildings1")
                     )
              ),
              column(width = 2,
                     box(
                       width = NULL, status = "info",
                       uiOutput("filter1")
                     )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       sliderInput("count", "Residence Count:",
                                   min = 1, max = 10, value = c(1,10)
                       )
                     )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "time",
                                   label = "Choose a Month for visualisation: ",
                                   choices = c("Mar 22",
                                               "Apr 22",
                                               "May 22",
                                               "Jun 22",
                                               "Jul 22",
                                               "Aug 22",
                                               "Sep 22",
                                               "Oct 22",
                                               "Nov 22",
                                               "Dec 22"),
                                   selected = "Mar 22")
                     ),   
              )
              
            ),
            fluidRow(
              column(width = 6,
                     box(width = NULL, color = "teal", plotOutput("buildingBarPlot"))),
              column(width = 6,
                     box(width = NULL, color = "teal", plotOutput("buildingPlot", brush=brushOpts(id="plot_click"))))
            ),
            fluidRow(
              column(width = 12,
                     box(width = NULL, color = "teal", dataTableOutput("plot_brushedpoints1")))
            )

    ),
    tabItem(tabName = "point2-1",
            fluidRow(
              infoBoxOutput(width = 6, "point2_info1"),
              infoBoxOutput(width = 6, "point2_info2"),
            ),
            fluidRow(
              column(width = 4,
                     box(
                       width = NULL, status = "info",
                       uiOutput("socialFilter"),
                       uiOutput("socialFilterfill"),
                       selectInput("t1", label="Filter Selection of Group Variable", choices=c(), multiple = TRUE),
                       actionButton("plot", "Plot Graph")
                     )
              ),
              column(
                width = 8,
                box(width = NULL, color = "teal", plotOutput("socialstatsPlot")))
              
            ),
            fluidRow(
              column(
                width = 12,
                box(width = NULL, color = "teal", plotOutput("socialstatsPlotgroup")))
            )
    ),
    tabItem(tabName = "point2-2",
            fluidRow(
              infoBoxOutput(width = 6, "point2_info3"),
              infoBoxOutput(width = 6, "point2_info4"),
            ),
            fluidRow(
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "month",
                                   label = "Choose a Month",
                                   choices = unique(social_interaction$MonYear),
                                   selected = "Mar 2022")
                     )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "workday",
                                   label = "Choose a Workday Type",
                                   choices = c( "Working Day",
                                                "Non-Working Day"
                                   ),
                                   selected = "Working Days")
                     )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "network",
                                   label = "Choose a Network Centrality Measure",
                                   choices = c( "Degree Centrality",
                                                "Eigenvector Centrality",
                                                "Hub Centrality",
                                                "Authority Centrality",
                                                "PageRank Centrality"))
                     )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       uiOutput("networkstatsFilter")
                     )
              )
            ),
            
            fluidRow(
              column(width = 6,
                     box(width = NULL, color = "teal", plotOutput("socialPlot"))),
              column(width = 6,
                     box(width = NULL, color = "teal", plotOutput("networkstatsPlot")))
            ),
            
            fluidRow(
              column(width = 12,
                     box(width = NULL, color = "teal", DT::dataTableOutput("mytable")))
            )
    ),
    tabItem(tabName = "point3-1",
            fluidRow(
              infoBoxOutput(width = 12, "point3_info1"),
            ),
            fluidRow(
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "ggstatfilter",
                                   label = "Choose Venue Type for Statistical Plot",
                                   choices = c( "Restaurant",
                                                "Pubs"),
                                   selected = "Restaurant")
                     )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "plotType1",
                                   label = "Choose Plot Type for Statistical Plot",
                                   choices = c( "Box & Violin Plot" = "boxviolin",
                                                "Box Plot" = "box",
                                                "Violin Plot" = "violin"
                                   ),
                                   selected = "boxviolin")
                     )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "testType1",
                                   label = "Choose the type of Statistical Test",
                                   choices = c( "Parametric" = "p",
                                                "Non-Parametric" = "np",
                                                "Robust" = "r",
                                                "Bayes Factor" = "bf"),
                                   selected = "p")
                     )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "pvalueType1",
                                   label = "Choose the P Value Type for Test",
                                   choices = c( "Holm" = "holm",
                                                "Hochberg" = "hochberg",
                                                "Hommel" = "hommel",
                                                "Bonferroni" = "bonferroni",
                                                "Benjamini & Hochberg" = "BH",
                                                "Benjamini & Yekutieli" = "BY",
                                                "None" = "none"),
                                   selected = "holm"))
                     )
              ),
            fluidRow(
              column(width = 12,
                     box(width = NULL, color = "teal", plotOutput("statsPlot")))
            )
      
    ),
    tabItem(tabName = "point3-2",
            fluidRow(
              infoBoxOutput(width = 12, "point3_info2"),
            ),
            fluidRow(
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "Weekday",
                                   label = "Choose a Weekday Type",
                                   choices = c( "Total Earnings",
                                                "Non-Working Day",
                                                "Working Day"
                                   ),
                                   selected = "Total Earnings")
                     )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "plotType",
                                   label = "Choose Plot Type for Statistical Plot",
                                   choices = c( "Box & Violin Plot" = "boxviolin",
                                                "Box Plot" = "box",
                                                "Violin Plot" = "violin"
                                   ),
                                   selected = "boxviolin")
                     )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "testType",
                                   label = "Choose the type of Statistical Test",
                                   choices = c( "Parametric" = "p",
                                                "Non-Parametric" = "np",
                                                "Robust" = "r",
                                                "Bayes Factor" = "bf"),
                                   selected = "p")
                     )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "pvalueType",
                                   label = "Choose the P Value Type for Test",
                                   choices = c( "Holm" = "holm",
                                                "Hochberg" = "hochberg",
                                                "Hommel" = "hommel",
                                                "Bonferroni" = "bonferroni",
                                                "Benjamini & Hochberg" = "BH",
                                                "Benjamini & Yekutieli" = "BY",
                                                "None" = "none"),
                                   selected = "holm"))
              )
            ),
            fluidRow(
              column(width = 6,
                     box(width = NULL, color = "teal", plotOutput("typePlot"))),
              column(width = 6,
                     box(width = NULL, color = "teal", tmapOutput("tMapEarnings")))
            )
    ),
    tabItem(tabName = "point3-3",
            fluidRow(
              infoBoxOutput(width = 6, "point3_info3"),
              infoBoxOutput(width = 6, "point3_info4"),
            ),
            fluidRow(
              column(width = 2,
                    box(
                      width = NULL, status = "info",
                      selectInput(inputId = "filterFunnel1",
                                  label = "Choose a Weekday Type",
                                  choices = c( "All",
                                               "Non-Working Day",
                                               "Working Day"
                                  ),
                                  selected = "All")
                    )
              ),
              column(width = 2,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "filterFunnel2",
                                   label = "Choose a Month to filter",
                                   choices = c( "All",
                                                "Mar 2022",
                                                "Apr 2022",
                                                "May 2022",
                                                "Jun 2022",
                                                "Jul 2022",
                                                "Aug 2022",
                                                "Sep 2022",
                                                "Oct 2022",
                                                "Nov 2022",
                                                "Dec 2022"
                                   ),
                                   selected = "All")
                     )
              ),
              column(width = 2,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "filterFunnel5",
                                   label = "Choose a Venue Type: ",
                                   choices = c( "All",
                                                "Pubs",
                                                "Restaurant"
                                   ),
                                   selected = "All")
                     )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "filterFunnel3",
                                   label = "Choose 1st Confidence Interval",
                                   choices = c( "90%" = "0.90",
                                                "95%" = "0.95",
                                                "99%" = "0.99",
                                                "99.9%" = "0.999"
                                                
                                   ),
                                   selected = "0.90")
                       )
              ),
              column(width = 3,
                     box(
                       width = NULL, status = "info",
                       selectInput(inputId = "filterFunnel4",
                                   label = "Choose 1st Confidence Interval",
                                   choices = c( "90%" = "0.90",
                                                "95%" = "0.95",
                                                "99%" = "0.99",
                                                "99.9%" = "0.999"
                                                
                                   ),
                                   selected = "0.99")
                     )
              )
            ),
            fluidRow(
              column(width = 6,
                     box(width = NULL, color = "teal", plotOutput("funnelPlot", brush = brushOpts(id = "plot_brush")))),
              column(width = 6,
                     box(width = NULL, color = "teal", dataTableOutput("plot_brushedpoints")))
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
  
  Participant_Details <- reactive({
    upr_income <- quantile(Resident_Details$Income,1-(as.numeric(input$demo_quantile)))
    lwr_income <- quantile(Resident_Details$Income,as.numeric(input$demo_quantile))
    
    upr_joy <- quantile(Resident_Details$Joviality,1-(as.numeric(input$demo_quantile)))
    lwr_joy <- quantile(Resident_Details$Joviality,as.numeric(input$demo_quantile))
    
    Resident_Details <- Resident_Details %>%
      mutate (`Income Level` = case_when(
        Income >= upr_income ~ "High Income",
        Income <= lwr_income ~ "Low Income",
        TRUE ~ "Medium Income"
      ) 
        ) %>% 
      mutate (`Joviality Level` = case_when(
        Joviality >= upr_joy ~ "Happy Participant",
        Joviality <= lwr_joy ~ "Average Participant",
        TRUE ~ "Dull Participant"
      ) 
      )
    
    Resident_Details
    
  })
  
  nonParticipant_Details <- reactive({

    upr_joy <- quantile(nonResident_Details$Joviality,1-(as.numeric(input$demo_quantile)))
    lwr_joy <- quantile(nonResident_Details$Joviality,as.numeric(input$demo_quantile))
    
    nonResident_Details <- nonResident_Details %>%
      mutate (`Joviality Level` = case_when(
        Joviality >= upr_joy ~ "Happy Participant",
        Joviality <= lwr_joy ~ "Average Participant",
        TRUE ~ "Dull Participant"
      ) 
      )
    
    nonResident_Details
  })
  
  
  ##Reactive Values for Demographics Plot
  
  demo_dataset <- reactive({
    if (input$Demo_Resident == "Resident") {
      Participant_Details <- Participant_Details()[, sapply(Participant_Details(), class) %in% c('character', 'factor', 'logical')]
      Participant_Details
    } else {
      nonParticipant_Details <- nonParticipant_Details()[, sapply(nonParticipant_Details(), class) %in% c('character', 'factor', 'logical')]
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
    
    if(input$Demo_Buildings == "Buildings") {
      switch(input$Demo_Buildings1,
             "Vacancy" = unique(demo_buildings$Vacancy),
             "Building Type"= unique(demo_buildings$`Building Type`)
      )
    }else if (input$Demo_Buildings == "Residential") {
      switch(input$Demo_Buildings1,
             "Vacancy" = unique(residential_data$Vacancy),
             "Shared Apartment"= unique(residential_data$`Shared Apartment`)
      )
    }
    
    
  })
  
  output$Demo_Buildings1 <- renderUI({
    
    if(input$Demo_Buildings == "Buildings") {
      bchoices = c( "Vacancy",
                   "Building Type"
      )
    } 
    
    if(input$Demo_Buildings == "Residential") {
      bchoices = c( "Vacancy",
                    "Shared Apartment"
      )
    } 
    
    
    selectInput(inputId = "Demo_Buildings1",
               label = "Choose a Category Type for Buildings Visualisation",
               choices = bchoices,
               selected = "Vacancy")
    
  })                     
  
  output$filter1 <- renderUI({
    radioButtons("fil1","Filter", choices=vards1())
  })
  
  buildingData <- reactive (
    {
      if(input$Demo_Buildings == "Buildings") {
      demo_buildings %>%
          filter(.data[[input$Demo_Buildings1]] == as.character(input$fil1))
      } else if (input$Demo_Buildings == "Residential") {
        residential_data %>%
          filter(.data[[input$Demo_Buildings1]] == as.character(input$fil1))
      }
    }
  )
  
   
   ##Reactive Values for Social Network Plot
  
  recreation_data <- reactive ({
  
    recreation_visit <- recreation_visit %>%
      left_join(Participant_Details(), by = c("Participant Id" = "Participant ID"))
    
    
    recreation_visit
    
  })
  
  recreation_visit_social1 <- reactive ({
    
    recreation_visit_social <- recreation_visit_social[, sapply(recreation_visit_social, class) %in% c('character', 'factor', 'logical')]
    recreation_visit_social
    
  })
  
  
  statsplot <- eventReactive (input$plot,{
    
    
    new1 <-  recreation_visit_social1() %>%
      group_by(Pub_Id, !! rlang::sym(input$socialfilter), !! rlang::sym(input$socialfiltergroup)) %>%
      summarise(Visitcount = n())
    
    new1
  })
  
  output$socialFilter <- renderUI({
    req(recreation_visit_social1())
    selectizeInput("socialfilter","Choose X Axis Variable for Statisitical Plot:", choices=colnames(recreation_visit_social1())[names(recreation_visit_social1()) %in% c("Visitcount", "Participant_Id", "Pub_Id") == FALSE])
  })
  
  output$socialFilterfill <- renderUI({
    req(recreation_visit_social1())
    selectInput("socialfiltergroup","Choose Group Variable for Statisitical Plot:", choices=colnames(recreation_visit_social1())[names(recreation_visit_social1())  %in% c("Visitcount", "Participant_Id", "Pub_Id", input$socialfilter) == FALSE])
  })
  
  observeEvent(input$socialfiltergroup, {
    choiceList <- recreation_visit_social1() %>%
      select(.data[[input$socialfiltergroup]]) %>%
      unique()
    
    updateSelectInput(session, "t1",choices = choiceList, selected = choiceList)
  })  
  
  
  
  output$networkstatsFilter <- renderUI({
    req(input$month)
    selectInput("networkstatsfilter","Choose Variable for Statsitical Plot:", choices=colnames(statstable())[names(statstable()) %in% c("label", input$network, "Participant Id") == FALSE])
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
                                         vertices = Participant_Details()) %>%
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
       
     } else if(input$network == "Hub Centrality") {
       V(new_graph)$value <- hub_score(new_graph)$vector
       V(new_graph)$label <- ifelse (V(new_graph)$value > quantile (V(new_graph)$value,0.99),V(new_graph)$name,NA)
       new_graph
     } else if(input$network == "Authority Centrality") {
       V(new_graph)$value <- authority.score(new_graph)$vector
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
       group_by(Id, Type) %>%
       summarise (Expenses = round(mean(Expenses),2)) 
   })
   
   df <- reactive ({
     
     if(input$filterFunnel1 == "All" & input$filterFunnel2 == "All") {
       
       total_data <- total_data %>%
         group_by (Id, Type) %>%
         summarise (Expenses = round(mean(Expenses),2), Visit = round(mean(Visit),0))
       
     }else if (input$filterFunnel1 != "All" & input$filterFunnel2 == "All") {
       
       total_data <- total_data %>%
         filter (workday == input$filterFunnel1) %>%
         group_by (Id, Type) %>%
         summarise (Expenses = round(mean(Expenses),2), Visit = round(mean(Visit),0))
       
     }else if (input$filterFunnel1 == "All" & input$filterFunnel2 != "All") {
       
       total_data <- total_data %>%
         filter (DateMonth == input$filterFunnel2) %>%
         group_by (Id, Type) %>%
         summarise (Expenses = round(mean(Expenses),2), Visit = round(mean(Visit),0))
       
     }else if (input$filterFunnel1 != "All" & input$filterFunnel2 != "All") {
       
       total_data <- total_data %>%
         filter (DateMonth == input$filterFunnel2) %>%
         filter (workday == input$filterFunnel1) %>%
         group_by (Id, Type) %>%
         summarise (Expenses = round(mean(Expenses),2), Visit = round(mean(Visit),0))
       
     }
     
     if (input$filterFunnel5 != "All") {
       
       total_data <- total_data %>%
         filter (Type == input$filterFunnel5)
     }
     
     m <- lm(Expenses ~ Visit, data=total_data) 
     fit95 <- predict(m, interval="conf", level=as.numeric(input$filterFunnel3))
     fit99 <- predict(m, interval="conf", level=as.numeric(input$filterFunnel4))
     total_data <- cbind.data.frame(total_data, 
                            lwr95=fit95[,"lwr"],  upr95=fit95[,"upr"],     
                            lwr99=fit99[,"lwr"],  upr99=fit99[,"upr"])
     
     total_data$lwr95[total_data$lwr95 < 0] <- 0
     total_data$upr95[total_data$upr95 < 0] <- 0
     total_data$lwr99[total_data$lwr99 < 0] <- 0
     total_data$upr99[total_data$upr99 < 0] <- 0
     total_data
   })
   
   #####################################################Plotting of Graph####################################################
   
   #Plot for Demographics Tabs
   
   output$point1_info1 <- renderInfoBox({
     infoBox(
       "Around 13%", "Non-residents among data Coming from Low and High Eduction Level", icon = icon("ok-sign", lib = "glyphicon"),
       color = "yellow", fill = TRUE)
   })
   
   output$point1_info2 <- renderInfoBox({
     infoBox(
       " ", HTML("<br/>High Income Level brings more Joviality"), icon = icon("pushpin", lib = "glyphicon"),
       color = "olive", fill = TRUE)
   })
   
   output$point1_info3 <- renderInfoBox({
     infoBox(
       "1.8%", "Shared Apartments in Residential Buildings", icon = icon("signal", lib = "glyphicon"),
       color = "red", fill = TRUE)
   })
   
   output$point1_info4 <- renderInfoBox({
     infoBox(
       "0.6%", "People keep changing their Residential Places", icon = icon("tag", lib = "glyphicon"),
       color = "light-blue", fill = TRUE)
   })
   
   output$point1_info5 <- renderInfoBox({
     infoBox(
       "99.4%", "People stay one Apartment throughout", icon = icon("bookmark", lib = "glyphicon"),
       color = "purple", fill = TRUE)
   })
   
   output$barPlot <- renderPlot({
     ggplot(demo_dataset(),
            aes(x = .data[[input$demo_category]])) + 
       geom_bar(fill= '#c7efe5') +
       geom_text(stat = 'count',
                 aes(label= paste0(stat(count), ', ', 
                                   round(stat(count)/sum(stat(count))*100, 
                                         1), '%')), vjust= -0.5, size= 2.5) +
       labs(y= 'No. of\nResidents', x= input$demo_category,
            title = paste0("Distribution of Residents by ",input$demo_category)) +
       theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
             panel.background= element_blank(), axis.line= element_line(color= 'grey'), legend.position="none",
             plot.title = element_text(size = 14, face = "bold"),
             text=element_text(family="serif")) 
     
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
                                                  plot.title = element_text(size = 12, face = "bold", hjust=0.5),
                                                  text=element_text(family="serif")),
       palette  = "Set2"
     )
     
   })
   
   output$buildingBarPlot <- renderPlot({
     ggplot(buildingData(),
            aes(x = Region, fill = .data [[input$Demo_Buildings1]])) + 
       geom_bar(position="dodge", stat="count") +
       scale_fill_manual(values=c("#ed81bf",
                                  "#539CF0")) +
       geom_text(stat = 'count',
                 aes(label= paste0(stat(count), ', ', 
                                   round(stat(count)/sum(stat(count))*100, 
                                         1), '%')), position=position_dodge(width=0.9), vjust=-0.35, size = 2.5) +
       labs(y= 'No. of\nBuildings', x= "Region",
            title = paste0("Distribution of Buildings by ", input$Demo_Buildings1)) +
       theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
             panel.background= element_blank(), axis.line= element_line(color= 'grey'),
             plot.title = element_text(size = 14, face = "bold"), legend.position="none", text=element_text(family="serif")) 
     
   })
   
   building_plot <- reactive ({
     
     buildings_details <- buildings_details %>%
       filter (time == input$time) %>%
       filter (count >= as.numeric(input$count[1]) & count <= as.numeric(input$count[2]))
     buildings_details
   })
   
   output$buildingPlot <- renderPlot ({
     
     ggplot () +
       geom_point(data = building_plot(), aes(x = long, y = lat, size = count)) +
       geom_sf(data = Region, aes(fill = region), alpha = 0.6) +
       scale_fill_manual("Region", values = c("#3747b4", "#01df8c", "#ac3400", "#a8c5ff")) +
       geom_point(data = building_plot(), aes(x = long, y = lat, size = count), color = "black", pch=21, fill = "2B54F0") +
       scale_size_continuous("Resident Count", breaks = c(1, 3, 5, 7, 10)) +
       theme_graph() + 
       theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                             size = 0.5), panel.background = element_rect(fill = "aliceblue"),
             text=element_text(family="serif"))
     
   })
   
   diam1 <- reactive({
     
     user_brush1 <- input$plot_click
     mysel <- brushedPoints(building_plot(), user_brush1)
     mysel_long <- mysel %>% unnest_longer(residents) %>%
       select(residents, buildingId)
     p_details <- mysel_long %>%
       left_join (Participant_Details(), by = c("residents" = "Participant ID"))
     return(p_details)
     
   })
   
   output$plot_brushedpoints1 <- DT::renderDataTable(DT::datatable(diam1()))
   

   ##Plot for Social Network Tabs
   
   output$point2_info1 <- renderInfoBox({
     infoBox(
       " ", HTML("<br/>People have more Social Activities in the Evening"), icon = icon("ok-sign", lib = "glyphicon"),
       color = "yellow", fill = TRUE)
   })
   
   output$point2_info2 <- renderInfoBox({
     infoBox(
       " ", HTML("<br/>Weekends has higher Social Activities (Pub Visit) throughout the day"), icon = icon("tag", lib = "glyphicon"),
       color = "olive", fill = TRUE)
   })
   
   output$point2_info3 <- renderInfoBox({
     infoBox(
       " ", HTML("Joviality, Region and Pub Visit are factors for Influence Level"), icon = icon("bookmark", lib = "glyphicon"),
       color = "red", fill = TRUE)
   })
   
   output$point2_info4 <- renderInfoBox({
     infoBox(
       " ", HTML("No Social Interaction recorded by Non-residents"), icon = icon("pushpin", lib = "glyphicon"),
       color = "light-blue", fill = TRUE)
   })
      
   
   output$socialstatsPlot <- renderPlot({
     req(input$plot)
     
     ggbetweenstats(
       data = statsplot(),
       x = !!rlang::sym(input$socialfilter),
       y = Visitcount,
       xlab = gsub("_", " ",input$socialfilter),
       ylab = "Visit Count",
       pairwise.comparisons = FALSE,
       ggtheme = ggplot2::theme_classic() + theme(axis.title.y= element_text(angle=0),
                                                  plot.title = element_text(size = 14, face = "bold", hjust=0.5),
                                                  text=element_text(family="serif")),
       ggplot.component = ggplot2::scale_color_manual(values = color_palettes),
       title = paste0("Visit Count of Pubs by ", input$socialfilter)
     )
     
   })
   
   output$socialstatsPlotgroup <- renderPlot({
    req(input$plot)
     
          grouped_ggbetweenstats(
                   data = dplyr::filter(statsplot(), !!rlang::sym(input$socialfiltergroup) %in% input$t1),
                   x = !!rlang::sym(input$socialfilter),
                   y = Visitcount,
                   grouping.var = !!rlang::sym(input$socialfiltergroup),
                   ylab = "Visit Count",
                   xlab = gsub("_", " ",input$socialfilter),
                   pairwise.comparisons = FALSE,
                   ggtheme = ggplot2::theme_classic() + theme(axis.title.y= element_text(angle=0),
                                                              plot.title = element_text(size = 14, face = "bold", hjust=0.5),
                                                              text=element_text(family="serif")),
                   ggplot.component = ggplot2::scale_color_manual(values = color_palettes),
                   annotation.args  = list(title = paste0("Visit Count of Pubs by ", input$socialfilter))
                 )
     
     
   })
   
   
   output$socialPlot <- renderPlot ({
     
     
     new_graph1 <- delete_vertices(social_graph(), V(social_graph())[value < quantile (V(social_graph())$value,0.9)])
     filter <- quantile (V(new_graph1)$value,0.9)
     
     ggraph(new_graph1, layout = "graphopt") +
       geom_edge_link(edge_colour = "#a46cb7", edge_width = 0.05) + 
       geom_node_point(aes(size = ifelse (V(new_graph1)$value > filter, 4, 0.001)),color = ifelse (V(new_graph1)$value > filter, "#cb6a49", "#7aa457")) +
       geom_node_label(aes(label = ifelse (V(new_graph1)$value > filter, V(new_graph1)$name, NA),family="serif" ), repel = TRUE) +
       theme_graph() +
       labs(title = paste0("Top 1% influential participant based on ",input$network)) +
       theme(legend.position = "none", plot.title=element_text(size = 10, hjust=0.5, vjust=0.5, face='bold'),
             text=element_text(family="serif"))
     

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
                                                    plot.title = element_text(size = 14, face = "bold", hjust=0.5),
                                                    text=element_text(family="serif")),
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
                                                    plot.title = element_text(size = 14, face = "bold", hjust=0.5),
                                                    text=element_text(family="serif")),
         ggplot.component = ggplot2::scale_color_manual(values = color_palettes),
         title = paste0(input$network, " comparison with ", input$networkstatsfilter)
       )
       
     }
     
     
   })
   
  
    ### Plot for BusinessPlot
   
   output$point3_info1 <- renderInfoBox({
     infoBox(
       " ", HTML("<br/>Revenue of Restaurants is stable, but Pubs is fluctuated among Months"), icon = icon("tag", lib = "glyphicon"),
       color = "yellow", fill = TRUE)
   })
   
   output$point3_info2 <- renderInfoBox({
     infoBox(
       " ", HTML("<br/>Pubs are the Predominant Business in Ohio"), icon = icon("pushpin", lib = "glyphicon"),
       color = "olive", fill = TRUE)
   })
   
   output$point3_info3 <- renderInfoBox({
     infoBox(
       " ", HTML("<br/>Venue Visit has no correlation with Revenue"), icon = icon("ok-sign", lib = "glyphicon"),
       color = "red", fill = TRUE)
   })
   
   output$point3_info4 <- renderInfoBox({
     infoBox(
       " ", HTML("<br/>Pubs have Higher Revenue despite Lesser Visit"), icon = icon("bookmark", lib = "glyphicon"),
       color = "light-blue", fill = TRUE)
   })
   
   output$statsPlot <- renderPlot({
     
     ggstatsplot::ggbetweenstats(
       data = ggstatsplot(),
       x = DateMonth,
       y = Expenses,
       type = input$testType1,
       xlab = "Mon/Year",
       ylab = "Revenue",
       p.adjust.method = input$pvalueType1,
       plot.type = input$plotType1,
       ggtheme = ggplot2::theme_classic() + theme(axis.title.y= element_text(angle=0),
                                               plot.title = element_text(size = 14, face = "bold", hjust=0.5),
                                               text=element_text(family="serif")),
       title = paste0("Revenue of ", input$ggstatfilter, "s for different Months")
     ) +
       ggplot2::scale_color_manual(values = color_palettes)
     
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
                                               plot.title = element_text(size = 14, face = "bold", hjust=0.5),
                                               text=element_text(family="serif")),
        ggplot.component = ggplot2::scale_y_continuous (labels = comma),
        title = paste0(input$Weekday," Revenue of Pubs and Restaurants ")
      ) 
      
    })
    
    output$tMapEarnings <- renderTmap({
      
      tm_shape(Region)+
        tm_polygons(col = "region",
                    palette = c("#3747b4", "#01df8c", "#ac3400", "#a8c5ff"),
                    size = 1,
                    title = "Region",
                    border.col = "black",
                    border.lwd = 1) +
        tm_shape(ggstatsplot1()) +
        tm_symbols(size = 0.5, col = "Expenses", style = "cont", title.col = "Average Revenue",
                   shape = "Type", shapes.labels = c("Pub", "Restaurant"), title.shape = "Venue Type"
        ) +
        tm_layout(main.title= 'Averge Expenses of Venues', 
                  main.title.position = c('left'),
                  main.title.size = 1.5, legend.outside = TRUE,)
      
    })
    
    
    output$funnelPlot <- renderPlot(
    {
      
      M.lm=lm(Expenses~Visit,data=df())
      
      ggplot(df(), aes(Visit, Expenses)) +
        # Add background
        geom_ribbon(aes(ymin= df()$upr99, ymax = Inf), fill = "#e2a49a", alpha = 0.5) +
        geom_ribbon(aes(ymin = df()$lwr99, ymax = df()$upr99), fill = "#e0ba9d", alpha = 0.5 ) +
        geom_ribbon(aes(ymin = 0, ymax = df()$lwr99), fill = "#8fd6c9", alpha = 0.5) +
        
        # Overlay points and lines
        geom_point(aes(shape = Type, fill = Type), size = 3, color = "black") + 
        scale_fill_manual(values=c('#ea007c', '#00782b')) +
        scale_shape_manual(values=c(21, 25))+
        geom_smooth(method="lm", colour="black", lwd=1.1, se=FALSE) + 
        geom_line(aes(y = upr95), color="black", linetype=2) + 
        geom_line(aes(y = lwr95), color="black", linetype=2) +
        geom_line(aes(y = upr99), color="red", linetype=3) + 
        geom_line(aes(y = lwr99), color="red", linetype=3) +
        theme_classic() +
        labs(y= 'Revenue of\nVenue', x= "No. of Visit",
             title = paste0("Does Visit affect Revenues?"), caption = paste0("Rsquared: ", round(summary(M.lm)$r.squared,3), "\nAdjusted Rsquared: ", round(summary(M.lm)$adj.r.squared,3))) +
        scale_y_continuous (labels = comma,limits = c(0, NA)) +
        theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
              panel.background= element_blank(), axis.line= element_line(color= 'grey'),
              plot.title = element_text(size = 14, face = "bold"),
              text=element_text(family="serif")) 
        
    })
    
    diam <- reactive({
      
      user_brush <- input$plot_brush
      mysel <- brushedPoints(df(), user_brush)
      mysel <- mysel %>%
        select(-lwr95, -upr95, -lwr99, - upr99, -geometry)
      return(mysel)
      
    })
    
    output$plot_brushedpoints <- DT::renderDataTable(DT::datatable(diam()))
}

# Run the application 
shinyApp(ui = ui, server = server)
