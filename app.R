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
           h2("Dashboard tab content"),
           fluidRow(
             column(5,
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
                                selected = "0.1")
                    )
             
           )),
    tabItem(tabName = "point1",
            h2("Demographics in City of Engagement"),
            fluidRow(
              valueBoxOutput("point1_info1"),
              valueBoxOutput("point1_info2"),
              valueBoxOutput("point1_info3")
            ),
            fluidRow(
              
              column(2, 
                     h4("Demographics of Population in City of Engagement"),
                     
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
                     h4("Demographics of Buildings in City of Engagement"),
                     radioButtons(
                       inputId = "Demo_Buildings",
                       label = "Choose Building Type",
                       choices = c("Buildings",
                                   "Residential",
                                   "Commercial"),
                       selected = "Buildings"
                     ),
                     selectInput(inputId = "Demo_Buildings1",
                                 label = "Choose a Category Type for Buildings Visualisation",
                                 choices = c( "Vacancy",
                                              "Building Type"
                                 ),
                                 selected = "Vacancy"),
                     br(),
                     
                     uiOutput("filter1"),
                     
                     br (),
                     
                     sliderInput("RentCost", "Rental Cost:",
                                 min = 1, max = 10, value = 100
                     ),
                     
              ),
              
              column(5,
                     plotOutput("buildingBarPlot")),
              
              column(5,
                     plotOutput("buildingPlot", brush = brushOpts(id = "plot_brush1")),
                     dataTableOutput("plot_brushedpoints1"))
              )
    ),
    tabItem(tabName = "point2",
            h2("Social Network Interaction in City of Engagement"),
            fluidRow(
              valueBoxOutput("point2_info1"),
              valueBoxOutput("point2_info2"),
              valueBoxOutput("point2_info3")
            ),
            fluidRow(
              column (4,
                      helpText(" Visualise the Social Network Interaction of the Population in City of Engagement"),
                      uiOutput("socialFilter"),
                      uiOutput("socialFilterfill"),
                      selectInput("t1", label="Filter Selection of Group Variable", choices=c(), multiple = TRUE),
                      actionButton("plot", "Plot Graph")
                      
                      
                      
              ),
              
              
              column (8,
                      plotOutput("socialstatsPlot")
              )
              
              
            ),
            
            hr(),
            
            fluidRow(
              plotOutput("socialstatsPlotgroup")
            ),
            
             
            hr(),
            
            fluidRow(
              column (2,                    
                      helpText(" Visualise the top 1% influential people in City of Engagement based on Month and Day"),
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
            h2("Predominant Business in City of Engagement"),
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
                     tmapOutput("tMapEarnings"))
            ),
            
            fluidRow(
              
              column(2, 
                     h4("Funnel Plot Selection"),
                     
                     selectInput(inputId = "filterFunnel1",
                                 label = "Choose a Weekday Type",
                                 choices = c( "All",
                                              "Non-Working Day",
                                              "Working Day"
                                 ),
                                 selected = "All"),
                     br(),
                     
                     selectInput(inputId = "filterFunnel2",
                                 label = "Choose a month to filter",
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
                                 selected = "All"),
                     br(),
                     selectInput(inputId = "filterFunnel5",
                                 label = "Choose a Venue Type: ",
                                 choices = c( "All",
                                              "Pubs",
                                              "Restaurant"
                                 ),
                                 selected = "All"),
                     
                     br(),
                     selectInput(inputId = "filterFunnel3",
                                 label = "Choose 1st Confidence Interval",
                                 choices = c( "90%" = "0.90",
                                              "95%" = "0.95",
                                              "99%" = "0.99",
                                              "99.9%" = "0.999"
                                              
                                 ),
                                 selected = "0.90"),
                     br(),
                     selectInput(inputId = "filterFunnel4",
                                 label = "Choose 1st Confidence Interval",
                                 choices = c( "90%" = "0.90",
                                              "95%" = "0.95",
                                              "99%" = "0.99",
                                              "99.9%" = "0.999"
                                              
                                 ),
                                 selected = "0.99")
                     
              ),
              
              column(7,
                     plotOutput("funnelPlot", brush = brushOpts(id = "plot_brush")),
                     dataTableOutput("plot_brushedpoints")
              ),
                  
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
    }
    
    
  })
  
  output$filter1 <- renderUI({
    radioButtons("fil1","Filter", choices=vards1())
  })
  
  buildingData <- reactive (
    {
      demo_buildings %>%
          filter(.data[[input$Demo_Buildings1]] == as.character(input$fil1))
    }
  )
  
   
   ##Reactive Values for Social Network Plot
  
  recreation_data <- reactive ({
  
    recreation_visit <- recreation_visit %>%
      left_join(Participant_Details(), by = c("Participant Id" = "Participant ID"))
    
    recreation_visit <- recreation_visit[, sapply(recreation_visit, class) %in% c('character', 'factor', 'logical')]
    
    recreation_visit
    
  })
  
  
  statsplot <- eventReactive (input$plot,{
    
    req(recreation_data())
    
    new1 <- recreation_data() %>%
      group_by(`Pub Id`, !! rlang::sym(input$socialfilter), !! rlang::sym(input$socialfiltergroup)) %>%
      summarise(Visitcount = n())
    
    new1
  })
  
  output$socialFilter <- renderUI({
    req(recreation_data())
    selectizeInput("socialfilter","Choose X Axis Variable for Statisitical Plot:", choices=colnames(recreation_data())[names(recreation_data()) %in% c("Visitcount", "Participant Id", "Pub Id") == FALSE])
  })
  
  output$socialFilterfill <- renderUI({
    req(recreation_data())
    selectInput("socialfiltergroup","Choose Group Variable for Statisitical Plot:", choices=colnames(recreation_data())[names(recreation_data())  %in% c("Visitcount", "Participant Id", "Pub Id", input$socialfilter) == FALSE])
  })
  
  observeEvent(input$socialfiltergroup, {
    choiceList <- recreation_visit %>%
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
       summarise (Expenses = mean(Expenses)) 
   })
   
   df <- reactive ({
     
     if(input$filterFunnel1 == "All" & input$filterFunnel2 == "All") {
       
       total_data <- total_data %>%
         group_by (Id, Type) %>%
         summarise (Expenses = mean(Expenses), Visit = mean(Visit))
       
     }else if (input$filterFunnel1 != "All" & input$filterFunnel2 == "All") {
       
       total_data <- total_data %>%
         filter (workday == input$filterFunnel1) %>%
         group_by (Id, Type) %>%
         summarise (Expenses = mean(Expenses), Visit = mean(Visit))
       
     }else if (input$filterFunnel1 == "All" & input$filterFunnel2 != "All") {
       
       total_data <- total_data %>%
         filter (DateMonth == input$filterFunnel2) %>%
         group_by (Id, Type) %>%
         summarise (Expenses = mean(Expenses), Visit = mean(Visit))
       
     }else if (input$filterFunnel1 != "All" & input$filterFunnel2 != "All") {
       
       total_data <- total_data %>%
         filter (DateMonth == input$filterFunnel2) %>%
         filter (workday == input$filterFunnel1) %>%
         group_by (Id, Type) %>%
         summarise (Expenses = mean(Expenses), Visit = mean(Visit))
       
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
       geom_bar(fill= '#c7efe5') +
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
             plot.title = element_text(size = 14, face = "bold"), legend.position="none") 
     
   })
   
   output$buildingPlot <- renderPlot ({
     
     buildings_details <- buildings_details %>%
       filter(time == "Mar 22") 
     
     
     # ggplot(data = buildings) +
     #   geom_sf() +
     #   geom_point(data = buildings_details, aes(x = long, y = lat, size = count)) +
     #   scale_size_continuous(breaks = c(2, 4, 6, 8, 10)) +
     #   theme_graph() + 
     #   labs(size = 'count') +
     #   ggtitle(paste0(input$time," @@ ")) +
     #   theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
     #                                         size = 0.5), panel.background = element_rect(fill = "aliceblue"))
     
     ggplot (data = buildings_details, aes(x = long, y = lat, size = count)) + 
       geom_point() +
       scale_size_continuous(breaks = c(2, 4, 6, 8, 10)) +
       theme_graph() + 
       labs(size = 'count') +
       theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                             size = 0.5), panel.background = element_rect(fill = "aliceblue"))
     
   })
   
   diam1 <- reactive({
     
     user_brush1 <- input$plot_brush1
     mysel <- brushedPoints(buildings_details, user_brush1)
     return(mysel)
     
   })
   
   output$plot_brushedpoints1 <- DT::renderDataTable(DT::datatable(diam1()))
   

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
      
   
   output$socialstatsPlot <- renderPlot({
     req(input$socialfilter)
     
     ggbetweenstats(
       data = statsplot(),
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
   
   output$socialstatsPlotgroup <- renderPlot({
    req(input$t1)
     
          grouped_ggbetweenstats(
                   data = dplyr::filter(statsplot(), !!rlang::sym(input$socialfiltergroup) %in% input$t1),
                   x = !!rlang::sym(input$socialfilter),
                   y = Visitcount,
                   grouping.var = !!rlang::sym(input$socialfiltergroup),
                   ylab = "VisitCount",
                   pairwise.comparisons = FALSE,
                   ggtheme = ggplot2::theme_classic() + theme(axis.title.y= element_text(angle=0),
                                                              plot.title = element_text(size = 14, face = "bold", hjust=0.5)),
                   ggplot.component = ggplot2::scale_color_manual(values = color_palettes),
                   annotation.args  = list(title = paste0("Visit Count of Pubs by ", input$socialfilter))
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
    
    output$tMapEarnings <- renderTmap({
      
      tm_shape(Region)+
        tm_polygons(col = "region",
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
        geom_point(aes(shape = Type, color = Type), size = 2) + 
        scale_color_manual(values=c('#E69F00', '#56B4E9')) +
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
              plot.title = element_text(size = 14, face = "bold")) 
        
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
