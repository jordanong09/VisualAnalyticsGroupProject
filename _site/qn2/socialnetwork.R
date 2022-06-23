packages = c('igraph', 'tidygraph', 
             'ggraph','lubridate', 'clock',
             'tidyverse', 'ggmap', 'ggstatsplot', 'ggside', 'ggdist', 'patchwork', 'hrbrthemes', 'ggplot2','zoo','d3Tree',"d3treeR")
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

finance <- read_csv("rawdata/FinancialJournal.csv")
Part_nodes <- read_csv("rawdata/Participants.csv")
Social_edge <- read_csv("rawdata/SocialNetwork.csv")

finance_new <- finance %>% 
  filter (category == "Wage") %>%
  group_by(participantId) %>%
  summarise(Weight = n()) %>%
  filter (Weight > 12) %>%
  ungroup

new_user <- finance_new[["participantId"]]

finance <- finance %>%
  filter(participantId %in% new_user)

financeJ <- finance %>% #load the financeJ data table
  select(c("participantId","category", "amount")) %>% #choose the columns to subset
  group_by(participantId,category)%>% 
  summarise(amount = round(sum(amount),2)) %>% #sum all the amount based on their ID and Category rounding off to 2 decimal place
  pivot_wider(names_from = "category",values_from = "amount") #pivot the table to have the categories in columns instead of rows
financeJ[is.na(financeJ)] = 0 #input a 0 value to all N.A field in the data table

financeJ_2 <- financeJ %>%
  mutate(Expenses = Education + Food + Recreation + Shelter + RentAdjustment) %>% #create new column to sum all the different categories of expenses
  mutate (Income = RentAdjustment + Wage) %>%
  select(participantId, Income, Expenses)

summary (financeJ_2)

Part_nodes <- Part_nodes %>%
  rename('Participant_ID' = 'participantId', 
         'Household_Size' = 'householdSize', 
         'Have_Kids' = 'haveKids', 
         'Age' = 'age', 
         'Education_Level' = 'educationLevel', 
         'Interest_Group' = 'interestGroup', 
         'Joviality' = 'joviality')

#rename value 
Part_nodes$Education_Level <- sub('HighSchoolOrCollege', 
                                  'High School or College',
                                  Part_nodes$Education_Level)

brks <- c(17, 20, 25, 30, 35, 40, 45, 50, 55, 60)
grps <- c('20 & Below', '21-25', '26-30', '31-35', '36-40', '41-45', 
          '46-50', '51-55', '56-60')

financeJ_2 <- financeJ_2 %>%
  rename('Participant_ID' = 'participantId')

Part_nodes$Age_Group <- cut(Part_nodes$Age, breaks=brks, labels = grps)

Part_nodes <- merge(financeJ_2,Part_nodes, by = "Participant_ID", all.x = TRUE, all.y = TRUE)

work_day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

Social_edge_selected <- Social_edge %>%
  mutate (Month = month(timestamp,
                        label = TRUE,
                        abbr = TRUE)) %>%
  mutate (Year = year(timestamp))



work_day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

Social_edge_selected_workdays <- Social_edge %>%
  mutate (Weekday = wday(timestamp,
                         label = TRUE,
                         abbr = FALSE)) %>%
  mutate (month = month(timestamp,
                        label = FALSE)) %>%
  mutate (week = lubridate::week(timestamp)) %>%
  mutate (work_day = case_when(
    Weekday %in% work_day ~ "Working Days",
    TRUE ~ "Non-Working Days"
  )) %>%
  select (participantIdFrom,participantIdTo,work_day)


Social_edge_aggregated <- Social_edge_selected_workdays %>% 
  group_by(participantIdFrom,participantIdTo,work_day) %>%
  summarise(Weight = n()) %>%
  filter (participantIdFrom != participantIdTo) %>%
  filter (Weight > 1) %>%
  ungroup

Part_nodes_aggregated <- Part_nodes %>%
  filter (Participant_ID  %in% c(Social_edge_aggregated$participantIdFrom, Social_edge_aggregated$participantIdTo))

social_graph <- graph_from_data_frame (Social_edge_aggregated,
                                 vertices = Part_nodes_aggregated) %>%
  as_tbl_graph()


saveRDS(cgraph,"social_graph.rds")

edge_attr(cgraph)

cgraph <- delete_edges(cgraph, which(E(cgraph)$work_day == "Non-Working Days"))


Social_edge_all<- Social_edge %>%
  mutate (MonthYear = as.yearmon(timestamp,"%m/%Y")) %>%
  rename('Participant_ID' = 'participantIdFrom') %>%
  select (Participant_ID, MonthYear)


Social_edge_selected_2022 <- Social_edge_selected %>%
  filter (Year == 2022) %>%
  rename('Participant_ID' = 'participantIdFrom') %>%
  select (Participant_ID, Month)

as <- merge(Social_edge_selected_2022,Part_nodes, by = "Participant_ID")

as_all <- merge(Social_edge_all,Part_nodes, by = "Participant_ID") 

as_1 <- as_all %>%
  group_by(MonthYear,Household_Size) %>%
  summarise(count = n()) %>%
  ungroup



as_2 <- as_all %>%
  group_by(Participant_ID,MonthYear, Interest_Group)%>%
  summarise(InteractionCount = n()) %>%
  ungroup


treemapPlot <- d3tree3(
    treemap(as_2,
            index = c("Interest_Group","Participant_ID"),
            vSize = "InteractionCount",
            type = "value",
            vColor = "InteractionCount",
            palette = "Set2",
            )  
    )


treemapPlot


index <- sample(1:nrow(Part_nodes_aggregated),100,replace = FALSE)
random <- dput(Part_nodes_aggregated[index,])

dput(head(cgraph))

df<-c(12,3,4,56,78,18,46,78,100)
quantile(df,0.9)