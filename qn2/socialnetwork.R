packages = c('igraph', 'tidygraph', 
             'ggraph','lubridate', 'clock',
             'tidyverse', 'ggmap', 'ggstatsplot', 'ggside', 'ggdist', 'patchwork', 'hrbrthemes', 'ggplot2','zoo','d3Tree',"d3treeR","graphlayouts","RColorBrewer","ggsci")
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

#finance <- read_csv("qn2/rawdata/FinancialJournal.csv")
#Part_nodes <- readRDS("data/Participant_Details.rds")
Social_edge <- read_csv("rawdata/SocialNetwork.csv")
check_in <- read_csv("rawdata/CheckinJournal.csv")
apartments<- read_csv("rawdata/Apartments.csv")
travel <- read_csv("rawdata/TravelJournal.csv")
buildings <- read_sf("data/Buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")


########################Revenue of Pubs and Restaurant ###################################################
travel_cost <- travel %>%
  mutate (expenses = abs(startingBalance - endingBalance))

travel_cost <- travel_cost %>%
  filter (travelEndLocationId %in% total_month_data$Id) %>%
  mutate (MonthYear = month(travelStartTime,
                            label = TRUE,
                            abbr = TRUE)) %>%
  mutate (Year = year(travelStartTime)) %>%
  mutate (Weekday = wday(travelStartTime,
                         label = TRUE,
                         abbr = FALSE)) %>%
  mutate (workday = case_when(
    Weekday %in% work_day ~ "Working Day",
    TRUE ~ "Non-Working Day"
  )) %>%
  select(-travelStartLocationId, -travelStartTime, -travelEndTime,-purpose,-checkInTime, -checkOutTime, -startingBalance, -endingBalance) %>%
  rename ("Id" = "travelEndLocationId")


travel_cost_monthly <- travel_cost %>%
  group_by(Id,MonthYear,Year,workday) %>%
  summarise(Expenses = sum(expenses), Visit = n())

travel_visit_monthly <- travel_cost %>%
  group_by(Id,MonthYear,Year,workday) %>%
  summarise(Visit = n())


travel_cost_monthly_1 <- travel_cost_monthly %>%
  unite('DateMonth',MonthYear:Year, sep= " ")

Pubs <- read_sf("rawdata/Pubs.csv", 
                options = "GEOM_POSSIBLE_NAMES=location")
Restaurants <- read_sf("rawdata/Restaurants.csv", 
                       options = "GEOM_POSSIBLE_NAMES=location")

Pubs$Type <- "Pubs"
Restaurants$Type <- "Restaurant"

Pubs <- Pubs %>%
  rename("Id" = "pubId") %>%
  select(Id,maxOccupancy,location,Type)

Restaurants <- Restaurants %>%
  rename("Id" = "restaurantId") %>%
  select(Id,maxOccupancy,location,Type)

Venue_Details <- rbind(Pubs,Restaurants)

month_level = c("Mar 2022", "Apr 2022", "May 2022", "Jun 2022", "Jul 2022", "Aug 2022", "Sep 2022", "Oct 2022", "Nov 2022", "Dec 2022", "Jan 2023", "Feb 2023", "Mar 2023", "Apr 2023", "May 2023")
travel_cost_monthly_1$DateMonth <- factor(travel_cost_monthly_1$DateMonth, levels=month_level)


business_plot <- merge(Venue_Details,travel_cost_monthly_1, by = "Id", all.x = TRUE, all.y = TRUE)

x <- 1

business_plot$long <- sapply(business_plot$geometry, "[", 1)
business_plot$lat <- sapply(business_plot$geometry, "[", 2)

business_plot <- business_plot %>%
  select (-geometry)

business_plot <- dplyr::select(as.data.frame(business_plot), -geometry)

test <- business_plot %>%
  filter(
    if (x != 2) {
      workday == workday
    } else {
      workday == "Working Day"
    }
  ) %>%
  group_by(Id, Type, long, lat) %>%
  summarise (Expenses = sum(Expenses), Visit = sum(Visit)) %>%
  ungroup

saveRDS(business_plot, "business_plot.rds")
########################Revenue of Pubs and Restaurant ###################################################



########################Social Activities of Pubs ###################################################
work_day <- c("Mon", "Tue", "Wed", "Thu", "Fri")

morning <- c(6:12)
afternoon <- c(13:18)
night <- c(19:23)

recreation_visit <- travel %>%
  filter (purpose == "Recreation (Social Gathering)") %>%
  mutate (MonthYear = month(travelStartTime,
                            label = TRUE,
                            abbr = TRUE)) %>%
  mutate (Year = year(travelStartTime)) %>%
  mutate (Hour = hour(checkInTime)) %>%
  mutate (Hour = hour(checkOutTime)) %>%
  mutate (Weekday = wday(travelStartTime,
                         label = TRUE,
                         abbr = TRUE)) %>%
  mutate (workday = case_when(
    Weekday %in% work_day ~ "Working Day",
    TRUE ~ "Non-Working Day"
  )) %>%
  mutate (session = case_when(
    Hour < 6 ~ "Midnight",
    Hour < 13 ~ "Morning",
    Hour < 19 ~ "Afternoon",
    TRUE ~ "Evening"
  )) %>%
  select(-travelStartLocationId, -travelStartTime, -travelEndTime,-purpose,-checkInTime, -checkOutTime, -startingBalance, -endingBalance) %>%
  rename ("Id" = "travelEndLocationId")

month_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri","Sat", "Sun")
session_levels <- c("Morning", "Afternoon", "Evening", "Midnight")

recreation_visit$Weekday <- factor(recreation_visit$Weekday , levels=month_levels)
recreation_visit$session <- factor(recreation_visit$session , levels=session_levels)
recreation_visit$Dates <- factor(recreation_visit$Dates, levels=month_level)


recreation_visit <- recreation_visit %>%
  unite('DateMonth',MonthYear:Year, sep= " ")

recreation_visit <- recreation_visit %>%
  rename (
    "Participant Id" = "participantId",
    "Pub Id" = "Id",
    "Dates"  = "DateMonth",
    "Time Period" = "Hour",
    "Workday Type" = "workday",
    "Session" = "session"
  )

travel_visit_monthly <- recreation_visit %>%
  group_by(Weekday) %>%
  summarise(Visit = n())

x <- subset(travel_visit_monthly, select=-c(Visit))

x <- colnames(travel_visit_monthly)[names(travel_visit_monthly) !="Visit"]


new_travel_visit_monthly_region <- travel_visit_monthly %>%
  left_join(Region_long, by = c("Id"="units"))


ggplot(new_travel_visit_monthly_region, aes(x = Weekday, y = Visit, fill = session)) +
  geom_bar(stat = "identity", position = "dodge")

count <- travel_visit_monthly %>%
  group_by (Id) %>%
  summarise (count = n())

part_vist <- recreation_visit %>%
  group_by (participantId) %>%
  summarise (count = n())


numbers <- recreation_visit %>%
  left_join (Region_long, by = c("Pub Id" = "units")) %>%
  select (-geometry)

recreation_visit <- numbers %>%
  rename("Region" = "region")

travel_cost_total_session <- travel_cost %>%
  group_by(Id,MonthYear,Year,workday) %>%
  summarise(Expenses = sum(expenses), Visit = n())

travel_cost_monthly_1 <- travel_cost_monthly %>%
  unite('DateMonth',MonthYear:Year, sep= " ")

Pubs <- read_sf("rawdata/Pubs.csv", 
                options = "GEOM_POSSIBLE_NAMES=location")
Restaurants <- read_sf("rawdata/Restaurants.csv", 
                       options = "GEOM_POSSIBLE_NAMES=location")

Pubs$Type <- "Pubs"
Restaurants$Type <- "Restaurant"

Pubs <- Pubs %>%
  rename("Id" = "pubId") %>%
  select(Id,maxOccupancy,location,Type)

Restaurants <- Restaurants %>%
  rename("Id" = "restaurantId") %>%
  select(Id,maxOccupancy,location,Type)

Venue_Details <- rbind(Pubs,Restaurants)

month_level = c("Mar 2022", "Apr 2022", "May 2022", "Jun 2022", "Jul 2022", "Aug 2022", "Sep 2022", "Oct 2022", "Nov 2022", "Dec 2022", "Jan 2023", "Feb 2023", "Mar 2023", "Apr 2023", "May 2023")
travel_cost_monthly_1$DateMonth <- factor(travel_cost_monthly_1$DateMonth, levels=month_level)


business_plot <- merge(Venue_Details,travel_cost_monthly_1, by = "Id", all.x = TRUE, all.y = TRUE)

x <- 1

business_plot$long <- sapply(business_plot$geometry, "[", 1)
business_plot$lat <- sapply(business_plot$geometry, "[", 2)

business_plot <- business_plot %>%
  select (-geometry)

business_plot <- dplyr::select(as.data.frame(business_plot), -geometry)

test <- business_plot %>%
  filter(
    if (x != 2) {
      workday == workday
    } else {
      workday == "Working Day"
    }
  ) %>%
  group_by(Id, Type, long, lat) %>%
  summarise (Expenses = sum(Expenses), Visit = sum(Visit)) %>%
  ungroup

time_levels <- c(0:23)

recreation_visit$`Participant Id` <- as.character(recreation_visit$`Participant Id` )

recreation_visit$`Time Period` <- factor(recreation_visit$`Time Period`, levels=time_levels)

recreation_visit$Weekday <- factor(recreation_visit$Weekday , ordered = FALSE )



saveRDS(recreation_visit, "recreation_visit.rds")
########################Revenue of Pubs and Restaurant ###################################################


#Check Where Non_Resident Work
check_in_nonR <- check_in %>%
  filter (venueType == "Workplace") %>%
  filter (!(participantId %in% `Participant_Details(880)`$`Participant ID`)) %>%
  group_by (participantId, venueId) %>%
  summarise (count = n())

check_in_check <- check_in_new %>%
  group_by(venueId) %>%
  summarise (count = n()) %>%
  rename ( "apartmentId" = "venueId")

newguide <- check_in_check %>%
  left_join(apartments,by = "apartmentId")

newguide$buildingId <- as.character(newguide$buildingId)

hey <- newguide %>%
  group_by(buildingId) %>%
  summarise(Freq = sum(count))

new_guide1 <- aggregate(newguide$count, by=list(buildingId=newguide$buildingId), FUN=sum)

buildings <- read_sf("data/Buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")

buildings <- buildings %>%
  left_join(hey, by="buildingId")

tm_shape(buildings)+
  tm_polygons(col = "Freq",
              size = 1,
              border.col = "black",
              border.lwd = 1)


Participant_Details <- participant_interaction %>%
  select(-No_of_Workplace) %>%
  left_join(check_in_new, by = "Participant ID")


check <- check_in_check$participantId

Participant_Details <- as.data.frame(Participant_Details)

Participant_Details <- st_drop_geometry(Participant_Details)

check_Part <- Participant_Details %>%
  filter (!(Participant_ID %in% check))


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
  mutate (MonthYear = month(timestamp,
                        label = TRUE,
                        abbr = TRUE)) %>%
  mutate (Year = year(timestamp)) %>%
  mutate (Weekday = wday(timestamp,
                         label = TRUE,
                         abbr = FALSE)) %>%
  mutate (workday = case_when(
    Weekday %in% work_day ~ "Working Day",
    TRUE ~ "Non-Working Day"
  ))


check_in_data <- check_in %>%
  mutate (Month = month(timestamp,
                            label = TRUE,
                            abbr = TRUE)) %>%
  mutate (Year = year(timestamp)) %>%
  mutate (Weekday = wday(timestamp,
                         label = TRUE,
                         abbr = FALSE)) %>%
  mutate (workday = case_when(
    Weekday %in% work_day ~ "Working Day",
    TRUE ~ "Non-Working Day"
  ))
  

parti <- Participant_Details %>%
  filter (Type == "Resident")

final_social_edge <- Social_edge_selected %>%
  filter (participantIdFrom %in% `Participant_Details(880)`$`Participant ID`) %>%
  filter (participantIdTo %in% `Participant_Details(880)`$`Participant ID`)

final_social_edge <- final_social_edge %>%
  unite('MonYear',MonthYear:Year, sep= " ") %>%
  select(-timestamp, -Weekday)


final_social_edge$participantIdFrom <- as.character(final_social_edge$participantIdFrom)


saveRDS(final_social_edge, "social_interaction_all.rds")
  
  



work_day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")


Social_edge_selected_workdays <- final_social_edge %>%
  filter (Year == 2022 | MonthYear == "Jan" | MonthYear == "Feb") %>%
  select(participantIdFrom,month,workday)

social_edge_plot <- Social_edge_selected_workdays %>%
  group_by (month) %>%
  summarise(Weight = n())
  

Participant_Details <- Participant_Details %>%
  rename ("Region" = "region")

saveRDS (final_social_edge, "social_interaction.rds")
saveRDS (Participant_Details, "participant_interaction.rds")


Social_edge_aggregated_workday <- final_social_edge %>%
  filter(MonYear == "Mar 2022" & workday == "Working Day") %>%
  group_by(participantIdFrom,participantIdTo) %>%
  summarise(Weight = n()) %>%
  filter (participantIdFrom != participantIdTo) %>%
  filter (Weight > 1) %>%
  ungroup

Social_edge_aggregated_nonworkday <- Social_edge_selected %>%
  filter(Month == "Aug" & Year == 2022 & workday == "Non-Working Day") %>%
  group_by(participantIdFrom,participantIdTo) %>%
  summarise(Weight = n()) %>%
  filter (participantIdFrom != participantIdTo) %>%
  filter (Weight > 1) %>%
  ungroup


Social_edge_aggregated <- Social_edge_aggregated %>%
  filter (participantIdTo %in% social_check$participantIdTo)

Part_nodes_aggregated_workday <- Participant_Details %>%
  filter (Participant_ID  %in% c(Social_edge_aggregated_workday$participantIdFrom, Social_edge_aggregated_workday$participantIdTo))

Part_nodes_aggregated_non_workingday <- Participant_Details %>%
  filter (Participant_ID  %in% c(Social_edge_aggregated_nonworkday$participantIdFrom, Social_edge_aggregated_nonworkday$participantIdTo))


new_graph <- graph_from_data_frame (Social_edge_aggregated_workday,
                                 vertices = `Participant_Details(880)`) %>%
  as_tbl_graph()

social_graph_sep_nonworking <- graph_from_data_frame (Social_edge_aggregated_nonworkday,
                                                  vertices = Part_nodes_aggregated_non_workingday) %>%
  as_tbl_graph()


V(new_graph)$value <- degree(new_graph)
V(social_graph_sep_working)$eig <- evcent(social_graph_sep_working)$vector
V(social_graph_sep_working)$hubs <- hub.score(social_graph_sep_working)$vector
V(social_graph_sep_working)$authorities <- authority.score(social_graph_sep_working)$vector
V(social_graph_sep_working)$closeness <- closeness(social_graph_sep_working)
V(social_graph_sep_working)$betweenness <- betweenness(social_graph_sep_working)
V(social_graph_sep_working)$pagerank <- page_rank(social_graph_sep_working)$vector

V(social_graph_sep_nonworking)$degree <- degree(social_graph_sep_nonworking)
V(social_graph_sep_nonworking)$eig <- evcent(social_graph_sep_nonworking)$vector
V(social_graph_sep_nonworking)$hubs <- hub.score(social_graph_sep_nonworking)$vector
V(social_graph_sep_nonworking)$authorities <- authority.score(social_graph_sep_nonworking)$vector
V(social_graph_sep_nonworking)$closeness <- closeness(social_graph_sep_nonworking)
V(social_graph_sep_nonworking)$betweenness <- betweenness(social_graph_sep_nonworking)
V(social_graph_sep_nonworking)$pagerank <- page_rank(social_graph_sep_nonworking)$vector

new_graph <- delete_vertices(new_graph, V(new_graph)[value < quantile (V(new_graph)$value,0.9)])

ggraph(new_graph, layout = "nicely") +
  geom_edge_link(edge_colour = "grey", edge_width = 0.05) + 
  geom_node_point(aes(size = ifelse (V(new_graph)$value > high_level, 4, 0.001)),color = ifelse (V(new_graph)$value > high_level, "#98984d", "#b3669e")) +
  geom_node_label(aes(label = ifelse (V(new_graph)$value > high_level, V(new_graph)$name, NA)), repel = TRUE) +
  theme_graph() +
  ggtitle("Text Network") +
  theme(panel.background = element_rect(fill = "white"), legend.position = "none", plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))


vertex_attr(new_graph)

V(new_graph)$degree <- degree(new_graph)
new_graph <- delete_vertices(new_graph, V(new_graph)[degree < quantile (V(new_graph)$degree,0.9)])
filter <- quantile (V(new_graph)$degree,0.9)
V(new_graph)$size <- ifelse (V(new_graph)$degree > filter, 10, 0.01)
V(new_graph)$color <- ifelse (V(new_graph)$degree > filter, "darkgoldenrod3", "azure3")
V(new_graph)$label <- ifelse (V(new_graph)$degree > filter,V(new_graph)$name,NA) +
  
dataframe <- as_tibble(new_graph, what="vertices")

participant_interaction <- participant_interaction %>%as.character(participant_interaction$Participant_ID)

new_table <- participant_interaction %>%
  filter (as.character(Participant_ID) %in% V(new_graph)$label)

plot(new_graph,layout=layout.mds, edge.arrow.size=0.1,edge.arrow.mode = "-", vertex.label.cex = 1, vertex.label.font = 2)


low_level

high_level <- quantile (V(new_graph)$value,0.9)
high_level1 <- quantile (V(social_graph_sep_working)$eig,0.99)
high_level2 <- quantile (V(social_graph_sep_working)$hubs,0.99)
high_level3 <- quantile (V(social_graph_sep_working)$authorities,0.99)
high_level4 <- quantile (V(social_graph_sep_working)$pagerank,0.99)


V(social_graph_sep_working)$color <- ifelse (V(social_graph_sep_working)$degree > high_level, "darkgoldenrod3", "azure3")
V(social_graph_jun)$size <- ifelse (V(social_graph_jun)$eig > high_level, 2, 0.05)
V(social_graph_jun)$label <- ifelse (V(social_graph_jun)$eig > high_level,V(social_graph_jun)$name,NA)

V(social_graph_jun)$clu <- as.character(membership(cluster_edge_betweenness(social_graph_jun)))


vertex_attr(x, "name", index = V(x)$degree > high_level)
vertex_attr(social_graph_sep_working, "name", index = V(social_graph_sep_working)$eig > high_level1)
vertex_attr(social_graph_sep_working, "name", index = V(social_graph_sep_working)$hubs > high_level2)
vertex_attr(social_graph_sep_working, "name", index = V(social_graph_sep_working)$authorities > high_level3)
vertex_attr(social_graph_sep_working, "name", index = V(social_graph_sep_working)$pagerank > high_level4)

plot(social_graph_jun,layout=layout.mds, edge.arrow.size=0.1,edge.arrow.mode = "-", vertex.label.cex = 0.65, vertex.label.font = 1)

saveRDS(Part_nodes_aggregated_non_workingday,"SUREWORK.rds")
saveRDS(social_graph_workingday,"social_graph_workingday.rds")

edge_attr(cgraph)

cgraph <- delete_edges(cgraph, which(E(cgraph)$work_day == "Non-Working Days"))


Treemap_Part <- Social_edge %>%
  rename('Participant ID' = 'participantIdFrom') %>%
  filter (`Participant ID` %in% Participant_Details$`Participant ID`) %>%
  group_by(`Participant ID`) %>%
  summarise (InteractionCount = n()) %>%
  left_join(Participant_Details, by = "Participant ID")

saveRDS(Treemap_Part, "Participant_Details.rds")

Social_edge_all<- Social_edge %>%
  mutate (MonthYear = as.yearmon(timestamp,"%m/%Y")) %>%
  rename('Participant_ID' = 'participantIdFrom') %>%
  select (Participant_ID, MonthYear)


Social_edge_selected_2022 <- Social_edge_selected %>%
  filter (Year == 2022) %>%
  rename('Participant_ID' = 'participantIdFrom') %>%
  select (Participant_ID, Month)

as <- merge(Social_edge_selected_2022,Part_nodes, by = "Participant_ID")

interaction_all <- merge(Social_edge_all,Part_nodes, by = "Participant_ID")



trial <- interaction_all %>%
  group_by(Participant_ID) %>%
  mutate (InteractionCount = n()) %>%
  distinct(Participant_ID, .keep_all = TRUE) %>%
  select(-MonthYear)

trial$Age_Group <- as.character(trial$Age_Group)

saveRDS(trial, "interaction_all.rds")



index <- sample(1:nrow(Part_nodes_aggregated),100,replace = FALSE)
random <- dput(Part_nodes_aggregated[index,])

dput(head(cgraph))


total_data$type <- sub("pub","Pub",total_data$type)


saveRDS(total_data,"total_data.rds")