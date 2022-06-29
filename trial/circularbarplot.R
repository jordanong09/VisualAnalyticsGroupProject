library(dplyr)
library(ggplot2)
library(stringr)

Social_edge_selected_workdays<- Social_edge_selected_workdays %>%
  rename('Participant_ID' = 'participantIdFrom')

as <- merge(Social_edge_selected_workdays,participants_details, by = "Participant_ID")

as_1 <- as %>%
  group_by(month,weeknum,Education_Level) %>%
  summarise(value = n())


social_edge_plot$month <- as.factor(social_edge_plot$month )

plt <- ggplot (social_edge_plot) +
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:5) * 100000),
    color = "lightgrey"
  ) +
  geom_col(
    aes(
      x = reorder(str_wrap(month, 5), Weight),
      y = Weight,
      fill = Weight
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  coord_polar()

plt
# Get the name and the y position of each label
label_data <- as_1 %>% group_by(id, weeknum) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
empty_bar <- 2
nObsType <- nlevels(as.factor(as_1$Education_Level))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(as_1$month)*nObsType, ncol(as_1as_1$month)) )
colnames(to_add) <- colnames(as_1)
to_add$group <- rep(levels(as_1$month), each=empty_bar*nObsType )
as_1 <- rbind(as_1, to_add)
as_1 <- as_1 %>% arrange(month, weeknum)
as_1$id <- rep( seq(1, nrow(as_1)/nObsType) , each=nObsType)

# prepare a data frame for base lines
base_data <- as_1 %>% 
  group_by(month) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))


# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


p <- ggplot(as_1) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=Education_Level), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(as_1$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=weeknum, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=month), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p


