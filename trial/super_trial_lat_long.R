Residential_Details <- st_as_sf(Residential_Details)

dput(head(`Participant_Details(880)`))


tm_shape(Residential_Details) +
  tm_bubbles (
    col = "Shared Apartment",
    size = "Rental Cost",
    border.col = "black",
    border.lwd = 1
  )


test <- `Participant_Details(880)` %>%
  select(`Participant ID`, `Have Kids`)


interaction_all <- interaction_all %>%
  filter (Type == "Resident") %>%
  select (Participant_ID, InteractionCount) %>%
  rename ("Participant ID" = "Participant_ID")

`Participant_Details(880)` <- `Participant_Details(880)` %>%
  left_join(interaction_all, by ="")



Region_long <- Region %>% tidyr::separate_rows(units, convert = TRUE)

Region_long <- Region_long %>%
  select (units,region)



`Participant_Details(880)` <- `Participant_Details(880)` %>% 
  rename("Residence Region" = "Region")


saveRDS(`Participant_Details(880)`, "Participant_Details(880).rds")

test$long <- sapply(test$geometry, "[", 1)
test$lat <- sapply(test$geometry, "[", 2)

ggplot (data = buildings) +
  geom_sf() +
  geom_point(data = test, aes(x = long, y = lat, shape = Type, color = Expenses, size = 1)) +
  scale_color_continuous(breaks = c(200000, 400000, 600000, 800000), label=comma) +
  theme_graph() + 
  labs(color = "Revenue", shape = 'Venue Type') +
  guides(size = "none") +
  ggtitle("Observation Sites", subtitle = "(20 Restaurants and 12 Pubs)") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))




ggplot() +
  geom_sf(data = buildings) +
  geom_point(data = test, aes(x = long, y = lat, shape = Type, color = Expenses, size = 1)) +
  scale_color_continuous(breaks = c(200000, 400000, 600000, 800000), label=comma) +
  labs(color = "Revenue", shape = 'Venue Type') +
  theme_graph() + 
  guides(size = "none")