## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.wide = TRUE----------------------------------------------------
library(refuge)
library(ggplot2)
library(fiftystater)
library(dplyr)
library(magrittr)
library(viridis)

all <- rfg_all_restrooms(accessible = TRUE, unisex = TRUE, verbose = FALSE)

all$state <-  tolower(trimws(all$state))

all$state <-if_else(all$state %in% tolower(state.name), 
                     all$state, 
                     tolower(state.name)[match(all$state,tolower(state.abb))])
  
all <- all %>% filter(country=="US", is.na(state)==FALSE) %>%
  group_by(state) %>% 
  summarise(n_toilets=n())

p_toilets <- ggplot(data=all, aes(map_id = state)) + 
  geom_map(aes(fill = n_toilets), map = fifty_states) + 
  scale_fill_viridis(name = "Number of\nRestrooms", label = scales::comma) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  fifty_states_inset_boxes() 

p_toilets

