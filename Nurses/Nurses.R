# 1 option ----------------------------------------------------------------

library(tidyverse)
library(urbnmapr)
library(urbnthemes)
library(janitor)

set_urbn_defaults(style = "map")

nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')

nurses <- nurses %>% 
  filter(Year == "2020") %>% 
  clean_names() %>% 
  right_join(states, by = c("state" = "state_name"))

nurses %>% 
  ggplot(aes(long, lat, group = group, fill = total_employed_rn)) +
    geom_polygon(color = "#ffffff", size = 0.25) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45)

nurses

# 1 option BUFFED ----------------------------------------------------------------
# https://www.cgoodman.com/blog/archives/2018/06/16/maps-in-r-using-urbnmapr/

library(tidyverse)
library(urbnmapr)
library(urbnthemes)
library(janitor)
library(viridis)
library(scales)

set_urbn_defaults(style = "map")

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-10-05/readme.md

nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')

nurses <- nurses %>% 
  filter(Year == "2020") %>% 
  clean_names() %>% 
  right_join(states, by = c("state" = "state_name"))

# state column not needed, but good to see
nurses <- nurses %>% 
  select(state, long, lat, group, total_employed_rn)
nurses

nurses %>% 
  ggplot(aes(long, lat, group = group, fill = total_employed_rn)) +
  geom_polygon(color = "#ffffff", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_viridis(
    option = "viridis",
    name = "RNs",
    # discrete = F,
    direction = -1, # flips the order so dark one is higher in this case
    # end=0.9, # lowers the width of the color scale, prefer not using it
    labels = comma,
    na.value = "red"
  ) +
  labs(
    title = "Total Employed RN by State, 2020",
    fill = "RNs" # alternate option to change fill text, does not work here bc of name = above
  ) +
  theme(
    text = element_text(family = "mono", size = 11),
    plot.title = element_text(size = 20,
                              hjust = 0.5)
  )

# County example ----------------------------------------------------------

# Setup -------------------------------------------------------------------

library(tidyverse)
library(urbnmapr)
library(urbnthemes)
library(janitor)
library(viridis)
library(scales)

set_urbn_defaults(style = "map")

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-11/readme.md
broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv') %>% 
  clean_names()

broadband_4_digits <- broadband %>% 
  mutate(id_length = str_length(county_id)) %>% 
  filter(id_length == 4) %>% 
  mutate(county_fips = paste0("0",county_id))

broadband_5_digits <- broadband %>% 
  mutate(id_length = str_length(county_id)) %>% 
  filter(id_length == 5) %>% 
  mutate(county_fips = county_id)

broadband <- rbind(broadband_4_digits, broadband_5_digits)

# JOINING BY NAME IS A PROBLEM BECAUSE THERE ARE MULTIPLE COUNTIES WITH THE SAME NAME IN THE US
broadband <- broadband %>% 
  right_join(counties, by = "county_fips")

# Basic plot with nothing else
counties %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)


# Attempt 1 unfilled ------------------------------------------------------

# Works, now how to fill???
broadband %>% 
  ggplot(aes(long, lat, group = group)) +
    geom_polygon(color = "#ffffff", size = .25) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45)


# Attempt 2 with filling --------------------------------------------------
# Fill attempt 1, WORKS but how to fill in missing counties / outline them
broadband %>% 
  filter(broadband_availability_per_fcc >= 0) %>% 
  mutate(broadband_availability_per_fcc = as.double(broadband_availability_per_fcc)) %>% 
  ggplot(aes(long, lat, group = group, fill = broadband_availability_per_fcc)) +
    geom_polygon(color = "#ffffff", size = .25) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_viridis(
      option = "viridis",
      name = "",
      # discrete = F,
      direction = -1, # flips the order so dark one is higher in this case
      # end=0.9, # lowers the width of the color scale, prefer not using it
      labels = percent
    ) +
    labs(
      title = "Broadband availability",
      fill = ""
    ) +
    theme(
      text = element_text(family = "mono", size = 11),
      plot.title = element_text(size = 20,
                                hjust = 0.5)
    )



# Attempt 3 ---------------------------------------------------------------
# Fill attempt 2 - Try to keep the - and just remove NA
# Had to add a color to geom_polygon or the unknown counties blended into the background
# Would like to outline the states in a darker color

broadband %>% 
  mutate(broadband_availability_per_fcc = case_when(
      broadband_availability_per_fcc == "-" ~ "0.00",
      TRUE ~ broadband_availability_per_fcc
    ),
    broadband_availability_per_fcc = as.double(broadband_availability_per_fcc)) %>% 
  ggplot(aes(long, lat, group = group, fill = broadband_availability_per_fcc)) +
    geom_polygon(color = "#ffffff", size = .25) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_viridis(
      na.value = "gray",
      option = "viridis",
      name = "Availability",
      # discrete = F,
      direction = -1, # flips the order -1 or 1
      # end=0.9, # lowers the width of the color scale, prefer not using it
      labels = percent
    ) +
    labs(
      title = "Broadband Availability",
      fill = ""
    ) +
    theme(
      text = element_text(family = "mono", size = 11),
      plot.title = element_text(size = 20,
                                hjust = 0.5)
    )

broadband %>% 
  filter(state_abbv == "OR") %>% 
  drop_na(broadband_availability_per_fcc) %>%
  mutate(broadband_availability_per_fcc = case_when(
    broadband_availability_per_fcc == "-" ~ "0.00",
    TRUE ~ broadband_availability_per_fcc
  ),
  broadband_availability_per_fcc = as.double(broadband_availability_per_fcc)) %>% 
  ggplot(aes(long, lat, group = group, fill = broadband_availability_per_fcc)) +
    geom_polygon(color = "#ffffff", size = .25) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_continuous(
      low = "lightgreen",
      high = "darkgreen",
      # na.value = "red",
      labels = percent
    ) +
    labs(
      title = "Broadband availability in OR",
      fill = "Availability"
    ) +
    theme(
      text = element_text(family = "mono", size = 11),
      plot.title = element_text(size = 20,
                                hjust = 0.5)
    )
