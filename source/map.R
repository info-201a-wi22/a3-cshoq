inmate_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(dplyr)
library(ggplot2)
library(usdata)

bw_2018 <- inmate_data %>%
  filter(year == 2018) %>%
  mutate(bw_jail_ratio = black_jail_pop / white_jail_pop) %>%
  select(state, bw_jail_ratio) %>%
  mutate(state = tolower(abbr2state(state)))
state_bw <- aggregate(bw_jail_ratio ~ state, data = bw_2018, mean)

state_bw <- mutate_all(state_bw, ~replace(., is.infinite(.), 0))

state_bw <- mutate(state_bw, )
state_bw <- filter(state_bw, state != "district of columbia") 

state_shape <- map_data("state") %>%
  dplyr::rename (state = region) %>%
  left_join(state_bw, by="state")


ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = bw_jail_ratio),
    color = "white",
    size = .1       
  ) +
  coord_map() +
  scale_fill_continuous(low = "Grey", high = "Red") +
  labs(
    title = "Black to White Jail Population in The US",
    x = "",
    y = "",
    fill = "Blacks to Whites in Jail Ratio")


