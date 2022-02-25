library(dplyr)

inmate_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#total number of inmates in the US
total_inamtes_2018 <- inmate_data %>%
  filter(year == 2018) %>% 
  summarise(sum(total_jail_pop, na.rm = TRUE))

#Which state has the most inmates?
totals_by_county <- inmate_data %>%
  filter(year == 2018) %>%
  select(state, total_jail_pop)
state_totals <- aggregate(total_jail_pop ~ state, data = totals_by_county, sum) 
highest_state <- state_totals %>%
  filter(total_jail_pop == max(total_jail_pop)) %>%
  select(state)

# What is the black incarceration rate vs the total incarceration rate?
black_pop <- inmate_data %>%
  filter(year == 2018) %>%
  select(state, county_name, total_pop, 
         black_pop_15to64, total_jail_pop, black_jail_pop) %>%
  mutate(total_prison_ratio = total_jail_pop / total_pop) %>%
  mutate(black_prison_ratio = black_jail_pop / black_pop_15to64) %>%
  summarise(mean(total_prison_ratio, na.rm = TRUE), mean(black_prison_ratio, na.rm = TRUE))
  
# Highest incarceration rate in Washington?
washington <- inmate_data %>%
  filter(year == 2018) %>%
  filter(state == "WA") %>%
  select(county_name, total_pop, total_jail_pop) %>%
  mutate(incarceration_rate = total_jail_pop / total_pop) %>%
  filter(incarceration_rate == max(incarceration_rate)) %>%
  select(county_name)

# How much do the King County inmate totals change each year?
king_county <- inmate_data %>%
  filter(state == "WA") %>%
  filter(county_name == "King County") %>%
  select(year, total_jail_pop) %>%
  mutate(change = total_jail_pop - lag(total_jail_pop)) %>%
  select(year,change)

# Chart over time
library(ggplot2)
kcp_data <- inmate_data %>%
  filter(state == "WA") %>%
  select(year, county_name,total_jail_pop, total_pop) %>%
  filter(total_pop >= 100000)
  


wash_county_plot <- ggplot(data = kcp_data, aes(x = year, y = total_jail_pop,
                                                 color = county_name)) +
  geom_line()+
  geom_point()+
  labs(
    title = "Washington Countys Jail Population",
    x = "Year",
    y = "Total Jail Population", 
    color = "County Name"
  )
wash_county_plot
# Whats the relationship between incarceration rate and population in Kitsap
# County?
bw_ratio_data <- inmate_data %>%
  filter(year == 2018) %>%
  mutate(bw_ratio = black_pop_15to64 / white_pop_15to64) %>%
  mutate(bw_jail_ratio = black_jail_pop / white_jail_pop) %>%
  filter(bw_ratio <= 2) %>%
  filter(bw_jail_ratio <= 5)


bw_urbanicity_plot <- ggplot(data = bw_ratio_data) +
  geom_point(
    mapping = aes(x = bw_ratio, y = bw_jail_ratio, color = urbanicity),
    alpha = .6
  ) +

  labs(
    title = "Black to White Population vs Jail Population",
    x = "Black to White Population",
    y = "Black to White Jail Population", 
    color = "Urbanicity"
  )
bw_urbanicity_plot

#map
install.packages("usdata")
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
state_shape

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


getwd()
