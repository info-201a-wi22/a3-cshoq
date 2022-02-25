inmate_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(dplyr)
library(ggplot2)




bw_ratio_data <- inmate_data %>%
  filter(year == 2018) %>%
  mutate(bw_ratio = black_pop_15to64 / white_pop_15to64) %>%
  mutate(bw_jail_ratio = black_jail_pop / white_jail_pop) %>%
  filter(bw_ratio <= 2) %>%
  filter(bw_jail_ratio <= 5)


ggplot(data = bw_ratio_data) +
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

