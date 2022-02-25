inmate_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(dplyr)
library(ggplot2)
kcp_data <- inmate_data %>%
  filter(state == "WA") %>%
  select(year, county_name,total_jail_pop, total_pop) %>%
  filter(total_pop >= 100000)



ggplot(data = kcp_data, aes(x = year, y = total_jail_pop,
                                                color = county_name)) +
  geom_line()+
  geom_point()+
  labs(
    title = "Washington Countys Jail Population",
    x = "Year",
    y = "Total Jail Population", 
    color = "County Name"
  )


