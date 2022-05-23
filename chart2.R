home_index <- read.csv("ZHVI.csv")
living_wage <-read.csv("livingwage.csv")
kids_spending <- read.csv("State-by-State Spending on Kids.csv")

library(dplyr)
library(usmap)

living_wage_by_state <- living_wage %>%
  group_by(state)%>%
  filter(one_adult_no_kids_living_wage == max(one_adult_no_kids_living_wage))
  summarise(state, one_adult_no_kids_living_wage)

living_wage_by_state_map <- plot_usmap(data=living_wage_by_state, values = "one_adult_no_kids_living_wage") +
  labs(title = "One Adult's Living Wage Across the US Cities") +
  scale_fill_continuous(name = "Living Wage ($)", low = "white", high = "green")
