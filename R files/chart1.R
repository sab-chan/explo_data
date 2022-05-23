home_index <- read.csv("/Users/tiaesperanzate/final-projects-sab-chan/Data/ZHVI.csv")
living_wage <-read.csv("/Users/tiaesperanzate/final-projects-sab-chan/Data/livingwage.csv")
kids_spending <- read.csv("/Users/tiaesperanzate/final-projects-sab-chan/Data/State-by-State Spending on Kids.csv")

library(dplyr)
library(ggplot2)

grouped_states_spending <- kids_spending %>%
  select(state, X2010) %>%
  group_by(state) %>%
  summarise("kids_spending_2010" = sum(X2010))

grouped_states_pop <- living_wage %>%
  select(state, population_2010, ) %>%
  group_by(state) %>%
  summarise("state_populations_2010" = sum(population_2010))

kids_spending_and_living_wage <- merge(grouped_states_pop, grouped_states_spending, by = "state", all.x = TRUE)

pop_vs_kids_spending_2010 <- kids_spending_and_living_wage %>%
  select(state, kids_spending_2010, state_populations_2010)%>%
  arrange(-state_populations_2010)%>%
  filter(state_populations_2010 > 2500000 | state_populations_2010 < 500000)


pop_and_kid_spending_2010 <- ggplot(pop_vs_kids_spending_2010, aes(x=kids_spending_2010, y = state_populations_2010, format(scientific = FALSE))) +
  geom_point(aes(col = state)) + 
  ylab("State Population") +
  xlab("Spending on Kids") +
  ggtitle("Populations and Kids Spending in 2010")
  