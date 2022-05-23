library(dplyr)
library(tidyverse)
library(ggplot2)

# bring in datasets
min_wage_df <- read.csv("/Users/tiaesperanzate/final-projects-sab-chan/Data/Minimum Wage Data.csv")

living_wage_df <- read.csv("/Users/tiaesperanzate/final-projects-sab-chan/Data/livingwage.csv")

# create dataset to use in chart

testOne_df <- living_wage_df %>%
  rename(State = 'state',
         Living = 'one_adult_no_kids_living_wage') %>%
  select(State, Living) %>%
  group_by(State) %>%
  filter(Living == mean(Living))

testTwo_df <- min_wage_df %>%
  rename(Minimum = 'State.Minimum.Wage.2020.Dollars') %>%
  filter(Year == 2020) %>%
  select(State, Minimum)
  
chart_df <- merge(testOne_df, testTwo_df)
chart_df <- chart_df %>%
  pivot_longer(
    -State
  )

# simplify states to make table easier to read
chart_df[chart_df$State == "Alaska", "State"] <- "AK"
chart_df[chart_df$State == "District of Columbia", "State"] <- "DC"
chart_df[chart_df$State == "Georgia", "State"] <- "GA"
chart_df[chart_df$State == "Hawaii", "State"] <- "HI"
chart_df[chart_df$State == "Idaho", "State"] <- "ID"
chart_df[chart_df$State == "Illinois", "State"] <- "IL"
chart_df[chart_df$State == "Kansas", "State"] <- "KS"
chart_df[chart_df$State == "Maryland", "State"] <- "MD"
chart_df[chart_df$State == "Massachusetts", "State"] <- "MA"
chart_df[chart_df$State == "Michigan", "State"] <- "MI"
chart_df[chart_df$State == "Minnesota", "State"] <- "MN"
chart_df[chart_df$State == "New Jersey", "State"] <- "NJ"
chart_df[chart_df$State == "New Mexico", "State"] <- "NM"
chart_df[chart_df$State == "Oregon", "State"] <- "OR"


# create chart
living_and_min_wage <- ggplot(chart_df, aes(fill = name, y = value, x = State,)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Living Wage vs. Minimum Wage", y = 'Dollars') +
  guides(fill = guide_legend(title = "Type")) +
  scale_fill_manual(values = c("darkgreen", "darkseagreen4"))



