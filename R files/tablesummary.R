# packages used
library(stringr)
library(dplyr)
library(tidyverse)

# bring in datasets
Zillow_df <- read.csv("/Users/tiaesperanzate/final-projects-sab-chan/Data/ZHVI.csv")

statespendingKids_df <- read.csv("/Users/tiaesperanzate/final-projects-sab-chan/Data/State-by-State Spending on Kids.csv")

min_wage_df <- read.csv("/Users/tiaesperanzate/final-projects-sab-chan/Data/Minimum Wage Data.csv")

living_wage_df <- read.csv("/Users/tiaesperanzate/final-projects-sab-chan/Data/livingwage.csv")

# combine and narrow down features

  # state spending on kids dataframe
statespendingKids_df <- unique(statespendingKids_df %>%
  pivot_longer(
    -state) %>%
  rename(State = 'state',
         Year = 'name',
         Public_spending_on_children = 'value'))

statespendingKids_df$Year <- gsub("^.", "", statespendingKids_df$Year)

  # Zillow home value index dataframe
Zillow_df <- unique(Zillow_df %>%
  pivot_longer(
    -X) %>% 
  rename(State = 'name',
         Home_value_index = 'value',
         Year = 'X'))

Zillow_df$Year <- gsub(".{6}$", "", Zillow_df$Year)


  # combine minimum wage dataframe with living wage dataframe
wage_df <- merge(min_wage_df, living_wage_df, by.x = 'State', by.y = 'state')

wage_df <- unique(wage_df %>%
  rename(State_minimum_wage = 'State.Minimum.Wage.2020.Dollars',
         Federal_minimum_wage = 'Federal.Minimum.Wage.2020.Dollars',
         Avg_living_wage = 'one_adult_no_kids_living_wage') %>%
  select(State, Year, State_minimum_wage, Federal_minimum_wage, Avg_living_wage) %>%
  arrange(Year)) %>%
  group_by(Year, State) %>% filter(Avg_living_wage == mean(Avg_living_wage))



# tables:

# calculates the mean/average home value index for each state by year
Zillow_mean_df <- Zillow_df %>%
  group_by(State, Year) %>%
  summarize(Avg_home_value_index = mean(Home_value_index))

# calculates the total state spending on kids for each state by year
total_kids_spending <- statespendingKids_df %>%
  group_by(State, Year) %>%
    summarize(Public_spending_on_children = sum(Public_spending_on_children)) %>%
  arrange(Year)

# calculates the state with the highest minimum wage from every year in the dataset 
highest_each_state <- wage_df %>%
  group_by(Year) %>%
  filter(State_minimum_wage == max(State_minimum_wage))

  # then the lowest minimum wage
lowest_each_state <- wage_df %>%
  group_by(Year) %>%
  filter(State_minimum_wage == min(State_minimum_wage))
