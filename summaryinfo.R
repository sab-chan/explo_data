#load packages
library(dplyr)
library(tidyverse)

#create dataframe

living_wage_df <- read.csv("livingwage.csv")
living_wage_df <- living_wage_df %>%
  unite("location", city:state, sep= ", ",remove = FALSE)
min_wage_df <- read.csv("minwage.csv")
state_spending_df <- read.csv("statespending.csv")

#summary functioni
summary_info <- list()
summary_info$highest_living_wage_2kids_2workingadult <- living_wage_df %>%
                                                          filter( two_adults_both_working_two_kids_living_wage == max(two_adults_both_working_two_kids_living_wage,na.rm = TRUE))%>% 
                                                          pull(location)
summary_info$lowest_recent_min_wage <- min_wage_df %>% 
                                        na.omit() %>% 
                                        filter(Year == max(Year)) %>% 
                                        filter(State.Minimum.Wage.2020.Dollars == min(State.Minimum.Wage.2020.Dollars)) %>% 
                                        pull(State.Minimum.Wage.2020.Dollars)
          
summary_info$state_highest_recent_min_wage <- min_wage_df %>% 
                                        na.omit() %>% 
                                        filter(Year == max(Year)) %>% 
                                        filter(State.Minimum.Wage.2020.Dollars == max(State.Minimum.Wage.2020.Dollars)) %>% 
                                        pull(State)

summary_info$location_highest_density <- living_wage_df %>%
                                          filter(density== max(density,na.rm = TRUE))%>% 
                                          pull(location) 

summary_info$mean_state_min_wage <- mean(min_wage_df$State.Minimum.Wage.2020.Dollars)
                                                
                                                

  
