library(tidyverse)
library(usmap)
library(plotly)
library(dplyr)
options(scipen = 999)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num = 6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#
# Load data frame
incarceration_df <- get_data()
globalVariables(names("incarceration_df"))
# Average jail population of female and male
average_female_jail_pop <- mean(incarceration_df$female_jail_pop, na.rm = TRUE)
average_male_jail_pop <- mean(incarceration_df$male_jail_pop, na.rm = TRUE)

# Maximum and minimum of total jail population in Washington
highest_total_in_wa <- incarceration_df %>%
  filter(state == "WA") %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  select(county_name, total_jail_pop)
lowest_total_in_wa <- incarceration_df %>%
  filter(state == "WA") %>%
  filter(total_jail_pop == min(total_jail_pop, na.rm = TRUE)) %>%
  select(county_name, total_jail_pop)
# Average jail population based on races nationally
mean_black_jail_pop <- 
  mean(incarceration_df$black_jail_pop_rate, na.rm = TRUE)
mean_white_jail_pop <- 
  mean(incarceration_df$white_jail_pop_rate, na.rm = TRUE)
mean_latinx_jail_pop <- 
  mean(incarceration_df$latinx_jail_pop_rate, na.rm = TRUE)
mean_aapi_jail_pop <- 
  mean(incarceration_df$aapi_jail_pop_rate, na.rm = TRUE)
mean_native_jail_pop <- 
  mean(incarceration_df$native_jail_pop_rate, na.rm = TRUE)
# Get a summary table
summary_info <- data.frame(mean_black_jail_pop,
                           mean_white_jail_pop,
                           mean_latinx_jail_pop,
                           mean_aapi_jail_pop,
                           mean_native_jail_pop)
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function selects necessary values for plotting jail population of U.S. 
get_year_jail_pop <- function() {
  incarceration_df %>%
    summarize(year, total_jail_pop)
}
# This function plots a bar chart for the jail population
plot_jail_pop_for_us <- function(){
  ggplot(get_year_jail_pop()) + 
    geom_col(mapping = aes(x = year, y = total_jail_pop),
             na.rm = TRUE) + 
    labs(title = "Increase of Jail Population in U.S. (1970-2018)",
         x = "Year",
         y = "Total Jail Population",
         caption = "Data source: Vera Institute")
} 
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# See Canvas
#----------------------------------------------------------------------------#
# This function selects columns that are necessary for plotting jail population of U.S. 
# The stakeholders could get the jail pop by states by passing a vector parameter to this function
get_jail_pop_by_states <- function(states){
  incarceration_df %>%
    filter(state == states) %>%
    group_by(state, year) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
}
# This function plots a grouped line chart for the parameter(a vector of states) that the stakeholder inputs
plot_jail_pop_by_states <- function(states){
  ggplot(get_jail_pop_by_states(states)) + 
    geom_line(aes(year, total_jail_pop, group = state, color = state)) + 
    labs(title = "Increase of Jail Population in U.S. (1970-2018) by States",
         x = "Year",
         y = "Total Jail Population",
         caption = "Data source: Vera Institute")
}
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# See Canvas
#----------------------------------------------------------------------------#
# This function filtered and calculated the average rate of black jail population 
# and white jail population for selected states. 
get_average_jail_pop_rate <- function(states) {
  incarceration_df %>%
    filter(state == states) %>%
    group_by(state, year) %>%
    summarize(black_jail_pop_rate = mean(black_jail_pop_rate, na.rm = TRUE), 
              white_jail_pop_rate = mean(white_jail_pop_rate, na.rm = TRUE)) %>%
    na.omit()
}

# This function plots a grouped bar chart of black jail population rate by year in selected states
plot_average_jail_pop_rate <- function(states){
  ggplot(get_average_jail_pop_rate(states), 
         aes(x = year, y = black_jail_pop_rate, fill = state, na.rm = TRUE)) +
    geom_col(position = "dodge", stat = "identity", na.rm = TRUE) + 
    labs(title = "Average Growth of Black Jail Population Rate in U.S. (1970-2018) by states",
         x = "Year",
         y = "Average Black Jail Population Rate",
         caption = "Data source: Vera Institute")
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# See Canvas
#----------------------------------------------------------------------------#
# This function selects necessary columns for plotting the female jail population on a map
get_jail_map_by_states <- function(states){
  incarceration_df %>% 
    filter(state == states) %>%
    group_by(state, year, fips) %>%
    summarize(black_prison_adm = sum(black_prison_adm, na.rm = TRUE),
              white_prison_adm = sum(white_prison_adm, na.rm = TRUE))
}

# This function plots a choropleth map based on input states
plot_jail_map_by_states <- function(states){
  plot_usmap(regions = "counties", 
             data = get_jail_map_by_states(states), 
             values = "black_prison_adm", 
             include = states) + 
    scale_fill_continuous() +
    labs(title = "Black Prison Admission Count") +
    theme(legend.position = "right")
}

## Load data frame ---- 
