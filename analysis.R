# Overview ----------------------------------------------------------------

#This is one of my first analysis projects done in R that looks at some Covid Data. Nothing too complex here, this is just meant to demonstrate that I can use R!


library("tidyverse")


# Load the *national level* data into a variable. `national`

national<-read.csv("C:\\Users\\samir\\Downloads\\us.csv")


# Load the *state level* data into a variable. `states`

states <- read.csv("C:\\Users\\samir\\Downloads\\us-states.csv")


# Load the *county level* data into a variable. `counties`
# (this is a large dataset, which may take ~30 seconds to load)

counties <- read.csv("C:\\Users\\samir\\Downloads\\us-counties.csv")



# How many observations (rows) are in each dataset?
# Create `obs_national`, `obs_states`, `obs_counties`

obs_national <- nrow(national)
obs_national #652

obs_states <- nrow(states)
obs_states #33606

obs_counties <- nrow(counties)
obs_counties #1878347

# Reflection: What does each row represent in each dataset?

#Each Row represents a date, location, and number of covid cases for that day


# How many features (columns) are there in each dataset?

num_features_national #3
num_features_states #5
num_features_counties #6

# Create `num_features_national`, `num_features_states`, `num_features_counties`
num_features_national <- ncol(national)
 

num_features_states <- ncol(states)
 

num_features_counties <- ncol(counties)
 


# Exploratory analysis ----------------------------------------------------

# How many total cases have there been in the U.S. by the most recent date
# in the dataset? `total_us_cases`

total_us_cases <- max(national$cases)
total_us_cases
#46135516

# How many total deaths have there been in the U.S. by the most recent date
# in the dataset? `total_us_deaths`

total_us_deaths <- max(national$deaths)
total_us_deaths
#748197

# Which state has had the highest number of cases?
# `state_highest_cases`

max(states$cases)
state_highest_cases <- filter(states, cases == 4938058)
state_highest_cases #California

# What is the highest number of cases in a state?
# `num_highest_state`

num_highest_state <- max(states$cases) #4938058

# Which state has the highest ratio of deaths to cases (deaths/cases), as of the
# most recent date? `state_highest_ratio`
# (hint: you may need to create a new column in order to do this!)

states <- mutate(states,  death_ratio = (deaths / cases))
state_highest_ratio_number <- max(states$death_ratio)
state_highest_ratio <- filter(states, death_ratio == state_highest_ratio_number)
#Washington

# Which state has had the lowest number of cases *as of the most recent date*?
# (hint, this is a little trickier to calculate than the maximum because
# of the meaning of the row). `state_lowest_cases`
newest_date <- max(states$date)
updated_states_data <- states[states$date == newest_date, ]
updated_states_data
state_lowest_cases_number <- min(updated_states_data$cases)
state_lowest_cases <- filter(updated_states_data, cases == state_lowest_cases_number)
state_lowest_cases #American Samoa

# Reflection: What did you learn about the dataset when you calculated
# the state with the lowest cases (and what does that tell you about
# testing your assumptions in a dataset)?

#When calculating the state with the lowest number of cases, I learned
#that the result was an island, I also learned that the dataset contained
#overseas territories, my assumption originially would've been Alaska
#for the lowest number of cases due to the low population density
#but American Samoa makes sense to me too cause of the geographic isolation.

# Which county has had the highest number of cases?
# `county_highest_cases`

county_highest_cases_num <- max(counties$cases)
county_highest_cases <- filter(counties, cases == county_highest_cases_num)
county_highest_cases #Los Angeles County

# What is the highest number of cases that have happened in a single county?
# `num_highest_cases_county`

num_highest_cases_county <- max(counties$cases)
num_highest_cases_county #1495718


counties <- unite(counties, "location", county:state, sep = ", " , na.rm = TRUE, remove = FALSE)
counties

# What is the name of the location (county, state) with the highest number
# of deaths? `location_most_deaths`

location_most_deaths_num <- max(counties$deaths)
location_most_deaths<- filter(counties, deaths == location_most_deaths_num)
location_most_deaths #New York City, NY


national$new_cases <- national$cases - lag(national$cases, 1)



national$new_deaths <- national$deaths - lag(national$deaths, 1)


# What was the date when the most new cases occured?
# `date_most_cases`

date_most_cases <- national[national$new_cases == max(national$new_cases, na.rm = TRUE), 1]
date_most_cases #2021-09-07

# What was the date when the most new deaths occured?
# `date_most_deaths`

date_most_deaths <- national[national$new_deaths == max(national$new_deaths, na.rm = TRUE), 1]
date_most_deaths #"2021-02-12"
# How many people died on the date when the most deaths occured? `most_deaths`

most_deaths <- national[national$date == date_most_deaths, 4]
most_deaths #99627

# Grouped analysis --------------------------------------------------------



highest_in_each_state <- counties %>%
  group_by(state) %>%
  filter(date == max(date)) %>%
  filter(cases == max(cases))%>%
  pull(location) #Jefferson, Alabama




    lowest_in_each_state <- counties %>%
    group_by(state)%>%
    filter(date == max(date)) %>%
    filter(deaths == min(deaths))%>%
    pull(location)  
    lowest_in_each_state
    
    
    state_by_day <- states %>%
    group_by(dates)%>%
    state_total <- sum(cases)%>%
    
    summarize(state_total - sum(cases))


      county_by_day <- counties %>%
      group_by(dates)%>%
      county_total <- sum(cases)%>%
      summarize(county_total - sum(counties$cases))



    totals_by_day <- merge(county_total, state_total)


all_totals <- national  %>%
  mutate(totals_by_day)
    
# How many rows are there where the state total *doesn't equal* the national
# cases reported? `num_state_diff`

  num_state_diff <-  all_totals %>%
    filter(state_total != national$cases)%>%
    nrow()

# How many rows are there where the county total *doesn't equal* the national
# cases reported? `num_county_diff`

  num_county_diff <-  all_totals %>%
    filter(county_total != national$cases)%>%
    nrow()


# Oh no! An inconsistency -- let's dig further into this. Let's see if we can
# find out *where* this inconsistency lies. Let's take the county level data,
# and add up all of the cases to the state level on each day (e.g.,
# aggregating to the state level). Store this dataframe with three columns
# (state, date, county_totals) in the variable `sum_county_to_state`.


sum_county_to_state <- counties %>%
  group_by(date, state) %>%
  summarize(county_totals = sum(cases), groups = "drop")

# Then, let's join together the `sum_county_to_state` dataframe with the
# `states` dataframe into the variable `joined_states`.
 
joined_states <- left_join(sum_county_to_state, states)


has_discrepancy <- joined_states %>%
  filter(county_totals != cases)



states_highest_difference <- has_discrepancy %>%
  mutate(state_difference = abs(county_totals - cases)) %>%
  filter(state_difference == max(state_difference))%>%
  pull(state)
states_highest_difference


