library(dplyr)
library(tidyr)

#################
#### PART 1 #####
#################

#1
projects_df <- read.csv("https://canvas.uw.edu/courses/1355751/files/61486325/download", 
                        stringsAsFactors = FALSE)

#2
print(nrow(projects_df))

print(colnames(projects_df))

#3
projects_df_saft <- read.csv("https://canvas.uw.edu/courses/1355751/files/61486325/download", 
                             stringsAsFactors = TRUE)
summary(projects_df)
summary(projects_df_saft)

#4

# I notice that the both the success rate and the categories vary. 
#It would be interesting to explore whether some categories have a higher success rate than others. 


#################
#### PART 2 #####
#################

#1
# dplyr packaged loaded at the top of page

#2
num_nothing_pledged <- nrow(filter(projects_df, pledged == 0)) 
print(num_nothing_pledged)  

#3
percent_successful <- (nrow(filter(projects_df, state == "successful")) / 
  nrow(projects_df))
print(percent_successful)

#4
num_projects_2018 <-
projects_df %>% 
  filter(startsWith(deadline, "2018")) %>%
  nrow()
print(num_projects_2018)

#5
  most_pledged <- 
projects_df %>% 
  filter(pledged == max(pledged)) %>% 
  select(name, category)
  print(most_pledged)

#6
  highest_goal <- 
projects_df %>% 
  filter(goal == max(goal)) %>% 
  select(name, category) %>% 
  arrange(category)
  print(highest_goal)

#7
biggest_failure <- 
  projects_df %>% 
  filter(state == "failed") %>% 
  filter(pledged == max(pledged)) %>% 
    select(name, pledged)
print(biggest_failure)

#8
projects_df %>% 
  mutate( margin = (pledged - goal)) %>% 
  filter(pledged == max(pledged)) %>% 
  select(name, margin) %>% 
  data.frame()


#9
projects_df %>% 
  filter(state != "successful") %>% 
  summarize(
    average = mean(pledged),
    total = sum(pledged)
  ) %>% 
  as.data.frame()

#################
#### PART 3 #####
#################

#1
category_counts <- 
projects_df %>% 
  count(category) %>% 
  rename(count = n) %>% 
  arrange(-count)
print(category_counts)
  
#2
highest_avg_backers <-
projects_df %>% 
  group_by(category) %>% 
  summarize(
    avg_backers = mean(backers, na.rm = TRUE)
  ) %>% 
  filter(avg_backers == max(avg_backers)) %>% 
  select(category)
print(highest_avg_backers)

#3
top_3_categories <-  
projects_df %>% 
  group_by(category) %>% 
  filter(startsWith(deadline, "2018")) %>% 
  summarise(
    total_backers = sum(backers)
  ) %>% 
  arrange(-total_backers) %>% 
  head(n=3) 
print(top_3_categories)

#4
popularity_by_year <- 
projects_df %>% 
  mutate(
    year = substr(launched, 1,4)
  ) %>% 
  group_by(year) %>% 
  summarise(
    total_backers = sum(backers), 
    total_pledged = sum(pledged)
  )
print(popularity_by_year)
  

#5
# Overall, Kickstater became more popular over the years.

#6
most_popular_launch_day <- 
projects_df %>% 
 mutate(
    week_day = weekdays(
      as.Date(launched)
     )
    ) %>% 
  group_by(week_day) %>% 
  count(week_day) %>% 
  as.data.frame() %>% 
  filter(n == max(n)) %>% 
pull(week_day)
print(most_popular_launch_day)

#7
unsuccessful_weekday <-
  projects_df %>% 
    mutate(
      week_days = weekdays(
        as.Date(launched)
      )
    ) %>% 
    group_by(week_days) %>% 
    filter(state != "successful") %>% 
    count(week_days) %>% 
    as.data.frame() %>% 
    pull(n)
  
total_weekday <-
  projects_df %>% 
    mutate(
      week_days = weekdays(
        as.Date(launched)
      )
    ) %>% 
    group_by(week_days) %>% 
    count(week_days) %>% 
    as.data.frame() %>% 
    pull(n)
    
least_successful_launch_day <-
  projects_df %>% 
    mutate(
      week_days = weekdays(
        as.Date(launched)
      )
    ) %>% 
    group_by(week_days) %>% 
    count(week_days) %>% 
    as.data.frame() %>% 
    mutate(
      failure_rate = unsuccessful_weekday/total_weekday
    ) %>% 
    filter(failure_rate == max(failure_rate)) %>% 
    pull(week_days)
  print(least_successful_launch_day)
    
#8

most_successful_launch_day <- 
  projects_df %>% 
    mutate(
      week_days = weekdays(
        as.Date(launched)
      )
    ) %>% 
    group_by(week_days) %>% 
    count(week_days) %>% 
    as.data.frame() %>% 
    mutate(
      failure_rate = unsuccessful_weekday/total_weekday
    ) %>% 
    filter(failure_rate == min(failure_rate)) %>% 
    pull(week_days)
  print(most_successful_launch_day)

#9
avg_year_category_df <-
  projects_df %>% 
    mutate(
      year = substr(deadline, 1,4)
    ) %>% 
  group_by(year, category) %>% 
    summarise(
      pledged_total = sum(pledged),
      backers_total = sum(backers),
      average_pledge_per_backer = mean (
        pledged_total/backers_total
      )
    ) %>% 
    select(year, category, average_pledge_per_backer) %>% 
    filter(year > 2012)
  

#################
#### PART 4 #####
#################


#2

cex_df <- read.csv("data/cex_multiyear.csv" , stringsAsFactors = FALSE)

#View(cex_df)

#3 
#install.packages("tidyr")
cex_long_df <- 
gather(
  cex_df,
  key = year,
    value = value,
    -Item
) %>% 
  mutate(
    year = substr(
      year, 2, 5
    )
  ) 

cex_wide_df <- 
  cex_long_df %>% 
    spread(
      key = Item,
      value = value
    )

#4
#View(avg_year_category_df)
all_spending <- 
left_join(
  #cex_wide_df, 
  avg_year_category_df, 
  cex_wide_df,
  by = "year"
)
View(all_spending)

#5
# Which year(s) had average pledges greater than the average spending on education?

all_spending %>% 
  filter(average_pledge_per_backer 
         == max(average_pledge_per_backer)) %>% 
  filter(
    #average_pledge_per_backer ==
      average_pledge_per_backer > Education
  ) %>% 
  select(year)


