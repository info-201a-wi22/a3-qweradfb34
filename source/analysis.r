incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
incarceration_trends_jail_jurisdiction <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")
# what is total jain popultion over time
# female vs male
# % female in jail in each state

library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)
library(scales)
library(maps)
library(leaflet)
library(tidyverse)
library(usmap)

#summary table
#total population in jail every year
total <- incarceration_trends_jail_jurisdiction %>%
  group_by(year) %>%
  summarise(total = sum(total_jail_pop, na.rm = TRUE))
#highest total population in jail
high_total <- max(total$total)
    # highest  total population in jail year
high_total_year <- total %>%
  filter(total == max(total)) %>%
  pull(year)

#lowest total population in jail
low_total <- min(total$total)
#lowest  total population in jail year
low_total_year <- total %>%
  filter(total == min(total)) %>%
  pull(year)
#female population in jail every year
females <- incarceration_trends_jail_jurisdiction %>%
  group_by(year) %>%
  summarise(female = sum(female_jail_pop, na.rm = TRUE))
#highest female population in jail
high_total_female <- max(females$female)
# highest  female population in jail year
high_female_year <- females %>%
  filter(female == max(female, na.rm=TRUE)) %>%
  pull(year)

#lowest female population in jail
low_female <- min(females$female)
#lowest  female population in jail year
low_female_year <- females %>%
  filter(female == min(female)) %>%
  pull(year)
#male population in jail every year
males <- incarceration_trends_jail_jurisdiction %>%
  group_by(year) %>%
  summarise(male = sum(male_jail_pop, na.rm = TRUE))
#highest male population in jail
high_total_male <- max(males$male)
# highest  male population in jail year
high_male_year <- males %>%
  filter(male == max(male, na.rm=TRUE)) %>%
  pull(year)

#lowest male population in jail
low_male <- min(males$male)
#lowest male population in jail year
low_male_year <- males %>%
  filter(male == min(male)) %>%
  pull(year)
#jail admin in jail every year
variables <- incarceration_trends_jail_jurisdiction %>%
  group_by(year) %>%
  summarise(total_adm = sum(total_jail_adm, na.rm = TRUE))
#highest male population in jail
high_total_adm <- max(variables$total_adm)
# highest  male population in jail year
high_adm_year <- variables %>%
  filter(total_adm == max(total_adm, na.rm=TRUE)) %>%
  pull(year)

#lowest male population in jail
low_adm <- min(variables$total_adm)
#lowest male population in jail year
low_male_adm <- variables %>%
  filter(total_adm == min(total_adm)) %>%
  pull(year)
#number of population in jail in each state in year 1970
maps <- incarceration_trends%>%
  filter(year == "1970") %>%
  group_by(state) %>%
  summarize(state_pop = sum(total_pop))

total_population <- sum(maps$state_pop)

maps <- maps %>%
  mutate(percent = state_pop/total_population *100)
#state that has highest percent in 1970
high_state <- maps %>%
  filter(percent == max(percent, na.rm=TRUE)) %>%
  pull(state)
#highest state percent 
high_state_pop <- max(maps$percent)
#state that has lowest percent
low_state <- maps %>%
  filter(percent == min(percent,na.rm=TRUE)) %>%
  pull(state)
#lowest state percent 
low_state_pop <- min(maps$percent)
# trend over time chart of populations of genders in jail

df1 <- merge(total,females, sort = TRUE)
summarys <- merge(df1, males, sort = TRUE)

line_chart <- ggplot(summarys, aes(x=year, y= total)) +
  geom_line(aes(y=total, color = "total")) +
  geom_line(aes(y=male, color = "male")) +
  geom_line(aes(y=female, color = "female")) +
  xlab("number of people in jail") +
  ylab("year") +
  ggtitle("total population in jail based on gender")

#variable comparasion chart of jail population vs jail adm

df2 <- merge(total,variables, sort = TRUE)

comparasion <- ggplot(df2, aes(x=total, y=total_adm)) + 
  geom_point() +
  xlab("total jail population") +
  ylab("total jail admin") +
  ggtitle("total population in jail vs total admin in jail")
  

#map
map_chart <- plot_usmap(data = maps, values = "percent", color = "blue") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "percent of total population in jail of US(1970)", label = scales::comma
  ) + labs(title = "percentage of jail population in US") + theme(legend.position = "right")