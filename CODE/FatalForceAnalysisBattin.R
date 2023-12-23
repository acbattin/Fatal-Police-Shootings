#' title: "Fatal U.S. Officer Involved Shootings using Fatal Force database"
#' author: "Andie Battin"
#' date: "2023-12-04"
#' output: pdf_document

#' Section 0. Setup: Set wd to same location on PC where data files are stored 
setwd('C:/Users/andie/Documents/R')
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(flexdashboard)
library(grid)
library(lattice)
library(hexbin)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(ggthemes)
library(magrittr)
library(readxl)
source("hw.r")
#' where the data files are saved & install and load required packages before 
#' loading the most recent version of victim-level data from the Washington Post
#' database and the most recent version of the agency-level data
WP_victim_data <- read.csv("C://Users/andersonbattin/R/fatal-police-shootings-data.csv")
summary(WP_victim_data) # summarize the data, evaluate variable types
head(WP_victim_data) # explore and evaluate the data entries
colnames(WP_victim_data) # explore the variables in the data
min(WP_victim_data$date) # oldest date in the dataset
max(WP_victim_data$date) # most recent date in the dataset
table(WP_victim_data$state)
table(WP_victim_data$gender)
table(WP_victim_data$race)
table(WP_victim_data$date)

#' use the column names to generate a data frame with the variables of interest
WP_victim_data <- select(WP_victim_data, date, threat_type, flee_status, 
                         armed_with, city, county, state, latitude, longitude, 
                         name, age, gender, race, was_mental_illness_related, 
                         body_camera, agency_ids)
head(WP_victim_data)
#' check for missing values the only variables with missing data are longitude, 
#' latitude, and age
sapply(WP_victim_data, function(x) sum(is.na(x)))
view(WP_victim_data)
table(WP_victim_data$age, useNA = "always")

#' Explore proportion of armed versus unarmed by race
table(WP_victim_data$armed_with, useNA = "always")
prop.table(table(WP_victim_data$race,WP_victim_data$armed_with=="gun"))
prop.table(table(WP_victim_data$race,WP_victim_data$armed_with=="unarmed"))
# Plot killings by race
bar1 <- ggplot(gen_df, aes(race))+
  geom_bar(aes(y=(after_stat(count))/sum(after_stat(count))), fill="forestgreen")+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "Total Fatal Police Shootings in the United States by Race since January 1, 2015", 
       subtitle = "Since January 1, 2015 as of December 1, 2023", 
       caption = "Source: Fatal Force Database v2, Washington Post (updated 12/1/2023)",
       x="Race", y="Percent (%)") +
  theme(axis.ticks = element_blank())
bar1

#' INTERACTIVE PLOT - fatal shootings by state, grouped by racial breakdown
#' generate a new data frame
WP_state_race <- WP_victim_data %>%
  filter(!race == "") %>%
  group_by(state, race) %>%
  summarise(fatalities = n())
#head(WP_state_race)
summary(WP_state_race)
WP_spread_state_race <- spread(WP_state_race, race, fatalities)
#WP_spread_state_race
WP_spread_state_race$TOTAL <- rowSums(WP_spread_state_race[,-1],na.rm = TRUE)
#WP_spread_state_race
map_hov <- WP_spread_state_race
map_hov$HOVER <- with(map_hov, paste("Asian", A, '<br>',
                                     "Black", B, '<br>',
                                     "Black & Hispanic", `B;H`, '<br>',
                                     "Hispanic", H, '<br>',
                                     "Native American", N, '<br>',
                                     "Other", O, '<br>',
                                     "White", W, '<br>',
                                     "Total Fatal Police Shootings:", `TOTAL`))
plot_1 <- list(
  scope = 'usa',
  projection = list(type='albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
plot_ly(WP_spread_state_race, z=WP_spread_state_race$TOTAL, text=map_hov$HOVER, 
        locations = map_hov$state, type = 'choropleth',
        locationmode='USA-states', color = WP_spread_state_race$TOTAL, 
        colors = 'Blues',
        colorbar=list(title="Fatality Counts")) %>%
  layout(title = 'Total Fatal Police Shootings since January 1, 2015\nSource: Fatal Force Database v2, Washington Post (12/1/2023)', geo=plot_1)

#' Box plots for race and age
victim_no_na <- filter(WP_victim_data, age > 0)
nrow(victim_no_na)
nrow(WP_victim_data)
victim_no_na %>%
  filter(!is.na(age) & race != '') %>%
  ggplot(aes(x=reorder(race, age), y=age)) +
  geom_boxplot(aes(color=race)) +
  labs(x="Racial Identity of Victim", y="Age of Victim",
       title = "Police Shooting Victims Age Distribution across Racial Groups",
       subtitle = "Since January 1, 2015 as of December 1, 2023",
       caption = "Source: Fatal Force Database v2, Washington Post (updated 12/1/2023)")+
  scale_x_discrete(labels=c('Black and Hispanic',
                            'Native American',
                            'Black',
                            'Hispanic',
                            'Other',
                            'Asian',
                            'White')) +
  coord_flip() +
  theme(legend.position = "none", axis.ticks = element_blank())


#' Gender and race
WP_gender_race <- WP_victim_data %>%
  filter(race!= "") %>%
  filter(gender!= "") %>%
  group_by(race, gender) %>%
  summarise(fatalities = n())
# side by side bar plot
clean_gender_race <- WP_gender_race[-5,]
ggplot(clean_gender_race, aes(x=race, y=fatalities, fill=gender)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75) +
  labs(x= 'Racial Identity of Victim', y= 'Number of Fatal Police Shooting Victims') +
  ggtitle('Victims of Police Fatal Shootings in the U.S. (1/1/2015 to 12/1/2023)
          Source: Fatal Encounters Database v2, Washington Post') +
  scale_x_discrete(labels=c('Asian',
                            'Black',
                            'Hispanic',
                            'Native American',
                            'Other',
                            'White')) +
  theme_few() +hw + 
  scale_fill_manual(values = c('palevioletred','dodgerblue3'))# female = pink, male = dark blue
# stacked bar plot
ggplot(clean_gender_race, aes(x=race, y=fatalities, fill=gender)) +
  geom_bar(stat = "identity") +
  labs(x= 'Racial Identity of Victim', y= 'Number of Fatal Police Shooting Victims') +
  ggtitle('Victims of Police Fatal Shootings in the U.S. (1/1/2015 to 12/1/2023)
          Source: Fatal Encounters Database v2, Washington Post') +
  scale_x_discrete(labels=c('Asian',
                            'Black',
                            'Hispanic',
                            'Native American',
                            'Other',
                            'White')) +
  theme_few() +hw+
  scale_fill_manual(values = c('palevioletred','dodgerblue3'))


# total killings by year
gen_df$year <- format(as.Date(gen_df$date, format = "%Y-%m-%d"), "%Y")
by_year <- ggplot(gen_df, aes(x = year,group = year)) +
  geom_bar(fill = 'forestgreen') + 
  geom_text(stat = "count", aes(label=after_stat(count)), vjust = -1) +
  labs(x="Year", y="Total Number of Police Shootings",
       title = "Total Number of Fatal Police Shootings in the US by Year",
       subtitle = "Since January 1, 2015 as of December 1, 2023",
       caption = "Source: Fatal Force Database v2, Washington Post (updated 12/1/2023)") +
  theme(axis.ticks = element_blank())
by_year
# total killings by month (since first entry)
WP_victim_data %>%
  mutate(monthly_annual = format(as.Date(date, "%Y-%m-%d"), "%Y/%m")) %>%
  group_by(monthly_annual) %>%
  summarise(net = n()) %>%
  ggplot(aes(x=monthly_annual, y=net, group = 1)) +
  geom_line(color = 'forestgreen')+
  geom_point(color = 'forestgreen') +
  labs(x="Month and Year of Incident", y="Number of fatal police shooting victims",
       title = "Total Number of Fatal Police Shootings in the U.S. by month and year",
       subtitle = "Since January 1, 2015 as of December 1, 2023",
       caption = "Source: Fatal Force Database v2, Washington Post (updated 12/1/2023)") +
  scale_x_discrete(breaks= levels(as.factor(format(as.Date(WP_victim_data$date, '%Y-%m-%d'), "%Y/%m")))[c(T, rep(F, 11))]) +
  theme(axis.ticks = element_blank())

# total police killings by state
ggplot(gen_df, aes(x = state)) +
  geom_bar(fill = 'forestgreen', width = 0.5) +
  labs(x="State", y="Total Number of Police Shootings",
       title = "Total Number of Fatal Police Shootings in the US by State",
       subtitle = "Since January 1, 2015 as of December 1, 2023",
       caption = "Source: Fatal Force Database v2, Washington Post (updated 12/1/2023)") +
  theme(axis.ticks = element_blank())

# threat level by age group
# age group variable for victim_no_na data 
victim_no_na$age_group <-
  cut(victim_no_na$age,
      breaks = c(-Inf, 1, 18, 22, 30, 40, 50, 60, 65, Inf),
      labels = c("Age Unknown", "Under 18", "18-21", "22-29", "30-39", "40-49",
                 "50-59", "60-64", "Above 65"), right = FALSE)
# clear missing threat values
victim_final <- victim_no_na[-c(3357,5761,7674,7733,7736,7860,7892,7906,7914,7922,7933,7939,7950,8039,8040,8092,8108,8118,8119,8125,8137,8180,8205,8206,8213,8219,8222,8250,8251,8252,8312,8313,8330,8338,8417,8449,8467,8041,8093,8109,8120,8126,8138,8181,8207,8214,8220,8223,8253,8314,8331,8339,8420,8466,8496,8506),]
# Plot the threat level by age group
ggplot(victim_final, aes(age_group, group = threat_type), na.rm = TRUE) +
  geom_bar(aes(y = after_stat(prop), fill = factor(..x..)), stat = "count") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percentage") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_grid(~threat_type) +
  labs(x="Age Group", y="Percent of Police Shootings",
       title = "Total Number of Fatal Police Shootings in the US by Age Group and Threat Type",
       subtitle = "Since January 1, 2015 as of December 1, 2023",
       caption = "Source: Fatal Force Database v2, Washington Post (updated 12/1/2023)") +
  scale_fill_discrete(name = "Age Group", labels = c("Under 18", "18-21", "22-29", "30-39", "40-49", "50-59","60-64","Above 65")) +
  theme(axis.ticks = element_blank())


# Explore body cam by race
WP_bodycam_race <- WP_victim_data %>%
  filter(!race == "") %>%
  group_by(body_camera, race) %>%
  summarise(fatalities = n())
# Plot body cam usage by police by race of victim
ggplot(victim_final, aes(body_camera, group = race)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Body Camera Presence", y="Percent of Fatal Police Shootings",
       title = "Fatal Police Shootings in the US by Race and Body Camera Status",
       subtitle = "Since January 1, 2015 as of December 1, 2023",
       caption = "Source: Fatal Force Database v2, Washington Post (updated 12/1/2023)") +
  facet_grid(~race) + theme(axis.ticks = element_blank()) +
  scale_fill_manual(name = "Body Camera Presence", labels = c("Not Present", "Present"),values = c('firebrick4','palegreen4'))


#'10 States with the most fatal police shootings (aggregate total) 
state_df <- WP_victim_data %>% group_by(state) %>% summarise(n=n()) %>%
  arrange(desc(n)) %>% top_n(10) %>%
  mutate(state=factor(state, levels = rev(unique(state))))
ggplot(state_df, aes(x=n, y=state))+
  geom_bar(stat="identity", aes(fill=n))+
  geom_text(aes(x=16, y=state, label=''), color="white", size=5)+
  labs(y=NULL, x="Number of Fatal Police Shootings", title = "Top 10 States with the Highest Count of Fatal Police Shootings in the U.S.",
       subtitle = "Since January 1, 2015 as of December 1, 2023",
       caption = "Source: Fatal Force Database v2, Washington Post (updated 12/1/2023)")+
  theme_minimal(base_size = 12)+
  theme(axis.text.y = element_blank())+
  theme(legend.position = "none")+
  scale_x_continuous(expand = c(0,0)) + theme_gray()


#' 20 States
state_df <- WP_victim_data %>% group_by(state) %>% summarise(n=n()) %>%
  arrange(desc(n)) %>% top_n(20) %>%
  mutate(state=factor(state, levels = rev(unique(state))))
ggplot(state_df, aes(x=n, y=state))+
  geom_bar(stat="identity", aes(fill=n))+
  geom_text(aes(x=16, y=state, label=''), color="white", size=5)+
  labs(y=NULL, x="Number of Fatal Police Shootings", title = "Top 20 States with the Highest Count of Fatal Police Shootings in the U.S.",
       subtitle = "Since January 1, 2015 as of December 1, 2023",
       caption = "Source: Fatal Force Database v2, Washington Post (updated 12/1/2023)")+
  theme_minimal(base_size = 12)+
  theme(axis.text.y = element_blank())+
  theme(legend.position = "none")+
  scale_x_continuous(expand = c(0,0)) + theme_gray()

#' determine the top 3 most common "armed_with" factors in the dataset
table(WP_victim_data$armed_with)
top_armed_with <- WP_victim_data %>%
  group_by(armed_with) %>%
  summarise(armed = n()) %>%
  arrange(desc(armed)) %>%
  head(3)
#' Explore relationship between race and armed
race_armed <- WP_victim_data %>%
  filter(race != '') %>%
  mutate(arm_race = ifelse(armed_with %in% c('gun', 'knife', 'unarmed', 'vehicle', 'replica'), as.character(armed_with), 'Other')) %>%
  group_by(race, arm_race) %>%
  summarise(fatalities = n())

spread_race_armed <- race_armed %>%
  spread(arm_race, fatalities)
spread_race_armed[is.na(spread_race_armed)] <- 0
print("% Distribution of Fatalities by Victim's Reported Race and Weapon Reported as being Armed with")
armed_race_table <- cbind(as.data.frame(spread_race_armed[,1]), as.data.frame(round(spread_race_armed[,-1]/rowSums(spread_race_armed[,-1])*100,2)))
armed_race_table
levels(armed_race_table$race) <- c("Asian", "Black", "Black and Hispanic", "Hispanic", "Native American", "Other", "White")
#print(armed_race_table)

#' Stacked bar chart (group by race and armed_with)
ggplot(race_armed, aes(x=race, y=fatalities, fill=arm_race))+
  geom_bar(stat = "identity", position = "dodge") +
  #scale_y_discrete(labels = scales::percent) +
  labs(x= 'Racial Identity of Victim', y= 'Number of Fatal Police Shooting Victims') +
  ggtitle('Victims of Police Fatal Shootings in the U.S. (1/1/2015 to 12/1/2023)
          Source: Fatal Encounters Database v2, Washington Post (updated 12/1/2023)') +
  scale_x_discrete(labels=c('Asian',
                            'Black',
                            'Black and Hispanic',
                            'Hispanic',
                            'Native American',
                            'Other',
                            'White')) +
  theme_few() +hw + scale_fill_discrete(name = "Armed Status", labels = c("Gun", "Knife", "Other", "Replica", "Unarmed", "Vehicle"))

ggplot(race_armed, aes(x=race, y=fatalities, fill=arm_race))+
  geom_bar(stat = "identity") +
  #scale_y_discrete(labels = scales::percent) +
  labs(x= 'Racial Identity of Victim', y= 'Number of Fatal Police Shooting Victims') +
  ggtitle('Victims of Police Fatal Shootings in the U.S. (1/1/2015 to 12/1/2023)
          Source: Fatal Encounters Database v2, Washington Post (updated 12/1/2023)') +
  scale_x_discrete(labels=c('Asian',
                            'Black',
                            'Black and Hispanic',
                            'Hispanic',
                            'Native American',
                            'Other',
                            'White')) +
  theme_few() +hw + scale_fill_discrete(name = "Armed Status", labels = c("Gun", "Knife", "Other", "Replica", "Unarmed", "Vehicle"))

#' Explore flee status
flee_or_not <- WP_victim_data %>%
  filter(race != '') %>%
  group_by(race, flee_status) %>%
  summarise(fatalities=n()) %>%
  spread(flee_status, fatalities)

flee_or_not[is.na(flee_or_not)] <- 0
flee_race_table <- cbind(as.data.frame(flee_or_not[,1]), as.data.frame(round(flee_or_not[,-1]/rowSums(flee_or_not[,-1])*100,2)))
#flee_race_table
levels(flee_race_table$race) <- c("Asian", "Black", "Black and Hispanic", "Hispanic", "Native American", "Other", "White")
#print(flee_race_table)

#'Read in the population data file
Census_population_data <- read_excel('state_population_data.xlsx')
head(Census_population_data)
##summary(Census_population_data)

#'Change the WP_victim data state abbreviations to indicate DC
index <- match(WP_victim_data$state, state.abb)
WP_victim_data$state_full <- ifelse(is.na(index), "District of Columbia", state.name[index])
#head(WP_victim_data)
# compute the number of total killings in terms of 100,000 people
pop_killings <- WP_victim_data %>%
  group_by(state_full, state) %>%
  summarise(count= n()) %>%
  merge(Census_population_data, by="state_full") %>%
  mutate(
    adjusted_killings = round(count/(tot_pop/100000), digits = 2),
    hover = paste(state_full, '<br>', adjusted_killings, 'per 100,000 people',
                  '<br>', count, 'fatal U.S. police shootings since January 2015')
  )
# state map for population adjusted police killings
region_graph <- list(scope = 'usa')
plot_ly(pop_killings, z=pop_killings$adjusted_killings, text = pop_killings$hover,
        locations = pop_killings$state, type = 'choropleth',
        locationmode='USA-states', color = pop_killings$adjusted_killings, colors = 'Blues',
        colorbar=list(title="Fatal Police Shootings in\n the U.S. per 100,000 people")) %>%
  layout(title='Total Fatal Police Shootings per 100,000 people since January 1, 2015\nSource: Fatal Force Database v2, Washington Post (12/1/2023) and U.S. Census Data', geo=plot_1)
#summary(pop_killings)


#' Predict future police fatal shootings, forecast crime for the next month / end of year
#install.packages('forecast')
library(forecast) # load the forecast package
shooting_tots <- WP_victim_data %>%
  mutate(monthly_annual = format(as.Date(date, '%Y-%m-%d'), "%Y/%m")) %>%
  group_by(monthly_annual) %>%
  summarise(net = n()) %>%
  select(net)
# ARIMA Model and plot one month forecast
WP_timeseries <- ts(shooting_tots)
model1 <- auto.arima(WP_timeseries, stepwise = FALSE, approximation = FALSE)
predict <- model1 %>% forecast(level=c(95), h=1)
print(predict %>%
        autoplot() +
        labs(x="Months Since Data Collection Began (Since January 2015)", y="Number of fatal police shooting victims",
             title = "Predicted Monthly Number of Fatal Police Shootings in the U.S. for January, 2024",
             subtitle = "Since January 1, 2015 as of December 1, 2023",
             caption = "Source: Fatal Force Database v2, Washington Post (updated 12/1/2023)")+
        theme_grey())+theme(axis.ticks = element_blank())
print(predict)

#' Poisson fit Analysis from Andrew P. Wheeler publication regarding their package ptools()
#install.packages("ptools")
library(ptools)
#install.packages('rstanarm')
library(rstanarm)
model_data <- read.csv("C://Users/andie/OneDrive/Desktop/R/fatal-police-shootings-data.csv", stringsAsFactors = F)
summary(model_data)
#sapply(model_data, function(x) sum(is.na(x)))
model_data$year <- as.integer(substr(model_data$date, 1,4))
agg_year <- table(model_data$year)
#date_mani <- cbind.data.frame(date=seq.Date(from = ymd("2015-01-01"), to = ymd("2023-12-01"), by = "day"))
agg_year <- table(model_data$year) [1:9]
print(agg_year)
#mean(agg_year[1:9])
#var(agg_year[1:9])
mean(agg_year)
var(agg_year)
model_data$date_val <- as.Date(model_data$date)
date_range <- paste0(seq(as.Date('2015-01-01'), max(model_data$date_val), by='days'))
day_counts <- as.data.frame(table(factor(model_data$date, levels=date_range)))
head(day_counts)
Poisson_fit <- check_pois(day_counts$Freq, 0, 10, mean(day_counts$Freq))
print(Poisson_fit)
chisq_stat <- sum((Poisson_fit$Freq - Poisson_fit$PoisF)^2/Poisson_fit$PoisF)
chi_df <- length(Poisson_fit$Freq) - 2
dchisq(chisq_stat, chi_df)