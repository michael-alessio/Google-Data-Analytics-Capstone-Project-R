#Prepare

##import packages
library(tidyverse)
library(readr)
library(dplyr)
library(here)
library(skimr)
library(janitor)
library(ggplot2)

##import data
activity <- bind_rows(read_csv("Fitbit_Data/march_data/dailyActivity_merged.csv"),read_csv("Fitbit_Data/april_data/dailyActivity_merged.csv"))
calories <- bind_rows(read_csv("Fitbit_Data/march_data/hourlyCalories_merged.csv"),read_csv("Fitbit_Data/april_data/hourlyCalories_merged.csv"))
intensity <- bind_rows(read_csv("Fitbit_Data/march_data/hourlyIntensities_merged.csv"),read_csv("Fitbit_Data/april_data/hourlyIntensities_merged.csv"))
steps <- bind_rows(read_csv("Fitbit_Data/march_data/hourlySteps_merged.csv"),read_csv("Fitbit_Data/april_data/hourlySteps_merged.csv"))

#Process

##eliminate duplicates
activity<-distinct(activity)
calories<-distinct(calories)
intensity<-distinct(intensity)
steps<-distinct(steps)

##clean column names
activity<-clean_names(activity)
calories<-clean_names(calories)
intensity<-clean_names(intensity)
steps<-clean_names(steps)

activity<-rename_with(activity,tolower)
calories<-rename_with(calories,tolower)
intensity<-rename_with(intensity,tolower)
steps<-rename_with(steps,tolower)

##convert dates to date/time types
activity$activity_date<-mdy(activity$activity_date)
calories$activity_hour<-mdy_hms(calories$activity_hour)
intensity$activity_hour<-mdy_hms(intensity$activity_hour)
steps$activity_hour<-mdy_hms(steps$activity_hour)

##aggregate hours into days
calories<-calories %>% 
  mutate(activity_date=as_date(activity_hour))
intensity<-intensity %>% 
  mutate(activity_date=as_date(activity_hour))
steps<-steps %>% 
  mutate(activity_date=as_date(activity_hour))

##sort by id/date and create summary statistic
calories<-calories %>% 
  group_by(id,activity_date) %>% 
  summarize(daily_calories=sum(calories))
intensity<-intensity %>% 
  group_by(id,activity_date) %>% 
  summarize(daily_intensity=sum(total_intensity))
steps<-steps %>% 
  group_by(id,activity_date) %>% 
  summarize(daily_steps=sum(step_total))

activity<-activity %>% 
  mutate(active_to_sedentary=round(((very_active_minutes+fairly_active_minutes+lightly_active_minutes/sedentary_minutes)/sedentary_minutes)*100,2))
activity<-activity %>% 
  group_by(id,activity_date) %>% 
  summarize(active_to_sedentary=sum(active_to_sedentary))
activity<-activity[-c(641),]

#Analyze

##categorize users by active level
avg_activity<-activity %>% 
  group_by(id) %>% 
  summarize(avg_active_to_sedentary=mean(active_to_sedentary))

avg_activity<-avg_activity %>% 
  mutate(active_type=case_when(avg_active_to_sedentary>=0&avg_active_to_sedentary<5 ~ "low_activity",
                               avg_active_to_sedentary>=5&avg_active_to_sedentary<10 ~ "moderate_activity",
                               .default = "high_activity"))

avg_activity_grouped<-avg_activity %>% 
  group_by(active_type) %>%
  summarize(amount=n()) %>%
  mutate(total=sum(amount),percentage=round((amount/total)*100,2))
  

ggplot(avg_activity_grouped, aes(x="", y=amount, fill=active_type)) +
  geom_bar(stat="identity", width=1) + 
  scale_fill_manual(values = c("lightgreen","maroon","lightblue")) +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title="Users by Activity Level") +
  theme(plot.title=element_text(hjust=0.5)) +
  geom_text(aes(label=percentage),position=position_stack(vjust=0.5))

##do the same with calories (based on average adult woman calories burned of 1600-2400 according to the Dietary Guidelines for Americans 2020-2025)
avg_calories<-calories %>% 
  group_by(id) %>% 
  summarize(avg_daily_calories=mean(daily_calories))

avg_calories<-avg_calories %>% 
  mutate(calorie_type=case_when(avg_daily_calories>=0&avg_daily_calories<1600 ~ "low_calorie",
                               avg_daily_calories>=1600&avg_daily_calories<2400 ~ "moderate_calorie",
                               .default = "high_calorie"))

avg_calories_grouped<-avg_calories %>% 
  group_by(calorie_type) %>%
  summarize(amount=n()) %>%
  mutate(total=sum(amount),percentage=round((amount/total)*100,2))


ggplot(avg_calories_grouped, aes(x="", y=amount, fill=calorie_type)) +
  geom_bar(stat="identity", width=1) + 
  scale_fill_manual(values = c("lightgreen","maroon","lightblue")) +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title="Users by Calorie Level") +
  theme(plot.title=element_text(hjust=0.5)) +
  geom_text(aes(label=percentage),position=position_stack(vjust=0.5))

##do the same with intensity
avg_intensity<-intensity %>% 
  group_by(id) %>% 
  summarize(avg_daily_intensity=mean(daily_intensity))

avg_intensity<-avg_intensity %>% 
  mutate(intensity_type=case_when(avg_daily_intensity>=0&avg_daily_intensity<150 ~ "low_intensity",
                                avg_daily_intensity>=150&avg_daily_intensity<300 ~ "moderate_intensity",
                                .default = "high_intensity"))

avg_intensity_grouped<-avg_intensity %>% 
  group_by(intensity_type) %>%
  summarize(amount=n()) %>%
  mutate(total=sum(amount),percentage=round((amount/total)*100,2))


ggplot(avg_intensity_grouped, aes(x="", y=amount, fill=intensity_type)) +
  geom_bar(stat="identity", width=1) + 
  scale_fill_manual(values = c("lightgreen","maroon","lightblue")) +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title="Users by Intensity Level") +
  theme(plot.title=element_text(hjust=0.5)) +
  geom_text(aes(label=percentage),position=position_stack(vjust=0.5))

##do the same with steps
avg_steps<-steps %>% 
  group_by(id) %>% 
  summarize(avg_daily_steps=mean(daily_steps))

avg_steps<-avg_steps %>% 
  mutate(steps_type=case_when(avg_daily_steps>=0&avg_daily_steps<5000 ~ "low_steps",
                                  avg_daily_steps>=5000&avg_daily_steps<10000 ~ "moderate_steps",
                                  .default = "high_steps"))

avg_steps_grouped<-avg_steps %>% 
  group_by(steps_type) %>%
  summarize(amount=n()) %>%
  mutate(total=sum(amount),percentage=round((amount/total)*100,2))


ggplot(avg_steps_grouped, aes(x="", y=amount, fill=steps_type)) +
  geom_bar(stat="identity", width=1) + 
  scale_fill_manual(values = c("lightgreen","maroon","lightblue")) +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title="Users by Steps Level") +
  theme(plot.title=element_text(hjust=0.5)) +
  geom_text(aes(label=percentage),position=position_stack(vjust=0.5))
