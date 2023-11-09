library(readxl)
wages <- read_excel("CPS1988.xls")
View(CPS1988)

library(tidyverse)


###### exploring data ########
View(wages$wage)

dim(wages)

glimpse(wages)


#what variables are in data set
names(wages)

#see what possible values exist in a variable
unique(wages$ethnicity)
unique(wages$region)

table(wages$ethnicity)
table(wages$region)
table(wages$smsa)
table(wages$parttime)

##view distribution of region
wages %>% 
  select(region) %>% 
  count(region) %>%
  arrange(desc(n)) %>% 
  View()

#check if missing data
which(is.na(wages))

summary(wages$wage)
boxplot(wages$wage)

summary(wages$education)
boxplot(wages$education)

summary(wages$experience) #someone has minus 4 years of experience? 
boxplot(wages$experience)

######## #Data cleaning #######

#variable types - switching character variables to factor variables
glimpse(wages)

wages$smsa <- as.factor(wages$smsa)
wages$ethnicity <- as.factor(wages$ethnicity)
wages$region <- as.factor(wages$region)
wages$parttime <- as.factor(wages$parttime)

class(wages$smsa)
class(wages$ethnicity)
class(wages$region)
class(wages$parttime)

#current wage is per week, change it per month by multiplying by 4
wages %>%
  mutate(wage = wage*4) %>% 
  view

#viewing cases where black americans and white america 
#earn more than 4000 USD per month and have more than 15 years of education
wages %>% select(ethnicity, wage, smsa, education) %>% 
  filter((ethnicity %in% "afam") & wage > 3999 & education > 15) %>% 
  View()

wages %>% select(ethnicity, wage, smsa, education) %>% 
  filter((ethnicity %in% "cauc") & wage > 3999 & education > 15)

######### manipulate data #######

#rename first column ...1
wages %>% rename(participant = ...1) %>% 
  view

#categorise education level
wages %>% select(education) %>% 
  mutate(education_level = case_when(
    education < 12 ~ "Less than 12",
    education >= 12 & education <= 15 ~ "12 to 15", 
    education > 15 ~ "16 and more"
  ))


#categorise wages into tranch
wages %>% select(wages) %>% 
  mutate(wages_level= case_when(
    wages < 1500 ~ "Less than 1500",
    wages >= 1500 & wages <= 2500 ~ "1500 to 2500", 
    wages > 2500 & wages <= 3100 ~ "2501 to 3100"
    wages > 3100 ~ "3101 and more"
  ))

#categorise experience variable
experience_level <- wages %>% 
  mutate(experience_level = if_else(experience <= 18.2, 
                                    "low to medium",
                                    "high")) %>% 
  view