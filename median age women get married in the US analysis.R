"median age women got married in the US"

df <- read.csv("https://query.data.world/s/c3i5abnwj7yrxqeqpaom2irkj3an26", header=TRUE, stringsAsFactors=FALSE);

library(tidyverse)
library(ggplot2)

names(df)

#change data set name
rename(df, "State" = "Name")

#view different time periods
unique(df$TimeFrame)

#compare median age of marriage in years 2006-2010 and years 2015-2019
mean_2006_2010 <- mean(df$Median.Age[TimeFrame=2006-2010])
mean_2015_2019 <- mean(df$Median.Age[TimeFrame=2015-2019])

#first create data sets that only include specific year in each
med_2015_2019 <- median(df %>%
  select(Name, Median.Age, TimeFrame) %>%
  filter(TimeFrame=="2015-2019"))

med_2006_2010 <- df %>%
  select(Name, Median.Age, TimeFrame) %>%
  filter(TimeFrame=="2006-2010")

#then create new variables with median
med1519 <- median(med_2015_2019$Median.Age)

med0910 <- median(med_2006_2010$Median.Age)


#######compare median age in each state from year X to year Y######
#line plot
ggplot(data=df, aes(x=TimeFrame, y=Median.Age, group=1)) +
  geom_line()+
  geom_point()


ggplot(data=df, aes(x=TimeFrame=, y="Mean 06-10", group=1)) +
  geom_line()+
  geom_point()

#1. set each period as a vector
v610 <- mean(df$Median.Age[df$TimeFrame=="2006-2010"]) 
v1519 <- mean(df$Median.Age[df$TimeFrame=="2015-2019"]) 

#3. combine the two vectors? 
crazy <- c(v610, v1519)

#4. look at standard deviation of crazy vector and then both vectors seperately
sd(crazy)
sd(v610)
sd(v1519)

#5. visualise vectors
ggplot(df, aes(x=TimeFrame)) + v610



#compare population dentisty in each state period X and period Y

#create different plots

###CHANGING DATA FRAME OF DF#####
#check if is a data frame
class(df)
#explore
str(df)

#create mean column
df["Mean 06-10"] <- mean(df$Median.Age[df$TimeFrame=="2006-2010"]) 
df["Mean 15-19"] <- mean(df$Median.Age[df$TimeFrame=="2015-2019"])

#graph in progress
df %>% ggplot(aes(TimeFrame, Median.Age))+
  stat_summary(fun = "mean_cl_boot", geom = "pointrange")+
  geom_point(size=3, alpha=0.5) +
  geom_line(aes(state_summary))+
  labs(x=NULL,title="Mean of Women married in the US in 2006-2010 and 2015-2019", 
      adj = 0.9)

df %>% ggplot(aes(TimeFrame, Median.Age, group = 1)) + 
  stat_summary(fun = "mean_cl_boot", siez=2, geom = "pointrange") + 
  stat_summary(fun = mean, geom = "line")+
  geom_point(size=3, alpha=0.5) +
  labs(x=NULL,title="Mean of Women married in the US in 2006-2010 and 2015-2019", 
       adj = 0.9)

df %>% ggplot(aes(TimeFrame, Median.Age))+
  geom_boxplot()+
  geom_point(alpha=0.5,
             aes(size=Population.Density.per.Sq.Mi))

df %>% ggplot(aes(TimeFrame, Median.Age, group = 1)) + 
  stat_summary(fun = "mean_cl_boot", geom = "pointrange") + 
  stat_summary(fun = mean, geom = "line", fatten=3)+
  geom_point(size=3, alpha=0.5) +
  labs(x=NULL,title="Mean of Women married in the US in 2006-2010 and 2015-2019", 
       adj = 0.9)

df %>% ggplot(aes(TimeFrame, Median.Age, group = 1)) + 
  stat_summary(fun.data = mean_se, geom = "line", size=1, colour="blue") +
  stat_summary(fun.data = mean_se, size=2, colour="blue") +
  geom_point(size=3, alpha=0.5) +
  labs(x=NULL,title="Mean of Women married in the US in 2006-2010 and 2015-2019", 
       adj = 0.9)
