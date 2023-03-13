## Load the libraries
library(readr)
library(ggplot2)
library(dplyr)
confirmed_cases_worldwide <- read_csv('C:/Users/karti/Videos/Datacamp/Covid-19/confirmed_cases_worldwide.csv')

##Confirmed cases throughout the world using ggplot
ggplot(data=confirmed_cases_worldwide,aes(x=date,y=cum_cases) )+geom_line() + labs(y='Cumulative confirmed cases')

##China compared to the rest of the world
confirmed_cases_china_vs_world <- read_csv('C:/Users/karti/Videos/Datacamp/Covid-19/confirmed_cases_china_vs_world.csv')
plt_cum_confirmed_cases_china_vs_world <- ggplot(data=confirmed_cases_china_vs_world,aes(x=date,y=cum_cases)) +
  geom_line(aes(x=date,y=cum_cases,color = 'is_china')) +
  ylab("Cumulative confirmed cases")


###Annotate
who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(date = as.Date(date))
plt_cum_confirmed_cases_china_vs_world + 
  geom_vline(aes(xintercept=date), data = who_events,linetype='dashed')+
  geom_text(aes(x=date,label=event),data = who_events, y=1e5)


##Adding a trend line to China
# Filter for China, from Feb 15
china_after_feb15 <- confirmed_cases_china_vs_world %>%
  filter(date >= '2020-02-15', is_china=='China')
plt_china_after_feb15 <- ggplot(data=china_after_feb15,aes(x=date,y=cum_cases) )+ geom_line() + 
  geom_smooth(method='lm',se=TRUE) +
  ylab("Cumulative confirmed cases")


# Filter confirmed_cases_china_vs_world for not China
not_china <- confirmed_cases_china_vs_world %>%
  filter(is_china == "China")

# Using not_china, draw a line plot cum_cases vs. date
# Add a smooth trend line using linear regression, no error bars
plt_not_china_trend_lin <- ggplot(not_china, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")

# See the result
plt_not_china_trend_lin 



##Which countries outside of China have been hit hardest?
confirmed_cases_by_country <- read_csv("C:/Users/karti/Videos/Datacamp/Covid-19/confirmed_cases_by_country.csv")
glimpse(confirmed_cases_by_country)
# Group by country, summarize to calculate total cases, find the top 7
top_countries_by_total_cases <- confirmed_cases_by_country %>%
  group_by(country) %>%
  summarize(total_cases=max(cum_cases)) %>%
  top_n(7,total_cases)

# See the result
top_countries_by_total_cases


# Read in the dataset from datasets/confirmed_cases_top7_outside_china.csv
confirmed_cases_top7_outside_china <- read_csv("C:/Users/karti/Videos/Datacamp/Covid-19/confirmed_cases_top7_outside_china.csv")

# Glimpse at the contents of confirmed_cases_top7_outside_
glimpse(confirmed_cases_top7_outside_china)
# Using confirmed_cases_top7_outside_china, draw a line plot of
# cum_cases vs. date, colored by country
ggplot(data=confirmed_cases_top7_outside_china,aes(x=date,y=cum_cases,color=country)) + geom_line()
