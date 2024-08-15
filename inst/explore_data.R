library(futbolData)
library(engsoccerdata)
library(tidyverse)

# Checking to see if the results from engsoccerdata and my futboldata match
# Turns out they do

futbolData::laliga %>%
  ggplot(.) + geom_histogram(aes(x=FinalScore)) + facet_wrap(~Season) + theme_bw()

engsoccerdata::spain %>%
  filter(Season %in% c(2005:2020)) %>%
  ggplot(.) + geom_histogram(aes(x=hgoal-vgoal)) + facet_wrap(~Season) + theme_bw()

futbolData::laliga %>%
  ggplot(.) + geom_bar(aes(x=FinalRes)) + facet_wrap(~Season) + theme_bw()

futbolData::laliga %>%
  filter(Season=="2015") %>%
  ggplot(.) + geom_bar(aes(x=FinalRes)) + facet_grid(Season~paste("Home Team:", HomeTeam)) + theme_bw()

engsoccerdata::spain %>%
  mutate(FinalRes = ifelse(hgoal==vgoal, "D", ifelse(hgoal<vgoal, "A", "H"))) %>%
  filter(Season %in% c(2005:2020)) %>%
  ggplot(.) + geom_bar(aes(x=FinalRes)) + facet_wrap(~Season) + theme_bw()



futbolData::laliga %>%
  filter(Season=="2015") %>%
  group_by()


