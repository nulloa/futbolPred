library(tidyverse)
library(futbolData)
library(rstan)
library(skellam)

fit <- readRDS(file="fit.rds")

exfit <- rstan::extract(fit)

teamID <- readRDS(file="teamID.rds")


# Create Teams dataset
teamGoals <- laliga_team %>%
  group_by(Team) %>%
  summarise(
    mnGoals = mean(Goals),
    mnGoalsAgainst = mean(GoalsAgainst)
  )

# Plot Offense estimates
as.data.frame(exfit[["offense"]]) %>%
  gather("teamID", "value") %>%
  mutate(homeID = as.numeric(gsub("[^0-9.-]", "", teamID))) %>%
  left_join(teamID %>% select(HomeTeam, homeID)) %>%
  group_by(HomeTeam) %>%
  summarise(
    est = mean(value),
    lb  = quantile(value, probs=0.025),
    ub  = quantile(value, probs=0.975),
  ) %>%
  rename(Team = HomeTeam) %>%
  left_join(teamGoals) %>%
  mutate(Team = factor(Team, levels=unique(Team)[order(est)])) %>%
  ggplot(aes(y=Team, color=mnGoals)) + 
  geom_point(aes(x=est)) + 
  geom_segment(aes(y=Team, yend=Team, x=lb, xend=ub)) + 
  theme_bw() + 
  labs(y="", x="Offense est", color="Mean Goals \nScored per Game")


# Plot Offense estimates
as.data.frame(exfit[["defense"]]) %>%
  gather("teamID", "value") %>%
  mutate(homeID = as.numeric(gsub("[^0-9.-]", "", teamID))) %>%
  left_join(teamID %>% select(HomeTeam, homeID)) %>%
  group_by(HomeTeam) %>%
  summarise(
    est = mean(value),
    lb  = quantile(value, probs=0.025),
    ub  = quantile(value, probs=0.975),
  ) %>%
  rename(Team = HomeTeam) %>%
  left_join(teamGoals) %>%
  mutate(Team = factor(Team, levels=unique(Team)[order(est)])) %>%
  ggplot(aes(y=Team, color=mnGoalsAgainst)) + 
  geom_point(aes(x=est)) + 
  geom_segment(aes(y=Team, yend=Team, x=lb, xend=ub)) + 
  theme_bw() + 
  labs(y="", x="Defense est", color="Mean Goals \nScored Against \nper Game")
