# Load libraries
library(tidyverse)
library(futbolData)
library(rstan)

df <- laliga %>%
  filter(Season %in% c(2019:2021)) %>%
  mutate(Date = as.Date(Date, format="%d/%m/%Y"))

train <- df %>%
  filter(Date != "2022-05-22")

test <- df %>%
  filter(Date == "2022-05-22")

# Create ID variables for teams
teamID <- data.frame(
  HomeTeam = unique(c(df$HomeTeam, df$AwayTeam)),
  AwayTeam = unique(c(df$HomeTeam, df$AwayTeam))
) %>%
  mutate(
    homeID = as.numeric(factor(HomeTeam)),
    awayID = as.numeric(factor(AwayTeam))
  )

# Merge datasets
 test <- test %>% 
  left_join(teamID %>% select(HomeTeam, homeID)) %>%
  left_join(teamID %>% select(AwayTeam, awayID))
 
 train <- train %>% 
   left_join(teamID %>% select(HomeTeam, homeID)) %>%
   left_join(teamID %>% select(AwayTeam, awayID))





# Setup data for model and MCMC parameters
nchains <- 3
niter   <- 2000
nwarmup <- 1000
dat <- list(n_games = nrow(train),
            n_teams = length(unique(c(train$HomeTeam, train$AwayTeam))), 
            n_seasons = length(unique(train$Season)),
            n_leagues = 1,
            league = 1,
            home_team = train$homeID,
            away_team = train$awayID,
            goal_difference = train$FinalScore
            )

# Setup number of cores to use
# if(nchains>1){
#   rstan_options(auto_write = TRUE)
#   options(mc.cores = parallel::detectCores())
# }

# Set up the model in stan
fit <- stan(file="ZeroInfHier.stan", data=dat, iter=niter, warmup=nwarmup, chains = 3, init_r=1)
          
          # control = list(adapt_delta = 0.99, max_treedepth = 15))

saveRDS(fit, "../inst/fit.rds")
saveRDS(teamID, "../inst/teamID.rds")
saveRDS(test, "../inst/test.rds")
saveRDS(train, "../inst/train.rds")






