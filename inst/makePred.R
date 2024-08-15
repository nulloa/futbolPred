library(tidyverse)
library(futbolData)
library(rstan)
library(skellam)

fit <- readRDS(file="fit.rds")

exfit <- rstan::extract(fit)

test <- readRDS(file="test.rds")

teamID <- readRDS(file="teamID.rds")


offense <- as.data.frame(exfit[["offense"]])[2001:3000,] %>%
  gather("teamID", "offense") %>% 
  mutate(homeID = as.numeric(gsub("[^0-9.-]", "", teamID))) %>%
  left_join(teamID %>% select(HomeTeam, homeID))

defense <- as.data.frame(exfit[["defense"]])[2001:3000,] %>%
  gather("teamID", "defense") %>% 
  mutate(homeID = as.numeric(gsub("[^0-9.-]", "", teamID))) %>%
  left_join(teamID %>% select(HomeTeam, homeID))

res <- data.frame(
  constant_mu    = exfit[["constant_mu"]][2001:3000],
  home_advantage = exfit[["home_advantage"]][2001:3000]
)


test[1,c("HomeTeam","homeID","AwayTeam","awayID")]

test$finalD <- NULL
test$finalH <- NULL
test$finalA <- NULL


for(matchup in 1:nrow(test)){
  home_expected_goals <- exp(
    rowSums(
      data.frame(
        res$constant_mu,
        res$home_advantage,
        offense[which(offense$homeID==test[matchup,"homeID"]),"offense"],
        defense[which(defense$homeID==test[matchup,"awayID"]),"defense"]
      )
    )
  )
  
  
  away_expected_goals = exp(
    rowSums(
      data.frame(
        res$constant_mu,
        offense[which(offense$homeID==test[matchup,"awayID"]),"offense"],
        defense[which(defense$homeID==test[matchup,"homeID"]),"defense"]
      )
    )
  )
  
  niter <- length(away_expected_goals)
  finalRes <- rep(NA, niter)
  for(i in 1:niter){
    finalRes[i] <- rskellam(1, home_expected_goals[i], away_expected_goals[i])
  }
  
  
  test[matchup, "finalD"]   <- mean(finalRes == 0)
  test[matchup, "finalH"]   <- mean(finalRes >  0)
  test[matchup, "finalA"]   <- mean(finalRes <  0)
  
  # test[matchup, "finalest"] <- median(finalRes)
  # test[matchup, "finalper"] <- ifelse(median(finalRes) == 0, mean(finalRes == 0), ifelse(median(finalRes)<0, mean(finalRes<0), mean(finalRes>0)))
  
}


tmp = test[,-c(9:18)] %>% 
  rowwise() %>%
  mutate(finalper=max(c_across(finalD:finalA)),
         finalest=which.max(c_across(finalD:finalA)),
         finalest=factor(finalest)) 


tmp$finalest <- recode(tmp$finalest, "1" = "Draw", "2" = "Home", "3"="Away")

tmp
  

summary(finalRes)

mean(finalRes > 0)
mean(finalRes < 0)
mean(finalRes ==0)



