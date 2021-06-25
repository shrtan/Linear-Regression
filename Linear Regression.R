###LINEAR MODELS

library(tidyverse)
library(broom)
library(Lahman)

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

#1a Use runs (R) per game to predict average attendance
Teams_small %>% 
  mutate(R_per_G = R/G) %>%
  lm(avg_attendance~R_per_G, data=.)

#1a. Use home runs (HR) per game to predict average attendance
Teams_small %>% 
  mutate(HR_per_G = HR/G) %>%
  lm(avg_attendance~HR_per_G, data=.)

#1b. Use number of wins to predict average attendance
Teams_small %>% lm(avg_attendance~W, data=.)

#1c. Use year to predict average attendance
Teams_small %>% lm(avg_attendance~yearID, data=.)


#2. Are wins and runs per game or wins and home runs per game 
#correlated?

temp <- Teams_small %>% mutate(R_per_G = R/G, 
                               HR_per_G = HR/G) 
#correlation coefficient for wins and runs per game
cor(temp$W, temp$R_per_G)
#correlation coefficient for wins and home runs per game
cor(temp$W, temp$HR_per_G)


#3. Stratify Teams_small by wins: divide number of wins by 10 
#and then round to the nearest integer 
#Keep only strata 5 through 10, which have 20 or more data points

strat_teams <- Teams_small %>% mutate(strata = round(W/10)) %>%
  filter(strata >= 5 & strata <= 10)

#3a. How many observations are in the 8 win strata?
strat_teams %>% group_by(strata) %>% summarize(n())

#3b. Calculate the slope of the regression line predicting 
#average attendance given runs per game for each of the win strata
strat_teams %>% mutate(R_per_G = R/G) %>%
  group_by(strata) %>% 
  do(tidy(lm(avg_attendance~R_per_G, data=.))) %>%
  filter(term == "R_per_G")

#Calculate the slope of the regression line predicting 
#average attendance given HR per game for each of the win strata
strat_teams %>% mutate(HR_per_G = HR/G) %>%
  group_by(strata) %>% 
  do(tidy(lm(avg_attendance~HR_per_G, data=.))) %>%
  filter(term == "HR_per_G")


#4. Fit a multivariate regression determining the effects of runs
#per game, home runs per game, wins, and year on average attendance
fit <- Teams_small %>% mutate(R_per_G = R/G, HR_per_G = HR/G) %>%
  lm(avg_attendance~HR_per_G+R_per_G+W+yearID, data=.)
fit %>% tidy()


#5. Suppose a team averaged 5 runs per game, 1.2 home runs per game, 
#and won 80 games in a season

#What would this team's average attendance be in 2002?
predict(fit, data.frame(R_per_G=5, HR_per_G=1.2, W=80, yearID=1960))


#6. Use the model to predict average attendance for teams in 2002 
#in the original Teams data frame
Teams2002 <- Teams %>% filter(yearID == 2002) %>%
  mutate(R_per_G = R/G, HR_per_G = HR/G)

predicted <- predict(fit, Teams2002)

#What is the correlation between the predicted attendance and actual attendance?
cor(predicted, Teams2002$attendance)



###CONFOUNDING

library(dslabs)
data("research_funding_rates")
research_funding_rates

#Construct a two-by-two table of gender (men/women) by 
#award status (awarded/not) using the total numbers across 
#all disciplines
two_by_two <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, no_men = applications_men-awards_men,
            yes_women = awards_women, no_women = applications_women-awards_women) %>%
  gather %>%
  separate(key, c('awarded', "gender"), "_") %>%
  spread(gender, value)


#What is the percentage of men awarded?
sum(two_by_two$men[two_by_two$awarded == "yes"])/sum(two_by_two$men)
#What is the percentage of women awarded?
sum(two_by_two$women[two_by_two$awarded == "yes"])/sum(two_by_two$women)


#Run a chi-squared test External link on the two-by-two table to 
#determine whether the difference in the two success rates is significant
two_by_two %>% select(-awarded) %>% chisq.test() %>%
  tidy() %>% pull(p.value)

#dataset with number of applications, awards, and success rate for each gender
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

#plot the success rates versus disciplines, which have been 
#ordered by overall success, with colors to denote the genders and 
#size to denote the number of applications
ggplot(dat) + geom_point(aes(discipline, success, color = gender, size = applications)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

