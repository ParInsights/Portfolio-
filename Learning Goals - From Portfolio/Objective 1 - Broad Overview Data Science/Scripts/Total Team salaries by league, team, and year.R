install.packages("Lahman")

library(Lahman)
library(dplyr)

str(LahmanData)
View(LahmanData)

#' ## Basic manipulations

 playerInfo(verlaju01)
View(playerInfo())



View(Salaries)
str(Salaries)

GraphTopSalary<-

##Team Salaries by league, team, year 

teamSalaries <- Salaries %>%
  group_by(lgID, teamID, yearID) %>%
  summarise(Salary = sum(as.numeric(salary))) %>%
  group_by(yearID, lgID) %>%
  arrange(desc(Salary))



# Highest paid players each year:
HighestSal_byear <- Salaries %>%
  group_by(yearID) %>%
  filter(salary == max(salary)) 
  maxPlayers <- bind_rows(lapply(HighestSal_byear$playerID, playerInfo)) %>%
  select(-playerID)
  HighestSal_byear <- bind_cols(maxPlayers, HighestSal_byear)

# Plot maximum MLB salary by year (1985-present)           
  salarybyYear_plot<- ggplot(HighestSal_byear, aes(x = yearID, y = salary/1e6)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Year", y = "Salary (millions)")
  
  print(salarybyYear_plot)
  
  
  
# Plot salary distributions by year for all players
  ggplot(Salaries, aes(x = factor(yearID), y = salary/1e5)) +
    geom_boxplot(fill = "lightblue", outlier.size = 1) +
    labs(x = "Year", y = "Salary ($100,000)") +
    coord_flip()

  
  
  # Plot median MLB salary per year
  Salaries %>%
    group_by(yearID) %>%
    summarise(medsal = median(salary)) %>%
    ggplot(., aes(x = yearID, y = medsal/1e6)) +
    geom_point() +
    geom_smooth() +
    labs(x = "Year", y = "Median MLB salary (millions)")
  
  
  
  # add salary to Batting data
  batting <- Batting %>%
    filter(yearID >= 1985) %>%
    left_join(select(Salaries, playerID, yearID, teamID, salary), 
              by=c("playerID", "yearID", "teamID"))
  str(batting)
  