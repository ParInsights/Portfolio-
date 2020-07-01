install.packages("Lahman")

library(Lahman)
library(dplyr)

str(LahmanData)
View(LahmanData)

#' ## Basic manipulations


View(Salaries)
str(Salaries)

# select salaries by team variables
salary <- select(tbl_df(Salaries), playerID, yearID, teamID, lgID, salary) 

# sort by Team , year, salary 
salary <- arrange(salary, teamID, yearID, lgID, salary)

# keep only recent years
salary <- filter(salary, yearID > 1985)


##Filter by Team Salaries by league, team, year 
salary <- Salaries %>%
  group_by(lgID, teamID, yearID) %>%
  summarise(Salary = sum(as.numeric(salary))) %>%
  group_by(yearID, lgID) %>%
  arrange(desc(Salary))





# Plot maximum MLB salary by year (1985-present)           
salarybyYear_plot<- ggplot(HighestSal_byear, aes(x = yearID, y = salary/1e6)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Year", y = "Salary (millions)")

histogram(Salaries$lgID)

#Filter salary by group
salaryperyear<-group_by(salary, playerID, yearID)

meanSalary %>% 
  # Calculate the max salary by player
  group_by(salaryperyear,playerID) %>% 
  summarize(max_salary = max(salary, na.rm = TRUE)) %>% 
  
  # Calculate the average of the max salaries
  summarize(avg_salary = mean(max_salary, na.rm = TRUE))




#Average salary of player 
salaryperyear %>%

#' ## Queries about players

# form a players data.frame, that is grouped by playerID
players <- group_by(batting, playerID)


# form a salary data.frame, that is grouped by teamID
teamsSalary <- group_by(salary, teamID)

