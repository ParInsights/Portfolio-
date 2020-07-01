install.packages("Lahman")

library(Lahman)
library(dplyr)

str(LahmanData)
#' ## Basic manipulations

str(Batting)
View(Batting)

# select batting variables
batting <- select(tbl_df(Batting), playerID, yearID, teamID, G, AB:H, HR) 
# sort by player, year, team
batting <- arrange(batting, playerID, yearID, teamID)
# keep only recent years
batting <- filter(batting, yearID > 1985)

# add salary to Batting data; need to match by player, year and team
# NB:  dplyr coerces yearID to character because it is a factor in Salaries
(batting <- batting %>% left_join(Salaries))

# the same in base R using merge():
batting2 <- merge(batting, 
                  Salaries[,c("playerID", "yearID", "teamID", "salary")], 
                  by=c("playerID", "yearID", "teamID"), all.x=TRUE)


# Add name, age and bat hand information from Master
master <- select(tbl_df(Master), playerID, birthYear, birthMonth, 
                 nameLast, nameFirst, bats)
batting <- batting %>%
  left_join(master) %>%
  mutate(age = yearID - birthYear - ifelse(birthMonth < 10, 0, 1)) %>%
  select(-(birthYear:birthMonth))

# same with base R	                                 
Master[, c('playerID', 'birthYear', 'birthMonth',
           'nameLast', 'nameFirst', 'bats')]
batting2 <- merge(batting, master, all.x = TRUE)
batting2$age <- with(batting, yearID, birthYear -
                       ifelse(birthMonth < 10, 0, 1))

#' ## Queries about players

# form a players data.frame, that is grouped by playerID
players <- group_by(batting, playerID)


# form a team data.frame, that is grouped by teamID
teams <- group_by(batting, teamID)

# For each player, find the two years with most hits
View(filter(players, min_rank(desc(H)) <= 2 & H > 0)) 
filter(players, min_rank(desc(H)) <= 2 & H > 0)


# Within each player, rank each year by the number of games played
mutate(players, G_rank = min_rank(G))

# For each player, find every year that was better than the previous year
filter(players, G > lag(G))
# For each player, compute avg change in games played per year
mutate(players, G_change = (G - lag(G)) / (yearID - lag(yearID)))

# For each player, find all where they played more games than average
filter(players, G > mean(G))

# For each, player compute a z score based on number of games played
mutate(players, G_z = (G - mean(G)) / sd(G))

##Sum salaries 

MeanSalaryaggregate()



# 2nd Analysis.
Master_Salaries_Plot_Ref4 <- salary %>% group_by(yearID,lgID) %>% summarise(Average_Salaries=mean(salary),Maximum_sal=max(salary))    
head(Master_Salaries_Plot_Ref4)

ggplot(salary, aes((yearID), Average_Salaries,col=lgID)) + geom_line()

ggplot(salary, aes(yearID, sqrt(Average_Salaries),col=lgID)) + geom_line()


##Salaries 

salaries_2016 <-
  salaries %>%
  filter(yearID == 2016) %>%
  group_by(playerID) %>%
  summarize(salary = sum(salary, na.rm = TRUE)) %>%
  mutate(salary = salary / 1000) %>%
  inner_join(players, by = "playerID") %>%
  select(playerID, salary)
