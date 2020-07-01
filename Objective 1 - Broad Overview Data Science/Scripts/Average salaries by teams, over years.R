# Average salaries by teams, over years
#Some franchises are multiply named, so add a new variable
# 'franchise' to the Salaries data as a lookup table

franchise <- c(`ANA` = "LAA", `ARI` = "ARI", `ATL` = "ATL", 
               `BAL` = "BAL", `BOS` = "BOS", `CAL` = "LAA",
               `CHA` = "CHA", `CHN` = "CHN", `CIN` = "CIN", 
               `CLE` = "CLE", `COL` = "COL", `DET` = "DET", 
               `FLO` = "MIA", `HOU` = "HOU", `KCA` = "KCA", 
               `LAA` = "LAA", `LAN` = "LAN", `MIA` = "MIA", 
               `MIL` = "MIL", `MIN` = "MIN", `ML4` = "MIL", 
               `MON` = "WAS", `NYA` = "NYA", `NYM` = "NYN", 
               `NYN` = "NYN", `OAK` = "OAK", `PHI` = "PHI", 
               `PIT` = "PIT", `SDN` = "SDN", `SEA` = "SEA",
               `SFG` = "SFN", `SFN` = "SFN", `SLN` = "SLN", 
               `TBA` = "TBA", `TEX` = "TEX", `TOR` = "TOR",
               `WAS` = "WAS")

Salaries$franchise <- unname(franchise[Salaries$teamID])



# Average salaries annual salaries by team, in millions USD
avg_team_salaries <- Salaries %>%
  group_by(yearID, franchise, lgID) %>%
  summarise(salary= mean(salary)/1e6) %>%
  filter(!(franchise == "CLE" & lgID == "NL"))

# Spaghetti plot of team salary over time by team
# Yankees have largest average team salary since 2003
ggplot(avg_team_salaries, 
       aes(x = yearID, y = salary, group = factor(franchise))) +
  geom_path() +
  labs(x = "Year", y = "Average team salary (millions USD)")
# }

#Plot Team Salaries
teamSalaries <- Salaries %>%
  group_by(lgID, teamID, yearID) %>%
  summarise(Salary = sum(as.numeric(salary))) %>%
  group_by(yearID, lgID) %>%
  arrange(desc(Salary))

# Highest paid players each year:
maxSal <- Salaries %>%
  filter(salary == max(salary)) 
maxPlayers <- bind_rows(lapply(maxSal$playerID, players)) %>%
  select(-playerID)
maxSal <- bind_cols(maxPlayers, maxSal)

install.packages("dyplr")
install.packages("xlsx")
install.packages("openxlsx")
library(openxlsx)
library("xlsx")
install.packages("rJava")
library("r.tools")
install.packages(r.tool)
r.too

write.xlsx(maxSal, file = "MaxSalaryForFilter.xlsx")
write.xlsx(maxSal, file = "MaxSalaryForFilter.xlsx", asTable = TRUE)


head(maxSal)
