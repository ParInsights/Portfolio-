################################################
####
#### IST 687 - Intro to Data Sciennce Project
####
####
#### Baseball data through 2017
####
#### 2019-01-02
####

# AllstarFull.csv ---------- All-Star appearances
# Appearances.csv ---------- Details on the positions a player played at
# AwardsManagers.csv ------- Awards won by managers
# AwardsPlayers.csv -------- Awards won by players
# AwardsShareManagers.csv -- Award voting for manager awards
# AwardsSharePlayers.csv --- Award voting for player awards
# Batting.csv -------------- Batting statistics
# BattingPost.csv ---------- Post-season batting statistics
# CollegePlaying.csv ------- List of players and the colleges they attended
# Fielding.csv ------------- Fielding statists
# FieldingOF.csv ----------- Outfield position data
# FieldingOFsplit.csv ------ ??
# FieldingPost.csv --------- Post-season fielding data
# HallOfFame.csv ----------- Hall of Fame voting data
# HomeGames.csv ------------ Number of home games and attendance by year and park
# Managers.csv ------------- Managerial statistics
# ManagersHalf.csv --------- Split season data for managers
# Parks.csv ---------------- Park lookup
# People.csv --------------- Player names, DOB, other biographical data
# Pitching.csv ------------- Pitching statistics
# PitchingPost.csv --------- Post-season pitching statistics
# readme2014.txt ----------- 
# Salaries.csv ------------- Player salary data
# Schools.csv -------------- List of colleges that players attended
# SeriesPost.csv ----------- Post-season series information
# Teams.csv ---------------- Yearly statistics and standings for teams
# TeamsFranchises.csv ------ Franchise information
# TeamsHalf.csv ------------ //1981 half-season team data

path <- "~/Documents/baseballdatabank-master/core"

dataAllStar             <- read.csv(paste(path, "/", "AllstarFull.csv", sep = ""), header = TRUE)
dataApperances          <- read.csv(paste(path, "/", "Appearances.csv", sep = ""), header = TRUE)
dataAwardsManagers      <- read.csv(paste(path, "/", "AwardsManagers.csv", sep = ""), header = TRUE)
dataAwardsPlayers       <- read.csv(paste(path, "/", "AwardsPlayers.csv", sep = ""), header = TRUE)
dataAwardsShareManagers <- read.csv(paste(path, "/", "AwardsShareManagers.csv", sep = ""), header = TRUE)
dataAwardsSharePlayers  <- read.csv(paste(path, "/", "AwardsSharePlayers.csv", sep = ""), header = TRUE)
dataBatting             <- read.csv(paste(path, "/", "Batting.csv", sep = ""), header = TRUE)
dataBattingPost         <- read.csv(paste(path, "/", "BattingPost.csv", sep = ""), header = TRUE)
dataCollegePlaying      <- read.csv(paste(path, "/", "CollegePlaying.csv", sep = ""), header = TRUE)
dataFielding            <- read.csv(paste(path, "/", "Fielding.csv", sep = ""), header = TRUE)
dataFieldingOF          <- read.csv(paste(path, "/", "FieldingOF.csv", sep = ""), header = TRUE)
dataFieldingOFsplit     <- read.csv(paste(path, "/", "FieldingOFsplit.csv", sep = ""), header = TRUE)
dataFieldingPost        <- read.csv(paste(path, "/", "FieldingPost.csv", sep = ""), header = TRUE)
dataHallOfFame          <- read.csv(paste(path, "/", "HallOfFame.csv", sep = ""), header = TRUE)
dataHomeGames           <- read.csv(paste(path, "/", "HomeGames.csv", sep = ""), header = TRUE)
dataManagers            <- read.csv(paste(path, "/", "Managers.csv", sep = ""), header = TRUE)
dataManagersHalf        <- read.csv(paste(path, "/", "ManagersHalf.csv", sep = ""), header = TRUE)
dataParks               <- read.csv(paste(path, "/", "Parks.csv", sep = ""), header = TRUE)
dataPeople              <- read.csv(paste(path, "/", "People.csv", sep = ""), header = TRUE)
dataPitching            <- read.csv(paste(path, "/", "Pitching.csv", sep = ""), header = TRUE)
dataPitchingPost        <- read.csv(paste(path, "/", "PitchingPost.csv", sep = ""), header = TRUE)
dataSalaries            <- read.csv(paste(path, "/", "Salaries.csv", sep = ""), header = TRUE)
dataSchools             <- read.csv(paste(path, "/", "Schools.csv", sep = ""), header = TRUE)
dataSeriesPost          <- read.csv(paste(path, "/", "SeriesPost.csv", sep = ""), header = TRUE)
dataTeams               <- read.csv(paste(path, "/", "Teams.csv", sep = ""), header = TRUE)
dataTeamsFranchises     <- read.csv(paste(path, "/", "TeamsFranchises.csv", sep = ""), header = TRUE)

# Pulls player information (name, DOB, etc) into fielding, batting, pitching stat tables
# The merge() function takes two data frames, and moves the data from the second one to the first, based on the field specified in the "by" argument
fieldingDetail <- merge(dataFielding, dataPeople, by = "playerID")
battingDetail  <- merge(dataBatting,  dataPeople, by = "playerID")
pitchingDetail <- merge(dataPitching, dataPeople, by = "playerID")

# Returns all stats and info based on first and last name
battingDetail[which(battingDetail$nameFirst == "Bryce" & battingDetail$nameLast == "Harper"), ]
pitchingDetail[which(pitchingDetail$nameFirst == "Clayton" & pitchingDetail$nameLast == "Kershaw"), ]
fieldingDetail[which(fieldingDetail$nameFirst == "Aaron" & fieldingDetail$nameLast == "Judge"), ]

# Career batting average. For Barry Bonds, this should compute to approx 0.298 -- see https://www.mlb.com/player/barry-bonds-111188
firstName <- "Barry"
lastName  <- "Bonds"
sum(battingDetail$H[which(battingDetail$nameFirst == firstName & battingDetail$nameLast == lastName)]) / sum(battingDetail$AB[which(battingDetail$nameFirst == firstName & battingDetail$nameLast == lastName)])
