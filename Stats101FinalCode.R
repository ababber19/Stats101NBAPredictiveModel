
library(data.table)
library(ggcorrplot)
library(data.table)
library(visreg)
library(ggplot2)
library(ggpubr)

# Imports files needed
player_data <- read.csv("~/Desktop/Stats Final/player_data.csv")
Seasons_Stats <- read.csv("~/Desktop/Stats Final/Seasons_Stats.csv", stringsAsFactors = FALSE)



# 1998-1999 rookie player_data
rookiePlayerData98 <- subset(player_data, player_data$year_start==1998)
# 1999-2000 rookie player_data
rookiePlayerData99 <- subset(player_data, player_data$year_start==1999)
# 2000-2001 rookie player_data
rookiePlayerData00 <- subset(player_data, player_data$year_start==2000)
# 2001-2002 rookie player_data
rookiePlayerData01 <- subset(player_data, player_data$year_start==2001)
# 2002-2003 rookie player_data
rookiePlayerData02 <- subset(player_data, player_data$year_start==2002)
# 2002-2003 rookie player_data
rookiePlayerData03 <- subset(player_data, player_data$year_start==2003)
# 2003-2004 rookie player_data
rookiePlayerData04 <- subset(player_data, player_data$year_start==2004)
# 2004-2005 rookie player_data
rookiePlayerData05 <- subset(player_data, player_data$year_start==2005)
# 2005-2006 rookie player_data
rookiePlayerData06 <- subset(player_data, player_data$year_start==2006)
# 2006-2007 rookie player_data
rookiePlayerData07 <- subset(player_data, player_data$year_start==2007)



# Function to add players' season data in a certain year played to the function name provided
addPlayers <- function(yearPlayed, rookieYearData) {
  tempDataFrame <- data.frame()
  for(i in 1:length(rookieYearData$name)) {
    name <- toString(rookieYearData[i, 1])
    
    allIndexes <- which(Seasons_Stats$Player %in% name)
    
    for(j in 1:length(allIndexes)) {
      if(toString(Seasons_Stats[allIndexes[j], 2]) == yearPlayed) {
        tempDataFrame <- rbind(tempDataFrame, Seasons_Stats[allIndexes[j],])
        break
      }
    }
  }
  return(tempDataFrame)
}



# All players rookie season_stats
rookieSeasonStats <- data.frame()
rookieSeasonStats <- rbind(rookieSeasonStats, addPlayers(1998, rookiePlayerData98),
                          addPlayers(1999, rookiePlayerData99),
                          addPlayers(2000, rookiePlayerData00),
                          addPlayers(2001, rookiePlayerData01),
                          addPlayers(2002, rookiePlayerData02),
                          addPlayers(2003, rookiePlayerData03),
                          addPlayers(2004, rookiePlayerData04),
                          addPlayers(2005, rookiePlayerData05),
                          addPlayers(2006, rookiePlayerData06),
                          addPlayers(2007, rookiePlayerData07))



# Combines all NBA players and summarizes their NBA season stats
Seasons_Stats_As_DataTable <- as.data.table(Seasons_Stats)#[,c(3, 1, 2, 4:53)])
totalSeasonStats <- (Seasons_Stats_As_DataTable[,lapply(.SD, mean), by=Player, .SDcols = !c("Pos", "Tm")])





# Function similar to addPlayer, but now using total league stats
addPlayersVer2 <- function(rookieYearData, totalData) {
  tempDataFrame <- data.frame()
  
  for(i in 1:length(rookieYearData$Player)) {
    name <- toString(rookieYearData[i, 3])
    
    allIndexes <- which(totalData$Player %in% name)
    
    for(j in 1:length(allIndexes)) {
      tempDataFrame <- rbind(tempDataFrame, totalData[allIndexes[j],])
    }
  }
  return(tempDataFrame)
}



# Data table with each rookies' NBA statistics
rookieNBAStats <- addPlayersVer2(rookieSeasonStats, totalSeasonStats)
rookieNBAStats <- rookieNBAStats[,c(2, 3, 1, 4:51)]
colnames(rookieNBAStats) <- paste(colnames(rookieNBAStats), "total_stats", sep = "_")
colnames(rookieNBAStats)[3] <- "Player"





# Rookie Season Stats to analyze
# analyzingRookieSeasonStats <- rookieSeasonStats[, c(3, 7:9, 11, 35:44, 45:53)]
analyzingRookieSeasonStats <- rookieSeasonStats[, c(3, 7:53)]



# Rookie NBA total Stats to analyze
# analyzingRookieNBAStats <- rookieNBAStats[, c(3, 5:8, 30, 32:33, 35:36, 38, 40, 42, 46:51)]
analyzingRookieNBAStats <- rookieNBAStats[, c(3, 7)]


# Combines both dataframes
combinedData <- merge(analyzingRookieNBAStats, analyzingRookieSeasonStats, by="Player")
combinedData <- combinedData[,c(1:17, 19:22, 24:49)]





# Density Plot of TSP
densOfTSP <- ggplot(combinedData, 
                           aes(x = combinedData$TS.)) + 
  geom_histogram(aes(y = ..density..), fill = "white", color = "black") +
  xlab("True Shooting Percentage") +
  ylab("Density") +
  ggtitle("Density Plot of True Shooting Percentage") +
  theme(plot.title = element_text(size = 15, face = "bold")) + 
  geom_density(fill = "red", alpha = 0.2, linetype = "dashed", color = "red")


densOfTSP



# Density Plot of VORP
densOfVORP <- ggplot(combinedData, 
                     aes(x = combinedData$VORP)) + 
  geom_histogram(aes(y = ..density..), fill = "white", color = "black") +
  xlab("VORP") +
  ylab("Density") +
  ggtitle("Density Plot of VORP") +
  theme(plot.title = element_text(size = 15, face = "bold")) + 
  geom_density(fill = "red", alpha = 0.2, linetype = "dashed", color = "red")


densOfVORP



# Density Plot of DBPM
densOfBDPM <- ggplot(combinedData, 
                     aes(x = combinedData$DBPM)) + 
  geom_histogram(aes(y = ..density..), fill = "white", color = "black") +
  xlab("DBPM") +
  ylab("Density") +
  ggtitle("Density Plot of DBPM") +
  theme(plot.title = element_text(size = 15, face = "bold")) + 
  geom_density(fill = "red", alpha = 0.2, linetype = "dashed", color = "red")


densOfBDPM


# Density Plot of MP
densOfMP <- ggplot(combinedData, 
                   aes(x = combinedData$MP_total_stats)) + 
  geom_histogram(aes(y = ..density..), fill = "white", color = "black") +
  xlab("MP") +
  ylab("Density") +
  ggtitle("Density Plot of MP") +
  theme(plot.title = element_text(size = 15, face = "bold")) + 
  geom_density(fill = "red", alpha = 0.2, linetype = "dashed", color = "red")


densOfMP



# Prints all the density plots together
ggarrange(densOfTSP, densOfVORP, densOfBDPM, densOfMP)




# Boxplot of TSP
bp_TSP <- boxplot(combinedData$TS., ylab = "True Shooting Percentage", 
        main = "Boxplot of True Shooting Percentage",
        col = "blue")



# Boxplot of VORP
bp_VORP <- boxplot(combinedData$VORP, ylab = "VORP", 
        main = "Boxplot of VORP",
        col = "blue")



# Boxplot of DBPM
bp_BDPM <- boxplot(combinedData$DBPM, ylab = "DBPM", 
        main = "Boxplot of DBPM",
        col = "blue")



# Boxplot of MN
bp_MP <- boxplot(combinedData$MP_total_stats, ylab = "MP", 
        main = "Boxplot of MP",
        col = "blue")




# Summary Statistics
summary(combinedData$TS.)
IQR(combinedData$TS.)
sd(combinedData$TS.)



# Summary Statistics
summary(combinedData$VORP)
IQR(combinedData$VORP)
sd(combinedData$VORP)



# Summary Statistics
summary(combinedData$DBPM)
IQR(combinedData$DBPM)
sd(combinedData$DBPM)




# Summary Statistics
summary(combinedData$MP_total_stats)
IQR(combinedData$MP_total_stats)
sd(combinedData$MP_total_stats)





# Creates correlation table
corVals <- round(cor(combinedData[, c(2:47)], use="complete.obs"), 2)



# Creates correlation table of variables we are interested in
corValsVer2 <- corVals[c("MP_total_stats", "TS.", "VORP", "DBPM"), c("MP_total_stats", "TS.", "VORP", "DBPM")]
names(corValsVer2)[names(corValsVer2) == "MP_total_stats"]  <- "MP" 
names(corValsVer2)[names(corValsVer2) == "TS."]  <- "TSP" 


# Correlation plot of variables we're interested in
corPlot <- ggcorrplot(corValsVer2,
                      outline.color = "white", 
                      lab = TRUE, 
                      title = "Correlation Matrix",
                      lab_size = 5.5,)

corPlot







# Lmodel for Data
lmod <- lm(MP_total_stats ~ TS. + VORP + DBPM, combinedData)


summary(lmod)




# Partial Regressions
v1 <- visreg(lmod, "TS.", plot = FALSE)
v2 <- visreg(lmod, "VORP", plot = FALSE)
v3 <- visreg(lmod, "DBPM", plot = FALSE)
plot(v1)
plot(v2)
plot(v3)



# Residual Plot
predicted <- predict(lmod)
resids <- residuals(lmod)
lmodData <- data.frame(predicted, resids)



residPlot <- ggplot(lmodData, aes(x=predicted,y=resids)) + 
  geom_point() +
  xlab("Predicted") + 
  ylab("Residuals") + 
  ggtitle("Residuals vs Predicted Values") +
  geom_hline(yintercept = 0, color = "blue")



residPlot



























































# UNUSED CODE

# Same players' stats five years later
fiveYearsLaterSeasonStats <- addPlayers(2003, rookiePlayerData98)
fiveYearsLaterSeasonStats <- rbind(fiveYearsLaterSeasonStats, addPlayers(2004, rookiePlayerData99))
fiveYearsLaterSeasonStats <- rbind(fiveYearsLaterSeasonStats, addPlayers(2005, rookiePlayerData00))
fiveYearsLaterSeasonStats <- rbind(fiveYearsLaterSeasonStats, addPlayers(2006, rookiePlayerData01))
fiveYearsLaterSeasonStats <- rbind(fiveYearsLaterSeasonStats, addPlayers(2007, rookiePlayerData02))
fiveYearsLaterSeasonStats <- rbind(fiveYearsLaterSeasonStats, addPlayers(2008, rookiePlayerData03))
fiveYearsLaterSeasonStats <- rbind(fiveYearsLaterSeasonStats, addPlayers(2009, rookiePlayerData04))
fiveYearsLaterSeasonStats <- rbind(fiveYearsLaterSeasonStats, addPlayers(20010, rookiePlayerData05))
fiveYearsLaterSeasonStats <- rbind(fiveYearsLaterSeasonStats, addPlayers(2011, rookiePlayerData06))
fiveYearsLaterSeasonStats <- rbind(fiveYearsLaterSeasonStats, addPlayers(2012, rookiePlayerData07))



# Combines all the rookie Player Data
rookiePlayerDataCombined <- data.frame()
rookiePlayerDataCombined <- rbind(rookiePlayerDataCombined, 
                                  rookiePlayerData98, 
                                  rookiePlayerData99, 
                                  rookiePlayerData00,
                                  rookiePlayerData01,
                                  rookiePlayerData02,
                                  rookiePlayerData03,
                                  rookiePlayerData04,
                                  rookiePlayerData05,
                                  rookiePlayerData06,
                                  rookiePlayerData07)


















corValsVer3 <- corVals[c(21, 25, 28, 36:41, 20,32,  22, 31, 3), c(21, 25, 28, 36:41, 20, 32, 22, 31, 3)]
which(colnames(corVals)=="FT.")

corPlotTemp <- ggcorrplot(corValsVer3,
                      outline.color = "white", 
                      lab = TRUE, 
                      title = "Correlation Matrix",
                      lab_size = 5.5,
)

corPlotTemp




