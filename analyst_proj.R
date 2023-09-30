# Importing Data
chances <- read.csv("~/Downloads/chances.csv")

# Checking situational data
unique(chances$score_margin)
unique(chances$period) ## Check with TJ
max(chances$gameClock_start) # 23.96

# % of time the team on defense won regardless
chances$defWon <- chances$team_nba_def == chances$team_nba_winner
chancesPerc <- chances %>%
  group_by(defWon) %>%
  summarise(Count = n())
chancesPerc <- chancesPerc %>%
  mutate(Percentage = round((Count/sum(Count)*100)))

ggplot(chancesPerc, aes(x="", y=Percentage, fill=defWon)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  geom_label(aes(label = Percentage), color = "white", position = position_stack(vjust= 0.5), show.legend = FALSE) +
  guides(fill = guide_legend(title = "Defense Won")) +
  scale_fill_manual(values = c("Dark Green", "Forest Green")) +
  ggtitle("% of Time Team on Defense Won Game") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))

# 90% of the time Defense wins regardless

# fouls_def indicates that the defense fouled on the play
library(stringr)
chances$fouls_def <- str_replace(chances$fouls_def, "null", "0")
which(chances$fouls_def == 2, arr.ind=TRUE) # Instance where data says that there are 2 fouls on the defense, going to ignore in this case
chances2 <- subset(chances, fouls_def<2)

chancesFouls <- chances2 %>%
  group_by(fouls_def) %>%
  summarise(Count = n())
chancesFouls <- chancesFouls %>%
  mutate(Percentage = round((Count/sum(Count)*100)))

ggplot(chancesFouls, aes(x="", y=Percentage, fill=fouls_def)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  theme_void()+
  ggtitle("% of Time Team on Defense Fouled") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  geom_label(aes(label = Percentage), color = "white", position = position_stack(vjust= 0.5), show.legend = FALSE) +
  guides(fill = guide_legend(title = "Defense Fouled")) +
  scale_fill_discrete(labels = c("No", "Yes"))
 
# 82 % of the time the team on defense did not foul when down 3 points

# % of time a defense team fouled and also won the game
chancesFouledWin <- chances2 %>%
  filter(fouls_def == 1) %>%
  group_by(defWon) %>%
  summarise(Count = n())
chancesFouledWin <- chancesFouledWin %>%
  mutate(Percentage = round((Count/sum(Count)*100)))

# 90% of time when defense fouls they still win

# For times when the team on defense did not win, how many of those instances had they fouled
gamesDefLost <- chances2 %>%
  filter(defWon ==0)
nrow(gamesDefLost) # 40 games defense lost

# Looking for teams that came back to win more than usual
length(unique(gamesDefLost$team_nba_winner)) #16
comebackTeams <- as.data.frame(table(gamesDefLost$team_nba_winner))
avgComebacks <- mean(comebackTeams$Freq) # 2.5 wins
comebackTeams <- comebackTeams %>%
  mutate(avgComebacks = mean(comebackTeams$Freq))
comebackTeams <- comebackTeams %>%
  mutate(percAA = ((Freq-avgComebacks)/avgComebacks)*100)

# Those teams Overall Win %
comebackTeams <- c("DAL", "GSW", "LAC", "WAS", "DEN", "CLE", "LAL")
chancesComeback <- chances %>%
  filter(team_nba_off %in% comebackTeams)

chancesComeback$defWon <- as.integer(as.logical(chancesComeback$defWon))

chancesComeback <- chancesComeback %>%
  group_by(team_nba_off) %>%
  mutate(Games = n())
chancesComeback <- chancesComeback %>%
  group_by(team_nba_off) %>%
  filter(defWon == 0) %>%
  mutate(GamesWon = n())
chancesComeback <- chancesComeback %>%
  mutate(percWon = (GamesWon/Games)*100)
chancesComeback <- chancesComeback[!duplicated(chancesComeback$team_nba_winner),]
  

chancesDefLost <- chances2 %>%
  filter(defWon ==0) %>%
  group_by(fouls_def) %>%
  summarise(Count = n())

chancesDefLost <- chancesDefLost %>%
  mutate(Percentage = round((Count/sum(Count)*100)))

# When defense lost, they still only fouled in 7 of those instances

ggplot(chancesDefLost, aes(x="", y=Percentage, fill=fouls_def)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  theme_void() +
  geom_label(aes(label = Percentage), color = "white", position = position_stack(vjust= 0.5), show.legend = FALSE) +
  guides(fill = guide_legend(title = "Defensive Fouls")) +
  scale_fill_manual(values = c("Dark Green", "Forest Green")) +
  ggtitle("% of Time Defense Fouled when Lost Game") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))

chances2$defWon <- as.integer(as.logical(chances2$defWon))
chances2$shot_qSP[chances2$shot_qSP =="null"] <- NA

chances2 <- chances2 %>%
  mutate(
    shot_qSP_range = case_when(
      shot_qSP >= 60 &  shot_qSP < 100 ~ "High",
      shot_qSP >= 30 & shot_qSP < 60 ~ "Medium",
      shot_qSP>= 0 & shot_qSP <30 ~ "Low"
    )
  )


# Teams on Defense Overall Win Distribution
chancesOverallWin <- chances2 %>%
  group_by(team_nba_def) %>%
  mutate(Games = n())

chancesOverallWin <- chancesOverallWin %>%
  group_by(team_nba_def) %>%
  filter(defWon == 1) %>%
  mutate(GamesWon = n())

chancesOverallWin <- chancesOverallWin %>%
  mutate(percWon = (GamesWon/Games)*100)

chancesOverallWin <- chancesOverallWin[!duplicated(chancesOverallWin$team_nba_def),]

WinDist <- as.data.frame(cbind(chancesOverallWin$team_nba_def, chancesOverallWin$percWon))
colnames(WinDist) <- c("Team", "WinPerc")
WinDist$WinPerc <- as.numeric(WinDist$WinPerc)

hist(WinDist$WinPerc, main = "Win %")

ggplot(WinDist, aes(x=WinPerc)) +
  geom_histogram(color ="darkgrey", fill = "forestgreen", just = 1, binwidth = 5) +
  labs(title = "Histogram of Win %", subtitle = "Teams on Defense w/ Given Constraints") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), plot.subtitle = element_text(hjust = 0.5, size = 14)) +
  labs(x = "Win %", y = "Count")

plot(density(WinDist$WinPerc, bw = 5), main = "Density of Win %s", xlab = "Win %, BW = 5", xlim = c(50,100))

# Creating a logistic regression model
model <- glm(defWon ~ fouls_def, family = binomial, data = chances2)
summary(model) 

# Include time left in the game
modelb <- glm(defWon ~ fouls_def + gameClock_start, family = binomial, data = chances2)
summary(modelb)


mean(WinDist$WinPerc) # 90.07
sd(WinDist$WinPerc) # 11.33

