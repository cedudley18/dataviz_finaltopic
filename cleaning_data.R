library(tidyverse)
library(broom)
library(glmnet)


## 2016-2017
nhl_2016 <- readxl::read_xlsx("data/nhl odds 2016-17 (1).xlsx")

# trade deadline and date indicator variables
nhl_2016 <-
  nhl_2016 %>%
  mutate(Deadline = as.numeric(Date %in% c(301:611)),
         Year = as.numeric(Date %in% c(101:611)))

# adding date variable
nhl_2016 <- 
  nhl_2016 %>%
  mutate(Date = ifelse(Year == 0, paste(16, Date, sep = ""),
                       paste(17, Date, sep = "0")))

# convert date into a "date"
nhl_2016 <-
  nhl_2016 %>%
  mutate(Date = lubridate::ymd(Date))

# taking out playoff games
nhl_2016 <-
  nhl_2016 %>%
  filter(Date < "2017-4-12")

# making ID variable for game
nhl_2016$ID <- seq.int(nrow(nhl_2016))
nhl_2016 <-
  nhl_2016 %>%
  mutate(ID = as.integer(ID),
         ID = ID / 2,
         ID = ceiling(ID))

# indicator variable for days since trade deadline
nhl_2016 <-
  nhl_2016 %>%
  mutate(Deadline2 = Date - lubridate::ymd(20170301))

# make data wider - each row is one game
nhl_2016_home<- nhl_2016 %>%
  filter(VH=="H")
nhl_2016_away<-nhl_2016 %>%
  filter(VH=="V")
nhl_2016_wide<-bind_cols(nhl_2016_home,nhl_2016_away)

# tidy wider data
nhl_2016_wide <- 
  nhl_2016_wide[, c(1, 2, 22, 3, 4, 24, 5, 25, 6, 
                    26, 7, 27, 8, 28, 9, 29, 10, 30, 
                    11, 31, 12, 32, 13, 33, 14, 34, 15, 
                    35, 16, 36, 17, 18, 19, 20, 21, 23, 
                    37, 38, 39, 40)]
nhl_2016_neat <- 
  nhl_2016_wide[-c(4, 24, 27, 35:40)]

nhl_2016_neat <-
  nhl_2016_neat %>%
  rename("Date" = Date...1,
         "Rot Home Team" = Rot...2,
         "HomeTeam" =Team...4,
         "1st Home Team Goals" = `1st...5`,
         "2nd Home Team Goals" = `2nd...6`,
         "3rd Home Team Goals" = `3rd...7`,
         "FinalHome" = Final...8,
         "Open Home Team" = Open...9,
         "CloseHomeTeam" = Close...10,
         "Puck Line Home Team" = `Puck Line...11`,
         "Open OU" = `Open OU...13`,
         "Close OU" = `Close OU...35`,
         "DeadlineInd" = Deadline...17,
         "Year" = Year...18,
         "Game ID" = ID...19,
         "DeadlineDays" = Deadline2...20,
         "Rot Away Team" = Rot...22,
         "AwayTeam" =Team...24,
         "1st Away Team Goals" = `1st...25`,
         "2nd Away Team Goals" = `2nd...26`,
         "3rd Away Team Goals" = `3rd...27`,
         "FinalAway" = Final...28,
         "Open Away Team" = Open...29,
         "CloseAwayTeam" = Close...30,
         "Puck Line Away Team" = `Puck Line...31`,
  )

# market probabilities
nhl_2016_neat <-
  nhl_2016_neat %>%
  mutate(BoundaryProbHome = ifelse(CloseHomeTeam >= 100, 100/(CloseHomeTeam + 100),
                                   abs(CloseHomeTeam) / (100 + abs(CloseHomeTeam)))) 

nhl_2016_neat <-
  nhl_2016_neat %>%
  mutate(BoundaryProbAway = ifelse(CloseAwayTeam >= 100, 100/(CloseAwayTeam + 100),
                                   abs(CloseAwayTeam) / (100 + abs(CloseAwayTeam)))) 

nhl_2016 <- 
  nhl_2016 %>%
  mutate(BoundaryProb = ifelse(Close >= 100, 100/(Close + 100),
                               abs(Close) / (100 + abs(Close)))) 

nhl_2016_neat <-
  nhl_2016_neat %>%
  mutate(BoundaryProbHome2 = BoundaryProbHome / (BoundaryProbHome + BoundaryProbAway),
         BoundaryProbAway2 = BoundaryProbAway / (BoundaryProbAway + BoundaryProbHome))

# making model matrix for home team effect
h116 = model.matrix(~-1+nhl_2016_neat$HomeTeam)
a116 = model.matrix(~-1+nhl_2016_neat$AwayTeam)
h1a116 = h116-a116

# linear model
nhl_2016_neat <-
  nhl_2016_neat %>%
  mutate(DeadlineDays = as.numeric(DeadlineDays))

homeandaway_lm16 <- lm(BoundaryProbHome2 ~ h1a116 + DeadlineInd + DeadlineDays + h1a116 * DeadlineInd + h1a116 * DeadlineDays + h1a116 * DeadlineInd * DeadlineDays,
                       data = nhl_2016_neat)

summary(homeandaway_lm16)

plot(homeandaway_lm16)

# Calculating who won/lost trade deadline: Linear Model
tidy_coef16 <- tidy(homeandaway_lm16)

# transposing df
tidy_coef16 <- t(tidy_coef16)
tidy_coef16 <- as.data.frame(tidy_coef16)

# tidying names
names(tidy_coef16) <- tidy_coef16[1,]
tidy_coef16 <- tidy_coef16[-1,]

# taking out sd and p-value
tidy_coef16 <- tidy_coef16[-c(2,3,4),]

# split by coefficients
hometeams <- tidy_coef16 %>%
  select(1:31)
deadlineind <- tidy_coef16 %>%
  select(DeadlineInd, 34:63)
deadlinedays <- tidy_coef16 %>%
  select(DeadlineDays, 64:93)
interaction <- tidy_coef16 %>%
  select(94:124)

# making data longer
hometeams2 <- 
  gather(hometeams, Team, intercept, 2:31)
deadlineind2 <- 
  gather(deadlineind, Team, deadline_indicator, 2:31)
deadlinedays2 <- 
  gather(deadlinedays, Team, deadline_days, 2:31)
interaction2 <- 
  gather(interaction, Team, deadline_interaction, 2:31)

# binding data set together
tidy_coef16_2 <- cbind(hometeams2, deadlineind2, deadlinedays2, interaction2)

# setting NAs to 0
tidy_coef16_2[is.na(tidy_coef16_2)] <- 0

# tidying
tidy_coef16_clean <- tidy_coef16_2[,-c(5,8,11)]
names(tidy_coef16_clean)[1] <- "main_intercept"

tidy_coef16_clean<-
  tidy_coef16_clean %>%
  mutate(
    main_intercept = as.numeric(main_intercept),
    intercept = as.numeric(intercept),
    DeadlineInd = as.numeric(DeadlineInd),
    deadline_indicator = as.numeric(deadline_indicator),
    DeadlineDays = as.numeric(DeadlineDays),
    deadline_days = as.numeric(deadline_days),
    `DeadlineInd:DeadlineDays` = as.numeric(`DeadlineInd:DeadlineDays`),
    deadline_interaction = as.numeric(deadline_interaction),
  )

tidy_coef16_fin <-
  tidy_coef16_clean %>%
  mutate(
    beforedeadline = main_intercept + intercept,
    atdeadline = main_intercept + intercept + (DeadlineInd + deadline_indicator),
    predictedend = main_intercept + intercept + (DeadlineInd + deadline_indicator) + 40 * (DeadlineDays + deadline_days + `DeadlineInd:DeadlineDays` + deadline_interaction),
    firstlinepredictedend = main_intercept + intercept + 40 * (DeadlineDays + deadline_days),
    diffat0 = atdeadline - beforedeadline,
    diffat40 = predictedend - firstlinepredictedend
  )

# saving data frame of coefficients and neat data
write.csv(tidy_coef16_fin, "data/coefs2016.csv")
write.csv(nhl_2016_neat, "data/nhl_2016_neat.csv")

# creating a "model" dataset including home team effect and Boundary Prob
h1a1_df16 <- as.data.frame(h1a116)
nhl_2016_neat <- 
  nhl_2016_neat %>%
  mutate(ID = row_number())
h1a1_df16 <-
  h1a1_df16 %>%
  mutate(ID = row_number())

model_subset16 <-
  nhl_2016_neat %>%
  select("BoundaryProbHome2", "BoundaryProbAway2", 
         "HomeTeam", 
         "AwayTeam",
         "DeadlineInd", 
         "DeadlineDays", "ID")

model_subset16 <- left_join(model_subset16, h1a1_df16,
                          by = "ID")

model_subset16 <- subset(model_subset16, select = -ID)

model_subset16 <-
  model_subset16 %>%
  mutate(DeadlineDays = as.numeric(DeadlineDays))

model_subset16 <-
  model_subset16 %>%
  rename("Anaheim" = `nhl_2016_neat$HomeTeamAnaheim`,
         "Arizona" = `nhl_2016_neat$HomeTeamArizona`,
         "Boston" =`nhl_2016_neat$HomeTeamBoston`,
         "Buffalo" = `nhl_2016_neat$HomeTeamBuffalo`,
         "Calgary" = `nhl_2016_neat$HomeTeamCalgary`,
         "Carolina" = `nhl_2016_neat$HomeTeamCarolina`,
         "Chicago" = `nhl_2016_neat$HomeTeamChicago`,
         "Colorado" = `nhl_2016_neat$HomeTeamColorado`,
         "Columbus" = `nhl_2016_neat$HomeTeamColumbus`,
         "Dallas" = `nhl_2016_neat$HomeTeamDallas`,
         "Detroit" = `nhl_2016_neat$HomeTeamDetroit`,
         "Edmonton" = `nhl_2016_neat$HomeTeamEdmonton`,
         "Florida" = `nhl_2016_neat$HomeTeamFlorida`,
         "LosAngeles" = `nhl_2016_neat$HomeTeamLosAngeles`,
         "Minnesota" = `nhl_2016_neat$HomeTeamMinnesota`,
         "Montreal" = `nhl_2016_neat$HomeTeamMontreal`,
         "Nashville" = `nhl_2016_neat$HomeTeamNashville`,
         "NewJersey" =`nhl_2016_neat$HomeTeamNewJersey`,
         "NYIslanders" = `nhl_2016_neat$HomeTeamNYIslanders`,
         "NYRangers" = `nhl_2016_neat$HomeTeamNYRangers`,
         "Ottawa" = `nhl_2016_neat$HomeTeamOttawa`,
         "Philadelphia" = `nhl_2016_neat$HomeTeamPhiladelphia`,
         "Pittsburgh" = `nhl_2016_neat$HomeTeamPittsburgh`,
         "SanJose" = `nhl_2016_neat$HomeTeamSanJose`,
         "St.Louis" = `nhl_2016_neat$HomeTeamSt.Louis`,
         "TampaBay" = `nhl_2016_neat$HomeTeamTampaBay`,
         "Toronto" = `nhl_2016_neat$HomeTeamToronto`,
         "Vancouver" = `nhl_2016_neat$HomeTeamVancouver`,
         "Washington" = `nhl_2016_neat$HomeTeamWashington`,
         "Winnipeg" = `nhl_2016_neat$HomeTeamWinnipeg`
  )

write.csv(model_subset16, "data/model_subset16.csv")


## 2017-2018
# loading data
nhl_2017 <- readxl::read_xlsx("data/nhl odds 2017-18.xlsx")

# trade deadline and date indicator variables
nhl_2017 <-
  nhl_2017 %>%
  mutate(Deadline = as.numeric(Date %in% c(226:607)),
         Year = as.numeric(Date %in% c(101:607)))

# adding date variable
nhl_2017 <-
  nhl_2017 %>%
  mutate(Date = ifelse(Year == 0, paste(2017, Date, sep = ""),
                       paste(2018, Date, sep = "0")))

# convert date into a "date"
nhl_2017 <-
  nhl_2017 %>%
  mutate(Date = lubridate::ymd(Date))

# taking out playoff games
nhl_2017 <-
  nhl_2017 %>%
  filter(Date < "2018-4-11")

# making ID variable for game
nhl_2017$ID <- seq.int(nrow(nhl_2017))
nhl_2017 <-
  nhl_2017 %>%
  mutate(ID = as.integer(ID),
         ID = ID / 2,
         ID = ceiling(ID))

# indicator variable for days since trade deadline
nhl_2017 <-
  nhl_2017 %>%
  mutate(Deadline2 = Date - lubridate::ymd(20180226))

# make data wider - each row is one game
nhl_2017_home <- nhl_2017 %>%
  filter(VH == "H")
nhl_2017_away <- nhl_2017 %>%
  filter(VH == "V")
nhl_2017_wide<-bind_cols(nhl_2017_home,nhl_2017_away)

# tidy wider data
nhl_2017_wide <- 
  nhl_2017_wide[, c(1, 2, 22, 3, 4, 24, 5,
                    25, 6, 26, 7, 27, 8, 28,
                    9, 29, 10, 30, 11, 31, 12,
                    32, 13, 33, 14, 34, 15, 35,
                    16, 36, 17, 18, 19, 20, 21,
                    23, 37, 38, 39, 40)]

nhl_2017_neat <- 
  nhl_2017_wide[-c(4, 24, 27, 35:40)]

nhl_2017_neat <-
  nhl_2017_neat %>%
  rename("Date" = Date...1,
         "Rot Home Team" = Rot...2,
         "HomeTeam" =Team...4,
         "1st Home Team Goals" = `1st...5`,
         "2nd Home Team Goals" = `2nd...6`,
         "3rd Home Team Goals" = `3rd...7`,
         "FinalHome" = Final...8,
         "Open Home Team" = Open...9,
         "CloseHomeTeam" = Close...10,
         "Puck Line Home Team" = `Puck Line...11`,
         "Open OU" = `Open OU...13`,
         "Close OU" = `Close OU...35`,
         "DeadlineInd" = Deadline...17,
         "Year" = Year...18,
         "Game ID" = ID...19,
         "DeadlineDays" = Deadline2...20,
         "Rot Away Team" = Rot...22,
         "AwayTeam" =Team...24,
         "1st Away Team Goals" = `1st...25`,
         "2nd Away Team Goals" = `2nd...26`,
         "3rd Away Team Goals" = `3rd...27`,
         "FinalAway" = Final...28,
         "Open Away Team" = Open...29,
         "CloseAwayTeam" = Close...30,
         "Puck Line Away Team" = `Puck Line...31`,
  )

# Market Probabilities
nhl_2017_neat <-
  nhl_2017_neat %>%
  mutate(BoundaryProbHome = ifelse(CloseHomeTeam >= 100, 100/(CloseHomeTeam + 100), abs(CloseHomeTeam) / (100 + abs(CloseHomeTeam)))) 

nhl_2017_neat <-
  nhl_2017_neat %>%
  mutate(BoundaryProbAway = ifelse(CloseAwayTeam >= 100, 100/(CloseAwayTeam + 100), abs(CloseAwayTeam) / (100 + abs(CloseAwayTeam)))) 

nhl_2017 <- 
  nhl_2017 %>%
  mutate(BoundaryProb = ifelse(Close >= 100, 100/(Close + 100), abs(Close) / (100 + abs(Close)))) 

nhl_2017_neat <-
  nhl_2017_neat %>%
  mutate(BoundaryProbHome2 = BoundaryProbHome / (BoundaryProbHome + BoundaryProbAway),
         BoundaryProbAway2 = BoundaryProbAway / (BoundaryProbAway + BoundaryProbHome))

# Making Model Matrix for Home Team Effect
h117 = model.matrix(~-1+nhl_2017_neat$HomeTeam)
a117 = model.matrix(~-1+nhl_2017_neat$AwayTeam)
h1a117 = h117-a117

# Linear Model
nhl_2017_neat <-
  nhl_2017_neat %>%
  mutate(DeadlineDays = as.numeric(DeadlineDays))

homeandaway_lm17 <- lm(BoundaryProbHome2 ~ h1a117 + DeadlineInd + DeadlineDays + h1a117 * DeadlineInd + h1a117 * DeadlineDays + h1a117 * DeadlineInd * DeadlineDays,
                       data = nhl_2017_neat)

summary(homeandaway_lm17)

plot(homeandaway_lm17)

## Calculating who won/lost trade deadline: Linear Model
tidy_coef17 <- tidy(homeandaway_lm17)

# transposing df
tidy_coef17 <- t(tidy_coef17)
tidy_coef17 <- as.data.frame(tidy_coef17)

# tidying names
names(tidy_coef17) <- tidy_coef17[1,]
tidy_coef17 <- tidy_coef17[-1,]

# taking out sd and p-value
tidy_coef17 <- tidy_coef17[-c(2,3,4),]

# split by coefficients
hometeams <- tidy_coef17 %>%
  select(1:32)
deadlineind <- tidy_coef17 %>%
  select(DeadlineInd, 35:65)
deadlinedays <- tidy_coef17 %>%
  select(DeadlineDays, 66:96)
interaction <- tidy_coef17 %>%
  select(97:128)

# making data longer
hometeams2 <- gather(hometeams, Team, intercept, 2:32)
deadlineind2 <- gather(deadlineind, Team, deadline_indicator, 2:32)
deadlinedays2 <- gather(deadlinedays, Team, deadline_days, 2:32)
interaction2 <- gather(interaction, Team, deadline_interaction, 2:32)

# binding data set together
tidy_coef17_2 <- cbind(hometeams2, deadlineind2, deadlinedays2, interaction2)

# setting NAs to 0
tidy_coef17_2[is.na(tidy_coef17_2)] <- 0

# tidying
tidy_coef17_clean <- tidy_coef17_2[,-c(5,8,11)]
names(tidy_coef17_clean)[1] <- "main_intercept"

tidy_coef17_clean<-
  tidy_coef17_clean %>%
  mutate(
    main_intercept = as.numeric(main_intercept),
    intercept = as.numeric(intercept),
    DeadlineInd = as.numeric(DeadlineInd),
    deadline_indicator = as.numeric(deadline_indicator),
    DeadlineDays = as.numeric(DeadlineDays),
    deadline_days = as.numeric(deadline_days),
    `DeadlineInd:DeadlineDays` = as.numeric(`DeadlineInd:DeadlineDays`),
    deadline_interaction = as.numeric(deadline_interaction),
  )

tidy_coef17_fin <-
  tidy_coef17_clean %>%
  mutate(
    beforedeadline = main_intercept + intercept,
    atdeadline = main_intercept + intercept + (DeadlineInd + deadline_indicator),
    predictedend = main_intercept + intercept + (DeadlineInd + deadline_indicator) + 40 * (DeadlineDays + deadline_days + `DeadlineInd:DeadlineDays` + deadline_interaction),
    firstlinepredictedend = main_intercept + intercept + 40 * (DeadlineDays + deadline_days),
    diffat0 = atdeadline - beforedeadline,
    diffat40 = predictedend - firstlinepredictedend
  )


# saving data frame of coefficients
write.csv(tidy_coef17_fin, "data/coefs2017.csv")
write.csv(nhl_2017_neat, "data/nhl_2017_neat.csv")


# creating a "model" dataset including home team effect and Boundary Prob
h1a1_df <- as.data.frame(h1a117)
nhl_2017_neat <- 
  nhl_2017_neat %>%
  mutate(ID = row_number())
h1a1_df <-
  h1a1_df %>%
  mutate(ID = row_number())

model_subset <-
  nhl_2017_neat %>%
  select("BoundaryProbHome2", "BoundaryProbAway2", 
         "HomeTeam", 
         "AwayTeam",
         "DeadlineInd", 
         "DeadlineDays", "ID")

model_subset <- left_join(model_subset, h1a1_df,
                          by = "ID")

model_subset <- subset(model_subset, select = -ID)

model_subset <-
  model_subset %>%
  mutate(DeadlineDays = as.numeric(DeadlineDays))

model_subset17 <-
  model_subset %>%
  rename("Anaheim" = `nhl_2017_neat$HomeTeamAnaheim`,
         "Arizona" = `nhl_2017_neat$HomeTeamArizona`,
         "Boston" =`nhl_2017_neat$HomeTeamBoston`,
         "Buffalo" = `nhl_2017_neat$HomeTeamBuffalo`,
         "Calgary" = `nhl_2017_neat$HomeTeamCalgary`,
         "Carolina" = `nhl_2017_neat$HomeTeamCarolina`,
         "Chicago" = `nhl_2017_neat$HomeTeamChicago`,
         "Colorado" = `nhl_2017_neat$HomeTeamColorado`,
         "Columbus" = `nhl_2017_neat$HomeTeamColumbus`,
         "Dallas" = `nhl_2017_neat$HomeTeamDallas`,
         "Detroit" = `nhl_2017_neat$HomeTeamDetroit`,
         "Edmonton" = `nhl_2017_neat$HomeTeamEdmonton`,
         "Florida" = `nhl_2017_neat$HomeTeamFlorida`,
         "LosAngeles" = `nhl_2017_neat$HomeTeamLosAngeles`,
         "Minnesota" = `nhl_2017_neat$HomeTeamMinnesota`,
         "Montreal" = `nhl_2017_neat$HomeTeamMontreal`,
         "Nashville" = `nhl_2017_neat$HomeTeamNashville`,
         "NewJersey" =`nhl_2017_neat$HomeTeamNewJersey`,
         "NYIslanders" = `nhl_2017_neat$HomeTeamNYIslanders`,
         "NYRangers" = `nhl_2017_neat$HomeTeamNYRangers`,
         "Ottawa" = `nhl_2017_neat$HomeTeamOttawa`,
         "Philadelphia" = `nhl_2017_neat$HomeTeamPhiladelphia`,
         "Pittsburgh" = `nhl_2017_neat$HomeTeamPittsburgh`,
         "SanJose" = `nhl_2017_neat$HomeTeamSanJose`,
         "St.Louis" = `nhl_2017_neat$HomeTeamSt.Louis`,
         "TampaBay" = `nhl_2017_neat$HomeTeamTampaBay`,
         "Toronto" = `nhl_2017_neat$HomeTeamToronto`,
         "Vancouver" = `nhl_2017_neat$HomeTeamVancouver`,
         "Vegas" = `nhl_2017_neat$HomeTeamVegas`,
         "Washington" = `nhl_2017_neat$HomeTeamWashington`,
         "Winnipeg" = `nhl_2017_neat$HomeTeamWinnipeg`
  )

write.csv(model_subset17, "data/model_subset.csv")



