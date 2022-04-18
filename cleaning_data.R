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

# indicator variable for whether a team played the day before
nhl_2016 <-
  nhl_2016 %>%
  group_by(Team) %>%
  mutate(Game = (c(1, diff(Date) > 1)))

# make data wider - each row is one game
nhl_2016_home<- nhl_2016 %>%
  filter(VH=="H")
nhl_2016_away<-nhl_2016 %>%
  filter(VH=="V")
nhl_2016_wide<-bind_cols(nhl_2016_home,nhl_2016_away)

# tidy wider data
nhl_2016_wide <- 
  nhl_2016_wide[, c(1, 2, 23, 21, 3, 4, 
                    25, 5, 26, 6, 27, 7, 
                    28, 8, 29, 9, 30, 10, 
                    31, 11, 32, 12, 33, 13, 
                    34, 14, 35, 15, 36, 16, 
                    37, 17, 18, 19, 20, 22, 
                    24, 38, 39, 40, 41, 42)]

nhl_2016_neat <- 
  nhl_2016_wide[-c(5, 25, 28, 36:41)]

nhl_2016_neat <-
  nhl_2016_neat %>%
  rename("Date" = Date...1,
         "Rot Home Team" = Rot...2,
         "Back2Back Home" = Game...21,
         "HomeTeam" =Team...4,
         "1st Home Team Goals" = `1st...5`,
         "2nd Home Team Goals" = `2nd...6`,
         "3rd Home Team Goals" = `3rd...7`,
         "FinalHome" = Final...8,
         "Open Home Team" = Open...9,
         "CloseHomeTeam" = Close...10,
         "Puck Line Home Team" = `Puck Line...11`,
         "Open OU" = `Open OU...13`,
         "Close OU" = `Close OU...36`,
         "DeadlineInd" = Deadline...17,
         "Year" = Year...18,
         "Game ID" = ID...19,
         "DeadlineDays" = Deadline2...20,
         "Rot Away Team" = Rot...23,
         "AwayTeam" =Team...25,
         "1st Away Team Goals" = `1st...26`,
         "2nd Away Team Goals" = `2nd...27`,
         "3rd Away Team Goals" = `3rd...28`,
         "FinalAway" = Final...29,
         "Open Away Team" = Open...30,
         "CloseAwayTeam" = Close...31,
         "Puck Line Away Team" = `Puck Line...32`,
         "Back2Back Away" = Game...42
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

# fixing back to back
nhl_2016_neat <-
  nhl_2016_neat %>%
  mutate(Back2BackHome = ifelse(`Back2Back Home` == 0, 1, 0),
         Back2BackAway = ifelse(`Back2Back Away` == 0, 1, 0))

# making model matrix for home team effect
h116 = model.matrix(~-1+nhl_2016_neat$HomeTeam)
a116 = model.matrix(~-1+nhl_2016_neat$AwayTeam)
h1a116 = h116-a116
h1a1_df16 <- as.data.frame(h1a116)
h1a1_df16 <-
  h1a1_df16 %>%
  mutate(ID = row_number())

# linear model
nhl_2016_neat <-
  nhl_2016_neat %>%
  mutate(DeadlineDays = as.numeric(DeadlineDays))

homeandaway_lm16 <- lm(BoundaryProbHome2 ~ h1a116 + 
                         DeadlineInd + DeadlineDays +
                         Back2BackHome + Back2BackAway +
                         h1a116 * DeadlineInd + 
                         h1a116 * DeadlineDays + 
                         h1a116 * DeadlineInd * DeadlineDays,
                       data = nhl_2016_neat)

summary(homeandaway_lm16)

plot(homeandaway_lm16)

# Calculating who won/lost trade deadline: Lasso
tidy_coef16 <- tidy(homeandaway_lm16)
# constructing lasso model
nhl_2016_neat <- 
  nhl_2016_neat %>%
  mutate(ID = row_number())

model_subset16 <-
  nhl_2016_neat %>%
  select("BoundaryProbHome2", "DeadlineInd",
         "DeadlineDays", "ID", "Back2BackHome",
         "Back2BackAway")

model_subset16 <-
  left_join(model_subset16, h1a1_df16, by = "ID")

model_subset16 <- subset(model_subset16, select = -ID)

# using model.matrix() function
model_y16 <-
  model_subset16$BoundaryProbHome2

model_x16 <- model.matrix(BoundaryProbHome2 ~ 
                            h1a116 + DeadlineInd + 
                            DeadlineDays + Back2BackHome +
                            Back2BackAway +
                            h1a116 * DeadlineInd + 
                            h1a116 * DeadlineDays + 
                            h1a116 * DeadlineInd * DeadlineDays,
                          data = model_subset16)[,-1]

# Lasso regression
fit_lasso_cv16 <- cv.glmnet(model_x16, model_y16, 
                            alpha = 1)

# finding who won/lost the trade deadline LASSO
tidy_coeffs16 <- coef(fit_lasso_cv16, s = "lambda.1se")
tidy_lasso_coef16 <- data.frame(name = tidy_coeffs16@Dimnames[[1]][tidy_coeffs16@i + 1], 
                                coefficient = tidy_coeffs16@x)

# cleaning to get coefficients
tidy_lasso_coef16 <- 
  rename(tidy_lasso_coef16, term = name)

tidy_lasso_coef16_tot <- left_join(tidy_coef16, tidy_lasso_coef16, by = "term")
tidy_lasso_coef16_2 <- tidy_lasso_coef16_tot[-c(2:5)] 
tidy_lasso_coef16_2[is.na(tidy_lasso_coef16_2)] = 0

# transposing df
tidy_lasso_coef16_2 <- t(tidy_lasso_coef16_2)
tidy_lasso_coef16_2 <- as.data.frame(tidy_lasso_coef16_2)

# tidying names
names(tidy_lasso_coef16_2) <- tidy_lasso_coef16_2[1,]
tidy_lasso_coef16_2 <- tidy_lasso_coef16_2[-1,]

# split by coefficients
hometeams <- tidy_lasso_coef16_2 %>%
  select(1:31)
deadlineind <- tidy_lasso_coef16_2 %>%
  select(DeadlineInd, 36:65)
deadlinedays <- tidy_lasso_coef16_2 %>%
  select(DeadlineDays, 66:95)
interaction <- tidy_lasso_coef16_2 %>%
  select(96:126)

# making data longer
hometeams2 <- gather(hometeams, Team, intercept, 2:31)
deadlineind2 <- gather(deadlineind, Team, deadline_indicator, 2:31)
deadlinedays2 <- gather(deadlinedays, Team, deadline_days, 2:31)
interaction2 <- gather(interaction, Team, deadline_interaction, 2:31)

# binding data set together
tidy_lasso_coef16_3 <- cbind(hometeams2, deadlineind2, deadlinedays2, interaction2)
tidy_lasso_coef16_3$Back2BackHome = tidy_lasso_coef16_2$Back2BackHome
tidy_lasso_coef16_3$Back2BackAway = tidy_lasso_coef16_2$Back2BackAway


# setting NAs to 0
tidy_lasso_coef16_3[is.na(tidy_lasso_coef16_3)] <- 0

# tidying
tidy_lasso_coef16_clean <- tidy_lasso_coef16_3[,-c(5,8,11)]
names(tidy_lasso_coef16_clean)[1] <- "main_intercept"

tidy_lasso_coef16_clean<-
  tidy_lasso_coef16_clean %>%
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

tidy_lasso_coef16_fin <-
  tidy_lasso_coef16_clean %>%
  mutate(
    beforedeadline = main_intercept + intercept,
    atdeadline = main_intercept + intercept + (DeadlineInd + deadline_indicator),
    predictedend = main_intercept + intercept + (DeadlineInd + deadline_indicator) + 40 * (DeadlineDays + deadline_days + `DeadlineInd:DeadlineDays` + deadline_interaction),
    firstlinepredictedend = main_intercept + intercept + 40 * (DeadlineDays + deadline_days),
    diffat0 = atdeadline - beforedeadline,
    diffat40 = predictedend - firstlinepredictedend
  )


# saving data frame of coefficients and neat data
write.csv(tidy_lasso_coef16_fin, "data/coefs2016.csv")
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


# Need to add Vegas, which will just be 0
model_subset16 <-
  model_subset16 %>%
  mutate(Vegas = 0)
model_subset16 = model_subset16[,c(1:34, 37, 35, 36)]


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

# indicator variable for whether a team played the day before
nhl_2017 <-
  nhl_2017 %>%
  group_by(Team) %>%
  mutate(Game = (c(1, diff(Date) > 1)))

# make data wider - each row is one game
nhl_2017_home <- nhl_2017 %>%
  filter(VH == "H")
nhl_2017_away <- nhl_2017 %>%
  filter(VH == "V")
nhl_2017_wide<-bind_cols(nhl_2017_home,nhl_2017_away)

# tidy wider data
nhl_2017_wide <- 
  nhl_2017_wide[, c(1, 2, 23, 21, 3, 
                    4, 25, 5, 26, 6, 
                    27, 7, 28, 8, 29, 
                    9, 30, 10, 31, 11, 
                    32, 12, 33, 13, 34, 
                    14, 35, 15, 36, 16, 
                    37, 17, 18, 19, 20, 
                    22, 24, 38, 39, 40, 
                    41, 42)]

nhl_2017_neat <- 
  nhl_2017_wide[-c(5, 25, 28, 36:41)]

nhl_2017_neat <-
  nhl_2017_neat %>%
  rename("Date" = Date...1,
         "Rot Home Team" = Rot...2,
         "Back2Back Home" = Game...21,
         "HomeTeam" =Team...4,
         "1st Home Team Goals" = `1st...5`,
         "2nd Home Team Goals" = `2nd...6`,
         "3rd Home Team Goals" = `3rd...7`,
         "FinalHome" = Final...8,
         "Open Home Team" = Open...9,
         "CloseHomeTeam" = Close...10,
         "Puck Line Home Team" = `Puck Line...11`,
         "Open OU" = `Open OU...13`,
         "Close OU" = `Close OU...36`,
         "DeadlineInd" = Deadline...17,
         "Year" = Year...18,
         "Game ID" = ID...19,
         "DeadlineDays" = Deadline2...20,
         "Rot Away Team" = Rot...23,
         "AwayTeam" =Team...25,
         "1st Away Team Goals" = `1st...26`,
         "2nd Away Team Goals" = `2nd...27`,
         "3rd Away Team Goals" = `3rd...28`,
         "FinalAway" = Final...29,
         "Open Away Team" = Open...30,
         "CloseAwayTeam" = Close...31,
         "Puck Line Away Team" = `Puck Line...32`,
         "Back2Back Away" = Game...42
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

# fixing back to back
nhl_2017_neat <-
  nhl_2017_neat %>%
  mutate(Back2BackHome = ifelse(`Back2Back Home` == 0, 1, 0),
         Back2BackAway = ifelse(`Back2Back Away` == 0, 1, 0))

# Making Model Matrix for Home Team Effect
h117 = model.matrix(~-1+nhl_2017_neat$HomeTeam)
a117 = model.matrix(~-1+nhl_2017_neat$AwayTeam)
h1a117 = h117-a117
h1a1_df17 <- as.data.frame(h1a117)
h1a1_df17 <-
  h1a1_df17 %>%
  mutate(ID = row_number())

# Linear Model
nhl_2017_neat <-
  nhl_2017_neat %>%
  mutate(DeadlineDays = as.numeric(DeadlineDays))

homeandaway_lm17 <- lm(BoundaryProbHome2 ~ h1a117 + DeadlineInd + 
                         DeadlineDays + Back2BackHome +
                         Back2BackAway + h1a117 * DeadlineInd + 
                         h1a117 * DeadlineDays + 
                         h1a117 * DeadlineInd * DeadlineDays,
                       data = nhl_2017_neat)

summary(homeandaway_lm17)

plot(homeandaway_lm17)

## Calculating who won/lost trade deadline: Lasso Model
tidy_coef17 <- tidy(homeandaway_lm17)

# constructing lasso model
nhl_2017_neat <- 
  nhl_2017_neat %>%
  mutate(ID = row_number())

model_subset17 <-
  nhl_2017_neat %>%
  select("BoundaryProbHome2", "DeadlineInd",
         "DeadlineDays", "ID", "Back2BackHome",
         "Back2BackAway")

model_subset17 <-
  left_join(model_subset17, h1a1_df17, by = "ID")

model_subset17 <- subset(model_subset17, select = -ID)

# using model.matrix() function
model_y17 <-
  model_subset17$BoundaryProbHome2

model_x17 <- model.matrix(BoundaryProbHome2 ~ 
                            h1a117 + DeadlineInd + 
                            DeadlineDays + 
                            h1a117 * DeadlineInd + 
                            h1a117 * DeadlineDays + 
                            h1a117 * DeadlineInd * DeadlineDays +
                            Back2BackHome + Back2BackAway,
                          data = model_subset17)[,-1]

# Lasso regression
fit_lasso_cv17 <- cv.glmnet(model_x17, model_y17, 
                          alpha = 1)

# finding who won/lost the trade deadline LASSO
tidy_coeffs17 <- coef(fit_lasso_cv17, s = "lambda.1se")
tidy_lasso_coef17 <- data.frame(name = tidy_coeffs17@Dimnames[[1]][tidy_coeffs17@i + 1], 
                                coefficient = tidy_coeffs17@x)

tidy_lasso_coef17 <- rename(tidy_lasso_coef17, term = name)

tidy_lasso_coef17_tot <- left_join(tidy_coef17, tidy_lasso_coef17, by = "term")
tidy_lasso_coef17_2 <- tidy_lasso_coef17_tot[-c(2:5)] 
tidy_lasso_coef17_2[is.na(tidy_lasso_coef17_2)] = 0

# transposing df
tidy_lasso_coef17_2 <- t(tidy_lasso_coef17_2)
tidy_lasso_coef17_2 <- as.data.frame(tidy_lasso_coef17_2)

# tidying names
names(tidy_lasso_coef17_2) <- tidy_lasso_coef17_2[1,]
tidy_lasso_coef17_2 <- tidy_lasso_coef17_2[-1,]

# split by coefficients
hometeams <- tidy_lasso_coef17_2 %>%
  select(1:32)
deadlineind <- tidy_lasso_coef17_2 %>%
  select(DeadlineInd, 37:67)
deadlinedays <- tidy_lasso_coef17_2 %>%
  select(DeadlineDays, 68:98)
interaction <- tidy_lasso_coef17_2 %>%
  select(99:130)

# making data longer
hometeams2 <- gather(hometeams, Team, intercept, 2:32)
deadlineind2 <- gather(deadlineind, Team, deadline_indicator, 2:32)
deadlinedays2 <- gather(deadlinedays, Team, deadline_days, 2:32)
interaction2 <- gather(interaction, Team, deadline_interaction, 2:32)

# binding data set together
tidy_lasso_coef17_3 <- cbind(hometeams2, deadlineind2, deadlinedays2, interaction2)
tidy_lasso_coef17_3$Back2BackHome <- tidy_lasso_coef17_2$Back2BackHome
tidy_lasso_coef17_3$Back2BackAway <- tidy_lasso_coef17_2$Back2BackAway

# setting NAs to 0
tidy_lasso_coef17_3[is.na(tidy_lasso_coef17_3)] <- 0

# tidying
tidy_lasso_coef17_clean <- tidy_lasso_coef17_3[,-c(5,8,11)]
names(tidy_lasso_coef17_clean)[1] <- "main_intercept"

tidy_lasso_coef17_clean<-
  tidy_lasso_coef17_clean %>%
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

tidy_lasso_coef17_fin <-
  tidy_lasso_coef17_clean %>%
  mutate(
    beforedeadline = main_intercept + intercept,
    atdeadline = main_intercept + intercept + (DeadlineInd + deadline_indicator),
    predictedend = main_intercept + intercept + (DeadlineInd + deadline_indicator) + 40 * (DeadlineDays + deadline_days + `DeadlineInd:DeadlineDays` + deadline_interaction),
    firstlinepredictedend = main_intercept + intercept + 40 * (DeadlineDays + deadline_days),
    diffat0 = atdeadline - beforedeadline,
    diffat40 = predictedend - firstlinepredictedend
  )





# saving data frame of coefficients
write.csv(tidy_lasso_coef17_fin, "data/coefs2017.csv")
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

## 2018-2019
# loading data
nhl_2018 <- readxl::read_xlsx("data/nhl odds 2018-19 (1).xlsx")

# trade deadline and date indicator variables
nhl_2018 <-
  nhl_2018 %>%
  mutate(Deadline = as.numeric(Date %in% c(225:607)),
         Year = as.numeric(Date %in% c(101:607)))

# adding date variable
nhl_2018 <-
  nhl_2018 %>%
  mutate(Date = ifelse(Year == 0, paste(2018, Date, sep = ""),
                       paste(2019, Date, sep = "0")))

# convert date into a "date"
nhl_2018 <-
  nhl_2018 %>%
  mutate(Date = lubridate::ymd(Date))

# taking out playoff games
nhl_2018 <-
  nhl_2018 %>%
  filter(Date < "2019-4-10")

# making ID variable for game
nhl_2018$ID <- seq.int(nrow(nhl_2018))
nhl_2018 <-
  nhl_2018 %>%
  mutate(ID = as.integer(ID),
         ID = ID / 2,
         ID = ceiling(ID))

# indicator variable for days since trade deadline
nhl_2018 <-
  nhl_2018 %>%
  mutate(Deadline2 = Date - lubridate::ymd(20190225))

# indicator variable for whether a team played the day before
nhl_2018 <-
  nhl_2018 %>%
  group_by(Team) %>%
  mutate(Game = (c(1, diff(Date) > 1)))

# make data wider - each row is one game
nhl_2018_home <- nhl_2018 %>%
  filter(VH == "H")
nhl_2018_away <- nhl_2018 %>%
  filter(VH == "V")
nhl_2018_wide<-bind_cols(nhl_2018_home,nhl_2018_away)

# tidy wider data
nhl_2018_wide <- 
  nhl_2018_wide[, c(1, 2, 23, 21, 3, 4, 
                    25, 5, 26, 6, 27, 7, 
                    28, 8, 29, 9, 30, 10, 
                    31, 11, 32, 12, 33, 13, 
                    34, 14, 35, 15, 36, 16, 
                    37, 17, 18, 19, 20, 22, 
                    24, 38, 39, 40, 41, 42)]

nhl_2018_neat <- 
  nhl_2018_wide[-c(5, 25, 28, 36:41)]

nhl_2018_neat <-
  nhl_2018_neat %>%
  rename("Date" = Date...1,
         "Rot Home Team" = Rot...2,
         "Back2Back Home" = Game...21,
         "HomeTeam" =Team...4,
         "1st Home Team Goals" = `1st...5`,
         "2nd Home Team Goals" = `2nd...6`,
         "3rd Home Team Goals" = `3rd...7`,
         "FinalHome" = Final...8,
         "Open Home Team" = Open...9,
         "CloseHomeTeam" = Close...10,
         "Puck Line Home Team" = `Puck Line...11`,
         "Open OU" = `Open OU...13`,
         "Close OU" = `Close OU...36`,
         "DeadlineInd" = Deadline...17,
         "Year" = Year...18,
         "Game ID" = ID...19,
         "DeadlineDays" = Deadline2...20,
         "Rot Away Team" = Rot...23,
         "AwayTeam" =Team...25,
         "1st Away Team Goals" = `1st...26`,
         "2nd Away Team Goals" = `2nd...27`,
         "3rd Away Team Goals" = `3rd...28`,
         "FinalAway" = Final...29,
         "Open Away Team" = Open...30,
         "CloseAwayTeam" = Close...31,
         "Puck Line Away Team" = `Puck Line...32`,
         "Back2Back Away" = Game...42
  )

# Market Probabilities
nhl_2018_neat <-
  nhl_2018_neat %>%
  mutate(BoundaryProbHome = ifelse(CloseHomeTeam >= 100, 100/(CloseHomeTeam + 100), abs(CloseHomeTeam) / (100 + abs(CloseHomeTeam)))) 

nhl_2018_neat <-
  nhl_2018_neat %>%
  mutate(BoundaryProbAway = ifelse(CloseAwayTeam >= 100, 100/(CloseAwayTeam + 100), abs(CloseAwayTeam) / (100 + abs(CloseAwayTeam)))) 

nhl_2018 <- 
  nhl_2018 %>%
  mutate(BoundaryProb = ifelse(Close >= 100, 100/(Close + 100), abs(Close) / (100 + abs(Close)))) 

nhl_2018_neat <-
  nhl_2018_neat %>%
  mutate(BoundaryProbHome2 = BoundaryProbHome / (BoundaryProbHome + BoundaryProbAway),
         BoundaryProbAway2 = BoundaryProbAway / (BoundaryProbAway + BoundaryProbHome))

nhl_2018_neat <-
  nhl_2018_neat %>%
  mutate(Back2BackHome = ifelse(`Back2Back Home` == 0, 1, 0),
         Back2BackAway = ifelse(`Back2Back Away` == 0, 1, 0))

# Making Model Matrix for Home Team Effect
h118 = model.matrix(~-1+nhl_2018_neat$HomeTeam)
a118 = model.matrix(~-1+nhl_2018_neat$AwayTeam)
h1a118 = h118-a118
h1a1_df18 <- as.data.frame(h1a118)
h1a1_df18 <-
  h1a1_df18 %>%
  mutate(ID = row_number())

# Linear Model
nhl_2018_neat <-
  nhl_2018_neat %>%
  mutate(DeadlineDays = as.numeric(DeadlineDays))

homeandaway_lm18 <- lm(BoundaryProbHome2 ~ h1a118 + DeadlineInd +
                       DeadlineDays + Back2BackHome + Back2BackAway +
                         h1a118 * DeadlineInd + 
                         h1a118 * DeadlineDays + 
                         h1a118 * DeadlineInd * DeadlineDays,
                       data = nhl_2018_neat)

summary(homeandaway_lm18)

plot(homeandaway_lm18)

## Calculating who won/lost trade deadline: Lasso Model
tidy_coef18 <- tidy(homeandaway_lm18)

# constructing lasso model
nhl_2018_neat <- 
  nhl_2018_neat %>%
  mutate(ID = row_number())

model_subset18 <-
  nhl_2018_neat %>%
  select("BoundaryProbHome2", "DeadlineInd",
         "DeadlineDays", "ID", "Back2BackHome",
         "Back2BackAway")

model_subset18 <-
  left_join(model_subset18, h1a1_df18, by = "ID")

model_subset18 <- subset(model_subset18, select = -ID)

# using model.matrix() function
model_y18 <-
  model_subset18$BoundaryProbHome2

model_x18 <- model.matrix(BoundaryProbHome2 ~ 
                            h1a118 + DeadlineInd + 
                            DeadlineDays + Back2BackHome +
                            Back2BackAway +
                            h1a118 * DeadlineInd + 
                            h1a118 * DeadlineDays + 
                            h1a118 * DeadlineInd * DeadlineDays,
                          data = model_subset18)[,-1]

# Lasso regression
fit_lasso_cv18 <- cv.glmnet(model_x18, model_y18, 
                            alpha = 1)

# finding who won/lost the trade deadline LASSO
tidy_coeffs18 <- coef(fit_lasso_cv18, s = "lambda.1se")
tidy_lasso_coef18 <- data.frame(name = tidy_coeffs18@Dimnames[[1]][tidy_coeffs18@i + 1], 
                                coefficient = tidy_coeffs18@x)

tidy_lasso_coef18 <- rename(tidy_lasso_coef18, term = name)

tidy_lasso_coef18_tot <- left_join(tidy_coef18, tidy_lasso_coef18, by = "term")
tidy_lasso_coef18_2 <- tidy_lasso_coef18_tot[-c(2:5)] 
tidy_lasso_coef18_2[is.na(tidy_lasso_coef18_2)] = 0

# transposing df
tidy_lasso_coef18_2 <- t(tidy_lasso_coef18_2)
tidy_lasso_coef18_2 <- as.data.frame(tidy_lasso_coef18_2)

# tidying names
names(tidy_lasso_coef18_2) <- tidy_lasso_coef18_2[1,]
tidy_lasso_coef18_2 <- tidy_lasso_coef18_2[-1,]

# split by coefficients
hometeams <- tidy_lasso_coef18_2 %>%
  select(1:32)
deadlineind <- tidy_lasso_coef18_2 %>%
  select(DeadlineInd, 37:67)
deadlinedays <- tidy_lasso_coef18_2 %>%
  select(DeadlineDays, 68:98)
interaction <- tidy_lasso_coef18_2 %>%
  select(99:130)

# making data longer
hometeams2 <- gather(hometeams, Team, intercept, 2:32)
deadlineind2 <- gather(deadlineind, Team, deadline_indicator, 2:32)
deadlinedays2 <- gather(deadlinedays, Team, deadline_days, 2:32)
interaction2 <- gather(interaction, Team, deadline_interaction, 2:32)

# binding data set together
tidy_lasso_coef18_3 <- cbind(hometeams2, deadlineind2, deadlinedays2, interaction2)
tidy_lasso_coef18_3$Back2BackHome = tidy_lasso_coef18_2$Back2BackHome
tidy_lasso_coef18_3$Back2BackAway = tidy_lasso_coef18_2$Back2BackAway


# setting NAs to 0
tidy_lasso_coef18_3[is.na(tidy_lasso_coef18_3)] <- 0

# tidying
tidy_lasso_coef18_clean <- tidy_lasso_coef18_3[,-c(5,8,11)]
names(tidy_lasso_coef18_clean)[1] <- "main_intercept"

tidy_lasso_coef18_clean<-
  tidy_lasso_coef18_clean %>%
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

tidy_lasso_coef18_fin <-
  tidy_lasso_coef18_clean %>%
  mutate(
    beforedeadline = main_intercept + intercept,
    atdeadline = main_intercept + intercept + (DeadlineInd + deadline_indicator),
    predictedend = main_intercept + intercept + (DeadlineInd + deadline_indicator) + 40 * (DeadlineDays + deadline_days + `DeadlineInd:DeadlineDays` + deadline_interaction),
    firstlinepredictedend = main_intercept + intercept + 40 * (DeadlineDays + deadline_days),
    diffat0 = atdeadline - beforedeadline,
    diffat40 = predictedend - firstlinepredictedend
  )





# saving data frame of coefficients
write.csv(tidy_lasso_coef18_fin, "data/coefs2018.csv")
write.csv(nhl_2018_neat, "data/nhl_2018_neat.csv")


# creating a "model" dataset including home team effect and Boundary Prob
h1a1_df18 <- as.data.frame(h1a118)
nhl_2018_neat <- 
  nhl_2018_neat %>%
  mutate(ID = row_number())
h1a1_df18 <-
  h1a1_df18 %>%
  mutate(ID = row_number())

model_subset18 <-
  nhl_2018_neat %>%
  select("BoundaryProbHome2", "BoundaryProbAway2", 
         "HomeTeam", 
         "AwayTeam",
         "DeadlineInd", 
         "DeadlineDays", "ID")

model_subset18 <- left_join(model_subset18, h1a1_df18,
                          by = "ID")

model_subset18 <- subset(model_subset18, select = -ID)

model_subset18 <-
  model_subset18 %>%
  mutate(DeadlineDays = as.numeric(DeadlineDays))

model_subset18 <-
  model_subset18 %>%
  rename("Anaheim" = `nhl_2018_neat$HomeTeamAnaheim`,
         "Arizona" = `nhl_2018_neat$HomeTeamArizona`,
         "Boston" =`nhl_2018_neat$HomeTeamBoston`,
         "Buffalo" = `nhl_2018_neat$HomeTeamBuffalo`,
         "Calgary" = `nhl_2018_neat$HomeTeamCalgary`,
         "Carolina" = `nhl_2018_neat$HomeTeamCarolina`,
         "Chicago" = `nhl_2018_neat$HomeTeamChicago`,
         "Colorado" = `nhl_2018_neat$HomeTeamColorado`,
         "Columbus" = `nhl_2018_neat$HomeTeamColumbus`,
         "Dallas" = `nhl_2018_neat$HomeTeamDallas`,
         "Detroit" = `nhl_2018_neat$HomeTeamDetroit`,
         "Edmonton" = `nhl_2018_neat$HomeTeamEdmonton`,
         "Florida" = `nhl_2018_neat$HomeTeamFlorida`,
         "LosAngeles" = `nhl_2018_neat$HomeTeamLosAngeles`,
         "Minnesota" = `nhl_2018_neat$HomeTeamMinnesota`,
         "Montreal" = `nhl_2018_neat$HomeTeamMontreal`,
         "Nashville" = `nhl_2018_neat$HomeTeamNashville`,
         "NewJersey" =`nhl_2018_neat$HomeTeamNewJersey`,
         "NYIslanders" = `nhl_2018_neat$HomeTeamNYIslanders`,
         "NYRangers" = `nhl_2018_neat$HomeTeamNYRangers`,
         "Ottawa" = `nhl_2018_neat$HomeTeamOttawa`,
         "Philadelphia" = `nhl_2018_neat$HomeTeamPhiladelphia`,
         "Pittsburgh" = `nhl_2018_neat$HomeTeamPittsburgh`,
         "SanJose" = `nhl_2018_neat$HomeTeamSanJose`,
         "St.Louis" = `nhl_2018_neat$HomeTeamSt.Louis`,
         "TampaBay" = `nhl_2018_neat$HomeTeamTampaBay`,
         "Toronto" = `nhl_2018_neat$HomeTeamToronto`,
         "Vancouver" = `nhl_2018_neat$HomeTeamVancouver`,
         "Vegas" = `nhl_2018_neat$HomeTeamVegas`,
         "Washington" = `nhl_2018_neat$HomeTeamWashington`,
         "Winnipeg" = `nhl_2018_neat$HomeTeamWinnipeg`
  )

write.csv(model_subset18, "data/model_subset18.csv")

## 2019-2020
# loading data
nhl_2019 <- readxl::read_xlsx("data/nhl odds 2019-20.xlsx")

# trade deadline and date indicator variables
nhl_2019 <-
  nhl_2019 %>%
  mutate(Deadline = as.numeric(Date %in% c(224:929)),
         Year = as.numeric(Date %in% c(101:929)))

# adding date variable
nhl_2019 <-
  nhl_2019 %>%
  mutate(Date = ifelse(Year == 0, paste(2019, Date, sep = ""),
                       paste(2020, Date, sep = "0")))

# convert date into a "date"
nhl_2019 <-
  nhl_2019 %>%
  mutate(Date = lubridate::ymd(Date))

# taking out playoff games
nhl_2019 <-
  nhl_2019 %>%
  filter(Date < "2020-8-01")

# making ID variable for game
nhl_2019$ID <- seq.int(nrow(nhl_2019))
nhl_2019 <-
  nhl_2019 %>%
  mutate(ID = as.integer(ID),
         ID = ID / 2,
         ID = ceiling(ID))

# indicator variable for days since trade deadline
nhl_2019 <-
  nhl_2019 %>%
  mutate(Deadline2 = Date - lubridate::ymd(20200224))

# make data wider - each row is one game
nhl_2019_home <- nhl_2019 %>%
  filter(VH == "H")
nhl_2019_away <- nhl_2019 %>%
  filter(VH == "V")
nhl_2019_wide<-bind_cols(nhl_2019_home,nhl_2019_away)

# tidy wider data
nhl_2019_wide <- 
  nhl_2019_wide[, c(1, 2, 22, 3, 4, 24, 5,
                    25, 6, 26, 7, 27, 8, 28,
                    9, 29, 10, 30, 11, 31, 12,
                    32, 13, 33, 14, 34, 15, 35,
                    16, 36, 17, 18, 19, 20, 21,
                    23, 37, 38, 39, 40)]

nhl_2019_neat <- 
  nhl_2019_wide[-c(4, 24, 27, 35:40)]

nhl_2019_neat <-
  nhl_2019_neat %>%
  rename("Date" = Date...1,
         "Rot Home Team" = Rot...2,
         "HomeTeam" =Team...4,
         "1st Home Team Goals" = `1st...5`,
         "2nd Home Team Goals" = `2nd...6`,
         "3rd Home Team Goals" = `3rd...7`,
         "FinalHome" = Final...8,
         "Open Home Team" = Open...9,
         "CloseHomeTeam" = Close...10,
         "Puck Line Home Team" = `PuckLine...11`,
         "Open OU" = `OpenOU...13`,
         "Close OU" = `CloseOU...35`,
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
         "Puck Line Away Team" = `PuckLine...31`,
  )

# Market Probabilities
nhl_2019_neat <-
  nhl_2019_neat %>%
  mutate(BoundaryProbHome = ifelse(CloseHomeTeam >= 100, 100/(CloseHomeTeam + 100), abs(CloseHomeTeam) / (100 + abs(CloseHomeTeam)))) 

nhl_2019_neat <-
  nhl_2019_neat %>%
  mutate(BoundaryProbAway = ifelse(CloseAwayTeam >= 100, 100/(CloseAwayTeam + 100), abs(CloseAwayTeam) / (100 + abs(CloseAwayTeam)))) 

nhl_2019 <- 
  nhl_2019 %>%
  mutate(BoundaryProb = ifelse(Close >= 100, 100/(Close + 100), abs(Close) / (100 + abs(Close)))) 

nhl_2019_neat <-
  nhl_2019_neat %>%
  mutate(BoundaryProbHome2 = BoundaryProbHome / (BoundaryProbHome + BoundaryProbAway),
         BoundaryProbAway2 = BoundaryProbAway / (BoundaryProbAway + BoundaryProbHome))

# Replacing error
nhl_2019_neat$HomeTeam = 
  str_replace(nhl_2019_neat$HomeTeam, "Arizonas",
              "Arizona")

# Making Model Matrix for Home Team Effect
h119 = model.matrix(~-1+nhl_2019_neat$HomeTeam)
a119 = model.matrix(~-1+nhl_2019_neat$AwayTeam)
h1a119 = h119-a119
h1a1_df19 <- as.data.frame(h1a119)
h1a1_df19 <-
  h1a1_df19 %>%
  mutate(ID = row_number())

# Linear Model
nhl_2019_neat <-
  nhl_2019_neat %>%
  mutate(DeadlineDays = as.numeric(DeadlineDays))

homeandaway_lm19 <- lm(BoundaryProbHome2 ~ h1a119 + DeadlineInd +
                         DeadlineDays + h1a119 * DeadlineInd + 
                         h1a119 * DeadlineDays + 
                         h1a119 * DeadlineInd * DeadlineDays,
                       data = nhl_2019_neat)

summary(homeandaway_lm19)

plot(homeandaway_lm19)

## Calculating who won/lost trade deadline: Lasso Model
tidy_coef19 <- tidy(homeandaway_lm19)

# constructing lasso model
nhl_2019_neat <- 
  nhl_2019_neat %>%
  mutate(ID = row_number())

model_subset19 <-
  nhl_2019_neat %>%
  select("BoundaryProbHome2", "DeadlineInd",
         "DeadlineDays", "ID")

model_subset19 <-
  left_join(model_subset19, h1a1_df19, by = "ID")

model_subset19 <- subset(model_subset19, select = -ID)

# using model.matrix() function
model_y19 <-
  model_subset19$BoundaryProbHome2

model_x19 <- model.matrix(BoundaryProbHome2 ~ 
                            h1a119 + DeadlineInd + 
                            DeadlineDays + 
                            h1a119 * DeadlineInd + 
                            h1a119 * DeadlineDays + 
                            h1a119 * DeadlineInd * DeadlineDays,
                          data = model_subset19)[,-1]

# Lasso regression
fit_lasso_cv19 <- cv.glmnet(model_x19, model_y19, 
                            alpha = 1)

# finding who won/lost the trade deadline LASSO
tidy_coeffs19 <- coef(fit_lasso_cv19, s = "lambda.1se")
tidy_lasso_coef19 <- data.frame(name = tidy_coeffs19@Dimnames[[1]][tidy_coeffs19@i + 1], 
                                coefficient = tidy_coeffs19@x)

tidy_lasso_coef19 <- rename(tidy_lasso_coef19, term = name)

tidy_lasso_coef19_tot <- left_join(tidy_coef19, tidy_lasso_coef19, by = "term")
tidy_lasso_coef19_2 <- tidy_lasso_coef19_tot[-c(2:5)] 
tidy_lasso_coef19_2[is.na(tidy_lasso_coef19_2)] = 0

# transposing df
tidy_lasso_coef19_2 <- t(tidy_lasso_coef19_2)
tidy_lasso_coef19_2 <- as.data.frame(tidy_lasso_coef19_2)

# tidying names
names(tidy_lasso_coef19_2) <- tidy_lasso_coef19_2[1,]
tidy_lasso_coef19_2 <- tidy_lasso_coef19_2[-1,]

# split by coefficients
hometeams <- tidy_lasso_coef19_2 %>%
  select(1:32)
deadlineind <- tidy_lasso_coef19_2 %>%
  select(DeadlineInd, 35:65)
deadlinedays <- tidy_lasso_coef19_2 %>%
  select(DeadlineDays, 66:96)
interaction <- tidy_lasso_coef19_2 %>%
  select(97:128)

# making data longer
hometeams2 <- gather(hometeams, Team, intercept, 2:32)
deadlineind2 <- gather(deadlineind, Team, deadline_indicator, 2:32)
deadlinedays2 <- gather(deadlinedays, Team, deadline_days, 2:32)
interaction2 <- gather(interaction, Team, deadline_interaction, 2:32)

# binding data set together
tidy_lasso_coef19_3 <- cbind(hometeams2, deadlineind2, deadlinedays2, interaction2)

# setting NAs to 0
tidy_lasso_coef19_3[is.na(tidy_lasso_coef19_3)] <- 0

# tidying
tidy_lasso_coef19_clean <- tidy_lasso_coef19_3[,-c(5,8,11)]
names(tidy_lasso_coef19_clean)[1] <- "main_intercept"

tidy_lasso_coef19_clean<-
  tidy_lasso_coef19_clean %>%
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

tidy_lasso_coef19_fin <-
  tidy_lasso_coef19_clean %>%
  mutate(
    beforedeadline = main_intercept + intercept,
    atdeadline = main_intercept + intercept + (DeadlineInd + deadline_indicator),
    predictedend = main_intercept + intercept + (DeadlineInd + deadline_indicator) + 16 * (DeadlineDays + deadline_days + `DeadlineInd:DeadlineDays` + deadline_interaction),
    firstlinepredictedend = main_intercept + intercept + 16 * (DeadlineDays + deadline_days),
    diffat0 = atdeadline - beforedeadline,
    diffat40 = predictedend - firstlinepredictedend
  )





# saving data frame of coefficients
write.csv(tidy_lasso_coef19_fin, "data/coefs2019.csv")
write.csv(nhl_2019_neat, "data/nhl_2019_neat.csv")


# creating a "model" dataset including home team effect and Boundary Prob
h1a1_df19 <- as.data.frame(h1a119)
nhl_2019_neat <- 
  nhl_2019_neat %>%
  mutate(ID = row_number())
h1a1_df19 <-
  h1a1_df19 %>%
  mutate(ID = row_number())

model_subset19 <-
  nhl_2019_neat %>%
  select("BoundaryProbHome2", "BoundaryProbAway2", 
         "HomeTeam", 
         "AwayTeam",
         "DeadlineInd", 
         "DeadlineDays", "ID")

model_subset19 <- left_join(model_subset19, h1a1_df19,
                            by = "ID")

model_subset19 <- subset(model_subset19, select = -ID)

model_subset19 <-
  model_subset19 %>%
  mutate(DeadlineDays = as.numeric(DeadlineDays))

model_subset19 <-
  model_subset19 %>%
  rename("Anaheim" = `nhl_2019_neat$HomeTeamAnaheim`,
         "Arizona" = `nhl_2019_neat$HomeTeamArizona`,
         "Boston" =`nhl_2019_neat$HomeTeamBoston`,
         "Buffalo" = `nhl_2019_neat$HomeTeamBuffalo`,
         "Calgary" = `nhl_2019_neat$HomeTeamCalgary`,
         "Carolina" = `nhl_2019_neat$HomeTeamCarolina`,
         "Chicago" = `nhl_2019_neat$HomeTeamChicago`,
         "Colorado" = `nhl_2019_neat$HomeTeamColorado`,
         "Columbus" = `nhl_2019_neat$HomeTeamColumbus`,
         "Dallas" = `nhl_2019_neat$HomeTeamDallas`,
         "Detroit" = `nhl_2019_neat$HomeTeamDetroit`,
         "Edmonton" = `nhl_2019_neat$HomeTeamEdmonton`,
         "Florida" = `nhl_2019_neat$HomeTeamFlorida`,
         "LosAngeles" = `nhl_2019_neat$HomeTeamLosAngeles`,
         "Minnesota" = `nhl_2019_neat$HomeTeamMinnesota`,
         "Montreal" = `nhl_2019_neat$HomeTeamMontreal`,
         "Nashville" = `nhl_2019_neat$HomeTeamNashville`,
         "NewJersey" =`nhl_2019_neat$HomeTeamNewJersey`,
         "NYIslanders" = `nhl_2019_neat$HomeTeamNYIslanders`,
         "NYRangers" = `nhl_2019_neat$HomeTeamNYRangers`,
         "Ottawa" = `nhl_2019_neat$HomeTeamOttawa`,
         "Philadelphia" = `nhl_2019_neat$HomeTeamPhiladelphia`,
         "Pittsburgh" = `nhl_2019_neat$HomeTeamPittsburgh`,
         "SanJose" = `nhl_2019_neat$HomeTeamSanJose`,
         "St.Louis" = `nhl_2019_neat$HomeTeamSt.Louis`,
         "TampaBay" = `nhl_2019_neat$HomeTeamTampaBay`,
         "Toronto" = `nhl_2019_neat$HomeTeamToronto`,
         "Vancouver" = `nhl_2019_neat$HomeTeamVancouver`,
         "Vegas" = `nhl_2019_neat$HomeTeamVegas`,
         "Washington" = `nhl_2019_neat$HomeTeamWashington`,
         "Winnipeg" = `nhl_2019_neat$HomeTeamWinnipeg`
  )

write.csv(model_subset19, "data/model_subset19.csv")

## 2020-2021
# loading data
nhl_2021 <- readxl::read_xlsx("data/nhl odds 2021.xlsx")

# trade deadline and date indicator variables
nhl_2021 <-
  nhl_2021 %>%
  mutate(Deadline = as.numeric(Date %in% c(412:707)),
         Year = as.numeric(Date %in% c(113:707)))

# adding date variable
nhl_2021 <-
  nhl_2021 %>%
  mutate(Date = ifelse(Year == 0, paste(2020, Date, sep = ""),
                       paste(2021, Date, sep = "0")))

# convert date into a "date"
nhl_2021 <-
  nhl_2021 %>%
  mutate(Date = lubridate::ymd(Date))

# taking out playoff games
nhl_2021 <-
  nhl_2021 %>%
  filter(Date < "2021-5-15")

# making ID variable for game
nhl_2021$ID <- seq.int(nrow(nhl_2021))
nhl_2021 <-
  nhl_2021 %>%
  mutate(ID = as.integer(ID),
         ID = ID / 2,
         ID = ceiling(ID))

# indicator variable for days since trade deadline
nhl_2021 <-
  nhl_2021 %>%
  mutate(Deadline2 = Date - lubridate::ymd(20210412))

# make data wider - each row is one game
nhl_2021_home <- nhl_2021 %>%
  filter(VH == "H")
nhl_2021_away <- nhl_2021 %>%
  filter(VH == "V")
nhl_2021_wide<-bind_cols(nhl_2021_home,nhl_2021_away)

# tidy wider data
nhl_2021_wide <- 
  nhl_2021_wide[, c(1, 2, 22, 3, 4, 24, 5,
                    25, 6, 26, 7, 27, 8, 28,
                    9, 29, 10, 30, 11, 31, 12,
                    32, 13, 33, 14, 34, 15, 35,
                    16, 36, 17, 18, 19, 20, 21,
                    23, 37, 38, 39, 40)]

nhl_2021_neat <- 
  nhl_2021_wide[-c(4, 24, 27, 35:40)]

nhl_2021_neat <-
  nhl_2021_neat %>%
  rename("Date" = Date...1,
         "Rot Home Team" = Rot...2,
         "HomeTeam" =Team...4,
         "1st Home Team Goals" = `1st...5`,
         "2nd Home Team Goals" = `2nd...6`,
         "3rd Home Team Goals" = `3rd...7`,
         "FinalHome" = Final...8,
         "Open Home Team" = Open...9,
         "CloseHomeTeam" = Close...10,
         "Puck Line Home Team" = `PuckLine...11`,
         "Open OU" = `OpenOU...13`,
         "Close OU" = `CloseOU...35`,
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
         "Puck Line Away Team" = `PuckLine...31`,
  )

# Market Probabilities
nhl_2021_neat <-
  nhl_2021_neat %>%
  mutate(BoundaryProbHome = ifelse(CloseHomeTeam >= 100, 100/(CloseHomeTeam + 100), abs(CloseHomeTeam) / (100 + abs(CloseHomeTeam)))) 

nhl_2021_neat <-
  nhl_2021_neat %>%
  mutate(BoundaryProbAway = ifelse(CloseAwayTeam >= 100, 100/(CloseAwayTeam + 100), abs(CloseAwayTeam) / (100 + abs(CloseAwayTeam)))) 

nhl_2021 <- 
  nhl_2021 %>%
  mutate(BoundaryProb = ifelse(Close >= 100, 100/(Close + 100), abs(Close) / (100 + abs(Close)))) 

nhl_2021_neat <-
  nhl_2021_neat %>%
  mutate(BoundaryProbHome2 = BoundaryProbHome / (BoundaryProbHome + BoundaryProbAway),
         BoundaryProbAway2 = BoundaryProbAway / (BoundaryProbAway + BoundaryProbHome))

# Making Model Matrix for Home Team Effect
h121 = model.matrix(~-1+nhl_2021_neat$HomeTeam)
a121 = model.matrix(~-1+nhl_2021_neat$AwayTeam)
h1a121 = h121-a121
h1a1_df21 <- as.data.frame(h1a121)
h1a1_df21 <-
  h1a1_df21 %>%
  mutate(ID = row_number())

# Linear Model
nhl_2021_neat <-
  nhl_2021_neat %>%
  mutate(DeadlineDays = as.numeric(DeadlineDays))

homeandaway_lm21 <- lm(BoundaryProbHome2 ~ h1a121 + DeadlineInd +
                         DeadlineDays + h1a121 * DeadlineInd + 
                         h1a121 * DeadlineDays + 
                         h1a121 * DeadlineInd * DeadlineDays,
                       data = nhl_2021_neat)

summary(homeandaway_lm21)

plot(homeandaway_lm21)

## Calculating who won/lost trade deadline: Lasso Model
tidy_coef21 <- tidy(homeandaway_lm21)

# constructing lasso model
nhl_2021_neat <- 
  nhl_2021_neat %>%
  mutate(ID = row_number())

model_subset21 <-
  nhl_2021_neat %>%
  select("BoundaryProbHome2", "DeadlineInd",
         "DeadlineDays", "ID")

model_subset21 <-
  left_join(model_subset21, h1a1_df21, by = "ID")

model_subset21 <- subset(model_subset21, select = -ID)

# using model.matrix() function
model_y21 <-
  model_subset21$BoundaryProbHome2

model_x21 <- model.matrix(BoundaryProbHome2 ~ 
                            h1a121 + DeadlineInd + 
                            DeadlineDays + 
                            h1a121 * DeadlineInd + 
                            h1a121 * DeadlineDays + 
                            h1a121 * DeadlineInd * DeadlineDays,
                          data = model_subset21)[,-1]

# Lasso regression
fit_lasso_cv21 <- cv.glmnet(model_x21, model_y21, 
                            alpha = 1)

# finding who won/lost the trade deadline LASSO
tidy_coeffs21 <- coef(fit_lasso_cv21, s = "lambda.1se")
tidy_lasso_coef21 <- data.frame(name = tidy_coeffs21@Dimnames[[1]][tidy_coeffs21@i + 1], 
                                coefficient = tidy_coeffs21@x)

tidy_lasso_coef21 <- rename(tidy_lasso_coef21, term = name)

tidy_lasso_coef21_tot <- left_join(tidy_coef21, tidy_lasso_coef21, by = "term")
tidy_lasso_coef21_2 <- tidy_lasso_coef21_tot[-c(2:5)] 
tidy_lasso_coef21_2[is.na(tidy_lasso_coef21_2)] = 0

# transposing df
tidy_lasso_coef21_2 <- t(tidy_lasso_coef21_2)
tidy_lasso_coef21_2 <- as.data.frame(tidy_lasso_coef21_2)

# tidying names
names(tidy_lasso_coef21_2) <- tidy_lasso_coef21_2[1,]
tidy_lasso_coef21_2 <- tidy_lasso_coef21_2[-1,]

# split by coefficients
hometeams <- tidy_lasso_coef21_2 %>%
  select(1:32)
deadlineind <- tidy_lasso_coef21_2 %>%
  select(DeadlineInd, 35:65)
deadlinedays <- tidy_lasso_coef21_2 %>%
  select(DeadlineDays, 66:96)
interaction <- tidy_lasso_coef21_2 %>%
  select(97:128)

# making data longer
hometeams2 <- gather(hometeams, Team, intercept, 2:32)
deadlineind2 <- gather(deadlineind, Team, deadline_indicator, 2:32)
deadlinedays2 <- gather(deadlinedays, Team, deadline_days, 2:32)
interaction2 <- gather(interaction, Team, deadline_interaction, 2:32)

# binding data set together
tidy_lasso_coef21_3 <- cbind(hometeams2, deadlineind2, deadlinedays2, interaction2)

# setting NAs to 0
tidy_lasso_coef21_3[is.na(tidy_lasso_coef21_3)] <- 0

# tidying
tidy_lasso_coef21_clean <- tidy_lasso_coef21_3[,-c(5,8,11)]
names(tidy_lasso_coef21_clean)[1] <- "main_intercept"

tidy_lasso_coef21_clean<-
  tidy_lasso_coef21_clean %>%
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

tidy_lasso_coef21_fin <-
  tidy_lasso_coef21_clean %>%
  mutate(
    beforedeadline = main_intercept + intercept,
    atdeadline = main_intercept + intercept + (DeadlineInd + deadline_indicator),
    predictedend = main_intercept + intercept + (DeadlineInd + deadline_indicator) + 32 * (DeadlineDays + deadline_days + `DeadlineInd:DeadlineDays` + deadline_interaction),
    firstlinepredictedend = main_intercept + intercept + 32 * (DeadlineDays + deadline_days),
    diffat0 = atdeadline - beforedeadline,
    diffat40 = predictedend - firstlinepredictedend
  )





# saving data frame of coefficients
write.csv(tidy_lasso_coef21_fin, "data/coefs2021.csv")
write.csv(nhl_2021_neat, "data/nhl_2021_neat.csv")


# creating a "model" dataset including home team effect and Boundary Prob
h1a1_df21 <- as.data.frame(h1a121)
nhl_2021_neat <- 
  nhl_2021_neat %>%
  mutate(ID = row_number())
h1a1_df21 <-
  h1a1_df21 %>%
  mutate(ID = row_number())

model_subset21 <-
  nhl_2021_neat %>%
  select("BoundaryProbHome2", "BoundaryProbAway2", 
         "HomeTeam", 
         "AwayTeam",
         "DeadlineInd", 
         "DeadlineDays", "ID")

model_subset21 <- left_join(model_subset21, h1a1_df21,
                            by = "ID")

model_subset21 <- subset(model_subset21, select = -ID)

model_subset21 <-
  model_subset21 %>%
  mutate(DeadlineDays = as.numeric(DeadlineDays))

model_subset21 <-
  model_subset21 %>%
  rename("Anaheim" = `nhl_2021_neat$HomeTeamAnaheim`,
         "Arizona" = `nhl_2021_neat$HomeTeamArizona`,
         "Boston" =`nhl_2021_neat$HomeTeamBoston`,
         "Buffalo" = `nhl_2021_neat$HomeTeamBuffalo`,
         "Calgary" = `nhl_2021_neat$HomeTeamCalgary`,
         "Carolina" = `nhl_2021_neat$HomeTeamCarolina`,
         "Chicago" = `nhl_2021_neat$HomeTeamChicago`,
         "Colorado" = `nhl_2021_neat$HomeTeamColorado`,
         "Columbus" = `nhl_2021_neat$HomeTeamColumbus`,
         "Dallas" = `nhl_2021_neat$HomeTeamDallas`,
         "Detroit" = `nhl_2021_neat$HomeTeamDetroit`,
         "Edmonton" = `nhl_2021_neat$HomeTeamEdmonton`,
         "Florida" = `nhl_2021_neat$HomeTeamFlorida`,
         "LosAngeles" = `nhl_2021_neat$HomeTeamLosAngeles`,
         "Minnesota" = `nhl_2021_neat$HomeTeamMinnesota`,
         "Montreal" = `nhl_2021_neat$HomeTeamMontreal`,
         "Nashville" = `nhl_2021_neat$HomeTeamNashville`,
         "NewJersey" =`nhl_2021_neat$HomeTeamNewJersey`,
         "NYIslanders" = `nhl_2021_neat$HomeTeamNYIslanders`,
         "NYRangers" = `nhl_2021_neat$HomeTeamNYRangers`,
         "Ottawa" = `nhl_2021_neat$HomeTeamOttawa`,
         "Philadelphia" = `nhl_2021_neat$HomeTeamPhiladelphia`,
         "Pittsburgh" = `nhl_2021_neat$HomeTeamPittsburgh`,
         "SanJose" = `nhl_2021_neat$HomeTeamSanJose`,
         "St.Louis" = `nhl_2021_neat$HomeTeamSt.Louis`,
         "TampaBay" = `nhl_2021_neat$HomeTeamTampaBay`,
         "Toronto" = `nhl_2021_neat$HomeTeamToronto`,
         "Vancouver" = `nhl_2021_neat$HomeTeamVancouver`,
         "Vegas" = `nhl_2021_neat$HomeTeamVegas`,
         "Washington" = `nhl_2021_neat$HomeTeamWashington`,
         "Winnipeg" = `nhl_2021_neat$HomeTeamWinnipeg`
  )

write.csv(model_subset21, "data/model_subset21.csv")

