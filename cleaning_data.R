library(tidyverse)
library(broom)
library(glmnet)


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
nhl_2017_wide


















