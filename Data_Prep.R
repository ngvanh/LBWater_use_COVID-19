setwd("~/LBWater Use_COVID-19")
source("Library.R")

#Load the 2018, 2019, and 2020 data
Reads2018 <- read_csv("data/AllAccountReads2018.csv")
Reads2018$ACCT_NBR <- as.character(Reads2018$ACCT_NBR)

Reads2019 <- read_csv("data/AllAccountReads2019.csv")
Reads2019$ACCT_NBR <- as.character(Reads2019$ACCT_NBR)

Reads2020 <- read_csv("data/AllAccountReads2020.csv")
Reads2020$ACCT_NBR <- as.character(Reads2020$ACCT_NBR)

#Join three year data
AllReadsWide <- Reads2018 %>% inner_join(Reads2019, by = "ACCT_NBR") %>% 
  inner_join(Reads2020, by = "ACCT_NBR")

#Convert the joint data from wide format to long format 
Rate <- AllReadsWide %>% select(., ACCT_NBR, contains("WTR_RATE")) %>%
  melt(., id.vars="ACCT_NBR", value.name = "WTR_RATE") %>%
  mutate(., "YEAR" = substr(variable, start = 1, stop = 4)) %>%
  select(., -variable)

WaterUse <- AllReadsWide %>% select(., ACCT_NBR, contains("WTR_USE")) %>%
  melt(., id.vars="ACCT_NBR", value.name = "WTR_USE") %>%
  mutate(., "YEAR" = substring(variable, first = 1, last = 4),
         "COUNT" = substring(variable, first = 13)) %>%
  select(., -variable)

ReadDate <- AllReadsWide %>% select(., ACCT_NBR, contains("WTR_READ_DT")) %>%
  melt(., id.vars="ACCT_NBR", value.name = "WTR_READ_DT") %>%
  mutate(., "YEAR" = substring(variable, first = 1, last = 4),
         "COUNT" = substring(variable, first = 17)) %>%
  select(., -variable)

ReadDays <- AllReadsWide %>% select(., ACCT_NBR, contains("WTR_READ_DAYS")) %>%
  melt(., id.vars="ACCT_NBR", value.name = "WTR_READ_DAYS") %>%
  mutate(., "YEAR" = substring(variable, first = 1, last = 4),
         "COUNT" = substring(variable, first = 19)) %>%
  select(., -variable)

ThreeYearReads <- Rate %>% inner_join(WaterUse, by = c("ACCT_NBR", "YEAR")) %>%
  inner_join(ReadDate, by = c("ACCT_NBR", "YEAR", "COUNT")) %>%
  inner_join(ReadDays, by = c("ACCT_NBR", "YEAR", "COUNT")) %>%
  mutate(., "MONTH" = 13 - as.numeric(COUNT), "WTR_READ_MTH" = as.yearmon(paste(YEAR, MONTH, 1, sep = "-"))) %>%
  select(., ACCT_NBR, WTR_RATE, WTR_READ_DT, WTR_USE, WTR_READ_DAYS, WTR_READ_MTH, YEAR, COUNT) %>%
  arrange(., ACCT_NBR)

#Fill in missing values using three month averaging method
cleanedAllReads <- ThreeYearReads %>%
  arrange(ACCT_NBR, desc(WTR_READ_MTH)) %>%
  group_by(ACCT_NBR) %>%
  mutate("ADJ_MTH_WTR_USE" = round(1/4*lead(WTR_USE,0) + 1/2*lead(WTR_USE,1) + 1/4*lead(WTR_USE,2))) %>%
  ungroup()

#write.csv(ThreeYearReads, "JointAllReads.csv")

rm(Rate, WaterUse, ReadDate, ReadDays, Reads2018, Reads2019, Reads2020, ThreeYearReads)



