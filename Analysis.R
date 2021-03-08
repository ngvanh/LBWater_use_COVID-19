setwd("~/LBWater Use_COVID-19")
source("Library.R")
source("Data_Prep.R")

#1. Compare monthly water use among 3 years
MonthlySummary <- cleanedAllReads %>% 
  mutate("ADJ_MTH_WTR_USE" = ifelse(is.na(ADJ_MTH_WTR_USE), 0, WTR_USE)) %>%
  group_by(., YEAR, months.Date(WTR_READ_MTH)) %>%
  summarise(average = mean(ADJ_MTH_WTR_USE),
            Total = sum(ADJ_MTH_WTR_USE)) %>%
  ungroup()


total_month_plot <- ggplot(data=MonthlySummary, aes(x=`months.Date(WTR_READ_MTH)`, y=Total, group=YEAR, colour=factor(YEAR))) + 
  geom_line(size=.75) + geom_point() +
  scale_x_discrete(limits=c("January","February","March","April","May","June",
                            "July", "August","September","October","November","December")) + 
  scale_y_continuous(labels=comma) + 
  scale_colour_manual(values= c("red", "blue", "green"), name="Year") +    
  ylab("Total Water Use") +
  xlab("Month")

#2. Compare trends in water use by Rate Type
RateSummary <- cleanedAllReads %>% 
  mutate("ADJ_MTH_WTR_USE" = ifelse(is.na(ADJ_MTH_WTR_USE), 0, WTR_USE)) %>%
  group_by(., WTR_RATE, YEAR, months.Date(WTR_READ_MTH)) %>%
  summarise(average = mean(ADJ_MTH_WTR_USE)) %>%
  ungroup()

RateSummary2 <- cleanedAllReads %>% 
  mutate("ADJ_MTH_WTR_USE" = ifelse(is.na(ADJ_MTH_WTR_USE), 0, WTR_USE)) %>%
  group_by(., WTR_RATE, WTR_READ_MTH) %>%
  summarise(average = mean(ADJ_MTH_WTR_USE)) %>%
  ungroup()

RateSummary3 <- cleanedAllReads %>% 
  mutate("ADJ_MTH_WTR_USE" = ifelse(is.na(ADJ_MTH_WTR_USE), 0, WTR_USE)) %>%
  group_by(., WTR_RATE, YEAR) %>%
  summarise(average = mean(ADJ_MTH_WTR_USE)) %>%
  ungroup()

#Plots
by_rate_plot <- ggplot(RateSummary, aes(x=`months.Date(WTR_READ_MTH)`, y=average, group=YEAR, colour=factor(YEAR))) + 
  geom_line(size=.75) + geom_point() + 
  scale_x_discrete(limits=c("January","February","March","April","May","June",
                            "July", "August","September","October","November","December")) + 
  scale_y_continuous(labels=comma) + 
  scale_colour_manual(values= c("red", "blue", "green"), name="Year") +    
  ylab("Average Water Use") +
  xlab("Month") + 
  facet_wrap(. ~ WTR_RATE) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

