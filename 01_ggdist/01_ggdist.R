if(!require(ggdist)) install.packages("ggdist")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(lubridate)) install.packages("lubridate")
if(!require(tidyquant)) install.packages("tidyquant")
library(scales)
# Download COVID-19 Case Surveillance Public Use Data with Geography from CDC
# https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4


# Data Read-in ------------------------------------------------------------
## The data record the cases from 2020-01-01 to 2021-07-02
## The size of data is 27.2M rows and 19 columns
dt <- read.csv("~/Downloads/COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv")

head(dt)


# Data Cleaning -----------------------------------------------------------
## Some important columns are:
## case_month; The date received by CDC
## res_state: State of residence
## symptom_status: What is the symptom status of this person? [Asymptomatic,Symptomatic,,Unknown,Missing]
## hosp_yn: Was the patient hostipoltized [Yes, No, Unkown, Missing]
## age_group: Age group [0 - 17 years,18 - 49 years,50 - 64 years,65 + years,Unknown,Missing, NA, if value suppressed for privacy protection.]



dt_cleaned <- dt %>% 
  select(case_month, res_state, hosp_yn, age_group) %>% 
  mutate(
    age_group = ifelse(age_group == "0 - 17 {", "0 - 17 years", age_group)
    ) %>% 
  filter(
    res_state == "CA",
    age_group %in% c(
      "0 - 17 years", "0 - 17 {", "18 to 49 years", "50 to 64 years", "65+ years"
    )
  ) %>%
  filter(grepl("2020|2021", case_month)) %>% 
  mutate(
    case_month = lubridate::ym(case_month)
  )


head(dt_cleaned)
unique(dt_cleaned$age_group)

dt_summary <- dt_cleaned %>% 
  group_by(case_month, age_group) %>% 
  summarise(
    TotalNum = n()
  )


# Visualization -----------------------------------------------------------
ggplot(dt_summary, aes(x = case_month, y = TotalNum, group = factor(age_group), color = factor(age_group), fill = factor(age_group))) +
  scale_x_date(labels = date_format("%Y/%m"), date_breaks = "2 month", 
               limits = as.Date(c("2020-01-01", "2021-07-01"))) +
  geom_area(alpha = 0.6) +
  labs(x = "", y = "",
       title = "Total Number of COVID-19 patients in CA",
       subtitle = "The data record the cases from 2020-01-01 to 2021-07-02",
       caption = "Source: COVID-19 Case Surveillance Public Use Data with Geography from CDC") +
  scale_fill_discrete(name = "") +
  scale_color_discrete(name = "") +
  facet_wrap(~ age_group, ncol = 1) +
  theme(legend.position = "none") +
  theme_tq() +
  coord_flip()


