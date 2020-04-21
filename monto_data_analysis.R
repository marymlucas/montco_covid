#Monto PA COVID-19 Data"


## Data Sources

# load libraries
library(tidyverse)
library(lubridate)
library(data.table) # need this for the "%like%"function


# load data
tz_latest <- read.csv("https://storage.googleapis.com/montco-stats/tz.csv")
jhu_latest <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")


# data cleanup
tz_latest$datetime <- ymd_hms(tz_latest$timeStamp, tz=NULL)
tz_latest$desc <- as.character(tz_latest$desc)
tz_latest$zip <- as.character(tz_latest$zip)


## 911 Calls for Fever
tz_latest_fever <- tz_latest[ which (tz_latest$title == "EMS: FEVER"), ]
fever_by_month <- table(cut(tz_latest_fever$datetime, 'month'))
fever_calls_by_month <- as.data.frame(fever_by_month)
colnames(fever_calls_by_month) <- c("month", "fever_number_of_calls")
fever_calls_by_month$month <- ymd(fever_calls_by_month$month, tz=NULL)
ggplot(data = fever_calls_by_month, aes(x=month, y=fever_number_of_calls)) +
  geom_line() + scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1))


## 911 Calls for Respiratory Emergencies

# Calls for Respiratory Emergencies
tz_latest_resp <- tz_latest[which (tz_latest$title == "EMS: RESPIRATORY EMERGENCY"),]
head(tz_latest_resp)
resp_by_month <- table(cut(tz_latest_resp$datetime, 'month'))
resp_calls_by_month <- as.data.frame(resp_by_month)

colnames(resp_calls_by_month) <- c("month", "resp_number_of_calls")
resp_calls_by_month$month <- ymd(resp_calls_by_month$month, tz=NULL)
ggplot(data = resp_calls_by_month, aes(x=month, y=resp_number_of_calls)) +
  geom_line() + scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months") +
  theme_grey() +
  theme(axis.text.x=element_text(angle=60, hjust=1))


## 911 Calls for Medic Alert Alarms
# Calls for Medic Alert Alarms
tz_latest_medicalert <- tz_latest[which (tz_latest$title == "EMS: MEDICAL ALERT ALARM"),]
head(tz_latest_medicalert)
medicalert_by_month <- table(cut(tz_latest_medicalert$datetime, 'month'))
medicalert_calls_by_month <- as.data.frame(medicalert_by_month)

colnames(medicalert_calls_by_month) <- c("month", "medicalert_number_of_calls")
medicalert_calls_by_month$month <- ymd(medicalert_calls_by_month$month, tz=NULL)
ggplot(data = medicalert_calls_by_month, aes(x=month, y=medicalert_number_of_calls)) +
  geom_line() + scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months") +
  theme_grey() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

## 911 Calls for Traffic-related incidents (impacts of social distancing?)

# Calls for traffic incidents
tz_latest_traffic_calls <- tz_latest[which (tz_latest$title %like% "Traffic:"),]
head(tz_latest_traffic_calls)
traffic_calls_by_month <- table(cut(tz_latest_traffic_calls$datetime, 'month'))
traffic_calls_by_month <- as.data.frame(traffic_calls_by_month)

colnames(traffic_calls_by_month) <- c("month", "traffic_number_of_calls")
traffic_calls_by_month$month <- ymd(traffic_calls_by_month$month, tz=NULL)
ggplot(data = traffic_calls_by_month, aes(x=month, y=traffic_number_of_calls)) +
  geom_line() + scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

## Relationship in time between increase in 911 calls for fever and data on confirmed COVID-19 cases


# Extract 2020 call data for fever
tz_latest_fever_2020 <- tz_latest_fever[tz_latest_fever$datetime >= "2020-01-01",]

fever_by_week_2020 <- table(cut(tz_latest_fever_2020$datetime, 'week'))
fever_calls_by_week_2020 <- as.data.frame(fever_by_week_2020)
colnames(fever_calls_by_week_2020) <- c("week", "fever_number_of_calls_2020")
fever_calls_by_week_2020$week <- as.Date(fever_calls_by_week_2020$week)
ggplot(data = fever_calls_by_week_2020, aes(x=week, y=fever_number_of_calls_2020)) +
  geom_line() +
  scale_x_date( date_breaks = "2 weeks") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

