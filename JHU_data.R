library(tidyverse)
library(tools)

dat <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

states <- dat %>% 
  filter(Country.Region == "US", 
         !grepl(",",Province.State),
         !grepl("Princess",Province.State),
         Province.State != "District of Columbia") %>% 
  pivot_longer(5:ncol(.), names_to = "Date", values_to = "Cases") %>% 
  mutate(Date = str_remove(Date,"X"),
         Date = as.Date(Date, "%m.%d.%y")) %>% 
  group_by(Province.State) %>% 
  filter(Cases == max(Cases)) %>% 
  distinct(Province.State,Cases) %>% 
  select(state = Province.State,
         cases = Cases)
 
counties <-  dat %>% 
  filter(Country.Region == "US", 
         grepl(",",Province.State),
         !grepl("Princess",Province.State),
         Province.State != "Washington, D.C.") %>% 
  separate(Province.State, c("county", "state"), ", ") %>% 
  mutate(county = str_remove(county, " County")) %>% 
  pivot_longer(6:ncol(.), names_to = "Date", values_to = "Cases") %>% 
  mutate(Date = str_remove(Date,"X"),
         Date = as.Date(Date, "%m.%d.%y")) %>% 
  group_by(county, state) %>% 
  filter(Cases == max(Cases)) %>% 
  ungroup() %>% 
  distinct(county, state,Cases) %>% 
  select(county = county,
         state,
         cases = Cases)

#write.csv(states, "state_cases.csv",row.names=FALSE)
#write.csv(counties, "county_cases.csv",row.names=FALSE)

#FIPS=====================================================================

county_fips <- read.csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_and_county_fips_master.csv") 
county_fips2 <- read_tsv("https://gist.githubusercontent.com/adamjanes/6cf85a4fd79e122695ebde7d41fe327f/raw/7040d66840468cbb1a81de2cd7a2138df7e02928/unemployment.tsv")
library(maps)
county_fips3 <- read_tsv("fips.tsv")

county_fips <- read_tsv("fips.tsv")%>% 
  select(id = fips,
         name,
         state) %>% 
  mutate(id = str_pad(as.character(id), 5, pad = "0"),
         name = str_remove(name, " County"),
         name = toTitleCase(tolower(as.character(name))),
         id = case_when(grepl("000", id) ~ str_sub(id, end=-4),
         TRUE ~ id))

#write.csv(county_fips, "county_fips.csv",row.names=FALSE)

county_fips <- read.csv("county_fips.csv")

state_cases <- county_fips %>% filter(is.na(state)) %>% 
  left_join(states, by= c("name" = "state")) %>% 
  select(-state)

county_cases <- county_fips %>% filter(!is.na(state)) %>% 
      left_join(counties, by= c("name" = "county","state" = "state")) %>% 
      select(-state,-name,rate=cases) %>% 
  mutate(rate = case_when(is.na(rate) ~ 0,
                          TRUE ~ as.numeric(rate)))

write.csv(state_cases, "state_cases.csv",row.names=FALSE)
write.csv(county_cases, "county_cases.csv",row.names=FALSE)

