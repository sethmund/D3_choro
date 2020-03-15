library(tidyverse)
library(tools)
library(readxl)

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

state_fips <- read.csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_and_county_fips_master.csv") %>% 
  filter(is.na(state)) %>% 
  select(id = fips,
         name,
         state) %>% 
  mutate(id = str_pad(as.character(id), 5, pad = "0"),
         name = str_remove(name, " County"),
         name = toTitleCase(tolower(as.character(name))),
         id = case_when(grepl("000", id) ~ str_sub(id, end=-4),
         TRUE ~ id))


county_fips <- read_xls("fips_codes_website.xls") %>% select(code=1,stfips=2,cfips=3,name=6,type=7) %>% 
  filter(type %in% c("Borough","County","Parish","Township","CDP","TDSA")) %>% 
  transmute(id = paste0(stfips,cfips),
            code,
            name)

state_cases <- state_fips %>% 
  left_join(states, by= c("name" = "state")) %>% 
  select(-state,-name, rate=cases)

county_cases <- county_fips %>% 
      left_join(counties, by= c("name" = "county","code" = "state")) %>% 
      select(-code,-name,rate=cases) %>% 
  mutate(rate = case_when(is.na(rate) ~ 0,
                          TRUE ~ as.numeric(rate)))


#==============================================================================

county_fips2 <- read_tsv("https://gist.githubusercontent.com/arunmallya/7131805ec108166dc5b9cdefd6fbca21/raw/ae84404e913e297b1fd0d5006ed33049b5067c96/unemployment.tsv")

county_cases <- county_cases %>% rbind(

county_fips2 %>% 
  anti_join(county_cases, by=c("id" = "id")) %>% 
  select(id) %>% 
  mutate(rate = 0)
)

write.csv(state_cases, "state_cases.csv",row.names=FALSE)
write.csv(county_cases, "county_cases.csv",row.names=FALSE)

