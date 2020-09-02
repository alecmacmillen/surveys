library(tidyverse)
library(tidylog)
library(assertr)
library(here)
library(plotly)
library(revgeo)
library(tidycensus)
library(revgeo)
rm(list = ls())

# EDA
# Validate location
# Filter out non-likely voters
# Create bar plots of survey vs. population distributions

ma.distributions <- read_csv(here::here("political/data/processed/census/cvap_distributions.csv")) %>% 
  filter(geoname == "Massachusetts")

responses <- read_csv(here::here("political/data/raw/ma/ma_08-26-20_responses_raw.csv"))

metadata <- read_csv(here::here("political/data/raw/ma/ma_08-26-20_metadata_raw.csv"))


# Validate proportion of responses with lat/lon in North Carolina
metadata.clean <- metadata %>% 
  mutate(location = revgeo::revgeo(long, lat)) %>% 
  separate(location, into = c("address", "city", "state", "zip", "country"), sep = ",") %>% 
  mutate(state = str_trim(state)) %>% 
  select(-c(address, city, zip, country))

in.ma <- sum(metadata.clean$state == "Massachusetts", na.rm = T) / nrow(metadata.clean)


# Function for a "clean" version of the data with binned variables
# ga.hhinc <- get_acs("state", variables = c(), year = 2018, state = "Georgia")
responses.clean <- responses %>%  
  filter(registered == "Yes") %>% 
  mutate(age_bin = case_when(dplyr::between(age, 18, 29) ~ "18-29",
                             dplyr::between(age, 30, 44) ~ "30-44",
                             dplyr::between(age, 45, 64) ~ "45-64",
                             TRUE ~ "65+"),
         educ_bin = case_when(educ %in% c("High school degree or equivalent (e.g. GED)",
                                          "Some college, no degree",
                                          "Associate degree (e.g. AA, AS)") ~ "no_bachelors",
                              TRUE ~ "bachelors"),
         hhinc_bin = case_when(hhinc >= 100 ~ "hhinc_ge_100",
                               TRUE ~ "hhinc_lt_100"))

responses.clean.dem <- responses.clean %>% 
  filter(party == "Democratic Party")

# Save cleaned metadata and response files
write_csv(metadata.clean, here::here("political/data/processed/ma/ma_08-26-20_metadata_clean.csv"))
write_csv(responses.clean, here::here("political/data/processed/ma/ma_08-26-20_responses_clean.csv"))
write_csv(responses.clean.dem, here::here("political/data/processed/ma/ma_08-26-20_dem_responses_clean.csv"))

# Calculate sample proportions and stack up against population proportions
### Gender
gender.prop <- c(sum(responses.clean$gender == "Male", na.rm=T) / nrow(responses.clean),
              sum(responses.clean$gender == "Female", na.rm=T) / nrow(responses.clean))
census.gender <- ma.distributions %>% select(p_male:p_female) %>% as.numeric()
gender.cats <- c("Male", "Female")
gender.data <- tibble(gender.cats, census.gender, gender.prop)

gender.fig <- plot_ly(gender.data, x=~gender.cats, y=~census.gender, type='bar', name='Population', marker=list(color='#fa8775')) %>% 
  add_trace(y=~gender.prop, name='Survey', marker=list(color='#0000ff')) %>% 
  layout(yaxis=list(title='Proportion', tickformat='%'),
         xaxis=list(title=NA),
         barmode='group',
         title='Gender',
         legend=list(orientation='h', xanchor='center', x=0.5))


### Age
age.bin.prop <- c(sum(responses.clean$age_bin == "18-29", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$age_bin == "30-44", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$age_bin == "45-64", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$age_bin == "65+", na.rm=T) / nrow(responses.clean))
census.age <- ma.distributions %>% select(p_18_29:p_65_plus) %>% as.numeric()
age.cats <- names(table(responses.clean$age_bin))
age.data <- tibble(age.cats, census.age, age.bin.prop)

age.fig <- plot_ly(age.data, x=~age.cats, y=~census.age, type='bar', name='Population', marker=list(color="#fa8775")) %>% 
  add_trace(y=~age.bin.prop, name='Survey', marker=list(color="#0000ff")) %>% 
  layout(yaxis=list(title='Proportion', tickformat='%'),
         xaxis=list(title=NA),
         barmode='group',
         title="Age bin",
         legend=list(orientation='h', xanchor='center', x=0.5))
###



### Hispanic
hisp.prop <- c(sum(responses.clean$hisp == "Yes", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$hisp == "No", na.rm=T) / nrow(responses.clean))
hisp <- ma.distributions %>% select(p_hisp) %>% as.numeric()
census.hisp <- c(hisp, 1-hisp)
hisp.cats <- c("Hispanic", "Non-Hispanic")
hisp.data <- tibble(hisp.cats, census.hisp, hisp.prop)

hisp.fig <- plot_ly(hisp.data, x=~hisp.cats, y=~census.hisp, type='bar', name='Population', marker=list(color="#fa8775")) %>% 
  add_trace(y=~hisp.prop, name='Survey', marker=list(color='#0000ff')) %>% 
  layout(yaxis=list(title='Proportion', tickformat='%'),
         xaxis=list(title=NA),
         barmode='group',
         title='Hispanic status',
         legend=list(orientation='h', xanchor='center', x=0.5))
###



### Race
race.prop <- c(sum(responses.clean$race == "Two or more races", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$race == "American Indian or Alaska Native", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$race == "Asian", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$race == "Black or African American", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$race == "Native Hawaiian or Pacific Islander", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$race == "White", na.rm=T) / nrow(responses.clean))

census.race <- ma.distributions %>% select(p_two_races:p_white) %>% as.numeric()
race.cats <- names(table(responses.clean$race))
race.cats <- c("Two or more", "American\nIndian", "Asian", "Black", "Native\nHawaiian", "White")
race.data <- tibble(race.cats, census.race, race.prop)

race.fig <- plot_ly(race.data, x=~race.cats, y=~census.race, type='bar', name='Population', marker=list(color="#fa8775")) %>% 
  add_trace(y=~race.prop, name='Survey', marker=list(color="#0000ff")) %>% 
  layout(yaxis=list(title='Proportion', tickformat='%'),
         xaxis=list(title=NA),
         barmode='group',
         title="Race",
         legend=list(orientation='h', xanchor='center', x=0.5))
###



### Education
educ.prop <- c(sum(responses.clean$educ == "High school diploma or equivalent (e.g. GED)", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$educ == "Some college, no degree", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$educ == "Associate degree (e.g. AA, AS)", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$educ == "Bachelor's degree (e.g. BA, BS)", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$educ %in% c("Master's degree (e.g. MA, MS, MEd)", 
                                               "Doctorate or professional degree (e.g. MD, DDS, PhD)", na.rm=T) / nrow(responses.clean)))

census.educ <- c(ma.distributions$p_ms + ma.distributions$p_hs + ma.distributions$p_hs_grad,
                 ma.distributions$p_some_college,
                 ma.distributions$p_associates,
                 ma.distributions$p_bachelors,
                 ma.distributions$p_graduate)
educ.cats <- c("High school", "Some college", "Associate", "Bachelor's", "Graduate")
educ.data <- tibble(educ.cats, census.educ, educ.prop)
educ.data$educ.cats <- factor(educ.data$educ.cats, 
                              levels = c("High school",
                                         "Some college",
                                         "Associate",
                                         "Bachelor's",
                                         "Graduate"))

educ.fig <- plot_ly(educ.data, x=~educ.cats, y=~census.educ, type='bar', name='Population', marker=list(color="#fa8775")) %>% 
  add_trace(y=~educ.prop, name='Survey', marker=list(color="#0000ff")) %>% 
  layout(yaxis=list(title='Proportion', tickformat='%'),
         xaxis=list(title=NA),
         barmode='group',
         title = "Education",
         legend = list(orientation='h', xanchor='center', x=0.5))
###


### 2016 presidential vote
# Sources: https://sos.ga.gov/admin/files/Voter%20Registration%20Statistics%20Historical%20-%20Updated%2011-26-18.pdf
# https://en.wikipedia.org/wiki/2016_United_States_presidential_election_in_Georgia
pres16.prop <- c(sum(responses.clean$pres16 == "Donald Trump, the Republican", na.rm=T) / nrow(responses.clean),
                 sum(responses.clean$pres16 == "Hillary Clinton, the Democrat", na.rm=T) / nrow(responses.clean),
                 sum(responses.clean$pres16 == "Someone else", na.rm=T) / nrow(responses.clean),
                 sum(responses.clean$pres16 == "Did not vote", na.rm=T) / nrow(responses.clean))
pres16.cats <- c("Trump", "Clinton", "Other", "Did not vote")
pres16.actual <- c(2089104 / 6637939, 1877963 / 6637939, (125306 + 13017) / 6637939, 2532549 / 6637939)
pres16.data <- tibble(pres16.cats, pres16.actual, pres16.prop)

pres16.fig <- plot_ly(pres16.data, x=~pres16.cats, y=~pres16.actual, type='bar', name='Population', marker=list(color='#fa8775')) %>% 
  add_trace(y=~pres16.prop, name='Survey', marker=list(color='#0000ff')) %>% 
  layout(yaxis=list(title='Proportion', tickformat='%'),
         xaxis=list(title=NA),
         barmode='group',
         title='2016 presidential vote',
         legend=list(orientation='h', xanchor='center', x=0.5))

###





