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

# WEIGHTED ANALYSIS
# Crosstabs
# Toplines

nc.distributions <- read_csv(here::here("political/data/processed/census/cvap_distributions.csv")) %>% 
  filter(geoname == "North Carolina")

responses <- read_csv(here::here("political/data/raw/nc/nc_08-10-20_1_responses_raw.csv"))
responses2 <- read_csv(here::here("political/data/raw/nc/nc_08-10-20_2_responses_raw.csv"))
responses3 <- read_csv(here::here("political/data/raw/nc/nc_08-10-20_3_responses_raw.csv"))

metadata <- read_csv(here::here("political/data/raw/nc/nc_08-10-20_1_metadata_raw.csv"))
metadata2 <- read_csv(here::here("political/data/raw/nc/nc_08-10-20_2_metadata_raw.csv"))
metadata3 <- read_csv(here::here("political/data/raw/nc/nc_08-10-20_3_metadata_raw.csv"))


# Validate proportion of responses with lat/lon in North Carolina
metadata.clean <- metadata %>% 
  bind_rows(metadata2) %>% 
  mutate(location = revgeo::revgeo(long, lat)) %>% 
  separate(location, into = c("address", "city", "state", "zip", "country"), sep = ",") %>% 
  mutate(state = str_trim(state)) %>% 
  select(-c(address, city, zip, country))

in.nc <- sum(metadata.clean$state == "North Carolina", na.rm = T) / nrow(metadata.clean)


# Function for a "clean" version of the data with binned variables
responses.clean <- responses %>% 
  bind_rows(responses2, responses3) %>% 
  filter(registered == "Yes" & propensity %in% c("I am 100% certain I will vote", "I am likely to vote")) %>% 
  # Only filter out unsure / don't remember responses for pres16 if you're actually going to weight on it in the
  # toplines and crosstabs
  filter(pres16 != "Unsure / Don't remember") %>% 
  mutate(age_bin = case_when(dplyr::between(age, 18, 29) ~ "18-29",
                             dplyr::between(age, 30, 44) ~ "30-44",
                             dplyr::between(age, 45, 64) ~ "45-64",
                             TRUE ~ "65+"),
         educ_bin = case_when(educ %in% c("High school degree or equivalent (e.g. GED)",
                                          "Some college, no degree",
                                          "Associate degree (e.g. AA, AS)") ~ "no_bachelors",
                              TRUE ~ "bachelors"))

# Save cleaned metadata and response files
write_csv(metadata.clean, here::here("political/data/processed/nc/nc_08-10-20_metadata_clean.csv"))
write_csv(responses.clean, here::here("political/data/processed/nc/nc_08-10-20_responses_clean.csv"))

# Calculate sample proportions and stack up against population proportions
### Party registration
party.prop <- c(sum(responses.clean$party_reg == "Democratic", na.rm=T) / nrow(responses.clean),
                sum(responses.clean$party_reg == "Republican", na.rm=T) / nrow(responses.clean),
                sum(responses.clean$party_reg == "Other", na.rm=T) / nrow(responses.clean),
                sum(responses.clean$party_reg == "Unaffiliated", na.rm=T) / nrow(responses.clean))
party.reg <- c(2540154 / 7055322, 2113806 / 7055322, 47864 / 7055322, 2353498 / 7055322)
party.cats <- c("Democratic", "Republican", "Other", "Unaffiliated")
party.data <- tibble(party.cats, party.reg, party.prop)

party.fig <- plot_ly(party.data, x=~party.cats, y=~party.reg, type='bar', name='Population', marker=list(color="#fa8775")) %>% 
  add_trace(y=~party.prop, name='Survey', marker=list(color="#0000ff")) %>% 
  layout(yaxis=list(title='Proportion', tickformat='%'),
         xaxis=list(title=NA),
         barmode='group',
         title='Party registration',
         legend=list(orientation='h', xanchor='center', x=0.5))
###

### Gender
gender.prop <- c(sum(responses.clean$gender == "Male", na.rm=T) / nrow(responses.clean),
              sum(responses.clean$gender == "Female", na.rm=T) / nrow(responses.clean))
census.gender <- nc.distributions %>% select(p_male:p_female) %>% as.numeric()
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
census.age <- nc.distributions %>% select(p_18_29:p_65_plus) %>% as.numeric()
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
hisp <- nc.distributions %>% select(p_hisp) %>% as.numeric()
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
race.prop <- c(sum(responses.clean$race == "American Indian or Alaska Native", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$race == "Asian", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$race == "Black or African American", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$race == "White", na.rm=T) / nrow(responses.clean))

census.race <- nc.distributions %>% select(p_am_ind:p_black, p_white) %>% as.numeric()
race.cats <- names(table(responses.clean$race))
race.cats <- c("American\nIndian", "Asian", "Black", "White")
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
educ.prop <- c(sum(responses.clean$educ == "High school degree or equivalent (e.g. GED)", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$educ == "Some college, no degree", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$educ == "Associate degree (e.g. AA, AS)", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$educ == "Bachelor's degree (e.g. BA, BS)", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$educ %in% c("Master's degree (e.g. MA, MS, MEd)", 
                                               "Doctorate or professional degree (e.g. MD, DDS, PhD)", na.rm=T) / nrow(responses.clean)))

census.educ <- c(nc.distributions$p_ms + nc.distributions$p_hs + nc.distributions$p_hs_grad,
                 nc.distributions$p_some_college,
                 nc.distributions$p_associates,
                 nc.distributions$p_bachelors,
                 nc.distributions$p_graduate)
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
pres16.prop <- c(sum(responses.clean$pres16 == "Donald Trump, the Republican", na.rm=T) / nrow(responses.clean),
                 sum(responses.clean$pres16 == "Hillary Clinton, the Democrat", na.rm=T) / nrow(responses.clean),
                 sum(responses.clean$pres16 == "Someone else", na.rm=T) / nrow(responses.clean),
                 sum(responses.clean$pres16 == "Did not vote", na.rm=T) / nrow(responses.clean))
pres16.cats <- c("Trump", "Clinton", "Other", "Did not vote")
pres16.actual <- c(2362631 / 6918150, 2189316 / 6918150, (130126 + 47386 + 12105) / 6918150, 2176586 / 6918150)
pres16.data <- tibble(pres16.cats, pres16.actual, pres16.prop)

pres16.fig <- plot_ly(pres16.data, x=~pres16.cats, y=~pres16.actual, type='bar', name='Population', marker=list(color='#fa8775')) %>% 
  add_trace(y=~pres16.prop, name='Survey', marker=list(color='#0000ff')) %>% 
  layout(yaxis=list(title='Proportion', tickformat='%'),
         xaxis=list(title=NA),
         barmode='group',
         title='2016 presidential vote',
         legend=list(orientation='h', xanchor='center', x=0.5))

###




house18.prop <- c(sum(responses.clean$house18 == "The Republican party candidate", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$house18 == "The Democratic party candidate", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$house18 == "Did not vote", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$house18 == "Someone else", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$house18 == "Unsure / Don't remember", na.rm=T) / nrow(responses.clean))



### Outcome vars
pres.prop <- c(sum(responses.clean$pres == "Donald Trump, the Republican", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$pres == "Joe Biden, the Democrat", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$pres == "Some other candidate", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$pres == "Undecided", na.rm=T) / nrow(responses.clean))

gov.prop <- c(sum(responses.clean$gov == "Dan Forest, the Republican", na.rm=T) / nrow(responses.clean),
              sum(responses.clean$gov == "Roy Cooper, the Democrat", na.rm=T) / nrow(responses.clean),
              sum(responses.clean$gov == "Some other candidate", na.rm=T) / nrow(responses.clean),
              sum(responses.clean$gov == "Undecided", na.rm=T) / nrow(responses.clean))

sen.prop <- c(sum(responses.clean$sen == "Thom Tillis, the Republican", na.rm=T) / nrow(responses.clean),
              sum(responses.clean$sen == "Cal Cunningham, the Democrat", na.rm=T) / nrow(responses.clean),
              sum(responses.clean$sen == "Some other candidate", na.rm=T) / nrow(responses.clean),
              sum(responses.clean$sen == "Undecided", na.rm=T) / nrow(responses.clean))






