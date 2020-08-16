

library(weights)
library(anesrake)
library(tidyverse)
library(tidylog)
library(assertr)
library(here)
library(plotly)
library(tidycensus)
rm(list = ls())

nc.distributions <- read_csv(here::here("political/data/processed/census/cvap_distributions.csv")) %>% 
  filter(geoname == "North Carolina")

responses <- read_csv(here::here("political/data/raw/nc/nc_08-10-20_1_responses.csv"))

# Function for a "clean" version of the data with binned variables
responses.clean <- responses %>% 
  filter(propensity %in% c("I am 100% certain I will vote", "I am likely to vote")) %>% 
  mutate(age_bin = case_when(dplyr::between(age, 18, 29) ~ "18-29",
                             dplyr::between(age, 30, 44) ~ "30-44",
                             dplyr::between(age, 45, 64) ~ "45-64",
                             TRUE ~ "65+"),
         educ_bin = case_when(educ %in% c("High school degree or equivalent (e.g. GED)",
                                          "Some college, no degree",
                                          "Associate degree (e.g. AA, AS)") ~ "no_bachelors",
                              TRUE ~ "bachelors"))


party.prop <- c(sum(responses.clean$party_reg == "Democratic", na.rm=T) / nrow(responses.clean),
                sum(responses.clean$party_reg == "Republican", na.rm=T) / nrow(responses.clean),
                sum(responses.clean$party_reg == "Other", na.rm=T) / nrow(responses.clean),
                sum(responses.clean$party_reg == "Unaffiliated", na.rm=T) / nrow(responses.clean))

sex.prop <- c(sum(responses.clean$gender == "Male", na.rm=T) / nrow(responses.clean),
              sum(responses.clean$gender == "Female", na.rm=T) / nrow(responses.clean))

###
age.bin.prop <- c(sum(responses.clean$age_bin == "18-29", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$age_bin == "30-44", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$age_bin == "45-64", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$age_bin == "65+", na.rm=T) / nrow(responses.clean))
census.age <- nc.distributions %>% select(p_18_29:p_65_plus) %>% as.numeric()
age.cats <- names(table(responses.clean$age_bin))
age.data <- tibble(age.cats, census.age, age.bin.prop)

age.fig <- plot_ly(age.data, x=~age.cats, y=~census.age, type='bar', name='Population', marker=list(color="#ffd700")) %>% 
  add_trace(y=~age.bin.prop, name='Survey', marker=list(color="#cd34b5")) %>% 
  layout(yaxis=list(title='Proportion', tickformat='%'),
         xaxis=list(title='Age bin'),
         barmode='group',
         title="Age bin")
###

hisp.prop <- c(sum(responses.clean$hisp == "Yes", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$hisp == "No", na.rm=T) / nrow(responses.clean))

###
race.prop <- c(sum(responses.clean$race == "American Indian or Alaska Native", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$race == "Asian", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$race == "Black or African American", na.rm=T) / nrow(responses.clean),
               sum(responses.clean$race == "White", na.rm=T) / nrow(responses.clean))

census.race <- nc.distributions %>% select(p_am_ind:p_black, p_white) %>% as.numeric()
race.cats <- names(table(responses.clean$race))
race.data <- tibble(race.cats, census.race, race.prop)

race.fig <- plot_ly(race.data, x=~race.cats, y=~census.race, type='bar', name='Population', marker=list(color="#ffb14e")) %>% 
  add_trace(y=~race.prop, name='Survey', marker=list(color="#9d02d7")) %>% 
  layout(yaxis=list(title='Proportion', tickformat='%'),
         xaxis=list(title='Race'),
         barmode='group',
         title="Race")
###

###
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
educ.cats <- c("High school degree or equivalent (e.g. GED)",
               "Some college, no degree",
               "Associate degree (e.g. AA, AS)",
               "Bachelor's degree (e.g. BA, BS)",
               "Graduates")
educ.data <- tibble(educ.cats, census.educ, educ.prop)
educ.data$educ.cats <- factor(educ.data$educ.cats, 
                              levels = c("High school degree or equivalent (e.g. GED)",
                                         "Some college, no degree",
                                         "Associate degree (e.g. AA, AS)",
                                         "Bachelor's degree (e.g. BA, BS)",
                                         "Graduates"))

educ.fig <- plot_ly(educ.data, x=~educ.cats, y=~census.educ, type='bar', name='Population', marker=list(color="#fa8775")) %>% 
  add_trace(y=~educ.prop, name='Survey', marker=list(color="#0000ff")) %>% 
  layout(yaxis=list(title='Proportion', tickformat='%'),
         xaxis=list(title='Education level'),
         barmode='group',
         title = "Education")
###



pres16.prop <- c(sum(responses.clean$pres16 == "Donald Trump, the Republican", na.rm=T) / nrow(responses.clean),
                 sum(responses.clean$pres16 == "Hillary Clinton, the Democrat", na.rm=T) / nrow(responses.clean),
                 sum(responses.clean$pres16 == "Someone else", na.rm=T) / nrow(responses.clean),
                 sum(responses.clean$pres16 == "Did not vote", na.rm=T) / nrow(responses.clean))

house18.prop <- c(sum(responses.clean$house18 == "The Republican party candidate", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$house18 == "The Democratic party candidate", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$house18 == "Did not vote", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$house18 == "Someone else", na.rm=T) / nrow(responses.clean),
                  sum(responses.clean$house18 == "Unsure / Don't remember", na.rm=T) / nrow(responses.clean))

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



### Combining plots
