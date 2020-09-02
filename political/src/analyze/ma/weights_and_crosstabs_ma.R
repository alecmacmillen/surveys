library(weights)
library(anesrake)
library(tidyverse)
library(tidylog)
library(assertr)
library(here)
library(plotly)
library(tidycensus)
library(questionr)
library(mice)

rm(list = ls())
set.seed(10)
responses <- read_csv(here::here("political/data/processed/ma/ma_08-26-20_dem_responses_clean.csv"))
metadata <- read_csv(here::here("political/data/processed/ma/ma_08-26-20_metadata_clean.csv"))
ma.distributions <- read_csv(here::here("political/data/processed/census/cvap_distributions.csv")) %>% 
  filter(geoname == "Massachusetts")

### STEP 1 - Convert to factors; impute missing values and branch dataset for RV/LV
# Drop duplicates based on IP address
responses <- responses %>% 
  left_join(select(metadata, response_id, ip_address), by=c("response_id")) %>% 
  distinct(ip_address, .keep_all=T) %>% 
  select(-ip_address)

# Note: later explore whether granular education-based weighting performs any differently from
# the blunt instrument (bachelor's vs. no bachelor's)
for (var in names(responses)[c(-1:-2)]) {
  print(var)
  print(table(responses[[var]], useNA = 'always'))
}

# We don't need to impute anything!
responses.factors <- responses %>%
  mutate(gender = as.factor(gender),
         hisp = as.factor(hisp),
         race = as.factor(race),
         county = as.factor(county),
         age_bin = as.factor(age_bin),
         educ_bin = as.factor(educ_bin),
         hhinc_bin = as.factor(hhinc_bin))

rv <- responses.factors %>% filter(registered == "Yes")



### STEP 2 - Find factor levels and store population-level characteristics
# Party registration. Source: https://vt.ncsbe.gov/RegStat/Results/?date=08%2F15%2F2020
# weights::wpct(responses.factors$party_reg)
# party.wt <- c(2113806 / 7055322, 2540154 / 7055322, 47864 / 7055322, 2353498 / 7055322)

# Gender
weights::wpct(responses.factors$gender)
gender.wt <- ma.distributions %>% select(p_female, p_male) %>% as.numeric()
gender.wt[1] <- gender.wt[1] - .005
gender.wt[2] <- gender.wt[2] - .005
gender.wt[3] <- .01

# Hisp
weights::wpct(responses.factors$hisp)
hisp <- ma.distributions %>% select(p_hisp) %>% as.numeric()
hisp.wt <- c(1 - hisp, hisp)

# Race
weights::wpct(responses.factors$race)
race.wt <- ma.distributions %>% select(p_asian, p_black, p_two_races, p_white) %>% as.numeric()

# Education bin
weights::wpct(responses.factors$educ_bin)
educ <- ma.distributions %>% select(p_ms:p_graduate) %>% as.numeric()
educ.wt <- c(sum(educ[6:7]), sum(educ[1:5]))

# Age bin
weights::wpct(responses.factors$age_bin)
age.wt <- ma.distributions %>% select(p_18_29:p_65_plus) %>% as.numeric()

# County
weights::wpct(responses.factors$county)
ma.county.cvap <- get_acs("county", variables = c(pop = "B29001_001"), year = 2018, state = "Massachusetts") %>% 
  select(NAME, estimate) %>% 
  mutate(county = gsub(" County, Massachusetts", "", NAME)) %>% 
  filter(county %in% names(weights::wpct(responses.factors$county))) %>% 
  mutate(total.pop = sum(estimate),
         pop.wt = estimate / total.pop)
county.wt <- ma.county.cvap[["pop.wt"]] %>% as.numeric()

# HHINC bin
# Distribution: https://www.census.gov/library/visualizations/2016/comm/citizen_voting_age_population/cb16-tps18_georgia.html
# This is close enough to what we have in the survey that we don't need to weight
weights::wpct(responses.factors$hhinc_bin)



### STEP 3 - Define targets list, apply appropriate names and generate unique ID
#targets <- list(party.wt, gender.wt, hisp.wt, race.wt, educ.wt, age.wt, pres16.wt)
#names(targets) <- c("party_reg", "gender", "hisp", "race", "educ_bin", "age_bin", "pres16")
targets <- list(age.wt, educ.wt, county.wt)
names(targets) <- c("age_bin", "educ_bin", "county")
rv$caseid <- 1:nrow(rv)

#names(targets$party_reg) <- levels(responses.factors$party_reg)
#names(targets$gender) <- levels(responses.factors$gender)
#names(targets$hisp) <- levels(responses.factors$hisp)
#names(targets$race) <- levels(responses.factors$race)
names(targets$age_bin) <- levels(responses.factors$age_bin)
names(targets$educ_bin) <- levels(responses.factors$educ_bin)
names(targets$county) <- levels(responses.factors$county)



### STEP 4 - Convert to DF and calculate weights
# Convert to data frame
rv.clean <- as.data.frame(rv)

# Use anesrake library to calculate the weights
weights.rv <- anesrake(targets, rv.clean, caseid = rv.clean$caseid,
                       verbose = FALSE, cap = 5, choosemethod = "total", type = "pctlim",
                       pctlim = .05, nlim = 5, iterate = TRUE, force1 = TRUE)

# Add weights to df
rv$weightvec <- unlist(weights.rv[1])


### STEP 5 - Calculated weighted and unweighted top-lines
# Calculating MOE: https://www.dummies.com/education/math/statistics/how-to-calculate-the-margin-of-error-for-a-sample-proportion/
# REGISTERED VOTERS
n.rv <- nrow(rv)
unweighted.sen.rv <- wpct(rv$dem)
weighted.sen.rv <- wpct(rv$dem, rv$weightvec)
tab.sen.rv <- data.frame(unweighted=round(unweighted.sen.rv, 4), 
                         weighted=round(weighted.sen.rv, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.sen.rv*(1-weighted.sen.rv))/n.rv),
         cand = names(table(rv$dem)),
         office = "Senate (Democratic Primary)") %>% 
  select(cand, office, everything())

tab.sen.rv







### STEP 6 - Weighted crosstabs
age.ct <- as.data.frame(questionr::wtd.table(x=rv$dem,
                                             y=rv$age_bin,
                                             weights=rv$weightvec)) %>% 
  pivot_wider(names_from=c("Var1"), values_from=c("Freq")) %>% 
  rename(age_group = Var2,
         markey = `Ed Markey`,
         kennedy = `Joe Kennedy III`,
         other = `Some other candidate`,
         undecided = `Undecided`) %>% 
  mutate(rowtotal = markey + kennedy + other + undecided,
         markey = markey/rowtotal,
         kennedy = kennedy/rowtotal,
         other = other/rowtotal,
         undecided = undecided/rowtotal) %>% 
  select(-rowtotal) %>% 
  as_tibble()



educ.ct <- as.data.frame(questionr::wtd.table(x=rv$dem,
                                              y=rv$educ_bin,
                                              weights=rv$weightvec)) %>% 
  pivot_wider(names_from=c("Var1"), values_from=c("Freq")) %>% 
  rename(age_group = Var2,
         markey = `Ed Markey`,
         kennedy = `Joe Kennedy III`,
         other = `Some other candidate`,
         undecided = `Undecided`) %>% 
  mutate(rowtotal = markey + kennedy + other + undecided,
         markey = markey/rowtotal,
         kennedy = kennedy/rowtotal,
         other = other/rowtotal,
         undecided = undecided/rowtotal) %>% 
  select(-rowtotal)



race.ct <- as.data.frame(questionr::wtd.table(x=rv$dem,
                                              y=rv$race,
                                              weights=rv$weightvec)) %>% 
  pivot_wider(names_from=c("Var1"), values_from=c("Freq")) %>% 
  rename(race = Var2,
         markey = `Ed Markey`,
         kennedy = `Joe Kennedy III`,
         other = `Some other candidate`,
         undecided = `Undecided`) %>% 
  mutate(rowtotal = markey + kennedy + other + undecided,
         markey = markey/rowtotal,
         kennedy = kennedy/rowtotal,
         other = other/rowtotal,
         undecided = undecided/rowtotal) %>% 
  select(-rowtotal)


