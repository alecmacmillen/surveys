library(weights)
library(anesrake)
library(tidyverse)
library(tidylog)
library(assertr)
library(here)
library(plotly)
library(tidycensus)
library(questionr)

rm(list = ls())

responses <- read_csv(here::here("political/data/processed/nc/nc_08-10-20_responses_clean.csv"))
nc.distributions <- read_csv(here::here("political/data/processed/census/cvap_distributions.csv")) %>% 
  filter(geoname == "North Carolina")


### STEP 1 - Convert weight variables to factors
# Note: later explore whether granular education-based weighting performs any differently from
# the blunt instrument (bachelor's vs. no bachelor's)
responses.factors <- responses %>% 
  mutate(party_reg = as_factor(party_reg),
         gender = as_factor(gender),
         hisp = as_factor(hisp),
         race = as_factor(race),
         educ_bin = as_factor(educ_bin),
         age_bin = factor(age_bin, levels=c("18-29","30-44","45-64","65+")),
         pres16 = as_factor(pres16),
         house18 = as_factor(house18),
         county = as_factor(county))

### STEP 2 - Find factor levels and store population-level characteristics
# Party registration. Source: https://vt.ncsbe.gov/RegStat/Results/?date=08%2F15%2F2020
weights::wpct(responses.factors$party_reg)
party.wt <- c(2113806 / 7055322, 2540154 / 7055322, 47864 / 7055322, 2353498 / 7055322)

# Gender
weights::wpct(responses.factors$gender)
gender.wt <- nc.distributions %>% select(p_male:p_female) %>% as.numeric()

# Hisp
weights::wpct(responses.factors$hisp)
hisp <- nc.distributions %>% select(p_hisp) %>% as.numeric()
hisp.wt <- c(1 - hisp, hisp)

# Race
weights::wpct(responses.factors$race)
race.wt <- nc.distributions %>% select(p_white, p_black, p_am_ind, p_asian) %>% as.numeric()

# Education bin
weights::wpct(responses.factors$educ_bin)
educ <- nc.distributions %>% select(p_ms:p_graduate) %>% as.numeric()
educ.wt <- c(sum(educ[1:5]), sum(educ[6:7]))

# Age bin
weights::wpct(responses.factors$age_bin)
age.wt <- nc.distributions %>% select(p_18_29:p_65_plus) %>% as.numeric()

# Presidential vote 2016
# Numerator (vote totals) from https://en.wikipedia.org/wiki/2016_United_States_presidential_election_in_North_Carolina#Results_4
# Denominator (total registered voters) from https://vt.ncsbe.gov/RegStat/Results/?date=11%2F08%2F2016
weights::wpct(responses.factors$pres16)
pres16.wt <- c(2362631 / 6918150, 2189316 / 6918150, 2176586 / 6918150, (130126 + 47386 + 12105) / 6918150)

# House vote 2018
# FOR NOW, EXCLUDE - HOW DO YOU ACCOUNT FOR "Unsure / Don't remember" IN APPLYING WEIGHTS?
# Numerator (vote totals) from https://en.wikipedia.org/wiki/2018_United_States_House_of_Representatives_elections_in_North_Carolina
# Denominator (total registered voters) from https://vt.ncsbe.gov/RegStat/Results/?date=11%2F06%2F2018
weights::wpct(responses.factors$house18)
house18.wt <- c()



### STEP 3 - Define targets list, apply appropriate names and generate unique ID
#targets <- list(party.wt, gender.wt, hisp.wt, race.wt, educ.wt, age.wt, pres16.wt)
#names(targets) <- c("party_reg", "gender", "hisp", "race", "educ_bin", "age_bin", "pres16")
targets <- list(educ.wt, age.wt, pres16.wt)
names(targets) <- c("educ_bin", "age_bin", "pres16")
responses.factors$caseid <- 1:nrow(responses.factors)

#names(targets$party_reg) <- levels(responses.factors$party_reg)
#names(targets$gender) <- levels(responses.factors$gender)
#names(targets$hisp) <- levels(responses.factors$hisp)
#names(targets$race) <- levels(responses.factors$race)
names(targets$educ_bin) <- levels(responses.factors$educ_bin)
names(targets$age_bin) <- levels(responses.factors$age_bin)
names(targets$pres16) <- levels(responses.factors$pres16)



### STEP 4 - Convert to DF and calculate weights
# Convert to data frame
clean.df <- as.data.frame(responses.factors)

# Use anesrake library to calculate the weights
calc.weights <- anesrake(targets, clean.df, caseid = clean.df$caseid,
                         verbose = FALSE, cap = 5, choosemethod = "total", type = "pctlim",
                         pctlim = .05, nlim = 5, iterate = TRUE, force1 = TRUE)

# Add weights to df
responses.factors$weightvec <- unlist(calc.weights[1])



### STEP 5 - Calculated weighted and unweighted top-lines
# Calculating MOE: https://www.dummies.com/education/math/statistics/how-to-calculate-the-margin-of-error-for-a-sample-proportion/
n <- nrow(responses.factors)
unweighted.pres <- wpct(responses.factors$pres)
weighted.pres <- wpct(responses.factors$pres, responses.factors$weightvec)
tab.pres <- data.frame(round(unweighted.pres, 4), round(weighted.pres, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.pres*(1-weighted.pres))/n),
         cand = names(table(responses.factors$pres))) %>% 
  select(cand, everything())
tab.pres

unweighted.gov <- wpct(responses.factors$gov)
weighted.gov <- wpct(responses.factors$gov, responses.factors$weightvec)
tab.gov <- data.frame(round(unweighted.gov, 4), round(weighted.gov, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.gov*(1-weighted.gov))/n),
         cand = names(table(responses.factors$gov))) %>% 
  select(cand, everything())
tab.gov

unweighted.sen <- wpct(responses.factors$sen)
weighted.sen <- wpct(responses.factors$sen, responses.factors$weightvec)
tab.sen <- data.frame(round(unweighted.sen, 4), round(weighted.sen, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.sen*(1-weighted.sen))/n),
         cand = names(table(responses.factors$sen))) %>% 
  select(cand, everything())
tab.sen



### STEP 6 - Weighted crosstabs


age.ct <- as.data.frame(questionr::wtd.table(x=responses.factors$pres,
                                             y=responses.factors$age_bin,
                                             weights=responses.factors$weightvec)) %>% 
  pivot_wider(names_from=c("Var1"), values_from=c("Freq")) %>% 
  rename(age_group = Var2,
         trump = `Donald Trump, the Republican`,
         biden = `Joe Biden, the Democrat`,
         other = `Some other candidate`,
         undecided = `Undecided`) %>% 
  mutate(rowtotal = trump + biden + other + undecided,
         trump = trump/rowtotal,
         biden = biden/rowtotal,
         other = other/rowtotal,
         undecided = undecided/rowtotal) %>% 
  select(-rowtotal)



educ.ct <- as.data.frame(questionr::wtd.table(x=responses.factors$pres,
                                              y=responses.factors$educ_bin,
                                              weights=responses.factors$weightvec)) %>% 
  pivot_wider(names_from=c("Var1"), values_from=c("Freq")) %>% 
  rename(age_group = Var2,
         trump = `Donald Trump, the Republican`,
         biden = `Joe Biden, the Democrat`,
         other = `Some other candidate`,
         undecided = `Undecided`) %>% 
  mutate(rowtotal = trump + biden + other + undecided,
         trump = trump/rowtotal,
         biden = biden/rowtotal,
         other = other/rowtotal,
         undecided = undecided/rowtotal) %>% 
  select(-rowtotal)



race.ct <- as.data.frame(questionr::wtd.table(x=responses.factors$pres,
                                              y=responses.factors$race,
                                              weights=responses.factors$weightvec)) %>% 
  pivot_wider(names_from=c("Var1"), values_from=c("Freq")) %>% 
  rename(race = Var2,
         trump = `Donald Trump, the Republican`,
         biden = `Joe Biden, the Democrat`,
         other = `Some other candidate`,
         undecided = `Undecided`) %>% 
  mutate(rowtotal = trump + biden + other + undecided,
         trump = trump/rowtotal,
         biden = biden/rowtotal,
         other = other/rowtotal,
         undecided = undecided/rowtotal) %>% 
  select(-rowtotal)




