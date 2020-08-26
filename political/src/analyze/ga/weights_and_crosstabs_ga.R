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
responses <- read_csv(here::here("political/data/processed/ga/ga_08-20-20_responses_clean.csv"))
ga.distributions <- read_csv(here::here("political/data/processed/census/cvap_distributions.csv")) %>% 
  filter(geoname == "Georgia")

### STEP 1 - Convert to factors; impute missing values and branch dataset for RV/LV
# Drop duplicates based on IP address
responses <- responses %>% 
  distinct(ip_address, .keep_all=T) %>% 
  select(-ip_address) %>% 
  # Update incorrect spelling of DeKalb county
  mutate(county = ifelse(county == "De Kalb", "DeKalb", county))

# Note: later explore whether granular education-based weighting performs any differently from
# the blunt instrument (bachelor's vs. no bachelor's)
for (var in names(responses)[c(-1:-2)]) {
  print(var)
  print(table(responses[[var]], useNA = 'always'))
}

# We need to impute for gender, pres16 (NA and Unsure), gov18 (NA and Unsure)
responses.to.impute <- responses %>% 
  mutate(pres16 = ifelse(pres16 == "Unsure / Don't remember", NA, pres16),
         gov18 = ifelse(gov18 == "Unsure / Don't remember", NA, gov18))

responses.factors.to.impute <- responses.to.impute %>% 
  select(-c(survey_id, response_id)) %>% 
  mutate(gender = as_factor(gender),
         hisp = as_factor(hisp),
         race = as_factor(race),
         educ_bin = as_factor(educ_bin),
         age_bin = factor(age_bin, levels=c("18-29","30-44","45-64","65+")),
         pres16 = as_factor(pres16),
         gov18 = as_factor(gov18),
         county = as.factor(county))

imp <- mice(responses.factors.to.impute, maxit=0)
predM <- imp$predictorMatrix
method <- imp$method

# Set the predictor matrix to 0 for all variables we don't want to impute
for (var in names(responses.factors.to.impute)[c(-6,-12:-13)]) {
  predM[, c(var)] <- 0
}
head(predM)

# Apply imputation models based on variable type
# Ordered categorical vars
poly <- c()
method[poly] <- "polr"

# Binary vars
log <- c("gender")
method[log] <- "logreg"

# Unordered categorical vars
poly2 <- c("pres16", "gov18")
method[poly2] <- "polyreg"
method["county"] <- ""
method

# Perform imputation
imp2 <- mice(responses.factors.to.impute, maxit=5, predictorMatrix=predM,
             method=method, print=F)

# Extract imputed values
# NOTE THAT THIS APPROACH INTRODUCES BIAS BECAUSE IN THE EVENT OF A TIE, THE
# TIEBREAK IS NOT RANDOM - THE FIRST APPEARING TIED VALUE WILL BE CHOSEN!!!
gender.imp <- imp2$imp$gender %>% 
  rownames_to_column("idx") %>% 
  mutate(gender.val = apply(., 1, function(x) names(which.max(table(x, useNA='always'))))) %>% 
  select(idx, gender.val)

pres16.imp <- imp2$imp$pres16 %>% 
  rownames_to_column("idx") %>% 
  mutate(pres16.val = apply(., 1, function(x) names(which.max(table(x, useNA='always'))))) %>% 
  select(idx, pres16.val)

gov18.imp <- imp2$imp$gov18 %>% 
  rownames_to_column("idx") %>% 
  mutate(gov18.val = apply(., 1, function(x) names(which.max(table(x, useNA='always'))))) %>% 
  select(idx, gov18.val)

responses.factors <- responses.factors.to.impute %>%
  rownames_to_column("idx") %>% 
  left_join(gender.imp, by=c("idx")) %>% 
  left_join(pres16.imp, by=c("idx")) %>% 
  left_join(gov18.imp, by=c("idx")) %>% 
  mutate(gender = coalesce(gender, as.factor(gender.val)),
         pres16 = coalesce(pres16, as.factor(pres16.val)),
         gov18 = coalesce(gov18, as.factor(gov18.val))) %>% 
  select(-c(gender.val, pres16.val, gov18.val))

rv <- responses.factors %>% filter(registered == "Yes")
lv <- responses.factors %>% filter(registered == "Yes" & propensity %in% c("I am 100% certain I will vote",
                                                                           "I am likely to vote"))



### STEP 2 - Find factor levels and store population-level characteristics
# Party registration. Source: https://vt.ncsbe.gov/RegStat/Results/?date=08%2F15%2F2020
# weights::wpct(responses.factors$party_reg)
# party.wt <- c(2113806 / 7055322, 2540154 / 7055322, 47864 / 7055322, 2353498 / 7055322)

# Gender
weights::wpct(responses.factors$gender)
gender.wt <- ga.distributions %>% select(p_female, p_male) %>% as.numeric()

# Hisp
weights::wpct(responses.factors$hisp)
hisp <- ga.distributions %>% select(p_hisp) %>% as.numeric()
hisp.wt <- c(1 - hisp, hisp)

# Race
weights::wpct(responses.factors$race)
race.wt <- ga.distributions %>% select(p_white, p_black, p_am_ind, p_two_races, p_asian, p_pac) %>% as.numeric()

# Education bin
weights::wpct(responses.factors$educ_bin)
educ <- ga.distributions %>% select(p_ms:p_graduate) %>% as.numeric()
educ.wt <- c(sum(educ[1:5]), sum(educ[6:7]))

# Age bin
weights::wpct(responses.factors$age_bin)
age.wt <- ga.distributions %>% select(p_18_29:p_65_plus) %>% as.numeric()

# County
weights::wpct(responses.factors$county)
ga.county.cvap <- get_acs("county", variables = c(pop = "B29001_001"), year = 2018, state = "Georgia") %>% 
  select(NAME, estimate) %>% 
  mutate(county = gsub(" County, Georgia", "", NAME)) %>% 
  filter(county %in% names(weights::wpct(responses.factors$county))) %>% 
  mutate(total.pop = sum(estimate),
         pop.wt = estimate / total.pop)
county.wt <- ga.county.cvap[["pop.wt"]] %>% as.numeric()

# Presidential vote 2016
# Numerator (vote totals) from https://en.wikipedia.org/wiki/2016_United_States_presidential_election_in_Georgia
# Denominator (total registered voters) from https://sos.ga.gov/admin/files/Voter%20Registration%20Statistics%20Historical%20-%20Updated%2011-26-18.pdf
weights::wpct(responses.factors$pres16)
pres16.wt <- c(2089104 / 6637939, 1877963 / 6637939, 2532549 / 6637939, (125306 + 13017) / 6637939)

# House vote 2018
# Numerator (vote totals) from https://en.wikipedia.org/wiki/2018_Georgia_gubernatorial_election#Results_4
# Denominator (total registered voters) from https://sos.ga.gov/admin/files/Voter%20Registration%20Statistics%20Historical%20-%20Updated%2011-26-18.pdf
weights::wpct(responses.factors$gov18) # R - D - None - Other
gov18.wt <- c(1978408 / 6935816, 1923685 / 6935816, 2996407 / 6935816, (37235 + 81) / 6935816)

# HHINC bin
# Distribution: https://www.census.gov/library/visualizations/2016/comm/citizen_voting_age_population/cb16-tps18_georgia.html
# This is close enough to what we have in the survey that we don't need to weight
weights::wpct(responses.factors$hhinc_bin)



### STEP 3 - Define targets list, apply appropriate names and generate unique ID
#targets <- list(party.wt, gender.wt, hisp.wt, race.wt, educ.wt, age.wt, pres16.wt)
#names(targets) <- c("party_reg", "gender", "hisp", "race", "educ_bin", "age_bin", "pres16")
targets <- list(age.wt, county.wt)
names(targets) <- c("age_bin", "county")
rv$caseid <- 1:nrow(rv)
lv$caseid <- 1:nrow(lv)

#names(targets$party_reg) <- levels(responses.factors$party_reg)
#names(targets$gender) <- levels(responses.factors$gender)
#names(targets$hisp) <- levels(responses.factors$hisp)
#names(targets$race) <- levels(responses.factors$race)
#names(targets$educ_bin) <- levels(responses.factors$educ_bin)
names(targets$age_bin) <- levels(responses.factors$age_bin)
names(targets$county) <- levels(responses.factors$county)
#names(targets$pres16) <- levels(responses.factors$pres16)
#names(targets$gov18) <- levels(responses.factors$gov18)



### STEP 4 - Convert to DF and calculate weights
# Convert to data frame
rv.clean <- as.data.frame(rv)
lv.clean <- as.data.frame(lv)

# Use anesrake library to calculate the weights
weights.rv <- anesrake(targets, rv.clean, caseid = rv.clean$caseid,
                       verbose = FALSE, cap = 5, choosemethod = "total", type = "pctlim",
                       pctlim = .05, nlim = 5, iterate = TRUE, force1 = TRUE)
weights.lv <- anesrake(targets, lv.clean, caseid = lv.clean$caseid,
                       verbose = FALSE, cap = 5, choosemethod = "total", type = "pctlim",
                       pctlim = .05, nlim = 5, iterate = TRUE, force1 = TRUE)


# Add weights to df
rv$weightvec <- unlist(weights.rv[1])
lv$weightvec <- unlist(weights.lv[1])



### STEP 5 - Calculated weighted and unweighted top-lines
# Calculating MOE: https://www.dummies.com/education/math/statistics/how-to-calculate-the-margin-of-error-for-a-sample-proportion/
# REGISTERED VOTERS
n.rv <- nrow(rv)
unweighted.pres.rv <- wpct(rv$pres)
weighted.pres.rv <- wpct(rv$pres, rv$weightvec)
tab.pres.rv <- data.frame(unweighted=round(unweighted.pres.rv, 4), 
                          weighted=round(weighted.pres.rv, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.pres.rv*(1-weighted.pres.rv))/n.rv),
         cand = names(table(rv$pres)),
         office = "US President") %>% 
  select(cand, office, everything())

unweighted.sen2.rv <- wpct(rv$sen2)
weighted.sen2.rv <- wpct(rv$sen2, rv$weightvec)
tab.sen2.rv <- data.frame(unweighted=round(unweighted.sen2.rv, 4), 
                          weighted=round(weighted.sen2.rv, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.sen2.rv*(1-weighted.sen2.rv))/n.rv),
         cand = names(table(rv$sen2)),
         office = "Senate (Class 2)") %>% 
  select(cand, office, everything())

unweighted.sen3.rv <- wpct(rv$sen3)
weighted.sen3.rv <- wpct(rv$sen3, rv$weightvec)
tab.sen3.rv <- data.frame(unweighted=round(unweighted.sen3.rv, 4), 
                          weighted=round(weighted.sen3.rv, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.sen3.rv*(1-weighted.sen3.rv))/n.rv),
         cand = names(table(rv$sen3)),
         office = "Senate (Class 3)") %>% 
  select(cand, office, everything())

tab.rv <- base::rbind(tab.pres.rv, tab.sen2.rv, tab.sen3.rv)



# LIKELY VOTERS
n.lv <- nrow(lv)
unweighted.pres.lv <- wpct(lv$pres)
weighted.pres.lv <- wpct(lv$pres, lv$weightvec)
tab.pres.lv <- data.frame(unweighted=round(unweighted.pres.lv, 4), 
                          weighted=round(weighted.pres.lv, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.pres.lv*(1-weighted.pres.lv))/n.lv),
         cand = names(table(lv$pres)),
         office = "US President") %>% 
  select(cand, office, everything())

unweighted.sen2.lv <- wpct(lv$sen2)
weighted.sen2.lv <- wpct(lv$sen2, lv$weightvec)
tab.sen2.lv <- data.frame(unweighted=round(unweighted.sen2.lv, 4), 
                          weighted=round(weighted.sen2.lv, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.sen2.lv*(1-weighted.sen2.lv))/n.lv),
         cand = names(table(lv$sen2)),
         office = "Senate (Class 2)") %>% 
  select(cand, office, everything())

unweighted.sen3.lv <- wpct(lv$sen3)
weighted.sen3.lv <- wpct(lv$sen3, lv$weightvec)
tab.sen3.lv <- data.frame(unweighted=round(unweighted.sen3.lv, 4), 
                          weighted=round(weighted.sen3.lv, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.sen3.lv*(1-weighted.sen3.lv))/n.lv),
         cand = names(table(lv$sen3)),
         office = "Senate (Class 3)") %>% 
  select(cand, office, everything())

tab.lv <- base::rbind(tab.pres.lv, tab.sen2.lv, tab.sen3.lv)








### STEP 6 - Weighted crosstabs
age.ct <- as.data.frame(questionr::wtd.table(x=rv$pres,
                                             y=rv$age_bin,
                                             weights=rv$weightvec)) %>% 
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
  select(-rowtotal) %>% 
  as_tibble()



educ.ct <- as.data.frame(questionr::wtd.table(x=rv$pres,
                                              y=rv$educ_bin,
                                              weights=rv$weightvec)) %>% 
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



race.ct <- as.data.frame(questionr::wtd.table(x=rv$pres,
                                              y=rv$race,
                                              weights=rv$weightvec)) %>% 
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




tab.rv