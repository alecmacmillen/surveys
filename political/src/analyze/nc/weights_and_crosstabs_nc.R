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

responses <- read_csv(here::here("political/data/processed/nc/nc_08-10-20_responses_clean.csv"))
nc.distributions <- read_csv(here::here("political/data/processed/census/cvap_distributions.csv")) %>% 
  filter(geoname == "North Carolina")

### STEP 1 - Convert to factors; impute missing values and branch dataset for RV/LV
# Note: later explore whether granular education-based weighting performs any differently from
# the blunt instrument (bachelor's vs. no bachelor's)
for (var in names(responses)[c(-1:-2)]) {
  print(var)
  print(table(responses[[var]], useNA = 'always'))
}

# We need to impute for registered, gender, hisp, pres16 (NA and Unsure), house18 (NA and Unsure)
responses.to.impute <- responses %>% 
  mutate(pres16 = ifelse(pres16 == "Unsure / Don't remember", NA, pres16),
         house18 = ifelse(house18 == "Unsure / Don't remember", NA, house18))

responses.factors.to.impute <- responses.to.impute %>% 
  mutate(party_reg = as_factor(party_reg),
         gender = as_factor(gender),
         hisp = as_factor(hisp),
         race = as_factor(race),
         educ_bin = as_factor(educ_bin),
         age_bin = factor(age_bin, levels=c("18-29","30-44","45-64","65+")),
         pres16 = as_factor(pres16),
         house18 = as_factor(house18),
         county = as.factor(county))

imp <- mice(responses.factors.to.impute, maxit=0)
predM <- imp$predictorMatrix
method <- imp$method

# Set the predictor matrix to 0 for all variables we don't want to impute
for (var in names(responses.factors.to.impute)[c(-3,-10:-11,-14:-15)]) {
  predM[, c(var)] <- 0
}
head(predM)

# Apply imputation models based on variable type
# Ordered categorical vars
poly <- c()
method[poly] <- "polr"

# Binary vars
log <- c("registered", "gender", "hisp")
method[log] <- "logreg"

# Unordered categorical vars
poly2 <- c("pres16", "house18")
method[poly2] <- "polyreg"
method

# Perform imputation
imp2 <- mice(responses.factors.to.impute, maxit=5, predictorMatrix=predM,
             method=method, print=F)

# Extract imputed values
# NOTE THAT THIS APPROACH INTRODUCES BIAS BECAUSE IN THE EVENT OF A TIE, THE
# TIEBREAK IS NOT RANDOM - THE FIRST APPEARING TIED VALUE WILL BE CHOSEN!!!
registered.imp <- imp2$imp$registered %>% 
  rownames_to_column("idx") %>% 
  mutate(registered.val = apply(., 1, function(x) names(which.max(table(x, useNA='always'))))) %>% 
  select(idx, registered.val)

hisp.imp <- imp2$imp$hisp %>% 
  rownames_to_column("idx") %>% 
  mutate(hisp.val = apply(., 1, function(x) names(which.max(table(x, useNA='always'))))) %>% 
  select(idx, hisp.val)

gender.imp <- imp2$imp$gender %>% 
  rownames_to_column("idx") %>% 
  mutate(gender.val = apply(., 1, function(x) names(which.max(table(x, useNA='always'))))) %>% 
  select(idx, gender.val)

pres16.imp <- imp2$imp$pres16 %>% 
  rownames_to_column("idx") %>% 
  mutate(pres16.val = apply(., 1, function(x) names(which.max(table(x, useNA='always'))))) %>% 
  select(idx, pres16.val)

house18.imp <- imp2$imp$house18 %>% 
  rownames_to_column("idx") %>% 
  mutate(house18.val = apply(., 1, function(x) names(which.max(table(x, useNA='always'))))) %>% 
  select(idx, house18.val)

responses.factors <- responses.factors.to.impute %>%
  rownames_to_column("idx") %>% 
  left_join(hisp.imp, by=c("idx")) %>% 
  left_join(gender.imp, by=c("idx")) %>% 
  left_join(pres16.imp, by=c("idx")) %>% 
  left_join(house18.imp, by=c("idx")) %>% 
  mutate(hisp = coalesce(hisp, as.factor(hisp.val)),
         gender = coalesce(gender, as.factor(gender.val)),
         pres16 = coalesce(pres16, as.factor(pres16.val)),
         house18 = coalesce(house18, as.factor(house18.val))) %>% 
  select(-c(hisp.val, gender.val, pres16.val, house18.val))

rv <- responses.factors %>% filter(registered == "Yes")
lv <- responses.factors %>% filter(registered == "Yes" & propensity %in% c("I am 100% certain I will vote",
                                                                           "I am likely to vote"))



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

# County
weights::wpct(responses.factors$county)
nc.county.cvap <- get_acs("county", variables = c(pop = "B29001_001"), year = 2018, state = "North Carolina") %>% 
  select(NAME, estimate) %>% 
  mutate(county = gsub(" County, North Carolina", "", NAME)) %>% 
  filter(county %in% names(weights::wpct(responses.factors$county))) %>% 
  mutate(total.pop = sum(estimate),
         pop.wt = estimate / total.pop) %>% 
  select(pop.wt)
county.wt <- nc.county.cvap[["pop.wt"]] %>% as.numeric()

# Presidential vote 2016
# Numerator (vote totals) from https://en.wikipedia.org/wiki/2016_United_States_presidential_election_in_North_Carolina#Results_4
# Denominator (total registered voters) from https://vt.ncsbe.gov/RegStat/Results/?date=11%2F08%2F2016
weights::wpct(responses.factors$pres16)
pres16.wt <- c(2362631 / 6918150, 2189316 / 6918150, 2176586 / 6918150, (130126 + 47386 + 12105) / 6918150)

# House vote 2018
# FOR NOW, EXCLUDE - HOW DO YOU ACCOUNT FOR "Unsure / Don't remember" IN APPLYING WEIGHTS?
# Numerator (vote totals) from https://en.wikipedia.org/wiki/2018_United_States_House_of_Representatives_elections_in_North_Carolina
# Denominator (total registered voters) from https://vt.ncsbe.gov/RegStat/Results/?date=11%2F06%2F2018
weights::wpct(responses.factors$house18) # R - other - no vote - D
house18.wt <- c(1845921 / 6918150, (38728 + 7496) / 6918150, 3255103 / 6918150, 1770902 / 6918150)



### STEP 3 - Define targets list, apply appropriate names and generate unique ID
#targets <- list(party.wt, gender.wt, hisp.wt, race.wt, educ.wt, age.wt, pres16.wt)
#names(targets) <- c("party_reg", "gender", "hisp", "race", "educ_bin", "age_bin", "pres16")
targets <- list(educ.wt, age.wt, county.wt, pres16.wt, house18.wt)
names(targets) <- c("educ_bin", "age_bin", "county", "pres16", "house18")
rv$caseid <- 1:nrow(rv)
lv$caseid <- 1:nrow(lv)

#names(targets$party_reg) <- levels(responses.factors$party_reg)
#names(targets$gender) <- levels(responses.factors$gender)
#names(targets$hisp) <- levels(responses.factors$hisp)
#names(targets$race) <- levels(responses.factors$race)
names(targets$educ_bin) <- levels(responses.factors$educ_bin)
names(targets$age_bin) <- levels(responses.factors$age_bin)
names(targets$county) <- levels(responses.factors$county)
names(targets$pres16) <- levels(responses.factors$pres16)
names(targets$house18) <- levels(responses.factors$house18)



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

unweighted.gov.rv <- wpct(rv$gov)
weighted.gov.rv <- wpct(rv$gov, rv$weightvec)
tab.gov.rv <- data.frame(unweighted=round(unweighted.gov.rv, 4), 
                         weighted=round(weighted.gov.rv, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.gov.rv*(1-weighted.gov.rv))/n.rv),
         cand = names(table(rv$gov)),
         office = "Governor") %>% 
  select(cand, office, everything())

unweighted.sen.rv <- wpct(rv$sen)
weighted.sen.rv <- wpct(rv$sen, rv$weightvec)
tab.sen.rv <- data.frame(unweighted=round(unweighted.sen.rv, 4), 
                         weighted=round(weighted.sen.rv, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.sen.rv*(1-weighted.sen.rv))/n.rv),
         cand = names(table(rv$sen)),
         office = "US Senator") %>% 
  select(cand, office, everything())

tab.rv <- base::rbind(tab.pres.rv, tab.gov.rv, tab.sen.rv)



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

unweighted.gov.lv <- wpct(lv$gov)
weighted.gov.lv <- wpct(lv$gov, lv$weightvec)
tab.gov.lv <- data.frame(unweighted=round(unweighted.gov.lv, 4), 
                         weighted=round(weighted.gov.lv, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.gov.lv*(1-weighted.gov.lv))/n.lv),
         cand = names(table(lv$gov)),
         office = "Governor") %>% 
  select(cand, office, everything())

unweighted.sen.lv <- wpct(lv$sen)
weighted.sen.lv <- wpct(lv$sen, lv$weightvec)
tab.sen.lv <- data.frame(unweighted=round(unweighted.sen.lv, 4), 
                         weighted=round(weighted.sen.lv, 4)) %>% 
  mutate(weighted.moe = 1.98*sqrt((weighted.sen.lv*(1-weighted.sen.lv))/n.lv),
         cand = names(table(lv$sen)),
         office = "US Senator") %>% 
  select(cand, office, everything())

tab.lv <- base::rbind(tab.pres.lv, tab.gov.lv, tab.sen.lv)








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




