
library(tidyverse)
library(tidycensus)
library(here)
library(tidylog)
library(assertr)


### GET CVAP RACE FROM DOWNLOADED FLAT CENSUS FILES
cvap.race <- read_csv(here::here("political/data/raw/census/cvap_race/State.csv"))
cvap.race.prop <- cvap.race %>% 
  select(geoname:geoid, cvap_est) %>% 
  filter(!(lntitle %in% c("Not Hispanic or Latino", "Hispanic or Latino"))) %>% 
  pivot_wider(names_from = lntitle, values_from = cvap_est) %>% 
  mutate(p_two_races = (`American Indian or Alaska Native and White` + `Asian and White` + 
           `Black or African American and White` + `American Indian or Alaska Native and Black or African American` +
           `Remainder of Two or More Race Responses`) / Total,
         p_am_ind = `American Indian or Alaska Native Alone` / Total,
         p_asian = `Asian Alone` / Total,
         p_black = `Black or African American Alone` / Total,
         p_pac = `Native Hawaiian or Other Pacific Islander Alone` / Total,
         p_white = `White Alone` / Total) %>% 
  select(geoname, geoid, c(p_two_races:p_white))

cvap.hisp.prop <- cvap.race %>% 
  select(geoname:geoid, cvap_est) %>% 
  filter(lntitle %in% c("Total", "Not Hispanic or Latino", "Hispanic or Latino")) %>% 
  pivot_wider(names_from = lntitle, values_from = cvap_est) %>% 
  mutate(p_hisp = `Hispanic or Latino` / Total) %>% 
  select(geoname, geoid, p_hisp)



### GET CVAP VARIABLES FROM 2018 5-YEAR ACS
vars18 <- load_variables(2018, "acs5")

acs18 <- get_acs(
  geography = "state",
  variables = c(total = "B01001_001",
                total_male = "B01001_002",
                total_female = "B01001_026",
                total_cvap_age = "B29001_001",
                cvap_18_29 = "B29001_002",
                cvap_30_44 = "B29001_003",
                cvap_45_64 = "B29001_004",
                cvap_65_plus = "B29001_005",
                total_cvap_educ = "B29002_001",
                cvap_ms = "B29002_002",
                cvap_hs = "B29002_003",
                cvap_hs_grad = "B29002_004",
                cvap_some_college = "B29002_005",
                cvap_associates = "B29002_006",
                cvap_bachelors = "B29002_007",
                cvap_graduate = "B29002_008",
                total_cvap_poverty = "B29003_001",
                cvap_below_poverty = "B29003_002",
                cvap_above_poverty = "B29003_003",
                cvap_med_hhinc = "B29004_001"),
  year = 2018,
  survey = "acs5"
)

acs18.wide <- acs18 %>% 
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(GEOID = paste0("04000US", GEOID),
         p_male = total_male / total,
         p_female = total_female / total,
         p_18_29 = cvap_18_29 / total_cvap_age,
         p_30_44 = cvap_30_44 / total_cvap_age,
         p_45_64 = cvap_45_64 / total_cvap_age,
         p_65_plus = cvap_65_plus / total_cvap_age,
         p_ms = cvap_ms / total_cvap_educ,
         p_hs = cvap_hs / total_cvap_educ,
         p_hs_grad = cvap_hs_grad / total_cvap_educ,
         p_some_college = cvap_some_college / total_cvap_educ,
         p_associates = cvap_associates / total_cvap_educ,
         p_bachelors = cvap_bachelors / total_cvap_educ,
         p_graduate = cvap_graduate / total_cvap_educ,
         p_above_poverty = cvap_above_poverty / total_cvap_poverty,
         p_below_poverty = cvap_below_poverty / total_cvap_poverty) %>% 
  select(-c(total:cvap_above_poverty)) %>% 
  rename(geoname = NAME, geoid = GEOID) #%>% 
  #pivot_longer(-c(GEOID, NAME), names_to = "measure", values_to = "value")


### MAKE GROUND-TRUTH TABLE
ground.truth <- acs18.wide %>% 
  full_join(cvap.race.prop, by=c("geoname","geoid")) %>% 
  full_join(cvap.hisp.prop, by=c("geoname","geoid"))

write_csv(ground.truth, here::here("political/data/processed/census/cvap_distributions.csv"))
